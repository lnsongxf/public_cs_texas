#==============================================================================
# code to estimate and format regression tables for bonus models
#==============================================================================
library(tidyverse)
library(lubridate)
library(lfe)
library(lmtest)
library(splines)
library(sandwich)

#==============================================================================
# BASIC TEXAS SETUP
#==============================================================================
root <- getwd()
while(basename(root) != "texas") {
  root <- dirname(root)
}

source(file.path(root, "code", "paths.R"))
source(file.path(root, "code", "texas_constants.R"))
source(file.path(root, "code", "functions", "random_forest_utils.R"))
source(file.path(root, "code", "functions", "regtable.R"))

#==============================================================================
# LOAD DATA CLEANED IN SAMPLE_SELECTION.R
#==============================================================================
load(file.path(gen, "final_leases.Rda"))

reg_data <-
  final_leases %>%
  filter(InSample)

#==============================================================================
# define specifications
#==============================================================================
base_controls <- "Auction + bs(Acres, df = 7) + Term + RoyaltyRate"

#==============================================================================
# estimate models
#==============================================================================
m_Bonus <-
  paste("BonusPerAcre", base_controls, sep = " ~ ") %>%
  paste("Grid10Yr + YearQtr", "0", "Grid10", sep = " | ") %>%
  as.formula %>%
  felm(reg_data)

m_Drilled <-
  paste("Drilled", base_controls, sep = " ~ ") %>%
  paste("Grid10Yr + YearQtr", "0", "Grid10", sep = " | ") %>%
  as.formula %>%
  felm(filter(reg_data, !Censored))

m_Output <-
  paste("DBOEPerAcre", base_controls, sep = " ~ ") %>%
  paste("Grid10Yr + YearQtr", "0", "Grid10", sep = " | ") %>%
  as.formula %>%
  felm(filter(reg_data, !Censored))

#==============================================================================
# get negotiation averages
#==============================================================================
negotiation_Bonus <-
  reg_data %>%
  filter(Auction == 0) %>%
  with(mean(BonusPerAcre))

negotiation_Drilled <-
  reg_data %>%
  filter(!Censored, Auction == 0) %>%
  with(mean(Drilled))

negotiation_Output <-
  reg_data %>%
  filter(!Censored, Auction == 0) %>%
  with(mean(DBOEPerAcre))

  
#==============================================================================
# make a figure
#==============================================================================
get_est <- function(x, y, name) {
  coeftest(x) %>%
    tidy %>%
    filter(term == "Auction") %>%
    mutate(estimate = estimate / y, Outcome = name) %>%
    select(Outcome, estimate)
}

fig <-
  list(list(m_Bonus, m_Drilled, m_Output),
       list(negotiation_Bonus,
            negotiation_Drilled,
            negotiation_Output),
       list("Bonus Payment", "Drilling", "Output")) %>%
  pmap_dfr(get_est) %>%
  ggplot(aes(Outcome, estimate)) +
  geom_col() +
  theme_classic() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  ylab("Percent Gain From Using Auctions")


ggsave(file.path(pdir, "epic.png"))
