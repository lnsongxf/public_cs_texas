#==============================================================================
# code to estimate and format regression tables for within-firm models
#==============================================================================
library(tidyverse)
library(lubridate)
library(lfe)
library(splines)
library(lmtest)
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
base_controls <-
  "Auction + bs(Acres, df = 7) + Term + RoyaltyRate"

#==============================================================================
# estimate models (include some specs already in bonus regs for presentation)
#==============================================================================
m_bonus <-
  paste("log(BonusPerAcre)", base_controls, sep = " ~ ") %>%
  paste("Grid10Yr + YearQtr", "0", "Grid10", sep = " | ") %>%
  as.formula %>%
  felm(reg_data)

m_bonus_firms <-
  paste("log(BonusPerAcre)", base_controls, sep = " ~ ") %>%
  paste("Grid10Yr + YearQtr + NewFirm", "0", "Grid10", sep = " | ") %>%
  as.formula %>%
  felm(reg_data, exactDOF = T)

m_output <-
  paste("DBOEPerAcre", base_controls, sep = " ~ ") %>%
  paste("Grid10Yr + YearQtr", "0", "Grid10", sep = " | ") %>%
  as.formula %>%
  felm(filter(reg_data, !Censored))

m_output_firms <-
  paste("DBOEPerAcre", base_controls, sep = " ~ ") %>%
  paste("Grid10Yr + YearQtr + NewFirm", "0", "Grid10", sep = " | ") %>%
  as.formula %>%
  felm(filter(reg_data, !Censored), exactDOF = T)

m_revenue <-
  paste("LeaseRevenuePerAcre", base_controls, sep = " ~ ") %>%
  paste("Grid10Yr + YearQtr", "0", "Grid10", sep = " | ") %>%
  as.formula %>%
  felm(filter(reg_data, !Censored))

m_revenue_firms <-
  paste("LeaseRevenuePerAcre", base_controls, sep = " ~ ") %>%
  paste("Grid10Yr + YearQtr + NewFirm", "0", "Grid10", sep = " | ") %>%
  as.formula %>%
  felm(filter(reg_data, !Censored), exactDOF = T)


#==============================================================================
# make tables
#==============================================================================
firms_tbl <-
  list(m_bonus,
       m_bonus_firms,
       m_output,
       m_output_firms,
       m_revenue,
       m_revenue_firms) %>%
  regtable(., est = "Auction",
           mnames = c(rep("Bonus", 2), rep("Output", 2), rep("Revenue", 2)),
           extra_rows = list("Firm FE" = rep(c("No", "Yes"), 3)),
           n_obs = TRUE,
           stats = c("r.squared"),
           stats_names = c("$R^2$"),
           decimals = c(2, 2, 3, 3, 2, 2),
           output_format = "latex")

writeLines(firms_tbl, file.path(tdir, "firms_regressions.tex"))

