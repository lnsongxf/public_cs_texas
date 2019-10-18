#==============================================================================
# code to estimate and format regression tables for bonus models
#==============================================================================
library(tidyverse)
library(lubridate)
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
load(file.path(ddir, "generated_data/final_leases.Rda"))

reg_data <-
  final_leases %>%
  mutate(ExtraInSample =
           Type %in% c("RAL", "STATE") &
           OnShale &
           !flag_missing &
           !flag_netgross &
           !flag_statenegotiate &
           !flag_undivided &
           !flag_ralownlease &
           !flag_status) %>%
  filter(ExtraInSample)

#==============================================================================
# define specifications
#==============================================================================
base_controls <- "Acres + Term + RoyaltyRate"
loctime_controls <- "CentLat + CentLong + EffDate"

extra_controls <-
  paste(paste0(parcel_characteristics_1, collapse = " + "), 
        paste0(parcel_characteristics_2, collapse = " + "),
        "MultiPolygon",
        "ShaleThickness",
        sep = " + ")


#==============================================================================
# estimate models
#==============================================================================
m_bonus <-
  paste("log(BonusPerAcre)", "Auction", sep = " ~ ") %>%
  paste(base_controls, sep = " | ") %>%
  paste(loctime_controls, sep = " + ") %>%
  as.formula %>%
  causal_forest2(reg_data,
                 seed = rfseed,
                 num.trees = 10000,
                 tune.parameters = FALSE) %>%
  overlap_average_treatment_effect

m_output <-
  paste("DBOEPerAcre", "Auction", sep = " ~ ") %>%
  paste(base_controls, sep = " | ") %>%
  paste(loctime_controls, sep = " + ") %>%
  as.formula %>%  
  causal_forest2(filter(reg_data, !Censored),
                 seed = rfseed,
                 num.trees = 10000,
                 tune.parameters = FALSE) %>%
  overlap_average_treatment_effect

m_revenue <-
  paste("LeaseRevenuePerAcre", "Auction", sep = " ~ ") %>%
  paste(base_controls, sep = " | ") %>%
  paste(loctime_controls, sep = " + ") %>%
  as.formula %>%  
  causal_forest2(filter(reg_data, !Censored),
                 seed = rfseed,
                 num.trees = 10000,
                 tune.parameters = FALSE) %>%
  overlap_average_treatment_effect

m_bonus_extra <-
  paste("log(BonusPerAcre)", "Auction", sep = " ~ ") %>%
  paste(base_controls, sep = " | ") %>%
  paste(loctime_controls, sep = " + ") %>%
  paste(extra_controls, sep = " + ") %>%  
  as.formula %>%
  causal_forest2(filter(reg_data, !is.na(ShaleThickness)),
                 seed = rfseed,
                 num.trees = 10000,
                 tune.parameters = FALSE) %>%
  overlap_average_treatment_effect

m_output_extra <-
  paste("DBOEPerAcre", "Auction", sep = " ~ ") %>%
  paste(base_controls, sep = " | ") %>%
  paste(loctime_controls, sep = " + ") %>%
  paste(extra_controls, sep = " + ") %>%    
  as.formula %>%  
  causal_forest2(filter(reg_data, !Censored, !is.na(ShaleThickness)),
                 seed = rfseed,
                 num.trees = 10000,
                 tune.parameters = FALSE) %>%
  overlap_average_treatment_effect

m_revenue_extra <-
  paste("LeaseRevenuePerAcre", "Auction", sep = " ~ ") %>%
  paste(base_controls, sep = " | ") %>%
  paste(loctime_controls, sep = " + ") %>%
  paste(extra_controls, sep = " + ") %>%    
  as.formula %>%  
  causal_forest2(filter(reg_data, !Censored, !is.na(ShaleThickness)),
                 seed = rfseed,
                 num.trees = 10000,
                 tune.parameters = FALSE) %>%
  overlap_average_treatment_effect

m_drilled <-
  paste("Drilled", "Auction", sep = " ~ ") %>%
  paste(base_controls, sep = " | ") %>%
  paste(loctime_controls, sep = " + ") %>%
  as.formula %>%  
  causal_forest2(filter(reg_data, !Censored),
                 seed = rfseed,
                 num.trees = 10000,
                 tune.parameters = FALSE) %>%
  overlap_average_treatment_effect

m_drilled_extra <-
  paste("Drilled", "Auction", sep = " ~ ") %>%
  paste(base_controls, sep = " | ") %>%
  paste(loctime_controls, sep = " + ") %>%
  paste(extra_controls, sep = " + ") %>%    
  as.formula %>%  
  causal_forest2(filter(reg_data, !Censored, !is.na(ShaleThickness)),
                 seed = rfseed,
                 num.trees = 10000,
                 tune.parameters = FALSE) %>%
  overlap_average_treatment_effect

cf_table <-
  list(m_bonus, m_bonus_extra,
       m_output, m_output_extra,
       m_revenue, m_revenue_extra,
       m_drilled, m_drilled_extra) %>%
  regtable("W.resid",
           mnames = c("Bonus", "Bonus",
                      "Output", "Output",
                      "Revenue", "Revenue",
                      "Drilled", "Drilled"),
           est_names = "Auction",
           se_fun = vcovHC,
           stats = NA,
           stats_name = NA,
           n_obs = TRUE,
           extra_rows = list("Extra" = rep(c("No", "Yes"), 4)),
           decimals = c(2, 2, 3, 3, 2, 2, 3, 3))

writeLines(cf_table, file.path(tdir, "causal_forests.tex"))





