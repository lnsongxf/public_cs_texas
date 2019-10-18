#==============================================================================
# what should we say about delay rentals?
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
load(file.path(ddir, "generated_data/final_leases.Rda"))

# here we assume that missing delay rental information implies zero
reg_data <-
  final_leases %>%
  filter(InSample)

#==============================================================================
# define specifications
#==============================================================================
base_controls <-
  "Auction + bs(Acres, df = 7) + Term + RoyaltyRate"

# set the covariates up
extra_controls <-
  paste(paste0(parcel_characteristics_1, collapse = " + "), 
        paste0(parcel_characteristics_2, collapse = " + "),
        "MultiPolygon",
        "ShaleThickness",
        sep = " + ")

#==============================================================================
# estimate models
#==============================================================================
m_Grid10 <-
  paste("RentalsPerAcre", base_controls, sep = " ~ ") %>%
  paste("Grid10 + YearQtr", "0", "Grid10", sep = " | ") %>%
  as.formula %>%
  felm(reg_data)

m_Grid10Yr <-
  paste("RentalsPerAcre", base_controls, sep = " ~ ") %>%
  paste("Grid10Yr + YearQtr", "0", "Grid10", sep = " | ") %>%
  as.formula %>%
  felm(reg_data)

m_Grid10YrQtr <-
  paste("RentalsPerAcre", base_controls, sep = " ~ ") %>%
  paste("Grid10YrQtr", "0", "Grid10", sep = " | ") %>%
  as.formula %>%
  felm(reg_data)

m_Grid20Yr <-
  paste("RentalsPerAcre", base_controls, sep = " ~ ") %>%
  paste("Grid20Yr + YearQtr", "0", "Grid20", sep = " | ") %>%
  as.formula %>%
  felm(reg_data)

m_RF <-
  paste("RentalsPerAcre", base_controls, sep = " ~ ") %>%
  paste("CentLat + CentLong + EffDate", sep = " | ") %>%
  as.formula %>%
  dml(reg_data, psi_plr, psi_plr_grad, psi_plr_op)

m_Grid10YrExtra <-
  paste("RentalsPerAcre", base_controls, sep = " ~ ") %>%
  paste(extra_controls, sep = " + ") %>%
  paste("Grid10Yr + YearQtr", "0", "Grid10", sep = " | ") %>%
  as.formula %>%
  felm(reg_data)

#==============================================================================
# estimate models
#==============================================================================
reg_data <-
  reg_data %>%
  filter(!Censored)

mu_Grid10 <-
  paste("RentalsPaidPerAcre", base_controls, sep = " ~ ") %>%
  paste("Grid10 + YearQtr", "0", "Grid10", sep = " | ") %>%
  as.formula %>%
  felm(reg_data)

mu_Grid10Yr <-
  paste("RentalsPaidPerAcre", base_controls, sep = " ~ ") %>%
  paste("Grid10Yr + YearQtr", "0", "Grid10", sep = " | ") %>%
  as.formula %>%
  felm(reg_data)

mu_Grid10YrQtr <-
  paste("RentalsPaidPerAcre", base_controls, sep = " ~ ") %>%
  paste("Grid10YrQtr", "0", "Grid10", sep = " | ") %>%
  as.formula %>%
  felm(reg_data)

mu_Grid20Yr <-
  paste("RentalsPaidPerAcre", base_controls, sep = " ~ ") %>%
  paste("Grid20Yr + YearQtr", "0", "Grid20", sep = " | ") %>%
  as.formula %>%
  felm(reg_data)

mu_RF <-
  paste("RentalsPaidPerAcre", base_controls, sep = " ~ ") %>%
  paste("CentLat + CentLong + EffDate", sep = " | ") %>%
  as.formula %>%
  dml(reg_data, psi_plr, psi_plr_grad, psi_plr_op)

mu_Grid10YrExtra <-
  paste("RentalsPaidPerAcre", base_controls, sep = " ~ ") %>%
  paste(extra_controls, sep = " + ") %>%
  paste("Grid10Yr + YearQtr", "0", "Grid10", sep = " | ") %>%
  as.formula %>%
  felm(reg_data)

#==============================================================================
# make table
#==============================================================================
rentals_contracted_tbl <-
  list(m_Grid10,
       m_Grid10Yr,
       m_Grid10YrQtr,
       m_Grid20Yr,       
       m_RF,
       m_Grid10YrExtra) %>%
  regtable(.,
           est = c("Auction"),
           est_names = c("Auction"),
           extra_rows = list("Grid" =
                               c("10", "10", "10", "20", "DML", "10"),
                             "Time" =
                               c("Q", "GY,Q", "GYQ", "GY,Q", "DML", "GY,Q"),
                             "Extra" = c(rep("No", 5), "Yes")),
           n_obs = TRUE,
           stats = c("r.squared"),
           stats_names = c("$R^2$"),
           se_fun = list(vcov, vcov, vcov, vcov, vcov, vcov),
           decimals = 2,
           output_format = "latex")

rentals_paid_tbl <-
  list(mu_Grid10,
       mu_Grid10Yr,
       mu_Grid10YrQtr,
       mu_Grid20Yr,       
       mu_RF,
       mu_Grid10YrExtra) %>%
  regtable(.,
           est = c("Auction"),
           est_names = c("Auction"),
           extra_rows = list("Grid" =
                               c("10", "10", "10", "20", "DML", "10"),
                             "Time" =
                               c("Q", "GY,Q", "GYQ", "GY,Q", "DML", "GY,Q"),
                             "Extra" = c(rep("No", 5), "Yes")),
           n_obs = TRUE,
           stats = c("r.squared"),
           stats_names = c("$R^2$"),
           se_fun = list(vcov, vcov, vcov, vcov, vcov, vcov),
           decimals = 2,
           output_format = "latex")

writeLines(rentals_contracted_tbl,
           file.path(tdir, "rentals_regressions_contracted.tex"))

writeLines(rentals_paid_tbl,
           file.path(tdir, "rentals_regressions.tex"))


