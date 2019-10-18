#==============================================================================
# code to estimate and format regression tables for output models, this time
# using pseudo-poisson methods
# dropping non-stacked tables here
#==============================================================================
library(tidyverse)
library(lubridate)
library(alpaca)
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
source(file.path(root, "code", "functions", "latex_number.R"))

#==============================================================================
# LOAD DATA CLEANED IN SAMPLE_SELECTION.R
#==============================================================================
load(file.path(gen, "final_leases.Rda"))

reg_data <-
  final_leases %>%
  filter(InSample, !Censored) %>%
  mutate(LeaseRevenue = LeaseRevenuePerAcre * Acres,
         DBOE = DBOEPerAcre * Acres)

# here we manually expand the spline terms in Acres so as not to upset the
# data.table internals of the feglm command
acres_spline_names <-
  c("AcresSpline1" = "1", "AcresSpline2" = "2", "AcresSpline3" = "3",
    "AcresSpline4" = "4", "AcresSpline5" = "5", "AcresSpline6" = "6",
    "AcresSpline7" = "7")

acres_df <-
  with(reg_data, bs(Acres, df = 7)) %>%
  as_tibble %>%
  rename(!!acres_spline_names)

reg_data <-
  bind_cols(reg_data, acres_df)

#==============================================================================
# define specifications
#==============================================================================
base_controls <- "Auction + Term + RoyaltyRate"
base_controls <-
  paste(base_controls,
        paste0(c("AcresSpline1", "AcresSpline2", "AcresSpline3",
                 "AcresSpline4", "AcresSpline5", "AcresSpline6",
                 "AcresSpline7"), collapse = " + "), sep = " + ")

dml_controls <- "Auction | Acres + Term + RoyaltyRate"

# set the covariates up
extra_controls <-
  paste(parcel_characteristics_1,
        parcel_characteristics_2,
        "MultiPolygon",
        "ShaleThickness",
        sep = " + ")

vcov10 <- function(x) vcov(x, "clustered", cluster ~ Grid10)
vcov20 <- function(x) vcov(x, "clustered", cluster ~ Grid20)

fitted.feglm <- function(x) rep(0, x$nobs["nobs"])
glance.feglm <- function(x) tibble("r.squared" = NA_real_,
                                   "adj.r.squared" = NA_real_)


#==============================================================================
# estimate DBOE/Acre models
#==============================================================================
m_DBOE_Grid20Yr <-
  paste("DBOE", base_controls, sep = " ~ ") %>%
  paste("Grid20Yr + YearQtr", "Grid20", sep = " | ") %>%
  as.formula %>%
  feglm(reg_data, family = poisson())

m_DBOE_Grid10Yr <-
  paste("DBOE", base_controls, sep = " ~ ") %>%
  paste("Grid10Yr + YearQtr", "Grid10", sep = " | ") %>%
  as.formula %>%
  feglm(reg_data, family = poisson())

m_DBOE_RF <-
  paste("DBOE", dml_controls, sep = " ~ ") %>%
  paste("CentLat + CentLong + EffDate", sep = " + ") %>%
  as.formula %>%
  dml(reg_data, psi_plpr, psi_plpr_grad, psi_plpr_op)

m_DBOE_Grid10 <-
  paste("DBOE", base_controls, sep = " ~ ") %>%
  paste("Grid10 + YearQtr", "Grid10", sep = " | ") %>%
  as.formula %>%
  feglm(reg_data, family = poisson())

m_DBOE_Grid10YrQtr <-

  paste("DBOE", base_controls, sep = " ~ ") %>%
  paste("Grid10YrQtr", "Grid10", sep = " | ") %>%
  as.formula %>%
  feglm(reg_data, family = poisson())

m_DBOE_Grid10YrExtra <-
  paste("DBOE", base_controls, sep = " ~ ") %>%
  paste(extra_controls, sep = " + ") %>%
  paste("Grid10Yr + YearQtr", "Grid10", sep = " | ") %>%
  as.formula %>%
  feglm(reg_data, family = poisson())

output_table <-
  list(m_DBOE_Grid10,
       m_DBOE_Grid10Yr,
       m_DBOE_Grid10YrQtr,
       m_DBOE_Grid20Yr,
       m_DBOE_RF,
       m_DBOE_Grid10YrExtra) %>%
  regtable(., est = "Auction",
           extra_rows = list("Grid" =
                               c("10", "10", "10", "20", "DML", "10"),
                             "Time" =
                               c("Q", "GY,Q", "GYQ", "GY,Q", "DML", "GY,Q"),
                             "Extra" = c(rep("No", 5), "Yes")),
           n_obs = TRUE,
           stats = NA,
           stats_names = NA,
           se_fun = list(vcov10, vcov10, vcov10, vcov20, vcov, vcov10),
           decimals = 2,
           output_format = "df")


#==============================================================================
# estimate Lease Revenue Per Acre models
#==============================================================================
m_LeaseRevenue_Grid20Yr <-
  paste("LeaseRevenue", base_controls, sep = " ~ ") %>%
  paste("Grid20Yr + YearQtr", "Grid20", sep = " | ") %>%
  as.formula %>%
  feglm(reg_data, family = poisson())

m_LeaseRevenue_Grid10Yr <-
  paste("LeaseRevenue", base_controls, sep = " ~ ") %>%
  paste("Grid10Yr + YearQtr", "Grid10", sep = " | ") %>%
  as.formula %>%
  feglm(reg_data, family = poisson())

latex_number(coeftest(m_Grid10Yr)["Auction", 1] * 100,
             "Poisson_Grid10Yr",
             format = "f", big.mark = ",", digits = 0)

m_LeaseRevenue_RF <-
  paste("LeaseRevenue", dml_controls, sep = " ~ ") %>%
  paste("CentLat + CentLong + EffDate", sep = " + ") %>%
  as.formula %>%
  dml(reg_data, psi_plpr, psi_plpr_grad, psi_plpr_op)

m_LeaseRevenue_Grid10 <-
  paste("LeaseRevenue", base_controls, sep = " ~ ") %>%
  paste("Grid10 + YearQtr", "Grid10", sep = " | ") %>%
  as.formula %>%
  feglm(reg_data, family = poisson())

m_LeaseRevenue_Grid10YrQtr <-
  paste("LeaseRevenue", base_controls, sep = " ~ ") %>%
  paste("Grid10YrQtr", "Grid10", sep = " | ") %>%
  as.formula %>%
  feglm(reg_data, family = poisson())

m_LeaseRevenue_Grid10YrExtra <-
  paste("LeaseRevenue", base_controls, sep = " ~ ") %>%
  paste(extra_controls, sep = " + ") %>%
  paste("Grid10Yr + YearQtr", "Grid10", sep = " | ") %>%
  as.formula %>%
  feglm(reg_data, family = poisson())

lease_revenue_table <-
  list(m_LeaseRevenue_Grid10,
       m_LeaseRevenue_Grid10Yr,
       m_LeaseRevenue_Grid10YrQtr,
       m_LeaseRevenue_Grid20Yr,
       m_LeaseRevenue_RF,
       m_LeaseRevenue_Grid10YrExtra) %>%
  regtable(., est = "Auction",
           extra_rows = list("Grid" =
                               c("10", "10", "10", "20", "DML", "10"),
                             "Time" =
                               c("Q", "GY,Q", "GYQ", "GY,Q", "DML", "GY,Q"),
                             "Extra" = c(rep("No", 5), "Yes")),
           n_obs = TRUE,
           stats = NA,
           stats_names = NA,
           se_fun = list(vcov10, vcov10, vcov10, vcov20, vcov, vcov10),
           decimals = 2,
           output_format = "df")


# ==============================================================================
# save tables
# ==============================================================================
## writeLines(output_table,
##            file.path(tdir, "output_regressions_poisson.tex"))
## writeLines(lease_revenue_table,
##            file.path(tdir, "revenue_regressions_poisson.tex"))
## writeLines(output_tbl_prez,
##            file.path(tdir, "output_regressions_poisson_prez.tex"))
