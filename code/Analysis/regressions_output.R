#==============================================================================
# code to estimate and format regression tables for output models
#==============================================================================
library(tidyverse)
library(lubridate)
library(lfe)
library(alpaca)
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

# id number is necessary for matching up the poisson data sets later
reg_data <-
  final_leases %>%
  filter(InSample, !Censored) %>%
  mutate(id = row_number())

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
# table layouts
#==============================================================================
# Table 7: custom stack of output + lease rev in levels, then same in poisson
# levels is per acre, poisson is total

# Table 8: log(SellerRevenues), sum of bonus, royalties and rentals paid

# appendix tables:
# - drilled
# - seller revenues per acre (levels)

#==============================================================================
# estimate LeaseRevenuePerAcre models
#==============================================================================
negotiation_avg_revenue <-
  with(filter(reg_data, Auction == 0), 1000 * mean(LeaseRevenuePerAcre))

negotiation_avg_acres <-
  with(filter(reg_data, Auction == 0), 1000 * mean(Acres))

latex_number(negotiation_avg_revenue,
             "negotiation_avg_revenue",
             format = "f",
             big.mark = ",",
             digits = 0)

m_LeaseRevenue_Grid20Yr <-
  paste("LeaseRevenuePerAcre", base_controls, sep = " ~ ") %>%
  paste("Grid20Yr + YearQtr", "0", "Grid20", sep = " | ") %>%
  as.formula %>%
  felm(reg_data)

m_LeaseRevenue_Grid10Yr <-
  paste("LeaseRevenuePerAcre", base_controls, sep = " ~ ") %>%
  paste("Grid10Yr + YearQtr", "0", "Grid10", sep = " | ") %>%
  as.formula %>%
  felm(reg_data)

m_LeaseRevenue_RF <-
  paste("LeaseRevenuePerAcre", base_controls, sep = " ~ ") %>%
  paste("CentLat + CentLong + EffDate", sep = " | ") %>%
  as.formula %>%
  dml(reg_data, psi_plr, psi_plr_grad, psi_plr_op, n = dml_n)

m_LeaseRevenue_Grid10 <-
  paste("LeaseRevenuePerAcre", base_controls, sep = " ~ ") %>%
  paste("Grid10 + YearQtr", "0", "Grid10", sep = " | ") %>%
  as.formula %>%
  felm(reg_data)

m_LeaseRevenue_Grid10YrQtr <-
  paste("LeaseRevenuePerAcre", base_controls, sep = " ~ ") %>%
  paste("Grid10YrQtr", "0", "Grid10", sep = " | ") %>%
  as.formula %>%
  felm(reg_data)

m_LeaseRevenue_Grid10YrExtra <-
  paste("LeaseRevenuePerAcre", base_controls, sep = " ~ ") %>%
  paste(extra_controls, sep = " + ") %>%
  paste("Grid10Yr + YearQtr", "0", "Grid10", sep = " | ") %>%
  as.formula %>%
  felm(reg_data)

#==============================================================================
# estimate DBOEPerAcre models
#==============================================================================
negotiation_avg_dboe <-
  with(filter(reg_data, Auction == 0), 1000 * mean(DBOEPerAcre))
latex_number(negotiation_avg_dboe,
             "negotiation_avg_dboe",
             format = "f",
             big.mark = ",",
             digits = 0)

m_DBOE_Grid20Yr <-
  paste("DBOEPerAcre", base_controls, sep = " ~ ") %>%
  paste("Grid20Yr + YearQtr", "0", "Grid20", sep = " | ") %>%
  as.formula %>%
  felm(reg_data)

m_DBOE_Grid10Yr <-
  paste("DBOEPerAcre", base_controls, sep = " ~ ") %>%
  paste("Grid10Yr + YearQtr", "0", "Grid10", sep = " | ") %>%
  as.formula %>%
  felm(reg_data)

m_DBOE_RF <-
  paste("DBOEPerAcre", base_controls, sep = " ~ ") %>%
  paste("CentLat + CentLong + EffDate", sep = " | ") %>%
  as.formula %>%
  dml(reg_data, psi_plr, psi_plr_grad, psi_plr_op, n = dml_n)

m_DBOE_Grid10 <-
  paste("DBOEPerAcre", base_controls, sep = " ~ ") %>%
  paste("Grid10 + YearQtr", "0", "Grid10", sep = " | ") %>%
  as.formula %>%
  felm(reg_data)

m_DBOE_Grid10YrQtr <-
  paste("DBOEPerAcre", base_controls, sep = " ~ ") %>%
  paste("Grid10YrQtr", "0", "Grid10", sep = " | ") %>%
  as.formula %>%
  felm(reg_data)

m_DBOE_Grid10YrExtra <-
  paste("DBOEPerAcre", base_controls, sep = " ~ ") %>%
  paste(extra_controls, sep = " + ") %>%
  paste("Grid10Yr + YearQtr", "0", "Grid10", sep = " | ") %>%
  as.formula %>%
  felm(reg_data)

#==============================================================================
# estimate Drilled models
#==============================================================================
m_Drilled_Grid20Yr <-
  paste("Drilled", base_controls, sep = " ~ ") %>%
  paste("Grid20Yr + YearQtr", "0", "Grid20", sep = " | ") %>%
  as.formula %>%
  felm(reg_data)

m_Drilled_Grid10Yr <-
  paste("Drilled", base_controls, sep = " ~ ") %>%
  paste("Grid10Yr + YearQtr", "0", "Grid10", sep = " | ") %>%
  as.formula %>%
  felm(reg_data)

m_Drilled_RF <-
  paste("Drilled", base_controls, sep = " ~ ") %>%
  paste("CentLat + CentLong + EffDate", sep = " | ") %>%
  as.formula %>%
  dml(reg_data, psi_plr, psi_plr_grad, psi_plr_op, n = dml_n)

m_Drilled_Grid10 <-
  paste("Drilled", base_controls, sep = " ~ ") %>%
  paste("Grid10 + YearQtr", "0", "Grid10", sep = " | ") %>%
  as.formula %>%
  felm(reg_data)

m_Drilled_Grid10YrQtr <-
  paste("Drilled", base_controls, sep = " ~ ") %>%
  paste("Grid10YrQtr", "0", "Grid10", sep = " | ") %>%
  as.formula %>%
  felm(reg_data)

m_Drilled_Grid10YrExtra <-
  paste("Drilled", base_controls, sep = " ~ ") %>%
  paste(extra_controls, sep = " + ") %>%
  paste("Grid10Yr + YearQtr", "0", "Grid10", sep = " | ") %>%
  as.formula %>%
  felm(reg_data)

#==============================================================================
# estimate log(SellerRevenue) models in main non-censored sample
#==============================================================================
m_LogSellerRevenue_Grid20Yr <-
  paste("log(SellerRevenuePerAcre)", base_controls, sep = " ~ ") %>%
  paste("Grid20Yr + YearQtr", "0", "Grid20", sep = " | ") %>%
  as.formula %>%
  felm(reg_data)

m_LogSellerRevenue_Grid10Yr <-
  paste("log(SellerRevenuePerAcre)", base_controls, sep = " ~ ") %>%
  paste("Grid10Yr + YearQtr", "0", "Grid10", sep = " | ") %>%
  as.formula %>%
  felm(reg_data)

m_LogSellerRevenue_RF <-
  paste("log(SellerRevenuePerAcre)", base_controls, sep = " ~ ") %>%
  paste("CentLat + CentLong + EffDate", sep = " | ") %>%
  as.formula %>%
  dml(reg_data, psi_plr, psi_plr_grad, psi_plr_op, n = dml_n)  

m_LogSellerRevenue_Grid10 <-
  paste("log(SellerRevenuePerAcre)", base_controls, sep = " ~ ") %>%
  paste("Grid10 + YearQtr", "0", "Grid10", sep = " | ") %>%
  as.formula %>%
  felm(reg_data)

m_LogSellerRevenue_Grid10YrQtr <-
  paste("log(SellerRevenuePerAcre)", base_controls, sep = " ~ ") %>%
  paste("Grid10YrQtr", "0", "Grid10", sep = " | ") %>%
  as.formula %>%
  felm(reg_data)

m_LogSellerRevenue_Grid10YrExtra <-
  paste("log(SellerRevenuePerAcre)", base_controls, sep = " ~ ") %>%
  paste(extra_controls, sep = " + ") %>%
  paste("Grid10Yr + YearQtr", "0", "Grid10", sep = " | ") %>%
  as.formula %>%
  felm(reg_data)

#==============================================================================
# estimate SellerRevenuePerAcre models in the same sample
#==============================================================================
m_SellerRevenue_Grid20Yr <-
  paste("SellerRevenuePerAcre", base_controls, sep = " ~ ") %>%
  paste("Grid20Yr + YearQtr", "0", "Grid20", sep = " | ") %>%
  as.formula %>%
  felm(reg_data)

m_SellerRevenue_Grid10Yr <-
  paste("SellerRevenuePerAcre", base_controls, sep = " ~ ") %>%
  paste("Grid10Yr + YearQtr", "0", "Grid10", sep = " | ") %>%
  as.formula %>%
  felm(reg_data)

latex_number(round(coeftest(m_SellerRevenue_Grid10Yr)["Auction", 1] *
                   1000 * negotiation_avg_acres, -3),
             "SellerRevenue_Grid10Yr_total",
             format = "f", big.mark = ",", digits = 0)

m_SellerRevenue_RF <-
  paste("SellerRevenuePerAcre", base_controls, sep = " ~ ") %>%
  paste("CentLat + CentLong + EffDate", sep = " | ") %>%
  as.formula %>%
  dml(reg_data, psi_plr, psi_plr_grad, psi_plr_op, n = dml_n)  

m_SellerRevenue_Grid10 <-
  paste("SellerRevenuePerAcre", base_controls, sep = " ~ ") %>%
  paste("Grid10 + YearQtr", "0", "Grid10", sep = " | ") %>%
  as.formula %>%
  felm(reg_data)

m_SellerRevenue_Grid10YrQtr <-
  paste("SellerRevenuePerAcre", base_controls, sep = " ~ ") %>%
  paste("Grid10YrQtr", "0", "Grid10", sep = " | ") %>%
  as.formula %>%
  felm(reg_data)

m_SellerRevenue_Grid10YrExtra <-
  paste("SellerRevenuePerAcre", base_controls, sep = " ~ ") %>%
  paste(extra_controls, sep = " + ") %>%
  paste("Grid10Yr + YearQtr", "0", "Grid10", sep = " | ") %>%
  as.formula %>%
  felm(reg_data)

#==============================================================================
# Now do pseudo-poisson models.  first define specifications and helper
# functions for subsequent standard error calculations
#==============================================================================
poisson_controls <-
  paste("Auction + Term + RoyaltyRate",
        paste0(c("AcresSpline1", "AcresSpline2", "AcresSpline3",
                 "AcresSpline4", "AcresSpline5", "AcresSpline6",
                 "AcresSpline7"),
               collapse = " + "),
        sep = " + ")

dml_controls <- "Auction | Acres + Term + RoyaltyRate"

vcov10 <- function(x) vcov(x, "clustered", cluster ~ Grid10)
vcov20 <- function(x) vcov(x, "clustered", cluster ~ Grid20)

fitted.feglm <- function(x) rep(0, x$nobs["nobs"])
glance.feglm <- function(x) tibble("r.squared" = NA_real_,
                                   "adj.r.squared" = NA_real_)



#==============================================================================
# set up the data for pseudo-poisson models in a useful way.  there are two
# steps to this: (1) characterize which rows in the original dataset will
# "survive" the poisson fixed effect filtering.  (2) manually construct the
# spline columns for acres within the actual space of acreage vales in this
# restricted dataset
#==============================================================================
make_poisson_data <- function(fespec, data) {
  temp_controls <- "Auction + Term + RoyaltyRate + Acres + id"
  
  newdata <-
    paste("DBOEPerAcre", temp_controls, sep = " ~ ") %>%
    paste(fespec, sep = " | ") %>%
    as.formula %>%
    feglm(data, family = poisson(), control = list(iter.max = 10000)) %>%
    pluck("data")

  # here we manually expand the spline terms in Acres so as not to upset the
  # data.table internals of the feglm command
  acres_spline_names <-
    c("AcresSpline1" = "1", "AcresSpline2" = "2", "AcresSpline3" = "3",
      "AcresSpline4" = "4", "AcresSpline5" = "5", "AcresSpline6" = "6",
      "AcresSpline7" = "7")
  
  acres_df <-
    with(newdata, bs(Acres, df = 7)) %>%
    as_tibble %>%
    rename(!!acres_spline_names)

  newdata <-
    newdata %>% 
    bind_cols(acres_df) %>%
    select(id, starts_with("AcresSpline"))

  return(select(inner_join(data, newdata), -id))
}

data_Grid10 <- make_poisson_data("Grid10 + YearQtr", reg_data)
data_Grid10Yr <- make_poisson_data("Grid10Yr + YearQtr", reg_data)
data_Grid10YrQtr <- make_poisson_data("Grid10YrQtr", reg_data)
data_Grid20Yr <- make_poisson_data("Grid20Yr + YearQtr", reg_data)
data_Grid10YrExtra <-
  make_poisson_data("Grid10Yr + YearQtr",
                    filter(reg_data, !is.na(ShaleThickness)))

#==============================================================================
# estimate Lease Revenue pseudo-poisson models
#==============================================================================
mp_LeaseRevenue_Grid20Yr <-
  paste("LeaseRevenuePerAcre", poisson_controls, sep = " ~ ") %>%
  paste("Grid20Yr + YearQtr", "Grid20", sep = " | ") %>%
  as.formula %>%
  feglm(data_Grid20Yr, family = poisson(), control = list(iter.max = 10000))

mp_LeaseRevenue_Grid10Yr <-
  paste("LeaseRevenuePerAcre", poisson_controls, sep = " ~ ") %>%
  paste("Grid10Yr + YearQtr", "Grid10", sep = " | ") %>%
  as.formula %>%
  feglm(data_Grid10Yr, family = poisson(), control = list(iter.max = 10000))

latex_number(coeftest(mp_LeaseRevenue_Grid10Yr)["Auction", 1] * 100,
             "Poisson_LeaseRevenue_Grid10Yr",
             format = "f", big.mark = ",", digits = 0)

mp_LeaseRevenue_RF <-
  paste("LeaseRevenuePerAcre", dml_controls, sep = " ~ ") %>%
  paste("CentLat + CentLong + EffDate", sep = " + ") %>%
  as.formula %>%
  dml(reg_data, psi_plpr, psi_plpr_grad, psi_plpr_op)

mp_LeaseRevenue_Grid10 <-
  paste("LeaseRevenuePerAcre", poisson_controls, sep = " ~ ") %>%
  paste("Grid10 + YearQtr", "Grid10", sep = " | ") %>%
  as.formula %>%
  feglm(data_Grid10, family = poisson(), control = list(iter.max = 10000))

mp_LeaseRevenue_Grid10YrQtr <-
  paste("LeaseRevenuePerAcre", poisson_controls, sep = " ~ ") %>%
  paste("Grid10YrQtr", "Grid10", sep = " | ") %>%
  as.formula %>%
  feglm(data_Grid10YrQtr, family = poisson(), control = list(iter.max = 10000))

mp_LeaseRevenue_Grid10YrExtra <-
  paste("LeaseRevenuePerAcre", poisson_controls, sep = " ~ ") %>%
  paste(extra_controls, sep = " + ") %>%
  paste("Grid10Yr + YearQtr", "Grid10", sep = " | ") %>%
  as.formula %>%
  feglm(data_Grid10YrExtra, family = poisson(),
        control = list(iter.max = 10000))

#==============================================================================
# estimate DBOE pseudo-poisson models
#==============================================================================
mp_DBOE_Grid20Yr <-
  paste("DBOEPerAcre", poisson_controls, sep = " ~ ") %>%
  paste("Grid20Yr + YearQtr", "Grid20", sep = " | ") %>%
  as.formula %>%
  feglm(data_Grid20Yr, family = poisson(), control = list(iter.max = 10000))

mp_DBOE_Grid10Yr <-
  paste("DBOEPerAcre", poisson_controls, sep = " ~ ") %>%
  paste("Grid10Yr + YearQtr", "Grid10", sep = " | ") %>%
  as.formula %>%
  feglm(data_Grid10Yr, family = poisson(), control = list(iter.max = 10000))

mp_DBOE_RF <-
  paste("DBOEPerAcre", dml_controls, sep = " ~ ") %>%
  paste("CentLat + CentLong + EffDate", sep = " + ") %>%
  as.formula %>%
  dml(reg_data, psi_plpr, psi_plpr_grad, psi_plpr_op)

mp_DBOE_Grid10 <-
  paste("DBOEPerAcre", poisson_controls, sep = " ~ ") %>%
  paste("Grid10 + YearQtr", "Grid10", sep = " | ") %>%
  as.formula %>%
  feglm(data_Grid10, family = poisson(), control = list(iter.max = 10000))

mp_DBOE_Grid10YrQtr <-
  paste("DBOEPerAcre", poisson_controls, sep = " ~ ") %>%
  paste("Grid10YrQtr", "Grid10", sep = " | ") %>%
  as.formula %>%
  feglm(data_Grid10YrQtr, family = poisson(), control = list(iter.max = 10000))

mp_DBOE_Grid10YrExtra <-
  paste("DBOEPerAcre", poisson_controls, sep = " ~ ") %>%
  paste(extra_controls, sep = " + ") %>%
  paste("Grid10Yr + YearQtr", "Grid10", sep = " | ") %>%
  as.formula %>%
  feglm(data_Grid10YrExtra, family = poisson(),
        control = list(iter.max = 10000))


#==============================================================================
# estimate log bonus regressions on the pseudo-poisson sample
#==============================================================================
mp_LogBonus_Grid20Yr <-
  paste("log(BonusPerAcre)", base_controls, sep = " ~ ") %>%
  paste("Grid20Yr + YearQtr", "0", "Grid20", sep = " | ") %>%
  as.formula %>%
  felm(data_Grid20Yr)

mp_LogBonus_Grid10Yr <-
  paste("log(BonusPerAcre)", base_controls, sep = " ~ ") %>%
  paste("Grid10Yr + YearQtr", "0", "Grid10", sep = " | ") %>%
  as.formula %>%
  felm(data_Grid10Yr)

mp_LogBonus_RF <-
  paste("log(BonusPerAcre)", base_controls, sep = " ~ ") %>%
  paste("CentLat + CentLong + EffDate", sep = " | ") %>%
  as.formula %>%
  dml(reg_data, psi_plr, psi_plr_grad, psi_plr_op, n = dml_n)

mp_LogBonus_Grid10 <-
  paste("log(BonusPerAcre)", base_controls, sep = " ~ ") %>%
  paste("Grid10 + YearQtr", "0", "Grid10", sep = " | ") %>%
  as.formula %>%
  felm(data_Grid10)

mp_LogBonus_Grid10YrQtr <-
  paste("log(BonusPerAcre)", base_controls, sep = " ~ ") %>%
  paste("Grid10YrQtr", "0", "Grid10", sep = " | ") %>%
  as.formula %>%
  felm(data_Grid10YrQtr)

mp_LogBonus_Grid10YrExtra <-
  paste("log(BonusPerAcre)", base_controls, sep = " ~ ") %>%
  paste(extra_controls, sep = " + ") %>%
  paste("Grid10Yr + YearQtr", "0", "Grid10", sep = " | ") %>%
  as.formula %>%
  felm(data_Grid10YrExtra)

#==============================================================================
# estimate log(SellerRevenue) models in corresponding pseudo-poisson samples
#==============================================================================
mp_LogSellerRevenue_Grid20Yr <-
  paste("log(SellerRevenuePerAcre)", base_controls, sep = " ~ ") %>%
  paste("Grid20Yr + YearQtr", "0", "Grid20", sep = " | ") %>%
  as.formula %>%
  felm(data_Grid20Yr)

mp_LogSellerRevenue_Grid10Yr <-
  paste("log(SellerRevenuePerAcre)", base_controls, sep = " ~ ") %>%
  paste("Grid10Yr + YearQtr", "0", "Grid10", sep = " | ") %>%
  as.formula %>%
  felm(data_Grid10Yr)

mp_LogSellerRevenue_RF <-
  paste("log(SellerRevenuePerAcre)", base_controls, sep = " ~ ") %>%
  paste("CentLat + CentLong + EffDate", sep = " | ") %>%
  as.formula %>%
  dml(reg_data, psi_plr, psi_plr_grad, psi_plr_op, n = dml_n)  

mp_LogSellerRevenue_Grid10 <-
  paste("log(SellerRevenuePerAcre)", base_controls, sep = " ~ ") %>%
  paste("Grid10 + YearQtr", "0", "Grid10", sep = " | ") %>%
  as.formula %>%
  felm(data_Grid10)

mp_LogSellerRevenue_Grid10YrQtr <-
  paste("log(SellerRevenuePerAcre)", base_controls, sep = " ~ ") %>%
  paste("Grid10YrQtr", "0", "Grid10", sep = " | ") %>%
  as.formula %>%
  felm(data_Grid10YrQtr)

mp_LogSellerRevenue_Grid10YrExtra <-
  paste("log(SellerRevenuePerAcre)", base_controls, sep = " ~ ") %>%
  paste(extra_controls, sep = " + ") %>%
  paste("Grid10Yr + YearQtr", "0", "Grid10", sep = " | ") %>%
  as.formula %>%
  felm(data_Grid10YrExtra)

#==============================================================================
# make single dependent variable tables
#==============================================================================
drilled_tbl <-
  list(m_Drilled_Grid10,
       m_Drilled_Grid10Yr,
       m_Drilled_Grid10YrQtr,       
       m_Drilled_Grid20Yr,
       m_Drilled_RF,
       m_Drilled_Grid10YrExtra) %>%
  regtable(., est = "Auction",
           extra_rows = list("Grid" =
                               c("10", "10", "10", "20", "DML", "10"),
                             "Time" =
                               c("Q", "GY,Q", "GYQ", "GY,Q", "DML", "GY,Q"),
                             "Extra" = c(rep("No", 5), "Yes")),
           n_obs = TRUE,
           stats = c("r.squared"),
           stats_names = c("$R^2$"),
           se_fun = list(vcov, vcov, vcov, vcov, vcov, vcov),
           output_format = "latex")

sellerrevenue_tbl <-
  list(m_SellerRevenue_Grid10,
       m_SellerRevenue_Grid10Yr,
       m_SellerRevenue_Grid10YrQtr,
       m_SellerRevenue_Grid20Yr,
       m_SellerRevenue_RF,
       m_SellerRevenue_Grid10YrExtra) %>%
  regtable(., est = "Auction",
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

log_sellerrevenue_tbl <-
  list(m_LogSellerRevenue_Grid10,
       m_LogSellerRevenue_Grid10Yr,
       m_LogSellerRevenue_Grid10YrQtr,
       m_LogSellerRevenue_Grid20Yr,
       m_LogSellerRevenue_RF,
       m_LogSellerRevenue_Grid10YrExtra) %>%
  regtable(., est = "Auction",
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

log_sellerrevenue_poissonsample_tbl <-
  list(mp_LogSellerRevenue_Grid10,
       mp_LogSellerRevenue_Grid10Yr,
       mp_LogSellerRevenue_Grid10YrQtr,
       mp_LogSellerRevenue_Grid20Yr,
       mp_LogSellerRevenue_RF,
       mp_LogSellerRevenue_Grid10YrExtra) %>%
  regtable(., est = "Auction",
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

log_bonus_tbl <-
  list(mp_LogBonus_Grid10,
       mp_LogBonus_Grid10Yr,
       mp_LogBonus_Grid10YrQtr,
       mp_LogBonus_Grid20Yr,
       mp_LogBonus_RF,
       mp_LogBonus_Grid10YrExtra) %>%
  regtable(., est = "Auction", 
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



writeLines(log_sellerrevenue_tbl,
           file.path(tdir, "log_sellerrevenue_regressions.tex"))

writeLines(log_bonus_tbl,
           file.path(tdir, "log_bonus_poissonsample_regressions.tex"))

writeLines(log_sellerrevenue_poissonsample_tbl,
           file.path(tdir, "log_sellerrevenue_poissonsample_regressions.tex"))

writeLines(drilled_tbl,
           file.path(tdir, "drilled_regressions.tex"))

writeLines(sellerrevenue_tbl,
           file.path(tdir, "sellerrevenue_regressions.tex"))

# =============================================================================
# make the stacked table
# =============================================================================
dboe_stack <-
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
           stats = c("r.squared"),
           stats_names = c("$R^2$"),
           se_fun = list(vcov, vcov, vcov, vcov, vcov, vcov),
           decimals = 2,
           output_format = "df")

leaserevenue_stack <-
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
           stats = c("r.squared"),
           stats_names = c("$R^2$"),
           se_fun = list(vcov, vcov, vcov, vcov, vcov, vcov),
           decimals = 2,
           output_format = "df")

poisson_dboe_stack <-
  list(mp_DBOE_Grid10,
       mp_DBOE_Grid10Yr,
       mp_DBOE_Grid10YrQtr,
       mp_DBOE_Grid20Yr,
       mp_DBOE_RF,
       mp_DBOE_Grid10YrExtra) %>%
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

poisson_leaserevenue_stack <-
  list(mp_LeaseRevenue_Grid10,
       mp_LeaseRevenue_Grid10Yr,
       mp_LeaseRevenue_Grid10YrQtr,
       mp_LeaseRevenue_Grid20Yr,
       mp_LeaseRevenue_RF,
       mp_LeaseRevenue_Grid10YrExtra) %>%
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

stacked_regressions_levels <- 
  list(leaserevenue_stack, dboe_stack) %>%
  regtable_stack(table_names = c("Lease Revenue", "Output"),
                 n_bottom = TRUE,
                 output_format = "latex")

stacked_regressions_poisson <- 
  list(poisson_leaserevenue_stack, poisson_dboe_stack) %>%
  regtable_stack(table_names = c("Lease Revenue", "Output"),
                 n_bottom = TRUE,
                 output_format = "latex")

writeLines(stacked_regressions_levels,
           file.path(tdir, "stacked_output_levels.tex"))

writeLines(stacked_regressions_poisson,
           file.path(tdir, "stacked_output_poisson.tex"))



