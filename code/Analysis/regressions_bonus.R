#==============================================================================
# code to estimate and format regression tables for bonus models
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
source(file.path(root, "code", "functions", "latex_number.R"))

#==============================================================================
# LOAD DATA CLEANED IN SAMPLE_SELECTION.R
#==============================================================================
load(file.path(gen, "final_leases.Rda"))

reg_data <-
  final_leases %>%
  filter(InSample) %>%
  mutate(Private = NParcels15 > 0 | Type == "RAL")

N_RAL <- reg_data %>% filter(Auction == 0) %>% nrow

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
        sep = " + ")

extra_controls_thickness <-
  paste(extra_controls, "ShaleThickness", sep = " + ")

#==============================================================================
# estimate models
#==============================================================================
negotiation_avg_bonus <-
  with(filter(reg_data, Auction == 0), 1000 * mean(BonusPerAcre))
latex_number(negotiation_avg_bonus,
             "negotiation_avg_bonus",
             format = "f",
             big.mark = ",",
             digits = 0)

negotiation_avg_bonus_payment <-
  with(filter(reg_data, Auction == 0), mean(Bonus))

m_Grid10 <-
  paste("BonusPerAcre", base_controls, sep = " ~ ") %>%
  paste("Grid10 + YearQtr", "0", "Grid10", sep = " | ") %>%
  as.formula %>%
  felm(reg_data)

m_Grid10Yr <-
  paste("BonusPerAcre", base_controls, sep = " ~ ") %>%
  paste("Grid10Yr + YearQtr", "0", "Grid10", sep = " | ") %>%
  as.formula %>%
  felm(reg_data)

m_Grid10YrQtr <-
  paste("BonusPerAcre", base_controls, sep = " ~ ") %>%
  paste("Grid10YrQtr", "0", "Grid10", sep = " | ") %>%
  as.formula %>%
  felm(reg_data)

m_Grid20Yr <-
  paste("BonusPerAcre", base_controls, sep = " ~ ") %>%
  paste("Grid20Yr + YearQtr", "0", "Grid20", sep = " | ") %>%
  as.formula %>%
  felm(reg_data)

m_RF <-
  paste("BonusPerAcre", base_controls, sep = " ~ ") %>%
  paste("CentLat + CentLong + EffDate", sep = " | ") %>%
  as.formula %>%
  dml(reg_data, psi_plr, psi_plr_grad, psi_plr_op)

m_Grid10YrExtra <-
  paste("BonusPerAcre", base_controls, sep = " ~ ") %>%
  paste(extra_controls, sep = " + ") %>%
  paste("Grid10Yr + YearQtr", "0", "Grid10", sep = " | ") %>%
  as.formula %>%
  felm(reg_data)

m_RFExtra <-
  paste("BonusPerAcre", base_controls, sep = " ~ ") %>%
  paste(extra_controls, sep = " + ") %>%
  paste("CentLat + CentLong + EffDate", sep = " | ") %>%
  as.formula %>%
  dml(reg_data, psi_plr, psi_plr_grad, psi_plr_op)

m_Grid10YrThickness <-
  paste("BonusPerAcre", base_controls, sep = " ~ ") %>%
  paste(extra_controls_thickness, sep = " + ") %>%
  paste("Grid10Yr + YearQtr", "0", "Grid10", sep = " | ") %>%
  as.formula %>%
  felm(filter(reg_data, !is.na(ShaleThickness)))

m_RFThickness <-
  paste("BonusPerAcre", base_controls, sep = " ~ ") %>%
  paste(extra_controls_thickness, sep = " + ") %>%
  paste("CentLat + CentLong + EffDate", sep = " | ") %>%
  as.formula %>%
  dml(filter(reg_data, !is.na(ShaleThickness)),
      psi_plr, psi_plr_grad, psi_plr_op)

m_Grid10YrPrivate <-
  paste("BonusPerAcre", base_controls, sep = " ~ ") %>%
  paste("Grid10Yr + YearQtr", "0", "Grid10", sep = " | ") %>%  
  as.formula %>%
  felm(filter(reg_data, Private))

m_RFPrivate <-
  paste("BonusPerAcre", base_controls, sep = " ~ ") %>%
  paste("CentLat + CentLong + EffDate", sep = " | ") %>%
  as.formula %>%
  dml(filter(reg_data, Private), psi_plr, psi_plr_grad, psi_plr_op)

#==============================================================================
# do the rest of table4 on the private sample
#==============================================================================
m_Grid10Private <-
  paste("BonusPerAcre", base_controls, sep = " ~ ") %>%
  paste("Grid10 + YearQtr", "0", "Grid10", sep = " | ") %>%
  as.formula %>%
  felm(filter(reg_data, Private))

m_Grid10YrQtrPrivate <-
  paste("BonusPerAcre", base_controls, sep = " ~ ") %>%
  paste("Grid10YrQtr", "0", "Grid10", sep = " | ") %>%
  as.formula %>%
  felm(filter(reg_data, Private))

m_Grid20YrPrivate <-
  paste("BonusPerAcre", base_controls, sep = " ~ ") %>%
  paste("Grid20Yr + YearQtr", "0", "Grid20", sep = " | ") %>%
  as.formula %>%
  felm(filter(reg_data, Private))

m_Grid10YrExtraPrivate <-
  paste("BonusPerAcre", base_controls, sep = " ~ ") %>%
  paste(extra_controls, sep = " + ") %>%
  paste("Grid10Yr + YearQtr", "0", "Grid10", sep = " | ") %>%
  as.formula %>%
  felm(filter(reg_data, Private))

#==============================================================================
# make tables
#==============================================================================
bonus_tbl <-
  list(m_Grid10,
       m_Grid10Yr,
       m_Grid10YrQtr,
       m_Grid20Yr,
       m_RF) %>%
  regtable(., est = "Auction",
           extra_rows = list("Grid" =
                               c("10", "10", "10", "20", "DML"),
                             "Time" =
                               c("Q", "GY,Q", "GYQ", "GY,Q", "DML")),
           n_obs = TRUE,
           stats = c("r.squared"),
           stats_names = c("$R^2$"),
           se_fun = list(vcov, vcov, vcov, vcov, vcov),
           decimals = 2,
           output_format = "latex")


bonus_tbl_prez <-
  list(m_Grid10Yr,
       m_Grid10YrThickness,
       m_Grid20Yr,
       m_RF) %>%
  regtable(., est = "Auction",
           extra_rows = list("Grid" =
                               c("10", "10", "20", "DML"),
                             "Extra" = c("No", "Yes", "No", "No")),
           n_obs = TRUE,
           stats = c("r.squared"),
           stats_names = c("$R^2$"),
           se_fun = list(vcov, vcov, vcov, vcov),
           decimals = 2,           
           output_format = "latex")

discussion_tbl <-
  list(m_Grid10YrExtra,
       m_RFExtra,
       m_Grid10YrThickness,
       m_RFThickness,
       m_Grid10YrPrivate,
       m_RFPrivate) %>%
  regtable(.,
           est = c("Auction"),
           est_names = c("Auction"),
           extra_rows = list("Estimate" = rep(c("G10Y", "DML"), 3),
                             "Surface Controls" =
                               c(rep("Yes", 4), rep("No", 2)),
                             "Thickness Controls" =
                               c("No", "No", "Yes", "Yes", "No", "No"),
                             "Private Only" =
                               c(rep("No", 4), rep("Yes", 2))),
           n_obs = TRUE,
           stats = c("r.squared"),
           stats_names = c("$R^2$"),
           se_fun = list(vcov, vcov, vcov, vcov, vcov, vcov),
           decimals = 2,           
           output_format = "latex")


writeLines(bonus_tbl, file.path(tdir, "bonus_regressions.tex"))
writeLines(bonus_tbl_prez, file.path(tdir, "bonus_regressions_prez.tex"))
writeLines(discussion_tbl, file.path(tdir, "bonus_regressions_discussion.tex"))


# REPEAT USING LOG BONUS AS THE DEPENDENT VARIABLE ----------------------------
m_Grid10 <-
  paste("log(BonusPerAcre)", base_controls, sep = " ~ ") %>%
  paste("Grid10 + YearQtr", "0", "Grid10", sep = " | ") %>%
  as.formula %>%
  felm(reg_data)

latex_number(coeftest(m_Grid10)["Auction", 1] * 100,
             "Bonus_Grid10_log",
             format = "f", big.mark = ",", digits = 0)

m_Grid10Yr <-
  paste("log(BonusPerAcre)", base_controls, sep = " ~ ") %>%
  paste("Grid10Yr + YearQtr", "0", "Grid10", sep = " | ") %>%
  as.formula %>%
  felm(reg_data)

latex_number(coeftest(m_Grid10Yr)["Auction", 1] * 100,
             "Bonus_Grid10Yr_log",
             format = "f", big.mark = ",", digits = 0)

latex_number(round((exp(coeftest(m_Grid10Yr)["Auction", 1]) - 1) *
                     negotiation_avg_bonus_payment, -3),
             "Bonus_Grid10Yr_log_total",
             format = "f", big.mark = ",", digits = 0)

m_Grid10YrQtr <-
  paste("log(BonusPerAcre)", base_controls, sep = " ~ ") %>%
  paste("Grid10YrQtr", "0", "Grid10", sep = " | ") %>%
  as.formula %>%
  felm(reg_data)

m_Grid20Yr <-
  paste("log(BonusPerAcre)", base_controls, sep = " ~ ") %>%
  paste("Grid20Yr + YearQtr", "0", "Grid20", sep = " | ") %>%
  as.formula %>%
  felm(reg_data)

m_RF <-
  paste("log(BonusPerAcre)", base_controls, sep = " ~ ") %>%
  paste("CentLat + CentLong + EffDate", sep = " | ") %>%
  as.formula %>%
  dml(reg_data, psi_plr, psi_plr_grad, psi_plr_op)

m_Grid10YrExtra <-
  paste("log(BonusPerAcre)", base_controls, sep = " ~ ") %>%
  paste(extra_controls, sep = " + ") %>%
  paste("Grid10Yr + YearQtr", "0", "Grid10", sep = " | ") %>%
  as.formula %>%
  felm(reg_data)

m_RFExtra <-
  paste("log(BonusPerAcre)", base_controls, sep = " ~ ") %>%
  paste(extra_controls, sep = " + ") %>%
  paste("CentLat + CentLong + EffDate", sep = " | ") %>%  
  as.formula %>%
  dml(reg_data, psi_plr, psi_plr_grad, psi_plr_op)  

m_Grid10YrThickness <-
  paste("log(BonusPerAcre)", base_controls, sep = " ~ ") %>%
  paste(extra_controls, sep = " + ") %>%
  paste("ShaleThickness", sep = " + ") %>%
  paste("Grid10Yr + YearQtr", "0", "Grid10", sep = " | ") %>%
  as.formula %>%
  felm(filter(reg_data, !is.na(ShaleThickness)))

m_RFThickness <-
  paste("log(BonusPerAcre)", base_controls, sep = " ~ ") %>%
  paste(extra_controls, sep = " + ") %>%
  paste("ShaleThickness", sep = " + ") %>%  
  paste("CentLat + CentLong + EffDate", sep = " | ") %>%
  as.formula %>%
  dml(filter(reg_data, !is.na(ShaleThickness)),
      psi_plr, psi_plr_grad, psi_plr_op)

m_Grid10YrPrivate <-
  paste("log(BonusPerAcre)", base_controls, sep = " ~ ") %>%
  paste("Grid10Yr + YearQtr", "0", "Grid10", sep = " | ") %>%  
  as.formula %>%
  felm(filter(reg_data, Private))

m_RFPrivate <-
  paste("log(BonusPerAcre)", base_controls, sep = " ~ ") %>%
  paste("CentLat + CentLong + EffDate", sep = " | ") %>%
  as.formula %>%
  dml(filter(reg_data, Private), psi_plr, psi_plr_grad, psi_plr_op)


#==============================================================================
# make tables
#==============================================================================
bonus_tbl <-
  list(m_Grid10,
       m_Grid10Yr,
       m_Grid10YrQtr,
       m_Grid20Yr,
       m_RF) %>%
  regtable(., est = "Auction",
           extra_rows = list("Grid" =
                               c("10", "10", "10", "20", "DML"),
                             "Time" =
                               c("Q", "GY,Q", "GYQ", "GY,Q", "DML")),
           n_obs = TRUE,
           stats = c("r.squared"),
           stats_names = c("$R^2$"),
           se_fun = list(vcov, vcov, vcov, vcov, vcov),
           decimals = 2,
           output_format = "latex")

discussion_tbl <-
  list(m_Grid10YrExtra,
       m_RFExtra,
       m_Grid10YrThickness,
       m_RFThickness,
       m_Grid10YrPrivate,
       m_RFPrivate) %>%
  regtable(.,
           est = c("Auction"),
           est_names = c("Auction"),
           extra_rows = list("Estimate" = rep(c("G10Y", "DML"), 3),
                             "Surface Controls" =
                               c(rep("Yes", 4), rep("No", 2)),
                             "Thickness Controls" =
                               c("No", "No", "Yes", "Yes", "No", "No"),
                             "Private Only" =
                               c(rep("No", 4), rep("Yes", 2))),
           n_obs = TRUE,
           stats = c("r.squared"),
           stats_names = c("$R^2$"),
           se_fun = list(vcov, vcov, vcov, vcov, vcov, vcov),
           decimals = 2,           
           output_format = "latex")

writeLines(bonus_tbl, file.path(tdir, "logbonus_regressions.tex"))
writeLines(discussion_tbl,
           file.path(tdir, "logbonus_regressions_discussion.tex"))

