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

#==============================================================================
# LOAD DATA CLEANED IN SAMPLE_SELECTION.R
#==============================================================================
load(file.path(ddir, "generated_data/final_leases.Rda"))

reg_data <-
  final_leases %>%
  filter(InSample) 

#==============================================================================
# define specifications
#==============================================================================
base_controls <- "Auction + bs(Acres, df = 7) + Term + RoyaltyRate"

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
m_Grid10Yr <-
  paste("log(BonusPerAcre)", base_controls, sep = " ~ ") %>%
  paste("Grid10Yr + YearQtr", "0", "Grid10", sep = " | ") %>%
  as.formula %>%
  felm(reg_data)

m_RF <-
  paste("log(BonusPerAcre)", base_controls, sep = " ~ ") %>%
  paste("CentLat + CentLong + EffDate", sep = " | ") %>%
  as.formula %>%
  dml(reg_data, psi_plr, psi_plr_grad, psi_plr_op)  

m_Grid10YrAddendaPgCl <-
  paste("log(BonusPerAcre)", base_controls, sep = " ~ ") %>%
  paste("AddendaPages", "AddendaClauses", sep = " + ") %>%
  paste("Grid10Yr + YearQtr", "0", "Grid10", sep = " | ") %>%  
  as.formula %>%
  felm(reg_data)

m_RFAddendaPgCl <-
  paste("log(BonusPerAcre)", base_controls, sep = " ~ ") %>%
  paste("AddendaPages", "AddendaClauses", sep = " + ") %>%  
  paste("CentLat + CentLong + EffDate", sep = " | ") %>%
  as.formula %>%
  dml(reg_data, psi_plr, psi_plr_grad, psi_plr_op)    

# note: c06 is omitted here because it apparently does not show up in our
# sample
m_Grid10YrAddenda <-
  paste("log(BonusPerAcre)", base_controls, sep = " ~ ") %>%
  paste("c01", "c02", "c03", "c04", "c05", "c07", "c08", "c09",
        "c10", "c11", "c12", "c13", "c14", "c15", "c16", sep = " + ") %>%    
  paste("Grid10Yr + YearQtr", "0", "Grid10", sep = " | ") %>%  
  as.formula %>%
  felm(reg_data)

m_RFAddenda <-
  paste("log(BonusPerAcre)", base_controls, sep = " ~ ") %>%
  paste("c01", "c02", "c03", "c04", "c05", "c07", "c08", "c09",
        "c10", "c11", "c12", "c13", "c14", "c15", "c16", sep = " + ") %>%
  paste("CentLat + CentLong + EffDate", sep = " | ") %>%  
  as.formula %>%
  dml(reg_data, psi_plr, psi_plr_grad, psi_plr_op)      

#==============================================================================
# make table
#==============================================================================
addenda_tbl <-
  list(m_Grid10Yr,
       m_RF,
       m_Grid10YrAddendaPgCl,
       m_RFAddendaPgCl,
       m_Grid10YrAddenda,
       m_RFAddenda) %>%
  regtable(.,
           est = c("Auction", "AddendaPages", "AddendaClauses",
                   "c01", "c02", "c03", "c04", "c05", "c07", "c08", "c09",
                   "c10", "c11", "c12", "c13", "c14", "c15", "c16"),
           est_names = c("Auction", "Pages", "Clauses",
                         "Surface Protection",
                         "Payment Terms",
                         "Location Requirements",
                         "Pugh Clause",
                         "Cleanup Terms",
                         "Livestock Protection",
                         "On-site Water Use",
                         "Waste Management",
                         "Definitional Changes",
                         "Pollution Protection",
                         "Infrastructure Constraints",
                         "Caliche Use",
                         "Additional Fees",
                         "Time Constraints",
                         "Miscellaneous"),
           extra_rows = list("Grid" =
                               c("10", "DML", "10", "DML", "10", "DML"),
                             "Time" =
                               c("GY,Q", "DML", "GY,Q", "DML", "GY,Q", "DML")),
           n_obs = TRUE,
           stats = c("r.squared"),
           stats_names = c("$R^2$"),
           se_fun = list(vcov, vcov, vcov, vcov, vcov, vcov),
           decimals = 2,
           output_format = "latex")

addenda_tbl_prez <-
  list(m_Grid10Yr,
       m_RF,
       m_Grid10YrAddendaPgCl,
       m_RFAddendaPgCl,
       m_Grid10YrAddenda,
       m_RFAddenda) %>%
  regtable(.,
           est = c("Auction"),
           est_names = c("Auction"),
           extra_rows = list("Grid" =
                               c("10", "DML", "10", "DML", "10", "DML"),
                             "Time" =
                               c("GY,Q", "DML", "GY,Q", "DML", "GY,Q", "DML"),
                             "Addenda" = c(" ", " ",
                                           rep("Pages, Clauses", 2),
                                           rep("Clause FE", 2))),
           n_obs = TRUE,
           stats = c("r.squared"),
           stats_names = c("$R^2$"),
           se_fun = list(vcov, vcov, vcov, vcov, vcov, vcov),
           decimals = 2,
           output_format = "latex")

writeLines(addenda_tbl,
           file.path(tdir, "bonus_regressions_addenda.tex"))
writeLines(addenda_tbl_prez,
           file.path(tdir, "bonus_regressions_addenda_prez.tex"))


