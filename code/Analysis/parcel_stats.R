# RICH CREATED THIS OFF OF THE OLD PARCEL_STATS.R
# HEAVY EDITS BY THOM IN FEBRUARY 2019

# BASIC TEXAS SETUP ==========================================================
root <- getwd()
while(basename(root) != "texas") {
  root <- dirname(root)
}
source(file.path(root, "code", "paths.R"))
library(tidyverse)
library(lubridate)
library(lfe)
library(splines)
library(modelr)
library(lmtest)
library(sandwich)


source(file.path(root, "code", "functions", "utils.R"))
source(file.path(root, "code", "functions", "random_forest_utils.R"))
source(file.path(root, "code", "texas_constants.R"))
source(file.path(root, "code", "functions", "regtable.R"))

# LOAD AND FILTER PARCELS =====================================================
load(file.path(gen, "final_parcels.Rda"))

parcels <-
  final_parcels %>%
  filter(InSample) %>%
  mutate(ParcelType = case_when(
    Type %in% c("STATEFULL", "STATEPARTIAL") ~ "Auction",
    Type == "FREE" ~ "Free",
    TRUE ~ "RAL"),
    ParcelType = factor(ParcelType),
    ParcelType = fct_relevel(ParcelType, "RAL"))

# BALANCE TEST REGS AT GRID LEVEL: is mechanism assignment as good as random?

ldep_vars <- c("ShaleThickness", "ParcelAcres", parcel_characteristics_1)

ldep_vars_names <- c("Thickness", "Acres", parcel_characteristics_names_1)

means <-
  parcels %>%
  select(ldep_vars) %>%
  summarize_all(~ mean(., na.rm = TRUE)) %>%
  as.numeric %>%
  round(3) %>%
  format(big.mark = ",")

fe <- "Grid10"
fe_label <- "Grid10"
lrhs <-  paste("ParcelType | ", fe, " | 0 | ", fe)

ldep_vars %>%
  paste0(., " ~ ", lrhs) %>%
  map(as.formula) %>%
  fit_with(parcels, felm, ., keepX = FALSE, keepCX = FALSE,
           exactDOF = TRUE) %>%
  regtable(., est = c("ParcelTypeAuction", "ParcelTypeFree"),
           mnames = ldep_vars_names,
           extra_rows = list("Average" = means),           
           stats = "r.squared", stats_names= "$R^2$",
           est_names = c("Auction", "Free Royalty")) %>%
  writeLines(file.path(tdir, "parcel_balance.tex"))

# appendix table: same thing, more surface characteristics, but no Free
# Royalties and only parcels that are leasable as of Jan 1 2005

ldep_vars <-
  c("ShaleThickness", "ParcelAcres",
    parcel_characteristics_1, parcel_characteristics_2)
ldep_vars_names <-
  c("Thickness", "Acres",
    parcel_characteristics_names_1,
    parcel_characteristics_names_2)

parcels <-
  parcels %>%
  filter(LandType %in% c("04", "07", "15", "16"),
         !ParcelLeftCensored,
         !AlreadyLeased)

lrhs <-  paste("Auction | ", fe, " | 0 | ", fe)

means <-
  parcels %>%
  select(ldep_vars) %>%
  summarize_all(~ mean(., na.rm = TRUE)) %>%
  as.numeric %>%
  round(3) %>%
  format(big.mark = ",")

ldep_vars %>%
  paste0(., " ~ ", lrhs) %>%
  map(as.formula) %>%
  fit_with(parcels, felm, ., keepX = FALSE, keepCX = FALSE,
           exactDOF = TRUE) %>%
  regtable(., est = "Auction",
           mnames = ldep_vars_names,
           extra_rows = list("Average" = means),
           stats = "r.squared", stats_names= "$R^2$",
           est_names = "Auction") %>%
  writeLines(file.path(tdir, "parcel_balance_leaseable.tex"))


# SURVIVAL FIGURE =============================================================
# make a "survival" curve figure: for each grid and each month between jan 2005
# and december 2016, compute what fraction of the parcels of each type have
# been leased at least once
parcel_ids <-
  parcels %>%
  select(ParcelID) %>%
  unique %>%
  pluck("ParcelID")

survival_figure <-
  crossing(ParcelID = parcel_ids,
           yr = seq(2005,2016),
           mn = seq(1,12)) %>%
  mutate(yrmn = make_date(yr, mn, 1)) %>%
  filter(yrmn <= make_date(2016, 9, 30)) %>%
  select(-yr, -mn) %>%
  left_join(select(parcels, ParcelID, Auction, Grid10, FirstEffectiveDate)) %>%
  mutate(leased = yrmn > FirstEffectiveDate) %>%
  group_by(Grid10) %>%
  filter(length(unique(Auction)) > 1) %>%
  ungroup %>%
  mutate(Auction = if_else(Auction == 1, "Yes", "No")) %>%
  group_by(yrmn, Auction, Grid10) %>%
  summarize(leased = mean(leased)) %>%
  summarize(leased = mean(leased)) %>%
  ungroup %>%
  ggplot(aes(x = yrmn, y = leased, color = Auction)) +
  geom_line(size = 1.5) +
  theme_classic() +
  xlab("Date") +
  ylab("Mean Fraction of Parcels Leased") +
  scale_x_date(date_breaks = "1 year", labels = year) +
  scale_color_manual(values = c("#a50026", "#74add1")) +
  theme(legend.position = c(0.8, 0.2),
        text = element_text(size=20),
        axis.text.x = element_text(angle = 90, vjust = 0.5))

ggsave(file.path(fdir, "survival.png"),
       survival_figure,
       width = 7.5,
       height = 5)

# EVER LEASED REGRESSIONS ====================================================

# set the covariates up
extra_controls <-
  paste(paste0(parcel_characteristics_1, collapse = " + "),
        paste0(parcel_characteristics_2, collapse = " + "),
        sep = " + ")

extra_shale <-
  paste(extra_controls,
        "ShaleThickness",
        sep = " + ")

base_fm <- "Leased ~ Auction + bs(ParcelAcres, df = 7)"
grid10_rhs <- "Grid10 | 0 | Grid10"
rf_rhs <- "CentLat + CentLong"

m_10 <-
  paste(base_fm, grid10_rhs, sep = " | ") %>%
  as.formula %>%
  felm(parcels, exactDOF = TRUE)

m_rf <-
  paste(base_fm, rf_rhs, sep = " | ") %>%
  as.formula %>%
  dml(parcels, psi_plr, psi_plr_grad, psi_plr_op, n = dml_n)

m_10_extra <-
  paste(base_fm, extra_controls, sep = " + ") %>%
  paste(grid10_rhs, sep = " | ") %>%
  as.formula %>%
  felm(parcels, exactDOF = TRUE)

m_rf_extra <-
  paste(base_fm, extra_controls, sep = " + ") %>%
  paste(rf_rhs, sep = " | ") %>%
  as.formula %>%
  dml(parcels, psi_plr, psi_plr_grad, psi_plr_op, n = dml_n)  

m_10_extra_shale <-
  paste(base_fm, extra_shale, sep = " + ") %>%
  paste(grid10_rhs, sep = " | ") %>%
  as.formula %>%
  felm(filter(parcels, !is.na(ShaleThickness)), exactDOF = TRUE)

m_rf_extra_shale <-
  paste(base_fm, extra_shale, sep = " + ") %>%
  paste(rf_rhs, sep = " | ") %>%
  as.formula %>%
  dml(filter(parcels, !is.na(ShaleThickness)),
      psi_plr, psi_plr_grad, psi_plr_op, n = dml_n)  

list(m_10,
     m_rf,
     m_10_extra,
     m_rf_extra,
     m_10_extra_shale,
     m_rf_extra_shale) %>%
  regtable(est = "Auction",
           stats = c("r.squared"),
           stats_names = c("$R^2$"),
           extra_rows = list("Grid" = rep(c("10", "DML"), 3),
                             "Surface Controls" = c(rep("No", 2),
                                                    rep("Yes", 4)),
                             "Thickness Controls" = c(rep("No", 4),
                                                      rep("Yes", 2))),
           se_fun = list(vcov, vcov, vcov, vcov, vcov, vcov),
           est_names = "Auction")  %>%
  writeLines(file.path(tdir, "reg_parcel_leased.tex"))
