# compute some basic summary statistics and figures

#===========================================================================
# BASIC TEXAS SETUP
#===========================================================================
root = getwd()
while(basename(root) != "texas") {
    root = dirname(root)
}
source(file.path(root, "code/paths.R"))

library(tidyverse)
library(lubridate)
library(knitr)
library(kableExtra)
library(broom)

source(file.path(root, "code", "texas_constants.R"))
source(file.path(root, "code", "functions", "latex_number.R"))

# SAMPLE DEFINED IN sample_selection.R
load(file.path(gen, "final_leases.Rda"))

lease_data <- 
  final_leases %>% 
  filter(Type == "RAL" | Type == "STATE")

# report how many of each lease type t there are
save_sample_size <- function(tlabel, df) {
    df %>%
        nrow %>%
            latex_number(paste("Nsample_", tlabel, sep = ""),
                         format = "f", big.mark = ",", digits = 0)
}

# save sample sizes before filtering to in sample
save_sample_size(tlabel ="RAL_RAW",df=filter(lease_data,Type=="RAL"))
save_sample_size(tlabel ="STATE_RAW",df=filter(lease_data,Type=="STATE"))
save_sample_size(tlabel ="ALL_RAW",df=filter(lease_data))
save_sample_size(tlabel ="RAL_AUCTION_RAW",
                 df=filter(lease_data,Type=="RAL",Auction==1))

# then do the in sample filters
save_sample_size(tlabel ="NEGOTIATION_CLEAN",
                 df=filter(lease_data, InSample, Auction == 0))

save_sample_size(tlabel ="AUCTION_CLEAN",
                 df=filter(lease_data,InSample, Auction == 1))


# COHORTS OVER TIME FIGURE ----------------------------------------------------
# how much data do we have and what time periods does it cover?
cohorts_data <-
  lease_data %>%
  filter(InSample) %>%
  mutate(Year = factor(year(Effective_Date)),
         Mechanism = if_else(Auction == 1, "Auction", "Negotiation")) %>%
  mutate(Mechanism = factor(Mechanism),
         Mechanism = relevel(Mechanism, "Negotiation")) %>%
  select(Lease_Number, Year, Mechanism)

cohorts <-
  cohorts_data %>%
  ggplot(aes(Year)) +
  geom_bar(aes(fill = Mechanism)) +
  ylab("Leases") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "top") +
  scale_fill_manual(values = c("#a50026", "#74add1"))

ggsave(file.path(fdir, "cohorts.png"),
       height = 2.75, width = 6)

cohorts_data <-
  cohorts_data %>%
  group_by(Year, Mechanism) %>%
  count()

write_csv(cohorts_data, path = file.path(pdir, "cohorts_data.csv"))

# CREATE SUMMARY STATISTICS TABLE  --------------------------------------------

save_sample_size(tlabel = "AUCTION_NOTHICK",
                 df = filter(lease_data,
                             InSample,
                             Auction == 1,
                             is.na(ShaleThickness)))

save_sample_size(tlabel = "NEGOTIATION_NOTHICK",
                 df = filter(lease_data,
                             InSample,
                             Auction == 0,
                             is.na(ShaleThickness)))

sumvars <-
  lease_data %>%
  filter(InSample) %>%
  select(Bonus = BonusPerAcre,
         RoyaltyRate,
         Term,
         RentalsContract = RentalsPerAcre,
         LeaseRevenue = LeaseRevenuePerAcre,
         RentalsPaid = RentalsPaidPerAcre,
         SellerRevenue = SellerRevenuePerAcre,
         Output = DBOEPerAcre,
         Acres,
         Drilled,
         Auction,
         ShapeQuality,
         MultiPolygon,
         ShaleThickness)

sumvar_stats <-
  sumvars %>%
  group_by(Auction) %>%
  summarise_all(list(min = ~ min(., na.rm = TRUE), 
                     max = ~ max(., na.rm = TRUE),
                     mean = ~ mean(., na.rm = TRUE), 
                     sd = ~ sd(., na.rm = TRUE))) %>%
  gather(stat, val, -Auction) %>%
  separate(stat, into = c("variable", "stat"), sep = "_") %>%
  mutate(nstat = paste(stat, Auction, sep = "_")) %>% 
  select(-stat, -Auction) %>%
  spread(nstat, val) %>%
  select(variable,
         min_0,
         max_0,
         mean_0,
         sd_0,
         min_1,
         max_1,
         mean_1,
         sd_1)

# NOW GET TTEST OF MEANS
sumvar_tstats <-
  sumvars %>%
  gather(var, value, -Auction) %>%
  group_by(var) %>%
  do(tidy(t.test(value ~ Auction, data = .))) %>%
  ungroup %>%
  select(variable = var, diff = estimate, p_value = p.value)

# COMBINE AND MAKE IT INTO A TABLE
n_Negotiation <-
  lease_data %>%
  filter(InSample, Auction == 0) %>%
  nrow %>%
  paste("Negotiation (N = ", ., ")", sep = "")

n_Auction <-
  lease_data %>%
  filter(InSample, Auction == 1) %>%
  nrow %>%
  paste("Auction (N = ", ., ")", sep = "")

n_header <- c(1, 4, 4)
names(n_header) <- c(" ", n_Negotiation, n_Auction)

summary_stats_table <-
  inner_join(sumvar_stats, sumvar_tstats, by = "variable") %>%
  select(variable,
         mean_0, sd_0, min_0, max_0,
         mean_1, sd_1, min_1, max_1,
         diff, p_value) %>%
  mutate(temp = case_when(
           variable == "Acres" ~ 1,
           variable == "ShapeQuality" ~ 2,
           variable == "MultiPolygon" ~ 3,
           variable == "ShaleThickness" ~ 4,
           variable == "Bonus" ~ 5,
           variable == "Term" ~ 6,
           variable == "RentalsContract" ~ 7,
           variable == "RoyaltyRate" ~ 8,
           variable == "Drilled" ~ 9,
           variable == "Output" ~ 10,
           variable == "LeaseRevenue" ~ 11,
           variable == "RentalsPaid" ~ 12,           
           variable == "SellerRevenue" ~ 13,
           TRUE ~ NA_real_)) %>%
  mutate(variable = case_when(
           variable == "ShapeQuality" ~ "Shape Quality",
           variable == "MultiPolygon" ~ "Multiple Parcels",
           variable == "ShaleThickness" ~ "Shale Thickness",
           variable == "RentalsContract" ~ "Contracted Rentals",
           variable == "RoyaltyRate" ~ "Royalty Rate",
           variable == "LeaseRevenue" ~ "Lease Production Revenue",
           variable == "RentalsPaid" ~ "Realized Rentals",
           variable == "SellerRevenue" ~ "Total Seller Revenue",
           TRUE ~ variable)) %>%
  arrange(temp) %>%
  select(-temp) %>%
  kable(digits = 2,
        format = "latex",
        booktabs = TRUE,
        linesep = "",
        col.names = c("Variable",
                      "mean", "sd", "min", "max", 
                      "mean", "sd", "min", "max", 
                      "Difference", "p-value")) %>%
  group_rows("Land Characteristics", 1, 4) %>%
  group_rows("Lease Characteristics", 5, 8) %>%
  group_rows("Lease Outcomes", 9, 13) %>%  
  add_header_above(n_header)

# NOTES that we should add in the draft
# Bonus, Revenue and Surplus are all reported in 1000 nominal dollars per acre.
# Output is reported in barrels of oil equivalent per acre.  Wells are reported
# per 1000 acres.  Acres are reported in 1000s

write(summary_stats_table,
      file = file.path(tdir, "summary_stats_by_type.tex"),
      append = FALSE,
      sep = " ")

# COMBINE AND MAKE IT INTO A TABLE, smaller version for presentation
summary_stats_table <-
  inner_join(sumvar_stats, sumvar_tstats, by = "variable") %>%
  select(variable,
         mean_0, sd_0, min_0, max_0,
         mean_1, sd_1, min_1, max_1) %>%
  mutate(temp = case_when(
           variable == "Acres" ~ 1,
           variable == "ShapeQuality" ~ 2,
           variable == "MultiPolygon" ~ 3,
           variable == "ShaleThickness" ~ 4,           
           variable == "Bonus" ~ 5,
           variable == "Term" ~ 6,
           variable == "RentalsContract" ~ 7,           
           variable == "RoyaltyRate" ~ 8,
           variable == "Drilled" ~ 9,
           variable == "Output" ~ 10,
           variable == "LeaseRevenue" ~ 11,
           variable == "RentalsPaid" ~ 12,
           variable == "SellerRevenue" ~ 13,
           TRUE ~ NA_real_)) %>%
  arrange(temp) %>%
  select(-temp) %>%
  kable(digits = 2,
        format = "latex",
        booktabs = TRUE,
        linesep = "",
        col.names = c("Variable",
                      "mean", "sd", "min", "max", 
                      "mean", "sd", "min", "max")) %>%
  group_rows("Land Characteristics", 1, 4) %>%
  group_rows("Lease Characteristics", 5, 8) %>%
  group_rows("Lease Outcomes", 9, 13) %>%  
  add_header_above(n_header)

# NOTES that we should add in the draft
# Bonus, Revenue and Surplus are all reported in 1000 nominal dollars per acre.
# Output is reported in barrels of oil equivalent per acre.  Wells are reported
# per 1000 acres.  Acres are reported in 1000s

write(summary_stats_table,
      file = file.path(tdir, "summary_stats_by_type_prez.tex"),
      append = FALSE,
      sep = " ")



# DATA CONSTRUCTION WATERFALL -----------------------------------------------
# create dictionary of variables for the table, filtering conditions for 
# lease_data, and the order we want to display them in
waterfall_vars <- 
  list("Not on Shale" = "OnShale",
    "Missing Value" = "!flag_missing", 
    "Less Than 10 or Greater Than 1,000 Acres" = "!flag_size", 
    "Gross and Net Acreage Differ" = "!flag_netgross", 
    "Undivided Interest" = "!flag_undivided", 
    "Term Less Than 1 Year" = "!flag_term", 
    "Cancelled or Withdrawn" = "!flag_status", 
    "Negotiated State Lease" = "!flag_statenegotiate",
    "Auctioned RAL Lease" = "!flag_ralauction",     
    "Lessee Owns RAL Surface" = "!flag_ralownlease", 
    "Final Sample" = "InSample") %>%
  map2_df(., names(.), c) %>%
  t() %>%
  as_tibble() %>%
  setNames(c("VarFilter", "DropReason")) %>%
  mutate(Order = row_number() + 1)

# first filter to RAL and State leases and count by Type
lease_data_filtered <- 
  lease_data %>%
  filter(Type %in% c("RAL", "STATE"))

output <- 
  lease_data_filtered %>%
  count(Auction) %>%
  mutate(DropReason = "All Leases", 
         Order = 1)

# for each filtering condition, apply the filter to lease_data, 
  # then record how many leases of each type, and pass on the filtered dataset
  # to the next condition
for(i in 1:nrow(waterfall_vars)){
  lease_data_filtered <-
    lease_data_filtered %>%
    filter_(waterfall_vars$VarFilter[i])

  output <-
    lease_data_filtered %>%
    count(Auction) %>%
    mutate(DropReason = waterfall_vars$DropReason[i], 
      Order = waterfall_vars$Order[i]) %>%
    bind_rows(output)
  print(output)
}

# shape the final tallies and output using kable
waterfall <-
  output %>%
  spread(Auction, n) %>%
  arrange(Order) %>%
  mutate(NData = case_when(DropReason == "All Leases" ~ "All Leases", 
    DropReason == "Final Sample" ~ "Final Sample", 
    TRUE ~ "")) %>%
  select(NData, DropReason, `0`, `1`)  %>%
  mutate(DropReason = if_else(DropReason %in% c("All Leases", "Final Sample"), 
    "", DropReason)) %>%
  kable(digits = 2,
        format = "latex",
        booktabs = TRUE,
        linesep = "",
        col.names = c("", "Drop Reason", "Negotiation", "Auction"), 
        format.args = list(big.mark = ','))


write(waterfall,
      file = file.path(tdir, "summary_data_construction.tex"),
      append = FALSE,
      sep = " ")
 
# ADDITIONAL STATS FOR PAPER =================================================
final_leases %>%
    filter(InSample, !Censored) %>% 
    select(Drilled,DBOEPerAcre) %>% summary()

td <- final_leases %>%
    filter(InSample, !Censored) %>% 
    filter(Drilled ==1) 

pctiles <- quantile(td$DBOEPerAcre, probs = seq(0, 1, by= 0.1)) # decile

pctiles

diff9010 = log(pctiles[10]) - log(pctiles[2])

diff9010
