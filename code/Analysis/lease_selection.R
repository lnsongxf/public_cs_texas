## load the final input data for analysis and save a definitive sample

#===========================================================================
# BASIC TEXAS SETUP
#===========================================================================
root = getwd()
while(basename(root) != "texas") {
  root = dirname(root)
}

library(tidyverse)
library(sf)
library(lubridate)

source(file.path(root, "code/paths.R"))
source(file.path(root, "code/functions/utils.R"))
source(file.path(root, "code/texas_constants.R"))

#===========================================================================
# SELECT LEASES AND COMPUTE OUTPUT/REVENUE AT LEASE LEVEL
#===========================================================================
# load coversheets so we can have a dummy variable for whether GLO improved
# on an RAL surface owner's negotiation
load(file.path(gen, "coversheets.Rda"))
falsena <- function(x) {
  if_else(is.na(x), FALSE, x)
}

ral_improved <-
  coversheets %>%
  filter(is.na(No_Review), is.na(No_Pdf)) %>%
  filter(BPA_Offered > 0, Royalty_Offered > 0, Terms_Offered > 0) %>%
  mutate(Royalty_Offered = if_else(Royalty_Offered > 1,
                                   Royalty_Offered / 100,
                                   Royalty_Offered)) %>%
  mutate(ImprovedBonus = BPA_Offered < BPA_Rec,
         ImprovedRoyalty = Royalty_Offered < Royalty_Rec,
         ImprovedTerms = Terms_Offered > Terms_Rec) %>%
  mutate_at(vars(contains("Improved")), falsena) %>%
  mutate(Improved = ImprovedBonus | ImprovedRoyalty | ImprovedTerms) %>%
  select(Lease_Number, contains("Improved")) %>%
  mutate(Lease_Number = paste0("MF", Lease_Number))

# load state lease data: tabulate presence of multi-polygon leases and keep
# leases that are unambiguously on appropriate land types 
load(file.path(gen, "leases_state.Rda"))
leases_state_multipolygon <-
  leases_state %>%
  st_cast("POLYGON") %>%
  st_set_geometry(NULL) %>%
  group_by(Lease_Number) %>%
  summarize(MultiPolygon = (n() > 1) * 1)

leases_state <-
  leases_state %>%
  st_set_geometry(NULL) %>%
  as_tibble %>%
  filter(Effective_Date >= cutoff_date,
         Type %in% c("RAL", "FREE", "STATEFULL")) %>%
  mutate(Type = if_else(Type == "STATEFULL", "STATE", Type)) %>%
  left_join(leases_state_multipolygon)


# load GLO revenue data
load(file.path(gen, "glo_production_revenues.Rda"))

glo_production_revenues <-
  glo_production_revenues %>%
  filter((OilRoyaltyRevenue + GasRoyaltyRevenue) > 0)

# load RAL addenda
ral_addenda <-
  file.path(raw, "addenda.csv") %>%
  read_csv %>%
  mutate(Lease_Number = paste("MF", mf_n, sep = "")) %>%
  select(Lease_Number,
         c01:c16,
         AddendaPages = pages_rounded,
         AddendaClauses = clauses_rounded)

## do some cleaning
# instead of dropping observations before 2014, we flag them so that its easy
# to do lease-only regressions that go through 2016
lease_data <-
  leases_state %>%
  mutate(Term = as.integer(Term),
         ImpliedTerm = if_else(!is.na(Term) & Term > 0,
                               Term,
                               makemonths(Effective_Date, Expire_Date)),
         RoyaltyRate = case_when(
           !is.na(Oil_Royalty) & !is.na(Gas_Royalty) ~
             0.5 * (Oil_Royalty + Gas_Royalty),
           is.na(Oil_Royalty) & !is.na(Gas_Royalty) ~
             Gas_Royalty,
           !is.na(Oil_Royalty) & is.na(Gas_Royalty) ~
             Oil_Royalty,
           TRUE ~ NA_real_),
         Year = year(Effective_Date),
         Quarter = quarter(Effective_Date),
         Grid5 = lGrid5,
         Grid10 = lGrid10,
         Grid20 = lGrid20,
         YearQtr = paste(Year, Quarter, sep = "-"),
         CountyYr = paste(County, Year, sep = "-"),
         Grid5Yr = paste(lGrid5, Year, sep = "-"),
         Grid10Yr = paste(lGrid10, Year, sep = "-"),
         Grid20Yr = paste(lGrid20, Year, sep = "-"),
         CountyYrQtr = paste(County, Year, Quarter, sep = "-"),
         Grid5YrQtr = paste(lGrid5, Year, Quarter, sep = "-"),
         Grid10YrQtr = paste(lGrid10, Year, Quarter, sep = "-"),
         Grid20YrQtr = paste(lGrid20, Year, Quarter, sep = "-"),
         EffDate = as.numeric(Effective_Date),
         RAL = Type == "RAL",
         ShapeQuality = Polygon_Acres / Hull_Acres,
         Censored =
           Effective_Date %m+% months(ImpliedTerm) > LastProductionDate) %>%
  left_join(ral_improved, by = "Lease_Number") %>%
  left_join(glo_production_revenues, by = "Lease_Number") %>%
  left_join(ral_addenda, by = "Lease_Number") %>%  
  replace_na(list(ImprovedBonus = FALSE,
                  ImprovedRoyalty = FALSE,
                  ImprovedTerms = FALSE,
                  Improved = FALSE,
                  HasEarlyProduction = 0,
                  FirstRoyaltyDate = make_date(2099, 12, 31),
                  OilRoyaltyRevenue = 0,
                  GasRoyaltyRevenue = 0,
                  OilLeaseRevenue = 0,
                  GasLeaseRevenue = 0,
                  OilLeaseProduction = 0,
                  GasLeaseProduction = 0,
                  Undivided = FALSE,
                  Undivided_Geom = FALSE,
                  LandTypeNo = "OTHER",
                  Rental_Year2 = 0,
                  Rental_Year3 = 0,
                  Rental_Year4 = 0,
                  Rental_Year5 = 0,
                  Rental_Year6 = 0,
                  Rental_Year7 = 0,
                  Rental_Year8 = 0,
                  Rental_Year9 = 0,
                  Rental_Year10 = 0,
                  Rental_Total = 0,
                  c01 = 0,
                  c02 = 0,
                  c03 = 0,
                  c04 = 0,
                  c05 = 0,
                  c06 = 0,
                  c07 = 0,
                  c08 = 0,
                  c09 = 0,
                  c10 = 0,
                  c11 = 0,
                  c12 = 0,
                  c13 = 0,
                  c14 = 0,
                  c15 = 0,
                  c16 = 0,
                  AddendaPages = 0,
                  AddendaClauses = 0)) %>%
  mutate(BonusPerAcre = Bonus / Original_Gross,
         RoyaltyRevenuePerAcre =
           (OilRoyaltyRevenue + GasRoyaltyRevenue) / Original_Gross,
         LeaseRevenuePerAcre =
           (OilLeaseRevenue + GasLeaseRevenue) / Original_Gross,
         DBOEPerAcre = (OilLeaseProduction + GasLeaseProduction / mcf_to_boe) /
           Original_Gross,
         SellerRevenuePerAcre = BonusPerAcre + RoyaltyRevenuePerAcre,
         Drilled = if_else(!is.na(HasEarlyProduction) &
                           pmax(OilRoyaltyRevenue, GasRoyaltyRevenue) > 0,
                           1,
                           0)) %>%
  mutate(flag_missing = if_else(is.na(Bonus) |
                                is.na(RoyaltyRate) |
                                is.na(ImpliedTerm) |
                                is.na(Original_Gross) |
                                is.na(Original_Net) |
                                is.na(County) |
                                is.na(Lessee),
                                TRUE,
                                FALSE)) %>%
  mutate(flag_missing2 = case_when(
    Type == "FREE" & is.na(Censored) ~ TRUE,
    Type == "FREE" & !is.na(Censored) ~ FALSE,
    TRUE ~ flag_missing)) %>%
  mutate(flag_missing = if_else(Type == "FREE",
                                flag_missing2,
                                flag_missing)) %>%
  select(-flag_missing2) %>%
  mutate(flag_size = if_else(Original_Gross < 10 | Original_Gross > 1000,
                             TRUE,
                             FALSE,
                             missing = FALSE)) %>% 
  mutate(flag_netgross = if_else((Original_Net/Original_Gross) < .99 |
                                 (Original_Net/Original_Gross) > 1.01,
                                 TRUE,
                                 FALSE,
                                 missing = FALSE)) %>%
  mutate(flag_term = if_else(ImpliedTerm < 12,
                             TRUE,
                             FALSE,
                             missing = FALSE)) %>%
  mutate(flag_status = if_else(Lease_Status %in% c("CANCELLED", "WITHDRAWN"),
                               TRUE,
                               FALSE)) %>%
  mutate(flag_statenegotiate = if_else(Type == "STATE" & !Auction,
                                       TRUE,
                                       FALSE)) %>%
  mutate(flag_ralauction = if_else(Type == "RAL" & Auction,
                                   TRUE,
                                   FALSE)) %>%
  mutate(flag_undivided = if_else(Undivided |
                                  Undivided_Geom,
                                  TRUE,
                                  FALSE)) %>%
  mutate(flag_ralownlease = if_else(Type == "RAL" &
                                    !Auction &
                                    str_detect(Lessor,
                                               regex("state of texas",
                                                     ignore_case=T)),
                                    TRUE,
                                    FALSE,
                                    missing = FALSE)) %>%
  select(Lease_Number, Type, Lease_Status, Effective_Date, Expire_Date,
         Dropped, Undivided, Undivided_Geom, Acres = Original_Gross,
         MultiPolygon, Bonus, Term = ImpliedTerm, RoyaltyRate,
         starts_with("Rental"), Lessee, Lessor,
         LesseeAlias = Lessee_alias, Assignee, AssigneeAlias = Assignee_alias,
         Firm, AssignDate, County, CentLat = Cent_Lat, CentLong = Cent_Long,
         OnShale, ShaleDist, ShalePlay = Shale_play, EffDate, ShapeQuality,
         Auction, RAL, Censored, DistRoads, DistRiverStreams, DistWaterbodies,
         Year, YearQtr, Grid10, Grid20, CountyYr, CountyYrQtr, Grid10Yr,
         Grid20Yr, Grid10YrQtr, Grid20YrQtr, BonusPerAcre, Drilled,
         FirstRoyaltyDate, RoyaltyRevenuePerAcre, LeaseRevenuePerAcre,
         SellerRevenuePerAcre, DBOEPerAcre, starts_with("flag_"), 
         starts_with("Improved"), c01:c16, starts_with("NParcels"),
         AddendaPages, AddendaClauses, ShaleThickness) %>%
  mutate(InSample =
           Type %in% c("RAL", "STATE") &
           OnShale &
           !flag_missing &
           !flag_size &
           !flag_netgross &
           !flag_term &
           !flag_statenegotiate &
           !flag_ralauction &           
           !flag_undivided &
           !flag_ralownlease &
           !flag_status) %>%
  mutate(InSampleFree =
           Type == "FREE" &
           OnShale &
           !flag_missing &
           !flag_size &
           !flag_netgross &
           !flag_term &
           !flag_statenegotiate &
           !flag_ralauction &
           !flag_undivided &
           !flag_ralownlease &
           !flag_status) %>%           
  mutate(drop_reason = case_when(
           !(Type %in% c("RAL", "STATE")) ~ "wrongtype1",
           flag_missing ~ "missing",
           flag_size ~ "size",
           flag_term ~ "term",
           flag_netgross ~ "netgross",
           flag_undivided ~ "undivided",
           flag_ralownlease ~ "ralown",
           flag_status ~ "status",
           flag_statenegotiate ~ "statenegotiate",
           flag_ralauction ~ "ralauction",
           !OnShale ~ "wrongplace",
           TRUE ~ "notdropped"))

# load update lease status information and characterize which delay rentals,
# if any, were paid.  Rules:
# Active: has paid anything up through Lease Status Date
# Expired: paid everything
# Producing: paid everything before Lease Status Date
# Terminated: paid everything before Lease Status Date
# Released: paid everything before Lease Status Date
# Held by Production: paid everything before Lease Status Date
# Unitized: paid everything before Lease Status Date

# except for Expired, relevant date is the earliest non-missing value among
# Lease_Status-date, FirstProdDate, and FirstObservedProd
# Expired pays everything

load(file.path(gen, "lease_statuses.Rda"))
delays_paid <-
  lease_data %>%
  filter(InSample) %>%
  select(Lease_Number, Effective_Date, FirstRoyaltyDate,
         starts_with("Rental_Year")) %>%
  left_join(lease_statuses)

# construct earliest no-rentals date
no_rentals_date <-
  delays_paid %>%
  select(Lease_Number, Effective_Date,
         Lease_Status_Date, FirstProdDate, FirstRoyaltyDate) %>%
  gather(DateType, Date, -Lease_Number, -Effective_Date) %>%
  filter(!is.na(Date)) %>%
  arrange(Lease_Number, Date) %>%
  mutate(Date = if_else(Date < Effective_Date, Effective_Date, Date)) %>%
  group_by(Lease_Number) %>%
  filter(row_number() == 1) %>%
  ungroup %>%
  select(Lease_Number, NoRentalsDate = Date)

delays_paid <-
  delays_paid %>%
  left_join(no_rentals_date) %>%
  select(Lease_Number, Lease_Status, Effective_Date, NoRentalsDate,
         starts_with("Rental_Year")) %>%
  gather(Rental_Year, Rental_Value,
         -Lease_Number, -Lease_Status, -Effective_Date, -NoRentalsDate) %>%
  filter(!is.na(Rental_Value)) %>%
  extract(Rental_Year, "Rental_Year", regex = "(\\d)",
          remove = TRUE, convert = TRUE) %>%
  mutate(Rental_Date = Effective_Date %m+% months(12 * Rental_Year)) %>%
  filter(Rental_Date <= NoRentalsDate) %>%
  group_by(Lease_Number) %>%
  summarize(RentalsPaid = sum(Rental_Value)) %>%
  ungroup

lease_data <-
  lease_data %>%
  left_join(delays_paid) %>%
  replace_na(list(RentalsPaid = 0)) %>%
  mutate(RentalsPerAcre = Rental_Total / Acres,
         RentalsPaidPerAcre = RentalsPaid / Acres,
         SellerRevenuePerAcre = RentalsPaidPerAcre + SellerRevenuePerAcre)

# update all per acre figures to be in thousands
# save term in years
lease_data <-
  lease_data %>%
  mutate_at(vars(contains("PerAcre")), ~ . / 1000) %>%
  mutate(Term = Term / 12)

# flag lessee's who we think are brokers
broker_regex <-
  c("propert", " land", "servic", "interest", "assoc", "dwyer",
    "david william", "johnson clay", "clay johnson", "san luis", "larry",
    "mayne", "trimble", "soape", "doug", "hodges", "bellomy", "blair",
    "robbins", "veritas", "horn", "culpepper", "cannon", "wormser", "reeves",
    "basin ventures", "transcendent", "dawson", "weatherl", "san saba",
    "saye", "christensen", "hemus", "kimmeridge", "burleson", "champion",
    "cole stephen", "dixon", "mccabe", "wolcott", "armstrong", "ramsey",
    "alpine", "webb", "huber", "shorthorn", "cervus", "angelle", "pinnacle",
    "mike gaddy", "parke", "leascher", "harris william p", "landsmith",
    "perry") %>%
  paste(collapse = "|") %>%
  regex(ignore_case = T)

# other firms we might want to just write over:
# Pure: pretty sure they are just an LP in HENRY PETROLEUM, see here:
# https://www.oaoa.com/inthepipeline/article_5aac627c-1542-11e4-bc38-001a4bcf6878.html
# other firms to do more cleaning on:
# henry resources vs. henry petroleum

nonbroker_regex <-
  regex("westridge|horizon|neuhaus|alta mesa", ignore_case = T)

lease_data <-
  lease_data %>%
  mutate(broker = str_detect(LesseeAlias, broker_regex) &
           !str_detect(LesseeAlias, nonbroker_regex))

max2 <- function(v) {
  if(length(v) == 1) {
    v[1]
  } else {
    sort(v, decreasing=T)[2]
  }
}

# tabulate each broker's assignee frequencies with a variety of groupings
probfirms <- function(d, ..., TypeN = 1) {
  group_vars <- quos(...)
  assignees <-
    d %>%
    mutate(Auction = Type == "STATE" | Type == "UT") %>%
    filter(broker, !is.na(AssigneeAlias)) %>%
    group_by(LesseeAlias, !!!group_vars, AssigneeAlias) %>%
    summarize(n=n()) %>%
    ungroup %>%
    arrange(LesseeAlias, !!! group_vars, desc(n)) %>%
    group_by(LesseeAlias, !!! group_vars) %>%
    mutate(hasmode = (max(n) > max2(n)) | n() == 1) %>%
    filter(row_number() == 1) %>%
    ungroup %>%
    filter(hasmode) %>%
    mutate(ProbFirmType = TypeN) %>%
    select(LesseeAlias, !!! group_vars,
           ProbFirm = AssigneeAlias, ProbFirmType)

  probfirms_d <-
    d %>% 
    mutate(Auction = Type == "STATE" | Type == "UT") %>%
    inner_join(assignees) %>%
    select(Lease_Number, ProbFirm, ProbFirmType)
  
  return(list(probfirms_d, assignees))
}

probfirms_lease_data <-
  bind_rows(probfirms(lease_data, County, Year, Auction, TypeN = 1)[[1]],
            probfirms(lease_data, County, Auction, TypeN = 2)[[1]],
            probfirms(lease_data, County, Year, TypeN = 3)[[1]],
            probfirms(lease_data, County, TypeN = 4)[[1]],
            probfirms(lease_data, Auction, TypeN = 5)[[1]],
            probfirms(lease_data, TypeN = 6)[[1]]) %>%
  arrange(Lease_Number, ProbFirmType) %>%
  group_by(Lease_Number) %>%
  filter(row_number() == 1) %>%
  ungroup
  
lease_data <-
  lease_data %>%
  left_join(probfirms_lease_data) %>%
  mutate(NewFirm = case_when(
           !is.na(AssigneeAlias) ~ AssigneeAlias,
           !broker ~ LesseeAlias,
           !is.na(ProbFirm) ~ ProbFirm,
           TRUE ~ LesseeAlias)) %>%
  mutate(NewFirmType = case_when(
           !is.na(AssigneeAlias) ~ "Assignment",
           !broker ~ "Unassigned Non-Broker Lessee",
           !is.na(ProbFirm) ~ "Broker's Most Probable Assignee",
           TRUE ~ "Broker"))
           
# merge in land cover data
load(file.path(gen, "landcover_lease.Rda"))

final_leases <-
  lease_data %>%
  left_join(landcover_lease) %>%
  mutate(DevIndex =
           0.10 * Developed_OS +
           0.30 * Developed_LI +
           0.65 * Developed_MI +
           0.90 * Developed_HI) %>%
  mutate_at(vars(ShaleThickness, Acres,
                 DistWaterbodies, DistRiverStreams, DistRoads),
            ~ . / 1000) %>%
  mutate_at(vars(DevIndex, Shrub_Scrub, CoverCultivated, CoverForest),
            ~ . * 100)

save(final_leases, file = file.path(gen, "final_leases.Rda"))

# save the ProbFirms maps we compute above
probfirms_map <-
  bind_rows(probfirms(lease_data, County, Year, Auction, TypeN = 1)[[2]],
            probfirms(lease_data, County, Auction, TypeN = 2)[[2]],
            probfirms(lease_data, County, Year, TypeN = 3)[[2]],
            probfirms(lease_data, County, TypeN = 4)[[2]],
            probfirms(lease_data, Auction, TypeN = 5)[[2]],
            probfirms(lease_data, TypeN = 6)[[2]])

save(probfirms_map, file = file.path(gen, "probfirms_map.Rda"))
