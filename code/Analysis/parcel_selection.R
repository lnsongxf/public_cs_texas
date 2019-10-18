# Created by Yixin Sun August 2018
# editing by Thom and Rich in February 2019
# read in parcel data and lease data, and match them

# BASIC TEXAS SETUP ===========================================================
root <- getwd()
while(basename(root) != "texas") {
  root <- dirname(root)
}

library(sf)
library(tidyverse)
library(lubridate)

source(file.path(root, "code", "paths.R"))
source(file.path(root, "code/texas_constants.R"))


# LOAD DATA ===================================================================
load(file.path(gen, "state_lease_parcel.Rda"))
load(file.path(gen, "state_parcels_map.Rda"))
load(file.path(gen, "final_leases.Rda"))


# CLEAN IT UP =================================================================

# want to drop any parcel that was CONTINUOUSLY leased between
# parcel_start_date and parcel_end_date. This means we drop parcels that are
# labelled as "active" now and has an effective date before parcel_start_date
cont_leased <- 
  state_lease_parcel %>%
  st_set_geometry(NULL) %>%
  mutate(ParcelLeftCensored = Active & Effective_Date < parcel_start_date) %>%
  group_by(ParcelID) %>%
  summarise(ParcelLeftCensored = max(ParcelLeftCensored))
  
# filter to just the dates we want
state_lease_sample <-
  state_lease_parcel %>%
  filter(Effective_Date >= parcel_start_date,
         Effective_Date <= parcel_end_date) %>%
  rename(AreaLeased = area_leased,
         FracLeased = frac_leased)

# COMPUTE PARCEL LEVEL INFORMATION ============================================
# total amount of parcel ever leased in this time period
# percent of parcel ever leased
# when/if the parcel is ever drilled
# when/if the parcel ever produces
# the lease number for the first lease we see on a parcel and the 
# largest / longest lease we see on a parcel


lease_stats <-
  state_lease_sample %>%
  left_join(select(final_leases, Lease_Number, Expire_Date)) %>%
  arrange(ParcelID, Effective_Date, Expire_Date, Lease_Number) %>%
  group_by(ParcelID) %>%
  mutate(NLeases = sum(FracLeased),
         GrossAreaLeased = sum(AreaLeased),
         MaxFracLeased = max(FracLeased)) %>%
  filter(row_number() == 1) %>%
  ungroup %>%
  rename(FirstLeaseNumber = Lease_Number,
         FirstEffectiveDate = Effective_Date,
         FirstLeaseArea = AreaLeased) %>%
  mutate(FirstLeaseFrac = if_else(FirstLeaseArea / ParcelAcres > 1, 1, 
                                  FirstLeaseArea / ParcelAcres)) %>%
  st_set_geometry(NULL) %>%
  select(ParcelID, NLeases, GrossAreaLeased, MaxFracLeased,
         FirstLeaseNumber, FirstEffectiveDate, FirstLeaseArea, FirstLeaseFrac)

  
# join these stats with the parcel map and create an InSample flag which flags 
# PSF leases, overlying shale formations, that aren't smaller than 10 acres,
# with straight forward LandType
final_parcels <- 
  state_parcels_map %>%
  st_set_geometry(NULL) %>%
  rename(HullAcres = Hull_Acres,
         AlreadyLeased = leased05,
         CentLat = Cent_Lat,
         CentLong = Cent_Long) %>%
  left_join(lease_stats) %>%
  left_join(cont_leased) %>%
  mutate(Leased = if_else(!is.na(MaxFracLeased), 1, 0),
         ShapeQuality = ParcelAcres / HullAcres) %>%
  mutate_at(vars(Grid5, Grid10, Grid20), as.factor) %>%
  replace_na(list(FirstEffectiveDate = ymd(20181127),
                  Term = 0, 
                  ParcelLeftCensored = FALSE, 
                  FirstLeaseFrac = 0, 
                  MaxFracLeased = 0, 
                  FirstLeaseArea = 0,
                  GrossAreaLeased = 0, 
                  NLeases = 0)) %>%
  mutate(InSample =
           (ParcelAcres > 10) &
           (OnShale == TRUE) &
           LandType %in% c("04", "07", "08", "15", "16"),
         Auction = if_else(LandType %in% c("04", "15", "16"), 1, 0),
         DevIndex =
           0.10 * Developed_OS +
           0.30 * Developed_LI +
           0.65 * Developed_MI +
           0.90 * Developed_HI) %>%
  mutate_at(vars(ShaleThickness, ParcelAcres,
                 DistWaterbodies, DistRiverStreams, DistRoads),
            ~ . / 1000) %>%
  mutate_at(vars(DevIndex, Shrub_Scrub, CoverCultivated, CoverForest),
            ~ . * 100) %>%
  as_tibble

save(final_parcels, file = file.path(gen, "final_parcels.Rda"))
