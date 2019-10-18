# Created by Yixin Sun May 2018
# want to create a map of state lands in texas
# let's keep all parcel - lease matches to make productivity calculations
# easier, and flag major events on the parcel
# 1. Combine ut lands, PSF + state agency lands
# 2. Intersect state lands with lease data
# 3. For each state parcel
  # - flag for what the land is (UT/RAL/State)
  # - if it's ever leased
  # - when it's leased
  # - when it's first drilled
  # - when it first starts producing
  # - whether the land is "leasable" in 2005

#load packages we need
library(sf)
library(tidyverse)
library(lubridate)
library(fst)

root = getwd()
while(basename(root) != "texas") {
  root <- dirname(root)
}
source(file.path(root, "code", "paths.R"))
source(file.path(root, "code", "functions", "utils.R"))
source(file.path(root, "code", "texas_constants.R"))

int_lb <- .05
temp <- Sys.time()

# ==============================================================================
# load in PSF lands
# ==============================================================================
# load in psf shapefile and filter out parcels that were not a part of the
# ORIGINAL PSF allocation.  at this point, current geographic identifiers do
# not uniquely identify the data, so also add in centroids as unique IDs
load(file.path(shape, "psf.Rda"))
state_parcels_map <-
  psf %>%
  filter(str_detect(LandType, regex("04|07|08|15|16")),
         ControlNum != " ") %>%
  arrange(ControlNum, desc(Polygon_Acres)) %>%
  group_by(ControlNum) %>%
  mutate(ParcelID = paste(ControlNum,
                          str_pad(as.character(row_number()),
                                  2,
                                  side = "left",
                                  pad = "0"),
                          sep = "-")) %>%
  ungroup %>%
  mutate(Type = case_when(LandType == "04" ~ "StateFull",
                          LandType == "07" ~ "RAL",
                          LandType == "08" ~ "Free",
                          LandType == "15" ~ "StateFull",
                          TRUE ~ "StatePartial")) %>%
  select(ParcelID, ControlNum, LandType, Type, ParcelAcres = Polygon_Acres) %>%
  st_transform(main_crs)

# =============================================================================
# load in raw state leases
# =============================================================================
# later, we want to flag whether a piece of land is leasable in 2005. We use
# whether or not a lease is active to help identify this, thus we use
# raw GLO lease data to accurately flag which leases are active and which are
# inactive
inactive <-
  file.path(raw_lease, "Inactive/InActive_OilGas_Leases.shp") %>%
  read_sf %>%
  mutate(Lease_Status = "INACTIVE")

active <-
  file.path(raw_lease, "Active/ActiveLeases.shp") %>%
  read_sf %>%
  mutate(Lease_Status = "ACTIVE") %>%
  anti_join(select(as.data.frame(inactive), LEASE_NUMB))

leases <-
  rbind(active, inactive) %>%
  st_transform(main_crs) %>%
  select(Lease_Number = LEASE_NUMB,
         Effective_Date = EFFECTIVE_,
         Expire_Date = PRIMARY_TE,
         Lease_Status,
         geometry)

# indicator (first two digits of control number)
control_numbers <-
  file.path(raw_lease, "LeaseLandFile.csv") %>%
  read_csv %>%
  mutate(LandTypeNo = substr(Control, 1, 2)) %>%
  group_by(StateLease) %>%
  summarize(HasPSF = max(LandTypeNo %in% c("04", "07", "08", "15", "16"))) %>%
  rename(Lease_Number = StateLease)

leases <-
  leases %>%
  left_join(control_numbers) %>%
  replace_na(list(HasPSF = 0)) %>%
  filter(HasPSF == 1) %>%
  select(-HasPSF) %>%
  group_by(Lease_Number) %>%
  summarise(Effective_Date = unique(Effective_Date),
            Expire_Date = unique(Expire_Date),
            Lease_Status = unique(Lease_Status)) %>%
  mutate(LeaseArea = as.numeric(st_area(.)),
         LeaseAcres = LeaseArea * m2_to_acre) %>%
  st_buffer(0)


# =============================================================================
# intersect state parcels and leases
# =============================================================================
state_lease_parcel <-
  st_intersection(leases, state_parcels_map) %>%
  mutate(area_leased = as.numeric(st_area(.)) * m2_to_acre,
         frac_leased = area_leased / ParcelAcres) %>%
  filter(frac_leased > int_lb) %>%
  mutate(Active = Lease_Status == "ACTIVE") %>%
  select(Lease_Number, Effective_Date, LeaseAcres, ParcelID, area_leased,
    frac_leased, LandType, Type, ParcelAcres, Active)

check <-
  state_lease_parcel %>%
  group_by(ParcelID, Lease_Number) %>%
  filter(n() > 1)

if(nrow(check) > 0){
  stop("check uniqueness of parcel-lease matches")
}

# =============================================================================
# leasability in 2005
# =============================================================================
# 2 ways to find land that's already leased
# 1. take set of wells that are producing in dec 2004 (not start producing,
# but have a first prod month on or before, last prod month after),
# intersect parcels with those wells
# 2. is there a lease in the parcel that is active? lease term spans 1/1/2005?

# loads in "key" which tells us which rows in the giant flatfile fst datasets
# we actually want read into memory
load(file.path(int, "index_key.Rda"))

tx_inds <-
  key %>%
  filter(state == "TX")

prod <-
  file.path(int, "pden_desc_arranged.fst") %>%
  read_fst(from = tx_inds$desc_begin,
           to = tx_inds$desc_end,
           as.data.table = T,
           columns = c("api_no", "first_prod_date", "last_prod_date",
            "longitude", "latitude")) %>%
  mutate(last_prod_date = if_else(is.na(last_prod_date),
                                  ymd(20180101), last_prod_date)) %>%
  filter(first_prod_date < cutoff_date, last_prod_date >= cutoff_date) %>%
  select(api_no, longitude, latitude) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = di_crs) %>%
  st_transform(main_crs)

# 1. intersect wells with parcels
parcel_well <-
  st_join(state_parcels_map, prod, join = st_covers, left = FALSE) %>%
  st_set_geometry(NULL) %>%
  select(ParcelID) %>%
  mutate(leased05 = TRUE)

# 2. find active leases whose term spans 1/1/2005
leases_active <-
  leases %>%
  st_set_geometry(NULL) %>%
  filter(Effective_Date <= cutoff_date, Expire_Date >= cutoff_date) %>%
  left_join(select(as.data.frame(state_lease_parcel),
                   Lease_Number, ParcelID)) %>%
  select(ParcelID) %>%
  mutate(leased05 = TRUE)

leased05 <-
  bind_rows(parcel_well, leases_active) %>%
  group_by(ParcelID) %>%
  summarize(leased05 = max(leased05) == TRUE) %>%
  filter(!is.na(ParcelID))

state_parcels_map <-
  state_parcels_map %>%
  left_join(leased05) %>%
  replace_na(list(leased05 = FALSE)) %>%
  st_buffer(0)

# =============================================================================
# add in grids
# =============================================================================
load(file.path(shape, "texas_grid5.Rda"))
load(file.path(shape, "texas_grid10.Rda"))
load(file.path(shape, "texas_grid20.Rda"))

grid_overlap5 <-
  select(state_parcels_map, ParcelID) %>%
  st_intersection(texas_grid5) %>%
  st_cast("MULTIPOLYGON") %>%
  mutate(area = st_area(.)) %>%
  group_by(ParcelID) %>%
  arrange(-area) %>%
  filter(row_number() == 1) %>%
  as.data.frame() %>%
  select(ParcelID, Grid5 = GridID)

grid_overlap10 <-
  select(state_parcels_map, ParcelID) %>%
  st_intersection(texas_grid10) %>%
  st_cast("MULTIPOLYGON") %>%
  mutate(area = st_area(.)) %>%
  group_by(ParcelID) %>%
  arrange(-area) %>%
  filter(row_number() == 1) %>%
  as.data.frame() %>%
  select(ParcelID, Grid10 = GridID)

grid_overlap20 <-
  select(state_parcels_map, ParcelID) %>%
  st_intersection(texas_grid20) %>%
  st_cast("MULTIPOLYGON") %>%
  mutate(area = st_area(.)) %>%
  group_by(ParcelID) %>%
  arrange(-area) %>%
  filter(row_number() == 1) %>%
  as.data.frame() %>%
  select(ParcelID, Grid20 = GridID)

state_parcels_map <-
  state_parcels_map %>%
  left_join(grid_overlap5) %>%
  left_join(grid_overlap10) %>%
  left_join(grid_overlap20) %>%
  mutate_if(is.character, toupper)


#==============================================================================
# add in shale play
#==============================================================================
# use function from utils.R that finds closest shale
state_parcels_map <-
  closest_shale(state_parcels_map)

# add in shale isopach values for eagle ford and permian shale parcels
load(file.path(shape, "isopach_permian_interpolate.Rda"))
load(file.path(shape, "isopach_eagle_ford_interpolate.Rda"))

parcels_isopach_permian <-
  raster::extract(isopach_permian_interpolate,
                  state_parcels_map,
                  fun = mean)

parcels_isopach_eagle_ford <-
  raster::extract(isopach_eagle_ford_interpolate,
                  state_parcels_map,
                  fun = mean)

state_parcels_map <-
  state_parcels_map %>%
  mutate(ShaleThicknessPermian = parcels_isopach_permian,
         ShaleThicknessEagleFord = parcels_isopach_eagle_ford) %>%
  mutate(ShaleThickness = case_when(
           !is.na(ShaleThicknessPermian) ~ ShaleThicknessPermian,
           !is.na(ShaleThicknessEagleFord) ~ ShaleThicknessEagleFord,
           TRUE ~ NA_real_)) %>%
  select(-ShaleThicknessPermian, -ShaleThicknessEagleFord)


# =============================================================================
# Add undivided flag
# =============================================================================
load(file.path(gen, "leases_state.Rda"))
undivided <-
  leases_state %>%
  st_set_geometry(NULL) %>%
  group_by(Lease_Number) %>%
  summarise(Undivided = Undivided | Undivided_Geom)

state_lease_parcel <-
  state_lease_parcel %>%
  left_join(undivided) %>%
  replace_na(list(Undivided = FALSE)) %>%
  mutate_if(is.character, toupper) %>%
  mutate(LeaseShare = area_leased / LeaseAcres)

# =============================================================================
# add distance to infrastructure
# =============================================================================
source(file.path(root, "code/Data_Cleaning/infrastructure_parcels.R"))

infrastructure_parcels <-
  infrastructure_parcels %>%
  ungroup

state_parcels_map <-
  left_join(state_parcels_map, infrastructure_parcels)

# =============================================================================
# add shape quality
# =============================================================================
hull <-
  state_parcels_map %>%
  st_convex_hull() %>%
  mutate(Hull_Acres = as.numeric(st_area(geometry) * m2_to_acre)) %>%
  as.data.frame() %>%
  select(ParcelID, Hull_Acres)

state_parcels_map <-
  state_parcels_map %>%
  left_join(hull)

# =============================================================================
# add in landcover data and add centroids
# =============================================================================
source(file.path(root, "code/Data_Cleaning/landcover_parcels.R"))

state_parcels_map <-
  state_parcels_map %>%
  cbind(st_coordinates(st_centroid(.))) %>%
  rename(Cent_Long = X, Cent_Lat = Y) %>%
  left_join(landcover_parcel)

save(state_lease_parcel, file = file.path(gen, "state_lease_parcel.Rda"))
save(state_parcels_map, file = file.path(gen, "state_parcels_map.Rda"))
