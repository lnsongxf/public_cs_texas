# Created by Yixin Sun on April 4th, 2017 Cleaning GLO leases
# updated by Thom Covert in August 2019

root <- getwd()
while (basename(root) != "texas") {
  root <- dirname(root)
}

library(tidyverse)
library(sf)
library(fuzzyjoin)
library(lubridate)
library(igraph)


# =============================================================================
# source utils.R and file paths 
# =============================================================================
source(file.path(root, "code", "functions", "utils.R"))
source(file.path(root, "code", "paths.R"))
source(file.path(root, "code", "texas_constants.R"))

buffer_time <- as.integer((365.25 / 12) * 3)

# discount rate for delay rentals and extensions payments
discount_rate <- 1.1

geom_lb <- .5
time_lb <- 366

# =============================================================================
# Read in raw GLO data
# =============================================================================
# these are the last lease activity for each plot of land - does not
# include all change of hands this shapefile joins together the active
# and inactive leases - done in QGIS
active <- 
  file.path(raw_lease, "Active/ActiveLeases.shp") %>%
  read_sf

inactive <- 
  file.path(raw_lease, "Inactive/InActive_OilGas_Leases.shp") %>%
  read_sf

# If lease A expires, that land is leased again with lease B, and then leased
# one more time for lease C, then lease B will show up in the Inactive 
# shapefile, lease A will show up in the active shapefile, and lease A 
# is gone from the shapefiles.  To backfill, we use data in LeaseLandFile.csv,
# tblMineralLeaseAllInfo.csv, and the proprietary PSF parcel shapefile to find
# the GLO leases that we currently do not have lease shapefiles for.  See the
# readme file for "dropped leases" for more detail on this is constructed.
dropped_geoms <-
  file.path(raw_lease, "dropped_leases/dropped_leases.shp") %>%
  read_sf

dropped_fields <-
  file.path(raw_lease, "dropped_leases/dropped_leases_fields.csv") %>%  
  read_csv

dropped <-
  dropped_fields %>%
  inner_join(dropped_geoms) %>%
  st_as_sf %>%
  select(-id) %>%
  st_transform(main_crs)

# note: because the sf distinct method seems to be unreliable, perform distinct
# manually by a group_by operation on all non-geometry columns and just count
# row order, keeping the first entry
leases_state <-
  rbind(active, inactive) %>%
  st_transform(main_crs) %>% 
  select(Lease_Number = LEASE_NUMB, 
         Lease_Status = LEASE_STAT,
         Lease_Status_Date = LEASE_ST_1,
         Effective_Date = EFFECTIVE_, 
         Expire_Date = PRIMARY_TE, 
         Term = PRIMARY__1, 
         Bonus = BONUS_AMOU, 
         Original_Gross = ORIGINAL_G, 
         Original_Net = ORIGINAL_N, 
         Current_Gross = CURRENT_GR, 
         Current_Net = CURRENT_NE, 
         Lessee = ORIGINAL_L, 
         Lessor = LESSOR, 
         Undivided = UNDIVIDED_, 
         Oil_Royalty = LEASE_ROYA, 
         Gas_Royalty = LEASE_RO_1, 
         Condensate_Royalty = LEASE_RO_2, 
         Depth_From = DEPTH_FROM, 
         Depth_To = DEPTH_TO, geometry) %>%
  group_by(Lease_Number, Lease_Status, Lease_Status_Date, Effective_Date,
           Expire_Date, Term, Bonus, Original_Gross, Original_Net,
           Current_Gross, Current_Net, Lessee, Lessor, Undivided, Oil_Royalty,
           Gas_Royalty, Condensate_Royalty, Depth_From, Depth_To) %>%
  mutate(temp_id = row_number()) %>%
  filter(temp_id == 1) %>%
  ungroup %>%
  select(-temp_id) %>%
  mutate(Dropped = FALSE) %>%
  rbind(dropped) %>%
  filter(!is.na(Lease_Number))

# =============================================================================
# Identify Land Types
# =============================================================================
# what land types do we want to keep?
# stuff in the ORIGINAL PSF: only 04, 07, 08, 15, 16.
# for each lease, retain flags for presence of each land type and at the end
# we'll get rid of leases that don't hit any of these land types
lease_landtype <-
  file.path(raw_lease, "LeaseLandFile.csv") %>%
  read_csv %>%
  mutate(LandTypeNo = as.numeric(str_sub(Control, 1, 2))) %>%
  group_by(StateLease) %>%
  summarize(NParcels = n(),
            NParcels04 = sum(LandTypeNo == 4),
            NParcels07 = sum(LandTypeNo == 7),
            NParcels08 = sum(LandTypeNo == 8),
            NParcels15 = sum(LandTypeNo == 15),
            NParcels16 = sum(LandTypeNo == 16)) %>%
  rename(Lease_Number = StateLease)

# merge and create variable identifying State, RAL or Free Royalty Land
# in case of ambiguity, note as Ambiguous for now, and otherwise call NonPSF
leases_state <-
  leases_state %>%
  left_join(lease_landtype) %>%
  replace_na(list(NParcels = 0, NParcels04 = 0, NParcels07 = 0,
                  NParcels08 = 0, NParcels15 = 0, NParcels16 = 0)) %>%
  mutate(NParcelsPSF =
           NParcels04 + NParcels07 + NParcels08 + NParcels15 + NParcels16) %>%
  mutate(Type = case_when(
    NParcelsPSF == 0 ~ "NonPSF",
    NParcels07 > 0 &
      pmax(NParcels04, NParcels08, NParcels15, NParcels16) == 0 ~ "RAL",
    NParcels08 > 0 &
      pmax(NParcels04, NParcels07, NParcels15, NParcels16) == 0 ~ "Free",
    ((NParcels04 > 0) | (NParcels15 > 0)) &
      pmax(NParcels07, NParcels08, NParcels16) == 0 ~ "StateFull",
    NParcels16 > 0 &
      pmax(NParcels04,
           NParcels07,
           NParcels08,
           NParcels15) == 0 ~ "StatePartial",
    TRUE ~ "Ambiguous"))

# clean up royalties and use company dictionary to clean up lessee names
leases_state <- 
  leases_state %>% 
  mutate_at(vars(contains("Royalty")), as.numeric) %>%
  clean_names(Lessee) %>%
  mutate(Lessee_alias = if_else(!is.na(alias), alias, Lessee)) %>%
  select(-alias)

# =============================================================================
# FIX MISSING BONUSES
# =============================================================================
# fill in missing bonuses with bonuses found manually
glo_missing_bonus <- 
  file.path(int, "leases/missing_bonus.csv") %>%
  read_csv %>% 
  select(Lease_Number, MissingBonus = Bonus) %>% 
  mutate(MissingBonus = if_else(MissingBonus == 0, NA_real_, MissingBonus))

leases_state <- 
  leases_state %>% 
  left_join(glo_missing_bonus, by = "Lease_Number") %>%
  mutate(Bonus = if_else(!is.na(MissingBonus), MissingBonus, Bonus)) %>%
  select(-MissingBonus)

# =============================================================================
# FLAG LEASES THAT WERE ALLOCATED BY AUCTION
# =============================================================================
load(file.path(gen, "glo_bids.Rda"))

# filter so we only get winning bid
glo_bids <-
  glo_bids %>%
  filter(Winner == 1) %>%
  select(Lease_Number, Winner, Bonus_Bid = Bonus)

leases_state <- 
  leases_state %>%
  left_join(glo_bids) %>%
  mutate(Auction = if_else(is.na(Winner), 0, 1)) %>%
  select(-Winner, -Bonus_Bid)

# bonuses and royalties have to be multiplied by 2 for non-Auction (RAL) leases
leases_state <- 
  leases_state %>%
  mutate(Bonus = if_else(Type == "RAL" & Auction == 0,
                         Bonus * 2,
                         Bonus),
         Oil_Royalty = if_else(Type == "RAL" & Auction == 0,
                               Oil_Royalty * 2,
                               Oil_Royalty),
         Gas_Royalty = if_else(Type == "RAL" & Auction == 0,
                               Gas_Royalty * 2,
                               Gas_Royalty),
         Condensate_Royalty = if_else(Type == "RAL" & Auction == 0,
                                      Condensate_Royalty * 2,
                                      Condensate_Royalty))

# ==================================================================
  # Polygon Acreage vastly different from Gross acreage 
# ==================================================================
# checked 88 polygons by hand and deleted the unsavalgable ones
problem_geoms <- 
  file.path(int, "leases/geom_problems.csv") %>%
  read_csv() %>%
  filter(Correct == 0) %>%
  select(Lease_Number) 

leases_state <-
  leases_state %>% 
  anti_join(problem_geoms)


# =============================================================================
# OTHER RANDOM EDITS FOR GLO LEASES
# =============================================================================
# Make undivided into TRUE/FALSE insteade of YES/NO
leases_state <-
  leases_state %>%
  mutate(Undivided = 
           if_else(str_detect(Undivided, regex("yes", ignore_case = T)), 
                   TRUE, FALSE))

# Make Term into a number - months
term_months <-
  leases_state %>%
  st_set_geometry(NULL) %>%
  select(Lease_Number, Term) %>%
  tidyr::extract(Term,
          c("TermYears", "TermMonths", "TermDays"),
          regex("(\\d+) years, (\\d+) months, (\\d+) days", ignore_case = TRUE),
          remove = FALSE,
          convert = TRUE) %>%
  mutate(Term2 = TermYears * 12 + TermMonths + TermDays / 30) %>%
  select(Lease_Number, Term2)

leases_state <-
  leases_state %>%
  left_join(term_months) %>%
  select(-Term) %>%
  rename(Term = Term2)

# if royalties are higher than 1, make NA
leases_state <- 
  leases_state %>%
  mutate_at(vars(contains("Royalty")), ~ if_else(. > 1, NA_real_, .))

# if acreage, bonus, or royalty is 0, change to NA
leases_state <- 
  leases_state %>%
  mutate_at(.vars = vars(Bonus, Oil_Royalty, Gas_Royalty,
                         Condensate_Royalty, Term, Original_Gross,
                         Original_Net, Current_Gross, Current_Net), 
            .funs = funs(if_else(. == 0, NA_real_, .)))

# fill in missing net acreages
leases_state <- 
  leases_state %>%
  mutate(Original_Net = case_when(
    is.na(Original_Net) & is.na(Current_Net) ~ Original_Gross,
    is.na(Original_Net) & !is.na(Current_Net) ~ Current_Net,
    TRUE ~ Original_Net),
    Current_Net = if_else(is.na(Current_Net), Current_Gross, Current_Net))
          
# if original_gross < original_net, this looks like a data entry
# problem just take original_gross = original_net in this case
leases_state <- 
  leases_state %>% 
  mutate(Original_Gross = if_else(Original_Gross < Original_Net, 
                                  Original_Net, 
                                  Original_Gross))


# Change depth restriction recordings
lease_depth_from <-
  leases_state %>%
  st_set_geometry(NULL) %>%
  select(Lease_Number, Depth_From) %>%
  unique %>%
  mutate(df_list = strsplit(Depth_From, ";")) %>%
  unnest(df_list) %>%
  filter(str_detect(df_list, "\\d")) %>%
  mutate(df_list = as.numeric(df_list)) %>%
  group_by(Lease_Number, Depth_From) %>%
  summarise(Depth_From2 = min(df_list)) %>%
  select(Lease_Number, Depth_From = Depth_From2)

lease_depth_to <-
  leases_state %>%
  st_set_geometry(NULL) %>%
  select(Lease_Number, Depth_To) %>%
  unique %>%
  mutate(dt_list = strsplit(Depth_To, ";")) %>%
  unnest(dt_list) %>%
  filter(str_detect(dt_list, "\\d")) %>%
  mutate(dt_list = as.numeric(dt_list)) %>%
  group_by(Lease_Number, Depth_To) %>%
  summarise(Depth_To2 = max(dt_list)) %>%
  select(Lease_Number, Depth_To = Depth_To2)

leases_state <-
  leases_state %>%
  select(-Depth_From, -Depth_To) %>%
  left_join(lease_depth_from) %>%
  left_join(lease_depth_to) %>%
  replace_na(list(Depth_From = 0, Depth_To = 999999))

# correct self intersections
leases_state <- 
  leases_state %>% 
  st_buffer(dist = 1e-04) %>% 
  st_cast("MULTIPOLYGON")

# If lease_number is duplicated and lessees are same, take latest record
leases_state <-
  leases_state %>%
  group_by(Lease_Number, Lessee) %>%
  arrange(desc(Effective_Date)) %>%
  filter(row_number() == 1) %>%
  ungroup 

# make Undivided into a logical, acres a number
leases_state <- 
  leases_state %>%
  mutate(Undivided = as.logical(Undivided)) %>%
  mutate(Polygon_Acres = as.numeric(st_area(.)) * m2_to_acre)

# =============================================================================
# CLEAN DUPLICATED GEOMETRIES
# =============================================================================
# Want to find leases that are "undivided" - save a crosswalk that flags
# the undivided leases
# add flag to the leases dataset that marks if a lease has undivided issues
# Undivided geometries are geometries that significantly overlap, 
# terms also significantly overlap, 
# and the recorded gross acreage != net acreage
  
# intersect all leases
leases_slim <- 
  leases_state %>%
  select(Lease_Number, Effective_Date, Expire_Date, 
         Original_Gross, Original_Net, Lessee_alias) %>%
  mutate(id = row_number(), 
         area = as.numeric(st_area(.)))

leases_int <- 
  st_intersection(leases_slim, leases_slim) %>%
  filter(id < id.1) %>%
  mutate(ia = as.numeric(st_area(.))) %>%
  filter(ia / pmin(area, area.1) >= geom_lb) %>%
  filter(Effective_Date <= Expire_Date.1, Expire_Date >= Effective_Date.1)

# Undivided geometries ---------------------------------------------
undivided_geoms <-
  leases_int %>% 
  filter(abs(Effective_Date - Effective_Date.1) < time_lb) 

# use graph theory magic to figure out groups of leases that all intersect
undivided_id <- 
  int_bundles(undivided_geoms$Lease_Number, undivided_geoms$Lease_Number.1, 
              "Lease_Number")

undivided <-
  undivided_id  %>%
  full_join(select(as.data.frame(leases_state), Lease_Number)) %>%
  mutate(ParcelID = if_else(is.na(sameid), Lease_Number, sameid)) %>%
  select(-sameid)

leases_state <-
  leases_state %>%
  left_join(undivided_id) %>%
  mutate(Undivided_Geom = if_else(is.na(sameid), FALSE, TRUE)) %>%
  select(-sameid)

# Date Change ---------------------------------------------
date_change <-
  leases_int %>%
  anti_join(select(as.data.frame(undivided_geoms),
                   Lease_Number,
                   Lease_Number.1)) 

date_change <-
  with(date_change,
       int_bundles(Lease_Number, Lease_Number.1, "Lease_Number")) %>%
  inner_join(leases_slim)
 
date_change <-
  date_change %>%
  group_by(sameid) %>%
  arrange(sameid, Effective_Date) %>%
  mutate(DateChange = TRUE,
         ReLeased = if_else(row_number() == n(), TRUE, FALSE)) %>%
  ungroup %>%
  select(Lease_Number, DateChange, ReLeased)

leases_state <-
  leases_state %>%
  left_join(date_change) %>%
  replace_na(list(DateChange = FALSE, ReLeased = FALSE))


# =============================================================================
# Bonus and Royalty Corrections
# =============================================================================
# this list of manual fixes includes
# hand checked bonuses (checked those < $10 / acre), 
# hand checked royalties (royalty > .3) 
fixes <- 
  file.path(int, "leases/lease_check.csv") %>%
  read_csv() %>%
  select(-Notes, -Effective_Date)

leases_state <-
  leases_state %>%
  left_join(fixes) %>%
  filter(is.na(Exclude)) %>%
  mutate(Original_Net = if_else(!is.na(True_Net),
                                True_Net,
                                Original_Net),
         Original_Gross = if_else(!is.na(True_Gross),
                                  True_Gross,
                                  Original_Gross),
         Bonus = if_else(!is.na(True_Bonus), True_Bonus, Bonus),
         Gas_Royalty = if_else(!is.na(True_Royalty),
                               True_Royalty, 
                               Gas_Royalty), 
         Oil_Royalty = if_else(!is.na(True_Royalty),
                               True_Royalty, 
                               Oil_Royalty)) %>%
  select(-starts_with("True"), -Exclude)

#---------------------------------------------------------------------------
# we want to check that data from shapefile and what was inputted by Maryland
# interns match up
# if they do not, then we use the payment and bid data to verify which one is
# right
load(file.path(gen, "coversheets.Rda"))
load(file.path(gen, "glo_rentals.Rda"))

glo_rentals <- 
  glo_rentals %>%
  filter(str_detect(varDescription , "Bonus"), 
         !str_detect(varDescription, "Depth")) %>%
  mutate(PaymentAmount = 
           ifelse(Lease_Type == "RAL", PaymentAmount*2, PaymentAmount)) %>% 
  group_by(Lease_Number) %>%
  summarise(Bonus_Payments = sum(PaymentAmount))

coversheets <-
  coversheets %>%
  filter(is.na(No_Pdf), is.na(No_Review)) %>%
  select(Lease_Number, BPA_Rec) %>%
  mutate(Lease_Number = paste0("MF", Lease_Number)) %>% 
  left_join(select(as.data.frame(leases_state), 
                   Lease_Number, Original_Net)) %>%
  replace_na(list(BPA_Rec = 0)) %>%
  mutate(BPA_Rec = as.numeric(BPA_Rec), 
         Bonus_CS = BPA_Rec * Original_Net) %>%
  select(-BPA_Rec)

ral <- 
  leases_state %>% 
  st_set_geometry(NULL) %>%
  filter(Effective_Date >= cutoff_date) %>%
  select(Lease_Number, Bonus_Shp = Bonus, Type) %>%
  filter(Type == "RAL") %>%
  left_join(select(coversheets, Lease_Number, Bonus_CS)) %>%
  mutate(CSDiff = abs(Bonus_CS - Bonus_Shp)/Bonus_Shp) %>%
  filter(CSDiff > .05) %>%
  left_join(glo_rentals) %>%
  mutate(CPDiff = abs(Bonus_CS - Bonus_Payments)/Bonus_Payments, 
         SPDiff = abs(Bonus_Shp - Bonus_Payments)/Bonus_Payments) %>%
  filter(CPDiff < .05 | SPDiff < .05) %>%
  mutate(Bonus_Fix = if_else(CPDiff < SPDiff, Bonus_CS, Bonus_Shp)) %>%
  select(Lease_Number, Bonus_Fix) %>%
  mutate(source = "ral")
         

state <-
  leases_state %>% 
  st_set_geometry(NULL) %>%
  filter(Effective_Date >= cutoff_date) %>%
  select(Lease_Number, Bonus_Shp = Bonus, Type) %>%
  filter(str_detect(Type, regex("state", ignore_case = T))) %>%
  left_join(glo_bids) %>%
  mutate(BidDiff = abs(Bonus_Bid - Bonus_Shp)/Bonus_Shp) %>%
  filter(BidDiff > .05) %>%
  left_join(glo_rentals) %>%
  mutate(BPDiff = abs(Bonus_Bid - Bonus_Payments)/Bonus_Payments, 
         SPDiff = abs(Bonus_Shp - Bonus_Payments)/Bonus_Payments) %>%
  filter(BPDiff < .05 | SPDiff < .05) %>%
  mutate(Bonus_Fix = if_else(BPDiff < SPDiff, Bonus_Bid, Bonus_Shp)) %>%
  select(Lease_Number, Bonus_Fix) %>%
  mutate(source = "state")

# for leases where all 3 do not match up, we fix manually by looking at the 
# lease documents
manual_fix <- 
  file.path(int, "leases/bonus_reconciliation.csv") %>%
  read_csv %>%
  mutate(Original_Net = if_else(!is.na(Original_Net_Fixed), Original_Net_Fixed,
                                Original_Net), 
         Bonus_Fix = Correct_Amount * Original_Net) %>%
  select(Lease_Number, Bonus_Fix, Original_Gross_Fixed,
         Original_Net_Fixed, Complicated) %>%
  mutate(source = "manual") 
  
all_fix <-
  bind_rows(ral, state, manual_fix) %>%
  group_by(Lease_Number) %>%
  mutate(HasManual = max(source == "manual")) %>%
  ungroup %>%
  filter(HasManual == FALSE | ((HasManual == TRUE) & (source == "manual"))) %>%
  select(-source, -HasManual)

leases_state <-
  leases_state %>%
  left_join(all_fix) %>%
  mutate(Bonus = if_else(!is.na(Bonus_Fix), Bonus_Fix, Bonus), 
         Original_Gross = if_else(!is.na(Original_Gross_Fixed), 
                                  Original_Gross_Fixed, Original_Gross), 
         Original_Net = if_else(!is.na(Original_Net_Fixed), 
                                Original_Net_Fixed, Original_Net), 
         Complicated = if_else(Complicated & !is.na(Complicated), 
                               TRUE, FALSE),
         Bonus_Per_Acre = Bonus/Original_Net) %>%
  select(-Original_Gross_Fixed, -Bonus_Fix, -Original_Net_Fixed)


# =============================================================================
# add columns for each year showing the upper bound for delay rental options
# =============================================================================

load(file.path(gen, "delay_rentals.Rda"))
delay_rentals <-
  leases_state %>%
  st_set_geometry(NULL) %>%
  select(Lease_Number, Original_Net) %>%
  inner_join(delay_rentals) %>%
  mutate(Rental_Year2 = Rental_Year2 * Original_Net,
         Rental_Year3 = Rental_Year3 * Original_Net,
         Rental_Year4 = Rental_Year4 * Original_Net,
         Rental_Year5 = Rental_Year5 * Original_Net,
         Rental_Year6 = Rental_Year6 * Original_Net,
         Rental_Year7 = Rental_Year7 * Original_Net,
         Rental_Year8 = Rental_Year8 * Original_Net,
         Rental_Year9 = Rental_Year9 * Original_Net,
         Rental_Year10 = Rental_Year10 * Original_Net) %>%
  select(-Original_Net) %>%
  mutate(Rental_Total = 
           select(., starts_with("Rental")) %>% rowSums(., na.rm = T))

# merge in and add 1.5% filing fee for state leases
leases_state <-
  leases_state %>%
  left_join(delay_rentals) %>%
  mutate(Bonus = if_else(Type == "RAL", Bonus, Bonus * 1.015))
    

# ===========================================================================
# add assignments
# ===========================================================================
# add in assignee information
load(file.path(gen, "assignments.Rda"))
assignments <- select(assignments, Lease_Number, Assignee, AssignDate)

leases_state <-
  leases_state %>%
  regex_left_join(assignments) %>%
  group_by(Lease_Number.x) %>%
  arrange(AssignDate) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  select(-Lease_Number.y) %>%
  rename(Lease_Number = Lease_Number.x)


# merge in alias names
leases_state <-
  leases_state  %>%
  clean_names("Assignee") %>%
  rename(Assignee_alias = alias) %>%
  mutate(Assignee_alias = if_else(is.na(Assignee_alias),
                                  Assignee, Assignee_alias),
         Firm = if_else(!is.na(Assignee_alias), Assignee_alias, Lessee_alias))


# =============================================================================
# Add in centroids
# =============================================================================
# add centroid long/lats
leases_state_centroid <-
  leases_state %>%
  st_centroid %>%
  st_coordinates %>%
  as_tibble %>%
  rename(Cent_Long = X, Cent_Lat = Y)

leases_state <- bind_cols(leases_state, leases_state_centroid)

# ===========================================================================
# Spatial join of lease to the closest shale formation
# ===========================================================================
# Using a join function from utils where the features in the join features
# will be matched if a target feature's center falls within them.
leases_state <- closest_shale(leases_state)

# ===========================================================================
# Overlay counties - seems to be some mislabelling in the data
# ===========================================================================
counties <-
  file.path(raw_shape, "us_county/cb_2015_us_county_500k.shp") %>%
  st_read(stringsAsFactors = T) %>%
  mutate(NAME = toupper(NAME)) %>%
  filter(STATEFP == "48") %>%
  st_transform(main_crs)

counties_overlap <-
  counties %>%
  st_intersection(leases_state) %>%
  st_cast("MULTIPOLYGON") %>%
  mutate(area = st_area(geometry)) %>%
  group_by(Lease_Number) %>%
  arrange(area) %>%
  filter(row_number() == 1) %>%
  as.data.frame() %>%
  select(Lease_Number, County = NAME)

# note: two kinds of land here with no county
# 1) 11 Free Royalty leases right on the OK border which are very thin
# 2) a bunch of offshore or rivers/bays
leases_state <-
  leases_state %>%
  left_join(counties_overlap)

# ===========================================================================
# Add area of smallest convex hull containing lease
# ===========================================================================
hull <-
  leases_state %>%
  st_convex_hull() %>%
  mutate(Hull_Acres = as.numeric(st_area(geometry) * m2_to_acre)) %>%
  as.data.frame() %>%
  select(Lease_Number, Hull_Acres)

leases_state <-
  leases_state %>%
  left_join(hull)

# ===========================================================================
# overlay 10 mile by 10 mile grid onto leases
# ===========================================================================
# overlay grid onto map of texas and output it so it can be used for
# overlaying onto wells too
load(file.path(shape, "texas_grid5.Rda"))
load(file.path(shape, "texas_grid10.Rda"))
load(file.path(shape, "texas_grid20.Rda"))

grid_overlap5 <-
  texas_grid5 %>%
  st_intersection(leases_state) %>%
  st_cast("MULTIPOLYGON") %>%
  mutate(area = st_area(geometry)) %>%
  group_by(Lease_Number) %>%
  arrange(-area) %>%
  filter(row_number() == 1) %>%
  st_set_geometry(NULL) %>%
  select(Lease_Number, lGrid5 = GridID)

grid_overlap10 <-
  texas_grid10 %>%
  st_intersection(leases_state) %>%
  st_cast("MULTIPOLYGON") %>%
  mutate(area = st_area(geometry)) %>%
  group_by(Lease_Number) %>%
  arrange(-area) %>%
  filter(row_number() == 1) %>%
  st_set_geometry(NULL) %>%  
  select(Lease_Number, lGrid10 = GridID)

grid_overlap20 <-
  texas_grid20 %>%
  st_intersection(leases_state) %>%
  st_cast("MULTIPOLYGON") %>%
  mutate(area = st_area(geometry)) %>%
  group_by(Lease_Number) %>%
  arrange(-area) %>%
  filter(row_number() == 1) %>%
  st_set_geometry(NULL) %>%  
  select(Lease_Number, lGrid20 = GridID)

leases_state <-
  leases_state %>%
  left_join(grid_overlap5) %>%
  left_join(grid_overlap10) %>%
  left_join(grid_overlap20)


#===========================================================================
# Find distance to infrastructure in separate leases_infrastructure.R file
#==========================================================================
source(file.path(root, 'code/Data_Cleaning/infrastructure_leases.R'))

leases_state <-
  left_join(leases_state, infrastructure_leases)

#===========================================================================
# add flag for fraction a lease still covered by another lease
#===========================================================================
leases_slim <-
  leases_state %>% 
  select(Lease_Number, Effective_Date, Expire_Date) %>%
  mutate(area = as.numeric(st_area(.))) %>%
  mutate(id = row_number())

overlap <- 
  st_intersection(leases_slim, leases_slim) %>%
  filter(Effective_Date <= Expire_Date.1, Expire_Date >= Effective_Date.1, 
         id != id.1) %>%
  mutate(int_perc = as.numeric(st_area(.)) / pmin(area, area.1)) %>%
  filter(int_perc > .01) %>%
  st_buffer(0.0001) 

lease_level_overlap <-
  select(overlap, Lease_Number, area) %>%
  rbind(select(overlap, Lease_Number = Lease_Number.1, area = area.1)) %>%
  group_by(Lease_Number) %>%
  summarise(area = unique(area)) %>%
  mutate(FracOverlap = as.numeric(st_area(.)) / area) %>%
  st_set_geometry(NULL) %>%
  select(-area)

leases_state <-
  leases_state %>%
  left_join(lease_level_overlap) %>%
  replace_na(list(FracOverlap = 0)) %>%
  mutate(FracOverlap = if_else(FracOverlap > 1, 1, FracOverlap))

#===========================================================================
# merge in the shale isopatch information
#===========================================================================
# add in shale isopach for eagle ford and permian
load(file.path(shape, "isopach_permian_interpolate.Rda"))
load(file.path(shape, "isopach_eagle_ford_interpolate.Rda"))

leases_state <-
  leases_state %>%
  st_transform(utm_crs)

leases_isopach_permian <-
  raster::extract(isopach_permian_interpolate, leases_state, fun = mean)

leases_isopach_eagle_ford <-
  raster::extract(isopach_eagle_ford_interpolate, leases_state, fun = mean)

leases_state <-
  leases_state %>%
  mutate(ShaleThicknessPermian = as.numeric(leases_isopach_permian),
         ShaleThicknessEagleFord = as.numeric(leases_isopach_eagle_ford)) %>%
  mutate(ShaleThickness = case_when(
           !is.na(ShaleThicknessPermian) ~ ShaleThicknessPermian,
           !is.na(ShaleThicknessEagleFord) ~ ShaleThicknessEagleFord,
           TRUE ~ NA_real_)) %>%
  select(-ShaleThicknessPermian, -ShaleThicknessEagleFord) %>%
  st_transform(main_crs)

# ===========================================================================
# output data
# ===========================================================================
# first convert all string variables to uppercase
leases_state <-
  leases_state %>%
  mutate_if(is.character, toupper)
save(leases_state, file = file.path(gen, "leases_state.Rda"))
