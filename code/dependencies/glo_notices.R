# Created by Yixin Sun on 10/16/2017 for 
# Issue 110, GLO Minimum Bid PDF Extraction

# load in packages
library(tabulizer) 
library(pdftools)
library(tibble)
library(stringr)
library(dplyr)
library(readr)
library(purrr)
library(lubridate)

root <- getwd()
while(basename(root) != "texas") {
  root <- dirname(root)
}
source(file.path(root, "code", "paths.R"))

ndir <- file.path(raw, "notices")
rdir <- file.path(ndir, "raw_output")
fdir <- file.path(int, "notices")

# ============================================================================
# functions for finding pages to extract from and 
# customizing extract_table function from tabula 
# ============================================================================
types <- c("SURVEYED SCHOOL LAND", "CRIMINAL JUSTICE", "PARKS AND WILDLIFE", 
           "MENTAL HEALTH", "PERMANENT SCHOOL FUND", "SULPHUR TRACTS", 
           "BLIND AND VISUALLY IMPAIRED", "AGING AND DISABILITY SERVICES", 
           "TRANSPORTATION", "GEOTHERMAL", "HISTORICAL COMMISSION", 
           "YOUTH COMMISSION")
regex_types <- regex(paste(types, collapse = "|"))

page_no_extract <- function(notice_file){
  notice_char <- pdf_text(notice_file)
  pageno <- 
    notice_char %>%
    as_tibble() %>%
    setNames("char") %>%
    mutate(min = str_detect(char, "MINIMUM BID"), 
           type = str_detect(char, regex_types)) 
  pageno <- c(1, which(pageno$min & pageno$type))
  
  date <-
    notice_file %>%
    basename() %>%
    str_replace(paste0(rdir, "/"), "") %>%
    strsplit("_") %>%
    unlist %>%
    as_tibble %>%
    filter(row_number() == 1) %>%
    mdy

  return(list(notice_file, pageno, date))
}

# function for extract the data tables of minimum bids
extract_personal <- function(path, pageno, date){
  extracted_list <- 
    extract_tables(file = path, pages = pageno, 
                 output = "data.frame") 
  
  output <- 
    basename(path) %>%
    str_replace(".pdf", "") %>%
    paste0(., ".csv")
  
  extracted_list %>%
    map(function(x) write_csv(x, 
        path = file.path(rdir, output), append = T))
}

# ============================================================================
# Extracting minimum bid tables
# ============================================================================
# get pdf paths, page we want to start, page we want to end, and the date of
# auction into a tibble
notices_files <- 
  list.files(ndir, full.names = T, pattern = ".pdf") %>%
  map(page_no_extract)

# extract tables
min_bids <- 
  map(notices_files, function(x) pmap(x, extract_personal))

# ============================================================================
# Which notices are not able to be read in 
# ============================================================================
unscanned <-
  list.files(ndir, full.names = T, pattern = ".pdf") %>%
  map(function(x) unique(cbind(path = x, text = pdf_text(x)))) %>%
  keep(~ length(.x) <= 2) %>%
  reduce(rbind) %>%
  as_tibble() %>%
  mutate(path = str_replace(path, rdir, ""))


# ============================================================================
# read in fixed output
# ============================================================================
# function for reading in csv and binding with the date of notice
read_notice <- function(file){
  notice <- 
    file %>%
    read_csv(col_types = cols(.default = "c")) %>%
    select(MGL = starts_with("MGL"),
           Min_Bid = starts_with("MINIMUM"), 
           Sec_Tract = ends_with("TRACT"),
           County = COUNTY, 
           County_Seat = starts_with("COUNTY SEAT"),
           Area = starts_with("AREA"), 
           Block = BLOCK, Tsp = TSP, 
           Survey = SURVEY, Acres = ACRES, 
           Comments = starts_with("PART / COMMENTS"))
  
  date <- 
    file %>%
    str_replace(paste0(fdir, "/"), "") %>%
    str_replace(".csv", "") %>%
    mdy
  notice <- mutate(notice, Date = date)
}

# load in glo bid tabulation data to link with notices and bids
load(file.path(gen, "glo_bids.Rda"))

glo_notices_raw <- 
  list.files(fdir, full.names = T, pattern = ".csv") %>%
  map_df(read_notice) %>%
  filter(!is.na(MGL)) %>% 
  mutate(Min_Bid = str_replace(Min_Bid, "\\$", ""), 
         Min_Bid = str_replace_all(Min_Bid, ",", ""), 
         Min_Bid = as.numeric(Min_Bid), 
         Acres = as.numeric(Acres)) %>%
  unique()
  
# ----------------------------- MANUAL CLEANING ----------------------------#
  # At this point, we read in the pdfs by hand that weren't abled to be scanned.
  # The files are then saved to raw_data/bids/notices/final_output.
  # Next we search through the notices and find leases that we believe
  # are the same parcel, and create an ID linking those
# --------------------------------------------------------------------------#

# merge in id created by manually linking parcels across different auctions
glo_notices <- 
  file.path(int, "glo_notices_final.csv") %>%
  read_csv() %>%
  rename(parcel_id = merge_id) %>%
  mutate(MGL = as.numeric(MGL)) %>%
  rename(Tract = MGL)

save(glo_notices, file = file.path(int, "glo_notices.Rda"))

