#reading in and reshaping bid tabulations 
#Created by Yixin Sun 5/1/2017
# Modified by Eric Karsten starting 08/07/2018

#load packages we need 
library(tidyverse)
library(lubridate)
library(readxl)
library(pdftools)

root = getwd()
while(basename(root) != "texas") {
  root <- dirname(root)
}
source(file.path(root, "code", "paths.R"))
source(file.path(root, "code", "functions", "utils.R"))

# =============================================================================
# read and clean GLO bid data, see steps for UT bid data below
# =============================================================================
read_glo <- function(x) {
  glo <-
    read_excel(x,
               col_names = c("page",
                             "mgl",
                             "Bidder",
                             "tract",
                             "Bid",
                             "comments",
                             "County",
                             "Acres")) %>%
    replace_na(list(page = "0")) %>%
    filter(!str_detect(page, regex("[a-zA-Z]", ignore_case = TRUE))) %>%
    mutate(Date = str_match(x, regex("([\\d\\.]+)\\.xlsx"))[, 2]) %>%
    mutate(Date = mdy(Date)) %>%
    fill(mgl, tract, County, Acres) %>%
    group_by(mgl) %>%
    mutate(survey = (row_number() == 1)) %>% 
    ungroup()

  glo_bidders <-
    glo %>%
    filter(!survey) %>%
    select(-survey) 
  
  glo_surveys <-
    glo %>%
    filter(survey) %>%
    select(mgl, Bidder) %>%
    rename(survey = Bidder) %>%
    unique()
  
  glo <-
    left_join(glo_bidders, glo_surveys) %>%
    mutate(Lease_Number = str_match(comments, regex("[\\d]+")), 
           Lease_Number = ifelse(Lease_Number == 0, NA, Lease_Number)) %>%
    group_by(mgl) %>%
    # this used to be a fill, but when lease no wasn't first
    # entry, that caused problems - EK
    mutate(Lease_Number = paste0("MF", unique(na.omit(Lease_Number)))) %>%
    ungroup() %>%
    select(Date, page, mgl, County, survey, tract,
           Acres, Bidder, Bid, Lease_Number)
 
  return(glo)
}


glo_bids <-
  list.files(raw_bids, pattern = "^Final_GLO_", full.names = TRUE) %>%
  map_df(read_glo) %>%
  select(Date, Acres, Bidder, Bonus = Bid, mgl,
         Lease = Lease_Number, County) %>%
  mutate(Agency = "GLO", 
         Acres = str_remove_all(Acres, "\\,"), 
         Bonus = as.numeric(Bonus)) %>%
  group_by(Lease_Number = Lease) %>%
  mutate(Winner = Bonus == max(Bonus), 
         Tract = str_replace_all(mgl, "\\D+", "")) %>%
  select(-mgl) %>%
  ungroup() %>%
  group_by(Date, Tract, Bonus) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  mutate(BonusPerAcre = Bonus/as.numeric(Acres)) 

save(glo_bids, file = file.path(gen, "glo_bids.Rda"))

