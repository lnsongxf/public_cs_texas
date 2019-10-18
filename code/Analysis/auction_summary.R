# BASIC TEXAS SETUP =================================================
library(tidyverse)
library(lubridate)
library(knitr)

root <- getwd()
while(basename(root) != "texas") {
  root <- dirname(root)
}

source(file.path(root, "code", "paths.R"))
source(file.path(root, "code", "functions", "utils.R"))

# Overview
# Tables and figures on the probability of success at auction and the bid 
# distribution conditional on a parcel selling.

#===========================================================================
# READ IN AND COMBINE DATA
#===========================================================================
load(file.path(gen, "glo_bids.Rda"))

bids_glo <-
  glo_bids %>%
  rename(Bid = Bonus) %>% 
  filter(Lease_Number != "MF") %>% 
  group_by(Date, Lease_Number, Acres) %>% 
  arrange(-Bid, Bidder) %>% # doing this for possible ties
  mutate(bidrank = rank(-Bid, ties.method = "first")) %>%
  mutate(nbids = length(Bid)) %>%
  ungroup %>%
  arrange(Date, Lease_Number, Acres, bidrank) %>%
  mutate_at(vars(Acres, Bid, Tract), as.numeric)

notices_glo <-
  file.path(int, "glo_notices_final.csv") %>%
  read_csv %>%
  rename(parcel_id = merge_id) %>%
  mutate(MGL = as.numeric(MGL)) %>%
  rename(Tract = MGL) %>%
  mutate_at(vars(Acres, MinBid, Tract), as.numeric)

nrow(filter(bids_glo, Winner))
nrow(notices_glo)

### First figure out how many we have
### dropping post 2016 since we didn't have all the bid data at that point
glo_notices_bids <- 
  bids_glo %>% 
  filter(Winner) %>%
  right_join(notices_glo, by = c("Tract","Date"), 
             suffix = c("_bid", "_notice")) %>% 
  mutate(cleared = if_else(is.na(Winner), 0, 1), 
         Year = year(Date)) %>%
  filter(Year < 2017) 

with(glo_notices_bids, summary(cleared))

glo_parcels <- 
  glo_notices_bids %>%
  group_by(parcel_id) %>%
  summarize(Nauctions= n(),
            Nsuccess = sum(cleared),
            County_notice = first(County_notice),
            ever_cleared = max(cleared),
            first_year = min(Year),
            last_year = max(Year)) 

summary(glo_parcels)

# next two tables show the probability a parcel ever clears by year and for 
# the top counties. 

tyear <- 
  glo_parcels %>% 
  group_by(first_year) %>% 
  summarise(avg = mean(ever_cleared), nparcels = n()) 

kable(tyear, caption = "Ever cleared by first year in notices")

tcount <- 
  glo_parcels %>% 
  group_by(County_notice) %>% 
  summarise(avg = mean(ever_cleared), nparcels = n()) %>%
  arrange(-nparcels)

kable(filter(tcount, nparcels > 20),
      caption = "Ever cleared by county (with N > 20)")


#===========================================================================
## SUCCESSFUL AUCTION SUMMARY
#===========================================================================
# BRING IN LEASE ALL (SIMPLY SO WE CAN GET InSample FLAG?)
load(file.path(gen, "final_leases.Rda"))
sample_flag <- 
  as_tibble(final_leases) %>%
  select(Lease_Number, InSample)

secondbids <- 
  bids_glo %>% 
  filter(bidrank == 2) %>% 
  select(Lease_Number, secondbid = Bid)

winbids <- 
  glo_notices_bids %>%
  filter(cleared, bidrank==1) %>%
  left_join(sample_flag, by="Lease_Number") %>%
  left_join(secondbids, by="Lease_Number") %>%  
  mutate(Bid_per_acre = if_else(is.na(Acres_bid),
                                Bid / Acres_notice,
                                Bid / Acres_bid)) %>%
  mutate(bid_reserve = Bid - MinBid, 
         bid_reserve_pct = Bid / MinBid, 
         markup = Bid / secondbid)  

#===========================================================================
# SUMMARY TABLE OF AUCTION STATS
#===========================================================================
winbids <-
  winbids %>%
  mutate(tbids = if_else(nbids > 5, "6 +", as.character(nbids)))

sum_stats <- 
  winbids %>% 
  filter(InSample) %>% 
  group_by(tbids) %>% 
  summarise(avg_bid = mean(Bid_per_acre),
            nleases = n(),
            bid_res = median(bid_reserve_pct),
            markup = median(markup)) %>%
  mutate(allsum = nleases/sum(nleases))

# SAVE TABLE (redone in kableExtra by TC)
options(knitr.kable.NA = '')
bid_table <-
  sum_stats %>%
  select(tbids, nleases, allsum, avg_bid, bid_res, markup) %>%
  kable(digits = c(2, 2, 2, 0, 2),
        format.args = list(big.mark = ','),
        format = "latex",
        booktabs = TRUE,
        linesep = "",
        col.names = c("Bids", "Auctions", "Fraction",
                      "Avg. Bonus ($/Acre)",
                      "Med. Bid/Reserve",
                      "Med. Markup"))

write(bid_table,
      file = file.path(root, "output/tables/auction_number_bids.tex"),
      append = FALSE, sep = " " )


#===========================================================================
## BID RANKING OF COMMON COMPETING FIRMS
#===========================================================================
# Find pairs of firms that often compete in the same auction and figure out 
# how likely it is that one bids higher than the other. 
# This is used as (not conclusive) evidence in support of match quality story.

bids_glo <- 
  glo_bids %>%
  rename(Bid = Bonus) %>% 
  filter(Lease_Number != "MF") %>% 
  inner_join(sample_flag) %>%
  filter(InSample) %>%
  group_by(Date, Lease_Number, Acres) %>% 
  arrange(-Bid, Bidder) %>% 
  mutate(bidrank = rank(-Bid, ties.method = "first")) %>%
  mutate(nbids = length(Bid)) %>%
  ungroup %>%
  arrange(Date, Lease_Number, Acres, bidrank) %>% 
  clean_names("Bidder") %>%
  mutate(Bidder_alias = if_else(is.na(alias), Bidder, alias)) %>%
  select(-alias)  %>%
  mutate(broker = str_detect(Bidder_alias, broker_regex) &
           !str_detect(Bidder_alias, nonbroker_regex)) %>%
  mutate(fnumber = as.integer(factor(Bidder_alias)))

bids_slim <-
  bids_glo %>%
  filter(!broker) %>%
  group_by(Bidder_alias) %>%
  filter(n() >= 10) %>%
  ungroup %>%
  select(Bidder_alias, Lease_Number, Bid)

eqprop_pval <- function(x, n) {map2_dbl(x, n, ~ binom.test(.x, .y)$p.value)}

top_table <-
  bids_slim %>%
  rename(Bidder_alias2 = Bidder_alias, Bid2 = Bid) %>%
  left_join(bids_slim, by = "Lease_Number") %>%
  filter(Bidder_alias2 < Bidder_alias) %>%
  mutate(win = Bid2 > Bid) %>%
  group_by(Bidder_alias2, Bidder_alias) %>%
  summarize(n = n(), wins = sum(win)) %>%
  ungroup %>%
  arrange(desc(n)) %>%
  filter(n >= 10) %>%
  mutate(p = round(eqprop_pval(wins, n), 3)) %>%
  mutate(wins = round(wins / n, 2)) %>%
  kable(format = "latex",
        booktabs = TRUE,
        linesep = "",
        escape = FALSE,
        col.names = c("Firm A", "Firm B",
                      "Auctions", "Share A $>$ B",
                      "p-value"))

write(top_table,
      file = file.path(tdir, "TopPairAuctionShares.tex"),
      append = FALSE,
      sep = " ")


