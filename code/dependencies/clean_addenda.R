# Created by Grace Park in August 2018

library(tidyverse)
library(readxl)
library(lubridate)

root <- getwd()
while (basename(root) != "texas") {
  root <- dirname(root)
}
source(file.path(root, "code/paths.R"))

# ===========================================================================
# helper functions
# ===========================================================================

# simple mode function
mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x,ux)))]
}

# mode function for when there could be several modes
num_mode <- function(x, return_multiple = TRUE) {
  ux <- unique(x)
  freq <- tabulate(match(x, ux))
  mode_loc <- if(return_multiple) which(freq==max(freq)) else which.max(freq)
  return(ux[mode_loc])
}


# ===========================================================================
# Read/clean data
# ===========================================================================
clean <- function(path) {
  print(path)
  
  enterer_id <- 
    basename(path) %>% 
    str_extract("[:alpha:]+(?=\\.)")

  if(enterer_id == "Cem") {
    cem <- read_excel(path)
    dat <-
      cem %>%
      extract(`Lease_Number,Effective_Date`, 
              c("MF # (Lease_#)", "Effective Date"), 
              regex("(\\d+),([\\d-]+)"))
  } else {
    dat <- read_excel(path)
  }
  
  charcolumns <- 
    dat %>% 
    select(`Full Name`,
           `Address`)

  numcolumns <-
    dat %>% 
    select(-`Full Name`,
           -`Address`,
           -starts_with("Effective")) %>%
    mutate_all(function(x) str_replace_all(x, regex("[^\\d-]"), "")) %>%
    mutate_all(as.integer)
  
  cleaned <-
    bind_cols(charcolumns, numcolumns) %>%
    mutate(enterer_id = enterer_id, path = basename(path))
  
  return(cleaned)
}

df_ad <- 
  file.path(ddir, "raw_data/addenda/finished") %>%
  list.files(pattern='*.xlsx', full.names=TRUE) %>%
  map_df(clean) %>% 
  select(mf_n=`MF # (Lease_#)`,
         name = `Full Name`,
         address = Address,
         c01 = `Surface (22, 23)`,
         c02 = `Payment (3-9)`,
         c03 = `Location (24)`,
         c04 = `Pugh (16)`,
         c05 = `Clean Up (26)`,
         c06 = `Force Majeure (18)`,
         c07 = `Livestock (22)`,
         c08 = `Water (21)`,
         c09 = `Waste (25, 36)`,
         c10 = `Def (2)`,
         c11 = `Pollution (25)`, 
         c12 = Infrastructure,
         c13 = Caliche,
         c14 = Fees,
         c15 = Time,
         c16 =  Misc,
         n_pages =`# of AC Pages`,
         n_parags = `# of AC Paragraphs`,
         n_clauses = `# of AC`,
         enterer_id,
         path)

#fix Cem's NAs and the incomplete entries
bad <- 
  df_ad %>% 
  filter(path == "1.12-Cem.xlsx", 
         is.na(c03))

maybe_bad <-
  anti_join(df_ad, bad, by="mf_n") %>%
  group_by(mf_n) %>% 
  mutate(c01_na = is.na(c01)) %>% 
  filter(length(unique(c01_na))==2,
         c01_na)

missing_files <-
  anti_join(df_ad, maybe_bad, by="mf_n") %>%
  group_by(mf_n) %>%
  filter(is.na(c01))

addenda_raw <-
  anti_join(df_ad, maybe_bad, by="mf_n") %>%
  group_by(mf_n) %>%
  filter(!is.na(c01)) %>%
  mutate(complete = length(mf_n)) %>%
  arrange(mf_n)

df_ad %>% map(~sum(is.na(.))) %>% unlist

# ===========================================================================
# Ranking
# ===========================================================================
# As a part of the RAL Addenda project, this script ranks the people
# who entered in addenda data by the quality of their work

df_rank <- 
 addenda_raw %>% 
  group_by(mf_n) %>% 
  filter(!duplicated(enterer_id), 
         n()==3 ) %>%
  select(mf_n,
         c01:c16,
         enterer_id) %>%
  gather(c_n,
         entry, 
         c01:c16, 
         na.rm = TRUE, 
         convert = FALSE) %>%
  arrange(mf_n, c_n) %>%
  group_by(mf_n, c_n) %>%
  mutate(agreement = length(unique(entry)), 
         agreed = if_else (agreement < 3, TRUE, FALSE)) %>%
  filter(agreed) %>%
  mutate(correct_entry = mode(entry),
         disagreer = ifelse(entry == correct_entry, FALSE, TRUE))

# count up number of times each enterer disagreed
# divide it by the number of times they appear in this rank dataset
rank <-
  df_rank %>%
  group_by(enterer_id) %>%
  summarise(disagreed = sum(disagreer), 
            total = n()) %>%
  mutate(per_err = disagreed / total) %>%
  arrange(per_err) %>%
  mutate(rank = row_number())

# =========================================================================
# Resolve
# =========================================================================
# we want to resolve when different 
# enterers put in different values for the addenda effect
# n_enterer = number of enterers
  # for the purpose of filtering out mineral files that < 3 people entererd
df_res <-
  addenda_raw %>%
  group_by(mf_n) %>%
  select(mf_n,
         c01:c16,
         enterer_id) %>%
  gather(c_n, 
         entry, 
         c01:c16, 
         na.rm = TRUE, 
         convert = FALSE) %>%
  left_join(rank) %>%
  select(c(mf_n, c_n, entry, rank)) %>%
  group_by(mf_n, c_n) %>%
  mutate(n_enterer = n())


#------------------
# Only 1 enterer
#------------------
df_one <-
  df_res %>%
  filter(n()==1) %>%
  mutate(n_enterer = 1,
         resolved = entry) %>%
  select(mf_n, c_n, resolved, n_enterer)


#------------------
# 2 enterers
#------------------
# if 2 people disagree, take the entry of the "more trustworthy" person 
df_two <-
  df_res %>%
  filter(n()==2) %>%
  mutate(n_enterer = 2,
         agreement = length(unique(entry)),
         resolved = if_else(agreement == 1, 
                            entry, 
                            entry[which.min(rank)])) %>%
  select(mf_n, c_n, resolved, n_enterer) %>%
  distinct()

#------------------
# 3 enterers
#------------------
# 3 cases
  # 1. all three agree
  # 2. 2 people agree - take mode 
  # 3. No one agrees - take the entry of the most trustworthy person
df_three <- 
  df_res %>%
    arrange(mf_n, c_n) %>%
    filter(n() == 3) %>%
    arrange(rank) %>%
    mutate(agreement = length(unique(entry)), 
           resolved = case_when(
           agreement == 1 ~ entry, 
           agreement == 2 ~ num_mode(entry),
           agreement == 3 ~ entry[which.min(rank)]), 
           n_enterer = 3) %>%
    select(mf_n, c_n, resolved, n_enterer) %>%
    distinct()

#------------------
# 4 or 5 Enterers
#------------------
# 4 cases
  # 1. Majority agrees
  # 2. 2 people have x, 2 people have y
    # add ranks of x enterers, add ranks of y enterers
    # resolve to the entry of the 2 enterers with lower cumulative rank
  # 3. everyone disagrees 
    # drop the entry of 0, which reduces it to a 3 enterer scenario 

# if a majority exists, take entry of majority
    # if not, all 4 people disagree or people agree for 2 entries
    # take the entry of the lowest cumulative rank 


df_gen <- 
  df_res %>%
  group_by(mf_n, c_n, entry) %>%
  mutate(cum_rank = sum(rank)) %>%
  group_by(mf_n, c_n) %>%
  filter(n() == 4 | n()==5) %>%
  mutate(n_enterer = if_else(length(entry) == 4, 4, 5),
         modeno = length(num_mode(entry))) %>%
  filter(!(modeno == 4 & entry == 0)) %>% 
  # get rid of 0 entries if 4 people disagree
  ungroup %>% 
  mutate(resolved = case_when(
    modeno == 1 ~ num_mode(entry),
    modeno == 2 ~ entry[which.min(cum_rank)], 
    modeno == 4 ~ entry[which.min(rank)])) %>%
  select(mf_n, c_n, resolved, n_enterer) %>%
  group_by(mf_n, c_n) %>%
  distinct()

#------------------
# 5+ Enterers (aka 6 and 9)
#------------------

df_more <-
  df_res %>% 
  filter(n()>5) %>%
  group_by(mf_n, c_n) %>%
  mutate(n_enterer = n()) %>%
  ungroup() %>%
  group_by(mf_n, c_n, entry) %>%
  mutate(cum_rank = sum(rank)) %>%
  group_by(mf_n, c_n) %>%
  mutate(modeno = length(num_mode(entry))) %>%
  filter(!(modeno == 3 & entry == 0)) %>%
  mutate(resolved = ifelse(modeno == 1, 
                           num_mode(entry), 
                           entry[which.min(cum_rank)])) %>%
  select(mf_n, c_n, resolved, n_enterer) %>%
  distinct()


#Resolved entries for all mineral files:
resolve_entry <- 
  bind_rows(df_one, df_two, df_three, df_gen, df_more) %>%
  spread(c_n, resolved)

#--------------------------------------
# Resolving length
#--------------------------------------
#Resolved by averaging the length of addenda
#Did not resort to most accurate enterer, because length is fairly arbitrary
#resolve_length has three fewer MF than resolve_entry

resolve_length <-
  addenda_raw %>%
  select(mf_n,
         n_pages,
         n_parags,
         n_clauses,
         enterer_id,
         path) %>%
  filter(!is.na(n_pages),
         !is.na(n_parags),
         !is.na(n_clauses)) %>%
  group_by(mf_n)%>%
  mutate(pages_resolved = mean(n_pages),
         parags_resolved = mean(n_parags),
         clauses_resolved = mean(n_clauses)) %>%
  mutate(pages_rounded = ceiling(pages_resolved),
         parags_rounded = ceiling(parags_resolved),
         clauses_rounded = ceiling(clauses_resolved)) %>%
  select(mf_n, 
         pages_resolved, 
         parags_resolved, 
         clauses_resolved,
         pages_rounded,
         parags_rounded,
         clauses_rounded) %>%
  distinct()%>%
  arrange(mf_n) 


#--------------------------------------
# Resolving info (name, address)
#--------------------------------------
#Resolved based off the most accurate enterer
#resolve_info has three fewer MF than resolve_entry, same as resolve_length,

resolve_info <-
  addenda_raw %>%
  left_join(rank) %>%
  group_by(mf_n)%>%
  mutate(name_resolved = name[which.min(rank)],
         address_resolved = address[which.min(rank)]) %>%
  select(mf_n,
         name_resolved,
         address_resolved) %>%
  distinct() %>%
  arrange(mf_n) 

#--------------------------------------
# Final resolved data
#--------------------------------------

addenda <-
  bind_cols(resolve_entry, 
            resolve_length, 
            resolve_info) %>%
  select(mf_n,
         name_resolved,
         address_resolved,
         n_enterer,
         c01:c16,
         pages_resolved,
         parags_resolved,
         clauses_resolved, 
         pages_rounded,
         parags_rounded,
         clauses_rounded)

save(addenda, file = file.path(int, "addenda.Rda"))