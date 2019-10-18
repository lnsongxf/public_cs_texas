# Scan makefile and figure out which raw files are necessary to recreate the
# paper.  do two versions of this:
# (1) a public version, which contains only data that we can redistribute.  we
# use this version to actually share with the world, and verify that what we
# are sharing can successfully be used to recreate the parts of the paper that
# do not contain proprietary information.
# (2) an internal private version, which includes proprietary data.  we use
# this version to verify that the makefile correctly captures all of the
# dependencies in the data and programs.

#===========================================================================
# BASIC TEXAS SETUP
#===========================================================================
root <- getwd()
while(basename(root) != "texas") {
  root = dirname(root)
}

library(tidyverse)

source(file.path(root, "code", "paths.R"))

odir <- file.path(ddir, "publish") # path to new datasets
pdir <- file.path(ddir, "public") # path to new public datasets

not_public <- regex("psf|pden|prod|index_key", ignore_case = TRUE)

# read in internal makefile
makefile <- read_file(file.path(root, "makefile"))

#===========================================================================
# function that moves raw data to new folder
#===========================================================================
move_file <- function(path, data_type, file_type, out){
  og <- file.path(data_type, dirname(path))
  new <- file.path(out, basename(data_type), dirname(path))
  stump <- basename(path)
  dir.create(new, showWarnings = FALSE, recursive = TRUE)

  if(file_type == "shape"){
    stump <- str_replace(stump, "\\.(.*)$", "")
  } else if(file_type == "wildcard"){
    stump <- basename(stump)
  } 

  list.files(og, pattern = stump, full.names = TRUE) %>%
    walk(function(x) file.copy(x, new))

  print(path)
}

file_labels <- function(file, public = FALSE){
  outfile <-
    file %>%
    str_split(pattern = "\n") %>%
    pluck(1) %>%
    keep(~ str_detect(.x, "DATA")) %>%
    keep(~ !str_detect(.x, "chk :=")) %>%
    str_replace_all("\t|\r|\\\\", "") %>%
    str_split(pattern = ":\\s") %>%
    unlist %>%
    tibble(path = .) %>%
    mutate(data_type = case_when(
        str_detect(path, "DATA_raw") ~ raw, 
        str_detect(path, "DATA_shape") ~ raw_shape, 
        str_detect(path, "DATA_int") ~ int,
        str_detect(path, "DATA_shale1") ~
          file.path(raw_shape,
                    "TightOil_ShaleGas_IndividualPlays_Lower48_EIA"),
        str_detect(path, "DATA_shale2") ~
          file.path(raw_shape,
                    "TightOil_ShalePlays_US_EIA_Jan2019")),
      file_type = case_when(
        str_detect(path, ".shp") ~ "shape", 
        str_detect(path, "wildcard") ~ "wildcard", 
      TRUE ~ "file")) %>%
    filter(!is.na(data_type), !(data_type == shape & file_type == "file"), 
      !str_detect(path, "\\s=\\s")) %>%
    mutate(path = str_replace(path, "\\$\\(DATA_.*\\)\\/", ""), 
      path = str_replace(path, "^(.*)\\$\\(wildcard\\s", ""), 
      path = str_trim(str_replace(path, "\\)", ""))) %>%
    distinct()

  if(public){
    outfile <-
      outfile %>%
      filter(!str_detect(path, not_public))
  }

  return(outfile)
}

#===========================================================================
# Run #1 Copy over all files 
#===========================================================================
dir.create(odir, showWarnings = FALSE)
dir.create(file.path(odir, "raw_data"), showWarnings = FALSE)
dir.create(file.path(odir, "intermediate_data"), showWarnings = FALSE)
dir.create(file.path(odir, "generated_data"), showWarnings = FALSE)
dir.create(file.path(odir, "generated_shape_files"), showWarnings = FALSE)
dir.create(file.path(odir, "shape_files"), showWarnings = FALSE)


# move over all datasets necessary to compile the paper
pwalk(file_labels(makefile), move_file, odir)


#===========================================================================
# Run #2 Copy over public files 
#===========================================================================
dir.create(pdir, showWarnings = FALSE)
dir.create(file.path(pdir, "raw_data"), showWarnings = FALSE)
dir.create(file.path(pdir, "intermediate_data"), showWarnings = FALSE)
dir.create(file.path(pdir, "generated_data"), showWarnings = FALSE)
dir.create(file.path(pdir, "generated_shape_files"), showWarnings = FALSE)
dir.create(file.path(pdir, "shape_files"), showWarnings = FALSE)

# move over all public files
pwalk(file_labels(makefile, public = TRUE), move_file, pdir)
