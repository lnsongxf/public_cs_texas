# Install necessary packages
# data shaping packages: tidyverse, lubridate, readxl, pdftools, sf, fst, 
# 	tictoc, furrr, raster, lwgeom
# tables and figures: knitr, grid, gridExtra, gtable, tigris, broom, kableExtra 
# regressions: lmtest, sandwich, splines, lfe, modelr, Formula, grf

packages <-
  c("tidyverse", "lubridate", "readxl", "pdftools", "sf", "fst", "fuzzyjoin",
    "tictoc", "furrr", "raster", "lwgeom", "knitr", "grid", "gridExtra", 
    "gtable", "tigris", "broom", "kableExtra", "lmtest", "sandwich", "sp",
    "splines", "lfe", "modelr", "Formula", "grf", "alpaca", "gstat", "rgdal")

check_install <- function(pkg){
  if(!(pkg %in% installed.packages())){
    install.packages(pkg, repos="http://cran.rstudio.com/")
    return(paste(pkg, " is now installed"))
  } else {
    return(paste(pkg, " is already installed"))
  }
}

lapply(packages, check_install)
