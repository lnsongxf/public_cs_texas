# Code and Data for Covert and Sweeney (2019)
The data can be downloaded at this [Dropbox link](https://www.dropbox.com/s/vor231hekpu6v2e/public_20191018.zip?dl=0). Please contact us if you want the data and are not able to access Dropbox.

To recreate the results and analysis datasets from the raw files, create a data.txt file in the root code directory. This file should just have a single file path to direct to where the data folder has been placed on your hard drive, such as `C:/Dropbox/texas/public`. Then, navigate to the code folder on your command line and type `make`.

* Mac users should already have make.
* Windows users can install [chocolatey](https://chocolatey.org/install), and then type `choco install make` on the command line
   * Note that make cannot be run on Windows computers if there is a space in the file path. In this circumstance, the user can create a [junction](https://www.sevenforums.com/tutorials/278262-mklink-create-use-links-windows.html).

To run the code, make sure the following packages are installed in R: 
* data shaping packages: tidyverse, lubridate, readxl, pdftools, sf, sp, fst, tictoc, furrr, raster, lwgeom, gstat, rgdal
* tables and figures: knitr, grid, gridExtra, gtable, tigris, broom, kableExtra
* regressions: lmtest, sandwich, splines, lfe, modelr, Formula, grf, alpaca
To automatically install all required packages, run the `package_installation.R` program in the `code` folder.

### Raw Data:
* `leases`: This folder contains lease shapefiles.  The last lease transaction on active GLO leases in Texas can be found in the `Active` sub-folder, which was downloaded in January of 2017 from the General Land Office (GLO) [GIS database](http://www.glo.texas.gov/land/land-management/gis/).  The last lease transaction on inactive GLO leases is in the `Inactive` folder.  This data is not available online but was sent by the Texas General Land Office (GLO) in January of 2017. The `New` folder contains a recent download of the active and inactive leases from the GLO, which we use to determine most recent lease status information.
* `LeaseLandFile.csv`: Sent by GLO in February of 2017. This file consists of Control Numbers (administrative number used by the GLO) for each lease. The first two digits of the control number indicate the lease type. 
* `tablMineralLeaseAllInfo.csv`: Sent by GLO in February of 2017. This dataset consists of the information we have from inactive and active lease files, but also those leases that were not mapped by the GIS department (usually because they are old and irrelevant for the purposes of GLO). 
* `bids`: All bids (above the reserve price) for GLO auctions are available [online](http://www.glo.texas.gov/energy-business/oil-gas/mineral-leasing/leasing/index.html) as PDFs. The PDFs were manually entered in and saved as excel sheets. 
* `payments`: Bonus, rental payment and royalty payment information obtained through a public information requests in June of 2017 and mid 2019.
* `prices`: historical spot prices for production were downloaded in October, 2019 from the EIA. Oil prices can be found [here](https://www.eia.gov/dnav/pet/hist_xls/RWTCm.xls), and gas prices can be found [here](https://www.eia.gov/dnav/ng/hist_xls/RNGWHHDm.xls)
* `addenda.csv`: While GLO state leases are uniform, RAL leases often have additional addenda clauses. These RAL lease contract terms can be found in the public contracts [online](http://www.glo.texas.gov/history/archives/land-grants/index.cfm), and were inputted and categorized manually in August 2018.  This file represents our human-entered and cleaned dataset of the the underlying lease addenda data
* `assignments`: Our human-entered and cleaned dataset of the RAL and auctioned lease assignment events
* `company_aliases.csv`: Our database of company name strings and associated consistent company names, or "aliases"
* `coversheets`: All RAL/GLO contracts are [online](http://www.glo.texas.gov/history/archives/land-grants/index.cfm), and the vast majority of RAL leases should have a review sheet that contains information on the proposed terms of a lease. The terms from the review sheet were manually entered in in July 2017.  This folder includes a set of human-entered and cleaned datasets of these reviews, which we used to verify RAL lease characteristics and collect delay rental payment information
* `extra_ral_delay_rentals.xlsx`: a small set of additional human-entered and cleaned records for RAL delay rentals

### Shapefiles:
* `Shale_Plays and Shale thickness information`: Downloaded from EIA https://www.eia.gov/maps/layer_info-m.php. We selected out the major shale plays, Barnett, Haynesville, Spraberry (Permian), Delaware (Permian), and Eagle Ford.  Shale thickness information ("isopach" data) is only available for the Wolfberry formation (spanning both Spraberry and Delaware portions of the Permian), and parts of the Eagle Ford.
* `Land_Cover`: Downloaded in November 2017 from [here](https://www.mrlc.gov/). The original dataset includes the entire US. The raster file saved here was clipped in ArcGIS to just include Texas.
* `texas_grids_bounding_box`: A shapefile containing a bounding box which spans the initial PSF land allocation.
* `txdot-roads_tx`: Road data was downloaded in August 2017 from the Transportation Planning and Programming (TPP) Division of the Texas Department of Transportation (TxDOT), who maintain a spatial dataset of roadway polylines for planning and asset inventory purposes, found here https://tnris.org/data-catalog/entry/txdot-roadways/. This includes data associate with On-System highways, County Roads, Functional Classified City Streets, Toll Roads and Local Streets. 
* `usgs-rivers_tx`: Downloaded in September 2018 from the [U.S. Geological Survey](https://tinyurl.com/yddlyuuv). This National Hydrography Dataset is a comprehensive set of digital spatial data that encodes information about naturally occurring and constructed bodies of water, paths through which water flows, and related entities.
* `us_county`: Census county shapefiles downloaded in January 2017 from the census [website](https://www.census.gov/geo/maps-data/data/cbf/cbf_cousub.html)

### Intermediate Data

This folder contains datasets that involved manual entry *after* analysis to either create or modify. 
* `assignments`: We manually fixed around 200 assignment dates that were inputted incorrectly, especially where assignment dates are earlier than effective dates
* `glo_notices_final.csv`: GLO notices to the public that a parcel will go up for a leasing auction.  These are available from the same source as the bids, and we used a combination of R and manual input to read in the underlying PDFs 
* `leases`: manual corrections to lease variables 

### Private Data

* `parcels`: Public School Land parcel level data was purchased from P2E in December 2017. While we cannot publish the raw data, we have included the following code which cleans and analyzes the data: `Data_Cleaning/clean_parcels.R`, `Analysis/parcel_selection.R`, and `Analysis/parcel_stats.R`. 
* `DI wells`: We also have data from DrillingInfo on location of wells, which we use in the parcels cleaning process. If a user has access to DI data and would like to replicate how we use this dataset, we can email the user our code. 
