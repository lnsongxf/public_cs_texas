# To run this makefile, make sure you have a local data.txt that holds 
# the directory path to the data

# ===========================================================================
# establish directory paths
# ===========================================================================
# load in a local .txt file that holds the directory path to the data
ifeq ($(OS), Windows_NT)
	DATA = $(file < data.txt)
else
	DATA = $(shell cat data.txt)
endif

DIR = ${CURDIR}

RDIR = $(DIR)/code
TDIR = $(DIR)/output/tables
FDIR = $(DIR)/output/figures
EDIR = $(DIR)/output/estimates
PDIR = $(DIR)/output/press

RDIR_clean = $(RDIR)/Data_Cleaning
RDIR_analysis = $(RDIR)/Analysis
RDIR_functions = $(RDIR)/functions

# data files
DATA_gen = $(DATA)/generated_data
DATA_gen_shape = $(DATA)/generated_shape_files
DATA_int = $(DATA)/intermediate_data
DATA_raw = $(DATA)/raw_data
DATA_shape = $(DATA)/shape_files

# Name of final files
DATA_final = $(DATA_gen)/final_leases.Rda
PAPER = $(DIR)/writeups/cs_texas

INSTALL := $(shell Rscript $(RDIR)/package_installation.R)
all: $(PAPER).pdf


# ============================================================================
# Check that PSF data files exist if not, make skips this section
# If user does not have PSF data, make sure the repo has the following files:
# output/tables/parcel_balance.tex
# output/tables/parcel_balance_leaseable.tex
# output/tables/reg_parcel_leased.tex 
# output/figures/survival.png
# ============================================================================
chk := $(wildcard $(DATA_shape)/PSF/* $(DATA_int)/pden_* $(DATA_int)/*key.Rda)
psf := $(wildcard $(DATA_shape)/PSF/Chicago_PSF_180207.gdb/*)
lc := $(wildcard $(DATA_shape)/Land_Cover/landcover/*)
lc_info := $(wildcard $(DATA_shape)/Land_Cover/info/*)

ifneq ($(chk), )
$(DATA_gen_shape)/psf.Rda: \
	$(RDIR_clean)/psf_conversion.R \
	$(psf)
	Rscript $<

$(TDIR)/parcel_balance.tex \
	$(TDIR)/parcel_balance_leaseable.tex \
	$(TDIR)/reg_parcel_leased.tex \
	$(FDIR)/survival.png: \
	$(RDIR_analysis)/parcel_stats.R \
	$(DATA_gen)/final_parcels.Rda \
	$(DATA_final)
	Rscript $<

$(DATA_gen)/final_parcels.Rda: \
	$(RDIR_analysis)/parcel_selection.R \
	$(DATA_gen)/final_leases.Rda \
	$(DATA_gen)/state_lease_parcel.Rda \
	$(DATA_gen)/state_parcels_map.Rda 
	Rscript $<

$(DATA_gen)/state_lease_parcel.Rda \
	$(DATA_gen)/state_parcels_map.Rda: \
	$(RDIR_clean)/clean_parcels.R \
	$(RDIR_clean)/infrastructure_parcels.R \
	$(RDIR_clean)/landcover_parcels.R \
	$(DATA_gen_shape)/psf.Rda \
	$(DATA_gen_shape)/roads.Rda \
	$(DATA_gen_shape)/waterbodies.Rda \
	$(DATA_gen_shape)/river_streams.Rda \
	$(DATA_gen_shape)/shale_plays.Rda \
	$(DATA_gen_shape)/isopach_permian_interpolate.Rda \
	$(DATA_gen_shape)/isopach_eagle_ford_interpolate.Rda \
	$(DATA_gen_shape)/texas_grid5.Rda \
	$(DATA_gen_shape)/texas_grid10.Rda \
	$(DATA_gen_shape)/texas_grid20.Rda \
	$(DATA_raw)/leases/Active/ActiveLeases.shp \
	$(DATA_raw)/leases/Inactive/InActive_OilGas_Leases.shp \
	$(DATA_raw)/leases/LeaseLandFile.csv \
	$(DATA_int)/index_key.Rda \
	$(DATA_int)/pden_desc_arranged.fst \
	$(lc) $(lc_info)
	Rscript $<

$(DATA_gen)/dropped.Rda: \
	$(RDIR_clean)/clean_glo_missing.R \
	$(DATA_raw)/leases/Active/ActiveLeases.shp \
	$(DATA_raw)/leases/Inactive/InActive_OilGas_Leases.shp \
	$(DATA_raw)/leases/LeaseLandFile.csv \
	$(DATA_raw)/leases/tblMineralLeaseAllInfo.csv \
	$(DATA_gen_shape)/psf.Rda
	Rscript $<

endif

# ===========================================================================
# Read in non-lease and non-parcel shapefiles and save as .Rda files
# ===========================================================================
isopach := $(wildcard $(DATA_shape)/TightOil_ShaleGas_IndividualPlays_Lower48_EIA/*Isopach*.shp)
$(DATA_gen_shape)/roads.Rda \
	$(DATA_gen_shape)/waterbodies.Rda \
	$(DATA_gen_shape)/river_streams.Rda \
	$(DATA_gen_shape)/shale_plays.Rda \
	$(DATA_gen_shape)/isopach_permian_interpolate.Rda \
	$(DATA_gen_shape)/isopach_eagle_ford_interpolate.Rda \
	$(DATA_gen_shape)/texas_grid5.Rda \
	$(DATA_gen_shape)/texas_grid10.Rda \
	$(DATA_gen_shape)/texas_grid20.Rda: \
	$(RDIR_clean)/rda_conversion.R \
	$(DATA_shape)/txdot-roads_tx/txdot-2015-roadways_tx.shp \
	$(DATA_shape)/usgs-rivers_tx/Waterbodies/Waterbodies.shp \
	$(DATA_shape)/usgs-rivers_tx/Rivers_Streams/Rivers_Streams.shp \
	$(isopach) \
	$(DATA_shape)/TightOil_ShalePlays_US_EIA_Jan2019/TightOil_ShalePlays_US_EIA_Jan2019.shp \
	$(DATA_shape)/texas_grids_bounding_box/texas_grids_bounding_box.shp
	Rscript $<

# ===========================================================================
# clean lease assignments and coversheets, which are needed to clean the 
# lease data itself
# ===========================================================================
$(DATA_gen)/assignments.Rda: $(RDIR_clean)/clean_assignments.R \
	$(DATA_raw)/assignments/glo_assignments.csv \
	$(DATA_int)/assignments/partial_assignments.csv \
	$(DATA_int)/assignments/glo_assignments_fix.csv
	Rscript $<

$(DATA_gen)/recommended_rentals.Rda \
	$(DATA_gen)/coversheets.Rda: \
	$(RDIR_clean)/clean_coversheets.R \
	$(DATA_raw)/coversheets/Final_Term_Sheet.xlsx \
	$(DATA_raw)/coversheets/Highlighted_Terms.xlsx
	Rscript $<

# ===========================================================================
# Clean Lease Related Files
# ===========================================================================
$(DATA_gen)/leases_state.Rda: $(RDIR_clean)/clean_leases.R \
	$(RDIR_clean)/infrastructure_leases.R \
	$(DATA_raw)/leases/Active/ActiveLeases.shp \
	$(DATA_raw)/leases/Inactive/InActive_OilGas_Leases.shp \
	$(DATA_raw)/leases/dropped_leases/dropped_leases.shp \
	$(DATA_raw)/leases/dropped_leases/dropped_leases_fields.csv \
	$(DATA_raw)/company_aliases.csv \
	$(DATA_shape)/us_county/cb_2015_us_county_500k.shp \
	$(DATA_gen)/glo_bids.Rda \
	$(DATA_gen)/coversheets.Rda \
	$(DATA_gen)/glo_rentals.Rda \
	$(DATA_gen)/delay_rentals.Rda \
	$(DATA_gen)/assignments.Rda \
	$(DATA_gen_shape)/shale_plays.Rda \
	$(DATA_gen_shape)/isopach_permian_interpolate.Rda \
	$(DATA_gen_shape)/isopach_eagle_ford_interpolate.Rda \
	$(DATA_gen_shape)/roads.Rda \
	$(DATA_gen_shape)/waterbodies.Rda \
	$(DATA_gen_shape)/river_streams.Rda \
	$(DATA_raw)/leases/LeaseLandFile.csv \
	$(DATA_int)/leases/missing_bonus.csv \
	$(DATA_int)/leases/geom_problems.csv \
	$(DATA_int)/leases/lease_check.csv \
	$(DATA_int)/leases/bonus_reconciliation.csv
	Rscript $<

$(DATA_gen)/landcover_lease.Rda: $(RDIR_clean)/landcover_leases.R \
	$(DATA_gen)/leases_state.Rda \
	$(lc) $(lc_info)
	Rscript $<

bids := $(wildcard $(DATA_raw)/bids/Final_GLO*)
$(DATA_gen)/glo_bids.Rda: \
	$(RDIR_clean)/clean_bids.R \
	$(bids)
	Rscript $<

$(DATA_gen)/delay_rentals.Rda: \
	$(RDIR_clean)/clean_delay_rentals.R \
	$(DATA_raw)/auction_delay_rentals.xlsx \
	$(DATA_gen)/recommended_rentals.Rda \
	$(DATA_raw)/extra_ral_delay_rentals.xlsx
	Rscript $<

$(DATA_gen)/lease_statuses.Rda: \
	$(RDIR_clean)/clean_lease_status.R \
	$(DATA_raw)/leases/Active/ActiveLeases.shp \
	$(DATA_raw)/leases/Inactive/InActive_OilGas_Leases.shp \
	$(DATA_raw)/leases/New/Active/ActiveLeases.shp \
	$(DATA_raw)/leases/New/Inactive/InactiveLeases.shp
	Rscript $<

# ===========================================================================
# Clean Revenue Related Files
# ===========================================================================
payments := $(wildcard $(DATA_raw)/payments/Royalty*)
$(DATA_gen)/glo_production_revenues.Rda: \
	$(RDIR_clean)/clean_glo_production_revs.R \
	$(DATA_gen)/leases_state.Rda \
	$(DATA_gen)/prices.Rda \
	$(payments)
	Rscript $<

$(DATA_gen)/glo_rentals.Rda: $(RDIR_clean)/clean_glo_rental_payments.R \
	$(DATA_raw)/payments/rentals.xlsx
	Rscript $<

$(DATA_gen)/prices.Rda: $(RDIR_clean)/clean_prices.R \
	$(DATA_raw)/prices/RNGWHHDm.xls \
	$(DATA_raw)/prices/RWTCm.xls
	Rscript $<

# ===========================================================================
# Create Analysis Dataset
# ===========================================================================
$(DATA_final): $(RDIR_analysis)/lease_selection.R \
	$(DATA_gen)/coversheets.Rda \
	$(DATA_gen)/leases_state.Rda \
	$(DATA_gen)/glo_production_revenues.Rda \
	$(DATA_raw)/addenda.csv \
	$(DATA_gen)/lease_statuses.Rda \
	$(DATA_gen)/landcover_lease.Rda 
	Rscript $<


# ===========================================================================
# Figures and Tables Output
# ===========================================================================
$(FDIR)/glo_leases_in_texas.png $(FDIR)/sample_glo_leases.png: \
	$(RDIR_analysis)/leases_maps.R \
	$(DATA_gen)/leases_state.Rda \
	$(DATA_shape)/us_county/cb_2015_us_county_500k.shp \
	$(DATA_shape)/TightOil_ShalePlays_US_EIA_Jan2019/TightOil_ShalePlays_US_EIA_Jan2019.shp \
	$(DATA_gen_shape)/texas_grid10.Rda
	Rscript $<

# descriptive statistics for leases
$(EDIR)/Nsample_RAL_RAW.tex \
	$(EDIR)/Nsample_ALL_RAW.tex \
	$(EDIR)/Nsample_RAL_AUCTION_RAW.tex \
	$(EDIR)/Nsample_STATE_RAW.tex \
	$(EDIR)/Nsample_NEGOTIATION_CLEAN.tex \
	$(EDIR)/Nsample_AUCTION_CLEAN.tex \
	$(EDIR)/Nsample_AUCTION_NOTHICK.tex \
	$(EDIR)/Nsample_NEGOTIATION_NOTHICK.tex \
	$(TDIR)/summary_stats_by_type.tex \
	$(FDIR)/cohorts.png \
	$(FDIR)/summary_data_construction.tex \
	$(PDIR)/cohorts_data.csv: \
	$(RDIR_analysis)/lease_stats.R \
	$(DATA_final)
	Rscript $<


# bonus regressions
$(TDIR)/logbonus_regressions.tex $(TDIR)/bonus_regressions_discussion.tex \
	$(TDIR)/bonus_regressions.tex \
	$(TDIR)/logbonus_regressions_discussion.tex \
	$(EDIR)/Bonus_Grid10_log.tex \
	$(EDIR)/Bonus_Grid10Yr_log.tex \
	$(EDIR)/Bonus_Grid10Yr_log_total.tex \
	$(EDIR)/negotiation_avg_bonus.tex: \
	$(RDIR_analysis)/regressions_bonus.R \
	$(DATA_final)
	Rscript $<

# output regressions
$(TDIR)/stacked_output_levels.tex \
	$(TDIR)/stacked_output_poisson.tex \
	$(TDIR)/drilled_regressions.tex \
	$(TDIR)/sellerrevenue_regressions.tex \
	$(TDIR)/log_sellerrevenue_regressions.tex \
	$(TDIR)/log_bonus_poissonsample_regressions.tex \
	$(TDIR)/log_sellerrevenue_poissonsample_regressions.tex \
	$(EDIR)/negotiation_avg_dboe.tex \
	$(EDIR)/negotiation_avg_revenue.tex \
	$(EDIR)/Poisson_LeaseRevenue_Grid10Yr.tex \
	$(EDIR)/SellerRevenue_Grid10Yr_total.tex: \
	$(RDIR_analysis)/regressions_output.R \
	$(DATA_final)
	Rscript $<

$(TDIR)/allocative.tex: $(RDIR_analysis)/allocative_diffs.R \
	$(DATA_final)
	Rscript $<

$(TDIR)/firms_regressions.tex: $(RDIR_analysis)/regressions_firms.R \
	$(DATA_final)
	Rscript $<

$(TDIR)/rentals_regressions.tex \
	$(TDIR)/rentals_regressions_contracted.tex: \
	$(RDIR_analysis)/regressions_delay_rentals.R \
	$(DATA_final)
	Rscript $<

$(TDIR)/auction_number_bids.tex $(TDIR)/TopPairAuctionShares.tex: \
	$(RDIR_analysis)/auction_summary.R \
	$(DATA_int)/glo_notices_final.csv \
	$(DATA_gen)/glo_bids.Rda \
	$(DATA_final)
	Rscript $<

$(TDIR)/bonus_regressions_addenda.tex: \
	$(RDIR_analysis)/regressions_bonus_raladdenda.R
	Rscript $<

$(TDIR)/causal_forests.tex: $(RDIR_analysis)/causal_forests.R \
	$(DATA_final)
	Rscript $<

# ===========================================================================
# Function dependencies
# ===========================================================================
$(RDIR_analysis)/parcel_stats.R \
	$(RDIR_analysis)/parcel_selection.R \
	$(RDIR_clean)/clean_parcels.R \
	$(RDIR_clean)/psf_conversion.R \
	$(RDIR_clean)/rda_conversion.R \
	$(RDIR_clean)/clean_leases.R \
	$(RDIR_clean)/landcover_leases.R \
	$(RDIR_clean)/clean_bids.R \
	$(RDIR_clean)/clean_delay_rentals.R \
	$(RDIR_clean)/clean_lease_status.R \
	$(RDIR_clean)/clean_glo_production_revs.R \
	$(RDIR_clean)/clean_glo_rental_payments.R \
	$(RDIR_clean)/clean_prices.R \
	$(RDIR_analysis)/lease_selection.R \
	$(RDIR_analysis)/leases_maps.R \
	$(RDIR_analysis)/lease_stats.R \
	$(RDIR_analysis)/regressions_bonus.R \
	$(RDIR_analysis)/regressions_output.R \
	$(RDIR_analysis)/allocative_diffs.R \
	$(RDIR_analysis)/regressions_firms.R \
	$(RDIR_analysis)/auction_summary.R \
	$(RDIR_analysis)/regressions_bonus_raladdenda.R \
	$(RDIR_analysis)/causal_forests.R: \
	$(RDIR)/functions/utils.R \
	$(RDIR)/functions/random_forest_utils.R \
	$(RDIR)/functions/regtable.R \
	$(RDIR)/functions/latex_number.R \
	$(RDIR)/texas_constants.R \
	$(RDIR)/paths.R


# ===========================================================================
# Output final paper
# ===========================================================================
$(PAPER).pdf: $(PAPER).tex \
	$(FDIR)/cohorts.png \
	$(FDIR)/glo_leases_in_texas.png \
	$(FDIR)/survival.png \
	$(FDIR)/sample_glo_leases.png \
	$(EDIR)/Bonus_Grid10Yr_log.tex \
	$(EDIR)/Bonus_Grid10_log.tex \
	$(EDIR)/Bonus_Grid10Yr_log_total.tex \
	$(EDIR)/Poisson_LeaseRevenue_Grid10Yr.tex \
	$(EDIR)/SellerRevenue_Grid10Yr_total.tex \
	$(EDIR)/Nsample_RAL_RAW.tex \
	$(EDIR)/Nsample_RAL_AUCTION_RAW.tex \
	$(EDIR)/Nsample_STATE_RAW.tex \
	$(EDIR)/Nsample_NEGOTIATION_CLEAN.tex \
	$(EDIR)/Nsample_AUCTION_CLEAN.tex \
	$(EDIR)/Nsample_NEGOTIATION_NOTHICK.tex \
	$(EDIR)/Nsample_AUCTION_NOTHICK.tex \
	$(EDIR)/negotiation_avg_bonus.tex \
	$(EDIR)/negotiation_avg_revenue.tex \
	$(EDIR)/negotiation_avg_dboe.tex \
	$(TDIR)/summary_stats_by_type.tex \
	$(TDIR)/parcel_balance.tex \
	$(TDIR)/logbonus_regressions.tex \
	$(TDIR)/logbonus_regressions_discussion.tex \
	$(TDIR)/reg_parcel_leased.tex \
	$(TDIR)/stacked_output_levels.tex \
	$(TDIR)/stacked_output_poisson.tex \
	$(TDIR)/sellerrevenue_regressions.tex \
	$(TDIR)/allocative.tex \
	$(TDIR)/firms_regressions.tex \
	$(TDIR)/TopPairAuctionShares.tex \
	$(TDIR)/parcel_balance_leaseable.tex \
	$(TDIR)/bonus_regressions.tex \
	$(TDIR)/bonus_regressions_discussion.tex \
	$(TDIR)/log_sellerrevenue_regressions.tex \
	$(TDIR)/log_bonus_poissonsample_regressions.tex \
	$(TDIR)/log_sellerrevenue_poissonsample_regressions.tex \
	$(TDIR)/drilled_regressions.tex \
	$(TDIR)/causal_forests.tex \
	$(TDIR)/rentals_regressions.tex \
	$(TDIR)/rentals_regressions_contracted.tex \
	$(TDIR)/auction_number_bids.tex \
	$(FDIR)/summary_data_construction.tex \
	$(TDIR)/bonus_regressions_addenda.tex
	cd writeups && pdflatex $< && bibtex cs_texas.aux && \
	pdflatex $< && pdflatex $<





