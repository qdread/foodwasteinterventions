# Control script
# This script will reproduce all analyses from the manuscript.

# Set all file paths ------------------------------------------------------

fp_github <- '.'
fp_out <- '.'
fp_data <- file.path(fp_github, 'data')
fp_rawdata <- '/nfs/qread-data/temp/data_for_fwi' # This can be changed later.
fp_crosswalks <- file.path(fp_data, 'crosswalks')

# Step 0. Preprocessing ---------------------------------------------------

# Create crosswalk to match BEA, 2007 NAICS, and 2012 NAICS codes and write to CSV.
source(file.path(fp_github, '0_preprocessing/bea_naics_0712_crosswalk_create.R'))

# Process raw Quarterly Food-at-Home Price Database data and write to CSV.
source(file.path(fp_github, '0_preprocessing/clean_qfahpd_data.R'))

# Process raw Statistics of US Businesses data and write to CSV.
source(file.path(fp_github, '0_preprocessing/clean_susb_data.R'))

# Extract subset of use table needed for "waste tracking & analytics" analysis and write to CSV.
source(file.path(fp_github, '0_preprocessing/create_bea_submatrix.R'))

# Download and process raw Economic Census data for NAICS codes beginning with 31 and write to CSV.
source(file.path(fp_github, '0_preprocessing/get_econ_census_2012.R'))

# Find the corresponding BEA codes for a manually created table of relevant industry NAICS codes for further analysis. Write to CSV.
source(file.path(fp_github, '0_preprocessing/get_industry_BEA_codes.R'))

#FIXME anything below here hasn't had its file paths cleaned up.

# Process raw Loss Adjusted Food Availability data and read them in as R data frames.
source(file.path(fp_github, '0_preprocessing/read_lafa.R'))

# Create a lookup table of food item names from the LAFA data and write to CSV.
source(file.path(fp_github, '0_preprocessing/write_lafa_cat_names.R'))

# Use LAFA data to estimate the proportion of fruit and vegetable waste that is fresh fruit.
source(file.path(fp_github, '0_preprocessing/lafa_find_proportion_fresh_fruit_waste.R'))

# Harmonize LAFA data with QFAHPD codes and BEA codes and calculate weighted average waste rates for those codes. Write to CSV.
source(file.path(fp_github, '0_preprocessing/lafa_rate_conversion.R'))

# Use manually input data from a published table to estimate the proportions of media costs by type.
source(file.path(fp_github, '0_preprocessing/media_cost_breakdown.R'))

# Extract necessary information from the FDA Reformulation Cost Model Excel files and write to CSV.
source(file.path(fp_github, '0_preprocessing/read_costmodel.R'))

# Use information from the Reformulation Cost Model to estimate the proportion of spoilage prevention packaging cost spent on each type of food.
source(file.path(fp_github, '0_preprocessing/proportion_packaging_foodtype.R'))

# Use list of Metropolitan Statistical Areas and download data from US Census Bureau to estimate percentage of the total population that lives in the MSAs.
source(file.path(fp_github, '0_preprocessing/read_msas.R'))

#### NOTE: The USEEIO model source code needed to run the script run_all_eeio2012.R is not provided here at least for now. 22 Sep 2020


# Step 1. Analysis, including uncertainty ---------------------------------

# This is the main script that does all the uncertainty analysis and writes the output to .RData files.
source(file.path(fp_github, '1_uncertaintyanalysis/all_uncertainty.R'))

# Process the .RData files from the previous step, calculating quantiles, and write all output to CSVs.
source(file.path(fp_github, '1_uncertaintyanalysis/all_uncertainty_quantiles.R'))

# Step 2. Visualization of results ----------------------------------------

# Read result CSVs, create main-text and supplemental figures, and write them to files.
source(file.path(fp_github, '2_vis/figs_for_pub.R'))

# Write Table 2 to a CSV as well as some additional output that's presented in the main body of the results section.
source(file.path(fp_github, '2_vis/savetables.R'))
