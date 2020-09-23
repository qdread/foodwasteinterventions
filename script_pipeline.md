# Script pipeline for food waste interventions work

The scripts should all be in the github repo `foodwasteinterventions` though some pre-processing scripts will probably still be in the `fwe` repository.

## Input data needed (and scripts where they are created)

- crossreference_tables/naics_crosswalk_final.csv (BEA codes)
  + C:\Users\qread\Documents\GitHub\foodwaste\fwe\USEEIO\partial_sector_proportions.r
  + Some post processing was done manually for this. 
- crossreference_tables/all_codes.csv (BEA codes with full text names)
  + fwe/read_data/all_demand_codes.r
  + this is also an output of the USEEIO model which we describe building -- just a lookup table.
- USEEIO/useeiopy/Model Builds/USEEIO2012/USEEIO2012_FinalDemand.csv (final demand vector from USEEIO model)
  + This is an output of the USEEIO model which we describe building
- crossreference_tables/waste_rates_bea.csv
  + 0_preprocessing/lafa_rate_conversion.R
- crossreference_tables/level13_to_level4678_inputs.csv (food_U)
  + 0_preprocessing/create_bea_submatrix.R
- crossreference_tables/lafa_category_structure.csv
  + 0_preprocessing/write_lafa_cat_names.R
- csv_exports/SUSB_NAICS_allvariables.csv
  + 0_preprocessing/clean_susb_data.R
- crossreference_tables/BEA_NAICS07_NAICS12_crosswalk.csv
  + 0_preprocessing\bea_naics_0712_crosswalk_create.r
- crossreference_tables/bea_qfahpd_crosswalk.csv
  + created manually
- crossreference_tables/qfahpd_lafa_crosswalk.csv
  + created manually
- raw_data/USDA/QFAHPD/tidy_data/qfahpd2.csv
  + 0_preprocessing/clean_qfahpd_data.R
- scenario_inputdata/intervention_parameters.csv
  + created manually
- eeio_all_industries.csv
  + 0_preprocessing/run_all_eeio2012.R
- scenario_inputdata/packaging_costs_byproduct.csv
  + 0_preprocessing/read_costmodel.R
- scenario_inputdata/ReFED-Data-Set.xlsx
  + downloaded directly from ReFED

## Pipeline

### Scripts outside the repo

- read_lafa.R (read LAFA data from the XLS files)

### Scripts in 0_preprocessing folder

- create_bea_submatrix.R (create submatrix of use table needed for the WTA intervention)
- write_lafa_cat_names.R (create lookup table of LAFA category names)
- clean_susb_data.R (create table of establishments and receipts from SUSB, grouped by NAICS and BEA codes)
- bea_naics_0712_crosswalk_create.R (reformat raw BEA data to create a crosswalk between BEA codes, NAICS 2007 codes, and NAICS 2012 codes)
- clean_qfahpd_data.R (tidy up the raw data from QFAHPD 2004-2010)
- get_econ_census_2012.R (get some economic census data by NAICS code from the US Census Bureau API)
- lafa_rate_conversion.R (harmonize LAFA, QFAHPD, and BEA to get waste rates for BEA codes from LAFA categories)
- get_industry_BEA_codes.R (identify six digit codes for industries that generate offsetting impacts, which are entered manually later on in the process)
- lafa_find_proportion_fresh_fruit_waste.R (confirm one of the assumptions about what percentage of consumer fruit and vegetable waste is specifically fresh fruit)
- media_cost_breakdown.R (check some assumptions about what industries generate offsetting impacts of media campaigns, and in what proportion)
- read_costmodel.R (convert the raw cost model output into the tidied output packaging_costs_byproduct.csv)
- read_lafa.R (originally created within the FWE project; reads raw LAFA XLS files)
- read_msas.R (read data on metropolitan statistical areas to get the population proportion number)
- run_all_eeio2012.R (environmental impact of $1 demand from each industry in the USEEIO model)

Subfolder `input_output_tables`:

- reaggregate_mat.R (source code for functions that retotal rows and columns of make and use tables for many-to-one and one-to-many mappings)
- create_2012_bea.R (script that implements the mapping to produce a 2012 make and use table in the correct format to be used in the USEEIO input-output model)

### Scripts in 1_uncertaintyanalysis folder

- all_uncertainty.R (calls all_uncertainty_setup.R and the four functions, one for each intervention)
- all_uncertainty_quantiles.R (pulls quantiles from all the results in previous step and saves them)

### Scripts in 2_vis folder

- figs_for_pub.R (creates the 2 main-text figures and the 6 supplemental figures)
- savetables.R (creates Table 2, which presents results in the manuscript, and some additional results presented as text in the main body of the manuscript)