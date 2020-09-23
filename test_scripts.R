# Script to test whether preprocessing scripts result in the correct output.

# create_2012_bea.R
use07_read <- read.csv(file.path(fp_bea, 'use2007.csv'), check.names = FALSE, row.names = 1)
all.equal(as.data.frame(use07_agg_disagg), use07_read) # Yes 

make12_read <- read.csv(file.path(fp_bea, 'make2012.csv'), check.names = FALSE, row.names = 1)
all.equal(as.data.frame(make12_agg_disagg), make12_read) # Yes

# bea_naics_0712_crosswalk_create.R
bea_naics_read <- read_csv(file.path(fp_crosswalks, 'BEA_NAICS07_NAICS12_crosswalk.csv'), col_types = 'cccnn')
all.equal(bea_naics, bea_naics_read) # Yes, except for attributes lost when writing to CSV

# clean_qfahpd_data.R
qfahpd2_read <- read_csv(file.path(fp_data, 'intermediate_output/qfahpd2.csv'))
all.equal(qfahpd2, qfahpd2_read) # Yes, except for attributes lost when writing to CSV

# clean_susb_data.R
susb_summ_read <- read_csv(file.path(fp_data, 'intermediate_output/SUSB_NAICS_allvariables.csv'), col_types = 'ccfnnnnn')
all.equal(susb_summ, susb_summ_read) # Yes, except for attributes lost when writing to CSV

# create_bea_submatrix.R
level1and3_inputs_tohigherlevels_read <- read.csv(file.path(fp_data, 'intermediate_output/level13_to_level4678_inputs.csv'), check.names = FALSE, row.names = 1)
# The old outputs have different ordered columns but if sorted, they are the same.
# This is satisfactory because the columns are ultimately pivoted (and automatically sorted) in future analysis.
level1and3_inputs_tohigherlevels_sorted <- level1and3_inputs_tohigherlevels[, dimnames(level1and3_inputs_tohigherlevels_read)[[2]]]
all.equal(level1and3_inputs_tohigherlevels_sorted, level1and3_inputs_tohigherlevels_read) # Yes

# get_econ_census_2012.R
# This script is not guaranteed to continue to work because it downloads external data.
foodmfg2012_read <- read_csv(file.path(fp_rawdata, 'raw/econcensus2012_naics31.csv'), col_types = 'ccncc')
all.equal(foodmfg2012, foodmfg2012_read)# Yes, except for attributes lost when writing to CSV and rownames

# get_industry_BEA_codes.R (this output is not directly used in any further analysis anyway)
industries_joined_read <- read_csv(file.path(fp_rawdata, 'raw/intervention_industry_NAICS_BEA.csv'))
industries_BEA_read <- read_csv(file.path(fp_rawdata, 'raw/intervention_industry_BEA.csv'))
# The outputs are not in the same order 
all.equal(industries_joined %>% arrange(BEA_Code), industries_joined_read %>% arrange(BEA_Code)) # Yes
all.equal(industries_BEA %>% arrange(BEA_Code), industries_BEA_read %>% arrange(BEA_Code)) # Yes

# read_lafa.R works correctly. It is needed for the following 3 scripts

# write_lafa_cat_names.R
lafa_read <- read_csv(file.path(fp_crosswalks, 'lafa_category_structure.csv'))
all.equal(lafa, lafa_read) # Yes except for attributes lost when writing to CSV

# lafa_find_proportion_fresh_fruit_waste.R
# No output is saved; script produces the correct value of 0.34

# lafa_rate_conversion.R
# Intermediate
all_lafa_rates_read <- read_csv(file.path(fp_data, 'intermediate_output/lafa_rates_with_groups.csv'))
all.equal(ungroup(all_lafa_rates), all_lafa_rates_read) # Yes except for attributes lost when writing to CSV
# Final
qfahpd_waste_rates_prices_read <- read_csv(file.path(fp_data, 'intermediate_output/waste_rates_qfahpd.csv'))
bea_waste_rates_read <- read_csv(file.path(fp_data, 'intermediate_output/waste_rates_bea.csv'))
qfahpd_waste_rates_prices <- qfahpd_waste_rates_prices %>% select(-LAFA_names)
# QFAHPD new and old outputs may have different order, but if sorted should be equal.
all.equal(qfahpd_waste_rates_prices %>% arrange(QFAHPD_name), qfahpd_waste_rates_prices_read %>% arrange(QFAHPD_name)) # Yes except for attributes lost when writing to CSV
all.equal(ungroup(bea_waste_rates), bea_waste_rates_read) # Yes except for attributes lost when writing to CSV

# media_cost_breakdown.R has no output saved; script produces correct values

# read_costmodel.R
# Intermediate
costmodel_byproduct_read <- read_csv(file.path(fp_data, 'intermediate_output', 'costmodel_byproduct.csv'))
costmodel_bycosttype_read <- read_csv(file.path(fp_data, 'intermediate_output', 'costmodel_bycosttype.csv'))
costmodel_allcosts_read <- read_csv(file.path(fp_data, 'intermediate_output', 'costmodel_allcosts.csv'))
all.equal(costmodel_byproduct, costmodel_byproduct_read) # Yes down to attributes
all.equal(costmodel_bycosttype, costmodel_bycosttype_read) # Yes down to attributes
all.equal(costmodel_allcosts, costmodel_allcosts_read) # Yes down to attributes

# Final
costmodel_byproduct_joined <- costmodel_byproduct_summary %>%
  left_join(units_byproduct)
costmodel_byproduct_joined_read <- read_csv(file.path(fp_data, 'intermediate_output/packaging_costs_byproduct.csv'), col_types = 'ccccnnncn')
all.equal(costmodel_byproduct_joined, costmodel_byproduct_joined_read) # Yes down to attributes

# read_msas.R
# This script is not guaranteed to continue to work because it downloads external data.
# No output is saved but the script produces the correct value of 0.856