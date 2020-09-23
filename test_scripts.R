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
foodmfg2012_read <- read_csv(file.path(fp_rawdata, 'raw/econcensus2012_naics31.csv'), col_types = 'ccncc')
all.equal(foodmfg2012, foodmfg2012_read)# Yes, except for attributes lost when writing to CSV and rownames

# get_industry_BEA_codes.R (this output is not directly used in any further analysis anyway)
industries_joined_read <- read_csv(file.path(fp_rawdata, 'raw/intervention_industry_NAICS_BEA.csv'))
industries_BEA_read <- read_csv(file.path(fp_rawdata, 'raw/intervention_industry_BEA.csv'))
# The outputs are not in the same order 
all.equal(industries_joined %>% arrange(BEA_Code), industries_joined_read %>% arrange(BEA_Code)) # Yes
all.equal(industries_BEA %>% arrange(BEA_Code), industries_BEA_read %>% arrange(BEA_Code)) # Yes
