# Script to create submatrix of BEA use table where rows are primary food industries and columns are retail food and foodservice industries
# QDR / FWI / 3 Jun 2020

# modified from an older script written in 2018 for the original FWE project

library(tidyverse)

# Load lookup table and use table

fp_crosswalk <- ifelse(dir.exists('Q:/'), 'Q:/crossreference_tables', '/nfs/qread-data/crossreference_tables')
fp_bea <- ifelse(dir.exists('Q:/'), 'Q:/raw_data/BEA', '/nfs/qread-data/raw_data/BEA')

naics_lookup <- read.csv(file.path(fp_crosswalk, 'naics_crosswalk_final.csv'), stringsAsFactors = FALSE)

U <- read.csv(file.path(fp_bea, 'formatted/use2012.csv'), stringsAsFactors = FALSE, check.names = FALSE, row.names = 1)

# Proportions FSC by sector -----------------------------------------------

# For each of the food service and institutional categories, calculate the proportion of intermediate inputs for that sector that is from the "earlier" FSC stages.

# Modified 07 June 2019: use most up to date proportions from lookup table

# Get the index of the intermediate rows in the use table
U_commod_row <- dimnames(U)[[1]][1:(which(dimnames(U)[[1]] == 'T005') - 1)]

# Weighting of FAO food categories within sectors ------------------------------

# Level 1 to Level 3 inputs

level3_fsc_sectors <- naics_lookup %>% filter(stage %in% 'processing') %>% pull(BEA_389_code)
level1_fsc_sectors <- naics_lookup %>% filter(stage %in% 'agriculture') %>% pull(BEA_389_code)

# Level 1 and 3 to levels 4 and above inputs
later_sectors <- naics_lookup %>% filter(stage %in% c('foodservice', 'institutional', 'retail'), proportion_food > 0) %>% pull(BEA_389_code) # 39 later sectors

level1and3_inputs_tohigherlevels <- U[c(level1_fsc_sectors, level3_fsc_sectors), later_sectors]

# Added 10 Feb. 2020: write this level 1 and 3 to levels 4 and up matrix to a CSV for use in later analysis.
write.csv(level1and3_inputs_tohigherlevels, file.path(fp_crosswalk, 'level13_to_level4678_inputs.csv'))