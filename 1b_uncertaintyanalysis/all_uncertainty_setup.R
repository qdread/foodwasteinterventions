# Modular script to load all files for uncertainty analyses

library(tidyverse)
library(readxl)
library(zoo)
library(reticulate)
library(mc2d)
library(furrr)

options(mc.cores = 8) # Use all cores on a single node.
plan(multicore)

# Load the BEA code data (erroneously called NAICS) to get the codes
bea_codes <- read_csv(file.path(fp, 'crossreference_tables/naics_crosswalk_final.csv'))

# Demand codes table to convert 6 digit codes to the ones used by USEEIO
all_codes <- read_csv(file.path(fp, 'crossreference_tables/all_codes.csv'))

# Read 2012 final demand vector from built USEEIO model
finaldemand2012 <- read_csv(file.path(fp_github, 'USEEIO/useeiopy/Model Builds/USEEIO2012/USEEIO2012_FinalDemand.csv'))

# Load waste rates for the BEA codes, calculated in lafa_rate_conversion.R
bea_waste_rates <- read_csv(file.path(fp, 'crossreference_tables/waste_rates_bea.csv'))

# BEA levels 1+3 to 4+6+7+8 is already subsetted from an older analysis I did.
food_U <- read.csv(file.path(fp, 'crossreference_tables/level13_to_level4678_inputs.csv'), row.names = 1, check.names = FALSE)

# Load LAFA
source(file.path(fp_github, 'fwe/read_data/read_lafa.r'))

# Read the description of LAFA's nested category structure in.
lafa_struct <- read_csv(file.path(fp, 'crossreference_tables/lafa_category_structure.csv'))

# Load numbers of establishments by NAICS
susb_naics <- read_csv(file.path(fp, 'csv_exports/SUSB_NAICS_allvariables.csv'))

# Load NAICS BEA crosswalks
load(file.path(fp, 'crossreference_tables/NAICS_BEA_SCTG_crosswalk.RData'))
bea_naics <- read_csv(file.path(fp, 'crossreference_tables/BEA_NAICS07_NAICS12_crosswalk.csv'))

# Load data for waste rate calculations
# Mapping will need to go bea --> qfahpd --> lafa
# Load the two necessary crosswalks.
bea2qfahpd <- read_csv(file.path(fp, 'crossreference_tables/bea_qfahpd_crosswalk.csv'))
qfahpd2lafa <- read_csv(file.path(fp, 'crossreference_tables/qfahpd_lafa_crosswalk.csv'))

# Also load the QFAHPD data so that we can get the prices.
qfahpd2 <- read_csv(file.path(fp, 'raw_data/USDA/QFAHPD/tidy_data/qfahpd2.csv'))

# Parameters for the interventions
intervention_params <- read_csv(file.path(fp, 'scenario_inputdata/intervention_parameters.csv'))

# Read pre-calculated EEIO
eeio_df <- read_csv(file.path(fp_out, 'eeio_all_industries.csv'))

# Annuity function as implemented in excel, f and t are zero.
pmt <- function(p, r, n, f, t) (p * r * (1+r)^n  - f) / (((1+r)^n - 1) * (1 + r * t))

# Function to calculate demand change given baseline waste rate, waste reduction rate, and proportion of demand that is food
demand_change_fn <- function(W0, r, p) p * ((1 - W0) / (1 - (1 - r) * W0) - 1) + 1

# Source PERT function
source(file.path(fp_github, 'foodwasteinterventions/get_pert.R'))

# Source functions for each intervention
source(file.path(fp_github, 'foodwasteinterventions/1b_uncertaintyanalysis/standardized_date_labeling_uncertainty.R'))
source(file.path(fp_github, 'foodwasteinterventions/1b_uncertaintyanalysis/spoilage_prevention_packaging_uncertainty.R'))
source(file.path(fp_github, 'foodwasteinterventions/1b_uncertaintyanalysis/consumer_education_campaigns_uncertainty.R'))
source(file.path(fp_github, 'foodwasteinterventions/1b_uncertaintyanalysis/waste_tracking_analytics_uncertainty.R'))
