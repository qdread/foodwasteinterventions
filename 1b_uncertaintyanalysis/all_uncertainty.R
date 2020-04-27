# All uncertainty analysis

n_iter <- 1000 # Number of MC iterations in uncertainty analysis

# Load data for all interventions -----------------------------------------

library(tidyverse)
library(readxl)
library(zoo)
library(reticulate)
library(mc2d)

is_local <- dir.exists('Q:/')
fp <- ifelse(is_local, 'Q:', '/nfs/qread-data')
fp_github <- file.path(ifelse(is_local, '~/Documents/GitHub/foodwaste', '~'))

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


# Parameters for the interventions
intervention_params <- read_csv(file.path(fp, 'scenario_inputdata/intervention_parameters.csv'))

# Annuity function as implemented in excel, f and t are zero.
pmt <- function(p, r, n, f, t) (p * r * (1+r)^n  - f) / (((1+r)^n - 1) * (1 + r * t))

# Function to calculate demand change given baseline waste rate, waste reduction rate, and proportion of demand that is food
demand_change_fn <- function(W0, r, p) p * ((1 - W0) / (1 - (1 - r) * W0) - 1) + 1

# Source EEIO python function
if (!is_local) use_python('/usr/bin/python3')
source_python(file.path(fp_github, 'fwe/USEEIO/eeio_lcia.py'))

# Source functions for each intervention
source(file.path(fp_github, 'foodwasteinterventions/1b_uncertaintyanalysis/standardized_date_labeling_uncertainty.R'))
source(file.path(fp_github, 'foodwasteinterventions/1b_uncertaintyanalysis/spoilage_prevention_packaging_uncertainty.R'))
source(file.path(fp_github, 'foodwasteinterventions/1b_uncertaintyanalysis/consumer_education_campaigns_uncertainty.R'))

# Standardized date labeling ---------------------------------------

datelabel_costs <- read_xlsx(file.path(fp, 'scenario_inputdata/intervention_costs_26mar2020/costs for date labeling change_3-26-2020.xlsx'), skip = 9)

datelabel_costs_coord <- datelabel_costs[which(datelabel_costs[,1] == "Total costs for date labeling (with coordination)"), 2:4] %>% t %>% c %>% setNames(c('lower','mean','upper'))

# Set up PERT distributions for each parameter and draw from them.
datelabel_pars <- intervention_params %>%
  filter(Intervention %in% c('all', 'standardized date labeling'), !is.na(Parameter))
# Replace initial costs with the exact numbers
datelabel_pars[datelabel_pars$Parameter == 'initial_cost', c('minimum','mode','maximum')] <- as.list(datelabel_costs_coord)

datelabel_par_draws <- map_dfr(1:n_iter, function(i) {
  vals <- with(datelabel_pars, rpert(nrow(datelabel_pars), min = minimum, mode = mode, max = maximum, shape = 4))
  vals %>% setNames(datelabel_pars$Parameter) %>% t %>% as.data.frame
})

# Do the actual model fitting 1000 times
datelabel_results <- pmap(datelabel_par_draws, standardized_date_labeling)

# Combine results and calculate quantiles



# Spoilage prevention packaging -------------------------------------------

packaging_costs <- read_xlsx(file.path(fp, 'scenario_inputdata/intervention_costs_26mar2020/costs for intelligent packaging_3-26-2020.xlsx'), skip = 10, col_names = c('cost_type_2012_dollars', 'low', 'mean', 'high', 'notes')) 

packaging_annual_equipment_costs <- packaging_costs[packaging_costs$cost_type_2012_dollars %in% 'Total annual costs', 2:4]
#packaging_total_costs <- packaging_costs[packaging_costs$cost_type_2012_dollars %in% 'Total annualized and annual costs', 2:4]
packaging_initial_costs <- packaging_costs[packaging_costs$cost_type_2012_dollars %in% 'Total initial costs', 2:4]

##########
# LAFA rate conversion for the fruit and meat codes in LAFA.
# These are probably more than needed.
fruit_meat <- bea_codes %>% filter(stage %in% c('agriculture','processing'),
                                   fruit_veg_fresh > 0 | fruit_veg_processed > 0 | meat > 0 | fish_fresh > 0)

# We need baseline retail loss and consumer loss for fruits and meats
lafa <- list(veg, fruit, meat)

# find the single year closest to 2012.
lafa_df <- lafa %>%
  map2_dfr(c('veg', 'fruit','meat'), ~ select(.x, Category, Year, Retail_weight_Lbs.year, Loss_from_retail__institutional_to_consumer_level_Percent, Consumer_weight_Lbs.year,
                                              Loss_at_consumer_level_Other__cooking_loss_and_uneaten_food__Percent) %>%
             mutate(Group = .y)) %>%
  setNames(c('Food','Year','retail_weight','retail_loss','consumer_weight','avoidable_consumer_loss','Group')) %>%
  filter(!is.na(avoidable_consumer_loss)) %>%
  group_by(Group, Food) %>%
  filter(abs(Year - 2012) == min(abs(Year - 2012)))

lafa_df <- lafa_df %>% 
  ungroup %>%
  left_join(lafa_struct) %>%
  filter(subgroup2 == 'Fresh fruit' | subgroup1 %in% c('Fresh vegetables', 'Red meat', 'Poultry')) %>%
  mutate(group_final = if_else(Group == 'fruit', subgroup2, subgroup1)) %>%
  select(group_final, Food:avoidable_consumer_loss)

fruitmeat_wtdavg_rates <- lafa_df %>%
  group_by(group_final) %>%
  summarize(retail_loss = weighted.mean(retail_loss, retail_weight),
            avoidable_consumer_loss = weighted.mean(avoidable_consumer_loss, consumer_weight))

##### offsetting impacts for the plastic packaging materials code
# do this as per $1 spent on each category so it can be multiplied by the different costs.

plastic_packaging_code <- "326110/plastic bags, films, and sheets/us"
eeio_packaging_offsetting_impacts <- eeio_lcia('USEEIO2012', list(1), list(plastic_packaging_code))
eeio_packaging_offsetting_impacts <- data.frame(category = row.names(eeio_packaging_offsetting_impacts),
                                                impact = eeio_packaging_offsetting_impacts$Total)

# Set up PERT distributions for each parameter and draw from them.
packaging_pars <- intervention_params %>%
  filter(Intervention %in% c('all', 'spoilage prevention packaging'), !is.na(Parameter), !Parameter %in% 'baseline_beverage_rate')
# Replace initial costs with the exact numbers
packaging_pars[packaging_pars$Parameter == 'initial_cost', c('minimum','mode','maximum')] <- as.list(packaging_initial_costs)
packaging_pars[packaging_pars$Parameter == 'material_cost', c('minimum','mode','maximum')] <- as.list(packaging_annual_equipment_costs)

packaging_par_draws <- map_dfr(1:n_iter, function(i) {
  vals <- with(packaging_pars, rpert(nrow(packaging_pars), min = minimum, mode = mode, max = maximum, shape = 4))
  vals %>% setNames(packaging_pars$Parameter) %>% t %>% as.data.frame
})

# Do the actual model fitting 1000 times
packaging_results <- pmap(packaging_par_draws, spoilage_prevention_packaging)

# Combine results and calculate quantiles



# Consumer education campaigns --------------------------------------------

# Proportion of baseline demand affected is equal to the proportion of population who lives in the metropolitan areas that are big enough to get a targeted campaign
# Cost is the annual cost per campaign multiplied by the number of metropolitan areas that will have campaigns
# There are 1251 counties within metropolitan statistical areas, together representing 85.6% of the US pop
pop_in_metro <- 0.856 # see read_msas.R for derivation of this number.
n_metro_counties <- 1251

# Find offsetting impacts per $1 for each media type that can be applied within each iteration.
# For content development, use specialized design services 541400
# For media consulting, use advertising/PR services 541800
# For media, use a combination of magazine, radio/TV, and internet publishing (no newspaper)

media_codes <- c('541400', '541800', '511120', '515100', '519130')

media_codes_long <- all_codes$sector_desc_drc[match(media_codes, all_codes$sector_code_uppercase)]

consumer_ed_offset_eeio <- map(media_codes_long, ~ eeio_lcia('USEEIO2012', list(1), list(.))) # offset per dollar on each media industry

consumer_ed_offset_eeio <- map2_dfr(consumer_ed_offset_eeio, media_codes_long, ~ data.frame(BEA_code = .y,
                                                                                            category = row.names(.x),
                                                                                            impact = .x[,'Total']))


# Set up PERT distributions for each parameter and draw from them.
consumered_pars <- intervention_params %>%
  filter(Intervention %in% c('all', 'consumer education campaigns'), !is.na(Parameter), !is.na(minimum), !Parameter %in% c('annuity_years', 'annuity_rate'))

consumered_par_draws <- map_dfr(1:n_iter, function(i) {
  vals <- with(consumered_pars, rpert(nrow(consumered_pars), min = minimum, mode = mode, max = maximum, shape = 4))
  vals %>% setNames(consumered_pars$Parameter) %>% t %>% as.data.frame
})

# Do the actual model fitting 1000 times
consumered_results <- pmap(consumered_par_draws, consumer_education_campaigns)

# Combine results and calculate quantiles
