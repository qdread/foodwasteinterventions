# Standardized date labeling analysis
# Split into a separate script by QDR, 23 April 2020

# Get them from the document

library(tidyverse)
library(readxl)
library(zoo)
library(reticulate)

is_local <- dir.exists('Q:/')
fp <- ifelse(is_local, 'Q:', '/nfs/qread-data')
fp_github <- file.path(ifelse(is_local, '~/Documents/GitHub/foodwaste', '~'))

# Load the BEA code data (erroneously called NAICS) to get the codes
bea_codes <- read_csv(file.path(fp, 'crossreference_tables/naics_crosswalk_final.csv'))

# Demand codes table to convert 6 digit codes to the ones used by USEEIO
all_codes <- read_csv(file.path(fp, 'crossreference_tables/all_codes.csv'))

datelabel_costs <- read_xlsx(file.path(fp, 'scenario_inputdata/intervention_costs_26mar2020/costs for date labeling change_3-26-2020.xlsx'), skip = 9)

#datelabel_costs_nocoord <- datelabel_costs[which(datelabel_costs[,1] == "Total costs for date labeling (w/o coordination)"), 2:4] %>% t %>% c %>% setNames(c('lower','mean','upper'))
datelabel_costs_coord <- datelabel_costs[which(datelabel_costs[,1] == "Total costs for date labeling (with coordination)"), 2:4] %>% t %>% c %>% setNames(c('lower','mean','upper'))

# We have one-time costs but need to annualize using 5 years at 7% interest as was done for waste tracking.

# Annuity function as implemented in excel, f and t are zero.
pmt <- function(p, r, n, f, t) (p * r * (1+r)^n  - f) / (((1+r)^n - 1) * (1 + r * t))

#(datelabel_costs_nocoord_annual <- pmt(datelabel_costs_nocoord, r = 0.07, n = 5, f = 0, t = 0)) # 350m to 1.4b
(datelabel_costs_coord_annual <- pmt(datelabel_costs_coord, r = 0.07, n = 5, f = 0, t = 0)) # 35m to 283m. This matches Mary's calculations.

#### 
# Waste reduction rates from date labeling standardization, and which categories it acts on.
consumer_response <- c(lower = 0.05, upper = 0.10, mean = 0.075)
# Refed also assumes 20% of avoidable household waste is due to confusion over expiration dates (this seems high so set it as an upper bound)
proportion_confusion_waste <- c(lower = 0.10, upper = 0.20, mean = 0.15)

# If baseline avoidable household rate is ~ 20 to 25 percent, then the baseline confusion rate is 0.20 * 0.20 = 0.04. If 5 to 10 percent of consumers 
# change their behavior, the post-intervention confusion rate is 3.6 to 3.8%.

# So we can use the baseline confusion rate for the baseline waste demand, then the post-intervention rate, for all households.
# 0.002 to 0.004 of all final consumer food demand, roughly. That actually seems pretty plausible.

# Using all final demand from levels 1 and 3, multiplied by the appropriately adjusted pre-intervention and post-intervention "confusion waste rate".
finaldemand2012 <- read_csv(file.path(fp_github, 'USEEIO/useeiopy/Model Builds/USEEIO2012/USEEIO2012_FinalDemand.csv'))

# Load waste rates for the BEA codes, calculated in lafa_rate_conversion.R
bea_waste_rates <- read_csv(file.path(fp, 'crossreference_tables/waste_rates_bea.csv'))

# Consumer demand baseline, averted in lower bound scenario, and averted in upper bound scenario
demand_change_fn <- function(W0, r, p) p * ((1 - W0) / (1 - (1 - r) * W0) - 1) + 1

# baseline waste rate is the baseline avoidable consumer waste rate times the proportion of that waste that is "confusion"
# waste reduction rate is the consumer response
# proportion food is the % food in that BEA code

# We also need to include a very simple accounting of beverages.
# 311920: coffee and tea, 311930 drink concentrates, 312110 soft drinks, bottled water, and ice.
# This is supplemented with very crude numbers I estimated from lit sources, which appear in the stoten II paper.
beverage_waste_rates <- bea_codes %>% 
  filter(beverages > 0, stage == 'processing') %>% 
  filter(!grepl('beer|wine|spirits', BEA_389_def)) %>%
  select(BEA_389_code, BEA_389_def) %>%
  mutate(primary_loss_value = 4.5, retail_loss_value = 5, avoidable_consumer_loss_value = 8)

bea_waste_rates <- bind_rows(bea_waste_rates, beverage_waste_rates)

datelabelingdemand <- finaldemand2012 %>%
  right_join(bea_waste_rates) %>%
  left_join(bea_codes) %>%
  mutate(baseline_demand = `2012_US_Consumption` * proportion_food,
         baseline_consumer_waste_demand = baseline_demand * avoidable_consumer_loss_value / 100,
         averted_demand_mean = `2012_US_Consumption` * (1 - demand_change_fn(W0 = proportion_confusion_waste['mean'] * avoidable_consumer_loss_value / 100,
                                                                              r = consumer_response['mean'],
                                                                              p = proportion_food)),
         averted_demand_lower = `2012_US_Consumption` * (1 - demand_change_fn(W0 = proportion_confusion_waste['lower'] * avoidable_consumer_loss_value / 100,
                                                                        r = consumer_response['lower'],
                                                                        p = proportion_food)),
         averted_demand_upper = `2012_US_Consumption` * (1 - demand_change_fn(W0 = proportion_confusion_waste['upper'] * avoidable_consumer_loss_value / 100,
                                                                        r = consumer_response['upper'],
                                                                        p = proportion_food))) %>%
  select(BEA_389_code, BEA_389_def, baseline_demand, baseline_consumer_waste_demand, averted_demand_mean, averted_demand_lower, averted_demand_upper)


# Join with long code names
datelabelingdemand <- datelabelingdemand %>%
  left_join(all_codes[,c(1,3)], by = c('BEA_389_code' = 'sector_code_uppercase'))

# Run EEIO for the baseline, averted mean, averted lower, and averted upper values
# For now, just sum everything up across food types (not that important which is which)

if (!is_local) use_python('/usr/bin/python3')
source_python(file.path(fp_github, 'fwe/USEEIO/eeio_lcia.py'))

datelabeling_baseline_eeio <- with(datelabelingdemand, eeio_lcia('USEEIO2012', as.list(baseline_demand), as.list(sector_desc_drc)))
datelabeling_avertedmean_eeio <- with(datelabelingdemand, eeio_lcia('USEEIO2012', as.list(averted_demand_mean), as.list(sector_desc_drc)))
datelabeling_avertedlower_eeio <- with(datelabelingdemand, eeio_lcia('USEEIO2012', as.list(averted_demand_lower), as.list(sector_desc_drc)))
datelabeling_avertedupper_eeio <- with(datelabelingdemand, eeio_lcia('USEEIO2012', as.list(averted_demand_upper), as.list(sector_desc_drc)))

# Convert EEIO output into a single data frame

eeio_datelabeling <- map2_dfr(list(datelabeling_baseline_eeio, datelabeling_avertedmean_eeio, datelabeling_avertedlower_eeio, datelabeling_avertedupper_eeio),
                              c('impact_baseline', 'impact_averted_mean', 'impact_averted_lower', 'impact_averted_upper'),
                              ~ data.frame(category = row.names(.x),
                                           scenario = .y,
                                           impact = .x[,'Total']))


# Combine impact and offset to get net impact reduced
eeio_datelabeling_result <- eeio_datelabeling %>% 
  pivot_wider(names_from = scenario, values_from = impact) %>%
  #left_join(datelabeling_offset_eeio) %>%
  mutate(net_averted_mean_coordination = impact_averted_mean,
         net_averted_lower_coordination = impact_averted_lower,
         net_averted_upper_coordination = impact_averted_upper,
         net_percent_averted_mean_coordination = 100 * net_averted_mean_coordination / impact_baseline,
         net_percent_averted_lower_coordination = 100 * net_averted_lower_coordination / impact_baseline,
         net_percent_averted_upper_coordination = 100 * net_averted_upper_coordination / impact_baseline)
# It averts 0.1% to 0.4% of the food system's environmental impact. That's plausible, if anything high.
  
# Cost per unit reduction for date labeling, using annualization of one-time costs
eeio_datelabeling_result <- eeio_datelabeling_result %>%
  mutate(cost_per_reduction_mean_coordination = datelabel_costs_coord_annual['mean'] / net_averted_mean_coordination,
         cost_per_reduction_lower_coordination = datelabel_costs_coord_annual['lower'] / net_averted_upper_coordination,
         cost_per_reduction_upper_coordination = datelabel_costs_coord_annual['upper'] / net_averted_lower_coordination)

# Display the results.

conversion_factors <- c(1e-9, 1e-6, 1e-9, 1e-10, 1e-9)
category_names <- c('energy (PJ)', 'eutrophication (kT N)', 'greenhouse gas (MT CO2)', 'land (Mha)', 'water (km3)')

datelabeling_impact_data <- eeio_datelabeling_result %>%
  filter(grepl('enrg|eutr|gcc|land|watr', category)) %>%
  select(category, contains('net')) %>%
  mutate(category = category_names) %>%
  mutate_at(vars(contains('net_averted')), ~ .* conversion_factors)

datelabeling_cost_data <- eeio_datelabeling_result %>%
  filter(grepl('enrg|eutr|gcc|land|watr', category)) %>%
  select(category, contains('cost'))

# Save result
write_csv(eeio_datelabeling_result, file.path(fp, 'scenario_results/interventions/eeio_datelabeling_all.csv'))

datelabeling_impact_data_tocsv <- eeio_datelabeling_result %>%
  filter(grepl('enrg|eutr|gcc|land|watr', category)) %>%
  select(category, contains('net')) %>%
  mutate_at(vars(contains('net_averted')), ~ .* conversion_factors)

datelabeling_cost_data_tocsv <- eeio_datelabeling_result %>%
  filter(grepl('enrg|eutr|gcc|land|watr', category)) %>%
  select(category, starts_with('cost'))

datelabeling_cost_data_tocsv %>%
  left_join(datelabeling_impact_data_tocsv) %>%
  write_csv(file.path(fp, 'scenario_results/interventions/eeio_datelabeling_5categories_processed.csv'))
  