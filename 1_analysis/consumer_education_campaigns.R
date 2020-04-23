# Consumer education campaign analysis
# Split into a separate script by QDR, 23 April 2020

# Load packages and check whether this is being run locally or on rstudio server.
library(tidyverse)
library(reticulate)

is_local <- dir.exists('Q:/')
fp <- ifelse(is_local, 'Q:', '/nfs/qread-data')
fp_github <- file.path(ifelse(is_local, '~/Documents/GitHub/foodwaste', '~'))

# Assumptions: 

# Reduction in rate of food waste
consumer_ed_waste_reduction <- (1/3) * (2/3) * c(.05,.1,.15) # 1.1 to 3.3 percent reduction, we can use this fairly small number

# Proportion of baseline demand affected is equal to the proportion of population who lives in the metropolitan areas that are big enough to get a targeted campaign

# Cost is the annual cost per campaign multiplied by the number of metropolitan areas that will have campaigns

# The offsetting impact should be fairly negligible, using the environmental impact from media industries which will be very low per $1 output.

# There are 1251 counties within metropolitan statistical areas, together representing 85.6% of the US pop
pop_in_metro <- 0.856 # see read_msas.R for derivation of this number.
n_metro_counties <- 1251

# Costs: content development 1x per year regardless, media consultant 6-12x per year (low/high), media costs 6-12x per year (low/high)
consumer_ed_costs <- c(content_development = 68.6e3,
                       media_consultant = 15.68e3,
                       media_costs = 29.4e3)

consumer_ed_costs_annual <- n_metro_counties * c(lower = sum(consumer_ed_costs * c(1,6,6)),
                                                 upper = sum(consumer_ed_costs * c(1,12,12)))
# 424 to 763 million

# Calculate baseline demand & baseline waste at consumer level, multiplied by the 85.6% number.

# Using all final demand from levels 1 and 3, multiplied by the appropriately adjusted pre-intervention and post-intervention "confusion waste rate".
finaldemand2012 <- read_csv(file.path(fp_github, 'USEEIO/useeiopy/Model Builds/USEEIO2012/USEEIO2012_FinalDemand.csv'))

# Load waste rates for the BEA codes, calculated in lafa_rate_conversion.R
bea_waste_rates <- read_csv(file.path(fp, 'crossreference_tables/waste_rates_bea.csv'))

# Consumer demand baseline, averted in lower bound scenario, and averted in upper bound scenario
demand_change_fn <- function(W0, r, p) p * ((1 - W0) / (1 - (1 - r) * W0) - 1) + 1

# add beverage rates (copied from above)
beverage_waste_rates <- bea_codes %>% 
  filter(beverages > 0, stage == 'processing') %>% 
  filter(!grepl('beer|wine|spirits', BEA_389_def)) %>%
  select(BEA_389_code, BEA_389_def) %>%
  mutate(primary_loss_value = 4.5, retail_loss_value = 5, avoidable_consumer_loss_value = 8)

bea_waste_rates <- bind_rows(bea_waste_rates, beverage_waste_rates)

############ edit the following to have lower, mean, and upper, and make sure numbers are correct
consumer_ed_demand <- finaldemand2012 %>%
  right_join(bea_waste_rates) %>%
  left_join(bea_codes) %>%
  mutate(baseline_demand = `2012_US_Consumption` * proportion_food,
         baseline_demand_metro = baseline_demand * pop_in_metro,
         baseline_consumer_waste_demand = baseline_demand_metro * avoidable_consumer_loss_value / 100,
         averted_demand_lower = `2012_US_Consumption` * (1 - demand_change_fn(W0 = avoidable_consumer_loss_value / 100,
                                                                              r = consumer_ed_waste_reduction[1],
                                                                              p = proportion_food * pop_in_metro)),
         averted_demand_mean = `2012_US_Consumption` * (1 - demand_change_fn(W0 = avoidable_consumer_loss_value / 100,
                                                                              r = consumer_ed_waste_reduction[2],
                                                                              p = proportion_food * pop_in_metro)),
         averted_demand_upper = `2012_US_Consumption` * (1 - demand_change_fn(W0 = avoidable_consumer_loss_value / 100,
                                                                              r = consumer_ed_waste_reduction[3],
                                                                              p = proportion_food * pop_in_metro))) %>%
  select(BEA_389_code, BEA_389_def, baseline_demand, baseline_demand_metro, baseline_consumer_waste_demand, averted_demand_lower, averted_demand_mean, averted_demand_upper)


# Join with long code names
consumer_ed_demand <- consumer_ed_demand %>%
  left_join(all_codes[,c(1,3)], by = c('BEA_389_code' = 'sector_code_uppercase'))

# Run EEIO for the baseline, averted lower, averted mean, and averted upper values
# For now, just sum everything up across food types (not that important which is which)

if (!is_local) use_python('/usr/bin/python3')
source_python(file.path(fp_github, 'fwe/USEEIO/eeio_lcia.py'))

consumer_ed_baseline_eeio <- with(consumer_ed_demand, eeio_lcia('USEEIO2012', as.list(baseline_demand), as.list(sector_desc_drc)))
consumer_ed_avertedlower_eeio <- with(consumer_ed_demand, eeio_lcia('USEEIO2012', as.list(averted_demand_lower), as.list(sector_desc_drc)))
consumer_ed_avertedmean_eeio <- with(consumer_ed_demand, eeio_lcia('USEEIO2012', as.list(averted_demand_mean), as.list(sector_desc_drc)))
consumer_ed_avertedupper_eeio <- with(consumer_ed_demand, eeio_lcia('USEEIO2012', as.list(averted_demand_upper), as.list(sector_desc_drc)))

# Convert EEIO output into a single data frame

eeio_consumer_ed <- map2_dfr(list(consumer_ed_baseline_eeio, consumer_ed_avertedlower_eeio, consumer_ed_avertedmean_eeio, consumer_ed_avertedupper_eeio),
                              c('impact_baseline', 'impact_averted_lower', 'impact_averted_mean', 'impact_averted_upper'),
                              ~ data.frame(category = row.names(.x),
                                           scenario = .y,
                                           impact = .x[,'Total']))

# Offsetting impacts
# Offsetting impacts, using the low and high rates of yearly media costs
# For content development, use specialized design services 541400
# For media consulting, use advertising/PR services 541800
# For media, use a combination of advertising, newspaper, magazine, radio, and internet publishing (use highest and lowest bound from these)

media_codes <- c('541400', '541800', '511110', '511120', '515100', '519130')

media_codes_long <- all_codes$sector_desc_drc[match(media_codes, all_codes$sector_code_uppercase)]

consumer_ed_offset_eeio <- map(media_codes_long, ~ eeio_lcia('USEEIO2012', list(1), list(.))) # offset per dollar on each media industry

consumer_ed_offset_eeio <- map2_dfr(consumer_ed_offset_eeio, media_codes_long, ~ data.frame(BEA_code = .y,
                                                                                            category = row.names(.x),
                                                                                            impact = .x[,'Total']))

consumer_ed_offset_eeio %>% filter(grepl('gcc',category)) # They vary by a factor of 2 at most.

# Calculate upper and lower bounds for offset based on assigning media impacts to upper and lower most impactful industries

# Calculate upper and lower bounds based on costs
consumer_ed_costs_lower <- n_metro_counties * consumer_ed_costs * c(1,6,6)
consumer_ed_costs_upper <- n_metro_counties * consumer_ed_costs * c(1,12,12)

# Find lower and upper limits for the media component
media_impacts <- consumer_ed_offset_eeio %>%
  filter(!BEA_code %in% media_codes_long[1]) %>%
  group_by(category) %>%
  summarize(media_costs_impact_lower = min(impact), media_costs_impact_upper = max(impact), media_costs_impact_median = median(impact))

consumer_ed_impacts_bytype <- consumer_ed_offset_eeio %>%
  filter(BEA_code %in% media_codes_long[1:2]) %>%
  pivot_wider(names_from = BEA_code, values_from = impact) %>%
  setNames(c('category', 'content_development', 'media_consultant')) %>%
  left_join(media_impacts)

consumer_ed_costs_mean <- (consumer_ed_costs_lower + consumer_ed_costs_upper) / 2

consumer_ed_offset_df <- consumer_ed_impacts_bytype %>%
  mutate(offset_lower = rowSums(sweep(.[,c('content_development', 'media_consultant', 'media_costs_impact_lower')], 2, consumer_ed_costs_lower, `*`)),
         offset_median = rowSums(sweep(.[,c('content_development', 'media_consultant', 'media_costs_impact_median')], 2, consumer_ed_costs_mean, `*`)),
         offset_upper = rowSums(sweep(.[,c('content_development', 'media_consultant', 'media_costs_impact_upper')], 2, consumer_ed_costs_upper, `*`)))

# Combine impact and offset to get net impact reduced
eeio_consumer_ed_result <- eeio_consumer_ed %>% 
  pivot_wider(names_from = scenario, values_from = impact) %>%
  left_join(consumer_ed_offset_df) %>%
  mutate(net_averted_lower = impact_averted_lower - offset_upper,
         net_averted_mean = impact_averted_mean - offset_median,
         net_averted_upper = impact_averted_upper - offset_lower,

         net_percent_averted_lower = 100 * net_averted_lower / impact_baseline,
         net_percent_averted_median = 100 * net_averted_mean / impact_baseline,
         net_percent_averted_upper = 100 * net_averted_upper / impact_baseline)
# It averts 0.2 to 0.7% of the food system's environmental impact.


# Cost per unit reduction for date labeling, using annualization of one-time costs
# Turns out to be a fairly cheap method.
eeio_consumer_ed_result <- eeio_consumer_ed_result %>%
  mutate(cost_per_reduction_lower = sum(consumer_ed_costs_lower) / net_averted_upper,
         cost_per_reduction_mean = sum(consumer_ed_costs_mean) / net_averted_mean,
         cost_per_reduction_upper = sum(consumer_ed_costs_upper) / net_averted_lower)

# Display the results.

conversion_factors <- c(1e-9, 1e-6, 1e-9, 1e-10, 1e-9)
category_names <- c('energy (PJ)', 'eutrophication (kT N)', 'greenhouse gas (MT CO2)', 'land (Mha)', 'water (km3)')

consumer_ed_impact_data <- eeio_consumer_ed_result %>%
  filter(grepl('enrg|eutr|gcc|land|watr', category)) %>%
  select(category, contains('net')) %>%
  mutate(category = category_names) %>%
  mutate_at(vars(contains('net_averted')), ~ .* conversion_factors)

consumer_ed_cost_data <- eeio_consumer_ed_result %>%
  filter(grepl('enrg|eutr|gcc|land|watr', category)) %>%
  select(category, starts_with('cost'))

consumer_ed_cost_data %>%
  mutate(category = c('energy ($/MJ)', 'eutrophication ($/kg N)', 'greenhouse gas ($/kg CO2)',  'land ($/m2)', 'water ($/m3)')) %>%
  filter(!grepl('eutr', category)) %>%
  ggplot(aes(x = category, color = category, y = cost_per_reduction_mean, ymin = cost_per_reduction_lower, ymax = cost_per_reduction_upper)) +
  geom_errorbar(size = 1, width = 0.1) +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::dollar) +
  theme_bw() + 
  theme(panel.grid = element_blank(), legend.position = 'none')

ggsave(file.path(fp, 'figures/intervention_analysis/consumer_ed_cost_per_reduction.pdf'), height = 5, width = 5)

# Save result
write_csv(eeio_consumer_ed_result, file.path(fp, 'scenario_results/interventions/eeio_consumer_ed_all.csv'))

consumer_ed_impact_data_tocsv <- eeio_consumer_ed_result %>%
  filter(grepl('enrg|eutr|gcc|land|watr', category)) %>%
  select(category, contains('net')) %>%
  mutate_at(vars(contains('net_averted')), ~ .* conversion_factors)

consumer_ed_cost_data_tocsv <- eeio_consumer_ed_result %>%
  filter(grepl('enrg|eutr|gcc|land|watr', category)) %>%
  select(category, starts_with('cost'))

consumer_ed_cost_data_tocsv %>%
  left_join(consumer_ed_impact_data_tocsv) %>%
  write_csv(file.path(fp, 'scenario_results/interventions/eeio_consumer_ed_5categories_processed.csv'))
