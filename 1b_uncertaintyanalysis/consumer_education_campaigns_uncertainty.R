# Consumer education campaign analysis
# Wrapped into a function that takes parameters as arguments (to do as uncertainty)
# QDR / foodwasteinterventions / 27 April 2020

consumer_education_campaigns <- function(consumer_ed_waste_reduction, cost_content, cost_mediaconsultant, cost_media, n_campaigns, p_print, p_internet, baseline_beverage_rate) {

  # Costs: content development 1x per year regardless, media consultant and media costs multiplied by number of campaigns and number of counties in a year.
  consumer_ed_costs <- c(cost_content, cost_mediaconsultant, cost_media)
  n_campaigns_total <- n_campaigns * n_metro_counties
  
  consumer_ed_costs_annual <- consumer_ed_costs * c(1, n_campaigns_total, n_campaigns_total)
  
  # Calculate baseline demand & baseline waste at consumer level, multiplied by the 85.6% number.
  
  # add beverage rates (copied from above)
  beverage_waste_rates <- bea_codes %>% 
    filter(beverages > 0, stage == 'processing') %>% 
    filter(!grepl('beer|wine|spirits', BEA_389_def)) %>%
    select(BEA_389_code, BEA_389_def) %>%
    mutate(primary_loss_value = 4.5, retail_loss_value = 5, avoidable_consumer_loss_value = baseline_beverage_rate)
  
  bea_waste_rates <- bind_rows(bea_waste_rates, beverage_waste_rates)
  
  consumer_ed_demand <- finaldemand2012 %>%
    right_join(bea_waste_rates) %>%
    left_join(bea_codes) %>%
    mutate(baseline_demand = `2012_US_Consumption` * proportion_food,
           baseline_demand_metro = baseline_demand * pop_in_metro,
           baseline_consumer_waste_demand = baseline_demand_metro * avoidable_consumer_loss_value / 100,
           averted_demand = `2012_US_Consumption` * (1 - demand_change_fn(W0 = avoidable_consumer_loss_value / 100,
                                                                                r = consumer_ed_waste_reduction,
                                                                                p = proportion_food * pop_in_metro))) %>%
    select(BEA_389_code, BEA_389_def, baseline_demand, baseline_demand_metro, baseline_consumer_waste_demand, averted_demand)
  
  
  # Join with long code names
  consumer_ed_demand <- consumer_ed_demand %>%
    left_join(all_codes[,c(1,3)], by = c('BEA_389_code' = 'sector_code_uppercase'))
  
  # Run EEIO for the baseline and averted demand values
  consumer_ed_baseline_eeio <- with(consumer_ed_demand, eeio_lcia('USEEIO2012', as.list(baseline_demand), as.list(sector_desc_drc)))
  consumer_ed_averted_eeio <- with(consumer_ed_demand, eeio_lcia('USEEIO2012', as.list(averted_demand), as.list(sector_desc_drc)))

  # Convert EEIO output into a single data frame
  eeio_consumer_ed <- map2_dfr(list(consumer_ed_baseline_eeio, consumer_ed_averted_eeio),
                               c('impact_baseline', 'impact_averted'),
                               ~ data.frame(category = row.names(.x),
                                            scenario = .y,
                                            impact = .x[,'Total']))
  
   # Use proportions of each type of advertising to get the media impacts
  consumer_ed_impacts_bytype <- consumer_ed_offset_eeio %>%
    pivot_wider(names_from = BEA_code, values_from = impact) %>%
    setNames(c('category','content_development', 'media_consultant', 'print', 'radio_tv', 'internet')) %>%
    mutate(print = print * p_print,
           radio_tv = radio_tv * (1 - p_print - p_internet),
           internet = internet * p_internet) %>%
    mutate(media = print + radio_tv + internet) %>%
    select(-print, -radio_tv, -internet)
  
  consumer_ed_offset_df <- consumer_ed_impacts_bytype %>%
    mutate(offset = rowSums(sweep(.[,-1], 2, consumer_ed_costs_annual, `*`)))
  
  # Combine impact and offset to get net impact reduced
  eeio_consumer_ed_result <- eeio_consumer_ed %>% 
    pivot_wider(names_from = scenario, values_from = impact) %>%
    left_join(consumer_ed_offset_df) %>%
    mutate(net_averted = impact_averted - offset,
           net_percent_averted = 100 * net_averted / impact_baseline,
           cost_per_reduction = sum(consumer_ed_costs_annual) / net_averted)

  cost_result <- data.frame(content_development_cost = consumer_ed_costs_annual[1],
                            media_consultant_cost = consumer_ed_costs_annual[2],
                            media_cost = consumer_ed_costs_annual[3])
  
  return(list(impact = eeio_consumer_ed_result, cost = cost_result))  

}