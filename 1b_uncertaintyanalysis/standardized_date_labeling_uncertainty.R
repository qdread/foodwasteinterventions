# Standardized date labeling analysis
# Wrapped into a function that takes parameters as arguments (to do as uncertainty)
# QDR / foodwasteinterventions / 24 April 2020


standardized_date_labeling <- function(consumer_response_rate, proportion_confusion_waste, initial_cost, annuity_years, annuity_rate, baseline_beverage_rate) {
  
  
  # Annualize initial costs
  datelabel_costs_coord_annual <- pmt(initial_cost, r = annuity_rate, n = annuity_years, f = 0, t = 0)
  
  
  # Consumer demand baseline, averted in lower bound scenario, and averted in upper bound scenario
  
  # baseline waste rate is the baseline avoidable consumer waste rate times the proportion of that waste that is "confusion"
  # waste reduction rate is the consumer response
  # proportion food is the % food in that BEA code
  
  # We also need to include a very simple accounting of beverages.
  # 311920: coffee and tea, 311930 drink concentrates, 312110 soft drinks, bottled water, and ice.
  beverage_waste_rates <- bea_codes %>% 
    filter(beverages > 0, stage == 'processing') %>% 
    filter(!grepl('beer|wine|spirits', BEA_389_def)) %>%
    select(BEA_389_code, BEA_389_def) %>%
    mutate(primary_loss_value = 4.5, retail_loss_value = 5, avoidable_consumer_loss_value = baseline_beverage_rate)
  
  bea_waste_rates_final <- bind_rows(bea_waste_rates, beverage_waste_rates)
  
  datelabelingdemand <- finaldemand2012 %>%
    right_join(bea_waste_rates_final) %>%
    left_join(bea_codes) %>%
    mutate(baseline_demand = `2012_US_Consumption` * proportion_food,
           baseline_consumer_waste_demand = baseline_demand * avoidable_consumer_loss_value / 100,
           averted_demand = `2012_US_Consumption` * (1 - demand_change_fn(W0 = proportion_confusion_waste * avoidable_consumer_loss_value / 100,
                                                                          r = consumer_response_rate,
                                                                          p = proportion_food))) %>%
    select(BEA_389_code, BEA_389_def, baseline_demand, baseline_consumer_waste_demand, averted_demand)
  
  # Join with long code names
  datelabelingdemand <- datelabelingdemand %>%
    left_join(all_codes[,c(1,3)], by = c('BEA_389_code' = 'sector_code_uppercase'))
  
  # Extract precalculated EEIO for the baseline, averted mean, averted lower, and averted upper values
  # For now, just sum everything up across food types (not that important which is which)
  
  eeio_datelabeling <- eeio_df %>%
    filter(sector_desc_drc %in% datelabelingdemand$sector_desc_drc) %>%
    full_join(datelabelingdemand) %>%
    mutate(impact_baseline = impact * baseline_demand,
           impact_averted = impact * averted_demand) %>%
    group_by(category) %>%
    summarize(impact_baseline = sum(impact_baseline),
              impact_averted = sum(impact_averted))
  
  # Combine results into data frame.
  eeio_datelabeling_result <- eeio_datelabeling %>% 
    mutate(net_averted_coordination = impact_averted,
           net_percent_averted_coordination = 100 * net_averted_coordination / impact_baseline,
           cost_per_reduction_coordination = datelabel_costs_coord_annual / net_averted_coordination)
  
  cost_result <- data.frame(initial_cost = initial_cost,
                            annualized_cost = datelabel_costs_coord_annual,
                            baseline_food_purchase = sum(datelabelingdemand$baseline_demand),
                            averted_food_purchase = sum(datelabelingdemand$averted_demand)) %>%
    mutate(net_cost = annualized_cost - averted_food_purchase,
           savings_multiplier = averted_food_purchase/annualized_cost,
           percent_food_purchase_reduction = averted_food_purchase/baseline_food_purchase)

  return(list(impact = eeio_datelabeling_result, cost = cost_result))
}