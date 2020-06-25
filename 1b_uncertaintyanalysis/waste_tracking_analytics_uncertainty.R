# Waste tracking and analytics analysis 
# Wrapped into a function that takes parameters as arguments (to do as uncertainty)
# QDR / foodwasteinterventions / 27 April 2020

# Edit 25 May 2020: Equipment cost is now a percentage of total equipment+fees cost
# Edit 20 May 2020: Equipment cost is a lease rate rather than an annualized cost
# Edit 06 May 2020: Split annual cost into wage and fee components
# Edit 29 Apr 2020: Also add some code to calculate average across the 3 industries, so a single value can be used for the main results.

# In this new version, instead of using proportions of receipts, use the # of foodservice contractors as the establishments
# But use the total amounts of food purchased by "eligible" industries, as identified by Steve, as the potential food that can be affected

waste_tracking_analytics <- function(wta_waste_reduction, proportion_kitchen_waste, p_equipment, annual_cost_wages, annual_cost_fees_equipment, p_scales, annuity_years, annuity_rate) {
  
  overall_waste_reduction <- proportion_kitchen_waste * wta_waste_reduction
  annual_cost_equipment_lease <- annual_cost_fees_equipment * p_equipment
  annual_cost_fees <- annual_cost_fees_equipment - annual_cost_equipment_lease
  
  # Join baseline waste rates with waste reduction rates
  
  food_U_LAFA_spread <- food_U_LAFA_spread %>% 
    left_join(susb_bea_proportion_affected, by = c('BEA_recipient_code' = 'BEA_Code')) %>%
    mutate(reduction_by_mass = demand_change_fn(W0 = avoidable_consumer_loss/100, r = overall_waste_reduction, p = proportion),
           mass_flow_post_intervention = mass_flow * reduction_by_mass,
           monetary_flow_post_intervention = mass_flow_post_intervention * price)
  
  # Sum up by old row and column index from the original food_U matrix, then reshape to make the same matrix.
  food_U_postintervention_df <- food_U_LAFA_spread %>%
    group_by(BEA_389_code, BEA_recipient_code) %>%
    summarize(monetary_flow = sum(monetary_flow_post_intervention, na.rm = TRUE)) 
  
  food_U_postintervention <- food_U_postintervention_df %>%
    pivot_wider(names_from = BEA_recipient_code, values_from = monetary_flow, values_fill = list(monetary_flow = 0)) %>%
    as.data.frame
  row.names(food_U_postintervention) <- food_U_postintervention$BEA_389_code
  food_U_postintervention <- food_U_postintervention[, !names(food_U_postintervention) %in% 'BEA_389_code']
  
  # Make the row and column order the same as food_U
  food_U_postintervention <- food_U_postintervention[, names(food_U)]
  
  # Summarize $ and mass flow changes ---------------------------------------
  
  # The % of waste reduced by mass won't be exactly equivalent to the % of waste reduced by $.
  
  postintervention <- food_U_postintervention_df %>%
    group_by(BEA_recipient_code) %>%
    summarize(monetary_flow_postintervention = sum(monetary_flow))
  
  preintervention <- data.frame(BEA_recipient_code = names(food_U), monetary_flow_preintervention = colSums(food_U))
  
  monetary_byintervention <- left_join(postintervention, preintervention) %>%
    mutate(reduction_bydollar = 1 - monetary_flow_postintervention / monetary_flow_preintervention) 
  
  mass_byintervention <- food_U_LAFA_spread %>%
    group_by(BEA_recipient_code) %>%
    summarize(mass_flow_preintervention = sum(mass_flow, na.rm = TRUE),
              mass_flow_postintervention = sum(mass_flow_post_intervention, na.rm = TRUE)) %>%
    mutate(reduction_bymass = 1 - mass_flow_postintervention / mass_flow_preintervention) 
  
  rate_changes <- left_join(mass_byintervention, monetary_byintervention)
  
  # A better way is to phrase it as reducing the operating costs (purchases of raw materials) by the affected industries
  # Since we have a waste rate reduction by mass, convert it back to waste rate reduction by money and get the purchasing rate reduction from each of the industries that supply the final foodservice industries.
  
  # Calculate pre and post incoming monetary food flow in millions of dollars
  total_prepost <- data.frame(BEA_Code = names(food_U),
                              food_purchases_baseline = colSums(food_U),
                              food_purchases_postintervention = colSums(food_U_postintervention)) %>%
    mutate(reduction = food_purchases_baseline - food_purchases_postintervention)
  
  
  # Calculate baseline waste by industry ------------------------------------
  
  # Weighted mean of waste rate by mass flow for each foodservice industry = final waste rate for the industries!
  baseline_waste_foodservice <- food_U_LAFA_spread %>%
    group_by(BEA_recipient_code) %>%
    summarize(avoidable_consumer_loss = weighted.mean(x = avoidable_consumer_loss, w = mass_flow, na.rm = TRUE))
  
  # Join up the names of the BEA codes and print the table of values
  baseline_waste_foodservice <- baseline_waste_foodservice %>% 
    left_join(codes_subset, by = c('BEA_recipient_code' = 'BEA_389_code')) %>%
    setNames(c('BEA_Code', 'baseline', 'BEA_Title')) 
  
  baseline_waste_foodservice <- baseline_waste_foodservice %>%
    left_join(susb_bea_proportion_affected %>% ungroup %>% select(BEA_Code, proportion)) %>%
    left_join(total_prepost)
  
  # Above they were marginal column totals for the recipient industries
  # Phrase it also as marginal row totals for the food types
  
  reduction_byfoodtype <- data.frame(BEA_Code = row.names(food_U),
                                     cost_averted = rowSums(food_U) - rowSums(food_U_postintervention))
  
  # Run for full service, limited service, and contractors separately. Sum up all the purchases for tourism & institutions to get contractors' food purchases.
  fullsvc_codes <- c("722110")
  limitedsvc_codes <- c("722A00", "722211")
  
  food_U_bygroup <- data.frame(full_service_restaurants = food_U[, fullsvc_codes],
                               limited_mobile_and_bars = rowSums(food_U[, limitedsvc_codes]),
                               contracted_foodservice = rowSums(food_U[, !names(food_U) %in% c(fullsvc_codes, limitedsvc_codes)]))
  
  food_U_bygroup_postintervention <- data.frame(full_service_restaurants = food_U_postintervention[, fullsvc_codes],
                                                limited_mobile_and_bars = rowSums(food_U_postintervention[, limitedsvc_codes]),
                                                contracted_foodservice = rowSums(food_U_postintervention[, !names(food_U_postintervention) %in% c(fullsvc_codes, limitedsvc_codes)]))
  
  # Run EEIO ----------------------------------------------------------------
  
  # (Edited 12 May: extract precalculated EEIO results instead)
  # Simply enough, just run it on the difference in the two rowSums for pre and post intervention
  # This represents final operating costs of the industries, as if it were final consumer demand
  
  # Match row names of food_U with longer codes that eeio_lcia() will recognize
  demand_codes <- all_codes$sector_desc_drc[match(rownames(food_U), all_codes$sector_code_uppercase)]
  
  # The model is already built so we don't need to build it again. 
  # All we need to do is match the demand vector 6 digit codes with the codes that include the full names
  # then run eeio_lcia on it.
  # Do for each of the 3 groups separately
  
  eeio_wta <- eeio_df %>%
    filter(sector_desc_drc %in% demand_codes) %>%
    left_join(all_codes)
  
  group_vars <- vars(full_service_restaurants, limited_mobile_and_bars, contracted_foodservice)
  
  eeio_wta_baseline_bygroup <- food_U_bygroup %>%
    mutate(sector_desc_drc = demand_codes) %>%
    full_join(eeio_wta) %>%
    mutate_at(group_vars, ~ . * impact * 1e6) %>%
    group_by(category) %>%
    summarize_at(group_vars, sum) %>%
    mutate(scenario = 'baseline') %>%
    select(category, scenario, full_service_restaurants, limited_mobile_and_bars, contracted_foodservice)
  
  eeio_wta_averted_bygroup <- (food_U_bygroup - food_U_bygroup_postintervention) %>%
    mutate(sector_desc_drc = demand_codes) %>%
    full_join(eeio_wta) %>%
    mutate_at(group_vars, ~ . * impact * 1e6) %>%
    group_by(category) %>%
    summarize_at(group_vars, sum) %>%
    mutate(scenario = 'impact_averted') %>%
    select(category, scenario, full_service_restaurants, limited_mobile_and_bars, contracted_foodservice)
  
  eeio_dat_bygroup <- bind_rows(eeio_wta_baseline_bygroup, eeio_wta_averted_bygroup) %>%
    pivot_longer(-c(category, scenario), names_to = 'group') %>%
    pivot_wider(names_from = scenario, values_from = value) %>%
    select(group, category, baseline, impact_averted) %>%
    mutate(group = c('contracted foodservice operations',
                     'full-service restaurants',
                     'limited-service restaurants, mobile foodservice, and bars')[as.numeric(factor(group))])

  # Deduct offsetting impacts from benefit ----------------------------------
  
  equipment_cost_bygroup <- establishments_implementing_total %>%
    ungroup %>%
    filter(can_implement) %>%
    mutate(group = c('contracted foodservice operations', 'full-service restaurants', rep('limited-service restaurants, mobile foodservice, and bars', 2))) %>%
    group_by(group) %>%
    summarize(establishments = sum(n_estab)) %>%
    mutate(equipment_cost_annual = establishments * annual_cost_equipment_lease,
           computers = equipment_cost_annual * (1 - p_scales)/2,
           peripherals = equipment_cost_annual * (1 - p_scales)/2,
           scales = equipment_cost_annual * p_scales)
  
  eeio_offsets_bytype <- as.matrix(eeio_offsets_wta[,2:4]) %*% t(as.matrix(equipment_cost_bygroup[,4:6])) 
  eeio_offsets_bygroup_combined <-  data.frame(category = eeio_offsets_wta$category, eeio_offsets_bytype) %>% 
    setNames(c('category','contracted foodservice operations', 'full-service restaurants', 'limited-service restaurants, mobile foodservice, and bars')) %>%
    pivot_longer(-category, names_to = 'group', values_to = 'offset')

  # Join EEIO results for the impact reduction with EEIO results for the offset
  
  eeio_dat_bygroup_withoffset <- eeio_dat_bygroup %>%
    left_join(equipment_cost_bygroup) %>%
    left_join(eeio_offsets_bygroup_combined) %>%
    mutate(percent_averted = signif(100 * impact_averted/baseline, 2),
           net_averted = impact_averted - offset,
           net_percent_averted = 100 * net_averted/baseline,
           total_cost = equipment_cost_annual + (annual_cost_wages + annual_cost_fees) * establishments,
           cost_per_reduction = total_cost / net_averted)
  
  cost_result <- equipment_cost_bygroup %>%
    mutate(annual_cost_wages = annual_cost_wages * establishments,
           annual_cost_fees = annual_cost_fees * establishments,
           annual_cost_equipment_lease = annual_cost_equipment_lease * establishments,
           averted_food_purchase = colSums(food_U_bygroup - food_U_bygroup_postintervention) * 1e6,
           net_cost = annual_cost_wages + annual_cost_fees + annual_cost_equipment_lease - averted_food_purchase) %>%
    select(-equipment_cost_annual)
  
  # Calculate total impact, total cost, and average cost-effectiveness across the 3 industries
  
  eeio_result_sums <- eeio_dat_bygroup_withoffset %>%
    group_by(category) %>%
    summarize_at(vars(baseline, impact_averted, establishments, equipment_cost_annual, computers, peripherals, scales, offset, net_averted, total_cost), sum)
  eeio_result_averages <- eeio_dat_bygroup_withoffset %>%
    group_by(category) %>%
    summarize_at(vars(percent_averted, net_percent_averted, cost_per_reduction), ~ weighted.mean(x = ., w = net_averted))
  
  eeio_result_total <- cbind(group = 'total', eeio_result_sums, eeio_result_averages %>% select(-category))
  
  cost_result_total <- cost_result %>% summarize_if(is.numeric, sum) %>% mutate(group = 'total')

  return(list(impact = bind_rows(eeio_dat_bygroup_withoffset, eeio_result_total), cost = bind_rows(cost_result, cost_result_total)))
  
}