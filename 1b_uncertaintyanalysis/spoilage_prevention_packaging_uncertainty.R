# Spoilage prevention packaging uncertainty analysis function
# QDR / foodwasteinterventions / 24 April 2020

# Modified 05 May 2020: implement new cost approach

spoilage_prevention_packaging <- function(wr_retail_fv, wr_household_fv, wr_retail_meat, wr_household_meat, p_fruit, p_veg, p_meat, p_seafood, cost_per_package, annuity_years, annuity_rate, unitcost_fruit, unitcost_meat, unitcost_misc, unitcost_poultry, unitcost_seafood, unitcost_vegetables) {
  
  param_table <- data.frame(food = c('fruit','vegetables','meat','poultry','seafood','misc'),
                            wr_retail = rep(c(wr_retail_fv, wr_retail_meat), c(2,4)),
                            wr_household = rep(c(wr_household_fv, wr_household_meat), c(2,4)),
                            proportion_affected = c(p_fruit, p_veg, rep(p_meat, 4)),
                            unitcost = c(unitcost_fruit, unitcost_vegetables, unitcost_meat, unitcost_poultry, unitcost_seafood, unitcost_misc))
  
  # Multiply the units sold for each food type by the proportion affected by the intervention
  param_table <- packaging_costs_by_food %>%
    left_join(param_table) %>%
    mutate(units_affected = proportion_affected * Units,
           material_cost = units_affected * cost_per_package,
           initial_cost = units_affected * unitcost,
           annualized_initial_cost = pmt(initial_cost, r = annuity_rate, n = annuity_years, f = 0, t = 0),
           annualized_total_cost = annualized_initial_cost + material_cost)
  
  # Annualize initial cost with pmt function, and add to annual material costs. (summed across food types)
  initial_cost <- sum(param_table$initial_cost)
  annualized_initial_cost <- sum(param_table$annualized_initial_cost)
  material_cost <- sum(param_table$material_cost)
  packaging_annual_cost <- material_cost + annualized_initial_cost
  
  ### Get the baseline consumer demand for fruit, vegetables, red meat, poultry, seafood, and misc. meat-containing products in 2012.
  fruitmeatdemand2012 <- packaging_proportion_by_BEA %>%
    rename(BEA_389_code = BEA_Code) %>%
    left_join(finaldemand2012)
  
  # Recreate packaging reduction rate data frame
  packaging_reduction_rates <- param_table %>% select(food, wr_retail, wr_household, proportion_affected) %>%
    pivot_longer(c(wr_retail, wr_household), names_to = 'level', values_to = 'waste_reduction') %>%
    mutate(level = gsub('wr_', '', level))

  # Post intervention demand under different scenarios
  packaging_reduction_rates_wide <- packaging_reduction_rates %>%
    pivot_wider(id_cols = food, names_from = level, values_from = -c(food,level))
  
  # Compute baseline demand (deducting non-fish proportion of the seafood codes)
  # and reduction rates of retail and household demand (lower and upper bounds)
  # from this get the averted demand after the intervention for each food
  fruitmeatdemand2012 <- fruitmeatdemand2012 %>%
    mutate(food = c('vegetables','fruit','misc','poultry','meat','seafood','misc','misc'), group_final = c('Fresh vegetables', 'Fresh fruit', 'Red meat', 'Poultry', 'Red meat', 'Total Fresh and Frozen Fish', 'Red meat', 'Red meat')) %>%
    left_join(fruitmeat_wtdavg_rates) %>%
    left_join(packaging_reduction_rates_wide) %>%
    mutate(baseline_demand = `2012_US_Consumption` * proportion,
           retail_reduction = demand_change_fn(W0 = retail_loss/100, r = waste_reduction_retail, p = proportion_affected_retail),
           household_reduction = demand_change_fn(W0 = avoidable_consumer_loss/100, r = waste_reduction_household, p = proportion_affected_household),
           demand_reduction = retail_reduction * household_reduction,
           demand_averted = baseline_demand * (1 - demand_reduction))
  
  # Multiply EEIO per-dollar results times the baseline and averted demand for each BEA code
  eeio_packaging_averted <- eeio_packaging_averted %>% mutate(BEA_Code = toupper(substr(BEA_Code, 1, 6)))
  eeio_packaging_averted <- fruitmeatdemand2012 %>% 
    full_join(eeio_packaging_averted, by = c('BEA_389_code' = 'BEA_Code')) %>%
    mutate(baseline = impact * baseline_demand,
           averted = impact * demand_averted)
  
  #### cost estimates
  # use the annual costs to get the annual impacts, using plastic packaging materials as the industry.
  packaging_annual_offset <- eeio_packaging_offsetting_impacts %>%
    mutate(offset = impact * material_cost)
  
    # Put results from packaging into a "standardized" form that is similar to the other ones.
  # totals things up, ignoring individual food types.
  eeio_packaging_averted_total <- eeio_packaging_averted %>%
    group_by(category) %>%
    summarize_if(is.numeric, sum)
  
  eeio_packaging_result <- eeio_packaging_averted_total %>%
    left_join(packaging_annual_offset %>% ungroup %>% select(category, contains('offset'))) %>%
    mutate(net_averted = averted - offset,
           cost_per_reduction = packaging_annual_cost / net_averted)
  
  # also include results for cost for each of the food types separately
  packaging_annual_offset_bytype <- outer(eeio_packaging_offsetting_impacts$impact, param_table$material_cost) %>%
    as.data.frame %>%
    setNames(param_table$food) %>%
    mutate(category = eeio_packaging_offsetting_impacts$category) %>%
    pivot_longer(-category, names_to = 'food', values_to = 'offset')
  
  eeio_packaging_result_bytype <- eeio_packaging_averted %>%
    left_join(packaging_annual_offset_bytype) %>%
    left_join(param_table %>% select(food, contains("cost"))) %>%
    mutate(net_averted = averted - offset,
           cost_per_reduction = annualized_total_cost / net_averted)
  
  cost_result <- data.frame(initial_cost = initial_cost,
                            material_cost = material_cost,
                            annualized_initial_cost = annualized_initial_cost,
                            annual_cost = packaging_annual_cost)
  
  cost_result_bytype <- param_table %>% select(food, material_cost:annualized_total_cost)
  
  return(list(impact = eeio_packaging_result, cost = cost_result, impact_byfoodtype = eeio_packaging_result_bytype, cost_byfoodtype = cost_result_bytype))

}