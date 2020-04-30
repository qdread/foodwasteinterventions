# Spoilage prevention packaging uncertainty analysis function
# QDR / foodwasteinterventions / 24 April 2020

spoilage_prevention_packaging <- function(wr_retail_fv, wr_household_fv, wr_retail_meat, wr_household_meat, p_fruit, p_veg, p_meat, initial_cost, material_cost, annuity_years, annuity_rate) {
  
  # Annualize initial cost with pmt function, and add to annual material costs.
  annualized_initial_cost <- pmt(initial_cost, r = annuity_rate, n = annuity_years, f = 0, t = 0)
  packaging_annual_cost <- material_cost + annualized_initial_cost
  
  proportion_packaging_byfoodtype <- proportion_packaging_byfoodtype %>%
    mutate(annualized_initial_cost = p * annualized_initial_cost,
           material_cost = p * material_cost) %>%
    mutate(annualized_total_cost = annualized_initial_cost + material_cost)

  ### Get the baseline consumer demand for fresh fruit, red meat, and poultry in 2012.
  # Do not include processed and frozen products, only fresh.
  fruit_meat_codes <- c('111200', '111300', '311615', '31161A')
  fruit_meat_proportions <- rep(1, 4)
  
  fruitmeatdemand2012 <- data.frame(BEA_389_code = fruit_meat_codes,
                                    proportion = fruit_meat_proportions) %>%
    left_join(finaldemand2012)
  
  # Recreate packaging reduction rate data frame
  packaging_reduction_rates <- data.frame(food = c('fruit','fruit','veg','veg','meat','meat'),
                                          level = c('retail','household'),
                                          proportion_affected = c(p_fruit, p_fruit, p_veg, p_veg, p_meat, p_meat),
                                          waste_reduction = c(wr_retail_fv, wr_household_fv, wr_retail_fv, wr_household_fv, wr_retail_meat, wr_household_meat))
  
  # Post intervention demand under different scenarios
  packaging_reduction_rates_wide <- packaging_reduction_rates %>%
    pivot_wider(id_cols = food, names_from = level, values_from = -c(food,level))
  
  # Compute baseline demand (deducting non-fish proportion of the seafood codes)
  # and reduction rates of retail and household demand (lower and upper bounds)
  # from this get the averted demand after the intervention for each food
  fruitmeatdemand2012 <- fruitmeatdemand2012 %>%
    mutate(food = c('veg','fruit','meat','meat'), group_final = c('Fresh vegetables', 'Fresh fruit', 'Poultry', 'Red meat')) %>%
    left_join(fruitmeat_wtdavg_rates) %>%
    left_join(packaging_reduction_rates_wide) %>%
    mutate(baseline_demand = `2012_US_Consumption` * proportion,
           retail_reduction = demand_change_fn(W0 = retail_loss/100, r = waste_reduction_retail, p = proportion_affected_retail),
           household_reduction = demand_change_fn(W0 = avoidable_consumer_loss/100, r = waste_reduction_household, p = proportion_affected_household),
           demand_reduction = retail_reduction * household_reduction,
           demand_averted = baseline_demand * (1 - demand_reduction))
  
  # Get full codes that will be recognized by USEEIO.
  fruitmeatdemand2012 <- all_codes %>%
    select(sector_desc_drc, sector_code_uppercase) %>%
    right_join(fruitmeatdemand2012, by = c('sector_code_uppercase' = 'BEA_389_code'))
  
  ### 
  # Run EEIO for the baseline, demand averted lower, and demand averted upper
  # Do separately for each food so that we can see the result for each one.
  
  # 4 categories x (1 baseline+1averted) = 8 runs of model
  # Already in units of dollars so don't need to multiply by any factor.
  eeio_packaging_averted <- pmap(fruitmeatdemand2012, function(sector_desc_drc, baseline_demand, demand_averted, ...) {
    demand_code <- as.list(sector_desc_drc)
    list(baseline = eeio_lcia('USEEIO2012', as.list(baseline_demand), demand_code),
         averted = eeio_lcia('USEEIO2012', as.list(demand_averted), demand_code))
  })
  
  # Put this into a data frame
  eeio_packaging_averted <- map2_dfr(c('fresh vegetables', 'fresh fruit', 'poultry', 'meat'), eeio_packaging_averted, function(x, y) {
    impacts <- do.call(cbind, y) %>% setNames(names(y))
    data.frame(food = x, category = row.names(impacts), impacts)
  })
  
  

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
  packaging_annual_offset_bytype <- outer(eeio_packaging_offsetting_impacts$impact, proportion_packaging_byfoodtype$material_cost) %>%
    as.data.frame %>%
    setNames(proportion_packaging_byfoodtype$food) %>%
    mutate(category = eeio_packaging_offsetting_impacts$category) %>%
    pivot_longer(-category, names_to = 'food', values_to = 'offset')
  
  eeio_packaging_result_bytype <- eeio_packaging_averted %>%
    left_join(packaging_annual_offset_bytype) %>%
    left_join(proportion_packaging_byfoodtype %>% select(-n, -p)) %>%
    mutate(net_averted = averted - offset,
           cost_per_reduction = annualized_total_cost / net_averted)
  
  cost_result <- data.frame(initial_cost = initial_cost,
                            material_cost = material_cost,
                            annualized_total_cost = packaging_annual_cost)
  
  cost_result_bytype <- proportion_packaging_byfoodtype %>% select(-n, -p)
  
  return(list(impact = eeio_packaging_result, cost = cost_result, impact_byfoodtype = eeio_packaging_result_bytype, cost_byfoodtype = cost_result_bytype))

}