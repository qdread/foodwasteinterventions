# Test to ensure that the results of uncertainty analysis are plausible, as well as code to make quantiles.

n <- 10

datelabel_results <- pmap(datelabel_par_draws[1:n,], standardized_date_labeling)
packaging_results <- pmap(packaging_par_draws[1:n,], spoilage_prevention_packaging)
consumered_results <- pmap(consumered_par_draws[1:n,], consumer_education_campaigns)
wta_results <- pmap(wta_par_draws[1:n,], waste_tracking_analytics)

# Calculate quantiles
# Takes as argument the list with impact and cost
# Groups by whatever grouping variables are specified, if there are any in addition to category
# Calculates summary statistics for each of the columns.
uncertainty_quantiles <- function(x, group = NULL) {
  impacts <- imap_dfr(x, ~ data.frame(iter = .y, .x$impact))
  costs <- imap_dfr(x, ~ data.frame(iter = .y, .x$cost))
  
  if (is.null(group)) {
    
    impact_quantiles <- impacts %>% 
      select(-iter) %>% 
      pivot_longer(-c(category)) %>% 
      group_by(category, name) %>% 
      summarize(q025 = quantile(value, probs = 0.025),
                q05 = quantile(value, probs = 0.05),
                q50 = quantile(value, probs =  0.5),
                q95 = quantile(value, probs = 0.95),
                q975 = quantile(value, probs = 0.975)) 
    cost_quantiles <- costs %>%
      pivot_longer(-iter) %>%
      group_by(name) %>%
      summarize(q025 = quantile(value, probs = 0.025),
                q05 = quantile(value, probs = 0.05),
                q50 = quantile(value, probs =  0.5),
                q95 = quantile(value, probs = 0.95),
                q975 = quantile(value, probs = 0.975)) 
  } else {
    impact_quantiles <- impacts %>% 
      select(-iter) %>% 
      pivot_longer(-c(category, group)) %>% 
      group_by(group, category, name) %>% 
      summarize(q025 = quantile(value, probs = 0.025),
                q05 = quantile(value, probs = 0.05),
                q50 = quantile(value, probs =  0.5),
                q95 = quantile(value, probs = 0.95),
                q975 = quantile(value, probs = 0.975)) 
    cost_quantiles <- costs %>%
      pivot_longer(-c(iter, group)) %>%
      group_by(group, name) %>%
      summarize(q025 = quantile(value, probs = 0.025),
                q05 = quantile(value, probs = 0.05),
                q50 = quantile(value, probs =  0.5),
                q95 = quantile(value, probs = 0.95),
                q975 = quantile(value, probs = 0.975)) 
  }
  return(list(impact = impact_quantiles, cost = cost_quantiles))
}

datelabel_qs <- uncertainty_quantiles(datelabel_results)
packaging_qs <- uncertainty_quantiles(packaging_results)
consumered_qs <- uncertainty_quantiles(consumered_results)
wta_qs <- uncertainty_quantiles(wta_results, group = TRUE)


# Check the validity ------------------------------------------------------

theme_set(theme_minimal())

datelabel_qs$impact %>%
  filter(grepl('gcc|land|enrg|watr', category), name == 'cost_per_reduction_coordination') %>%
  ggplot(aes(x = category, y = q50, ymin = q025, ymax = q975)) +
  geom_pointrange()

packaging_qs$impact %>%
  filter(grepl('gcc|land|enrg|watr', category), name == 'cost_per_reduction') %>%
  ggplot(aes(x = category, y = q50, ymin = q025, ymax = q975)) +
  geom_pointrange()

consumered_qs$impact %>%
  filter(grepl('gcc|land|enrg|watr', category), name == 'cost_per_reduction') %>%
  ggplot(aes(x = category, y = q50, ymin = q025, ymax = q975)) +
  geom_pointrange()

wta_qs$impact %>%
  filter(grepl('gcc|land|enrg|watr', category), name == 'cost_per_reduction') %>%
  ggplot(aes(x = group, y = q50, ymin = q025, ymax = q975)) +
  facet_wrap(~ category, scales = 'free_y') +
  geom_pointrange()
