# Compile uncertainty results from each intervention and calculate quantiles
# QDR / foodwasteinterventions / 29 April 2020


# Function to calculate quantiles -----------------------------------------

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


# Load data and calculate for each intervention ---------------------------

library(tidyverse)

is_local <- dir.exists('Q:/')
fp <- ifelse(is_local, 'Q:', '/nfs/qread-data')
fp_github <- file.path(ifelse(is_local, '~/Documents/GitHub/foodwaste', '~'))
fp_out <- file.path(fp, 'scenario_results/intervention_uncertainty')

# Load all RData files in results directory
walk(dir(fp_out, pattern = '*.RData', full.names = TRUE), load, .GlobalEnv)

datelabel_qs <- uncertainty_quantiles(datelabel_results)
packaging_qs <- uncertainty_quantiles(packaging_results)
consumered_qs <- uncertainty_quantiles(consumered_results)
wta_qs <- uncertainty_quantiles(wta_results, group = TRUE)

all_qs <- map2_dfr(list(datelabel_qs, packaging_qs, consumered_qs, wta_qs),
                   c('standardized date labeling', 'spoilage prevention packaging', 'consumer education campaigns', 'waste tracking and analytics'),
                   ~ data.frame(intervention = .y,
                                bind_rows(.x$impact, .x$cost))) %>%
  mutate(name = gsub('_coordination', '', name)) # clean up the names in standardized date labeling part

write_csv(all_qs, file.path(fp_out, 'intervention_quantiles.csv'))


# Get separate results for packaging by food type -------------------------

packaging_impact_qs_byfoodtype <- imap_dfr(packaging_results, ~ data.frame(iter = .y, .x$impact_byfoodtype)) %>%
  select(-iter) %>% 
  pivot_longer(-c(category, food)) %>% 
  group_by(food, category, name) %>% 
  summarize(q025 = quantile(value, probs = 0.025),
            q05 = quantile(value, probs = 0.05),
            q50 = quantile(value, probs =  0.5),
            q95 = quantile(value, probs = 0.95),
            q975 = quantile(value, probs = 0.975)) 
packaging_cost_qs_byfoodtype <- imap_dfr(packaging_results, ~ data.frame(iter = .y, .x$cost_byfoodtype)) %>%
  select(-iter) %>% 
  pivot_longer(-c(food)) %>% 
  group_by(food, name) %>% 
  summarize(q025 = quantile(value, probs = 0.025),
            q05 = quantile(value, probs = 0.05),
            q50 = quantile(value, probs =  0.5),
            q95 = quantile(value, probs = 0.95),
            q975 = quantile(value, probs = 0.975)) 

packaging_all_qs_byfoodtype <- bind_rows(packaging_impact_qs_byfoodtype, packaging_cost_qs_byfoodtype)

write_csv(packaging_all_qs_byfoodtype, file.path(fp_out, 'packaging_quantiles_byfoodtype.csv'))
