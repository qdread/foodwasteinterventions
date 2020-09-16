table_costbreakdown_alternate <- dat_costbreakdown_alternate %>%
  ungroup %>%
  bind_rows(dat_totalcost_alternate %>% mutate(cost_type = 'total') %>% mutate_if(is.numeric, ~ round(./1e6))) %>%
  mutate_if(is.numeric, round) %>%
  mutate(cost_with_quantiles = paste0(q50, ' (', q05, '; ', q95, ')')) %>%
  select(intervention, cost_type, cost_with_quantiles) %>%
  pivot_wider(names_from = cost_type, values_from = cost_with_quantiles, values_fill = list(cost_with_quantiles = "--")) %>%
  select(intervention, initial_raw, initial_annualized, annual, total) %>%
  setNames(c('Intervention', 'Initial capital cost', 'Annualized initial cost', 'Annual cost', 'Total (annualized + annual) cost'))

table_netcost_alternate <- dat_netcost_alternate %>%
  mutate_if(is.numeric, ~ round(./1e6)) %>%
  mutate(cost_with_quantiles = paste0(q50, ' (', q05, '; ', q95, ')')) %>%
  select(intervention, name, cost_with_quantiles) %>%
  pivot_wider(names_from = name, values_from = cost_with_quantiles, values_fill = list(cost_with_quantiles = "--")) %>%
  select(intervention, total_cost, averted_food_purchase, net_cost) %>%
  arrange(intervention) %>%
  setNames(c('Intervention', 'Cost', 'Averted food purchase', 'Net cost or savings'))

cbind(table_costbreakdown_alternate, table_netcost_alternate[,3:4]) %>%
  write_csv(file.path(fp_out, 'table2.csv')

# Extract additional column for percentage reduction

all_qs %>%
  filter(name %in% c('percent_food_purchase_reduction')) %>%
  filter(is.na(group) | group == 'contracted foodservice operations') %>%
  mutate_if(is.numeric, ~ round(.*100, 2)) %>%
  mutate(percent_reduction = paste0(q50, '% (', q05, '%; ', q95, '%)')) %>%
  arrange(intervention) %>%
  select(intervention, percent_reduction)


# Extract cost-effectiveness numbers into a table.

costeffdat <- dat_unitcost %>% 
  mutate_at(vars(starts_with("q")), ~ 1/.) %>%
  select(intervention:q975) %>%
  select(-name)

write_csv(costeffdat, file.path(fp_out, 'costeff_output.csv')
 