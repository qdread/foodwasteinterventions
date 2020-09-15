# Extract cost-effectiveness numbers into a table.

costeffdat <- dat_unitcost_alternate %>% 
  mutate_at(vars(starts_with("q")), ~ 1/.) %>%
  select(intervention:q975) %>%
  select(-name)

write_csv(costeffdat, '~/Dropbox/Q/projects/foodwaste/Data/intervention_uncertainty/costeff_output.csv')

# Reshape costeffdat to only include medians

costeff_wide <- costeffdat %>%
  select(intervention, category, q50) %>%
  pivot_wider(names_from=category, values_from=q50) %>%
  arrange(intervention) %>%
  select(1,5,4,2,3) 

write.table(costeff_wide[,-1], sep = ',', row.names = F)
