# Extract cost-effectiveness numbers into a table.

costeffdat <- dat_unitcost_alternate %>% 
  mutate_at(vars(starts_with("q")), ~ 1/.) %>%
  select(intervention:q975) %>%
  select(-name)

write_csv(costeffdat, '~/Dropbox/Q/projects/foodwaste/Data/intervention_uncertainty/costeff_output.csv')