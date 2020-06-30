cbind(table_costbreakdown_alternate, table_netcost_alternate[,3:4]) %>%
  write_csv('~/google_drive/SESYNC Food Waste/MS3_Interventions/figs/table2.csv')

# Extract additional column for percentage reduction

all_qs %>%
  filter(name %in% c('percent_food_purchase_reduction')) %>%
  filter(is.na(group) | group == 'contracted foodservice operations') %>%
  mutate_if(is.numeric, ~ round(.*100, 2)) %>%
  mutate(percent_reduction = paste0(q50, '% (', q05, '%; ', q95, '%)')) %>%
  arrange(intervention) %>%
  select(intervention, percent_reduction)
 