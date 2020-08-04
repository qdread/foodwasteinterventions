# Figures to show net societal savings for each intervention
# to emphasize the difference between 3 consumer-based interventions and the 1 other one.

# Averted food purchase per unit dollar spent


dat_averted_food <- all_qs %>% 
  filter(name == 'averted_food_purchase', is.na(group) | group == 'total') %>%
  bind_rows(dat_totalcost_alternate %>% mutate(name = 'total cost')) %>%
  select(-category, -group) %>% 
  pivot_longer(-c(intervention, name), names_to = 'quantile') %>%
  pivot_wider(id_cols = c(intervention, quantile)) %>%
  mutate(savings_per_cost = averted_food_purchase/`total cost`)

# Figure with the savings per cost.

dat_savingspercost <- dat_averted_food %>%
  select(intervention, quantile, savings_per_cost) %>%``
  pivot_wider(names_from = quantile, values_from = savings_per_cost)

p_savingspercost <- ggplot(dat_savingspercost, aes(x = intervention, color = intervention, y = q50, ymin = q025, ymax = q975)) +
  geom_segment(aes(xend = intervention, y = q05, yend = q95), size = 2, alpha = 0.5) +
  geom_point(size = 2) +
  geom_errorbar(width = 0.05) +
  scale_x_discrete(labels = c('CEC', 'SPP', 'SDL', 'WTA')) +
  scale_y_continuous(name = 'Savings multiplier', expand = expansion(mult = c(0, 0)), labels = scales::dollar, limits = c(0,20)) +
  theme(axis.text.x = element_text(size = 6), legend.position = 'none') +
  interv_colors 
