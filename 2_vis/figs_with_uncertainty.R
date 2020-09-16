# New figures with uncertainty from Monte Carlo
# QDR / foodwasteinterventions / 30 April 2020

# Using local copy of output

# Modified 21 May 2020: Include net cost or savings

library(tidyverse)
library(directlabels)

fp_out <- '.'

all_qs <- read_csv(file.path(fp_out, 'intervention_quantiles.csv'))
pkg_result <- read_csv(file.path(fp_out, 'packaging_quantiles_byfoodtype.csv'))

interv_colors <- scale_color_brewer(type = 'qual', palette = 'Set2', guide = guide_legend(nrow = 2))

theme_set(theme_bw() +
            theme(panel.grid.major.x = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  axis.text.x = element_blank(),
                  axis.title.x = element_blank(),
                  axis.ticks.x = element_blank(),
                  strip.background = element_blank(),
                  legend.position = 'none'))

# Define data frame with better formatted labels and conversion factors to join to the raw data
conv_factors <- data.frame(category = c("resource use/enrg/mj", "impact potential/gcc/kg co2 eq", "resource use/land/m2*yr", "resource use/watr/m3"),
                           category_labels = c('energy~(PJ)', 'greenhouse~gas~(MT~CO[2])', 'land~(Mha)', 'water~(km^3)'),
                           cost_labels = c('energy~("$"/MJ)', 'greenhouse~gas~("$"/kg~CO[2])', 'land~("$"/m^2)', 'water~("$"/m^3)'),
                           costeff_labels = c('energy~(MJ/"$")', 'greenhouse~gas~(kg~CO[2]/"$")', 'land~(m^2/"$")', 'water~(m^3/"$")'),
                           conv_factor = c(1e-9, 1e-9, 1e-10, 1e-9))

# Define dummy dataframe to stick in and make the axes always go down to zero
dummy_dat <- data.frame(q025 = 0, q50 = 0, q975 = 0, category_labels = c('energy~(PJ)', 'greenhouse~gas~(MT~CO[2])', 'land~(Mha)', 'water~(km^3)'), intervention = NA)

# Total cost --------------------------------------------------------------

dat_cost <- all_qs %>%
  filter(grepl('cost', name), is.na(category)) %>%
  filter(!grepl('net_cost', name))

# One figure for total costs, one figure for breakdowns.
# For date labeling, use annualized cost, for spoilage prevention use annualized total cost, for cons ed, use sum of 3 components
# for WTA use total cost annualized (names are inconsistent)
dat_totalcost <- dat_cost %>%
  filter(grepl('annual', name) | grepl('consumer', intervention), !grepl('content|media', name), name != 'annualized_initial_cost', is.na(group) | group == 'contracted foodservice operations') %>%
  group_by(intervention) %>%
  summarize_if(is.numeric, sum)

p_totalcost <- ggplot(dat_totalcost, aes(y = q50/1e6, ymin = q025/1e6, ymax = q975/1e6, x = intervention, color = intervention)) +
  geom_segment(aes(xend = intervention, y = q05/1e6, yend = q95/1e6), size = 2, alpha = 0.5) +
  geom_point(size = 2) +
  geom_errorbar(width = 0.05) +
  scale_x_discrete(labels = c('CEC', 'SPP', 'SDL', 'WTA')) +
  scale_y_continuous(name = 'Total annual cost (million $)', limits = c(0, 850), expand = c(0,0)) +
  interv_colors +
  theme(axis.text.x = element_text(size = 6), legend.position = 'none')

# Cost breakdowns
# SDL: all initial, no recurring (might be able to break down by type of food)
# SPP: initial costs (testing) and recurring cost (also can break down by type of food)
# CEC: all recurring, no initial (the campaigns are redone every year)
# WTA: initial costs (purchase equipment) and recurring cost (licenses and fees, costs to employers in wages & benefits)

# For each one, show
# initial cost (raw)
# initial cost (annualized)
# recurring annual cost

# assign a cost type column 

dat_costbreakdown <- dat_cost %>%
  filter(is.na(group) | group %in% 'contracted foodservice operations') %>%
  mutate(cost_type = case_when( intervention == 'spoilage prevention packaging' & name == 'total_annual_cost' ~ 'other',
                                grepl('^initial', name) ~ 'initial_raw',
                                grepl('annual_|material|_annual', name) ~ 'annual',
                                
                                grepl('annualized', name) ~ 'initial_annualized',
                                TRUE ~ 'other')) %>%
  filter(!cost_type %in% 'other') %>%
  group_by(intervention, cost_type) %>%
  summarize_if(is.numeric, ~ sum(.)/1e6)


# Make a figure as well as a table
p_costbreakdown <- ggplot(dat_costbreakdown %>% 
                                      ungroup %>%
                                      filter(!cost_type %in% 'initial_raw') %>%
                                      mutate(intervention = factor(intervention, levels = sort(unique(intervention), decreasing = TRUE))), 
                                    aes(x = intervention, y = q50)) +
  geom_col(aes(fill = cost_type), position = 'stack') +
  scale_fill_manual(name = 'Cost type', values = c('gray50', 'black'), labels = c('recurring\nannual cost', 'annualized initial\ncapital investment')) +
  scale_y_continuous(name = 'Annual cost (million $)', expand = expansion(mult = c(0, 0.05))) +
  theme(axis.text.x = element_text(), axis.title.x = element_text(), legend.position = 'bottom') +
  coord_flip()

# Reformat for table
table_costbreakdown <- dat_costbreakdown %>%
  ungroup %>%
  bind_rows(dat_totalcost %>% mutate(cost_type = 'total') %>% mutate_if(is.numeric, ~ round(./1e6))) %>%
  mutate_if(is.numeric, round) %>%
  mutate(cost_with_quantiles = paste0(q50, ' (', q05, '; ', q95, ')')) %>%
  select(intervention, cost_type, cost_with_quantiles) %>%
  pivot_wider(names_from = cost_type, values_from = cost_with_quantiles, values_fill = list(cost_with_quantiles = "--")) %>%
  select(intervention, initial_raw, initial_annualized, annual, total) %>%
  setNames(c('Intervention', 'Initial capital cost', 'Annualized initial cost', 'Annual cost', 'Total (annualized + annual) cost')) %>%
  knitr::kable('markdown')


# Averted food purchase and net cost --------------------------------------

dat_netcost <- all_qs %>%
  filter(name %in% c('averted_food_purchase', 'net_cost')) %>%
  filter(is.na(group) | group == 'contracted foodservice operations') %>%
  bind_rows(dat_totalcost %>% mutate(name = 'total_cost')) %>%
  mutate_if(is_numeric, ~ if_else(name == 'averted_food_purchase', -., .))

pd <- position_dodge(width = 0.1)

p_netcost <- ggplot(dat_netcost %>% mutate_if(is.numeric, ~ ./1e6), aes(x = intervention, y = q50, color = name)) +
  geom_hline(yintercept = 0, linetype = 'dotted') +
  geom_errorbar(aes(ymin = q05, ymax = q95), size = 2, alpha = 0.5, width = 0, position = pd) +
  geom_point(size = 2, position = pd) +
  geom_errorbar(aes(ymin = q025, ymax = q975), width = 0.05, position = pd) +
  scale_x_discrete(labels = c('CEC', 'SPP', 'SDL', 'WTA')) +
  scale_y_continuous(name = 'Total annual cost (million $)', expand = expansion(mult = 0.01)) +
  ggsci::scale_color_nejm(labels = c('Averted food purchases', 'Net cost or savings', 'Cost of implementation')) +
  theme(axis.text.x = element_text(size = 6), legend.position = 'bottom', legend.title = element_blank(),
        axis.text.y = element_text(color = c(rep('forestgreen',3),'black')))

# Tables with total cost, averted food purchase, and net cost or savings
table_netcost <- dat_netcost %>%
  mutate_if(is.numeric, ~ round(./1e6)) %>%
  mutate(cost_with_quantiles = paste0(q50, ' (', q05, '; ', q95, ')')) %>%
  select(intervention, name, cost_with_quantiles) %>%
  pivot_wider(names_from = name, values_from = cost_with_quantiles, values_fill = list(cost_with_quantiles = "--")) %>%
  select(intervention, total_cost, averted_food_purchase, net_cost) %>%
  arrange(intervention) %>%
  setNames(c('Intervention', 'Cost', 'Averted food purchase', 'Net cost or savings')) %>%
  knitr::kable('markdown')

# Total impact reduced ----------------------------------------------------

dat_netaverted <- all_qs %>%
  filter(grepl('enrg|gcc|land|watr', category), name %in%  c('net_averted', 'net_impact_averted'), is.na(group) | group == 'contracted foodservice operations') %>%
  left_join(conv_factors) %>%
  mutate_at(vars(starts_with('q')), ~ . * conv_factor)

p_totalimpact <- ggplot(dat_netaverted, aes(x = intervention, color = intervention, y = q50, ymin = q025, ymax = q975)) +
  facet_wrap(~ category_labels, scales = 'free_y', labeller = label_parsed) +
  geom_segment(aes(xend = intervention, y = q05, yend = q95), size = 2, alpha = 0.5) +
  geom_point(size = 2) +
  geom_blank(data = dummy_dat) +
  geom_errorbar(width = 0.05) +
  scale_x_discrete(labels = c('CEC', 'SPP', 'SDL', 'WTA')) +
  scale_y_continuous(name = 'Net impact averted', expand = expansion(mult = c(0, 0.05))) +
  theme(axis.text.x = element_text(size = 6), legend.position = 'none') +
  interv_colors 

# Cost per unit reduction -------------------------------------------------

dat_unitcost <- all_qs %>%
  filter(grepl('enrg|gcc|land|watr', category), name %in%  c('cost_per_reduction'), is.na(group) | group == 'contracted foodservice operations') %>%
  left_join(conv_factors)

p_unitcost <- ggplot(dat_unitcost, aes(x = intervention, color = intervention, y = q50, ymin = q025, ymax = q975)) +
  facet_wrap(~ cost_labels, scales = 'free_y', labeller = label_parsed) +
  geom_segment(aes(xend = intervention, y = q05, yend = q95), size = 2, alpha = 0.5) +
  geom_point(size = 2) +
  geom_blank(data = dummy_dat) +
  geom_errorbar(width = 0.05) +
  scale_x_discrete(labels = c('CEC', 'SPP', 'SDL', 'WTA')) +
  scale_y_continuous(name = 'Cost per unit reduction ($)', labels = scales::dollar, expand = expansion(mult = c(0, 0.05))) +
  theme(axis.text.x = element_text(size = 6), legend.position = 'none') +
  interv_colors 


# Reduction per unit cost (cost-effectiveness) ----------------------------

p_costeff <- ggplot(dat_unitcost, aes(x = intervention, color = intervention, y = 1/q50, ymin = 1/q025, ymax = 1/q975)) +
  facet_wrap(~ costeff_labels, scales = 'free_y', labeller = label_parsed) +
  geom_segment(aes(xend = intervention, y = 1/q05, yend = 1/q95), size = 2, alpha = 0.5) +
  geom_point(size = 2) +
  geom_blank(data = dummy_dat, aes(y = 0, ymin = 0, ymax = 0, yend = 0)) +
  geom_errorbar(width = 0.05) +
  scale_x_discrete(labels = c('CEC', 'SPP', 'SDL', 'WTA')) +
  scale_y_continuous(name = 'Reduction per $1 spent',expand = expansion(mult = c(0, 0.05))) +
  theme(axis.text.x = element_text(size = 6), legend.position = 'none') +
  interv_colors 


# Results by food type for SPP --------------------------------------------

food_colors <- ggsci::scale_color_jco()

# Total annualized+annual cost for each food
pkg_totalcost_byfood <- pkg_result %>% filter(name %in% c('annualized_initial_cost', 'annualized_total_cost', 'material_cost')) %>%
  group_by(food, name) %>%
  slice(1) %>%
  ungroup %>%
  select(-category) %>%
  mutate_if(is.numeric, ~ ./1e6)

theme_withxaxis <- theme_bw() +
            theme(panel.grid.major.x = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  strip.background = element_blank(),
                  legend.position = 'bottom')

pos_dodge <- position_dodge(width = 0.1)

p_pkgtotalcostbyfood <- ggplot(pkg_totalcost_byfood %>% filter(name %in% c('annualized_initial_cost', 'material_cost')), aes(x = food, color = name, group = name, y = q50, ymin = q025, ymax = q975)) +
  geom_errorbar(aes(ymin = q05, ymax = q95), size = 2, alpha = 0.5, width = 0, position = pos_dodge) +
  geom_point(size = 2, position = pos_dodge) +
  geom_errorbar(width = 0.05, position = pos_dodge) +
  scale_y_continuous(name = 'Cost (million $)', limits = c(0, 255), expand = c(0,0)) +
  ggsci::scale_color_startrek(name = 'Cost type', labels = c('Annualized initial cost', 'Annual material cost')) + 
  theme_withxaxis

# Cost, averted food purchase, and net cost or savings for each food
pkg_netcost_byfood <- pkg_result %>% filter(name %in% c('annualized_total_cost', 'averted_food_purchase', 'net_cost')) %>%
  group_by(food, name) %>%
  slice(1) %>%
  ungroup %>%
  select(-category) %>%
  mutate_if(is.numeric, ~ ./1e6) %>%
  mutate_if(is_numeric, ~ if_else(name == 'averted_food_purchase', -., .))

p_pkgnetcostbyfood <- ggplot(pkg_netcost_byfood %>% mutate(name = if_else(name == 'annualized_total_cost', 'total_cost', name)), aes(x = food, y = q50, color = name)) +
  geom_hline(yintercept = 0, linetype = 'dotted') +
  geom_errorbar(aes(ymin = q05, ymax = q95), size = 2, alpha = 0.5, width = 0, position = pos_dodge) +
  geom_point(size = 2, position = pos_dodge) +
  geom_errorbar(aes(ymin = q025, ymax = q975), width = 0.05, position = pos_dodge) +
  scale_y_continuous(name = 'Cost (million $)', expand = expansion(mult = 0.01)) +
  ggsci::scale_color_nejm(labels = c('Averted food purchases', 'Net cost or savings', 'Cost of implementation')) +
  theme_withxaxis +
  theme(legend.position = 'bottom', legend.title = element_blank(),
        axis.text.y = element_text(color = c('forestgreen','forestgreen','black')))

# Averted impacts for each food
pkg_averted_byfood <- pkg_result %>% filter(name %in% 'net_averted', grepl('enrg|gcc|land|watr', category)) %>%
  left_join(conv_factors) %>%
  mutate_at(vars(starts_with('q')), ~ . * conv_factor)

p_pkgimpactbyfood <- ggplot(pkg_averted_byfood %>% filter(!food %in% 'misc'), aes(x = food, color = food, y = q50, ymin = q025, ymax = q975)) +
  facet_wrap(~ category_labels, scales = 'free_y', labeller = label_parsed) +
  geom_segment(aes(xend = food, y = q05, yend = q95), size = 2, alpha = 0.5) +
  geom_point(size = 2) +
  geom_errorbar(width = 0.05) +
  scale_y_continuous(name = 'Net impact averted', expand = expansion(mult = c(0, 0.05))) +
  theme_withxaxis +
  food_colors

# Cost per reduction for each food
pkg_cost_byfood <- pkg_result %>%
filter(grepl('enrg|gcc|land|watr', category), name %in%  c('cost_per_reduction')) %>%
  left_join(conv_factors)

p_pkgcostbyfood <- ggplot(pkg_cost_byfood %>% filter(!food %in% 'misc'), aes(x = food, color = food, y = q50, ymin = q025, ymax = q975)) +
  facet_wrap(~ cost_labels, scales = 'free_y', labeller = label_parsed) +
  geom_segment(aes(xend = food, y = q05, yend = q95), size = 2, alpha = 0.5) +
  geom_point(size = 2) +
  geom_errorbar(width = 0.05) +
  scale_y_continuous(name = 'Cost per unit reduction') +
  theme_withxaxis +
  food_colors

# Cost-effectiveness for each food
p_pkgcosteffbyfood <- ggplot(pkg_cost_byfood %>% filter(!food %in% 'misc'), aes(x = food, color = food, y = 1/q50, ymin = 1/q025, ymax = 1/q975)) +
  facet_wrap(~ costeff_labels, scales = 'free_y', labeller = label_parsed) +
  geom_segment(aes(xend = food, y = 1/q05, yend = 1/q95), size = 2, alpha = 0.5) +
  geom_point(size = 2) +
  geom_errorbar(width = 0.05) +
  scale_y_continuous(name = 'Reduction per $1 spent', expand = expansion(mult = c(0, 0.05))) +
  theme_withxaxis +
  food_colors


