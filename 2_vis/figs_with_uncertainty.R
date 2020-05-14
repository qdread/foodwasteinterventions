# New figures with uncertainty from Monte Carlo
# QDR / foodwasteinterventions / 30 April 2020

# Using local copy of output

library(tidyverse)
library(directlabels)

all_qs <- read_csv('~/Dropbox/Q/projects/foodwaste/Data/intervention_uncertainty/intervention_quantiles.csv')

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
  filter(grepl('cost', name), is.na(category))

# One figure for total costs, one figure for breakdowns.
# For date labeling, use annualized cost, for spoilage prevention use annualized total cost, for cons ed, use sum of 3 components
# for WTA use total cost annualized (names are inconsistent)
dat_totalcost <- dat_cost %>%
  filter(grepl('annual', name) | grepl('consumer', intervention), !grepl('content|media', name), name != 'annualized_initial_cost', is.na(group) | group == 'total') %>%
  group_by(intervention) %>%
  summarize_if(is.numeric, sum)

p_totalcost <- ggplot(dat_totalcost, aes(y = q50/1e6, ymin = q025/1e6, ymax = q975/1e6, x = intervention, color = intervention)) +
  geom_segment(aes(xend = intervention, y = q05/1e6, yend = q95/1e6), size = 2, alpha = 0.5) +
  geom_point(size = 2) +
  geom_errorbar(width = 0.05) +
  scale_x_discrete(labels = c('CEC', 'SPP', 'SDL', 'WTA')) +
  scale_y_continuous(name = 'Total annual cost (million $)', limits = c(0, 4500), expand = c(0,0)) +
  interv_colors +
  theme(axis.text.x = element_text(size = 6), legend.position = 'none')

# Alternate version with only contracted foodservice
dat_totalcost_alternate <- dat_cost %>%
  filter(grepl('annual', name) | grepl('consumer', intervention), !grepl('content|media', name), name != 'annualized_initial_cost', is.na(group) | group == 'contracted foodservice operations') %>%
  group_by(intervention) %>%
  summarize_if(is.numeric, sum)

p_totalcost_alternate <- ggplot(dat_totalcost_alternate, aes(y = q50/1e6, ymin = q025/1e6, ymax = q975/1e6, x = intervention, color = intervention)) +
  geom_segment(aes(xend = intervention, y = q05/1e6, yend = q95/1e6), size = 2, alpha = 0.5) +
  geom_point(size = 2) +
  geom_errorbar(width = 0.05) +
  scale_x_discrete(labels = c('CEC', 'SPP', 'SDL', 'WTA')) +
  scale_y_continuous(name = 'Total annual cost (million $)', limits = c(0, 650), expand = c(0,0)) +
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
  filter(is.na(group) | group %in% 'total') %>%
  mutate(cost_type = case_when( intervention == 'spoilage prevention packaging' & name == 'total_annual_cost' ~ 'other',
                                grepl('^initial', name) ~ 'initial_raw',
                               grepl('annual_|material', name) ~ 'annual',
                              
                               grepl('annualized|equipment_cost', name) ~ 'initial_annualized',
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
  mutate(cost_with_quantiles = paste0(q50, ' (', q05, ',', q95, ')')) %>%
  select(intervention, cost_type, cost_with_quantiles) %>%
  pivot_wider(names_from = cost_type, values_from = cost_with_quantiles, values_fill = list(cost_with_quantiles = "--")) %>%
  select(intervention, initial_raw, initial_annualized, annual, total) %>%
  setNames(c('Intervention', 'Initial capital cost', 'Annualized initial cost', 'Annual cost', 'Total (annualized + annual) cost')) %>%
  knitr::kable('markdown')

### alternate version with only contracted foodservice
dat_costbreakdown_alternate <- dat_cost %>%
  filter(is.na(group) | group %in% 'contracted foodservice operations') %>%
  mutate(cost_type = case_when( intervention == 'spoilage prevention packaging' & name == 'total_annual_cost' ~ 'other',
                                grepl('^initial', name) ~ 'initial_raw',
                                grepl('annual_|material', name) ~ 'annual',
                                
                                grepl('annualized|equipment_cost', name) ~ 'initial_annualized',
                                TRUE ~ 'other')) %>%
  filter(!cost_type %in% 'other') %>%
  group_by(intervention, cost_type) %>%
  summarize_if(is.numeric, ~ sum(.)/1e6)

# Make a figure as well as a table
p_costbreakdown_alternate <- ggplot(dat_costbreakdown_alternate %>% 
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
table_costbreakdown_alternate <- dat_costbreakdown_alternate %>%
  ungroup %>%
  bind_rows(dat_totalcost_alternate %>% mutate(cost_type = 'total') %>% mutate_if(is.numeric, ~ round(./1e6))) %>%
  mutate_if(is.numeric, round) %>%
  mutate(cost_with_quantiles = paste0(q50, ' (', q05, ',', q95, ')')) %>%
  select(intervention, cost_type, cost_with_quantiles) %>%
  pivot_wider(names_from = cost_type, values_from = cost_with_quantiles, values_fill = list(cost_with_quantiles = "--")) %>%
  select(intervention, initial_raw, initial_annualized, annual, total) %>%
  setNames(c('Intervention', 'Initial capital cost', 'Annualized initial cost', 'Annual cost', 'Total (annualized + annual) cost')) %>%
  knitr::kable('markdown')


# Total impact reduced ----------------------------------------------------

dat_netaverted <- all_qs %>%
  filter(grepl('enrg|gcc|land|watr', category), name %in%  c('net_averted', 'net_impact_averted'), is.na(group) | group == 'total') %>%
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

# Alternate showing the contracted food service for WTA
dat_netaverted_alternate <- all_qs %>%
  filter(grepl('enrg|gcc|land|watr', category), name %in%  c('net_averted', 'net_impact_averted'), is.na(group) | group == 'contracted foodservice operations') %>%
  left_join(conv_factors) %>%
  mutate_at(vars(starts_with('q')), ~ . * conv_factor)

p_totalimpact_alternate <- ggplot(dat_netaverted_alternate, aes(x = intervention, color = intervention, y = q50, ymin = q025, ymax = q975)) +
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
  filter(grepl('enrg|gcc|land|watr', category), name %in%  c('cost_per_reduction'), is.na(group) | group == 'total') %>%
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


# Alternate version showing only contracted foodservice.
dat_unitcost_alternate <- all_qs %>%
  filter(grepl('enrg|gcc|land|watr', category), name %in%  c('cost_per_reduction'), is.na(group) | group == 'contracted foodservice operations') %>%
  left_join(conv_factors)

p_unitcost_alternate <- ggplot(dat_unitcost_alternate, aes(x = intervention, color = intervention, y = q50, ymin = q025, ymax = q975)) +
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

p_costeff_alternate <- ggplot(dat_unitcost_alternate, aes(x = intervention, color = intervention, y = 1/q50, ymin = 1/q025, ymax = 1/q975)) +
  facet_wrap(~ costeff_labels, scales = 'free_y', labeller = label_parsed) +
  geom_segment(aes(xend = intervention, y = 1/q05, yend = 1/q95), size = 2, alpha = 0.5) +
  geom_point(size = 2) +
  geom_blank(data = dummy_dat, aes(y = 0, ymin = 0, ymax = 0, yend = 0)) +
  geom_errorbar(width = 0.05) +
  scale_x_discrete(labels = c('CEC', 'SPP', 'SDL', 'WTA')) +
  scale_y_continuous(name = 'Reduction per $1 spent',expand = expansion(mult = c(0, 0.05))) +
  theme(axis.text.x = element_text(size = 6), legend.position = 'none') +
  interv_colors 


# Total cost vs unit cost curve -------------------------------------------

# McKinsey curve: x axis is potential reduction in GT/y, y axis is cost per ton

# If the benefit of reducing 1 kg CO2 is the social cost of carbon, or around 5 cents, you can take 0.05 - cost_per_reduction to get the net cost or net benefit
# $0.05 per kg is $50 per tonne.

dat_mckinsey <- rbind(dat_unitcost, dat_netaverted) %>%
  filter(grepl('gcc', category)) %>%
  select(intervention, name, starts_with('q')) %>%
  mutate(name = gsub('_impact', '', name)) %>%
  pivot_wider(names_from = name, values_from = q025:q975)

p_costbytotal <- 
  ggplot(dat_mckinsey, aes(x = q50_net_averted/1000, y = q50_cost_per_reduction * 1000, color = intervention)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = q025_cost_per_reduction * 1000, ymax = q975_cost_per_reduction * 1000), width = 0.00005) +
  geom_errorbarh(aes(xmin = q025_net_averted/1000, xmax = q975_net_averted/1000)) +
  geom_hline(yintercept = 50, linetype = 'dotted', size = 1.5) +
  theme_bw() +
  scale_x_continuous(name = 'Total potential CO2 reduction (GT)') +
  scale_y_continuous(name = 'Cost per ton CO2 emissions reduced', labels = scales::dollar) +
  interv_colors +
  theme(legend.position = 'bottom')

dat_mckinsey_alternate <- rbind(dat_unitcost_alternate, dat_netaverted) %>%
  filter(grepl('gcc', category)) %>%
  select(intervention, name, starts_with('q')) %>%
  mutate(name = gsub('_impact', '', name)) %>%
  pivot_wider(names_from = name, values_from = q025:q975)

p_costbytotal_alternate <- 
  ggplot(dat_mckinsey_alternate, aes(x = q50_net_averted/1000, y = q50_cost_per_reduction * 1000, color = intervention)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = q025_cost_per_reduction * 1000, ymax = q975_cost_per_reduction * 1000), width = 0.00005) +
  geom_errorbarh(aes(xmin = q025_net_averted/1000, xmax = q975_net_averted/1000)) +
  geom_hline(yintercept = 50, linetype = 'dotted', size = 1.5) +
  theme_bw() +
  scale_x_continuous(name = 'Total potential CO2 reduction (GT)') +
  scale_y_continuous(name = 'Cost per ton CO2 emissions reduced', labels = scales::dollar) +
  interv_colors +
  theme(legend.position = 'bottom')


# Results by food type for SPP --------------------------------------------

# Load additional results
pkg_result <- read_csv('~/Dropbox/Q/projects/foodwaste/Data/intervention_uncertainty/packaging_quantiles_byfoodtype.csv')

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
  scale_y_continuous(name = 'Cost (million $)', expand = expansion(mult = c(0, 0.05))) +
  ggsci::scale_color_startrek(name = 'Cost type', labels = c('Annualized initial cost', 'Annual material cost')) + 
  theme_withxaxis

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
  scale_y_continuous(name = 'Cost per unit reduction', expand = expansion(mult = c(0, 0.05))) +
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

# Results by industry group for WTA ---------------------------------------

group_names_short <- c('contracted\nfoodservice', 'full-service\nrestaurants', 'limited-service\nrestaurants')

dat_cost_wta <- dat_cost %>% 
  filter(grepl('waste', intervention), !group %in% 'total', !name %in% 'initial_cost') %>% 
  mutate_if(is.numeric, ~ ./1e6) %>%
  mutate(group = group_names_short[as.numeric(factor(group))])

pos_dodge <- position_dodge(width = 0.1)

p_wta_cost_byindustry <- ggplot(dat_cost_wta, aes(x = group, y = q50, ymin = q025, ymax = q975, color = name)) +
  geom_errorbar(aes(ymin = q05, ymax = q95), size = 2, alpha = 0.5, width = 0, position = pos_dodge) +
  geom_point(size = 2, position = pos_dodge) +
  geom_errorbar(width = 0.05, position = pos_dodge) +
  scale_y_continuous(name = 'Cost (million $)', expand = expansion(mult = c(0, 0.05))) +
  ggsci::scale_color_jco(name = 'Cost type', labels = c('Fees', 'Wages', 'Equipment (annualized)')) +
  theme_withxaxis +
  theme(legend.position = c(0.25, 0.8), legend.background = element_rect(fill = 'transparent'))

dat_netaverted_wta <- all_qs %>%
  filter(grepl('enrg|gcc|land|watr', category), name %in%  c('net_averted', 'net_impact_averted'), !is.na(group)) %>%
  left_join(conv_factors) %>%
  mutate_at(vars(starts_with('q')), ~ . * conv_factor) %>%
  mutate(group = c(group_names_short, 'total')[as.numeric(factor(group))])

p_wta_impact_byindustry <- ggplot(dat_netaverted_wta, aes(x = group, color = group, y = q50, ymin = q025, ymax = q975)) +
  facet_wrap(~ category_labels, scales = 'free_y', labeller = label_parsed) +
  geom_segment(aes(xend = group, y = q05, yend = q95), size = 2, alpha = 0.5) +
  geom_point(size = 2) +
 # geom_blank(data = dummy_dat) +
  geom_errorbar(width = 0.05) +
  scale_y_continuous(name = 'Net impact averted', expand = expansion(mult = c(0, 0.05))) +
  ggsci::scale_color_futurama() +
  theme(legend.position = 'bottom')


dat_unitcost_wta <- all_qs %>%
  filter(grepl('enrg|gcc|land|watr', category), name %in%  c('cost_per_reduction'), !is.na(group)) %>%
  left_join(conv_factors) %>%
  mutate(group = c(group_names_short, 'total')[as.numeric(factor(group))])

p_wta_unitcost_byindustry <- ggplot(dat_unitcost_wta, aes(x = group, color = group, y = q50, ymin = q025, ymax = q975)) +
  facet_wrap(~ cost_labels, scales = 'free_y', labeller = label_parsed) +
  geom_segment(aes(xend = group, y = q05, yend = q95), size = 2, alpha = 0.5) +
  geom_point(size = 2) +
  geom_errorbar(width = 0.05) +
  scale_y_continuous(name = 'Cost per unit reduction ($)', labels = scales::dollar, expand = expansion(mult = c(0, 0.05))) +
  ggsci::scale_color_futurama() +
  theme(legend.position = 'bottom')

p_wta_costeff_byindustry <- ggplot(dat_unitcost_wta, aes(x = group, color = group, y = 1/q50, ymin = 1/q025, ymax = 1/q975)) +
  facet_wrap(~ costeff_labels, scales = 'free_y', labeller = label_parsed) +
  geom_segment(aes(xend = group, y = 1/q05, yend = 1/q95), size = 2, alpha = 0.5) +
  geom_point(size = 2) +
  geom_errorbar(width = 0.05) +
  scale_y_continuous(name = 'Reduction per $1 spent',expand = expansion(mult = c(0, 0.05))) +
  ggsci::scale_color_futurama() +
  theme(legend.position = 'bottom')


# Export figures ----------------------------------------------------------

figs <- list(
  p_totalcost,
  p_totalcost_alternate,
  p_costbreakdown,
  p_costbreakdown_alternate,
  p_costbytotal,
  p_costbytotal_alternate,
  p_pkgtotalcostbyfood,
  p_wta_cost_byindustry
)

figs_multipanel <- list(
  p_totalimpact,
  p_totalimpact_alternate,
  p_unitcost,
  p_unitcost_alternate,
  p_costeff,
  p_costeff_alternate,
  p_pkgimpactbyfood,
  p_pkgcostbyfood,
  p_pkgcosteffbyfood,
  p_wta_impact_byindustry,
  p_wta_unitcost_byindustry,
  p_wta_costeff_byindustry
)

fig_names <- c(
  'total_cost_v1',
  'total_cost_v2',
  'total_cost_breakdown_v1',
  'total_cost_breakdown_v2',
  'mckinsey_plot_v1',
  'mckinsey_plot_v2',
  'packaging_total_cost_by_food_type',
  'wta_total_cost_by_industry'
)

fig_multipanel_names <- c(
  'total_impact_averted_v1',
  'total_impact_averted_v2',
  'cost_per_unit_reduction_v1',
  'cost_per_unit_reduction_v2',
  'cost_effectiveness_v1',
  'cost_effectiveness_v2',
  'packaging_impact_averted_by_food_type',
  'packaging_cost_per_reduction_by_food_type',
  'packaging_cost_effectiveness_by_food_type',
  'wta_impact_averted_by_industry',
  'wta_cost_per_reduction_by_industry',
  'wta_cost_effectiveness_by_industry'
)

fp_fig <- '~/google_drive/SESYNC Food Waste/MS3_Interventions/figs'

walk2(figs, fig_names, ~ ggsave(file.path(fp_fig, paste0(.y, '.png')), .x, dpi = 400, height = 4, width = 5))
walk2(figs_multipanel, fig_multipanel_names, ~ ggsave(file.path(fp_fig, paste0(.y, '.png')), .x, dpi = 400))