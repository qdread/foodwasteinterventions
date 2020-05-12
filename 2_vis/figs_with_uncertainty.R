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
                  legend.position = 'bottom'))

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
# For date labeling, use annualized cost, for spoilage prevention usd annualized total cost, for cons ed, use sum of 3 components
# for WTA use total cost annualized (names are inconsistent)
dat_totalcost <- dat_cost %>%
  filter(grepl('annual', name) | grepl('consumer', intervention), !grepl('equipment', name), is.na(group) | group == 'total') %>%
  group_by(intervention) %>%
  summarize_if(is.numeric, sum)

p_totalcost <- ggplot(dat_totalcost, aes(y = q50/1e6, ymin = q025/1e6, ymax = q975/1e6, x = intervention, color = intervention)) +
  geom_segment(aes(xend = intervention, y = q05/1e6, yend = q95/1e6), size = 2, alpha = 0.5) +
  geom_point(size = 2) +
  geom_errorbar(width = 0.05) +
  scale_y_continuous(name = 'Total annual cost (million $)') +
  interv_colors 

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
  mutate(cost_type = case_when(grepl('^initial', name) ~ 'initial_raw',
                               grepl('annual_', name) ~ 'annual',
                               grepl('annualized|equipment_cost', name) ~ 'initial_annualized',
                               TRUE ~ 'other')) %>%
  filter(!cost_type %in% 'other') %>%
  group_by(intervention, cost_type) %>%
  summarize_if(is.numeric, ~ sum(.)/1e6)
  
# Make a figure as well as a table
ggplot(dat_costbreakdown %>% filter(!cost_type %in% 'initial_raw'), aes(x = intervention, y = q50)) +
  geom_col(aes(fill = intervention, alpha = cost_type), position = 'stack') +
  interv_colors +
  scale_alpha_manual(values = c(0.6, 0.9), labels = c('recurring annual cost', 'annualized initial capital investment')) +
  scale_y_continuous(name = 'Annual cost (million $)', expand = expansion(mult = c(0, 0.05)))
  
# Reformat for table
dat_costbreakdown %>%
  ungroup %>%
  bind_rows(dat_totalcost %>% mutate(cost_type = 'total') %>% mutate_if(is.numeric, ~ round(./1e6))) %>%
  mutate_if(is.numeric, round) %>%
  mutate(cost_with_quantiles = paste0(q50, ' (', q05, ',', q95, ')')) %>%
  select(intervention, cost_type, cost_with_quantiles) %>%
  pivot_wider(names_from = cost_type, values_from = cost_with_quantiles, values_fill = list(cost_with_quantiles = "0")) %>%
  select(intervention, initial_raw, initial_annualized, annual, total) %>%
  knitr::kable('pandoc')



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
  scale_y_continuous(name = 'Net impact averted', expand = expansion(mult = c(0, 0.05))) +
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
  scale_y_continuous(name = 'Cost per unit reduction ($)', labels = scales::dollar, expand = expansion(mult = c(0, 0.05))) +
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
  scale_y_continuous(name = 'Cost per unit reduction ($)', labels = scales::dollar, expand = expansion(mult = c(0, 0.05))) +
  interv_colors 


# Reduction per unit cost (cost-effectiveness) ----------------------------

p_costeff <- ggplot(dat_unitcost, aes(x = intervention, color = intervention, y = 1/q50, ymin = 1/q025, ymax = 1/q975)) +
  facet_wrap(~ costeff_labels, scales = 'free_y', labeller = label_parsed) +
  geom_segment(aes(xend = intervention, y = 1/q05, yend = 1/q95), size = 2, alpha = 0.5) +
  geom_point(size = 2) +
  geom_blank(data = dummy_dat, aes(y = 0, ymin = 0, ymax = 0, yend = 0)) +
  geom_errorbar(width = 0.05) +
  scale_y_continuous(name = 'Reduction per $1 spent',expand = expansion(mult = c(0, 0.05))) +
  interv_colors 

p_costeff_alternate <- ggplot(dat_unitcost_alternate, aes(x = intervention, color = intervention, y = 1/q50, ymin = 1/q025, ymax = 1/q975)) +
  facet_wrap(~ costeff_labels, scales = 'free_y', labeller = label_parsed) +
  geom_segment(aes(xend = intervention, y = 1/q05, yend = 1/q95), size = 2, alpha = 0.5) +
  geom_point(size = 2) +
  geom_blank(data = dummy_dat, aes(y = 0, ymin = 0, ymax = 0, yend = 0)) +
  geom_errorbar(width = 0.05) +
  scale_y_continuous(name = 'Reduction per $1 spent',expand = expansion(mult = c(0, 0.05))) +
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
