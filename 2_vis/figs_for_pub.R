# Publication-quality figures.
# QDR / Foodwaste interventions / 04 Aug 2020

# Figure 1 : total impact
# Figure 2 : cost-effectiveness 
# Supplemental figures:
# 1 is net cost and differences for SPP intervention by food type
# 2 is impact reduction for SPP by food type
# 3 is cost effectiveness for SPP by food type
# 4 is refed comparison of total cost
# 5 is refed comparison of GHG
# 6 is refed comparison of water use


# Load and wrangle figure data --------------------------------------------

library(tidyverse)
library(readxl)
library(units)

fp_out <- '.'

all_qs <- read_csv(file.path(fp_out, 'intervention_quantiles.csv'))
pkg_result <- read_csv(file.path(fp_out, 'packaging_quantiles_byfoodtype.csv'))

refed <- read_xlsx(file.path(fp_data, 'ReFED-Data-Set.xlsx'), .name_repair = 'universal') %>%
  setNames(gsub('[\\.]+', '_', names(.)))

# Define data frame with better formatted labels and conversion factors to join to the raw data
conv_factors <- data.frame(category = c("resource use/enrg/mj", "impact potential/gcc/kg co2 eq", "resource use/land/m2*yr", "resource use/watr/m3"),
                           category_labels = c('energy~(PJ)', 'greenhouse~gas~(MT~CO[2])', 'land~(Mha)', 'water~(km^3)'),
                           cost_labels = c('energy~("$"/MJ)', 'greenhouse~gas~("$"/kg~CO[2])', 'land~("$"/m^2)', 'water~("$"/m^3)'),
                           costeff_labels = c('energy~(MJ/"$")', 'greenhouse~gas~(kg~CO[2]/"$")', 'land~(m^2/"$")', 'water~(m^3/"$")'),
                           conv_factor = c(1e-9, 1e-9, 1e-10, 1e-9))

# Define dummy dataframe to stick in and make the axes always go down to zero
dummy_dat <- data.frame(q025 = 0, q50 = 0, q975 = 0, category_labels = c('energy~(PJ)', 'greenhouse~gas~(MT~CO[2])', 'land~(Mha)', 'water~(km^3)'), intervention = NA)

### Total cost

dat_cost <- all_qs %>%
  filter(grepl('cost', name), is.na(category)) %>%
  filter(!grepl('net_cost', name))

# For date labeling, use annualized cost, for spoilage prevention use annualized total cost, for cons ed, use sum of 3 components
# for WTA use total cost annualized (names are inconsistent)
dat_totalcost <- dat_cost %>%
  filter(grepl('annual', name) | grepl('consumer', intervention), !grepl('content|media', name), name != 'annualized_initial_cost', is.na(group) | group == 'contracted foodservice operations') %>%
  group_by(intervention) %>%
  summarize_if(is.numeric, sum)

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

### Averted food purchase and net cost

dat_netcost <- all_qs %>%
  filter(name %in% c('averted_food_purchase', 'net_cost')) %>%
  filter(is.na(group) | group == 'contracted foodservice operations') %>%
  bind_rows(dat_totalcost %>% mutate(name = 'total_cost')) %>%
  mutate_if(is_numeric, ~ if_else(name == 'averted_food_purchase', -., .))

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

### Total averted impact

dat_netaverted <- all_qs %>%
  filter(grepl('enrg|gcc|land|watr', category), name %in%  c('net_averted', 'net_impact_averted'), is.na(group) | group == 'contracted foodservice operations') %>%
  left_join(conv_factors) %>%
  mutate_at(vars(starts_with('q')), ~ . * conv_factor)

### Cost per unit reduction

dat_unitcost <- all_qs %>%
  filter(grepl('enrg|gcc|land|watr', category), name %in%  c('cost_per_reduction'), is.na(group) | group == 'contracted foodservice operations') %>%
  left_join(conv_factors)

# Total annualized+annual cost for each food
pkg_totalcost_byfood <- pkg_result %>% filter(name %in% c('annualized_initial_cost', 'annualized_total_cost', 'material_cost')) %>%
  group_by(food, name) %>%
  slice(1) %>%
  ungroup %>%
  select(-category) %>%
  mutate_if(is.numeric, ~ ./1e6)

# Cost, averted food purchase, and net cost or savings for each food
pkg_netcost_byfood <- pkg_result %>% filter(name %in% c('annualized_total_cost', 'averted_food_purchase', 'net_cost')) %>%
  group_by(food, name) %>%
  slice(1) %>%
  ungroup %>%
  select(-category) %>%
  mutate_if(is.numeric, ~ ./1e6) %>%
  mutate_if(is_numeric, ~ if_else(name == 'averted_food_purchase', -., .))

# Averted impacts for each food
pkg_averted_byfood <- pkg_result %>% filter(name %in% 'net_averted', grepl('enrg|gcc|land|watr', category)) %>%
  left_join(conv_factors) %>%
  mutate_at(vars(starts_with('q')), ~ . * conv_factor)

# Cost per reduction for each food
pkg_cost_byfood <- pkg_result %>%
  filter(grepl('enrg|gcc|land|watr', category), name %in%  c('cost_per_reduction')) %>%
  left_join(conv_factors)

### Refed data

# Match intervention names in refed.
refed <- refed %>%
  mutate(intervention = gsub('&', 'and', Solution) %>% tolower)

# Compare GHG emissions averted. 
all_ghg <- all_qs %>%
  filter(grepl('gcc',category), !grepl('waste tracking', intervention) | group == 'contracted foodservice operations')

all_ghg_table <- all_ghg %>% 
  filter(name == 'net_averted') %>%
  left_join(refed) %>%
  select(intervention, GHGs_K_tons_year_, starts_with('q')) %>%
  rename(ReFED_estimate = GHGs_K_tons_year_) %>%
  mutate(ReFED_estimate = ReFED_estimate / 1000) %>%
  mutate_at(vars(starts_with('q')), ~ ./1e9)

# Compare water saved in the same way.
all_h2o <- all_qs %>%
  filter(grepl('watr',category), !grepl('waste tracking', intervention) | group == 'contracted foodservice operations')


# km^3 for our estimate, billion gallons for refed, must convert
all_h2o_table <- all_h2o %>%
  filter(name == 'net_averted') %>%
  left_join(refed) %>%
  select(intervention, Water_Conservation_B_gals_yr_, starts_with('q')) %>%
  rename(ReFED_estimate = Water_Conservation_B_gals_yr_) %>%
  mutate(ReFED_estimate = ReFED_estimate * 1e9 %>% set_units(gallon) %>% set_units(km^3)) %>%
  mutate_at(vars(starts_with('q')), ~ ./1e9)



# Compare total cost between Refed and our data

# Compare total cost
# for refed this is Cost_M_year_
dat_cost_vsrefed <- all_qs %>%
  filter(grepl('cost', name), is.na(category))

dat_totalcost_vsrefed <- dat_cost_vsrefed %>%
  filter(grepl('annual', name) | grepl('consumer', intervention), !grepl('net_cost', name), !grepl('content|media', name), name != 'annualized_initial_cost', is.na(group) | group == 'contracted foodservice operations') %>%
  group_by(intervention) %>%
  summarize_if(is.numeric, sum)

all_cost_table <- dat_totalcost_vsrefed %>% 
  left_join(refed) %>%
  select(intervention, Cost_M_year_, starts_with('q')) %>%
  rename(ReFED_estimate = Cost_M_year_) %>%
  mutate(ReFED_estimate = -ReFED_estimate) %>%
  mutate_at(vars(starts_with('q')), ~ . / 1e6)


# Compare cost effectiveness
# for refed, divide cost by the tons of ghg or water saved.
refed_costeff <- refed %>%
  select(intervention, Cost_M_year_, GHGs_K_tons_year_, Water_Conservation_B_gals_yr_) %>%
  setNames(c('intervention','annual_cost', 'ghg', 'water')) %>%
  mutate(water = water * 1e9 %>% set_units(gallon) %>% set_units(m^3),
         ghg = ghg %>% set_units(kilotonne) %>% set_units(kg),
         costeff_ghg = -1e-6 * ghg/annual_cost,
         costeff_water = -1e-6 * water/annual_cost) 

# GHG cost effectiveness
costeff_ghg_table <- all_qs %>%
  filter(grepl('gcc', category), name == 'cost_per_reduction') %>%
  select(intervention, group, starts_with('q')) %>%
  mutate_at(vars(starts_with('q')), ~ 1 / .) %>%
  left_join(refed_costeff %>% select(intervention, costeff_ghg)) %>%
  select(intervention, group, costeff_ghg, starts_with('q')) %>%
  rename(ReFED_estimate = costeff_ghg)

# Water cost effectiveness
costeff_water_table <- all_qs %>%
  filter(grepl('watr', category), name == 'cost_per_reduction') %>%
  select(intervention, group, starts_with('q')) %>%
  mutate_at(vars(starts_with('q')), ~ 1 / .) %>%
  left_join(refed_costeff %>% select(intervention, costeff_water)) %>%
  select(intervention, group, costeff_water, starts_with('q')) %>%
  rename(ReFED_estimate = costeff_water)


# Main figures ------------------------------------------------------------

# Color scheme set by RTI editor
rti_colors <- scale_color_manual(values = c('#8dc63f', '#d73642', '#6b489d', '#00aae7'), guide = guide_legend(nrow = 2))

theme_set(theme_bw() +
            theme(panel.grid.major.x = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  axis.text.x = element_blank(),
                  axis.title.x = element_blank(),
                  axis.ticks.x = element_blank(),
                  strip.background = element_blank(),
                  legend.position = 'none'))

p_totalimpact <- ggplot(dat_netaverted, aes(x = intervention, color = intervention, y = q50, ymin = q025, ymax = q975)) +
  facet_wrap(~ category_labels, scales = 'free', labeller = label_parsed) +
  geom_segment(aes(xend = intervention, y = q05, yend = q95), size = 2, alpha = 0.5) +
  geom_point(size = 2) +
  geom_blank(data = dummy_dat) +
  geom_errorbar(width = 0.05) +
  scale_x_discrete(labels = c('CEC', 'SPP', 'SDL', 'WTA')) +
  scale_y_continuous(name = 'Net impact averted', expand = expansion(mult = c(0, 0.05))) +
  theme(axis.text.x = element_text(size = 6), legend.position = 'none') +
  interv_colors 

p_costeff <- ggplot(dat_unitcost, aes(x = intervention, color = intervention, y = 1/q50, ymin = 1/q025, ymax = 1/q975)) +
  facet_wrap(~ costeff_labels, scales = 'free', labeller = label_parsed) +
  geom_segment(aes(xend = intervention, y = 1/q05, yend = 1/q95), size = 2, alpha = 0.5) +
  geom_point(size = 2) +
  geom_blank(data = dummy_dat, aes(y = 0, ymin = 0, ymax = 0, yend = 0)) +
  geom_errorbar(width = 0.05) +
  scale_x_discrete(labels = c('CEC', 'SPP', 'SDL', 'WTA')) +
  scale_y_continuous(name = 'Reduction per $1 spent',expand = expansion(mult = c(0, 0.05))) +
  theme(axis.text.x = element_text(size = 6), legend.position = 'none') +
  interv_colors 

theme_maintext <- theme_bw() +
  theme(axis.text.x = element_text(size = 9, color = 'black'),
        axis.text.y = element_text(size = 9, color = 'black'),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.background = element_blank(),
        legend.position = 'none',
        panel.grid = element_blank(),
         text = element_text(color = 'black'))

fig1 <- p_totalimpact + 
  geom_text(data = data.frame(category_labels = sort(unique(dat_netaverted$category_labels)),
                              letter = letters[1:4],
                              intervention = NA, q025 = 0, q50 = 0, q975 = 0),
            aes(label = letter),
            x = -Inf, y = Inf, hjust = -0.1, vjust = 1.1, color = 'black', fontface = 'bold', size = 6) +
  theme_maintext + rti_colors

fig2 <- p_costeff + 
  geom_text(data = data.frame(costeff_labels = sort(unique(dat_unitcost$costeff_labels)),
                              letter = letters[1:4],
                              intervention = NA, q025 = 0, q50 = 0, q975 = 0),
            aes(label = letter),
            x = -Inf, y = Inf, hjust = -0.1, vjust = 1.1, color = 'black', fontface = 'bold', size = 6) +
  theme_maintext + rti_colors

W <- 1656
H <- 1209
ggsave(file.path(fp_out, 'fig1.png'), fig1, height = H/300, width = W/300, dpi = 300)
ggsave(file.path(fp_out, 'fig2.png'), fig2, height = H/300, width = W/300, dpi = 300)

# Supplemental figures ----------------------------------------------------

dark_colors <- RColorBrewer::brewer.pal(8, 'Dark2')
food_colors <- scale_color_manual(values = dark_colors[c(4,7,6,2,5)])
food_colors <- scale_color_manual(values = c('#6b489d','#806213', '#bf931c','#f58021', '#8dc63f'))

theme_withxaxis <- theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.background = element_blank(),
        legend.position = 'bottom')

pos_dodge <- position_dodge(width = 0.2)

p_pkgnetcostbyfood <- ggplot(pkg_netcost_byfood %>% mutate(name = if_else(name == 'annualized_total_cost', 'total_cost', name)), aes(x = food, y = q50, color = name)) +
  geom_hline(yintercept = 0, linetype = 'dotted') +
  geom_errorbar(aes(ymin = q05, ymax = q95), size = 2, alpha = 0.5, width = 0, position = pos_dodge) +
  geom_point(size = 2, position = pos_dodge) +
  geom_errorbar(aes(ymin = q025, ymax = q975), width = 0.05, position = pos_dodge) +
  scale_y_continuous(name = 'Cost (million $)', expand = expansion(mult = 0.01)) +
  scale_color_manual(values = c('#d73642', '#00aae7', '#b5931c'),
                     labels = c('Averted food purchases', 'Net cost or savings', 'Cost of implementation'),
                     guide = guide_legend(nrow = 2)) +
  theme_withxaxis +
  theme(legend.position = 'bottom', legend.title = element_blank(),
        axis.text.x = element_text(color = 'black'),
        axis.text.y = element_text(color = c('forestgreen','forestgreen','black')),
        axis.title.x = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.x = element_blank())

p_pkgimpactbyfood <- ggplot(pkg_averted_byfood %>% filter(!food %in% 'misc'), aes(x = food, color = food, y = q50, ymin = q025, ymax = q975)) +
  facet_wrap(~ category_labels, scales = 'free_y', labeller = label_parsed) +
  geom_segment(aes(xend = food, y = q05, yend = q95), size = 2, alpha = 0.5) +
  geom_point(size = 2) +
  geom_errorbar(width = 0.05) +
  scale_y_continuous(name = 'Net impact averted', expand = expansion(mult = c(0, 0.05))) +
  theme_withxaxis +
  theme(axis.title.x = element_blank(), legend.position = 'none', axis.text.x = element_text(size = 7, color = 'black'),
        axis.text.y = element_text(color = 'black'),
        panel.grid = element_blank(),
        axis.ticks.x = element_blank()) +
  food_colors +
  geom_text(data = data.frame(category_labels = sort(unique(pkg_averted_byfood$category_labels)),
                              letter = letters[1:4],
                              food = NA, q025 = 0, q50 = 0, q975 = 0),
            aes(label = letter),
            x = -Inf, y = Inf, hjust = -0.1, vjust = 1.1, color = 'black', fontface = 'bold', size = 6)

p_pkgcosteffbyfood <- ggplot(pkg_cost_byfood %>% filter(!food %in% 'misc'), aes(x = food, color = food, y = 1/q50, ymin = 1/q025, ymax = 1/q975)) +
  facet_wrap(~ costeff_labels, scales = 'free_y', labeller = label_parsed) +
  geom_segment(aes(xend = food, y = 1/q05, yend = 1/q95), size = 2, alpha = 0.5) +
  geom_point(size = 2) +
  geom_errorbar(width = 0.05) +
  scale_y_continuous(name = 'Reduction per $1 spent', expand = expansion(mult = c(0, 0.05))) +
  theme_withxaxis +
  theme(axis.title.x = element_blank(), legend.position = 'none', axis.text.x = element_text(size = 7, color = 'black'),
        axis.text.y = element_text(color = 'black'),
        panel.grid = element_blank(),
        axis.ticks.x = element_blank()) +
  food_colors +
  geom_text(data = data.frame(costeff_labels = sort(unique(pkg_cost_byfood$costeff_labels)),
                                         letter = letters[1:4],
                                         food = NA, q025 = 0, q50 = 0, q975 = 0),
                       aes(label = letter),
                       x = -Inf, y = Inf, hjust = -0.1, vjust = 1.1, color = 'black', fontface = 'bold', size = 6)


# Refed supp figs ---------------------------------------------------------

theme_set(theme_bw() +
            theme(panel.grid = element_blank(),
                  legend.position = 'none',
                  axis.text.x = element_text(color = 'black'),
                  axis.text.y = element_text(color = 'black'),
                  axis.title.x = element_blank(),
                  axis.ticks.x = element_blank()))

p_totalcost <- ggplot(all_cost_table %>% mutate(intervention = str_wrap(intervention, width = 25)), aes(x = intervention, color = intervention)) +
  geom_point(aes(y = ReFED_estimate), pch = 1, size = 4, color = 'black', stroke = .8) +
  geom_segment(aes(xend = intervention, y = q05, yend = q95), size = 2, alpha = 0.5) +
  geom_point(aes(y = q50), size = 2) +
  geom_errorbar(aes(ymin = q025, ymax = q975), width = 0.05) +  scale_y_continuous(name = 'Total annual cost (million $)') +
  rti_colors

p_ghgaverted <- ggplot(all_ghg_table %>% mutate(intervention = str_wrap(intervention, width = 25)), aes(x = intervention, color = intervention)) +
  geom_point(aes(y = ReFED_estimate), pch = 1, size = 4, color = 'black', stroke = .8) +
  geom_segment(aes(xend = intervention, y = q05, yend = q95), size = 2, alpha = 0.5) +
  geom_point(aes(y = q50), size = 2) +
  geom_errorbar(aes(ymin = q025, ymax = q975), width = 0.05) +
  scale_y_continuous(name = parse(text = 'Potential~GHG~emissions~reduced~(MT~CO[2])')) +
  rti_colors

p_wateraverted <- ggplot(all_h2o_table %>% mutate(intervention = str_wrap(intervention, width = 25)), aes(x = intervention, color = intervention)) +
  geom_point(aes(y = as.numeric(ReFED_estimate)), pch = 1, size = 4, color = 'black', stroke = .8) +
  geom_segment(aes(xend = intervention, y = q05, yend = q95), size = 2, alpha = 0.5) +
  geom_point(aes(y = q50), size = 2) +
  geom_errorbar(aes(ymin = q025, ymax = q975), width = 0.05) +
  scale_y_continuous(name = parse(text = 'Potential~water~use~reduced~(km^3)')) +
  rti_colors

W2 <- 5.75
ggsave(file.path(fp_out, 'figS1.png'), p_pkgnetcostbyfood, height = 4, width = W2, dpi = 400)
ggsave(file.path(fp_out, 'figS2.png'), p_pkgimpactbyfood, height = 4, width = W2, dpi = 400)
ggsave(file.path(fp_out, 'figS3.png'), p_pkgcosteffbyfood, height = 4, width = W2, dpi = 400)
ggsave(file.path(fp_out, 'figS4.png'), p_totalcost, height = 4, width = W2, dpi = 400)
ggsave(file.path(fp_out, 'figS5.png'), p_ghgaverted, height = 4, width = W2, dpi = 400)
ggsave(file.path(fp_out, 'figS6.png'), p_wateraverted, height = 4, width = W2, dpi = 400)
