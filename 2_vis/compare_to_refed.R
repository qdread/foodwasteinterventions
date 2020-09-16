# Compare our results to ReFED's results for the same interventions
# QDR / FWE / 15 May 2020
# (updated from previous version, using new values with uncertainty)

# Edit 25 Jun 2020: create versions without restaurants (only contracted foodservice)

library(tidyverse)
library(readxl)
library(units)

fp_github <- '.'
fp_out <- '.'
fp_data <- file.path(fp_github, 'data')

refed <- read_xlsx(file.path(fp_data, 'ReFED-Data-Set.xlsx'), .name_repair = 'universal') %>%
  setNames(gsub('[\\.]+', '_', names(.)))

all_qs <- read_csv(file.path(fp_out, 'intervention_quantiles.csv'))

interv_colors <- scale_color_brewer(type = 'qual', palette = 'Set2', guide = guide_legend(nrow = 2))

# Define data frame with better formatted labels and conversion factors to join to the raw data
conv_factors <- data.frame(category = c("resource use/enrg/mj", "impact potential/gcc/kg co2 eq", "resource use/land/m2*yr", "resource use/watr/m3"),
                           category_labels = c('energy~(PJ)', 'greenhouse~gas~(MT~CO[2])', 'land~(Mha)', 'water~(km^3)'),
                           cost_labels = c('energy~("$"/MJ)', 'greenhouse~gas~("$"/kg~CO[2])', 'land~("$"/m^2)', 'water~("$"/m^3)'),
                           costeff_labels = c('energy~(MJ/"$")', 'greenhouse~gas~(kg~CO[2]/"$")', 'land~(m^2/"$")', 'water~(m^3/"$")'),
                           conv_factor = c(1e-9, 1e-9, 1e-10, 1e-9))

# Define dummy dataframe to stick in and make the axes always go down to zero
dummy_dat <- data.frame(q025 = 0, q50 = 0, q975 = 0, category_labels = c('energy~(PJ)', 'greenhouse~gas~(MT~CO[2])', 'land~(Mha)', 'water~(km^3)'), intervention = NA)

# Match intervention names in refed.
refed <- refed %>%
  mutate(intervention = gsub('&', 'and', Solution) %>% tolower)


# Compare impacts averted -------------------------------------------------


# Compare GHG emissions averted. Use only the total for waste tracking
# all_ghg <- all_qs %>%
#   filter(grepl('gcc',category), !grepl('waste tracking', intervention) | group == 'total')
all_ghg <- all_qs %>%
  filter(grepl('gcc',category), !grepl('waste tracking', intervention) | group == 'contracted foodservice operations')

all_ghg %>% 
  filter(name == 'net_averted') %>%
  left_join(refed) %>%
  ggplot(aes(x = GHGs_K_tons_year_/1000, y = q50)) +
    geom_point()

all_ghg_table <- all_ghg %>% 
  filter(name == 'net_averted') %>%
  left_join(refed) %>%
  select(intervention, GHGs_K_tons_year_, starts_with('q')) %>%
  rename(ReFED_estimate = GHGs_K_tons_year_) %>%
  mutate(ReFED_estimate = ReFED_estimate / 1000) %>%
  mutate_at(vars(starts_with('q')), ~ ./1e9)

# Compare water saved in the same way.
# all_h2o <- all_qs %>%
#   filter(grepl('watr',category), !grepl('waste tracking', intervention) | group == 'total')
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



# Compare costs -----------------------------------------------------------

# Compare total cost
# for refed this is Cost_M_year_
dat_cost <- all_qs %>%
  filter(grepl('cost', name), is.na(category))

dat_totalcost <- dat_cost %>%
  # filter(grepl('annual', name) | grepl('consumer', intervention), !grepl('content|media', name), name != 'annualized_initial_cost', is.na(group) | group == 'total') %>%
  filter(grepl('annual', name) | grepl('consumer', intervention), !grepl('net_cost', name), !grepl('content|media', name), name != 'annualized_initial_cost', is.na(group) | group == 'contracted foodservice operations') %>%
  group_by(intervention) %>%
  summarize_if(is.numeric, sum)

all_cost_table <- dat_totalcost %>% 
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


# Make plots --------------------------------------------------------------

theme_set(theme_bw() +
            theme(panel.grid.major.y = element_blank(),
                  panel.grid.minor = element_blank(),
                  legend.position = 'none'))

p_ghgaverted <- ggplot(all_ghg_table %>% mutate(intervention = str_wrap(intervention, width = 25)), aes(x = intervention, color = intervention)) +
  geom_point(aes(y = ReFED_estimate), pch = 1, alpha = 0.5, size = 4, color = 'black') +
  geom_segment(aes(xend = intervention, y = q05, yend = q95), size = 2, alpha = 0.5) +
  geom_point(aes(y = q50), size = 2) +
  geom_errorbar(aes(ymin = q025, ymax = q975), width = 0.05) +
  scale_y_continuous(name = parse(text = 'Potential~GHG~emissions~reduced~(MT~CO[2])')) +
  coord_flip() +
  interv_colors

p_wateraverted <- ggplot(all_h2o_table %>% mutate(intervention = str_wrap(intervention, width = 25)), aes(x = intervention, color = intervention)) +
  geom_point(aes(y = as.numeric(ReFED_estimate)), pch = 1, alpha = 0.5, size = 4, color = 'black') +
  geom_segment(aes(xend = intervention, y = q05, yend = q95), size = 2, alpha = 0.5) +
  geom_point(aes(y = q50), size = 2) +
  geom_errorbar(aes(ymin = q025, ymax = q975), width = 0.05) +
  scale_y_continuous(name = parse(text = 'Potential~water~use~reduced~(km^3)')) +
  coord_flip() +
  interv_colors

p_totalcost <- ggplot(all_cost_table %>% mutate(intervention = str_wrap(intervention, width = 25)), aes(x = intervention, color = intervention)) +
  geom_point(aes(y = ReFED_estimate), pch = 1, alpha = 0.5, size = 4, color = 'black') +
  geom_segment(aes(xend = intervention, y = q05, yend = q95), size = 2, alpha = 0.5) +
  geom_point(aes(y = q50), size = 2) +
  geom_errorbar(aes(ymin = q025, ymax = q975), width = 0.05) +  scale_y_continuous(name = 'Total annual cost (million $)') +
  coord_flip() +
  interv_colors

pd <- position_dodge(width = 0.2)

p_ghgcosteff <- ggplot(costeff_ghg_table %>% filter(!group %in% 'total') %>% mutate(intervention = str_wrap(intervention, width = 25)), aes(x = intervention, color = intervention, group = group, y = q50)) +
  geom_point(data = costeff_ghg_table %>% filter(is.na(group) | group == 'total') %>% mutate(intervention = str_wrap(intervention, width = 25)), aes(y = as.numeric(ReFED_estimate)), pch = 1, alpha = 0.5, size = 4, color = 'black') +
  geom_errorbar(aes(ymin = q05, ymax = q95), width = 0, size = 2, alpha = 0.5, position = pd) +
  geom_point(aes(y = q50), size = 2, position = pd) +
  geom_errorbar(aes(ymin = q025, ymax = q975), width = 0.05, position = pd) + 
  annotate(geom = 'text', x = c(4.15, 3.85), y = c(10, 16), label = c('restaurants', 'contractors'), size = 3) +
  scale_y_continuous(name = parse(text = 'GHG~cost-effectiveness~(kg~CO[2]/"$")')) +
  coord_flip() +
  interv_colors


p_watercosteff <- ggplot(costeff_water_table %>% filter(!group %in% 'total') %>% mutate(intervention = str_wrap(intervention, width = 25)), aes(x = intervention, color = intervention, group = group, y = q50)) +
  geom_point(data = costeff_water_table %>% filter(is.na(group) | group == 'total') %>% mutate(intervention = str_wrap(intervention, width = 25)), aes(y = as.numeric(ReFED_estimate)), pch = 1, alpha = 0.5, size = 4, color = 'black') +
  geom_errorbar(aes(ymin = q05, ymax = q95), width = 0, size = 2, alpha = 0.5, position = pd) +
  geom_point(aes(y = q50), size = 2, position = pd) +
  geom_errorbar(aes(ymin = q025, ymax = q975), width = 0.05, position = pd) + 
  annotate(geom = 'text', x = c(4.15, 3.85), y = c(5, 5), label = c('restaurants', 'contractors'), size = 3) +
  scale_y_continuous(name = parse(text = 'water~use~cost-effectiveness~(m^3/"$")')) +
  coord_flip() +
  interv_colors
