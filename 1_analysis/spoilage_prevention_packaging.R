# Spoilage prevention packaging analysis
# Split into a separate script by QDR, 23 April 2020

# Assume same % of fruit and vegetables are packaged. 1/3 packaging for both fruit and veg, and 1/2 for meat and poultry.

# For packaging, we are going to use one-time costs, then per-unit costs. 
# Mary has already done the cost totals. 
# The annualized initial cost is relatively small; most of it is the annual cost. We will represent 100% of the annual cost being materials.

# Load packages and check whether this is being run locally or on rstudio server.
library(tidyverse)
library(reticulate)

is_local <- dir.exists('Q:/')
fp <- ifelse(is_local, 'Q:', '/nfs/qread-data')
fp_github <- file.path(ifelse(is_local, '~/Documents/GitHub/foodwaste', '~'))

# We now use ReFED's assumptions that this can be done for 15% of fruit and 25% of meat, with 10-33% retail waste reduction and 5-10% residential waste reduction.

packaging_costs <- read_xlsx(file.path(fp, 'scenario_inputdata/intervention_costs_26mar2020/costs for intelligent packaging_3-26-2020.xlsx'), skip = 10, col_names = c('cost_type_2012_dollars', 'low', 'mean', 'high', 'notes')) 

packaging_annual_equipment_costs <- packaging_costs[packaging_costs$cost_type_2012_dollars %in% 'Total annual costs', 2:4]
packaging_total_costs <- packaging_costs[packaging_costs$cost_type_2012_dollars %in% 'Total annualized and annual costs', 2:4]
packaging_initial_costs <- packaging_costs[packaging_costs$cost_type_2012_dollars %in% 'Annualized initial costs (5 years, 7%)', 2:4]

# We also use the values from the cost model for material costs to get the one-time materials costs, and annualize them too.
# Cost of materials for packaging assessment are lower $86, median $100, upper $114, per formula.
# This is fairly negligible since there are only 18416 formulas so the cost is around 1.8 million, but it can be accounted for.

# There is retail and household loss reduction. Load table of rates.
packaging_reduction_rates <- read_csv(file.path(fp, 'scenario_inputdata/reduction_rate_packaging.csv'))

# Load use table to find the purchases of fruit and meat by retailers.

# BEA levels 1+3 to 4+6+7+8 is already subsetted from an older analysis I did.
food_U <- read.csv(file.path(fp, 'crossreference_tables/level13_to_level4678_inputs.csv'), row.names = 1, check.names = FALSE)

# rows will be fruit and meat rows, columns will be retail.

# Load the BEA code data (erroneously called NAICS) to get the codes
bea_codes <- read_csv(file.path(fp, 'crossreference_tables/naics_crosswalk_final.csv'))

retail_codes <- bea_codes$BEA_389_code[bea_codes$stage %in% 'retail']

# Get the rows for fruit and meat
fruit_rows <- grep("fruit", bea_codes$BEA_389_def, ignore.case = TRUE)
# The fresh fruit code is 100% fruit, and the processed fruit code is 74.2% fruit and vegetables, but not all of that is fruit. Need to go back and look.

U <- read.csv(file.path(fp, 'raw_data/BEA/formatted/use2012.csv'), stringsAsFactors = FALSE, check.names = FALSE, row.names = 1)

prop_fruit_veg <- U[c('111200', '111300'), c('311420')]
prop_processed_fruit <- prop_fruit_veg[2]/sum(prop_fruit_veg) # 85.16% by $ value of the processed fruit/veg industry is fruit. (This isn't needed anymore)

food_U[fruit_rows, retail_codes] # This is basically zero. We will need to just change the final consumer demand by the product of the waste rates, and use consumer price.

##########
# LAFA rate conversion for the fruit and meat codes in LAFA.
# These are probably more than needed.
fruit_meat <- bea_codes %>% filter(stage %in% c('agriculture','processing'),
                                  fruit_veg_fresh > 0 | fruit_veg_processed > 0 | meat > 0 | fish_fresh > 0)

# We need baseline retail loss and consumer loss for fruits and meats

# Load LAFA
source(file.path(fp_github, 'fwe/read_data/read_lafa.r'))
lafa <- list(veg, fruit, meat)

# Read the description of LAFA's nested category structure in.
lafa_struct <- read_csv(file.path(fp, 'crossreference_tables/lafa_category_structure.csv'))

# find the single year closest to 2012.
lafa_df <- lafa %>%
  map2_dfr(c('veg', 'fruit','meat'), ~ select(.x, Category, Year, Retail_weight_Lbs.year, Loss_from_retail__institutional_to_consumer_level_Percent, Consumer_weight_Lbs.year,
                   Loss_at_consumer_level_Other__cooking_loss_and_uneaten_food__Percent) %>%
             mutate(Group = .y)) %>%
  setNames(c('Food','Year','retail_weight','retail_loss','consumer_weight','avoidable_consumer_loss','Group')) %>%
  filter(!is.na(avoidable_consumer_loss)) %>%
  group_by(Group, Food) %>%
  filter(abs(Year - 2012) == min(abs(Year - 2012)))

lafa_df <- lafa_df %>% 
  ungroup %>%
  left_join(lafa_struct) %>%
  filter(subgroup2 == 'Fresh fruit' | subgroup1 %in% c('Fresh vegetables', 'Red meat', 'Poultry')) %>%
  mutate(group_final = if_else(Group == 'fruit', subgroup2, subgroup1)) %>%
  select(group_final, Food:avoidable_consumer_loss)

fruitmeat_wtdavg_rates <- lafa_df %>%
  group_by(group_final) %>%
  summarize(retail_loss = weighted.mean(retail_loss, retail_weight),
            avoidable_consumer_loss = weighted.mean(avoidable_consumer_loss, consumer_weight))

# We now have the baseline retail and consumer waste rate for 4 groups: fresh fruit, poultry, seafood, and red meat.

### Load the baseline consumer demand for fresh fruit, red meat, and poultry in 2012.
# Do not include processed and frozen products, only fresh.
fruit_meat_codes <- c('111200', '111300', '311615', '31161A')
fruit_meat_proportions <- rep(1, 4)

finaldemand2012 <- read_csv(file.path(fp_github, 'USEEIO/useeiopy/Model Builds/USEEIO2012/USEEIO2012_FinalDemand.csv'))

fruitmeatdemand2012 <- data.frame(BEA_389_code = fruit_meat_codes,
                                  proportion = fruit_meat_proportions) %>%
  left_join(finaldemand2012)

# We need to deduct the non-fish proportion of the seafood demand categories.

# Post intervention demand under different scenarios
demand_change_fn <- function(W0, r, p) p * ((1 - W0) / (1 - (1 - r) * W0) - 1) + 1

packaging_reduction_rates_wide <- packaging_reduction_rates %>%
  pivot_wider(id_cols = food, names_from = level, values_from = -c(food,level))

# Compute baseline demand (deducting non-fish proportion of the seafood codes)
# and reduction rates of retail and household demand (lower and upper bounds)
# from this get the averted demand after the intervention for each food
fruitmeatdemand2012 <- fruitmeatdemand2012 %>%
  mutate(food = c('fruit','fruit','meat','meat'), group_final = c('Fresh vegetables', 'Fresh fruit', 'Poultry', 'Red meat')) %>%
  left_join(fruitmeat_wtdavg_rates) %>%
  left_join(packaging_reduction_rates_wide) %>%
  mutate(baseline_demand = `2012_US_Consumption` * proportion,
         retail_reduction_lower = demand_change_fn(W0 = retail_loss/100, r = waste_reduction_lower_retail, p = proportion_affected_retail),
         retail_reduction_upper = demand_change_fn(W0 = retail_loss/100, r = waste_reduction_upper_retail, p = proportion_affected_retail),
         household_reduction_lower = demand_change_fn(W0 = avoidable_consumer_loss/100, r = waste_reduction_lower_household, p = proportion_affected_household),
         household_reduction_upper = demand_change_fn(W0 = avoidable_consumer_loss/100, r = waste_reduction_upper_household, p = proportion_affected_household),
         demand_reduction_lower = retail_reduction_lower * household_reduction_lower,
         demand_reduction_upper = retail_reduction_upper * household_reduction_upper,
         demand_averted_lower = baseline_demand * (1 - demand_reduction_lower),
         demand_averted_upper = baseline_demand * (1 - demand_reduction_upper))

c(sum(fruitmeatdemand2012$demand_averted_lower), sum(fruitmeatdemand2012$demand_averted_upper))/1e9 # 1.34 to 3.12 billion dollars.

# Get full codes that will be recognized by USEEIO.
fruitmeatdemand2012 <- all_codes %>%
  select(sector_desc_drc, sector_code_uppercase) %>%
  right_join(fruitmeatdemand2012, by = c('sector_code_uppercase' = 'BEA_389_code'))

### 
# Run EEIO for the baseline, demand averted lower, and demand averted upper
# Do separately for each food so that we can see the result for each one.

if (!is_local) use_python('/usr/bin/python3')
source_python(file.path(fp_github, 'fwe/USEEIO/eeio_lcia.py'))

# 4 categories x (1 baseline+1averted lower+1averted upper) = 12 runs of model
# Already in units of dollars so don't need to multiply by any factor.
eeio_packaging_averted <- pmap(fruitmeatdemand2012, function(sector_desc_drc, baseline_demand, demand_averted_lower, demand_averted_upper, ...) {
  demand_code <- as.list(sector_desc_drc)
  list(baseline = eeio_lcia('USEEIO2012', as.list(baseline_demand), demand_code),
       averted_lower = eeio_lcia('USEEIO2012', as.list(demand_averted_lower), demand_code),
       averted_upper = eeio_lcia('USEEIO2012', as.list(demand_averted_upper), demand_code))
})

# Put this into a data frame
eeio_packaging_averted <- map2_dfr(c('fresh vegetables', 'fresh fruit', 'poultry', 'meat'), eeio_packaging_averted, function(x, y) {
  impacts <- do.call(cbind, y) %>% setNames(names(y))
  data.frame(food = x, category = row.names(impacts), impacts)
})

conversion_factors <- c(1e-9, 1e-6, 1e-9, 1e-10, 1e-9)
category_names <- c('energy (PJ)', 'eutrophication (kT N)', 'greenhouse gas (MT CO2)', 'land (Mha)', 'water (km3)')

# Make a preliminary plot to show how much of the impact is averted
errorbarplot_data <- eeio_packaging_averted %>%
  filter(grepl('enrg|eutr|gcc|land|watr', category)) %>%
  mutate_if(is.numeric, ~ . * conversion_factors) %>%
  mutate(category = rep(category_names, times = nrow(.)/5),
         food = factor(food, levels = c('meat', 'poultry', 'fresh vegetables', 'fresh fruit'))) 


pkg_errorbarplot <- ggplot(errorbarplot_data, aes(x = food, ymin = averted_lower, ymax = averted_upper)) +
  geom_errorbar(size = 1, width = 0.15) +
  facet_wrap(~ category, scales = 'free') + 
  coord_flip() +
  theme_bw() +
  theme(panel.grid = element_blank(), strip.background = element_blank()) +
  ggtitle('Environmental impacts averted by spoilage-prevention packaging',
          'uncertainty range due to ReFED\'s estimate of waste reduction rate')

ggsave(file.path(fp, 'figures/intervention_analysis/packaging_impact_averted.pdf'), pkg_errorbarplot, height = 6, width = 9)


##### offsetting impacts for the possible industry codes for packaging materials and equipment
# for now, do this as per $1 spent on each category so it can be multiplied by the different costs.

intervention_industry_BEA <- read_csv(file.path(fp, 'scenario_inputdata/intervention_industry_BEA.csv'))

# Get packaging industries and join with full length codes
packaging_industries <- intervention_industry_BEA %>%
  filter(industry_type %in% 'packaging materials' | BEA_Title %in% 'Packaging machinery manufacturing') %>%
  left_join(all_codes %>% select(sector_desc_drc, sector_code_uppercase), by = c('BEA_Code' = 'sector_code_uppercase'))

eeio_packaging_offsetting_impacts <- packaging_industries %>%
  group_by(BEA_Code, BEA_Title, industry_type) %>%
  group_modify(function(x, ...) {
    eeio <- eeio_lcia('USEEIO2012', list(1), list(x$sector_desc_drc))
    data.frame(category = row.names(eeio), impact = eeio[,'Total'])
  })

packaging_offset_perdollar_plot <- eeio_packaging_offsetting_impacts %>%
  ungroup %>%
  filter(grepl('enrg|gcc|land', category)) %>%
  mutate(category = rep(c('Energy (MJ)', 'GHG (kg CO2 eq.)', 'Land (m2)'), times = nrow(.)/3),
         BEA_Title = trunc_ellipsis(BEA_Title, 50)) %>%
  mutate(BEA_Title = factor(BEA_Title, levels = unique(BEA_Title))) %>%
  ggplot(aes(x = BEA_Title, y = impact)) +
    facet_grid(. ~ category, scales = 'free_x') +
    geom_col() +
    coord_flip() +
    scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
    theme_bw() +
    theme(panel.grid = element_blank(), strip.background = element_blank()) +
    ggtitle('Packaging environmental impact per $1 final demand',
            'for packaging machinery and many possible types of packaging materials')

ggsave(file.path(fp, 'figures/intervention_analysis/packaging_impact_offset_perdollar.pdf'), packaging_offset_perdollar_plot, height = 8.5, width = 11)

#### cost estimates
# use the annualized initial cost to get the initial impacts, using packaging machinery as the industry
# use the annual costs to get the annual impacts, using plastic packaging materials as the industry.

packaging_annual_offset <- eeio_packaging_offsetting_impacts %>%
  filter(grepl('Plastics packaging', BEA_Title)) %>%
  mutate(offset_lower = impact * packaging_annual_equipment_costs$low,
         offset_mean = impact * packaging_annual_equipment_costs$mean,
         offset_upper = impact * packaging_annual_equipment_costs$high)

# for the annualized initial cost, we use $86, $100, $114 per formula times the number of formulas.
n_formulas <- 18416
materials_cost_per_formula <- c(low = 86, mean = 100, high = 114)
pmt(p = n_formulas * materials_cost_per_formula, r = 0.07, n = 5, t = 0, f = 0)
# This is basically negligible so I will ignore it.

offsetplot_data <- packaging_annual_offset %>%
  ungroup %>%
  filter(grepl('enrg|eutr|gcc|land|watr', category)) %>%
  mutate_if(is.numeric, ~ . * conversion_factors) %>%
  mutate(category = rep(category_names, times = nrow(.)/5))

impactandoffset_plot_data <- errorbarplot_data %>%
  group_by(category) %>%
  summarize_if(is.numeric, sum) %>%
  left_join(offsetplot_data %>% select(category, contains('offset'))) %>%
  mutate(net_lower = averted_lower - offset_upper,
         net_upper = averted_upper - offset_lower)

impactandoffset_plot_data_long <- impactandoffset_plot_data %>%
  select(-baseline) %>%
  pivot_longer(-category) %>%
  separate(name, into = c('impact', 'bound')) %>%
  pivot_wider(names_from = bound, values_from = value) %>%
  mutate(impact = factor(impact, levels = c('averted', 'offset', 'net')))

impactandoffset_plot <- ggplot(impactandoffset_plot_data_long, aes(x = impact, ymin = lower, ymax = upper, color = impact)) +
  facet_wrap(~ category, scales = 'free_y') +
  geom_errorbar(size = 1, width = 0.1) +
  scale_y_continuous(name = "impact", expand = expand_scale(mult = c(0, 0.1))) +
  expand_limits(y = 0) +
  theme_bw() +
  theme(panel.grid = element_blank(), strip.background = element_blank(), axis.title.x = element_blank(), legend.position = 'none') +
  ggtitle("Net environmental impact averted by packaging intervention",
          "offset by impacts of producing additional plastic packaging material")

ggsave(file.path(fp, 'figures/intervention_analysis/packaging_net_impact_averted_total.pdf'), impactandoffset_plot, height = 6, width = 9)

#### cost per unit impact reduction.
# Total annualized and annual costs divided by total net impact averted


cost_per_reduction_packaging <- eeio_packaging_averted %>%
  group_by(category) %>%
  summarize_if(is.numeric, sum) %>%
  left_join(packaging_annual_offset %>% ungroup %>% select(category, contains('offset'))) %>%
  mutate(net_lower = averted_upper - offset_lower,
         net_upper = averted_lower - offset_upper) %>%
  group_by(category) %>%
  group_modify(~ as.data.frame(outer(as.numeric(packaging_total_costs), as.numeric(.[,-(1)]), `/`))) %>%
  ungroup %>%
  setNames(c("category",      "averted_lower", "averted_upper", "offset_lower",  "offset_mean",   "offset_upper", "net_lower", "net_upper")) %>%
  mutate(bound = rep(c('lower','mean','upper'), times = nrow(.)/3)) %>%
  select(category, bound, net_lower, net_upper) %>%
  group_by(category) %>%
  summarize(net_lower = min(net_lower), net_upper = max(net_upper))

cost_per_reduction_packaging %>% 
  filter(grepl('enrg|eutr|gcc|land|watr', category)) %>%
  mutate(category = c('eutrophication ($/kg N)', 'greenhouse gas ($/kg CO2)', 'energy ($/MJ)', 'land ($/m2)', 'water ($/m3)'))
# Net result: 5 to 36 cents per kg CO2 averted. Compares favorably to WTA. Scale could be interesting too.
         
# Save results

write_csv(eeio_packaging_averted, file.path(fp, 'scenario_results/interventions/eeio_packaging_byfoodtype_all.csv'))
write_csv(errorbarplot_data, file.path(fp, 'scenario_results/interventions/eeio_packaging_byfoodtype_5categories_processed.csv'))
write_csv(impactandoffset_plot_data, file.path(fp, 'scenario_results/interventions/eeio_packaging_byfoodtype_5categories_withoffset.csv'))
write_csv(cost_per_reduction_packaging, file.path(fp, 'scenario_results/interventions/eeio_packaging_costperreduction_all.csv'))

# Put results from packaging into a "standardized" form that is similar to the other ones.

eeio_packaging_averted_total <- eeio_packaging_averted %>%
  group_by(category) %>%
  summarize_if(is.numeric, sum)

# This includes some irrelevant categories of packaging material
# eeio_packaging_offsetting_impacts

eeio_packaging_result <- eeio_packaging_averted_total %>%
  left_join(packaging_annual_offset %>% ungroup %>% select(category, contains('offset'))) %>%
  mutate(net_averted_lower = averted_lower - offset_upper,
         net_averted_mean = (averted_lower + averted_upper) / 2 - offset_mean,
         net_averted_upper = averted_upper - offset_lower)

# Add costs
eeio_packaging_result <- eeio_packaging_result %>%
  cbind(packaging_total_costs %>% setNames(paste0('total_cost_', names(.)))) %>%
  mutate(cost_per_reduction_lower = total_cost_low / net_averted_upper,
         cost_per_reduction_mean = total_cost_mean / net_averted_mean,
         cost_per_reduction_upper = total_cost_high / net_averted_lower)

write_csv(eeio_packaging_result, file.path(fp, 'scenario_results/interventions/eeio_packaging_all.csv'))

