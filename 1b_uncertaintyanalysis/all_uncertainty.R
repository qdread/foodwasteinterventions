# All uncertainty analysis
# QDR / foodwasteinterventions

# Modified 5 May 2020: implement new approach to calculating packaging costs, also modularize script
# Modified 29 April 2020: parallelize with furrr

n_iter <- 1000 # Number of MC iterations in uncertainty analysis

# Load data for all interventions -----------------------------------------

is_local <- dir.exists('Q:/')
fp <- ifelse(is_local, 'Q:', '/nfs/qread-data')
fp_github <- file.path(ifelse(is_local, '~/Documents/GitHub/foodwaste', '~'))
fp_out <- file.path(fp, 'scenario_results/intervention_uncertainty')

source(file.path(fp_github, 'foodwasteinterventions/1b_uncertaintyanalysis/all_uncertainty_setup.R'))

# Standardized date labeling ---------------------------------------

datelabel_costs <- read_xlsx(file.path(fp, 'scenario_inputdata/intervention_costs_26mar2020/costs for date labeling change_3-26-2020.xlsx'), skip = 9)

datelabel_costs_coord <- datelabel_costs[which(datelabel_costs[,1] == "Total costs for date labeling (with coordination)"), 2:4] %>% t %>% c %>% setNames(c('lower','mean','upper'))

# Set up PERT distributions for each parameter and draw from them.
datelabel_pars <- intervention_params %>%
  filter(Intervention %in% c('all', 'standardized date labeling'), !is.na(Parameter))
# Replace initial costs with the exact numbers (using PERT bounds)
datelabel_cost_pert_pars <- get_pert_bounds(p = c(0.05, 0.95), q = datelabel_costs_coord[c(1,3)], mode = datelabel_costs_coord[2])
datelabel_pars[datelabel_pars$Parameter == 'initial_cost', c('minimum','mode','maximum')] <- as.list(datelabel_cost_pert_pars)

set.seed(111)
datelabel_par_draws <- map_dfr(1:n_iter, function(i) {
  vals <- with(datelabel_pars, rpert(nrow(datelabel_pars), min = minimum, mode = mode, max = maximum, shape = 4))
  vals %>% setNames(datelabel_pars$Parameter) %>% t %>% as.data.frame
})

# Do the actual model fitting 1000 times
datelabel_results <- future_pmap(datelabel_par_draws, standardized_date_labeling)

save(datelabel_results, file = file.path(fp_out, 'datelabel_uncertainty.RData'))

# Spoilage prevention packaging -------------------------------------------

packaging_costs_proportions <- read_csv(file.path(fp, 'scenario_inputdata/packaging_costs_proportions_byproduct.csv'))

##########
# LAFA rate conversion for the fruit and meat codes in LAFA.
# These are probably more than needed.
fruit_meat <- bea_codes %>% filter(stage %in% c('agriculture','processing'),
                                   fruit_veg_fresh > 0 | fruit_veg_processed > 0 | meat > 0 | fish_fresh > 0)

# We need baseline retail loss and consumer loss for fruits and meats
lafa <- list(veg, fruit, meat)

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
  filter(subgroup2 == 'Fresh fruit' | subgroup1 %in% c('Fresh vegetables', 'Red meat', 'Poultry', 'Total Fresh and Frozen Fish')) %>%
  mutate(group_final = if_else(Group == 'fruit', subgroup2, subgroup1)) %>%
  select(group_final, Food:avoidable_consumer_loss)

fruitmeat_wtdavg_rates <- lafa_df %>%
  group_by(group_final) %>%
  summarize(retail_loss = weighted.mean(retail_loss, retail_weight),
            avoidable_consumer_loss = weighted.mean(avoidable_consumer_loss, consumer_weight))

##### offsetting impacts for the plastic packaging materials code
# do this as per $1 spent on each category so it can be multiplied by the different costs.

plastic_packaging_code <- "326110/plastic bags, films, and sheets/us"
eeio_packaging_offsetting_impacts <- eeio_lcia('USEEIO2012', list(1), list(plastic_packaging_code))
eeio_packaging_offsetting_impacts <- data.frame(category = row.names(eeio_packaging_offsetting_impacts),
                                                impact = eeio_packaging_offsetting_impacts$Total)


# Sum up packaging costs and proportions by the appropriate categories
packaging_proportion_by_BEA <- packaging_costs_proportions %>%
  group_by(BEA_Code) %>%
  summarize(proportion = min(1, sum(proportion_final)))

# Convert percentiles to PERT parameters for packaging costs for each food item
packaging_costs_by_food <- packaging_costs_proportions %>%
  group_by(food) %>%
  summarize_at(vars(Units:`95th Percentile`), sum) %>%
  mutate(unit_cost_q05 = `5th Percentile`/Units, unit_cost_mean = Mean/Units, unit_cost_q95 = `95th Percentile`/Units)

packaging_pert_pars <- pmap_dfr(packaging_costs_by_food, function(unit_cost_q05, unit_cost_mean, unit_cost_q95, ...) get_pert_bounds(p = c(0.05, 0.95), q = c(unit_cost_q05, unit_cost_q95), mode = unit_cost_mean))

# The initial costs per unit for each food type.
packaging_costs_by_food <- cbind(packaging_costs_by_food, packaging_pert_pars)

#### run eeio for $1 per food type so it can be multiplied later by the costs within each iteration
packaging_food_eeio_codes <- with(all_codes, sector_desc_drc[match(packaging_proportion_by_BEA$BEA_Code, sector_code_uppercase)])

eeio_packaging_averted <- map(packaging_food_eeio_codes, ~ eeio_lcia('USEEIO2012', list(1), list(.)))

eeio_packaging_averted <- map2_dfr(packaging_proportion_by_BEA$BEA_Code, eeio_packaging_averted, 
                                   ~ data.frame(BEA_Code = .x, category = row.names(.y), impact = .y$Total))

# Set up PERT distributions for each parameter and draw from them.
packaging_pars <- intervention_params %>%
  filter(Intervention %in% c('all', 'spoilage prevention packaging'), !is.na(Parameter), !Parameter %in% 'baseline_beverage_rate')

set.seed(222)
packaging_par_draws <- map_dfr(1:n_iter, function(i) {
  vals <- with(packaging_pars, rpert(nrow(packaging_pars), min = minimum, mode = mode, max = maximum, shape = 4))
  vals %>% setNames(packaging_pars$Parameter) %>% t %>% as.data.frame
})

packaging_par_draws_byfood <- map_dfr(1:n_iter, function(i) {
  vals <- with(packaging_costs_by_food, rpert(nrow(packaging_costs_by_food), min = min, mode = mode, max = max, shape = 4))
  vals %>% setNames(paste0('unitcost_', packaging_costs_by_food$food)) %>% t %>% as.data.frame
})

# Do the actual model fitting 1000 times
packaging_results <- future_pmap(cbind(packaging_par_draws, packaging_par_draws_byfood), spoilage_prevention_packaging)

save(packaging_results, file = file.path(fp_out, 'packaging_uncertainty.RData'))

# Consumer education campaigns --------------------------------------------

# Proportion of baseline demand affected is equal to the proportion of population who lives in the metropolitan areas that are big enough to get a targeted campaign
# Cost is the annual cost per campaign multiplied by the number of metropolitan areas that will have campaigns
# There are 1251 counties within metropolitan statistical areas, together representing 85.6% of the US pop
pop_in_metro <- 0.856 # see read_msas.R for derivation of this number.
n_metro_counties <- 1251

# Find offsetting impacts per $1 for each media type that can be applied within each iteration.
# For content development, use specialized design services 541400
# For media consulting, use advertising/PR services 541800
# For media, use a combination of magazine, radio/TV, and internet publishing (no newspaper)

media_codes <- c('541400', '541800', '511120', '515100', '519130')

media_codes_long <- all_codes$sector_desc_drc[match(media_codes, all_codes$sector_code_uppercase)]

consumer_ed_offset_eeio <- map(media_codes_long, ~ eeio_lcia('USEEIO2012', list(1), list(.))) # offset per dollar on each media industry

consumer_ed_offset_eeio <- map2_dfr(consumer_ed_offset_eeio, media_codes_long, ~ data.frame(BEA_code = .y,
                                                                                            category = row.names(.x),
                                                                                            impact = .x[,'Total']))


# Set up PERT distributions for each parameter and draw from them.
consumered_pars <- intervention_params %>%
  filter(Intervention %in% c('all', 'consumer education campaigns'), !is.na(Parameter), !is.na(minimum), !Parameter %in% c('annuity_years', 'annuity_rate'))

set.seed(333)
consumered_par_draws <- map_dfr(1:n_iter, function(i) {
  vals <- with(consumered_pars, rpert(nrow(consumered_pars), min = minimum, mode = mode, max = maximum, shape = 4))
  vals %>% setNames(consumered_pars$Parameter) %>% t %>% as.data.frame
})

# Do the actual model fitting 1000 times
consumered_results <- future_pmap(consumered_par_draws, consumer_education_campaigns)

save(consumered_results, file = file.path(fp_out, 'consumered_uncertainty.RData'))

# Waste tracking and analytics --------------------------------------------


lafa <- list(dairy, fat, fruit, grain, meat, sugar, veg)

# Subsets of industries to target

codes_subset <- bea_codes %>% filter(stage %in% c('foodservice', 'institutional')) %>% select(BEA_389_code, BEA_389_def)

restaurants <- codes_subset$BEA_389_code[1:3]
tourism_hospitality <- codes_subset$BEA_389_code[c(4,6,9, 12, 13, 14, 15, 16, 17, 18)] # Include promoters/agents (arena operators) Include air and ships. Exclude movies and performances.
institutions <- codes_subset$BEA_389_code[c(19, 20, 22:27)] # Leave out other educational services.

# 3 codes represent restaurants, 10 represent tourism/hospitality industry, 8 represent institutions that could adopt "WTA"

bea_to_use_df <- data.frame(BEA_Code = c(restaurants, tourism_hospitality, institutions),
                            sector = rep(c('restaurants', 'tourism and hospitality', 'institutions'), c(length(restaurants), length(tourism_hospitality), length(institutions))))

bea_naics_to_use <- bea_to_use_df %>% left_join(bea_naics)
# any(duplicated(bea_naics_to_use$related_2012_NAICS_6digit)) # There are no duplicates. 83 unique NAICS codes.

# Create subset with only food service.
susb_naics_foodservice <- bea_naics_to_use %>%
  select(BEA_Code, BEA_Title, sector, related_2012_NAICS_6digit) %>%
  rename(NAICS = related_2012_NAICS_6digit) %>%
  left_join(susb_naics)

# Flag rows that aren't going to be used (for example, freight transportation industries within transportation codes)
# Also remove campgrounds within the hospitality industry.
# unique(susb_naics_foodservice$`NAICS description`)
words_to_remove <- c('Freight', 'Nonscheduled', 'Air Traffic', 'Support Activities', 'Port', 'Cargo', 'Navigational', 'Towing', 'Packing', 'Campground')

susb_naics_foodservice <- susb_naics_foodservice %>%
  mutate(use = !grepl(paste(words_to_remove, collapse = '|'), `NAICS description`),
         `Size class` = factor(`Size class`, levels = c('fewer than 20', '20 to 99', '100 to 499', 'more than 500', 'total')))

# New way of getting institutions. Use primary food service operations, and contractors/caterers.

restaurant_naics <- c(722511, 722513, 722514, 722515, 722330, 722410)
contractor_naics <- c(722310, 722320)

susb_naics_foodservice <- susb_naics_foodservice %>%
  mutate(category = case_when(NAICS %in% restaurant_naics ~ 'restaurant',
                              NAICS %in% contractor_naics ~ 'contractor',
                              TRUE ~ 'other')) 

# Sum up establishments by size class in the two large categories
# Assume all contractors/caterers can adopt, but only restaurants with >= 20 employees can adopt.

susb_food_sums <- susb_naics_foodservice %>%
  group_by(category, `Size class`) %>%
  summarize_at(vars(`No. firms`:`Total receipts`), sum) %>%
  filter(!is.na(`Size class`), !`Size class` %in% 'total')


# What are the proportions

susb_food_cumul_prop <- susb_food_sums %>%
  mutate_at(vars(`No. firms`:`Total receipts`), ~ cumsum(.x) / sum(.x))

####
# We need two different sets of totals. 
# 1. The total number of establishments that implement leanpath (for costs). 
# --- This equals the number of restaurants >=20 employees
# --- Plus the number of foodservice contractors (other food and drinking estbs with bars and food trucks removed)
# 2. The proportion of food purchases in industries that contract foodservice operators, that are affected by WTA implementation. 
# --- We will assume that this is the % of purchases made by size class >= 20 employees.
# --- Also, we assume that inputs are proportional to receipts.
# --- So this would exclude the small operations for all industries EXCEPT those listed as contractors

# 1. establishments that can implement
establishments_implementing <- susb_naics_foodservice %>%
  filter(!category %in% 'other') %>%
  group_by(category, BEA_Code, NAICS, `NAICS description`, `Size class`) %>%
  summarize_at(vars(`No. firms`:`Total receipts`), sum) %>%
  select(-`No. employees`, -`Total payroll`) %>% 
  filter(!`Size class` %in% 'total') %>%
  mutate(can_implement = category == 'contractor' | category == 'restaurant' & `Size class` != 'fewer than 20')

# establishments_implementing %>% print(n = nrow(.))

establishments_implementing_total <- establishments_implementing %>%
  group_by(category, BEA_Code, can_implement) %>%
  summarize(n_estab = sum(`No. establishments`))


# Within each BEA code, get the percentage of total receipts that will be affected by WTA implementation
# All establishments with >20 employees, and also account for the fact that some tourism and hospitality BEA codes contain NAICS codes that aren't going to adopt WTA

susb_bea_food_sums <- susb_naics_foodservice %>%
  mutate(sector = if_else(category == 'contractor', 'contractors', as.character(sector))) %>%
  group_by(sector, BEA_Code, BEA_Title, use, `Size class`) %>%
  summarize_at(vars(`No. firms`:`Total receipts`), sum) %>%
  filter(!is.na(`Size class`), !`Size class` %in% 'total')

# Impute missing values

recbyestb <- susb_bea_food_sums %>%
  filter(use) %>%
  mutate(empl_per_firm = `No. employees`/`No. firms`,
         receipts_per_estb = `Total receipts`/`No. establishments`,
         receipts_per_firm = `Total receipts`/`No. firms`) 

# Impute the air transit number.
lm_air <- lm(log(receipts_per_firm) ~ log(empl_per_firm), data = recbyestb, subset = BEA_Title == 'Air transportation' & receipts_per_firm > 0)

predicted_air <- exp(predict(lm_air, newdata = recbyestb %>% filter(BEA_Title == 'Air transportation') %>% select(empl_per_firm)))
# predicted_air[4] # The imputed value for air transportation total receipts for firms with 500 or more employees.

# For water transportation, our problem is that we don't know either the average number of employees per firm or the receipts, the employees were
# also clearly censored wince only 202 employees for 11 firms is a lot less than 500 per firm. 
# Let's look for a similar industry to see whether we can get a number of employees per firm to use to impute.
lm_water <- lm(log(receipts_per_firm) ~ log(empl_per_firm), data = recbyestb, subset = BEA_Title == 'Water transportation' & receipts_per_firm > 0)

# We will use the number from scenic transportation (700) which seems fairly conservative
# This number can be sampled from in an uncertainty analysis too.
predicted_water <- exp(predict(lm_water, newdata = recbyestb %>% filter(BEA_Title == 'Water transportation') %>% mutate(empl_per_firm = if_else(`Size class` == 'more than 500', 700, empl_per_firm)) %>% select(empl_per_firm)))

# Create new imputed dataset.
susb_bea_food_sums[susb_bea_food_sums$BEA_Code %in% c('481000', '483000') & susb_bea_food_sums$use & susb_bea_food_sums$`Size class` %in% 'more than 500', "Total receipts"] <- c(predicted_air[4], predicted_water[4])

# Get final proportions affected 

# The second type of sum:
# 2. sum up the non-excluded number of receipts in each industry so that food bought by very small firms isn't eligible for waste reduction
#### Here are the proportions of receipts that will be affected by adoption of WTA

sums_by_affected <- susb_bea_food_sums %>%
  ungroup %>%
  mutate(use = `Size class` != 'fewer than 20' | sector == 'contractors') %>%
  group_by(sector, BEA_Code, BEA_Title, use) %>%
  summarize_at(vars(`No. firms`:`Total receipts`), sum)

# Sum up again so that the other food and drinking places is included.
sums_by_affected_final <- susb_bea_food_sums %>%
  ungroup %>%
  mutate(use = `Size class` != 'fewer than 20' | sector == 'contractors') %>%
  group_by(BEA_Code, BEA_Title, use) %>%
  summarize_at(vars(`No. firms`:`Total receipts`), sum)

susb_bea_proportion_affected <- sums_by_affected_final %>%
  select(-(`No. firms`:`Total payroll`)) %>%
  pivot_wider(names_from = use, values_from = `Total receipts`, values_fill = list(`Total receipts` = 0), names_prefix = 'receipts_') %>%
  mutate(proportion = receipts_TRUE / (receipts_TRUE + receipts_FALSE))

# Calculation of waste rates.

# Get rid of unneeded rows and columns
food_U <- food_U[, susb_bea_proportion_affected$BEA_Code]
food_U <- food_U[rowSums(food_U) > 0, ]

# Beverage codes should be removed.
beveragecodes <- c('311920','311930','312110','312120','312130','312140')
food_U <- food_U[!row.names(food_U) %in% beveragecodes, ]


# Map BEA to QFAHPD 

# Convert the comma-separated string columns to list columns.
bea2qfahpd <- bea2qfahpd %>%
  mutate(QFAHPD_code = strsplit(QFAHPD_code, ';'))
qfahpd2lafa <- qfahpd2lafa %>%
  mutate(LAFA_names = strsplit(LAFA_names, ';'))

# Do the mapping of food_U to QFAHPD codes.
food_U_QFAHPD <- food_U %>%
  mutate(BEA_389_code = row.names(food_U)) %>%
  pivot_longer(-BEA_389_code, names_to = 'BEA_recipient_code', values_to = 'monetary_flow') %>%
  left_join(bea2qfahpd %>% select(-BEA_389_def)) %>%
  group_by(BEA_389_code, BEA_recipient_code) %>%
  group_modify(~ data.frame(QFAHPD_code = .$QFAHPD_code[[1]], monetary_flow = .$monetary_flow/length(.$QFAHPD_code[[1]])))

# Now we have the use table where each BEA code has multiple rows for the different QFAHPD codes that make it up
# Create an aggregated version of QFAHPD to get the final price values for each code
# Weighted average across all market groups, years, and quarters
qfahpd_agg <- qfahpd2 %>%
  group_by(foodgroup) %>%
  summarize(price = weighted.mean(price, aggweight, na.rm = TRUE))

# Join the aggregated QFAHPD back up with its numerical codes and LAFA names
# Meanwhile correct a couple wrong names in the dairy category
qfahpd_agg <- qfahpd_agg %>% 
  mutate(foodgroup = gsub('Whole and 2%', 'Regular fat', foodgroup)) %>%
  left_join(qfahpd2lafa, by = c('foodgroup' = 'QFAHPD_name')) %>%
  mutate(QFAHPD_code = as.character(QFAHPD_code))

# Now join the aggregated QFAHPD with the food_U mapped to QFAHPD so that the total $ can be divided by $/weight to yield a weight (or mass).
# The units mass is in don't matter since they are all relative
food_U_LAFA <- food_U_QFAHPD %>%
  left_join(qfahpd_agg) %>%
  mutate(mass_flow = monetary_flow / price)


# Get LAFA rates including lower-level averages 

# For the unique LAFA names in the QFAHPD to LAFA mapping, extract the waste rates for 2012 or the closest year post-2012.
lafa_to_extract <- Reduce(union, qfahpd2lafa$LAFA_names)

# Split it up again by LAFA so that we can get the weights.
# Get only the columns we care about from each LAFA element in the list
# Then get the year closest to 2012
lafa_df <- lafa %>% 
  map_dfr(~ select(., Category, Year, Loss_at_consumer_level_Other__cooking_loss_and_uneaten_food__Percent, Consumer_weight_Lbs.year)) %>%
  rename(avoidable_consumer_loss = Loss_at_consumer_level_Other__cooking_loss_and_uneaten_food__Percent,
         consumer_weight = Consumer_weight_Lbs.year) %>%
  filter(!is.na(avoidable_consumer_loss)) %>%
  group_by(Category) %>%
  summarize(avoidable_consumer_loss = avoidable_consumer_loss[which.min(abs(Year-2012))],
            consumer_weight = consumer_weight[which.min(abs(Year-2012))])

# Use nested category structure to get weighted mean rates for the coarser LAFA groups
# for QFAHPD foods that do not resolve to the finest available level of LAFA
lafa_df <- lafa_df %>%
  left_join(lafa_struct, by = c('Category' = 'Food'))

lafa_group_rates <- map_dfr(1:4, function(i) lafa_df %>% 
                              rename_(subgroup = paste0('subgroup', i)) %>%
                              group_by(subgroup) %>% 
                              summarize(avoidable_consumer_loss = weighted.mean(x = avoidable_consumer_loss, w = consumer_weight))) %>%
  filter(!is.na(subgroup))

# Use LAFA overall mean for prepared food in the "other" category
overall_mean <- with(lafa_df, weighted.mean(avoidable_consumer_loss, consumer_weight))

all_lafa_rates <- data.frame(Category = c(lafa_df$Category, lafa_group_rates$subgroup, 'prepared food'),
                             avoidable_consumer_loss = c(lafa_df$avoidable_consumer_loss, lafa_group_rates$avoidable_consumer_loss, overall_mean))


# Map mass flows to LAFA, convert back to $ 

food_U_LAFA_spread <- food_U_LAFA %>%
  group_by(BEA_389_code, BEA_recipient_code, QFAHPD_code, price) %>%
  group_modify(~ data.frame(LAFA_name = .$LAFA_names[[1]], 
                            mass_flow = .$mass_flow/length(.$LAFA_names[[1]]),
                            monetary_flow = .$monetary_flow/length(.$LAFA_names[[1]]))) %>%
  left_join(all_lafa_rates, by = c('LAFA_name' = 'Category'))



# Offsetting impacts
# The three potential BEA industry codes to assign costs of equipment are:
# computer manufacturing, computer monitor and peripheral manufacturing, and the other machinery category which includes industrial and retail scales
industries_offset <- c('334111', '33411A', '33399A')

# Find the full names of the codes for the offsetting industries
industries_offset_codes <- all_codes$sector_desc_drc[match(industries_offset, all_codes$sector_code_uppercase)]

# Run EEIO for the industries
eeio_offsets_wta <- map(industries_offset_codes, ~ eeio_lcia('USEEIO2012', list(1), list(.)))

eeio_offsets_wta <- data.frame(category = row.names(eeio_offsets_wta[[1]]),
                               computers = eeio_offsets_wta[[1]]$Total,
                               peripherals = eeio_offsets_wta[[2]]$Total,
                               scales = eeio_offsets_wta[[3]]$Total)

# Set up PERT distributions for each parameter and draw from them.
wta_pars <- intervention_params %>%
  filter(Intervention %in% c('all', 'waste tracking and analytics'), !is.na(Parameter), !Parameter %in% 'baseline_beverage_rate')

# Convert wages from 10th and 90th percentiles to the min and max of PERT distribution
wta_wages_pert_pars <- get_pert_bounds(p = c(0.1, 0.9), q = as.numeric(wta_pars[wta_pars$Parameter == 'annual_cost_wages', c('minimum','maximum')]), mode = as.numeric(wta_pars[wta_pars$Parameter == 'annual_cost_wages', c('mode')]))
wta_pars[wta_pars$Parameter == 'annual_cost_wages', c('minimum','mode','maximum')] <- as.list(wta_wages_pert_pars)

set.seed(444)
wta_par_draws <- map_dfr(1:n_iter, function(i) {
  vals <- with(wta_pars, rpert(nrow(wta_pars), min = minimum, mode = mode, max = maximum, shape = 4))
  vals %>% setNames(wta_pars$Parameter) %>% t %>% as.data.frame
})

# Do the actual model fitting 1000 times
wta_results <- future_pmap(wta_par_draws, waste_tracking_analytics)

save(wta_results, file = file.path(fp_out, 'wta_uncertainty.RData'))
