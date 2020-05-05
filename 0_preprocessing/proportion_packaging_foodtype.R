# Proportion of spoilage prevention packaging cost for each type of food.
# Updated 04 May 2020 with new values.

# New methodology:
# 1 Get 2012 economic census and 2012 census of agriculture revenue by 6 digit NAICS (use API to get econ census 2012 in another script)
# 2 Filter by 6 digit NAICS used in the reformulation cost model
# 3 For each product row in reformulation cost model, get proportion of its parent NAICS code
# 4 For each NAICS code, get proportion of its parent BEA code
# 5 Group each row also by the correct food type (fruit, vegetable, meat, and seafood)


# Load data ---------------------------------------------------------------


library(tidyverse)
library(readxl)
is_local <- dir.exists('Q:/')
fp <- ifelse(is_local, 'Q:', '/nfs/qread-data')

bea_naics <- read_csv(file.path(fp, 'crossreference_tables/BEA_NAICS07_NAICS12_crosswalk.csv'))
productdat <- read_csv(file.path(fp, 'scenario_inputdata/packaging_costs_byproduct.csv'))
foodmfg2012 <- read_csv(file.path(fp, 'scenario_inputdata/econcensus2012_naics31.csv'))
naics_index <- read_xls(file.path(fp, 'crossreference_tables/NAICS/2012_NAICS_Index_File.xls'))
naics_07to12 <- read_xlsx(file.path(fp, 'crossreference_tables/NAICS/2007_to_2012_NAICS.xlsx'), skip = 2)

# Join data tables and crosswalks -----------------------------------------

# Ensure that productdat is in 2012 NAICS codes
naics_update <- naics_07to12 %>% filter(`2007 NAICS Code` %in% productdat$NAICS)

update_idx <- match(naics_update$`2007 NAICS Code`, productdat$NAICS)
productdat$NAICS[update_idx] <- naics_update$`2012 NAICS Code`

# Join with a summary table saying how many items are included in each naics code
n_naics_items <- naics_index %>%
  group_by(NAICS12) %>%
  summarize(n_items = n())

productdat <- productdat %>%
  left_join(n_naics_items, by = c('NAICS' = 'NAICS12'))

# Join with the NAICS to BEA crosswalk
# Exclude all duplicated rows
bea_naics_2012only <- bea_naics %>%
  select(BEA_Code, BEA_Title, related_2012_NAICS_6digit) %>%
  filter(!duplicated(.))

productdat <- productdat %>%
  left_join(bea_naics_2012only, by = c('NAICS' = 'related_2012_NAICS_6digit'))

# Join the total receipts data from 2012 economic census with the NAICS to BEA crosswalk
foodmfg2012 <- foodmfg2012 %>%
  filter(!NAICS2012 %in% "31-33") %>%
  mutate(NAICS2012 = as.numeric(NAICS2012)) %>%
  left_join(bea_naics_2012only, by = c('NAICS2012' = 'related_2012_NAICS_6digit'))

# Determine what percentage each NAICS code is of the BEA code it belongs to.
foodmfg2012 <- foodmfg2012 %>%
  group_by(BEA_Code) %>%
  mutate(proportion = RCPTOT / sum(RCPTOT)) %>%
  ungroup

# Join product data with the proportion data
productdat <- productdat %>%
  left_join(foodmfg2012 %>% select(NAICS2012, RCPTOT, proportion), by = c('NAICS' = 'NAICS2012'))

productdat %>% 
  select(BEA_Code, BEA_Title, RCPTOT, proportion) %>% 
  filter(!duplicated(.)) %>%
  mutate(proportion = if_else(is.na(proportion), 1, proportion))


# Specify which proportions to use for each row ---------------------------

# For the misc food types, use the n. items x proportion * 2/3 (Mary assumed 2/3 of these items contained meat)
# For the fruits and vegetables, use 1
# For the meats, poultry, and seafood, use proportion

productdat <- productdat %>%
  mutate(proportion_final = case_when(
    NAICS %in% c(311421, 311911, 311991, 311999) ~ (2/3) * proportion/n_items,
    NAICS %in% c(111339, 111219) ~ 1,
    NAICS %in% c(311612, 311615, 311710) ~ proportion
  ))

# Check
productdat %>% select(product_subcategory, NAICS, BEA_Code, n_items, proportion, proportion_final) 

write_csv(productdat, '/nfs/qread-data/scenario_inputdata/packaging_costs_proportions_byproduct.csv')
