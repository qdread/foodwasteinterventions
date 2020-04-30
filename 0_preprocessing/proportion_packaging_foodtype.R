# Proportion of spoilage prevention packaging cost for each type of food.

library(readxl)
library(tidyverse)

productdat <- read_xlsx('/nfs/qread-data/scenario_inputdata/intervention_costs_26mar2020/reformulation cost model run_03-26-2020.xlsx', sheet = 'Affected - Product Category', skip = 1)
# Keep only the column with the units

n_units <- productdat %>%
  filter(is.na(`Brand Type`)) %>%
  select(`Product Subcategory`, Units...26) 

n_units_bygroup <- data.frame(food = c('fresh fruit','fresh vegetables','meat','poultry'),
                              n = unlist(c(sum(n_units[1:2,2]), sum(n_units[3:7,2]),n_units[9:10,2]))) %>%
  mutate(p = n/as.numeric(n_units[12,2]))

write_csv(n_units_bygroup, '/nfs/qread-data/scenario_inputdata/proportion_packaging_byfoodtype.csv')
