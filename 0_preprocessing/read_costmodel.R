# Script to extract all information from FDA reformulation cost model spreadsheet
# QDR / fwi / 5 May 2020

is_local <- dir.exists('Q:/')
fp <- ifelse(is_local, 'Q:', '/nfs/qread-data')

filename <- file.path(fp, 'scenario_inputdata/intervention_costs_04may2020/reformulation cost model run_05-04-2020.xlsx')

# Read in as tidyxl

library(tidyverse)
library(tidyxl)
library(unpivotr)
library(zoo)

costmodel_cells <- xlsx_cells(filename)

costmodel_cells_processed <- costmodel_cells %>%
  select(sheet, address, row, col, data_type, numeric, character) %>%
  filter(!sheet %in% c('Inputs', 'Data')) %>%
  group_by(sheet) %>%
  nest() %>%
  mutate(data = map(data, ~ behead(., direction = 'N', name = 'company_size')))

# Sheet 1 has 5 group columns before data
# Sheet 2 has only 1 group column brefore data
# Sheet 3 has 5 group columns and has an additional header row

costmodel_byproduct <- costmodel_cells_processed$data[[1]] %>%
  behead('W', product_category) %>%
  behead('W', product_subcategory) %>%
  behead('W', NAICS) %>%
  behead('W', NAICS_description) %>%
  behead('W', brand_type) %>%
  behead('N', statistic) %>%
  filter(!grepl('Total', product_category), !grepl('Total', product_subcategory)) %>%
  mutate_at(vars(product_category, product_subcategory, NAICS, NAICS_description, company_size), na.locf) %>%
  select(-address, -row, -col, -data_type, -character) %>%
  pivot_wider(names_from = statistic, values_from = numeric)

costmodel_bycosttype <- costmodel_cells_processed$data[[2]] %>%
  behead('W', reformulation_activity) %>%
  behead('N', statistic) %>%
  mutate(company_size = na.locf(company_size)) %>%
  filter(!is.na(reformulation_activity)) %>%
  select(-address, -row, -col, -data_type, -character) %>%
  pivot_wider(names_from = statistic, values_from = numeric)

costmodel_allcosts <- costmodel_cells_processed$data[[3]] %>%
  behead('W', product_category) %>%
  behead('W', product_subcategory) %>%
  behead('W', NAICS) %>%
  behead('W', NAICS_description) %>%
  behead('W', reformulation_activity) %>%
  behead('N', cost_type) %>%
  behead('N', statistic) %>%
  filter(!is.na(reformulation_activity)) %>%
  mutate_at(vars(product_category, product_subcategory, NAICS, NAICS_description, cost_type, company_size), na.locf) %>%
  select(-address, -row, -col, -data_type, -character) %>%
  pivot_wider(names_from = statistic, values_from = numeric)

write_csv(costmodel_byproduct, file.path(fp, 'scenario_inputdata/intervention_costs_04may2020', 'costmodel_byproduct.csv'))
write_csv(costmodel_bycosttype, file.path(fp, 'scenario_inputdata/intervention_costs_04may2020', 'costmodel_bycosttype.csv'))
write_csv(costmodel_allcosts, file.path(fp, 'scenario_inputdata/intervention_costs_04may2020', 'costmodel_allcosts.csv'))


# Summary stats by product ------------------------------------------------

costmodel_byproduct_summary <- costmodel_byproduct %>%
  group_by(company_size, product_category, product_subcategory, NAICS, NAICS_description) %>%
  summarize_if(is.numeric, sum) %>%
  filter(company_size %in% 'Total') %>%
  ungroup %>%
  mutate(food = c('misc', 'misc', 'misc', 'misc', 'fruit', 'fruit', 'vegetables', 'vegetables', 'vegetables', 'vegetables', 'meat', 'poultry', 'seafood', 'vegetables'))

write_csv(costmodel_byproduct_summary, file.path(fp, 'scenario_inputdata/packaging_costs_byproduct.csv'))

# 311421, 311911: subsets of NAICS code, representing misc dips and such
# 311991, 311999: refrigerated meals and sandwiches, subset of NAICS code
# 111339: fruit - use the refed proportions.
# 111219: veggies: use the refed proportions
# 