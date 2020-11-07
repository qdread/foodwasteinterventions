# Load data from Kraak et al. Table 4 and convert all to 2012 dollars with PPI for advertising agencies obtained from BLS
# Source of BLS data: https://download.bls.gov/pub/time.series/pc/

# read PPI data from BLS

library(tidyverse)
library(readxl)

# read file with a mixture of tab and space delimiters
ppi_all <- read.delim(file.path(fp_rawdata, "pc.data.63.ProfessionalandTechnicalServ.txt"), sep = '\t', strip.white = TRUE)

# get only advertising PPI and average by year
ppi_advertising <- ppi_all %>% 
  filter(grepl('541810', series_id)) %>%
  group_by(series_id, year) %>%
  summarize(n = n(), PPI = mean(value))

ppi_advertising_agencies <- ppi_advertising %>%
  filter(series_id == 'PCU541810541810') 

ppi2012 <- ppi_advertising_agencies$PPI[ppi_advertising_agencies$year == 2012]

ppi_advertising_agencies <- ppi_advertising_agencies %>%
  mutate(PPI_factor = ppi2012/PPI)

# read Kraak et al data. Convert to 2012 dollars
kraaktable4 <- read_xlsx(file.path(fp_rawdata, 'campaign_costs_kraak_table4.xlsx')) %>%
  rename_with(function(x) gsub(' ', '_', tolower(x))) %>%
  left_join(ppi_advertising_agencies, by = c('middle_year' = 'year')) %>%
  mutate(annual_cost_2012_dollars = annual_cost * PPI_factor)


mean(kraaktable4$annual_cost_2012_dollars[-7])# 11.6 million a year

# Exclude the got milk campaign as an outlier. That is a more intensive campaign than what we really want to be looking at.
# We can say it's between 3 and 29 million, with a mean of 12 million.
# This is roughly in line with Refed's ballpark figure of 10 million.