# Process US MSA data
# QDR / FWE / 20 Feb 2020

# Replace readLines('~/censusapikey.txt') with your own API key.

library(tidyverse)
library(readxl)

msas <- read_xls(file.path(fp_rawdata, 'raw/metroareas_sep2018.xls'), skip = 2) %>%
  filter(!is.na(`FIPS State Code`))

table(msas$`Metropolitan/Micropolitan Statistical Area`)

msas %>% 
  filter(`Metropolitan/Micropolitan Statistical Area` == 'Metropolitan Statistical Area') %>%
  pull(`CBSA Title`) %>%
  unique

# There are 392 metropolitan statistical areas in the US, across 1251 counties.

# look at Seattle

msas %>% filter(grepl('Seattle', `CSA Title`))


# US population by county  ------------------------------------------------

# Query API manually (DATE_CODE 5 is 2012 estimate)
api_call <- paste0('https://api.census.gov/data/2018/pep/population?get=POP&DATE_CODE=5&for=county:*&key=',
                   readLines('~/censusapikey.txt'))

library(httr)
library(jsonlite)

countypop2012 <- GET(api_call) %>%
  content(as = 'text') %>%
  fromJSON()

dimnames(countypop2012)[[2]] <- countypop2012[1,]
countypop2012 <- as_tibble(apply(countypop2012[-1,], 2, as.numeric))

# Join the counties with the MSAs they are in, based on FIPS codes

county_msas <- countypop2012 %>%
  mutate(state = sprintf('%02d', state),
         county = sprintf('%03d', county)) %>%
         left_join(msas, by = c('state' = 'FIPS State Code', 'county' = 'FIPS County Code')) %>%
  mutate(metro_area = if_else(is.na(`Metropolitan/Micropolitan Statistical Area`),
                              'non-metro',
                              `Metropolitan/Micropolitan Statistical Area`))

# Total populations in and out of MSAs
county_msas %>%
  group_by(metro_area) %>%
  summarize(population = sum(POP)) %>%
  mutate(proportion = population/sum(population))
# 85.6% of the population lives in a metropolitan statistical area, 8.6% in micro, 5.8% in neither.

sum(county_msas$POP)
