# Get revenue by NAICS code for economic census 2012, using US Census Bureau API


library(httr)
library(jsonlite)

# For code 31 (food manufacturing)

api_call <- paste0('https://api.census.gov/data/2012/ewks?get=NAICS2012,NAICS2012_TTL,RCPTOT,OPTAX&for=us:*&key=',
                   readLines('~/censusapikey.txt'))



econ2012_raw <- GET(api_call) %>%
  content(as = 'text') 

foodmfg2012 <- fromJSON(econ2012_raw)
dimnames(foodmfg2012)[[2]] <- foodmfg2012[1,]
foodmfg2012 <- as.data.frame(foodmfg2012[-1,], stringsAsFactors = FALSE)
foodmfg2012$RCPTOT <- as.numeric(foodmfg2012$RCPTOT)

foodmfg2012 <- subset(foodmfg2012, substr(NAICS2012,1,2) == '31')

write.csv(foodmfg2012, '/nfs/qread-data/scenario_inputdata/econcensus2012_naics31.csv', row.names = FALSE)


# For code 11 (farming), use the 2012 census data query tool we already have
# cdqt_naics2012 <- read_csv('/nfs/qread-data/cfs_io_analysis/NASS2012_receipts_workers_NAICS_imputed.csv')
