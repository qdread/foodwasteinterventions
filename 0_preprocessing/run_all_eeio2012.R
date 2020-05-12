# Run all EEIO for food demand and offsets, so that we can save the results and apply them later.
# QDR / FWI / 12 May 2020

library(tidyverse)
library(reticulate)

is_local <- dir.exists('Q:/')
fp <- ifelse(is_local, 'Q:', '/nfs/qread-data')
fp_github <- file.path(ifelse(is_local, '~/Documents/GitHub/foodwaste', '~'))
fp_out <- file.path(fp, 'scenario_results/intervention_uncertainty')

# Source EEIO python function
if (!is_local) use_python('/usr/bin/python3')
source_python(file.path(fp_github, 'fwe/USEEIO/eeio_lcia.py'))

# Demand codes table to convert 6 digit codes to the ones used by USEEIO
all_codes <- read_csv(file.path(fp, 'crossreference_tables/all_codes.csv'))

# Run EEIO 388 times, one for $1 demand in each industry.
eeio_all <- map(all_codes$sector_desc_drc, ~ eeio_lcia("USEEIO2012", list(1), list(.)))

# Place results into a long form data frame
eeio_df <- map2_dfr(eeio_all, all_codes$sector_desc_drc, ~ tibble(sector_desc_drc = .y, category = row.names(.x), impact = .x$Total))

write_csv(eeio_df, file.path(fp_out, 'eeio_all_industries.csv'))
