# Check the variability in environmental impacts by media type

media_codes <- c('541400', '541800', '511110', '511120', '515100', '519130')

media_codes_long <- all_codes$sector_desc_drc[match(media_codes, all_codes$sector_code_uppercase)]

consumer_ed_offset_eeio <- map(media_codes_long, ~ eeio_lcia('USEEIO2012', list(1), list(.))) # offset per dollar on each media industry

consumer_ed_offset_eeio <- map2_dfr(consumer_ed_offset_eeio, media_codes_long, ~ data.frame(BEA_code = .y,
                                                                                            category = row.names(.x),
                                                                                            impact = .x[,'Total']))

consumer_ed_offset_eeio %>%
  filter(grepl('enrg|gcc|land|watr', category)) %>%
  ggplot(aes(x = BEA_code, y = impact)) + 
  geom_point(size = 3) +
  coord_flip() +
  facet_wrap(~ category, scales = 'free_x')

####
# MacMonegle et al paper
# ratio of digital media costs to total media costs
digital <- 14.093
nondigital <- 183.354
digital / (digital+nondigital) # 7% of ad budget was for internet

# Table 1. All rows for 2014 except for media buying and digital media
total_nationwide_2014 <- 164227+12494574+468597+315284+724660 # 14 million.

# Core CPI derived from 
# https://fred.stlouisfed.org/series/CPILFESL
core_cpi <- read_csv('/nfs/qread-data/scenario_inputdata/core_cpi_bymonth.csv') %>%
  mutate(DATE = as.Date(DATE, format = '%m/%d/%Y'),
         year = lubridate::year(DATE)) %>%
  group_by(year) %>%
  summarize(CPI = mean(CPILFESL))

cpi_factor <- with(core_cpi, CPI[year == 2012]/CPI[year == 2014])

total_nationwide_2014 * cpi_factor * c(.8,1,1.2)

# Print media is around 50% still
# https://www.washingtonpost.com/technology/2019/02/20/digital-advertising-surpass-print-tv-first-time-report-says/