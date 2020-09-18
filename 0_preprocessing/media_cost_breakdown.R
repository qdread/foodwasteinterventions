####
# Use manually entered values from MacMonegle et al paper to derive estimates for how media cost is divided among media types.
# ratio of digital media costs to total media costs
digital <- 14.093
nondigital <- 183.354
digital / (digital+nondigital) # 7% of ad budget was for internet


# Use manually entered values from MacMonegle et al., Table 1, to derive estimate for total cost of campaign.

# Table 1. All rows for 2014 except for media buying and digital media
total_nationwide_2014 <- 164227+12494574+468597+315284+724660 # 14 million.

ppi_factor <- 0.96 # Factor to convert 2012 to 2014 by PPI

total_nationwide_2014 * ppi_factor * c(.8,1,1.2)

# Print media is around 50% still
# https://www.washingtonpost.com/technology/2019/02/20/digital-advertising-surpass-print-tv-first-time-report-says/