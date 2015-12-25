### Question 5
#
# How have emissions from motor vehicle sources
# changed from 1999–2008 in Baltimore City?

# Fetch the data
source('get_data.R')

library("ggplot2")

# Read in the source data
emissions_data <- readRDS("data/summarySCC_PM25.rds")
source_codes   <- readRDS("data/Source_Classification_Code.rds")

# To determine what is meant by 'motor vehicle sources' we examine the source-codes
# data (and documentation, found here: http://www3.epa.gov/ttn/chief/net/2008neiv3/2008_neiv3_tsd_draft.pdf)
# For our purposes, we'll use the "On-road – all Diesel and Gasoline vehicles" information
# (see page 112 of the linked report, section 4.6).
# This consists of 4 EI.Sector codes, which are prefixed with "Mobile - On-Road"

mv_sources <- source_codes[grepl('^Mobile - On-Road', as.character(source_codes$EI.Sector)), ]

# This gives us 1138 motor-vehicle source codes (across 4 EI.Sector's)
# We're interested in the change of particulate output across these codes for
# only Baltimore City, so we'll need to filter the main list to the correct codes,
# then aggregate by year.
baltimore_mv_source_emissions <- emissions_data[emissions_data$fips == '24510' &
                                                emissions_data$SCC %in% mv_sources$SCC, ]

baltimore_mv_source_emissions_by_year <- aggregate(baltimore_mv_source_emissions$Emissions,
                                                   by = list(year = baltimore_mv_source_emissions$year),
                                                   sum)

# Convert years to factors for nicer x-axis labels
baltimore_mv_source_emissions_by_year$year <- as.factor(baltimore_mv_source_emissions_by_year$year)

# We want to see the change over time, but there are only four points,
# so a bar plot may be easier to look at than a line.
bar_chart <- ggplot(baltimore_mv_source_emissions_by_year, aes(x = year, y = x)) +
  geom_bar(stat = 'identity') +
  ggtitle('PM2.5 From Motor-Vehicle Sources in Baltimore City, 1999-2008') +
  xlab('Year') +
  ylab('Tons of PM2.5') +

  # We'll produce a horizontal line to help identify the relationship of
  # subsequent years to the first observed year.
  geom_hline(aes(yintercept = x),
             data = baltimore_mv_source_emissions_by_year[baltimore_mv_source_emissions_by_year$year ==
                                                          min(as.character(baltimore_mv_source_emissions_by_year$year)), ],
             linetype = 'dashed')
print(bar_chart)

# Use the png graphic device to generate the image file
dev.copy(png, file = "plot5.png", width = 480, height = 480)
dev.off()

### RESULT
# How have emissions from motor vehicle sources
# changed from 1999–2008 in Baltimore City?
#
# They have decreased substantially since 1999.
#