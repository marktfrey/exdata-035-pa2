### Question 6
#
# Compare emissions from motor vehicle sources in Baltimore City
# with emissions from motor vehicle sources in Los Angeles County,
# California (fips == "06037"). Which city has seen greater changes
# over time in motor vehicle emissions?

# Fetch the data
source('get_data.R')

library(ggplot2)
library(gridExtra)

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
# only Baltimore City and Los Angeles County, so we'll need to filter the main list to the correct
# codes, then aggregate by year.

mv_source_emissions <- emissions_data[emissions_data$fips %in% c('24510', '06037') &
                                      emissions_data$SCC %in% mv_sources$SCC, ]

# Add the text for the location, instead of just the fips number.
mv_source_emissions$location <- ifelse(mv_source_emissions$fips == '24510',
                                       'Baltimore City, MD', 'Los Angeles County, CA')
mv_source_emissions$location <- as.factor(mv_source_emissions$location)

# Create the aggregates
mv_source_emissions_by_year <- aggregate(mv_source_emissions$Emissions,
                                         by = list(year = mv_source_emissions$year,
                                                   location = mv_source_emissions$location),
                                         sum)

mv_source_emissions_by_year$year_factor  <- as.factor(mv_source_emissions_by_year$year)

# We want to see the change over time, and this time there are multiple factors
# Additionally, the numbers for LA County are MUCH LARGER than those for Baltimore City.

total_bars <- ggplot(mv_source_emissions_by_year,
                     aes(x = year_factor, y = x, color = location, fill = location)
                    ) +
                    geom_bar(stat = 'identity') +
                    guides(color=FALSE, fill=FALSE) +
                    ggtitle('PM2.5 per year\nfrom Motor-Vehicle Sources') +
                    xlab('Year') +
                    ylab('Tons of PM2.5') +
                    facet_wrap(~ location, ncol = 1) +
                    geom_hline(aes(yintercept = x, color = location),
                               data = mv_source_emissions_by_year[mv_source_emissions_by_year$year == min(mv_source_emissions_by_year$year), ],
                               linetype = 'dashed') +
                    theme(plot.margin = unit(c(0.25, 0.25, 3.5, 0.25), "line"))

# As a line chart it is difficult to see the relative magnitude, and the change we
# are being asked to investigate.  The bars give us a pretty good sense of magnitude
# and some sense of the change, but we could do one better by looking at the deltas.

mv_source_emissions_by_year$delta_2009 <- apply(mv_source_emissions_by_year, 1, function(row){
  v1999 <- mv_source_emissions_by_year[mv_source_emissions_by_year$location == row["location"] &
                                       mv_source_emissions_by_year$year == min(mv_source_emissions_by_year$year),
                                       c('x')]
  as.numeric(row["x"]) - v1999
})

# Figure out a fixed magnitude so we can use a scale that shows change from zero
# with zero centered in the middle
yrange_mag <- round(max(abs(mv_source_emissions_by_year$delta_2009))/100) * 100

delta_bars <- ggplot(mv_source_emissions_by_year,
                      aes(x = year_factor, y = delta_2009, group = location, fill = location)
                    ) +
                    geom_bar(stat = 'identity') +
                    ggtitle('Change in Motor-Vehicle\nPM2.5 Emissions') +
                    xlab('Year') +
                    ylab('Tons of PM2.5') +
                    coord_cartesian(ylim = range(-yrange_mag, yrange_mag)) +
                    geom_hline(aes(yintercept = 0, color = location),
                               linetype = 'dashed') +
                    facet_wrap(~ location, ncol = 1) +
                    theme(legend.position="bottom")

# Here we're calculating the percentage change (per measurement year) from the 1999 value
# for each source
mv_source_emissions_by_year$delta_2009_pct <- apply(mv_source_emissions_by_year, 1, function(row){
    v1999 <- mv_source_emissions_by_year[mv_source_emissions_by_year$location == row["location"] &
                                         mv_source_emissions_by_year$year == min(mv_source_emissions_by_year$year),
                                         c('x')]
    ((as.numeric(row["x"]) - v1999)/v1999) * 100
  })

delta_lines <- ggplot(mv_source_emissions_by_year,
                      aes(x = year_factor,
                          y = delta_2009_pct,
                          group = location,
                          color = location)
                     ) +
                     geom_line() +
                     ggtitle('Change in Motor-Vehicle\nPM2.5 Emissions') +
                     xlab('Year') +
                     ylab('Percent change from 1999 value') +
                     coord_cartesian(ylim = c(-75, 75)) +
                     guides(color=FALSE) +
                     facet_wrap(~ location, ncol = 1) +
                     theme(plot.margin = unit(c(0.25, 0.25, 3.5, 0.25), "line"))

grid.arrange(total_bars, delta_bars, delta_lines, ncol=3)

# Use the png graphic device to generate the image file
dev.copy(png, file = "plot6.png", width = 960, height = 480)
dev.off()

### RESULT
# Compare emissions from motor vehicle sources in Baltimore City
# with emissions from motor vehicle sources in Los Angeles County,
# California (fips == "06037"). Which city has seen greater changes
# over time in motor vehicle emissions?
#
# We can see from the plots that thought the total motor vehicle emissions
# are much, much higher in LA County, the actual volume of change is similar
# to that in Baltimore -- but in the opposite direction (in 2008, LA produced about
# 200 more tons than they did in 1999, while Baltimore produced about 200 less).
# As a percentage of their previous emissions levels, Baltimore's reduction
# over the period is far greater than the increase seen in LA.
#