### Question 3
#
# Of the four types of sources indicated by the type
# (point, nonpoint, onroad, nonroad) variable, which of these four sources
# have seen decreases in emissions from 1999–2008 for Baltimore City?
# Which have seen increases in emissions from 1999–2008?
# Use the ggplot2 plotting system to make a plot answer this question.

# Fetch the data
source('get_data.R')

library(ggplot2)

# Read in the source data
emissions_data <- readRDS("data/summarySCC_PM25.rds")

# Get the data for Baltimore
baltimore_data <- emissions_data[emissions_data$fips == '24510', ]

# Aggregate by both year and type
baltimore_pm25_by_year_and_type <- aggregate(baltimore_data$Emissions,
                                             by = list(year = baltimore_data$year,
                                                       type = baltimore_data$type),
                                             sum)

# Convert years to factors (this way the x labels on the bar plot are treated
# as points rather than a numeric continuum, and we get labels that match the years)
baltimore_pm25_by_year_and_type$year <- as.factor(baltimore_pm25_by_year_and_type$year)

# We want to see the change over time, but there are only four points,
# so a bar plot may be easier to look at than a line.
ggplot(baltimore_pm25_by_year_and_type,
       aes(x = year, y = x, color = type, fill = type)
      ) +
      geom_bar(stat = 'identity') +
      ggtitle('Tons of PM2.5 per year in Baltimore City, by Category') +
      xlab('Year') +
      ylab('Tons of PM2.5') +

      # This facets the charts on the `type` var, yielding us a bar graph
      # for each separate source type.
      facet_wrap(~ type, ncol = 1, scales = 'free_y') +

      # Here we add a horizontal line to the top of the first year we have data for
      # (1999).  This way we can easily see whether the subsequent years had more or
      # less PM2.5 than the first
      geom_hline(aes(yintercept = x, color = type),
                 data = baltimore_pm25_by_year_and_type[baltimore_pm25_by_year_and_type$year ==
                                                        min(as.character(baltimore_pm25_by_year_and_type$year)), ],
                 linetype = 'dashed')

# Use the png graphic device to generate the image file
dev.copy(png, file = "plot3.png", width = 480, height = 480)
dev.off()

### RESULT
# Of the four types of sources indicated by the type
# (point, nonpoint, onroad, nonroad) variable, which of these four sources
# have seen decreases in emissions from 1999–2008 for Baltimore City?
#
# Non-road, Non-point, and On-road have all seen decreases over the period.
#
# Which have seen increases in emissions from 1999–2008?
#
# Point emissions have actually increased slightly since 1999, peaking in 2005
#