### Question 1
#
# Have total emissions from PM2.5 decreased in the United States
# from 1999 to 2008? Using the base plotting system, make a plot
# showing the total PM2.5 emission from all sources for each of
# the years 1999, 2002, 2005, and 2008.

# Fetch the data
source('get_data.R')

# Read in the source data
emissions_data <- readRDS("data/summarySCC_PM25.rds")

# We want TOTAL PM2.5 by YEAR for this plot.
emissions_by_year <- aggregate(emissions_data$Emissions,
                               by = list(year = emissions_data$year),
                               sum)

# We'll display this as a barplot
# Since the y-axis is quite large, we'll manually set it to display
# millions-of-tons, rather than tons in scientific notation.
# We use `yaxt = "n"` to not render the y axis, then manually render below.
max_y <- (round(max(emissions_by_year$x) / 1000000) + 1) * 1000000
max_y_mil <- max_y / 1000000

barplot( emissions_by_year$x, 
         names.arg=emissions_by_year$year,
         main = 'Total PM2.5 Emission (all sources)',
         xlab = 'Year',
         yaxt = 'n',
         ylab = 'Total PM2.5 (millions of tons)',
         ylim = c(0, max_y)
)

axis(side=2, at= 1000000 * c(0:max_y_mil), labels = c(0:max_y_mil))

# Use the png graphic device to generate the image file
dev.copy(png, file = "plot1.png", width = 480, height = 480)
dev.off()

### RESULT
# Have total emissions from PM2.5 decreased in the United States
# from 1999 to 2008?
#
# Yes, total emissions from PM2.5 have decreased overall for each year
# there is a reading.
#
