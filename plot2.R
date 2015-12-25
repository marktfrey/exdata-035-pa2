### Question 2
# 
# Have total emissions from PM2.5 decreased in the Baltimore City,
# Maryland (fips == "24510") from 1999 to 2008? Use the base plotting 
# system to make a plot answering this question.

# Fetch the data
source('get_data.R')

# Read in the source data
emissions_data <- readRDS("data/summarySCC_PM25.rds")

# Get the data/year for Baltimore City
baltimore_data <- emissions_data[emissions_data$fips == '24510', ]
baltimore_pm25_by_year <- aggregate(baltimore_data$Emissions, by = list(year = baltimore_data$year), sum)

# Well make a line chart since we're looking to show change over time
plot(baltimore_pm25_by_year$year, 
     baltimore_pm25_by_year$x,
     pch = 19,
     main = 'Tons of PM2.5 per year in Baltimore City',
     xlab = 'Year',
     ylab = 'Tons of PM2.5',
     ylim = range(0, max(baltimore_pm25_by_year$x))
)

lines(baltimore_pm25_by_year$year, baltimore_pm25_by_year$x)

# Use the png graphic device to generate the image file
dev.copy(png, file = "plot2.png", width = 480, height = 480)
dev.off()

### RESULT
# Have total emissions from PM2.5 decreased in the Baltimore City,
# Maryland (fips == "24510") from 1999 to 2008?
#
# Yes, despite an increase between 2002-2005, the total emissions from PM2.5
# have decreassed overall between 1999 and 2008.
#