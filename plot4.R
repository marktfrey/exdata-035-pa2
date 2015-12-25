### Question 4
# 
# Across the United States, how have emissions 
# from coal combustion-related sources changed from 1999–2008?

# Fetch the data
source('get_data.R')

# Read in the source data
emissions_data <- readRDS("data/summarySCC_PM25.rds")
source_codes   <- readRDS("data/Source_Classification_Code.rds")

# To determine what is meant by 'coal combustion-related' we examine the source-codes
# data.  The source Short.Name contains a main category / sub category string,
# and combustion (abbreviated as 'comb') and coal can be found by searching these.

# First we need to get all the coal related source codes
coal_sources <- source_codes[grepl('[cC]oal', as.character(source_codes$Short.Name)), ]

# Then narrow this to the ones that are combustion-related
coal_comb_sources <- coal_sources[grepl('[cC]omb', as.character(coal_sources$Short.Name)), ]

# This gives us 91 coal-combustion related codes.
# We're interested in the change of particulate output across these codes for the whole country, so
# we need to filter the main list to the correct codes, then aggregate by year.
coal_comb_emissions_data <- emissions_data[emissions_data$SCC %in% coal_comb_sources$SCC, ]

coal_comb_emissions_by_year <- aggregate(coal_comb_emissions_data$Emissions, by = list(year = coal_comb_emissions_data$year), sum)
coal_comb_emissions_by_year$year <- as.factor(coal_comb_emissions_by_year$year) 

# We want to see the change over time, but there are only four points,
# so a bar plot may be easier to look at than a line.
ggplot(coal_comb_emissions_by_year, 
       aes(x = year, y = x),
      ) + 
      geom_bar(stat = 'identity') +
      ggtitle('Tons of PM2.5 related to Coal Combustion, by year') +
      xlab('Year') +
      ylab('Tons of PM2.5') +
  
      # We'll produce a horizontal line to help identify the relationship of
      # subsequent years to the first observed year.
      geom_hline(aes(yintercept = x), 
                     data = coal_comb_emissions_by_year[coal_comb_emissions_by_year$year == min(as.character(coal_comb_emissions_by_year$year)), ], 
                     linetype = 'dashed')

# Use the png graphic device to generate the image file
dev.copy(png, file = "plot4.png", width = 480, height = 480)
dev.off()

### RESULT
# Across the United States, how have emissions 
# from coal combustion-related sources changed from 1999–2008?
# 
# Emissions from coal combustion-related sources have decreased overall
# during the period from 1999 - 2008.
#
