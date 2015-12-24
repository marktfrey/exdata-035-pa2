# Exploring PM2.5 Emissions Data

### Peer Assessment 2, Exploratory Data Analysis

The overall goal of this assignment is to explore the National Emissions Inventory database and see what it say about fine particulate matter pollution in the United states over the 10-year period 1999–2008. You may use any R package you want to support your analysis.

You must address the following questions and tasks in your exploratory analysis. For each question/task you will need to make a single plot. Unless specified, you can use any plotting system in R to make your plot.



---

## Question 1

> Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? Using the base plotting system, make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.


```r
# We want TOTAL PM2.5 by YEAR for this plot.
emissions_by_year <- aggregate(emissions_data$Emissions, by = list(year = emissions_data$year), sum)

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
```

![](README_files/figure-html/unnamed-chunk-2-1.png) 


```r
# Use the png graphic device to generate the image file
dev.copy(png, file = "plot1.png", width = 480, height = 480)
dev.off()
```

---

## Question 2

> Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008? Use the base plotting system to make a plot answering this question.


```r
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
```

![](README_files/figure-html/unnamed-chunk-4-1.png) 


```r
# Use the png graphic device to generate the image file
dev.copy(png, file = "plot2.png", width = 480, height = 480)
dev.off()
```

Yes, we can see that total PM2.5 emissions have decreased in Baltimore over the time period 1999-2008.

---

## Question 3

> Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system to make a plot answer this question.


```r
# Get the data for baltimore
baltimore_data <- emissions_data[emissions_data$fips == '24510', ]

# Aggregate by both year and type
baltimore_pm25_by_year_and_type <- aggregate(baltimore_data$Emissions, by = list(year = baltimore_data$year, type = baltimore_data$type), sum)

baltimore_pm25_by_year_and_type$year <- as.factor(baltimore_pm25_by_year_and_type$year)

library("reshape2")
library("ggplot2")
```

```
## Warning: package 'ggplot2' was built under R version 3.2.3
```

```r
ggplot(baltimore_pm25_by_year_and_type, 
       aes(x = year, y = x, color = type, fill = type)
) + 
    geom_bar(stat = 'identity') +
    ggtitle('Tons of PM2.5 per year in Baltimore City, by Category') +
    xlab('Year') +
    ylab('Tons of PM2.5') +
    facet_wrap(~ type, ncol = 1, scales = 'free_y') + 
    geom_hline(aes(yintercept = x, color = type), 
               data = baltimore_pm25_by_year_and_type[baltimore_pm25_by_year_and_type$year == min(as.character(baltimore_pm25_by_year_and_type$year)), ], 
               linetype = 'dashed')
```

![](README_files/figure-html/unnamed-chunk-6-1.png) 

```r
ggplot(baltimore_pm25_by_year_and_type, 
       aes(x = year, y = x, group = type, color = type)
      ) + 
      geom_line() + 
      ggtitle('Tons of PM2.5 per year in Baltimore City, by Category') +
      xlab('Year') +
      ylab('Tons of PM2.5') + 
      geom_hline(aes(yintercept = x, color = type), 
               data = baltimore_pm25_by_year_and_type[baltimore_pm25_by_year_and_type$year == min(as.character(baltimore_pm25_by_year_and_type$year)), ], 
               linetype = 'dashed')
```

![](README_files/figure-html/unnamed-chunk-6-2.png) 


```r
# Use the png graphic device to generate the image file
dev.copy(png, file = "plot3.png", width = 480, height = 480)
dev.off()
```


---

## Question 4

> Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?


---

## Question 5

> How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?


---

## Question 6

> Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, California (fips == "06037"). Which city has seen greater changes over time in motor vehicle emissions?

