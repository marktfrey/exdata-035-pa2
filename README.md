# Exploring PM2.5 Emissions Data

### Peer Assessment 2, Exploratory Data Analysis

The overall goal of this assignment is to explore the National Emissions Inventory database and see what it say about fine particulate matter pollution in the United states over the 10-year period 1999–2008. You may use any R package you want to support your analysis.

You must address the following questions and tasks in your exploratory analysis. For each question/task you will need to make a single plot. Unless specified, you can use any plotting system in R to make your plot.

---

## Question 1

> Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? Using the base plotting system, make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.

![](README_files/figure-html/plot1-1.png) 

---

## Question 2

> Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008? Use the base plotting system to make a plot answering this question.

![](README_files/figure-html/plot2-1.png) 

---

## Question 3

> Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system to make a plot answer this question.


```
## Warning: package 'ggplot2' was built under R version 3.2.3
```

![](README_files/figure-html/plot3-1.png) 

---

## Question 4

> Across the United States, how have emissions from coal combustion-related sources changed
from 1999–2008?

![](README_files/figure-html/plot4-1.png) 

---

## Question 5

> How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?

![](README_files/figure-html/plot5-1.png) 

---

## Question 6

> Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, California (fips == "06037"). Which city has seen greater changes over time in motor vehicle emissions?


```
## Warning: Stacking not well defined when ymin != 0
```

![](README_files/figure-html/plot6-1.png) 
