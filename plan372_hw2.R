# PLAN 372 Homework 2
# Danielle Owens
# 28 February 2023

# Exploratory analysis of restaurant_inspections data set
# Records of the most recent health inspection for food-service establishments in Wake County
# Data was retrieved from course Canvas

# Load in libraries
library(tidyverse)  # library for dealing with tabular data4
library(lubridate)  # library for handling dates

# Read data into a table, from CSV file
rest = read_csv("restaurant_inspections.csv")

# Viewing first 20 rows and all columns of the data
View(rest[1:20,])

# Task 1
# Histogram of restaurant scores
ggplot(data = rest, aes(x = SCORE)) + geom_histogram(bins = 200)


# Task 2
# First, making sure that open date is in dttm format to sort
rest = mutate(rest, opendate = ymd_hms(RESTAURANTOPENDATE))
view(rest[1:20,])

# Sorting by open date and getting rid of rows with missing open dates
sorted_rest = na.omit(rest[order(rest$opendate),na.last = na])
view(sorted_rest)

# Adding row with year of open date to observe trends chronologically
sorted_rest$year = year(sorted_rest$opendate)

# Ensuring that it worked
unique(sorted_rest$year)

# Separating older and newer restaurants
older = slice_min(sorted_rest, order_by = year, prop = 0.50)
newer = slice_max(sorted_rest, order_by = year, prop = 0.50)

# Getting and printing the mean score of older and newer groups
older_mean = mean(older$SCORE)
older_mean
newer_mean = mean(newer$SCORE)
newer_mean

# Combining into one data frame
group_means = data.frame(older_mean, newer_mean)

# Viewing this, newer restaurants have a marginally better mean score than older restaurants
view(group_means)

# Task 3
# Do inspection scores vary by city?

# Looking to see what the city names are
unique(rest$CITY)

# Recoding the city names
rest = str_to_upper(rest$CITY)

rest = recode(rest$CITY, 
                   `FUQUAY VARINA` = "FUQUAY-VARINA",
                   `RESEARCH TRIANGLE PARK` = "FTP",
                   `HOLLY SPRING` = "HOLLY SPRINGS")

# Making sure that the groups are correct
unique(rest$CITY)

# Calculating means by city
