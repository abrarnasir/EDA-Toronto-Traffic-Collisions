library(tidyverse)
library(opendatatoronto)
library(ggplot2)
library(arsenal)

#Importing data
traffic_collisions <- search_packages("Police Annual Statistical Report - Traffic Collisions")
traffic_collisions <- traffic_collisions %>% list_package_resources()
df <- traffic_collisions %>% get_resource()

#Data cleaning
df <- drop_na(df)
df <- df[-c(1, 2, 5)]

#Helpers
count_by_geoDiv <- aggregate(df$Count_, by=list(Category=df$GeoDivision), FUN=sum)
count_by_geoDiv <- count_by_geoDiv[count_by_geoDiv$Category != "NSA", , drop=FALSE]

count_by_year <- aggregate(df$Count_, by=list(Category=df$OccurredYear), FUN=sum)

count_by_type <- aggregate(df$Count_, by=list(Category=df$Subtype), FUN=sum)
count_by_type <- count_by_type[count_by_type$Category != "Total Collisions", , drop=FALSE]
count_by_type <- count_by_type[count_by_type$Category != "Fail to Remain - Injury & Property Damage Collisions*", , drop=FALSE]

#Visualisation
by_geoDiv <- ggplot(data = count_by_geoDiv, aes(x=x, y=Category)) + geom_bar(stat="identity", fill="steelblue") + labs(y="Geographic division", x="Count of collision occurences") + ggtitle("Bar plot showing geographic divisions and respective collision counts")

TimeSeries <- ggplot(data = count_by_year, aes(x = Category, y = x)) + geom_line(color = "#00AFBB", size = 1.5) + labs(y="Count of collision occurences", x="Years") + ggtitle("Relationship between year and collision occurences")

#Statistics
summary(count_by_geoDiv$x)
summary(count_by_year$x)
summary(count_by_type$x)

sd(count_by_geoDiv$x)
sd(count_by_year$x)
sd(count_by_type$x)