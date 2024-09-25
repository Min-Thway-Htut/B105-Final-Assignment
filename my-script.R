library(dplyr)
library(ggplot2)
library(stats)
library(tidyverse)

df <- read.csv("data/climate_change_impact_on_agriculture_2024.csv")
head(df)

# Data Cleaning
# Delete unwanted columns

df$Year <- NULL
df$Region <- NULL
df$Adaptation_Strategies <- NULL
df$Economic_Impact_Million_USD <- NULL

head(df)


## Check if there were any missing values

sum(is.na(df))

## Exploray Data Analysis

# Check if were any outliers

boxplot(df$Average_Temperature_C)
boxplot(df$CO2_Emissions_MT)
boxplot(df$Crop_Yield_MT_per_HA)
boxplot(df$Extreme_Weather_Events)


### Summary of dataset

summary(df)


### Relationship between temperature and crop yield

ggplot(df, aes(x = Average_Temperature_C, y = Crop_Yield_MT_per_HA)) + 
  geom_point() + 
  geom_smooth(method = 'lm', col = 'blue') + 
  labs(title = "Temperature vs Crop Yield")


## CO2 Emission and Crop yield

ggplot(df, aes(x = CO2_Emissions_MT, y = Crop_Yield_MT_per_HA)) + 
  geom_point() + 
  geom_smooth(method = 'lm', col = 'blue') + 
  labs(title = "CO2 Emissions vs Crop Yield")

## Hypothesis test
head(df)

model <- lm(Crop_Yield_MT_per_HA ~ CO2_Emissions_MT + Average_Temperature_C + Soil_Health_Index +  Fertilizer_Use_KG_per_HA, data = df)
summary(model)
