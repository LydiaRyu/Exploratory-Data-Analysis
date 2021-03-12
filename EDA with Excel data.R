# EDA with Excel data

library(readxl)
library(stringr)
library(dplyr)
library(tidyverse)

setwd("C:/Users/User/Desktop/EUN/Exploratory Data Analysis")
data <- read_excel("China-Global-Investment-Tracker-2019-Spring.xlsx",skip=5)

# Change column names
colnames(data) <- data %>% colnames() %>%
  str_replace_all(" "
                  ,"_") %>% str_to_lower()
glimpse(data)


single<-data %>%
  group_by(country) %>%
  summarize(len=length(unique(region)))

head(single)

single %>%
  filter(len>=2)

data %>%
  filter(country =="Indonesia")%>%
  select(country, region)



sum_data<-data %>%
  group_by(region, sector) %>%
  summarize(Q_sum = sum(quantity_in_millions)) 

Rsum_data <- sum_data %>%
  group_by(region) %>%
  summarize(R_sum=sum(Q_sum))


left_join(sum_data, Rsum_data) %>%
  mutate(proportion=round(Q_sum/R_sum, digits = 3)) %>%
  select(sector, region, proportion) %>%
  spread(region, proportion) %>%
  rename(AME_NA ="Arab Middle East and North Africa",
         Au="Australia", 
         EA="East Asia",
         Eu="Europe",
         N.A="North America",
         SA="South America",
         SSA="Sub-Saharan Africa",
         USA="USA",
         WA="West Asia")


data %>%
  group_by(sector) %>%
  summarize(Q_mean = mean(quantity_in_millions), Q_sd = sd(quantity_in_millions)) %>%
  arrange(Q_sd, Q_mean)


data %>% 
  group_by(sector, year) %>%
  summarize(Total=sum(quantity_in_millions)) %>%
  select(sector, year, Total) %>%
  spread(sector, Total)