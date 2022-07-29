
##########################
# Title: 2000-2020 Presidential Election data by county 
# Project: Everyday Respect - Legal Foundations
# By: Darik de Jong
# Source File: "countypres_2000-2020.csv"
# Data Publisher: MIT Election Data and Science Lab
# Data URL: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/VOQCHQ 
# Original Script: 07/19/2022 
# Last Modified: 07/19/2022 by DDDJ
# Purpose: Find 10 counties, 2 from 5 different states, one is heavily Democratic and the other heavily Republican. 
# Method: County must have a population of at least 100,000. This isn't recorded in the data so it will be checked manually. 
# For each of the five states Find the county where Biden won by the greatest percentage (Democratic proxy).
# Find the county where Trump won by the greatest percentage (Republican Proxy).
#########################

setwd("/Users/Darik_MBP/Documents/Documents/Work/Junior/Summer SPEC/Everday Respect/Election Data/dataverse_files") 

library(dplyr)
library(tidyverse)

data <- read.csv("countypres_2000-2020.csv", stringsAsFactors = F)
View(data)

states <- c("CALIFORNIA", "FLORIDA", "ILLINOIS", "NEW YORK", "TEXAS")  
# The 5 states we are interested in. Reasoning for their inclusion is explained elsewhere. 

parties <- c("REPUBLICAN", "DEMOCRAT") 
# We don't care about third party candidates e.g. Libertarian or Green.

data_2020 <- data%>%
  select(-c("state_po", "office", "version", "mode")) %>%
  filter(year%in%2020)%>%
  filter(state%in%states)%>%
  filter(party%in%parties)%>%
  mutate(Percentage = candidatevotes/totalvotes)
View(data_2020)
# Removed irrelevant columns, removed data for elections before 2020, removed all states except our 5, removed 3rd party candidates

data_pivot <- data_2020%>%
  pivot_wider(names_from = candidate, values_from = Percentage)%>%
  select(-c("year", "party", "candidatevotes", "totalvotes"))%>%
  rename(Biden = "JOSEPH R BIDEN JR", Trump = "DONALD J TRUMP", State = "state", County = "county_name")
View(data_pivot)
# Creates two columns (Biden and Trump) which states what percentage of vote they received. Irrelevant columns are removed.
  
data_merge <- aggregate(x=data_pivot[c("State","County","Biden","Trump")], by=list(County_Name=data_pivot$county_fips), min, na.rm = TRUE)
data_merge <- data_merge%>%
  select(-c("County_Name"))
View(data_merge)
# Merge the two rows for each county to remove NA. Remove duplicate column created by aggregate. How to Aggregate with pipe?

data_final <- data_merge%>%
  arrange(State, desc(Biden)) 
View(data_final)
# Arramge by state in descending order

final_counties <- data_final%>%
  filter(County%in% c("SHASTA", "LOS ANGELES", "SANTA ROSA", "BROWARD", "MACON", "COOK", "OSWEGO", "NEW YORK", "PARKER", "TRAVIS")) 
View(final_counties)
# Creates table of just the 10 counties we want. Explanation of selection down below

write.csv(final_counties, "/Users/Darik_MBP/Documents/Documents/Work/Junior/Summer SPEC/Everday Respect/Election Data//Shortlist of 10 Counties.csv", row.names = FALSE)
# Export table

# Based on these data the 10 Counties are:
# California - Republican: Shasta pop. 180,000 32.3% Biden 65.4% Trump (Lassen, Modoc and Tehama have sub 100,000 population)
# California - Democratic: Los Angeles pop. 10,000,000 71.0% Biden 26.9% Trump (In the top 9 for most Democrat-leaning and most relevant for our purposes)
# Florida - Republican: Santa Rosa pop. 180,000 25.8% Biden 72.4% Trump (Holmes:Gulf have sub 100,000 population)
# Florida - Democratic: Broward pop. 1,900,000 64.6% Biden 34.8% (Gadsden has sub 100,000 population)
# Illinois - Republican: Macon pop. 105,000 57.9% Trump 40.2% Biden (Wayne:Putnam have sub 100,000 population)
# Illinois - Democratic: Cook  pop. 5,170,000 74.3% Biden 24.1% Trump
# New York - Republican: Oswego pop. 117,000 38.5% Biden 60.0% Trump (Wyoming:Tioga have sub 100,000 population)
# New York - Democratic: New York pop. 1,600,000 86.0% 12.1%
# Texas - Republican: Parker pop. 138,000 17.1% Biden 81.5% Trump (Roberts:Cottle have sub 100,000 populations) 
# Texas - Democratic: Travis pop. 1,250,000 71.6% Biden 26.5% Trump 
