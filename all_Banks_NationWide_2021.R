# Script Name:          all_Banks_NationWide_2021.R
# Created on:           Thu-June-30-2022
# Author:               Dr. Martin Calvino
# Documentation:        write weblink to Medium post on here
# Purpose:              Visualize the percentage of denied loans for latino and non-latino applicants
# Version:              v2.05.14.2023 (version1.June.30.2022)
# Execution:            Run interactively on RStudio
# Data Source:          Home Mortgage Disclosure Act - URL: https://ffiec.cfpb.gov/data-browser/data/2021?category=nationwide&ethnicities=Hispanic%20or%20Latino,Not%20Hispanic%20or%20Latino
# Data Comment:         Downloaded pre-filtered dataset for ethnicity (Hispanic or Latino vs Not Hispanic or Latino)
# Data Period:          2021
# R Version used:       4.2.1


# load libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(sf)
library(rgeos)
library(MetBrewer)
library(rnaturalearth)
library(remotes)


# this file consist of code following this logic:
# SECTION A > home loan data wrangling
# SECTION B > load geographic data to display counties
# SECTION C > incorporate home loan data into county geography data
# SECTION D > visualize home loan data on map of the US (partitioned by county)

################################################################################

# SECTION A > home loan data wrangling

# load dataset
# all banks nationwide 2021 as 'aban21'
aban21 <- read.csv(file.choose()) #19,182,225 observations x 99 variables

# inspect variable names
colnames(aban21)

# select variables of interest (voi)
county <- aban21$county_code
ethnicity <- aban21$derived_ethnicity
action <- aban21$action_taken

# create new data frame
aban21.voi <- data.frame(county, ethnicity, action)

# identify and remove missing values
sum(is.na(aban21.voi)) # 211,955 missing values
aban21.voi <- na.omit(aban21.voi) # 18,970,270 observations x 3 variables
head(aban21.voi, n = 25)

# recode ethnicity
aban21.voi$ethnicity[aban21.voi$ethnicity == "Hispanic or Latino"] <- "Latino"
aban21.voi$ethnicity[aban21.voi$ethnicity == "Not Hispanic or Latino"] <- "Not.Latino"

# arrange observations by county
aban21.voi <- arrange(aban21.voi, county)
head(aban21.voi, n = 25)

# is there home loan data for how many counties?
COunties <- factor(aban21.voi$county)
length(levels(COunties)) # 3,222 counties
View(COunties)

# partition aban21.voi into 2 data frames based on ethnicity
# each data frame will lead to two maps

# latino data frame
latino.df <- filter(aban21.voi, ethnicity == "Latino") # 2,298,965 observations
# not.latino data frame
not.latino.df <- filter(aban21.voi, ethnicity == "Not.Latino") # 16,671,305 observations

# count loans per county for latinos' applicants
latino.loans.per.county <- latino.df %>%
  group_by(county) %>%
  tally()

View(latino.loans.per.county) # financial institutions in 3,086 counties processed loan applications from latinos

# count loans per county for not.latinos' applicants
not.latinos.per.county <- not.latino.df %>%
  group_by(county) %>%
  tally()

View(not.latinos.per.county) # financial institutions in 3,203 counties processed loan applications from not.latinos

# NOW... select denied applications for both latinos and not.latinos and count how many applications there are per county
latinos.denied <- filter(latino.df, action == 3 | action == 7)
not.latinos.denied <- filter(not.latino.df, action == 3 | action == 7)

# count denied loans per county for latinos' applicants
denied.latino.loans.per.county <- latinos.denied %>%
  group_by(county) %>%
  tally()
View(denied.latino.loans.per.county)

# count denied loans per county for not.latinos' applicants
denied.not.latinos.per.county <- not.latinos.denied %>%
  group_by(county) %>%
  tally()
View(denied.not.latinos.per.county)

# implement join operation to combine latino.loans.per.county with denied.latinos.loans.per.county
j1.latino.per.county <- latino.loans.per.county %>%
  left_join(denied.latino.loans.per.county, by = "county")

View(j1.latino.per.county)

# rename variables
names(j1.latino.per.county)[2] <- "all.loans"
names(j1.latino.per.county)[3] <- "denied.loans"
View(j1.latino.per.county)

# calculate percentage of loans that were denied for each county
j1.latino.per.county <- mutate(j1.latino.per.county, pct.denied = (denied.loans*100)/all.loans)
View(j1.latino.per.county)


# implement join operation to combine not.latinos.per.county with denied.not.latinos.per.county
j2.not.latinos.per.county <- not.latinos.per.county %>%
  left_join(denied.not.latinos.per.county, by = "county")

View(j2.not.latinos.per.county)

# rename variables
names(j2.not.latinos.per.county)[2] <- "all.loans"
names(j2.not.latinos.per.county)[3] <- "denied.loans"
View(j2.not.latinos.per.county)

# calculate percentage of loans that were denied for each county
j2.not.latinos.per.county <- mutate(j2.not.latinos.per.county, pct.denied = (denied.loans*100)/all.loans)
View(j2.not.latinos.per.county)

################################################################################

# SECTION B > load geographic data to display counties

# load the grographical data

# locate ne_10m_admin_2_counties.shp loaded from Natural Earth:
# https://www.naturalearthdata.com/downloads/10m-cultural-vectors/10m-admin-2-counties/
counties.file <- file.choose()
counties.file # "/home/calviot/Desktop/ne_10m_admin_2_counties.shp"

# read file with read_sf
counties <- read_sf(counties.file)
print(counties, n=0)
View(counties)

# get CRS details of counties
st_crs(counties)

# EPSG codes of different coordinate systems can be retrieved from https://epsg.io/
# You use the EPSG code to specify the CRS to use in the coord_sf() function.
# counties = st_set_crs(counties, "EPSG:4326) # set CRS

# reorder rows based on county codes
counties <- arrange(counties, CODE_LOCAL) # county codes start with 0 in CODE_LOCAL
counties

################################################################################

# SECTION C > incorporate home loan data into county geography data


# add leading 0s to county codes in home loan datasets and rename the variable CODE_LOCAL so I can perform left_join() operation
j2.not.latinos.per.county$county[1:316] <- paste0("0", j2.not.latinos.per.county$county) # add leading 0 only until row 316
names(j2.not.latinos.per.county)[1] <- "CODE_LOCAL"
names(j2.not.latinos.per.county)[4] <- "pct.denied.NOT.latinos"
View(j2.not.latinos.per.county)
# keep only COUNTY_CODE and pct.denied.NOT.latinos variables
j2 <- j2.not.latinos.per.county[, -c(2,3)]
View(j2) # great!

j1.latino.per.county$county[1:256] <- paste0("0", j1.latino.per.county$county) # add leading 0 only until row 256
names(j1.latino.per.county)[1] <- "CODE_LOCAL"
names(j1.latino.per.county)[4] <- "pct.denied.latinos"
View(j1.latino.per.county) 
# keep only COUNTY_CODE and pct.denied.latinos variables
j1 <- j1.latino.per.county[, -c(2,3)]
View(j1) # great!

# join j2 with j1
j21 <- j2 %>%
  left_join(j1, by = "CODE_LOCAL")
View(j21) # super great!


# add a column with percentage difference in denials between not.latinos and latinos
j21 <- mutate(j21, pct.diff =  pct.denied.latinos - pct.denied.NOT.latinos)
View(j21) # super duper
summary(j21[, 2:4])


# incorporate loan data into county geography data
counties.loanData.WF <- counties %>%
  left_join(j21, by = "CODE_LOCAL")
View(counties.loanData.WF)

################################################################################

# SECTION D > visualize home loan data on map of the US (partitioned by county)

# not.latinos loan applicants
counties.loanData.WF <- mutate(counties.loanData.WF, pct.diff_category = cut(pct.denied.NOT.latinos, breaks = c(1, 5, 10, 15, 20, 25, 100)))

map <- ggplot(counties.loanData.WF, aes(fill = pct.diff_category)) +
  geom_sf(color = "white", size = 0.1) +
  scale_fill_brewer(palette = "Blues", name = "pct.diff per county",
                    labels = c("1-5", "6-10", "11-15", "16-20", "21-25", "26% or more")) +
  theme_minimal()

map %+%
  filter(counties.loanData.WF, REGION != "AK", REGION != "HI")

# latino loan applicants
counties.loanData.WF <- mutate(counties.loanData.WF, pct.diff_category = cut(pct.denied.latinos, breaks = c(1, 5, 10, 15, 20, 25, 100)))

map <- ggplot(counties.loanData.WF, aes(fill = pct.diff_category)) +
  geom_sf(color = "white", size = 0.1) +
  scale_fill_brewer(palette = "Blues", name = "pct.diff per county",
                    labels = c("1-5", "6-10", "11-15", "16-20", "21-25", "26% or more")) +
  theme_minimal()

map %+%
  filter(counties.loanData.WF, REGION != "AK", REGION != "HI")
