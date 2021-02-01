#!/usr/bin/env Rscript
# 
# read individual team CSV files 
# and save as single CSV

######################
# Preamble
# --------------------
require(readr)
require(reshape2)
require(readxl)
require(tidyverse)
require(dplyr)

######################
# Read command-line args
# --------------------

args <- commandArgs(trailingOnly = TRUE)

input_county_attr_file <- args[1]
input_county_deaths_file <- args[2]
output_folder <- args[3]

# input_county_attr_file <- "./Elicitation1/public_repo/data/processed/county/county_attributes.xlsx"
# input_county_deaths_file <- "./Elicitation1/public_repo/data/processed/county/time_series_covid19_deaths_US.csv"
# output_folder <- "./Elicitation1/public_repo/data/processed/county"

######################
# Load data
# --------------------
df_county_attr <- readxl::read_excel(input_county_attr_file)
df_county_deaths <- read_csv(input_county_deaths_file)

######################
# Format county data (for Figs S14-S15)
# --------------------
dates <- names(df_county_deaths)[which(names(df_county_deaths) == "1/22/20"):ncol(df_county_deaths)]
df_deaths_long <- pivot_longer(df_county_deaths, all_of(dates), names_to = "date", values_to = "cumu_deaths")
df_deaths_long <- subset(df_deaths_long, as.Date(date, format = "%m/%d/%y") >= as.Date("05/15/20", format = "%m/%d/%y"))

# add column with logical for closed
df_county_attr$is_closed_strict <- as.numeric(df_county_attr$FIPS %in% subset(df_county_attr, SimilarToAdams == 1 & StayAtHomeStart <= "2020-05-15" & is.na(StayAtHomeEnd))$FIPS)

# merge with cumulative deaths for the subset of counties
df_county <- left_join(df_county_attr %>% subset(SimilarToAdams ==1), df_deaths_long, by = "FIPS")

#fix date format
df_county$date <- as.Date(df_county$date, format = "%m/%d/%y")
df_county <- arrange(df_county, date) 

# subset to closed only
df_county_closed_strict <- subset(df_county, is_closed_strict ==1)
df_county_closed_partial <- subset(df_county, Closed == 1)

closed_strict_deaths_summary <- aggregate(df_county_closed_strict$cumu_deaths, by=list(date = df_county_closed_strict$date),FUN=function(observations){quantile(observations, probs=seq(0,1,0.01))})

closed_partial_deaths_summary <- aggregate(df_county_closed_partial$cumu_deaths, by=list(date = df_county_closed_partial$date),FUN=function(observations){quantile(observations, probs=seq(0,1,0.01))})

write_csv(cbind(closed_strict_deaths_summary, as_tibble(closed_strict_deaths_summary$x)), file = file.path(paste0(output_folder, '/county_deaths_cdf_closed_strict.csv')))

write_csv(cbind(closed_partial_deaths_summary, as_tibble(closed_partial_deaths_summary$x)), file = file.path(paste0(output_folder, '/county_deaths_cdf_closed_partial.csv')))


######################
# Text
# --------------------

counts <- tibble(
  mu_pop_100K = length(unique(df_county_attr$FIPS)), # number of counties with population size 90K - 110K
  same_mobility = nrow(subset(df_county_attr, SimilarToAdams == 1)),
  closed_strict = nrow(subset(df_county_attr, SimilarToAdams == 1 & StayAtHomeStart <= "2020-05-15" & is.na(StayAtHomeEnd))),
  closed_any = nrow(subset(df_county_attr, SimilarToAdams == 1 & Closed == 1))
) 

print(counts)

