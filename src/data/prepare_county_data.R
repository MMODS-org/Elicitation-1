#!/usr/bin/env Rscript

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
input_county_cases_file <- args[3]
output_folder <- args[4]
s 
######################
# Load data
# --------------------
df_county_attr <- readxl::read_excel(input_county_attr_file)
df_county_deaths <- read_csv(input_county_deaths_file)
df_county_cases <- read_csv(input_county_cases_file)

######################
# Format county data (for Figs S14-S15)
# --------------------
dates <- names(df_county_deaths)[which(names(df_county_deaths) == "1/22/20"):ncol(df_county_deaths)]
df_deaths_long <- pivot_longer(df_county_deaths, all_of(dates), names_to = "date", values_to = "cumu_deaths") %>% subset(as.Date(date, format = "%m/%d/%y") >= as.Date("05/15/20", format = "%m/%d/%y"))
df_deaths_long <- mutate(df_deaths_long, cumu_deaths_first = with(df_deaths_long, ave(cumu_deaths, FIPS, FUN=first)), cumu_deaths_adj = cumu_deaths - cumu_deaths_first)
df_deaths_long$cumu_deaths_adj[which(df_deaths_long$cumu_deaths_adj<0)] = 0

df_cases_long <- pivot_longer(df_county_cases, all_of(dates), names_to = "date", values_to = "cumu_cases") %>% subset(as.Date(date, format = "%m/%d/%y") >= as.Date("05/15/20", format = "%m/%d/%y"))
df_cases_long <- mutate(df_cases_long, cumu_cases_first = with(df_cases_long, ave(cumu_cases, FIPS, FUN=first)), cumu_cases_adj = cumu_cases - cumu_cases_first)
df_cases_long$cumu_cases_adj[which(df_cases_long$cumu_cases_adj<0)] = 0

# add column with logical for closed
df_county_attr$is_closed_strict <- as.numeric(df_county_attr$FIPS %in% subset(df_county_attr, SimilarToAdams == 1 & StayAtHomeStart <= "2020-05-15" & is.na(StayAtHomeEnd))$FIPS)

# merge with cumulative deaths for the subset of counties
df_county_deaths <- left_join(df_county_attr %>% subset(SimilarToAdams ==1), df_deaths_long, by = "FIPS")
df_county_cases <- left_join(df_county_attr %>% subset(SimilarToAdams ==1), df_cases_long, by = "FIPS")


#fix date format
df_county_deaths$date <- as.Date(df_county_deaths$date, format = "%m/%d/%y")
df_county_deaths <- arrange(df_county_deaths, date)

df_county_cases$date <- as.Date(df_county_cases$date, format = "%m/%d/%y")
df_county_cases <- arrange(df_county_cases, date)

# subset to closed only
df_county_deaths_closed_strict <- subset(df_county_deaths, is_closed_strict ==1)
df_county_deaths_closed_partial <- subset(df_county_deaths, Closed == 1)
df_county_deaths_open <- subset(df_county_deaths, Closed != 1)

df_county_cases_closed_strict <- subset(df_county_cases, is_closed_strict ==1)
df_county_cases_closed_partial <- subset(df_county_cases, Closed == 1)
df_county_cases_open <- subset(df_county_cases, Closed != 1)


# change to cumu_deaths_adj here to see new cumulative deaths (and cases)
closed_strict_deaths_summary <- aggregate(df_county_deaths_closed_strict$cumu_deaths, by=list(date = df_county_deaths_closed_strict$date),FUN=function(observations){quantile(observations, probs=seq(0,1,0.01))})
closed_partial_deaths_summary <- aggregate(df_county_deaths_closed_partial$cumu_deaths, by=list(date = df_county_deaths_closed_partial$date),FUN=function(observations){quantile(observations, probs=seq(0,1,0.01))})
open_deaths_summary <- aggregate(df_county_deaths_open$cumu_deaths, by=list(date = df_county_deaths_open$date),FUN=function(observations){quantile(observations, probs=seq(0,1,0.01))})

closed_strict_cases_summary <- aggregate(df_county_cases_closed_strict$cumu_cases, by=list(date = df_county_cases_closed_strict$date),FUN=function(observations){quantile(observations, probs=seq(0,1,0.01))})
closed_partial_cases_summary <- aggregate(df_county_cases_closed_partial$cumu_cases, by=list(date = df_county_cases_closed_partial$date),FUN=function(observations){quantile(observations, probs=seq(0,1,0.01))})
open_cases_summary <- aggregate(df_county_cases_open$cumu_cases, by=list(date = df_county_cases_open$date),FUN=function(observations){quantile(observations, probs=seq(0,1,0.01))})

write_csv(cbind(date = closed_strict_deaths_summary$date, as_tibble(closed_strict_deaths_summary$x)), file = file.path(paste0(output_folder, '/county_deaths_cdf_closed_strict.csv')))
write_csv(cbind(date = closed_partial_deaths_summary$date, as_tibble(closed_partial_deaths_summary$x)), file = file.path(paste0(output_folder, '/county_deaths_cdf_closed_partial.csv')))
write_csv(cbind(date = open_deaths_summary$date, as_tibble(open_deaths_summary$x)), file = file.path(paste0(output_folder, '/county_deaths_cdf_open.csv')))

write_csv(cbind(date = closed_strict_cases_summary$date, as_tibble(closed_strict_cases_summary$x)), file = file.path(paste0(output_folder, '/county_cases_cdf_closed_strict.csv')))
write_csv(cbind(date = closed_partial_cases_summary$date, as_tibble(closed_partial_cases_summary$x)), file = file.path(paste0(output_folder, '/county_cases_cdf_closed_partial.csv')))
write_csv(cbind(date = open_cases_summary$date, as_tibble(open_cases_summary$x)), file = file.path(paste0(output_folder, '/county_cases_cdf_open.csv')))

######################
# Text
# --------------------

counts <- tibble(
  mu_pop_100K = length(unique(df_county_attr$FIPS)), # number of counties with population size 90K - 110K
  same_mobility = nrow(subset(df_county_attr, SimilarToAdams == 1)), # number of counties with the same mobility
  closed_strict = nrow(subset(df_county_attr, SimilarToAdams == 1 & StayAtHomeStart <= "2020-05-15" & is.na(StayAtHomeEnd))), # number of counties strictly closed
  closed_partial = nrow(subset(df_county_attr, SimilarToAdams == 1 & Closed == 1)), # number of counties partially closed
  open = nrow(subset(df_county_attr, SimilarToAdams == 1 & Closed != 1))
)

print(counts)
