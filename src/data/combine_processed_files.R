#!/usr/bin/env Rscript
# 
# read individual team CSV files 
# and save as single CSV

######################
# Preamble
# --------------------
require(readr)
require(reshape2)

######################
# Read command-line args
# --------------------

args <- commandArgs(trailingOnly = TRUE)

input_folder <- args[1]
output_csv_file <- args[2]


######################
# Load data
# --------------------

# Find files in input folder starting with 'team' and ending with 'csv'
files <- list.files(file.path(input_folder))
files <- files[grepl("team.*csv", files)]

dat <- list()
for(i in 1:length(files)){
  dat[[i]] <- read_csv(file.path(input_folder, files[i]), col_types = "cddcddddd")
}

dat_all <- do.call(rbind, dat)

# Create a long dataframe  (easier for ggplot/dplyr and other packages/functions)
dat_all_long <- melt(as.data.frame(dat_all), 
    id.vars = c("round", "quantile", "intervention", "team"))
names(dat_all_long) <- c("round", "quantile", "intervention", "id", "objective", "value")

write_csv(dat_all_long, file = output_csv_file)

