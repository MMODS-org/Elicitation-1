#!/usr/bin/env Rscript 


######################
# Read command-line args
# --------------------

args <- commandArgs(trailingOnly = TRUE)

mmods_analysis_tools <- args[1]
input_csv_file <- args[2]
input_model_weights <- args[3]
output_csv_file <- args[4]
mmods_round <- args[5]

######################
# Preamble
# --------------------

source(mmods_analysis_tools)

######################
# Load data 
# --------------------

dat <- read.csv(input_csv_file, stringsAsFactors = FALSE) # input clean individual team data all quantiles
model_weights <- read.csv(input_model_weights, stringsAsFactors = FALSE)

######################
# Calcualte aggregate for all intervention/objectives pairs
# --------------------
out_agg <- calculate_aggregate(dat,unique(dat$id), unique(dat$objective), unique(dat$intervention), model_weights, mmods_round)


######################
# Combine with team data
# --------------------
dat_all_agg = rbind(dat, out_agg)

write_csv(dat_all_agg, file = output_csv_file)





