#!/usr/bin/env Rscript 

######################
# Read command-line args
# --------------------

args <- commandArgs(trailingOnly = TRUE)

mmods_analysis_tools <- args[1]
input_csv_file <- args[2]
output_csv_file <- args[3]

######################
# Preamble
# --------------------

source(mmods_analysis_tools)


######################
# Load data 
# --------------------

dat <- read.csv(input_csv_file, stringsAsFactors = FALSE)

######################
# Restructure
# --------------------

df_plot <- dat %>%  #df_long_wp
  group_by(id, intervention, objective) %>% 
  summarise(q1 = min(value), 
            q5 = value[which(quantile==5)],
            q10 = value[which(quantile==10)],
            q25 = value[which(quantile==25)],
            #q50 = median(value),
            q50=value[which(quantile==50)],
            q75 = value[which(quantile==75)],
            q90 = value[which(quantile==90)],
            q95 = value[which(quantile==95)],
            q100 = max(value), .groups = 'drop')

######################
# Calculate ranks of interventions within each team's/aggregate's CDFs
# --------------------
df_plot <- df_plot %>% 
         group_by(id, objective) %>% 
         mutate(intervention_rank = rank(q50))

# Arrange rows by team, then objective, then intervention
df_plot <- df_plot %>%
    arrange(id, objective, intervention)

write_csv(df_plot, path = output_csv_file)
