#!/usr/bin/env Rscript 
# 
# Script to produce individual panels for 
# multi-panel figure from all of MMODS1 data
# highlighting all the aspects of the framework.  

######################
# Read command-line args
# --------------------

args <- commandArgs(trailingOnly = TRUE)

mmods_viz_tools <- args[1]
input_file <- args[2]
output_dir <- args[3]

######################
# Preamble
# --------------------

source(mmods_viz_tools)

######################
# Load data
# --------------------

load(input_file)

######################
# Plotting dataframes 
# (all subsets of df_plot, 
# simply for convenience/tidyness)
# --------------------

df_aggregate <- df_plot %>% 
                    filter(id == "Aggregate") %>% 
                    group_by("objective", "intervention")

df_cumu_infections_open <- df_plot %>% 
        filter(id != "Aggregate" & intervention == "open" & objective == "cumu_infections")

font.size <- 20

# Plot of cumulative infections (aggregate)
plot_agg_cumu_infections <- ggplot(data = df_aggregate %>% filter(objective == "cumu_infections"), 
        mapping = aes(x = q50, y = intervention, color = as.factor(intervention_rank))) +
    geom_errorbarh(mapping = aes(xmin = q5, xmax = q95), size = 1, alpha = 0.7, height = 0) +
    geom_errorbarh(mapping = aes(xmin = q25, xmax = q75), size = 2, alpha = 0.7, height = 0)+
    geom_point(size = 3) +
    scale_color_manual(values = rev(brewer.pal(11, "RdYlBu")[c(1,3,9,11)]), 
        breaks = c(1,4), labels = c("Lowest Rank", "Highest Rank"))+
    scale_x_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
    scale_y_discrete(labels = rev(int.labs)) +
    labs(color = "", x = "",y = "") + #Cumulative infections
    theme_bw() + 
    theme(text = element_text(size=font.size),
        axis.text.x = element_text(angle = 30, hjust = 1),
        strip.background.y = element_blank(),
        axis.text.y = element_blank(),
        strip.text.y = element_text(angle = 180),
        axis.ticks.y = element_blank(),
        strip.placement = "outside",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

plot_all_cumu_infections_open <- ggplot(data = df_cumu_infections_open, 
        mapping = aes(x = q50, y = id.rev, color = as.factor(intervention_rank))) +
    geom_errorbarh(mapping = aes(xmin = q5, xmax = q95), size = 1, alpha = 0.7, height = 0) +
    geom_errorbarh(mapping = aes(xmin = q25, xmax = q75), size = 2, alpha = 0.7, height = 0)+
    geom_point(size = 3) +
    scale_color_manual(values = rev(brewer.pal(11, "RdYlBu")[c(1,3)]), 
                       breaks = c(1,4), labels = c("Lowest Rank", "Highest Rank"))+
    scale_x_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
    scale_y_discrete(labels = rev(int.labs)) +
    labs(color = "", x = "",y = "") + #Cumulative infections
    theme_bw() + 
    theme(text = element_text(size=font.size),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
        axis.text.x = element_text(angle = 30, hjust = 1),
        strip.background.y = element_blank(),
        strip.text.y = element_text(angle = 180),
        strip.placement = "outside",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")


plot_agg_peak_hosp <- ggplot(data = df_aggregate %>% filter(objective == "peak_hosp"), 
        mapping = aes(x = q50, y = intervention, color = as.factor(intervention_rank))) +
    geom_vline(xintercept = 200, size = 1, alpha = 0.7, color = "black", linetype = "dashed") +
    geom_errorbarh(mapping = aes(xmin = q5, xmax = q95), size = 1, alpha = 0.7, height = 0) +
    geom_errorbarh(mapping = aes(xmin = q25, xmax = q75), size = 2, alpha = 0.7, height = 0)+
    geom_point(size = 3) +
    scale_color_manual(values = rev(brewer.pal(11, "RdYlBu")[c(1,3,9,11)]), 
        breaks = c(1,4), labels = c("Lowest Rank", "Highest Rank"))+
    scale_x_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
    scale_y_discrete(labels = rev(int.labs)) +
    labs(color = "", x = "",y = "") + #Peak hospitilizations
    theme_bw() + 
    theme(text = element_text(size=font.size),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.background.y = element_blank(),
        strip.text.y = element_text(angle = 180),
        strip.placement = "outside",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

i <- 1

for(p in list(plot_agg_cumu_infections, plot_all_cumu_infections_open, plot_agg_peak_hosp)){
    
    output_fig <- paste0("multipanel_draft_panel_", i, ".png")
    
    ggsave(file.path(output_dir, output_fig),
        p,
        width = 4,
        height = 4,
        units = "in",
        dpi = 300)
    
    i <- i + 1
}

# NB: these subpanels might be able to be arrange within R using something like the following,
# although I had issues making the subpanels a consistent size.  
# 
# marrangeGrob(list(plot_agg_cumu_infections, plot_all_cumu_infections_open, plot_agg_peak_hosp),
#    nrow = 2, ncol = 2)
