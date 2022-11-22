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
input_compare_rounds <- args[3]
output_dir <- args[4]

######################
# Preamble
# --------------------

source(mmods_viz_tools)

######################
# Load data
# --------------------

load(input_file)
load(input_compare_rounds)

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
        mapping = aes(x = q50, y = intervention.rev, color = as.factor(intervention_rank))) +
    geom_errorbarh(mapping = aes(xmin = q5, xmax = q95), size = 1, alpha = 0.7, height = 0) +
    geom_errorbarh(mapping = aes(xmin = q25, xmax = q75), size = 2, alpha = 0.7, height = 0)+
    geom_point(size = 3) +
    scale_color_manual(values = rev(brewer.pal(11, "RdYlBu")[c(1,3,9,11)]))+
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
    scale_color_manual(values = rev(brewer.pal(11, "RdYlBu")[c(1,3)]))+
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


plot_agg_peak_hosp <- ggplot(data = df_aggregate %>% 
                               filter(objective == "peak_hosp"), 
                             mapping = aes(x = q50, y = intervention.rev, color = as.factor(intervention_rank))) +
  geom_vline(xintercept = 200, size = 1, alpha = 0.7, color = "black", linetype = "dashed") +
  geom_errorbarh(mapping = aes(xmin = q5, xmax = q95), size = 1, alpha = 0.7, height = 0) +
  geom_errorbarh(mapping = aes(xmin = q25, xmax = q75), size = 2, alpha = 0.7, height = 0)+
  geom_point(size = 3) +
  scale_color_manual(values = rev(brewer.pal(11, "RdYlBu")[c(1,3,9,11)]))+
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


## days closed round 1 vs round 2
df_compare$id.rev = factor(df_compare$id, levels = rev(levels(df_compare$id)))

## linguistic uncertainty plot days closed
plot_ling_uncert = ggplot(data = df_compare %>% filter(intervention %in%  c("closed")) %>% filter(objective=="days_closed") %>% filter(id != "aggregate") %>%
             group_by("objective","intervention"), mapping = aes(x = q50, y = id.rev, color = as.factor(round))) + #, color = intervention
  geom_errorbarh(mapping = aes(xmin = q5, xmax = q95),
                 size = 1, height = 0, alpha = 0.5) +
  geom_errorbarh(mapping = aes(xmin=q25,xmax=q75),size=2,height = 0, alpha = 0.5)+
  geom_point(size = 3, alpha = 0.7)+
  geom_point(shape = 1,size = 3,colour = "black")+
  scale_color_manual(values = c("black","#A50026"), breaks = c(1,2), labels = c("Round 1", "Round 2"))+
  scale_x_continuous(breaks=c(50,100,150,184,200,228,244),labels=c(50,100,150,"Start of forecast: 184",200,"Stay at home: 228","State of emergency: 244"))+
  labs(x = "", y = "", color = "")+
  theme_bw() + 
  theme(text = element_text(size=font.size),
        axis.text.y= element_blank(),
        axis.text.x=element_text(angle=30,hjust=1),
        axis.title.x = element_blank(),
        strip.background.y = element_blank(),
        strip.placement = "outside",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.text.y.right = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = c(.25,.25))



#### save separately
i <- 1

for(p in list(plot_agg_cumu_infections, plot_all_cumu_infections_open, plot_agg_peak_hosp, plot_ling_uncert)){
    
    output_fig <- paste0("Figure4_panel", i, ".pdf")
    
    ggsave(file.path(output_dir, output_fig),
        p,
        width = 4,
        height = 4,
        units = "in",
        dpi = 300)
    
    i <- i + 1
}

