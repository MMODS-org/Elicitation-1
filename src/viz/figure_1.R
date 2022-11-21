#!/usr/bin/env Rscript 

library(cowplot)
library(patchwork)

######################
# Read command-line args
# --------------------

args <- commandArgs(trailingOnly = TRUE)

mmods_viz_tools <- args[1]
input_round2_rank_file <- args[2]
input_round2_allCDFs <- args[3]
output_file <- args[4]


######################
# Preamble
# --------------------

source(mmods_viz_tools)

load(input_round2_rank_file)
df_cdfs <- read_csv(input_round2_allCDFs, show_col_types = FALSE)

#### Legends: horizontal and vertical
# horizontal
leg = data.frame(x = rep(1,7),
                 y = 1:7,
                 lab = c("4","","3","","2","","1"))
text = data.frame(x = rep(1,2),
                  y = c(0,8),
                  lab = c("Worst", "Best"))

legend = ggplot(data = leg,aes(x = x, y = y))+
  geom_point(aes(color = as.factor(y)), size = 5)+
  geom_text(aes(label = lab), fontface = "bold", color = "white", size = 2.5,vjust = 0.4, hjust = 0.4)+
  geom_text(data = text, aes(x = x, y = y, label = lab))+
  geom_text(aes(x = 1, y = 9, label = "Rank"), fontface = "bold", size = 5)+
  scale_color_manual(values = brewer.pal(11,"RdYlBu")[c(1:3,5,9:11)], breaks = seq(1,7,by=1)) + #, labels = c("Lowest Rank", "Highest Rank") 
  theme_void()+
  theme(legend.position = "none")

#### Figure 1A: aggregate results by scenario, color code plot by rank ####
p <- ggplot(data = df_plot %>% filter(id == "Aggregate") %>% group_by("objective","intervention"), 
            mapping = aes(x = q50, y = intervention.rev, color = as.factor(intervention_rank))) +
  geom_errorbarh(mapping = aes(xmin = q5, xmax = q95),size = 1, alpha = 0.7, height = 0) +
  geom_errorbarh(mapping = aes(xmin=q25,xmax=q75),size=2,alpha=0.7,height = 0)+
  geom_point(size = 3)+
  geom_point(data = df_plot %>% group_by(objective) %>% filter(q95 == max(q95)),aes(x = q95), color = NA)+ # include as dummy to fix scales
  facet_grid(cols=vars(objective),scales="free_x", labeller = labeller(objective = obj.labs.numeral), switch ="y")+
  labs(subtitle = "Aggregate results", tag = "A")+
  scale_color_manual(values = rev(brewer.pal(11, "RdYlBu")[c(1,3,9,11)]))+
  scale_x_continuous(labels = function(x) format(x, big.mark = ",",scientific = FALSE))+
  scale_y_discrete(labels = rev(int.labs))+
  theme_bw(base_size = 12) + 
  theme(axis.text.x=element_text(angle=45,hjust=1),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        strip.background.y = element_blank(),
        strip.text.y = element_text(angle = 180, size = 8),
        strip.text.x = element_text(size = 8),
        strip.placement = "outside",
        legend.position = "none")

inset <- ggplot(data = df_plot %>% 
                  filter(id == "Aggregate", objective == "cumu_deaths") %>% 
                  group_by("objective","intervention"), 
                mapping = aes(x = q50, y = intervention.rev, color = as.factor(intervention_rank))) +
  geom_errorbarh(mapping = aes(xmin = q5, xmax = q95),size = 0.5, height = 0) +
  geom_errorbarh(mapping = aes(xmin=q25,xmax=q75),size=1,height = 0)+
  geom_point(size = 2)+
  #geom_vline(xintercept=500,linetype="dashed")+
  facet_grid(cols=vars(objective),scales="free_x", labeller = labeller(objective = obj.labs), switch ="y")+
  coord_cartesian(xlim=c(0,1000))+
  scale_color_manual(values = rev(brewer.pal(11, "RdYlBu")[c(1,3,9,11)]))+
  scale_x_continuous(labels = function(x) format(x, big.mark = ",",scientific = FALSE))+
  scale_y_discrete(labels = rev(int.labs))+
  labs(color = "", x = "",y = "")+
  theme_bw(base_size = 12) + 
  theme(axis.text.x=element_text(angle=45,hjust=1, size = 8),
                     axis.text.y = element_blank(),
                     axis.ticks.y = element_blank(),
                     axis.title = element_blank(),
                     strip.background.x = element_blank(),
                     strip.text.x = element_blank(),
                     strip.text.y = element_text(angle = 180),
                     strip.placement = "outside",
                     legend.position = "none",
                     panel.grid = element_blank(),
                     plot.margin=grid::unit(rep(0,4), "mm"))

inset2 <- ggplot(data = df_plot %>% 
                   filter(id == "Aggregate", objective == "peak_hosp") %>% 
                   group_by("objective","intervention"), 
                 mapping = aes(x = q50, y = intervention.rev, color = as.factor(intervention_rank))) +
  geom_errorbarh(mapping = aes(xmin = q5, xmax = q95),size = 0.5, height = 0) +
  geom_errorbarh(mapping = aes(xmin=q25,xmax=q75),size=1,height = 0)+
  geom_point(size = 2)+
  geom_vline(xintercept=200,linetype="dashed")+
  facet_grid(cols=vars(objective),scales="free_x", labeller = labeller(objective = obj.labs), switch ="y")+
  coord_cartesian(xlim=c(0,300))+
  scale_color_manual(values = rev(brewer.pal(11, "RdYlBu")[c(1,3,9,11)]))+
  scale_x_continuous(labels = function(x) format(x, big.mark = ",",scientific = FALSE))+
  scale_y_discrete(labels = rev(int.labs))+
  labs(color = "", x = "",y = "")+
  theme_bw(base_size = 12) + 
  theme(axis.text.x=element_text(angle=45,hjust=1, size = 8),
                     axis.text.y = element_blank(),
                     axis.ticks.y = element_blank(),
                     axis.title = element_blank(),
                     strip.background.x = element_blank(),
                     strip.text.x = element_blank(),
                     strip.text.y = element_text(angle = 180),
                     strip.placement = "outside",
                     legend.position = "none",
                     panel.grid = element_blank(),
                     plot.margin=grid::unit(rep(0,4), "mm"))

# add inset to plot
pA <- p +
  annotation_custom2(grob=ggplotGrob(inset), data=df_plot %>% 
                       filter(id == "Aggregate", objective == "cumu_deaths") %>% 
                       group_by("objective","intervention"),
                     ymin = 1.75, ymax=Inf, xmin=2000, xmax=4950)+
  annotation_custom2(grob=ggplotGrob(inset2), data=df_plot %>%
                       filter(id == "Aggregate", objective == "peak_hosp") %>%
                       group_by("objective","intervention"),
                     ymin = 2, ymax=Inf, xmin=550, xmax=1100)
  

#### Figure 1B: aggregate results by scenario, color code plot by rank ####
pB <- ggplot(data = df_plot %>% filter(id!="Aggregate") %>%
               group_by("objective", "intervention"), 
             mapping = aes(x = q50, y = id.rev, color = as.factor(intervention_rank))) + 
  geom_errorbarh(mapping = aes(xmin = q5, xmax = q95), size = 1, height = 0) +
  geom_errorbarh(mapping = aes(xmin = q25, xmax = q75), size = 2, height = 0) +
  geom_point(size = 2.75) +
  geom_point(data = df_plot %>% group_by(objective) %>% filter(q95 == max(q95)),aes(x = q95), color = NA)+ # include as dummy to fix scales
  facet_grid(rows = vars(intervention), cols = vars(objective), scales = "free_x", 
             labeller = labeller(objective = obj.labs.numeral, intervention = int.labs), switch = "y") +
  scale_color_manual(values = rev(brewer.pal(11,"RdYlBu")[c(1:3,5,9:11)])) + #, labels = c("Lowest Rank", "Highest Rank")
  scale_x_continuous(labels = function(x) format(x, big.mark = ",",scientific = FALSE)) +
  labs(subtitle = "Individual model results", tag = "B")+
  theme_bw(base_size = 12) + 
  theme(axis.text.x=element_text(angle = 45,hjust = 1),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y.left = element_blank(),
        legend.key = element_blank(),
        legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        strip.text = element_text(size = 8),
        plot.margin = unit(c(0.1,0.1,0.1,0.1),"cm"),
        strip.background.y = element_blank()
  )


#### combine into single figure ####
legend_patch = (plot_spacer() / legend / plot_spacer() + plot_layout(heights = c(3.5,4,3.5)))
combo_patch = (pA/pB + plot_layout(heights = c(2,12)))

fig1 =  combo_patch - legend_patch + 
  plot_layout(widths = c(10,1)) 
ggsave(file.path(paste0(output_file,"/Figure1.pdf")), fig1, width = 7,  height = 9, units = "in", scale = 1.3)

