#!/usr/bin/env Rscript 


######################
# Read command-line args
# --------------------

args <- commandArgs(trailingOnly = TRUE)

mmods_viz_tools <- args[1]
input_compare_rounds <- args[2]
output_file <- args[3]


######################
# Preamble
# --------------------

source(mmods_viz_tools)

######################
# Load data
# --------------------

load(input_compare_rounds)

######################
# Plot Figure 2
# --------------------

## days closed round 1 vs round 2
df_compare$id.rev = factor(df_compare$id, levels = rev(levels(df_compare$id)))

## linguistic uncertainty plot days closed
p = ggplot(data = df_compare %>% filter(intervention %in%  c("closed")) %>% filter(objective=="days_closed") %>% filter(id != "aggregate") %>%
              group_by("objective","intervention"), mapping = aes(x = q50, y = id.rev, color = as.factor(round))) + #, color = intervention
  geom_errorbarh(mapping = aes(xmin = q5, xmax = q95),
                 size = 1, height = 0, alpha = 0.5) +
  geom_errorbarh(mapping = aes(xmin=q25,xmax=q75),size=2,height = 0, alpha = 0.5)+
  geom_point(size = 3, alpha = 0.7)+
  geom_point(shape = 1,size = 3,colour = "black")+
  scale_color_manual(values = c("black","#A50026"), breaks = c(1,2), labels = c("Round 1", "Round 2"))+
  scale_x_continuous(breaks=c(50,100,150,184,200,228,244),labels=c(50,100,150,"Start of forecast: 184",200,"Stay at home: 228","State of emergency: 244"))+
  labs(color = "", x = "",y = "")+
  theme_bw() + 
  theme(axis.text.y= element_blank(),
        axis.text.x=element_text(angle=30,hjust=1),
        strip.background.y = element_blank(),
        strip.placement = "outside",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.text.y.right = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = c(.25,.25))
ggsave(file.path(paste0(output_file,"/Figure2.pdf")), width = 3.42,  height = 4, units = "in")
