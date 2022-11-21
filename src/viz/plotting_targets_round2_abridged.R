#!/usr/bin/env Rscript 


######################
# Read command-line args
# --------------------

args <- commandArgs(trailingOnly = TRUE)

mmods_viz_tools <- args[1]
input_round2_rank_file <- args[2]
input_round2_allCDFs <- args[3]
input_round1_allCDFs <- args[4]
input_compare_rounds <- args[5]
input_compare_aggregate <- args[6]
output_file <- args[7]


######################
# Preamble
# --------------------

source(mmods_viz_tools)

######################
# Load data
# --------------------

load(input_round2_rank_file)
df_cdfs <- read_csv(input_round2_allCDFs, show_col_types = FALSE)
df_cdfs_R1 <- read_csv(input_round1_allCDFs, show_col_types = FALSE)

load(input_compare_rounds)
load(input_compare_aggregate)

######################
# Visualization
# --------------------

#### Figure S2: results by scenario, color code plot by rank, group by team ####
p <- ggplot(data = df_plot %>% group_by("objective","intervention"), 
            mapping = aes(x = q50, y = intervention.rev, color = as.factor(intervention_rank))) + #, color = intervention
  geom_errorbarh(mapping = aes(xmin = q5, xmax = q95),
                 size = 1, alpha = 0.7, height = 0) +
  geom_errorbarh(mapping = aes(xmin=q25,xmax=q75),size=2,alpha=0.7,height = 0)+
  geom_point(size = 3)+
  facet_grid(rows=vars(id), cols=vars(objective),scales="free_x", labeller = labeller(intervention = int.labs, objective = obj.labs), switch ="y")+
  scale_color_manual(values = rev(brewer.pal(11, "RdYlBu")[c(1:3,5,9:11)]))+
  scale_x_continuous(labels = function(x) format(x, big.mark = ",",scientific = FALSE))+
  scale_y_discrete(position = "right",labels = rev(int.labs))+
  #scale_colour_brewer(palette = "RdYlBu", length = 4) 
  labs(color = "", x = "",y = "")+
  theme_bw(base_size = 14) + 
  theme(axis.text.x=element_text(angle=45,hjust=1),
        strip.background.y = element_blank(),
        strip.text.y = element_text(angle = 180),
        strip.placement = "outside",
        legend.position = "none")
ggsave(file.path(paste0(output_file,"/FigureS2.pdf")), p, width = 12,  height = 14, units = "in")

#### Figure S3-S7: pariwise comparision for interventions, one plot per objective ####
nextplot = list()
for(obj in c("cumu_infections","cumu_deaths","days_closed","peak_hosp","prob_outbreak")){
  print(obj)
  abs_val_by_int = plot_pairwise_ranges(df_plot[df_plot$objective == obj,], plot_val = "q50", 
                                        facet_by = "intervention", facet_labs = int.labs,flip_y_axis_text = 1,
                                        range_lower = "q25", range_upper = "q75")
  ## pull out 2wk/5pct plot (for next fig)
  nextplot[[obj]] = abs_val_by_int[[4]]
  p = arrangeGrob(grobs = list(abs_val_by_int[[1]],  abs_val_by_int[[2]], abs_val_by_int[[3]], 
                               blankPlot,            abs_val_by_int[[4]], abs_val_by_int[[5]], 
                               blankPlot,            blankPlot,           abs_val_by_int[[6]]),
                  ncol = 3,widths = c(0.01,0.01,0.01), heights = c(0.01,0.01,0.01),
                  top = obj.labs[obj])
  ggsave(file.path(paste0(output_file,"/FigureS",which(c("cumu_infections","cumu_deaths","days_closed","peak_hosp","prob_outbreak") == obj)+2,".pdf")), 
         p, width = 9,  height = 9, units = "in")
}

#### Figure S8: team and aggregate CDFS, for each intervention/objective ####
# manually omit ID 14 (point estimate entered only in Q50) - for now
df_cdfs$omit_flag = ifelse(df_cdfs$objective == "prob_outbreak"&
                             df_cdfs$id == "M",1,0)
df_cdfs$intervention <- factor(df_cdfs$intervention, levels = c("closed","5percent","2weeks","open"), ordered = TRUE)
df_cdfs$intervention.rev <- factor(df_cdfs$intervention, levels = rev(c("closed","5percent","2weeks","open")), ordered = TRUE)
df_cdfs$objective <- factor(df_cdfs$objective, levels = obj.order, ordered = TRUE)

ggplot(df_plot %>% filter(id == "Aggregate"))+
  geom_line(data = df_cdfs %>% filter(id != "aggregate", omit_flag == 0), aes(x = value, y = quantile,color = as.factor(id)), size =0.7)+
  geom_line(data = df_cdfs %>% filter(id == "aggregate"),
            aes(x = value, y = quantile),size = 1.5)+
  geom_errorbarh(mapping = aes(xmin = q5, xmax = q95, y = 50),
                 size = 1.4, height = 0) +
  geom_errorbarh(mapping = aes(xmin=q25,xmax=q75, y =50),size=3,height = 0)+
  geom_point(aes(x = q50, y = 50),size = 5, color = "red")+
  facet_grid(rows = vars(intervention), cols = vars(objective), 
             scales = "free", labeller = labeller(objective = obj.labs, intervention = int.labs), switch = "y")+
  labs(x="", y="CDF")+
  theme_bw()+
  theme(axis.title = element_blank(),
        strip.background.y = element_blank(),
        strip.placement = "outside",
        legend.position = "none")
ggsave(file.path(paste0(output_file,"/FigureS8.pdf")), width = 8,  height = 8, units = "in")

#### Figure S9: intervention rank across pairs of objectives ####
rank_by_obj = plot_pairwise_scatter(df_plot, plot_val = "intervention_rank", facet_by = "objective", legend_title = "Intervention",
                                    color_by = "intervention", facet_labs = obj.labs, rev_y_axis = 0, jitter_flag = 1)
legend = get_legend(rank_by_obj[[2]])
rank_by_obj = rank_by_obj[[1]]
p = arrangeGrob(grobs = list(rank_by_obj[[1]],  rank_by_obj[[2]], rank_by_obj[[3]],   rank_by_obj[[4]],
                             blankPlot,         rank_by_obj[[5]], rank_by_obj[[6]],   rank_by_obj[[7]],
                             blankPlot,         blankPlot,        rank_by_obj[[8]],   rank_by_obj[[9]],
                             legend,            blankPlot,        blankPlot,          rank_by_obj[[10]]),
                ncol = 4,widths = c(0.01,0.01,0.01,0.01), heights = c(0.01,0.01,0.01,0.01))
ggsave(file.path(paste0(output_file,"/FigureS9.pdf")),p, width = 12,  height = 12, units = "in")

#### Figure S10: relative median vs. relative IQR for each intervention/objective ####
aggregate = df_plot %>% filter(id == "Aggregate")
df_plot_agg = merge(df_plot, aggregate[,c("intervention", "objective","q25", "q50","q75")], by = c("intervention", "objective"))
df_plot_agg$rel_IQR = (df_plot_agg$q75.x-df_plot_agg$q25.x)/(df_plot_agg$q75.y-df_plot_agg$q25.y)
df_plot_agg$rel_median = (df_plot_agg$q50.x-df_plot_agg$q50.y)/df_plot_agg$q50.y
df_plot_agg$rel_median_plus1 = (df_plot_agg$q50.x-df_plot_agg$q50.y)/df_plot_agg$q50.y+1
df_plot_agg$IQR = df_plot_agg$q75.x-df_plot_agg$q25.x


p <- ggplot(data = df_plot_agg %>% filter(id !="Aggregate", rel_median_plus1>0,rel_IQR>0),
            aes(x = rel_median_plus1, y = rel_IQR, color = as.factor(intervention_rank)))+
  geom_vline(aes(xintercept = 1), linetype = "dashed")+
  geom_hline(aes(yintercept = 1), linetype = "dashed")+
  geom_point(size = 3)+
  facet_grid(cols = vars(objective), rows = vars(intervention),labeller = labeller(objective = obj.labs, intervention = int.labs))+
  labs(x = "Relative Median", y = "RelativeIQR")+
  scale_color_manual(values = rev(brewer.pal(11, "RdYlBu")[c(1:3,5,9:11)]))+
  scale_x_log10(expand = c(0,0))+
  scale_y_log10(expand = c(0,0))+
  theme_bw()+
  theme(legend.position = "none")
ggsave(file.path(paste0(output_file,"/FigureS10.pdf")), p, width = 12,  height = 9, units = "in")


#### Figure S11: presentation slide shared with teams ####


#### Figure S12: comparison of 2week/5pct interventions ####
# Panel A 
p = grid.arrange(grobs = list(nextplot[["cumu_infections"]]+
                                ggtitle("i. Cumulative infections")+
                                scale_x_continuous(limits = c(0,max((df_plot %>% group_by(id) %>% filter(objective == "cumu_infections", intervention %in% c("2weeks", "5percent")))[,"q75"])),
                                                   labels = function(x) format(x, big.mark = ",",scientific = FALSE))+
                                scale_y_continuous(limits = c(0,max((df_plot %>% group_by(id) %>% filter(objective == "cumu_infections", intervention %in% c("2weeks", "5percent")))[,"q75"])),
                                                   labels = function(x) format(x, big.mark = ",",scientific = FALSE))+
                                theme_classic(base_size = 10)+
                                theme(axis.title = element_blank(),
                                      axis.text.y = element_text(angle = 90, hjust = 0.5),
                                      panel.border = element_rect(color = "black", fill = NA),
                                      plot.title = element_text(size = 11)),
                              nextplot[["cumu_deaths"]]+
                                ggtitle("ii. Cumulative deaths")+
                                scale_x_continuous(limits = c(0,max((df_plot %>% group_by(id) %>% filter(objective == "cumu_deaths", intervention %in% c("2weeks", "5percent")))[,"q75"])),
                                                   labels = function(x) format(x, big.mark = ",",scientific = FALSE))+
                                scale_y_continuous(limits = c(0,max((df_plot %>% group_by(id) %>% filter(objective == "cumu_deaths", intervention %in% c("2weeks", "5percent")))[,"q75"])),
                                                   labels = function(x) format(x, big.mark = ",",scientific = FALSE))+
                                theme_classic(base_size = 10)+
                                theme(axis.title = element_blank(),
                                      axis.text.y = element_text(angle = 90, hjust = 0.5),
                                      panel.border = element_rect(color = "black", fill = NA),
                                      plot.title = element_text(size = 11)),
                              nextplot[["peak_hosp"]]+
                                ggtitle("iii. Peak Hospitalizations")+
                                scale_x_continuous(limits = c(0,max((df_plot %>% group_by(id) %>% filter(objective == "peak_hosp", intervention %in% c("2weeks", "5percent")))[,"q75"])),
                                                   labels = function(x) format(x, big.mark = ",",scientific = FALSE))+
                                scale_y_continuous(limits = c(0,max((df_plot %>% group_by(id) %>% filter(objective == "peak_hosp", intervention %in% c("2weeks", "5percent")))[,"q75"])),
                                                   labels = function(x) format(x, big.mark = ",",scientific = FALSE))+
                                theme_classic(base_size = 10)+
                                theme(axis.title = element_blank(),
                                      axis.text.y = element_text(angle = 90, hjust = 0.5),
                                      panel.border = element_rect(color = "black", fill = NA),
                                      plot.title = element_text(size = 11)),
                              nextplot[["days_closed"]]+
                                ggtitle("iv. Days closed")+
                                scale_x_continuous(limits = c(0,max((df_plot %>% group_by(id) %>% filter(objective == "days_closed", intervention %in% c("2weeks", "5percent")))[,"q75"])),
                                                   labels = function(x) format(x, big.mark = ",",scientific = FALSE))+
                                scale_y_continuous(limits = c(0,max((df_plot %>% group_by(id) %>% filter(objective == "days_closed", intervention %in% c("2weeks", "5percent")))[,"q75"])),
                                                   labels = function(x) format(x, big.mark = ",",scientific = FALSE))+
                                theme_classic(base_size = 10)+
                                theme(axis.title = element_blank(),
                                      axis.text.y = element_text(angle = 90, hjust = 0.5),
                                      panel.border = element_rect(color = "black", fill = NA),
                                      plot.title = element_text(size = 11))),nrow = 2, 
                 bottom = "2-weeks", left = "5-percent")
ggsave(file.path(paste0(output_file,"/FigureS12A.pdf")), p, width = 6,  height = 6, units = "in")

# Panel B
## code included in `plotting_targets_checklist.R` 


#### Figure S13: results by scenario, by team, compare round 1 vs round 2 ####
df_compare$intervention = factor(df_compare$intervention, levels = c("open","2weeks","closed"))
df_compare$objective = factor(df_compare$objective, levels = c("cumu_infections","cumu_deaths","peak_hosp"))

# labels for plots
obj.labs = c("Cumulative infections", "Cumulative deaths", "Peak hospitalizations")
names(obj.labs) = c("cumu_infections","cumu_deaths","peak_hosp")
int.labs= c("closed","2-weeks", "open")
names(int.labs) = c("closed","2weeks","open")


df_compare<- df_compare %>% filter(objective %in% c("cumu_infections","cumu_deaths","peak_hosp"))

n_levels = length(levels(df_compare$id))
df_compare$id.rev = factor(df_compare$id, levels = rev(c("aggregate", LETTERS[1:6], "G.1", "G.2", LETTERS[8:16])))
df_compare$intervention.rev = factor(df_compare$intervention, levels = rev(levels(df_compare$intervention)))

p = ggplot(data = df_compare  %>% group_by("objective","intervention"), mapping = aes(x = q50, y = id.rev, color = as.factor(round))) + #, color = intervention
  geom_errorbarh(mapping = aes(xmin = q5, xmax = q95),
                 size = 1, height = 0, alpha = 0.5) +
  geom_errorbarh(mapping = aes(xmin=q25,xmax=q75),size=2,height = 0, alpha = 0.5)+
  geom_point(size = 3, alpha = 0.5)+
  facet_grid(rows=vars(intervention.rev), cols=vars(objective),scales="free_x", labeller = labeller(objective = obj.labs, intervention = int.labs), switch ="y")+
  #scale_color_manual(values = rev(brewer.pal(11,"RdYlBu")[c(1:3,5,9:11)]), breaks = c(1,4), labels = c("Lowest Rank", "Highest Rank"))+
  scale_x_continuous(labels = function(x) format(x, big.mark = ",",scientific = FALSE))+
  scale_y_discrete(position = "right")+
  labs(color = "", x = "",y = "")+
  theme_bw(base_size = 14) + 
  theme(axis.text.x=element_text(angle=45,hjust=1),
        axis.title.x = element_blank(),
        strip.background.y = element_blank(),
        #strip.text.y = element_text(angle = 180),
        strip.placement = "outside",
        legend.position = "none")
ggsave(file.path(paste0(output_file,"/FigureS13.pdf")), p, width = 10,  height = 11, units = "in")


#### Figure S14: IQR relative to aggregate, round 1 & round 2  (WIP) ####

# compare IQR of individual models to aggregate
df_IQRs <- bind_rows(df_cdfs_R1, 
                     df_cdfs %>% 
                       select(round, quantile, intervention, id, objective, value)) %>%
  filter(quantile %in% c(25, 75)) %>%
  group_by(round, intervention, id, objective) %>%
  summarise(IQR = value[quantile == 75] - value[quantile == 25]) %>%
  group_by(round, intervention, objective) %>%
  mutate(ratio.IQR = IQR/IQR[id == "aggregate"]) %>% 
  filter(id != "aggregate", 
         objective %in% c("cumu_deaths", "cumu_infections", "peak_hosp"))
df_IQRs$objective = factor(df_IQRs$objective, levels = c("cumu_infections", "cumu_deaths", "peak_hosp"))
df_IQRs$intervention = factor(df_IQRs$intervention, levels = rev(c("open","2weeks","1percent", "5percent","closed")))
df_IQRs$intervention.rev =  factor(df_IQRs$intervention, levels = rev(levels(df_IQRs$intervention)))

## bottom and top rows aren't quite aligned on x-axis ##
# labels for plots
x_limits = c(min(df_IQRs$ratio.IQR), max(df_IQRs$ratio.IQR))
x_limits = ifelse(x_limits ==0, 0.5, x_limits)

r1 <- ggplot(df_IQRs %>% filter(round==1)  %>% group_by(intervention,objective,round),
             aes(x=ratio.IQR,y=intervention.rev))+
  geom_vline(xintercept=1,linetype="dashed",color="orange",size = 1.5)+
  geom_point(alpha=.2,size=3)+
  facet_wrap(vars(objective), labeller = labeller(objective = obj.labs))+
  labs(y = "Round 1")+
  scale_x_log10(expand = c(0,0))+
  scale_y_discrete(labels = c("open", "2-weeks", "1-percent", "closed"))+
  expand_limits(x = x_limits[2])+
  theme_bw()+
  theme(axis.title.x = element_blank())
r2 <- ggplot(df_IQRs %>% filter(round==2)  %>% group_by(intervention,objective,round),
             aes(x=ratio.IQR,y=intervention.rev))+
  geom_vline(xintercept=1,linetype="dashed",color="orange",size = 1.5)+
  geom_point(alpha=.2,size=3)+
  facet_wrap(vars(objective), labeller = labeller(objective = obj.labs))+
  labs(x = "IQR Relative to Aggregate", y = "Round 2")+
  scale_x_log10(expand = c(0,0))+
  scale_y_discrete(labels = c("open", "2-weeks", "5-percent", "closed"))+
  expand_limits(x = x_limits[2])+
  theme_bw()+
  theme(strip.background.x = element_blank(),
        strip.text.x = element_blank())
p = arrangeGrob(grobs = list(r1, r2), nrow=2)
ggsave(file.path(paste0(output_file,"/FigureS14.pdf")),p, width = 6,  height = 4, units = "in")


#### Figure S15: relative IQR for round 1 vs round 2 (WIP) ####
df_compare_rounds_subset <- df_compare_rounds %>% 
  filter(ratio.IQR <200) %>% 
  filter(objective %in% c("cumu_infections","cumu_deaths","peak_hosp"))
df_compare_rounds_subset$objective = factor(df_compare_rounds_subset$objective, 
                                            levels = c("cumu_infections","cumu_deaths","peak_hosp"))
df_compare_rounds_subset$intervention = factor(df_compare_rounds_subset$intervention, 
                                               levels = c("open","2weeks","closed"))

p=ggplot(df_compare_rounds_subset %>% group_by(intervention,objective),aes(x=ratio.IQR,y=intervention))+
  geom_vline(xintercept=1,linetype="dashed",color="orange",size = 1.5)+
  geom_point(alpha=.2,size=3)+
  scale_x_log10()+
  scale_y_discrete(labels = c("open", "2-weeks","closed"))+
  facet_wrap(vars(objective), labeller = labeller(objective = obj.labs))+
  labs(x = "Relative IQR (Round 2/Round 1)")+
  theme_bw()+
  theme(axis.title.y = element_blank())
ggsave(file.path(paste0(output_file,"/FigureS15.pdf")),p, width = 6,  height = 2, units = "in")


#### Figure S16: cumulative reported deaths, similar counties  ####
## code included in `plotting_targets_county.R`

#### Figure S17: model vs data, cumulative deaths ####
## code included in `plotting_targets_county.R`

#### Figure S18: model components included by team ####
## code included in `plotting_targets_checklist.R`

#### Figure S19: data sources used by team ####
## code included in `plotting_targets_checklist.R`

#### Figure S20: projected deaths, susceptibles by team ####
## code included in `plotting_targets_checklist.R`



######################
# Tables
# --------------------

#### Table S2: importation rates
## code included in `plotting_targets_checklist.R`
