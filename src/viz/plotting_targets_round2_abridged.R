#!/usr/bin/env Rscript 


######################
# Read command-line args
# --------------------

args <- commandArgs(trailingOnly = TRUE)

mmods_viz_tools <- args[1]
input_round2_rank_file <- args[2]
input_round2_allCDFs <- args[3]
output_file <- args[4]

mmods_viz_tools = "./public_repo/src/viz/mmods_viz_tools.R"
input_round2_rank_file = "./public_repo/data/processed/round2/mmods1_round_2_results_clean.Rdata"
input_round2_allCDFs = "./public_repo/data/processed/round2/team_submissions_incl_agg_round2_allquantiles.csv"
output_file = "./public_repo/output/figures"

input_compare_rounds = "./public_repo/data/processed/compare/compare_rounds.Rdata"
input_compare_aggregate = "./public_repo/data/processed/compare/compare_aggregate.Rdata"
######################
# Preamble
# --------------------

source(mmods_viz_tools)

######################
# Load data
# --------------------

load(input_round2_rank_file)
df_cdfs <- read_csv(input_round2_allCDFs)

load(input_compare_rounds)
load(input_compare_aggregate)

######################
# Visualization
# --------------------

#### Figure 1: aggregate results by scenario, color code plot by rank ####
p <- ggplot(data = df_plot %>% filter(id == "Aggregate") %>% group_by("objective","intervention"), 
            mapping = aes(x = q50, y = intervention.rev, color = as.factor(rank))) +
  geom_errorbarh(mapping = aes(xmin = q5, xmax = q95),size = 1, alpha = 0.7, height = 0) +
  geom_errorbarh(mapping = aes(xmin=q25,xmax=q75),size=2,alpha=0.7,height = 0)+
  geom_point(size = 3)+
  facet_grid(cols=vars(objective),scales="free_x", labeller = labeller(objective = obj.labs.incletters), switch ="y")+
  scale_color_manual(values = rev(brewer.pal(11, "RdYlBu")[c(1,3,9,11)]), breaks = c(1,4), labels = c("Lowest Rank", "Highest Rank"))+
  scale_x_continuous(labels = function(x) format(x, big.mark = ",",scientific = FALSE))+
  scale_y_discrete(labels = rev(int.labs))+
  labs(color = "", x = "",y = "")+
  theme_bw() + theme(axis.text.x=element_text(angle=45,hjust=1),
                     strip.background.y = element_blank(),
                     strip.text.y = element_text(angle = 180),
                     strip.placement = "outside",
                     legend.position = "none")
inset <- ggplot(data = df_plot %>% 
                 filter(id == "Aggregate", objective == "cumu_deaths") %>% 
                 group_by("objective","intervention"), 
               mapping = aes(x = q50, y = intervention.rev, color = as.factor(rank))) +
  geom_errorbarh(mapping = aes(xmin = q5, xmax = q95),size = 1, alpha = 0.7, height = 0) +
  geom_errorbarh(mapping = aes(xmin=q25,xmax=q75),size=2,alpha=0.7,height = 0)+
  geom_point(size = 3)+
  facet_grid(cols=vars(objective),scales="free_x", labeller = labeller(objective = obj.labs), switch ="y")+
  coord_cartesian(xlim=c(0,1000))+
  scale_color_manual(values = rev(brewer.pal(11, "RdYlBu")[c(1,3,9,11)]), breaks = c(1,4), labels = c("Lowest Rank", "Highest Rank"))+
  scale_x_continuous(labels = function(x) format(x, big.mark = ",",scientific = FALSE))+
  scale_y_discrete(labels = rev(int.labs))+
  labs(color = "", x = "",y = "")+
  theme_bw() + theme(axis.text.x=element_text(angle=45,hjust=1, size = 8),
                     axis.text.y = element_blank(),
                     axis.ticks.y = element_blank(),
                     axis.title = element_blank(),
                     strip.background.x = element_blank(),
                     strip.text.x = element_blank(),
                     strip.text.y = element_text(angle = 180),
                     strip.placement = "outside",
                     legend.position = "none",
                     plot.margin=grid::unit(rep(0,4), "mm"))
# add inset to plot
p <- p +
  annotation_custom2(grob=ggplotGrob(inset), data=df_plot %>% 
                       filter(id == "Aggregate", objective == "cumu_deaths") %>% 
                       group_by("objective","intervention"),
                     ymin = 2.25, ymax=Inf, xmin=2000, xmax=4500)
ggsave(file.path(paste0(output_file,"/Figure1.png")), p,width = 10,  height = 3, units = "in", dpi = 300)


#### Figure 2: results by scenario, by team, color code plot by rank ####
p <- ggplot(data = df_plot %>% group_by("objective", "intervention"), 
            mapping = aes(x = q50, y = id.rev, color = as.factor(rank))) + 
  geom_errorbarh(mapping = aes(xmin = q5, xmax = q95), size = 1, height = 0) +
  geom_errorbarh(mapping = aes(xmin = q25, xmax = q75), size = 2, height = 0) +
  geom_point(size = 3) +
  facet_grid(rows = vars(intervention), cols = vars(objective), scales = "free_x", 
      labeller = labeller(objective = obj.labs, intervention = int.labs), switch = "y") +
  scale_color_manual(values = rev(brewer.pal(11,"RdYlBu")[c(1:3,5,9:11)]), 
      breaks = c(1,4), labels = c("Lowest Rank", "Highest Rank")) + 
  scale_x_continuous(labels = function(x) format(x, big.mark = ",",scientific = FALSE)) +
  scale_y_discrete(position = "right") +
  labs(color = "", x = "",y = "") +
  theme_bw(base_size = 14) + 
  theme(axis.text.x=element_text(angle = 45,hjust = 1),
        axis.title.x = element_blank(),
        strip.background.y = element_blank(),
        strip.placement = "outside",
        legend.position = "none")
ggsave(file.path(paste0(output_file,"/Figure2.png")), p, width = 10,  height = 11, units = "in", dpi = 300)


#### Figure 3A: pairwise plots for 2week/5pct interventions, for each objective ####
## code included below Figure S3-S7 (plot uses output from figures S3-S7)

#### Figure 3B: start date of intervention for 2week/5pct interventions  ####
## code included in `plotting_targets_checklist.R`

#### Figure 4: relative median vs. relative IQR for each intervention/objective ####
aggregate = df_plot %>% filter(id == "Aggregate")
df_plot_agg = merge(df_plot, aggregate[,c("intervention", "objective","q25", "q50","q75")], by = c("intervention", "objective"))
df_plot_agg$rel_IQR = (df_plot_agg$q75.x-df_plot_agg$q25.x)/(df_plot_agg$q75.y-df_plot_agg$q25.y)
df_plot_agg$rel_median = (df_plot_agg$q50.x-df_plot_agg$q50.y)/df_plot_agg$q50.y
df_plot_agg$rel_median_plus1 = (df_plot_agg$q50.x-df_plot_agg$q50.y)/df_plot_agg$q50.y+1
df_plot_agg$IQR = df_plot_agg$q75.x-df_plot_agg$q25.x


p <- ggplot(data = df_plot_agg %>% filter(id !="Aggregate", rel_median_plus1>0,rel_IQR>0),
       aes(x = rel_median_plus1, y = rel_IQR, color = as.factor(rank)))+
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
ggsave(file.path(paste0(output_file,"/Figure4.png")), p, width = 12,  height = 9, units = "in", dpi = 300)


#### Figure S2: results by scenario, color code plot by rank, group by team ####
p <- ggplot(data = df_plot %>% group_by("objective","intervention"), mapping = aes(x = q50, y = intervention.rev, color = as.factor(rank))) + #, color = intervention
  geom_errorbarh(mapping = aes(xmin = q5, xmax = q95),
                 size = 1, alpha = 0.7, height = 0) +
  geom_errorbarh(mapping = aes(xmin=q25,xmax=q75),size=2,alpha=0.7,height = 0)+
  geom_point(size = 3)+
  facet_grid(rows=vars(id), cols=vars(objective),scales="free_x", labeller = labeller(intervention = int.labs, objective = obj.labs), switch ="y")+
  scale_color_manual(values = rev(brewer.pal(11, "RdYlBu")[c(1:3,5,9:11)]), breaks = c(1,4), labels = c("Lowest Rank", "Highest Rank"))+
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
ggsave(file.path(paste0(output_file,"/FigureS2.png")), p, width = 12,  height = 14, units = "in", dpi = 300)

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
  ggsave(file.path(paste0(output_file,"/FigureS",which(c("cumu_infections","cumu_deaths","days_closed","peak_hosp","prob_outbreak") == obj)+2,".png")), 
         p, width = 9,  height = 9, units = "in", dpi = 300)
}

#### Figure 3A: pairwise plots for 2week/5pct interventions, for each objective ####
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
ggsave(file.path(paste0(output_file,"/Figure3A.png")), p, width = 6,  height = 6, units = "in", dpi = 300)


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
ggsave(file.path(paste0(output_file,"/FigureS8.png")), width = 8,  height = 8, units = "in", dpi = 300)

#### Figure S9: intervention rank across pairs of objectives ####
rank_by_obj = plot_pairwise_scatter(df_plot, plot_val = "rank", facet_by = "objective", legend_title = "Intervention",
                                    color_by = "intervention", facet_labs = obj.labs, rev_y_axis = 0, jitter_flag = 1)
legend = get_legend(rank_by_obj[[2]])
rank_by_obj = rank_by_obj[[1]]
p = arrangeGrob(grobs = list(rank_by_obj[[1]],  rank_by_obj[[2]], rank_by_obj[[3]],   rank_by_obj[[4]],
                             blankPlot,         rank_by_obj[[5]], rank_by_obj[[6]],   rank_by_obj[[7]],
                             blankPlot,         blankPlot,        rank_by_obj[[8]],   rank_by_obj[[9]],
                             legend,            blankPlot,        blankPlot,          rank_by_obj[[10]]),
                ncol = 4,widths = c(0.01,0.01,0.01,0.01), heights = c(0.01,0.01,0.01,0.01))
ggsave(file.path(paste0(output_file,"/FigureS9.png")),p, width = 12,  height = 12, units = "in", dpi = 300)

#### Figure S10A: days closed round 1 vs round 2 -  ADD  #### ???


#### Figure S10B: days closed round 1 vs round 2 -  ####
font.size=20 # this should possibly be removed or changed to work with the multi-panel
df_compare$id.rev = factor(df_compare$id, levels = rev(levels(df_compare$id)))

## linguistic uncertainty plot days closed
pB = ggplot(data = df_compare %>% filter(intervention %in%  c("closed")) %>% filter(objective=="days_closed") %>% filter(id != "aggregate") %>%
         group_by("objective","intervention"), mapping = aes(x = q50, y = id.rev, color = as.factor(round))) + #, color = intervention
  geom_errorbarh(mapping = aes(xmin = q5, xmax = q95),
                 size = 1, height = 0, alpha = 0.5) +
  geom_errorbarh(mapping = aes(xmin=q25,xmax=q75),size=2,height = 0, alpha = 0.5)+
  geom_point(size = 3, alpha = 0.7)+
  geom_point(shape = 1,size = 3,colour = "black")+
  scale_color_manual(values = c("black","#A50026"), breaks = c(1,2), labels = c("Round 1", "Round 2"))+ #rev(brewer.pal(11,"RdYlBu")[c(1:3,5,9:11)])
  #  scale_x_continuous(labels = function(x) format(x, big.mark = ",",scientific = FALSE))+
  scale_x_continuous(breaks=c(50,100,150,184,200,228,244),labels=c(50,100,150,"Start of forecast: 184",200,"Stay at home: 228","State of emergency: 244"))+
  # scale_y_discrete(position = "left")+
  labs(color = "", x = "",y = "")+  #Days closed #,title="Closed intervention"
  theme_bw(base_size = 14) + 
  theme(text=element_text(size=font.size),
        axis.text.y= element_blank(),
        axis.text.x=element_text(angle=30,hjust=1),
        #     axis.title.x = element_blank(),
        strip.background.y = element_blank(),
        #strip.text.y = element_text(angle = 180),
        strip.placement = "outside",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.text.y.right = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = c(.25,.25))

#p = arrangeGrob(grobs = list(pA, pB), nrow=1)
#ggsave(file.path(paste0(output_file,"/FigureS10.png")), width = 4,  height = 4.5, units = "in", dpi = 300)


#### Figure S11: results by scenario, by team, compare round 1 vs round 2  (WIP) ####
## needed for plotting ##

## TO DO: ids "aggregate" needs to be above "A" (vertical index in each row)

df_compare$intervention = factor(df_compare$intervention, levels = c("open","2weeks","closed"))
df_compare$objective = factor(df_compare$objective, levels = c("cumu_infections","cumu_deaths","peak_hosp"))

# labels for plots
obj.labs = c("Cumulative infections", "Cumulative deaths", "Peak hospitalizations")
names(obj.labs) = c("cumu_infections","cumu_deaths","peak_hosp")
int.labs= c("closed","2-weeks", "open")
names(int.labs) = c("closed","2weeks","open")


df_compare<- df_compare %>% filter(objective %in% c("cumu_infections","cumu_deaths","peak_hosp"))

n_levels = length(levels(df_compare$id))
df_compare$id.rev = factor(df_compare$id, levels = rev(levels(df_compare$id)))
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
ggsave(file.path(paste0(output_file,"/FigureS11.png")), p, width = 10,  height = 11, units = "in", dpi = 300)


#### Figure S12: IQR relative to aggregate, round 1 & round 2  (WIP) ####
#### UPDATED BY EH 8/13/2020 ###
#### UPDATED BY RB 11/26/2020 ###

## bottom and top rows aren't quite aligned on x-axis ##


df_IQRs$intervention.rev = factor(df_IQRs$intervention, levels = rev(levels(df_IQRs$intervention)))
# labels for plots
obj.labs = c("Cumulative infections", "Cumulative deaths", "Peak hospitalizations")
names(obj.labs) = c("cumu_infections","cumu_deaths","peak_hosp")#unique(df_IQRs$objective)
x_limits = c(min(df_IQRs$ratio.IQR), max(df_IQRs$ratio.IQR))
x_limits = ifelse(x_limits ==0, 0.5, x_limits)

r1 <- ggplot(df_IQRs %>% filter(round==1)  %>% group_by(intervention,objective,round),aes(x=ratio.IQR,y=intervention.rev))+
  geom_vline(xintercept=1,linetype="dashed",color="orange",size = 1.5)+
  geom_point(alpha=.2,size=3)+
  facet_wrap(vars(objective), labeller = labeller(objective = obj.labs))+
  labs(y = "Round 1")+
  scale_x_log10()+
  scale_y_discrete(labels = c("open", "2-weeks", "1-percent", "closed"))+
  expand_limits(x = x_limits[2])+
  theme_bw()+
  theme(axis.title.x = element_blank())
r2 <- ggplot(df_IQRs %>% filter(round==2)  %>% group_by(intervention,objective,round),aes(x=ratio.IQR,y=intervention.rev))+
  geom_vline(xintercept=1,linetype="dashed",color="orange",size = 1.5)+
  geom_point(alpha=.2,size=3)+
  facet_wrap(vars(objective), labeller = labeller(objective = obj.labs))+
  labs(x = "IQR Relative to Aggregate", y = "Round 2")+
  scale_x_log10()+
  scale_y_discrete(labels = c("open", "2-weeks", "5-percent", "closed"))+
  expand_limits(x = x_limits[2])+
  theme_bw()+
  theme(strip.background.x = element_blank(),
        strip.text.x = element_blank())
p = arrangeGrob(grobs = list(r1, r2), nrow=2)
ggsave(file.path(paste0(output_file,"/FigureS12.png")),p, width = 6,  height = 4, units = "in", dpi = 300)


#### Figure S13: relative IQR for round 1 vs round 2 (WIP) ####
## needed for plotting ##

df_compare_rounds_subset <- df_compare_rounds %>% filter(ratio.IQR <200) %>% filter(objective %in% c("cumu_infections","cumu_deaths","peak_hosp"))


df_compare_rounds_subset$intervention = factor(df_compare_rounds_subset$intervention, levels = c("open","2weeks","closed"))
# labels for plots
obj.labs = c("Cumulative infections", "Cumulative deaths", "Peak hospitalizations")
names(obj.labs) = c("cumu_infections","cumu_deaths","peak_hosp")#unique(df_compare_rounds$objective)
int.labs= c("closed","2-weeks", "open")
names(int.labs) = c("closed","2weeks","open")

#df_compare_rounds_subset <- df_compare_rounds %>% filter(ratio.IQR <200) %>% filter(intervention %in% c("cumu_infections","cumu_deaths","peak_hosp"))
p=ggplot(df_compare_rounds_subset %>% group_by(intervention,objective),aes(x=ratio.IQR,y=intervention))+
  geom_vline(xintercept=1,linetype="dashed",color="orange",size = 1.5)+
  geom_point(alpha=.2,size=3)+
  scale_x_log10()+
  scale_y_discrete(labels = c("open", "2-weeks","closed"))+
  facet_wrap(vars(objective), labeller = labeller(objective = obj.labs))+
  labs(x = "Relative IQR (Round 2/Round 1)")+
  theme_bw()+
  theme(axis.title.y = element_blank())
ggsave(file.path(paste0(output_file,"/FigureS13.png")),p, width = 6,  height = 4, units = "in", dpi = 300)


#### Figure S14: cumulative reported deaths, similar counties  ####
## code included in `plotting_targets_county.R`

#### Figure S15: model vs data, cumulative deaths ####
## code included in `plotting_targets_county.R`

#### Figure S16: model components included by team ####
## code included in `plotting_targets_checklist.R`

#### Figure S17: data sources used by team ####
## code included in `plotting_targets_checklist.R`

#### Figure S18: projected deaths, susceptibles by team ####
## code included in `plotting_targets_checklist.R`



######################
# Tables
# --------------------

#### Table S2: importation rates
## code included in `plotting_targets_checklist.R`
