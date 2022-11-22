#!/usr/bin/env Rscript 

######################
# Read command-line args
# --------------------

args <- commandArgs(trailingOnly = TRUE)

mmods_viz_tools <- args[1]
input_county_deathsCDFs_closed_strict <-args[2]
input_county_deathsCDFs_closed_partial <- args[3]
input_round2_aggCDFs <- args[4]
output_file <- args[5]
font.size <- 12

######################
# Preamble
# --------------------

source(mmods_viz_tools)

######################
# Load data
# --------------------

df_county_deaths_cdfs_closed_strict <- read_csv(input_county_deathsCDFs_closed_strict, show_col_types = FALSE)
df_county_deaths_cdfs_closed_partial <- read_csv(input_county_deathsCDFs_closed_partial, show_col_types = FALSE)
df_agg_cdfs <- read_csv(input_round2_aggCDFs, show_col_types = FALSE)

## deaths
county_last_day_deaths_closed_partial <- melt(df_county_deaths_cdfs_closed_partial[which(df_county_deaths_cdfs_closed_partial$date == "2020-11-15"),][,c(which(names(df_county_deaths_cdfs_closed_partial) == "0%"):ncol(df_county_deaths_cdfs_closed_partial))])
county_last_day_deaths_closed_strict <- melt(df_county_deaths_cdfs_closed_strict[which(df_county_deaths_cdfs_closed_strict$date == "2020-11-15"),][,c(which(names(df_county_deaths_cdfs_closed_strict) == "0%"):ncol(df_county_deaths_cdfs_closed_strict))])

df_compare_deaths <- cbind(
  county_last_day_deaths_closed_strict$value,
  county_last_day_deaths_closed_partial$value, 
  subset(df_agg_cdfs %>% filter(id == "aggregate") %>% group_by("variable","intervention"), intervention == "closed" & objective == "cumu_deaths")[,c("quantile", "value")], 
  subset(df_agg_cdfs %>% filter(id == "aggregate") %>% group_by("variable","intervention"), intervention == "5percent" & objective == "cumu_deaths")[,"value"], 
  subset(df_agg_cdfs %>% filter(id == "aggregate") %>% group_by("variable","intervention"), intervention == "2weeks" & objective == "cumu_deaths")[,"value"], 
  subset(df_agg_cdfs %>% filter(id == "aggregate") %>% group_by("variable","intervention"), intervention == "open" & objective == "cumu_deaths")[,"value"]
)

names(df_compare_deaths) <- c("Reported deaths\n(closed, full orders only)",
                              "Reported deaths\n(closed, full and partial orders)",
                              "q",
                              "Modeled deaths\n(aggregate, closed intervention)",
                              "Modeled deaths\n(aggregate, 5 percent intervention)",
                              "Modeled deaths\n(aggregate, 2 weeks intervention)",
                              "Modeled deaths\n(aggregate, open intervention)")
meltdf_compare_deaths <- melt(df_compare_deaths, id.vars = "q", measure.vars = c(1,2,4))
meltdf_compare_deaths_all <- melt(df_compare_deaths, id.vars = "q")
meltdf_compare_deaths$variable = factor(meltdf_compare_deaths$variable, levels(meltdf_compare_deaths$variable)[c(3,2,1)], ordered=TRUE)
meltdf_compare_deaths_all$variable = factor(meltdf_compare_deaths_all$variable, levels=levels(meltdf_compare_deaths_all$variable)[c(6,5,4,3,2,1)], ordered=TRUE)

meltdf_compare_deaths <- meltdf_compare_deaths %>% 
  filter(q %in% c(5, 25, 50, 75, 95)) %>%
  mutate(q = paste0("q", q)) %>%
  reshape2::dcast(variable ~ q)


## just one definition of closed
p <- ggplot(meltdf_compare_deaths %>% filter(variable != "Reported deaths\n(closed, full orders only)"), aes(x=variable)) + 
  geom_boxplot(aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95), color=c("#313695","black"), stat = "identity") + 
  ylab("Cumulative deaths") +xlab("") + 
  scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +coord_flip() +
  theme_bw() +
  theme(text = element_text(size=font.size),
        strip.background.y = element_blank(),
        strip.text.y = element_text(angle = 180),
        strip.placement = "outside",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none") +
  annotation_custom(
    ggplotGrob(
      ggplot(meltdf_compare_deaths %>% filter(variable != "Reported deaths\n(closed, full orders only)"), aes(x=variable)) + 
        geom_boxplot(aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95), color=c("#313695","black"), stat = "identity") + 
        coord_flip(ylim=c(0,300)) + ylab("") + xlab("") +
        scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
        theme_bw() +
        theme(text = element_text(size=font.size),
              strip.background.y = element_blank(),
              strip.text.y = element_text(angle = 180),
              strip.placement = "outside",
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              legend.position = "none")), 
    ymin = 235, ymax = 1600, xmin = 1, xmax=2.5) 

ggsave(file.path(paste0(output_file,"/Figure3.pdf")), plot = p, width = 3.42,  height = 1.8, units = "in", scale = 2.4)

