#!/usr/bin/env Rscript 

######################
# Read command-line args
# --------------------

args <- commandArgs(trailingOnly = TRUE)

mmods_viz_tools <- args[1]
input_county_deathsCDFs_closed_strict <-args[2]
input_county_deathsCDFs_closed_partial <- args[3]
input_county_casesCDFs_closed_strict <-args[4]
input_county_casesCDFs_closed_partial <- args[5]
input_round2_aggCDFs <- args[6]
output_file <- args[7]
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
df_county_cases_cdfs_closed_strict <- read_csv(input_county_casesCDFs_closed_strict, show_col_types = FALSE)
df_county_cases_cdfs_closed_partial <- read_csv(input_county_casesCDFs_closed_partial, show_col_types = FALSE)
df_agg_cdfs <- read_csv(input_round2_aggCDFs, show_col_types = FALSE)

######################
# Visualization
# --------------------
#### Figure S16: cumulative reported deaths/cases, similar counties  ####
## include any closed (Closed = 1) at least one closure remains
p_death_partial <- ggplot(df_county_deaths_cdfs_closed_partial) + 
  geom_ribbon(aes(x = date, ymin = `25%`, ymax =`75%`), fill = "#00798c", alpha = 0.4) + 
  geom_ribbon(aes(x = date, ymin = `5%`, ymax =`25%`), fill = "#00798c", alpha = 0.2) + 
  geom_ribbon(aes(x = date, ymin = `75%`, ymax = `95%`), fill = "#00798c", alpha = 0.2) + 
  geom_line(aes(x=date,y=`50%`),alpha=0.9,  size=0.5, color = "#00798c") +
  ylab("Cumulative deaths") +xlab("Date")
p_case_partial <- ggplot(df_county_cases_cdfs_closed_partial) + 
  geom_ribbon(aes(x = date, ymin = `25%`, ymax =`75%`), fill = "#00798c", alpha = 0.4) + 
  geom_ribbon(aes(x = date, ymin = `5%`, ymax =`25%`), fill = "#00798c", alpha = 0.2) + 
  geom_ribbon(aes(x = date, ymin = `75%`, ymax = `95%`), fill = "#00798c", alpha = 0.2) + 
  geom_line(aes(x=date,y=`50%`),alpha=0.9,  size=0.5, color = "#00798c") +
  ylab("Cumulative cases") +xlab("Date")

## include strictly closed (is_closed_strict = 1) at least one closure has been lifted
p_death_strict <- ggplot(df_county_deaths_cdfs_closed_strict) + 
  geom_ribbon(aes(x = date, ymin = `25%`, ymax =`75%`), fill = "#00798c", alpha = 0.4) + 
  geom_ribbon(aes(x = date, ymin = `5%`, ymax =`25%`), fill = "#00798c", alpha = 0.2) + 
  geom_ribbon(aes(x = date, ymin = `75%`, ymax = `95%`), fill = "#00798c", alpha = 0.2) + 
  geom_line(aes(x=date,y=`50%`),alpha=0.9,  size=0.5, color = "#00798c") +
  ylab("Cumulative deaths") +xlab("Date")
p_case_strict <- ggplot(df_county_cases_cdfs_closed_strict) + 
  geom_ribbon(aes(x = date, ymin = `25%`, ymax =`75%`), fill = "#00798c", alpha = 0.4) + 
  geom_ribbon(aes(x = date, ymin = `5%`, ymax =`25%`), fill = "#00798c", alpha = 0.2) + 
  geom_ribbon(aes(x = date, ymin = `75%`, ymax = `95%`), fill = "#00798c", alpha = 0.2) + 
  geom_line(aes(x=date,y=`50%`),alpha=0.9,  size=0.5, color = "#00798c") +
  ylab("Cumulative cases") +xlab("Date")

p <- grid.arrange(p_death_strict, p_death_partial, p_case_strict, p_case_partial, nrow = 2)

ggsave(file.path(paste0(output_file,"/FigureS16.pdf")), plot = p, width = 8,  height = 7, units = "in")


#### Figure S17: model vs data, cumulative deaths ####

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

## deaths under partial and strict closed vs model deaths closed (orig fig)
p_death <- ggplot(meltdf_compare_deaths, aes(x=variable)) + 
  geom_boxplot(aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95), color=c("#313695","black","black"), stat = "identity") + 
  ylab("Cumulative deaths") +xlab("") + 
  scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +coord_flip() +
  theme_bw() +
  theme(text = element_text(size=font.size),
        #axis.text.x = element_text(angle = 30, hjust = 1),
        strip.background.y = element_blank(),
        #axis.text.y = element_blank(),
        strip.text.y = element_text(angle = 180),
        #axis.ticks.y = element_blank(),
        strip.placement = "outside",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none") +
  annotation_custom(
    ggplotGrob(
      ggplot(meltdf_compare_deaths, aes(x=variable)) + 
        geom_boxplot(aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95), color=c("#313695","black","black"), stat = "identity") + 
        coord_flip(ylim=c(0,300)) + ylab("") + xlab("") +
        scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
        theme_bw() +
        theme(text = element_text(size=font.size),
              #axis.text.x = element_text(angle = 30, hjust = 1),
              strip.background.y = element_blank(),
              #axis.text.y = element_blank(),
              strip.text.y = element_text(angle = 180),
              #axis.ticks.y = element_blank(),
              strip.placement = "outside",
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              legend.position = "none")), 
    ymin = 300, ymax = 1600, xmin = 2, xmax=3.5) 

## cases
county_last_day_cases_closed_partial <- melt(df_county_cases_cdfs_closed_partial[which(df_county_cases_cdfs_closed_partial$date == "2020-11-15"),][,c(which(names(df_county_cases_cdfs_closed_partial) == "0%"):ncol(df_county_cases_cdfs_closed_partial))])
county_last_day_cases_closed_strict <- melt(df_county_cases_cdfs_closed_strict[which(df_county_cases_cdfs_closed_strict$date == "2020-11-15"),][,c(which(names(df_county_cases_cdfs_closed_strict) == "0%"):ncol(df_county_cases_cdfs_closed_strict))])

df_compare_cases <- cbind(
  county_last_day_cases_closed_strict$value,
  county_last_day_cases_closed_partial$value, 
  subset(df_agg_cdfs %>% filter(id == "aggregate") %>% group_by("variable","intervention"), intervention == "closed" & objective == "cumu_infections")[,c("quantile","value")], 
  subset(df_agg_cdfs %>% filter(id == "aggregate") %>% group_by("variable","intervention"), intervention == "5percent" & objective == "cumu_infections")[,"value"], 
  subset(df_agg_cdfs %>% filter(id == "aggregate") %>% group_by("variable","intervention"), intervention == "2weeks" & objective == "cumu_infections")[,"value"], 
  subset(df_agg_cdfs %>% filter(id == "aggregate") %>% group_by("variable","intervention"), intervention == "open" & objective == "cumu_infections")[,"value"]
)

names(df_compare_cases) <- c("Reported cases\n(closed, full orders only)",
                             "Reported cases\n(closed, full and partial orders)",
                             "q",
                             "Modeled cases\n(aggregate, closed intervention)",
                             "Modeled cases\n(aggregate, 5 percent intervention)",
                             "Modeled cases\n(aggregate, 2 weeks intervention)",
                             "Modeled cases\n(aggregate, open intervention)")
meltdf_compare_cases <- melt(df_compare_cases, id.vars = "q", measure.vars = c(1,2,4))
meltdf_compare_cases_all <- melt(df_compare_cases, id.vars = "q")
meltdf_compare_cases$variable = factor(meltdf_compare_cases$variable, levels(meltdf_compare_cases$variable)[c(3,2,1)], ordered=TRUE)
meltdf_compare_cases_all$variable = factor(meltdf_compare_cases_all$variable, levels(meltdf_compare_cases_all$variable)[c(6,5,4,3,2,1)], ordered=TRUE)

meltdf_compare_cases <- meltdf_compare_cases %>% 
  filter(q %in% c(5, 25, 50, 75, 95)) %>%
  mutate(q = paste0("q", q)) %>%
  reshape2::dcast(variable ~ q)

## cases under partial and strict closed vs model deaths closed
p_case <- ggplot(meltdf_compare_cases, aes(x=variable)) + 
  geom_boxplot(aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95), color=c("#313695","black","black"), stat = "identity") + 
  ylab("Cumulative cases") +xlab("") + 
  scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +coord_flip() +
  theme_bw() +
  theme(text = element_text(size=font.size),
        #axis.text.x = element_text(angle = 30, hjust = 1),
        strip.background.y = element_blank(),
        #axis.text.y = element_blank(),
        strip.text.y = element_text(angle = 180),
        #axis.ticks.y = element_blank(),
        strip.placement = "outside",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none") +
  annotation_custom(
    ggplotGrob(
      ggplot(meltdf_compare_cases, aes(x=variable)) + 
        geom_boxplot(aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95), color=c("#313695","black","black"), stat = "identity") + 
        coord_flip(ylim=c(0,30000)) + ylab("") + xlab("") +
        scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
        theme_bw() +
        theme(text = element_text(size=font.size),
              #axis.text.x = element_text(angle = 30, hjust = 1),
              strip.background.y = element_blank(),
              #axis.text.y = element_blank(),
              strip.text.y = element_text(angle = 180),
              #axis.ticks.y = element_blank(),
              strip.placement = "outside",
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              legend.position = "none")), 
    ymin = 8000, ymax = 75000, xmin = 2, xmax=3.5) 

p <- grid.arrange(p_death, p_case, ncol = 1)

ggsave(file.path(paste0(output_file,"/FigureS17.pdf")), plot = p, width = 10,  height = 12, units = "in")

