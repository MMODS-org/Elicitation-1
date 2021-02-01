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

# mmods_viz_tools = "./Elicitation1/public_repo/src/viz/mmods_viz_tools.R"
# input_county_deathsCDFs_closed_strict <- "./Elicitation1/public_repo/data/processed/county/county_deaths_cdf_closed_strict.csv"
# input_county_deathsCDFs_closed_partial <- "./Elicitation1/public_repo/data/processed/county/county_deaths_cdf_closed_partial.csv"
# input_round2_aggCDFs <- "./Elicitation1/public_repo/data/processed/round2/team_submissions_incl_agg_round2_allquantiles.csv"
# output_file = "./Elicitation1/public_repo/output/figures"


######################
# Preamble
# --------------------

source(mmods_viz_tools)

######################
# Load data
# --------------------

df_county_cdfs <- read_csv(input_county_deathsCDFs)
df_county_cdfs_closed_strict <- read_csv(input_county_deathsCDFs_closed_strict)
df_county_cdfs_closed_partial <- read_csv(input_county_deathsCDFs_closed_partial)
df_agg_cdfs <- read_csv(input_round2_aggCDFs)

######################
# Visualization
# --------------------
#### Figure S14: cumulative reported deaths, similar counties  ####
## code for figure included in `plotting_targets_county.R`
## include any closed (Closed = 1) at least one closure remains
p <- ggplot(df_county_cdfs_closed_partial) + 
  geom_ribbon(aes(x = date, ymin = `25%`, ymax =`75%`), fill = "#00798c", alpha = 0.4) + 
  geom_ribbon(aes(x = date, ymin = `5%`, ymax =`25%`), fill = "#00798c", alpha = 0.2) + 
  geom_ribbon(aes(x = date, ymin = `75%`, ymax = `95%`), fill = "#00798c", alpha = 0.2) + 
  geom_line(aes(x=date,y=`50%`),alpha=0.9,  size=0.5, color = "#00798c") +
  ylab("Cumulative deaths") +xlab("Date")

ggsave("FigureS14_closed_partial.png", plot = p, path = output_file, width = 5,  height = 4, units = "in", dpi = 300)

## include strictly closed (is_closed_strict = 1) at least one closure has been lifted
p <- ggplot(df_county_cdfs_closed_strict) + 
  geom_ribbon(aes(x = date, ymin = `25%`, ymax =`75%`), fill = "#00798c", alpha = 0.4) + 
  geom_ribbon(aes(x = date, ymin = `5%`, ymax =`25%`), fill = "#00798c", alpha = 0.2) + 
  geom_ribbon(aes(x = date, ymin = `75%`, ymax = `95%`), fill = "#00798c", alpha = 0.2) + 
  geom_line(aes(x=date,y=`50%`),alpha=0.9,  size=0.5, color = "#00798c") +
  ylab("Cumulative deaths") +xlab("Date")

ggsave("FigureS14_closed_strict.png", plot = p, path = output_file, width = 5,  height = 4, units = "in", dpi = 300)

#### Figure S15: model vs data, cumulative deaths ####
last_day <- "2020-11-15"
county_last_day <- melt(df_county_cdfs[which(df_county_cdfs_closed_partial$date == last_day),][,c(which(names(df_county_cdfs_closed_partial) == "0%"):ncol(df_county_cdfs_closed_partial))])
county_last_day_strict <- melt(df_county_cdfs_closed_strict[which(df_county_cdfs_closed_strict$date == last_day),][,c(which(names(df_county_cdfs_closed_strict) == "0%"):ncol(df_county_cdfs_closed_strict))])

agg_closed_deaths <- subset(df_agg_cdfs %>% filter(id == "aggregate") %>% group_by("variable","intervention"), intervention == "closed" & objective == "cumu_deaths")

df_compare <- cbind(agg_closed_deaths[,c("quantile","value")], county_last_day$value, county_last_day_strict$value)
names(df_compare) <- c("q","Modeled death results (aggregate)","Reported death data (closed counties, incl. partial orders)","Reported death data (closed counties, full orders only)")
meltdf_compare <- melt(df_compare[,c(2,3,4)])

p <- ggplot(meltdf_compare, aes(x=value,y=variable)) + 
  geom_boxplot(color=c("blue","black","black")) + xlab("Cumulative deaths") +ylab("") + scale_x_continuous(limits=c(0,4000)) +
  annotation_custom(
    ggplotGrob(
      ggplot(meltdf_compare, aes(x=value,y=variable)) + 
        geom_boxplot(color=c("blue","black","black")) + coord_cartesian(xlim=c(0,300)) + ylab("") + xlab("")), 
    xmin = 1000, xmax = 4000, ymin = 2, ymax=3.5) 

ggsave("FigureS15.png", plot = p, path = output_file, width = 10,  height = 6, units = "in", dpi = 300)


