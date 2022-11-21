## look at team IQRS For intervention objective pairs relative to the 
## corressponding aggregate IQR

require(gridExtra)

# Save CSV file as Rdata object
# to preserve ordering of factors etc
# that are used in plotting

######################
# Read command-line args
# --------------------

args <- commandArgs(trailingOnly = TRUE)

input_round1_results_csv <- args[1]
input_round2_results_csv <- args[2]
output_rdata_file <- args[3]

######################
# Preamble
# --------------------
library(tidyr)
library(dplyr)


#####################
# Load data
# --------------------

df_all_agg1 <- read.csv(input_round1_results_csv)
df_all_agg1$round = 1
## read in R2 results
df_all_agg2 <- read.csv(input_round2_results_csv)
df_all_agg2$round = 2
df_all_agg = rbind(df_all_agg1,df_all_agg2)

# #####
# ## read in R1 results
# df_all_agg1 <-  read.csv("data/processed/round1/team_submissions_incl_agg_round1.csv")
# df_all_agg1$round = 1
# ## read in R2 results
# df_all_agg2 <-  read.csv("data/processed/round2/team_submissions_incl_agg_round2.csv")
# df_all_agg2$round =2
# df_all_agg=rbind(df_all_agg1,df_all_agg2)


df_all_agg$intervention <- factor(df_all_agg$intervention,levels=c("closed","1percent","5percent","2weeks","open"))
df_all_agg$round <- factor(df_all_agg$round, levels=c(1,2))

## remove prob_outbreak and days_closed, for which the IQR is often 0 and not conducive to IQR ratio analysis
df_all_agg <- df_all_agg %>% mutate(prob_outbreak=NULL,days_closed=NULL)
##df_all <- melt(as.data.frame(df_all), id.vars = c("id","quantile", "round", "intervention"))
##colnames(df_all) <- c("id","quantile","round","intervention","objective","value")


#df_agg=rbind(df_agg_round1,df_agg_round2)
#df_agg$id=as.factor("agg")
#df_agg$intervention <- as.character(df_agg$intervention)
#df_agg$intervention[which(df_agg$intervention %in% c("1percent","5percent"))] = "percent"
#df_agg$intervention <- factor(df_agg$intervention,levels=c("closed","1percent","5percent","2weeks","open"))
#df_agg$round <- factor(df_agg$round, levels=c(1,2))


agg_IQRs <- df_all_agg %>% filter(id=="aggregate") %>%
   group_by(intervention, objective, round) %>% 
    summarise(aggIQR=q75-q25)


model_IQRs <- df_all_agg %>% filter(!id == "aggregate") %>%
  group_by(id, intervention, objective, round) %>% 
#  summarise(modelIQR=value[which(quantile==75)]- value[which(quantile==25)])
  summarise(modelIQR=q75-q25)



df_IQRs <- left_join(model_IQRs,agg_IQRs,by = c("intervention","objective","round"))
df_IQRs$intervention <- factor(df_IQRs$intervention,levels=c("closed","1percent","5percent","2weeks","open"))
df_IQRs$objective <- factor(df_IQRs$objective,levels=c("cumu_infections","cumu_deaths","peak_hosp","prob_outbreak","days_closed"))


df_IQRs <- df_IQRs %>% mutate(ratio.IQR = modelIQR/aggIQR)

## calculate quantities of interest 

## ratio of model IQRs to the aggregate IQR
## TO DO: create table of median.IQR.ratio values
## calculated below, grouped by round 

df_medians <- df_IQRs %>% 
  group_by(intervention, objective, round,aggIQR) %>%
  summarise(median.IQR.ratio=median(ratio.IQR))

# ## ratio of model IQRs - difference between round 2 and round 1
# r.ratios <- df_IQRs %>% #filter(round==1) %>%
#   group_by(intervention, objective, round,aggIQR) #%>%
#   #  mutate(r1.ratio=ratio.IQR)

df_IQRs <- df_IQRs %>%
  filter(objective %in% c("cumu_infections","cumu_deaths","peak_hosp")) %>%
  mutate(modelIQR=NULL,aggIQR=NULL)

save(list = c("df_IQRs"), file = output_rdata_file)
