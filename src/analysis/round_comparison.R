## Round comparison results


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
library('tidyr')
library('dplyr')
require(readr)
#source(mmods_analysis_tools)


#####################
# Load data
# --------------------

df_all_agg1 <- read.csv(input_round1_results_csv)
df_all_agg1$round = 1
## read in R2 results
df_all_agg2 <- read.csv(input_round2_results_csv)
df_all_agg2$round = 2
df_all_agg = rbind(df_all_agg1,df_all_agg2)

df_all_agg$id=as.factor(df_all_agg$id)

#####################
# Analysis
# --------------------

## create data frame with only teams having comparable submissions for both rounds
df_compare_agg = df_all_agg[which(df_all_agg$id %in% intersect(unique(df_all_agg1$id),unique(df_all_agg2$id))),]
df_compare_agg$round=as.factor(df_compare_agg$round)
df_compare_agg$round_id=paste0(df_compare_agg$id,"-R",df_compare_agg$round)
df_compare_agg$intervention=as.factor(df_compare_agg$intervention)
df_compare_agg$objective=as.factor(df_compare_agg$objective)


## select targets of interest for comparison between
## round 1 and round 2
compare.targets=c("cumu_infections","cumu_deaths","peak_hosp","days_closed")
df_compare <- df_compare_agg %>% filter(objective %in% compare.targets) %>% filter(intervention %in% c("closed","2weeks","open")) 
  
df_compare <- df_compare %>% mutate(lengthIQR = q75-q25,
                                    length90 = q95-q5)

df_compare_rounds <- df_compare %>% group_by(intervention,objective,id) %>% 
  transmute(diff.IQR = diff(lengthIQR),
            diff.90 = diff(length90),
            ratio.IQR = lengthIQR[which(round==2)]/lengthIQR[which(round==1)]) %>% unique()

#write_csv(df_compare_rounds, path = output_file)

save(list = c("df_compare","df_compare","df_compare_rounds"), file = output_rdata_file)

#df_compare_rounds <- df_compare_rounds %>% filter(objective != "days_closed")

#df_compare_rounds %>% filter(id=="aggregate")




#df_compare_rounds <- df_compare_rounds %>% group_by(intervention,objective) %>% #unique() #filter(objective=="cumu_infections")


##df_compare_targets <- df_compare %>%
#  filter(intervention %in% c("closed","2weeks","open")) %>%
#  filter(objective %in% targets )

# ggplot(df_compare_rounds %>% filter(!id %in% "aggregate") %>% 
#          group_by(intervention,objective),aes(x=ratio.IQR,y=intervention))+
#   geom_boxplot()+
#   geom_vline(xintercept=1,linetype="dashed",color="orange",size = 1.5)+
#   scale_x_log10()+
#   facet_wrap(vars(objective))

#write.csv(df_compare_rounds,file="round_differences.csv")






