#!/usr/bin/env Rscript 


######################
# Read command-line args
# --------------------

args <- commandArgs(trailingOnly = TRUE)

mmods_viz_tools <- args[1]
input_checklist_data <- args[2]
input_pars <- args[3]
output_file <- args[4]

######################
# Preamble
# --------------------

source(mmods_viz_tools)

######################
# Load data
# --------------------
df_form <- read_csv(input_checklist_data, show_col_types = FALSE) 
df_pars <- read_csv(input_pars, show_col_types = FALSE)


######################
# Visualization
# --------------------
# 
# #### Figure S12B: start date of intervention for 2week/5pct interventions  ####
# Summarize by grouping variables
df_plot_dates <- df_pars %>%
  subset((param %in% c("param_dist_dates_2wk_intervention","param_dist_dates_5percent_intervention","param_initial_number_susc_individuals"))) %>% 
  subset(param != "param_initial_number_susc_individuals") %>%
  group_by(letter_id, intervention,param) %>% 
  summarise(
    q5 = value[which(quantile==5)],
    q25 = value[which(quantile==25)],
    q50.test=value[which(quantile==50)],
    q75 = value[which(quantile==75)],
    q95 = value[which(quantile==95)],
    id = unique(letter_id)
  )

#reverse the levels of the id
df_plot_dates$id = factor(as.factor(df_plot_dates$id), levels = rev(levels(as.factor(df_plot_dates$id))))
param.labs = c("2-weeks intervention start day", "5-percent intervention start day")
names(param.labs) = unique(df_plot_dates$param)

#set intervention order
df_plot_dates$intervention = factor(df_plot_dates$intervention, levels=c("closed","5percent", "2weeks", "open"))

#rename levels
levels(df_plot_dates$intervention) <- c("closed","5-percent","2-weeks","open")
df_plot_dates = transform(df_plot_dates, q5 = as.numeric(q5), q25 = as.numeric(q25),q50.test = as.numeric(q50.test),q75 = as.numeric(q75),q95 = as.numeric(q95))

## separate interventions to allow for staggered plotting on the y-axis
df_5p <- df_plot_dates[df_plot_dates$intervention == '5-percent',]
df_2w <- df_plot_dates[df_plot_dates$intervention == '2-weeks',]

## setup plot
o <- 0.09

p <- ggplot(df_plot_dates, aes(x = q50.test, y = id, color = param)) + 
  geom_blank() + 
  ylab("Model ID") + xlab("Intervention start day (number of days since May 15, 2020)") +
  scale_colour_manual(name = "Intervention", breaks = df_plot_dates$param, values = c("#797979","#000000"), labels=df_plot_dates$intervention) +
  coord_cartesian(xlim = c(0,184)) +
  theme_bw() + theme(axis.text.y = element_text(size = 11),
                     axis.text.x=element_text(size=11,hjust=0.5),
                     legend.position = "none",
                     strip.text = element_text(size = 11)) +
  geom_point(data = df_5p,mapping = aes(x = q50.test, y = as.numeric(id)-o), size=2) +
  geom_errorbarh(data = df_5p, mapping = aes(xmin = q5, xmax = q95, y = as.numeric(id)-o), 
                 size = 0.5, alpha = 0.75, height = 0) +
  geom_errorbarh(data = df_5p, mapping = aes(xmin = q25, xmax = q75, y = as.numeric(id)-o), 
                 size = 1, alpha = 0.75, height = 0) +
  geom_point(data = df_2w,mapping = aes(x = q50.test, y = as.numeric(id)+o),size=2) +
  geom_errorbarh(data = df_2w, mapping = aes(xmin = q5, xmax = q95, y = as.numeric(id)+o), 
                 size = 0.5, alpha = 0.75, height = 0) +
  geom_errorbarh(data = df_2w, mapping = aes(xmin = q25, xmax = q75, y = as.numeric(id)+o), 
                 size = 1, alpha = 0.75, height = 0)

ggsave("FigureS12B.pdf", plot = p, path = output_file, width = 7,  height = 6, units = "in")

#### Figure S18: model components included by team ####
components <- df_form %>% 
  select(letter_id, components_susceptibility:components_testing) %>%
  melt("letter_id")
components$variable = substr(as.character(components$variable), 12, nchar(as.character(components$variable)))
components$variable = factor(components$variable, 
                          levels = c("susceptibility","symptoms","disease_severity","time_until_infectious",
                                     "time_until_symptomatic","reinfection","hospitilization","icu",
                                     "recovery","death","testing" ))

p <- ggplot(data = components, aes(x = variable, y = letter_id, fill = value)) +
  geom_tile(color="white") +
  theme(axis.text = element_text(size=10), 
        axis.title = element_text(size=14), 
        axis.text.x = element_text(angle = 45, hjust=1))+
  ylab("Model ID")+xlab("Model component") + 
  scale_fill_manual(values=c("#CCCCCC", "#696969","#000000","#FFFFFF"), 
                    labels = c("Explicitly included", 
                               "Explicitly included, structured by age", 
                               "Explicitly included, structured by age AND sex/gender")) + 
  labs(fill="Response") +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(limits = rev(levels(as.factor(df_form$letter_id))), 
                   expand = c(0,0))

ggsave("FigureS18.pdf", plot = p, path = output_file, width = 9,  height = 5, units = "in")


#### Figure S17: data sources used by team ####
data.sources<-c("Cases",'Deaths',"Mobility","Demographic","Testing","Stay at home") #excluded 'none'

data.mat<-matrix(NA, nrow=nrow(df_form), ncol=length(data.sources))
for(i in 1:nrow(df_form)){
  for(j in 1:length(data.sources)){
    if(grepl(data.sources[j],df_form$data_data_sources_used[i])){
      data.mat[i,j]= "Used"
    } else (data.mat[i,j]= "Did not use")
  }
}

rownames(data.mat) <- df_form$letter_id
colnames(data.mat) <- data.sources

melted.data.mat <- melt(data.mat)

p <- ggplot(melted.data.mat, aes(y=as.character(Var1), x=Var2, fill=value)) + 
  geom_tile(color="white") + 
  theme(axis.text = element_text(size=12), axis.title = element_text(size=14), axis.text.x = element_text(angle = 45, hjust=1)) + 
  ylab("Model ID")+xlab("Data Sources") + 
  scale_fill_manual(values=c(c("#F0F0F0","#000000"))) + 
  labs(fill="Response") + 
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(limits = rev(levels(as.factor(df_form$letter_id))), expand = c(0,0))

ggsave("FigureS19.pdf", plot = p, path = output_file, width = 7,  height = 5, units = "in", dpi = 300)

#### Figure S20: projected deaths, susceptibles by team ####
df_pars_intonly_nodates <- subset(df_pars, !(param %in% c("param_dist_dates_2wk_intervention","param_dist_dates_5percent_intervention","param_initial_number_susc_individuals")))

df_plot_pars <- df_pars_intonly_nodates %>%
  group_by(letter_id, intervention, param) %>% 
  summarise(
    q5 = value[which(quantile==5)],
    q25 = value[which(quantile==25)],
    q50.test=value[which(quantile==50)],
    q75 = value[which(quantile==75)],
    q95 = value[which(quantile==95)],
    id = unique(letter_id)
  )

df_plot_pars <- transform(df_plot_pars, q5 = as.numeric(q5), q25 = as.numeric(q25),q50.test = as.numeric(q50.test),q75 = as.numeric(q75),q95 = as.numeric(q95))

#reverse the levels of the id
df_plot_pars$id = factor(as.factor(df_plot_pars$id), levels = rev(levels(as.factor(df_plot_pars$id))))

# set types
df_plot_pars[,c(4:8)] <- sapply(df_plot_pars[,c(4:8)], as.numeric)

param.labs = c("Deaths final day", "Susceptibles final day", "New infections final day")
names(param.labs) = unique(df_plot_pars$param)

#set intervention order
df_plot_pars$intervention = factor(df_plot_pars$intervention, levels=c("closed","5percent", "2weeks", "open"))

#rename levels
levels(df_plot_pars$intervention) <- c("closed","5-percent","2-weeks","open")

p <- ggplot(data = df_plot_pars %>% group_by("param","intervention"), mapping = aes(x = q50.test, y = id)) + #, color = intervention
  geom_errorbarh(mapping = aes(xmin = q5, xmax = q95),
                 size = 0.3, alpha = 0.7, height = 0) +
  geom_errorbarh(mapping = aes(xmin=q25,xmax=q75),size=0.9,alpha=0.7,height = 0)+
  geom_point()+
  scale_y_discrete(name = "Model ID")+
  facet_grid(rows=vars(intervention), cols=vars(param),scales="free_x", labeller = labeller(param = param.labs, intervention = unique(df_plot_pars$intervention)))+
  scale_colour_brewer(palette = "Set1") + xlab("") + 
  theme_bw() + theme(axis.text.y = element_text(size = 11)
                     ,axis.text.x=element_text(size=11,angle=90,hjust=1)
                     ,strip.text = element_text(size = 14))

ggsave("FigureS20.pdf", plot = p, path = output_file, width = 15,  height = 10, units = "in", dpi = 300)


######################
# Tables
# --------------------

#### Table S2: importation rates
df_pars_import <- subset(df_pars, param=="param_importation_rates_num")[,c("letter_id","value")]                 
names(df_pars_import) =c("Model ID", "Importation rate")

## displays table (but does not save it)
formattable(df_pars_import[,c(1,2)], align=c("l","l"), list(`Indicator Name` = formatter(
  "span", style = ~ style(color = "grey",font.weight = "bold"))))                         

######################
# Text
# --------------------

#### Number of FTE hours
quantile(df_form$admin_fte)
