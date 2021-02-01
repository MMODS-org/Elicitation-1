# Save CSV file as Rdata object
# to preserve ordering of factors etc
# that are used in plotting

######################
# Read command-line args
# --------------------

args <- commandArgs(trailingOnly = TRUE)

input_csv_file <- args[1]
output_rdata_file <- args[2]


input_csv_file = "./public_repo/data/processed/round2/team_submissions_incl_agg_round2.csv" 
output_rdata_file = "./public_repo/data/processed/round2/mmods1_round_2_results_clean.Rdata"

######################
# Load data
# --------------------

df_plot <- read.csv(input_csv_file, stringsAsFactors = FALSE)

######################
# Plotting parameters
# --------------------

# Intervention labels
int.labs <-  c("closed", "5-percent", "2-weeks", "open")
names(int.labs) <-  c("closed","5percent","2weeks","open")

# Labels for plots
obj.labs <- c(
    "Cumulative infections", 
    "Cumulative deaths", 
    "Peak hospitalizations", 
    "Probability of outbreak", 
    "Days closed")
names(obj.labs) <- c("cumu_infections", "cumu_deaths", "peak_hosp", "prob_outbreak", "days_closed")

obj.labs.incletters = c("A: Cumulative infections",
                        "B: Cumulative deaths",
                        "C: Peak hospitalizations",
                        "D: Probability of outbreak",
                        "E: Days closed" )
names(obj.labs.incletters)= c("cumu_infections","cumu_deaths","peak_hosp","prob_outbreak","days_closed")

obj.order <- c("cumu_infections", "cumu_deaths", "peak_hosp", "prob_outbreak", "days_closed")

# Capitalise "aggregate"
df_plot$id[df_plot$id == "aggregate"] <- "Aggregate"

# Create ordering of id
id.order <- sort(unique(df_plot$id))
id.order <- c("Aggregate", id.order[!(id.order %in% "Aggregate")])

######################
# Viz-specific cleaning
# --------------------

# Create new columns used for plotting
df_plot$id <- factor(df_plot$id, levels = id.order, ordered = TRUE)
df_plot$id.rev <- factor(df_plot$id, levels = rev(id.order), ordered = TRUE)
df_plot$intervention <- factor(df_plot$intervention, levels = c("closed","5percent","2weeks","open"), ordered = TRUE)
df_plot$intervention.rev <- factor(df_plot$intervention, levels = rev(c("closed","5percent","2weeks","open")), ordered = TRUE)
df_plot$objective <- factor(df_plot$objective, levels = obj.order, ordered = TRUE)

#####################
# Save as Rdata type
# -------------------

save(list = c("df_plot", "int.labs", "obj.labs", "obj.labs.incletters","obj.order", "id.order"), 
    file = output_rdata_file)
