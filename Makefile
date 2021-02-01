#######################
# Files and directories
# ---------------------
mmods_viz_tools="src/viz/mmods_viz_tools.R"
mmods_analysis_tools = "src/analysis/mmods_analysis_tools.R"
fig_dir="output/figures"


################
# Data cleaning
# --------------
combine_teamdata_round1:
	Rscript src/data/combine_processed_files.R \
		"data/processed/round1/submissions" \
		"data/processed/round1/team_submissions_only_round1.csv"

combine_teamdata_round2:
	Rscript src/data/combine_processed_files.R \
		"data/processed/round2/submissions" \
		"data/processed/round2/team_submissions_only_round2.csv"

#requirements: add_ranks_round2
data_round2:
	Rscript src/data/create_rdata_object.R \
		"data/processed/round2/team_submissions_incl_agg_round2.csv" \
		"data/processed/round2/mmods1_round_2_results_clean.Rdata"

prepare_county_data:
	Rscript src/data/prepare_county_data.R \
		"data/processed/county/county_attributes.xlsx" \
		"data/processed/county/time_series_covid19_deaths_US.csv" \
		"data/processed/county"
		
################
# Analysis
# --------------
# Calculate aggregate distribution
# add new 'aggregate' column to cleaned data

add_aggregate_round1: combine_teamdata_round1
	Rscript src/analysis/calculate_aggregate.R \
		$(mmods_analysis_tools) \
		"data/processed/round1/team_submissions_only_round1.csv"\
		"data/model_weights.csv"\
		"data/processed/round1/team_submissions_incl_agg_round1_allquantiles.csv" \
		1

add_ranks_round1: add_aggregate_round1
	Rscript src/analysis/calculate_ranks.R \
		$(mmods_analysis_tools) \
		"data/processed/round1/team_submissions_incl_agg_round1_allquantiles.csv" \
		"data/processed/round1/team_submissions_incl_agg_round1.csv"

add_aggregate_round2: combine_teamdata_round2
	Rscript src/analysis/calculate_aggregate.R \
		$(mmods_analysis_tools) \
		"data/processed/round2/team_submissions_only_round2.csv"\
		"data/model_weights.csv"\
		"data/processed/round2/team_submissions_incl_agg_round2_allquantiles.csv" \
		2

add_ranks_round2: add_aggregate_round2
	Rscript src/analysis/calculate_ranks.R \
		$(mmods_analysis_tools) \
		"data/processed/round2/team_submissions_incl_agg_round2_allquantiles.csv"\
		"data/processed/round2/team_submissions_incl_agg_round2.csv"
		
compare_rounds: 
	Rscript src/analysis/round_comparison.R \
    "data/processed/round1/team_submissions_incl_agg_round1.csv"\
		"data/processed/round2/team_submissions_incl_agg_round2.csv"\
		"data/processed/compare/compare_rounds.Rdata" 

compare_agg: 
	Rscript src/analysis/compare_w_aggregate.R \
    "data/processed/round1/team_submissions_incl_agg_round1.csv"\
		"data/processed/round2/team_submissions_incl_agg_round2.csv"\
		"data/processed/compare/compare_aggregate.Rdata" 


###############
# Visualization
# -------------

# Figure 2: Individual model results for 
# each objective and intervention scenario pair
# WIP
# fig2:
#	Rscript src/viz/plotting_targets_round2_abridged.R \
#		$(mmods_viz_tools) \
		"data/processed/round2/submissions/mmods1_round_2_results_clean.Rdata" \
		"$(fig_dir)/fig2_scenario_interval_full_colorbyrank.png"

fig_multipanel:
	Rscript src/viz/multipanel_figure.R \
		$(mmods_viz_tools) \
		"data/processed/round2/submissions/mmods1_round_2_results_clean.Rdata" \
		$(fig_dir)

figs_county:
	Rscript src/viz/plotting_targets_county.R \
		$(mmods_viz_tools) \
		"data/processed/county/county_deaths_cdf_closed_strict.csv" \
		"data/processed/county/county_deaths_cdf_closed_partial.csv" \
		"data/processed/round2/team_submissions_incl_agg_round2_allquantiles.csv" \
		$(fig_dir)
		
figs_checklist:
	Rscript src/viz/plotting_targets_county.R \
		"data/processed/round2/checklist_data.csv" \
		"data/processed/round2/checklist_parameter_data.csv"
		$(fig_dir)
