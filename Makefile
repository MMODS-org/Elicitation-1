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

data_round2: add_ranks_round2
	Rscript src/data/create_rdata_object.R \
		"data/processed/round2/team_submissions_incl_agg_round2.csv" \
		"data/processed/round2/mmods1_round_2_results_clean.Rdata"

prepare_county_data:
	Rscript src/data/prepare_county_data.R \
		"data/processed/county/county_attributes.xlsx" \
		"data/processed/county/time_series_covid19_deaths_US.csv" \
		"data/processed/county/time_series_covid19_confirmed_US.csv" \
		"data/processed/county/processed"
		
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
		"data/processed/compare_rounds.Rdata" 

compare_agg: 
	Rscript src/analysis/compare_w_aggregate.R \
    	"data/processed/round1/team_submissions_incl_agg_round1.csv"\
		"data/processed/round2/team_submissions_incl_agg_round2.csv"\
		"data/processed/compare_aggregate.Rdata" 

###############
# Visualization
# -------------

# Figure 1: aggregate and individual model results for
#           each objective and intervention pair
fig1: 
	Rscript src/viz/figure_1.R \
		$(mmods_viz_tools) \
		"data/processed/round2/mmods1_round_2_results_clean.Rdata" \
		"data/processed/round2/team_submissions_incl_agg_round2_allquantiles.csv" \
		"$(fig_dir)"

# Figure 2: days closed R1 vs. R2
fig2:
	Rscript src/viz/figure_2.R \
		$(mmods_viz_tools) \
		"data/processed/compare_rounds.Rdata" \
		$(fig_dir)

fig4_panels:
	Rscript src/viz/multipanel_figure.R \
		$(mmods_viz_tools) \
		"data/processed/round2/mmods1_round_2_results_clean.Rdata" \
		"output/figures/Figure4_panels"

## supplemental figures

# projection results
# Figure S2 - S15
sfigs_proj:
	Rscript src/viz/plotting_targets_round2_abridged.R \
		$(mmods_viz_tools) \
		"data/processed/round2/mmods1_round_2_results_clean.Rdata" \
		"data/processed/round2/team_submissions_incl_agg_round2_allquantiles.csv" \
		"data/processed/round1/team_submissions_incl_agg_round1_allquantiles.csv" \
		"data/processed/compare_rounds.Rdata" \
		"data/processed/compare_aggregate.Rdata" \
		"output/figures/supplemental_figures"

# county data comparison results
# Figure S16 - 17
figs_county:
	Rscript src/viz/plotting_targets_county.R \
		$(mmods_viz_tools) \
		"data/processed/county/processed/county_deaths_cdf_closed_strict.csv" \
		"data/processed/county/processed/county_deaths_cdf_closed_partial.csv" \
		"data/processed/county/processed/county_cases_cdf_closed_strict.csv" \
		"data/processed/county/processed/county_cases_cdf_closed_partial.csv" \
		"data/processed/round2/team_submissions_incl_agg_round2_allquantiles.csv" \
		"output/figures/supplemental_figures"
		
figs_checklist:
	Rscript src/viz/plotting_targets_county.R \
		"data/processed/round2/checklist_data.csv" \
		"data/processed/round2/checklist_parameter_data.csv"
		$(fig_dir)
