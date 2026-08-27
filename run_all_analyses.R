# run_all_analyses.R
# -----------------------------------------------------------------------
# Master script. Sources every analysis script in this project in the
# right order so one run produces everything:
#
#   1. log_fit_function.R        - defines log_fit_function()/exp_fit_function(),
#                                   needed by sars2_time_series.R
#   2. time_series.R              - positivity-rate time series, all states
#   3. sars2_time_series.R        - deaths by election timing / party / region
#   4. plot_generation_script.R   - current-snapshot plots; builds state_infec,
#                                    which the next script needs
#   5. turnout v death graphic.R  - turnout-vs-death maps and regression models
#
# Steps 2-4 pull their COVID data through covid_data_source.R, which
# downloads (and locally caches) the archived COVID Tracking Project
# data the first time it's needed. See that file to change the analysis
# date window.
#
# HOW TO RUN:
#   Set your R working directory to this project's folder (the one with
#   all the other .R files and the reference .csv files) before running
#   - e.g. open the .Rproj, or in RStudio: Session > Set Working Directory
#   > To Source File Location. Then run this whole script (source it, or
#   Rscript run_all_analyses.R from a terminal).
#
# OUTPUT:
#   Every plot produced by the steps above is written, in order, to a
#   single PDF in output/all_plots_<timestamp>.pdf. If a step's packages
#   aren't installed or it otherwise errors, that step is skipped (with
#   a message) so the rest of the run still completes.
# -----------------------------------------------------------------------

if (!file.exists("log_fit_function.R")) 
{
  stop("Can't find log_fit_function.R in the current working directory.\n",
       "Set your working directory to the project folder (the one containing ",
       "the other .R files and the reference .csv files) and try again.")
}

if (!dir.exists("output")) dir.create("output")
pdf_path <- file.path("output", paste0("all_plots_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf"))
pdf(pdf_path, width = 11, height = 8.5)

run_step <- function(script_name) 
{
  message("\n==== ", script_name, " ====")
  tryCatch(
    source(script_name, print.eval = TRUE),
    error = function(e) 
    {
      message("  FAILED: ", conditionMessage(e))
      message("  Skipping to the next script.")
    }
  )
}

run_step("log_fit_function.R")
run_step("time_series.R")
run_step("sars2_time_series.R")
run_step("plot_generation_script.R")
run_step("turnout v death graphic.R")
run_step("excess_death_analysis.R")
run_step("hispanic_interaction_term.R")
run_step("infection_start_supplement.R")

dev.off()
message("\nAll done. Plots saved to: ", pdf_path)
