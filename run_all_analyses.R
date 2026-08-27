# run_all_analyses.R
# -----------------------------------------------------------------------
# Master script. Runs every analysis script in this project in the
# right order so one run produces everything:
#
# HOW TO RUN:
#   Set your R working directory to this project's folder and then source
# or run this file.
#
# OUTPUT:
#	Data tables will be present in the local environment. Various figures 
# will be plotted to pdf with a filename that includes a timestamp.

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
