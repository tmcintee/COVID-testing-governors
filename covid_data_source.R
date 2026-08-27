require(tidyverse)
require(lubridate)

get_covidtracking_data <- function(start_date = "2020-03-15",
	end_date = "2020-12-31",
	force_refresh = FALSE,
	covid_data_url = "https://raw.githubusercontent.com/COVID19Tracking/covid-tracking-data/master/data/states_daily_4pm_et.csv",
	covid_cache_dir = "covid_data_cache") 
{
	covid_window_start <- as.Date(start_date)
	covid_window_end   <- as.Date(end_date)
	if (!dir.exists(covid_cache_dir)) dir.create(covid_cache_dir)
	{
		local_path <- file.path(covid_cache_dir, paste0("states_covid_data", ".csv"))
	}
	if (force_refresh || !file.exists(local_path)) 
	{
		message("Downloading COVID Tracking Project data to ", local_path, " ...")
		result <- try(download.file(covid_data_url, 
			local_path, mode = "wb", quiet = TRUE),
			silent = TRUE)
		if (inherits(result, "try-error") || !file.exists(local_path)) 
		{
			stop("Could not download '", dataset, "' data and no local cache exists at ",
			local_path, ". Check your internet connection, or manually save the file from\n  ",
			covid_data_url, "\nto that path and re-run.")
		}
	}
	out <- read_csv(local_path, col_types = cols(date = col_character(), .default = col_guess())) %>% 
		filter(ymd(date) <= covid_window_end & ymd(date) >= covid_window_start)
	out$date <- ymd(out$date)
	return(out)
}