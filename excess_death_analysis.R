aamr <- read.csv("AAMR_by_state.csv") %>%
	select(-Population)
state_pop <- read.csv("state populations.csv")
death_baseline <- aamr %>%
	group_by(State) %>%
	filter(Year != 2020) %>%
	summarize(crude_death = mean(Crude.Rate),
		adjusted_death_rate = mean(Age.Adjusted.Rate),
		average_death = mean(Deaths),
		linear_death = coef(lm(Deaths ~ Year))[[1]]+
			2020*coef(lm(Deaths ~ Year))[[2]]) %>%
	ungroup()
overall_baseline <- mean(death_baseline$adjusted_death)
state_key <- data.frame(State = state.name, state = state.abb)
excess_deaths <- aamr %>%
	filter(Year == 2020) %>%
	inner_join(death_baseline) %>%
	inner_join(state_key) %>%
	inner_join(state_pop) %>%
	inner_join(state_elec) %>%
	inner_join(state_turnout) %>%
	mutate(excess_deaths = Deaths - linear_death,
		excess_deaths_mean = Deaths - average_death,
		excess_deaths_aamr = (population / 1e5) * (Age.Adjusted.Rate - adjusted_death_rate),
		excess_death_rate_aamr = (Age.Adjusted.Rate - adjusted_death_rate),
		excess_death_rate = 1e5 * (Deaths - linear_death) / population,
		excess_death_rate_mean = 1e5 * (Deaths - average_death) / population,
		Crude.Rate.Check = excess_deaths / population)
summary_excess <- function(df)
{
	return (df %>%
		summarize(excess_deaths = sum(excess_deaths),
			excess_rate = 1e5*sum(excess_deaths)/sum(population),
			excess_rate_pooled = mean(excess_death_rate),
			excess_rate_mean = 1e5*sum(excess_deaths_mean)/sum(population),
			excess_rate_pooled_mean = mean(excess_death_rate_mean),
			excess_rate_aamr = 1e5*sum(excess_deaths_aamr)/sum(population),
			excess_rate_pooled_aamr = mean(excess_death_rate_aamr),
			group_size = sum(population)))
}

excess_death_groups_election <- excess_deaths %>%
	group_by(election) %>%
	summary_excess()
excess_death_groups_2020 <- excess_deaths %>%
	group_by(election %in% c("Biennial","Presidential")) %>%
	summary_excess()
excess_death_parties <- excess_deaths %>%
	group_by(current) %>%
	summary_excess()
excess_death_parties_2020 <- excess_deaths %>%
	group_by(current,election %in% c("Biennial","Presidential")) %>%
	summary_excess()

turn_model = lm(1e5*(excess_deaths/population) ~ Last_gov_turnout, data = excess_deaths)
# Model 2: Turnout + 2020 election
turn_model_2 = lm(1e5*(excess_deaths/population) ~ Last_gov_turnout + 
		(election == "Midterm"|election =="Biennial"), data = excess_deaths)
turn_model_3 = lm(1e5*(excess_deaths/population) ~ Last_gov_turnout + 
		election, data = excess_deaths)
# Add partisanship
turn_model_4 = lm(1e5*(excess_deaths/population) ~ Last_gov_turnout + 
		(election == "Midterm"|election =="Biennial") +
		current, 
	data = excess_deaths)
turn_model_5 = lm(1e5*(excess_deaths/population) ~ Last_gov_turnout + 
		election + 
		current, 
	data = excess_deaths)
