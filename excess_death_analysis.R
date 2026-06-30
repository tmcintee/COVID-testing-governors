aamr <- read.csv("AAMR_by_state.csv") %>%
	select(-Population)
state_pop <- read.csv("state populations.csv")
death_baseline <- aamr %>%
	group_by(State) %>%
	filter(Year != 2020) %>%
	summarize(crude_death = mean(Crude.Rate),
		adjusted_death = mean(Age.Adjusted.Rate),
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
	mutate(excess_deaths = Deaths - linear_death,
		excess_deaths_state = population*(Age.Adjusted.Rate - adjusted_death)/1e5,
		excess_deaths_national = population*(Age.Adjusted.Rate - overall_baseline)/1e5)
summary_excess <- function(df)
{
	return (df %>%
		summarize(excess_deaths = sum(excess_deaths),
			excess_rate = 1e5*sum(excess_deaths)/sum(population),
			#excess_pooled_rate = 1e5*sum(excess_deaths_state)/sum(population),
			#excess_pooled_deaths = sum(excess_deaths_state),
			#excess_excess_by_state = mean(1e5*excess_deaths_state/population),
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

