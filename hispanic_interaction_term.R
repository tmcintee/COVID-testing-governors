death_hispanic <- read.csv("death_hispanic_supplement.csv")
hispanic_baseline <- death_hispanic %>%
	group_by(State,Hispanic.Origin) %>%
	filter(Year != 2020,
		Hispanic.Origin != "Not Stated") %>%
	summarize(linear_death = coef(lm(Deaths ~ Year))[[1]]+
			2020*coef(lm(Deaths ~ Year))[[2]]) %>%
	ungroup()
hispanic_excess_deaths <- death_hispanic %>%
	filter(Year == 2020,
		Hispanic.Origin != "Not Stated") %>%
	inner_join(hispanic_baseline) %>%
	inner_join(state_key) %>%
	inner_join(state_elec) %>%
	mutate(excess_deaths = Deaths - linear_death) %>%
	mutate(excess_death_rate = 1e5*excess_deaths/as.numeric(Population))
hispanic_by_election <- hispanic_excess_deaths %>%
	group_by(election,Hispanic.Origin) %>%
	summarize(Population = sum(as.numeric(Population)),
		Deaths = sum(Deaths),
		excess_deaths = sum(excess_deaths)) %>%
	mutate(excess_death_rate = 1e5*excess_deaths/Population)

hispanic_by_election_2020 <- hispanic_excess_deaths %>%
	group_by(election %in% c("Biennial","Presidential"),Hispanic.Origin) %>%
	summarize(Population = sum(as.numeric(Population)),
		Deaths = sum(Deaths),
		excess_deaths = sum(excess_deaths)) %>%
	mutate(excess_death_rate = 1e5*excess_deaths/Population)