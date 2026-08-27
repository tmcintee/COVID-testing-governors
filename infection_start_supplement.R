infec_start <- read.csv("infection_early.csv") %>%
	inner_join(state_elec)
start_elec <- infec_start %>% 
	group_by(election) %>%
	summarize(Documented.Mean = mean(Documented),
		REMEDID.Mean = mean(REMEDID),
		Difference.Mean = mean(Difference.in.Days))


start_2020 <- infec_start %>% 
	group_by(election %in% c("Presidential","Biennial")) %>%
	summarize(Documented.Mean = mean(Documented),
		REMEDID.Mean = mean(REMEDID),
		Difference.Mean = mean(Difference.in.Days))
bind_rows(tidy(aov(Documented ~ election, data = infec_start)) %>%
		filter(term == "election") %>%
		mutate(variable = "Documented"),
	tidy(aov(REMEDID ~ election, data = infec_start)) %>%
		filter(term == "election") %>%
		mutate(variable = "REMEDID"),
	tidy(aov(Difference.in.Days ~ election, data = infec_start)) %>%
		filter(term == "election") %>%
		mutate(variable = "Difference.in.Days")) %>%
	select(variable, p.value)

infec_start <- infec_start %>%
	mutate(election_2020 = election %in% c("Presidential", "Biennial"))

start_2020 <- infec_start %>%
	group_by(election_2020) %>%
	summarize(
		Documented.Mean = mean(Documented, na.rm = TRUE),
		REMEDID.Mean = mean(REMEDID, na.rm = TRUE),
		Difference.Mean = mean(Difference.in.Days, na.rm = TRUE)
	)

bind_rows(
	tidy(t.test(Documented ~ election_2020, data = infec_start)) %>%
		mutate(variable = "Documented"),
	tidy(t.test(REMEDID ~ election_2020, data = infec_start)) %>%
		mutate(variable = "REMEDID"),
	tidy(t.test(Difference.in.Days ~ election_2020, data = infec_start)) %>%
		mutate(variable = "Difference.in.Days")
) %>%
	select(variable, estimate1, estimate2, p.value)