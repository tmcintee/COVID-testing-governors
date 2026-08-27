# load("C:/repos/COVID-testing-governors/statecapacitydata_final.RData")
state_key <- data.frame(state.name = state.name, state = state.abb)
state_capacity <- sc_data_master %>%
	rename(state.name = state) %>%
	inner_join(state_key) %>%
	inner_join(state_pop) %>%
	inner_join(state_elec)
capacity_by_election <- state_capacity %>% 
	group_by(election) %>%
	summarize(SC = mean(SC),
		MeanExcess = mean(MeanExcess),
		infantmortality2018 = sum(infantmortality2018 * population) / sum(population), 
		illiteracy = sum(illiteracy * population) / sum(population),
		innumeracy = sum(innumeracy * population) / sum(population),
		gradrate = sum(gradrate * population) / sum(population),
		op_ratio = sum(op_ratio * population) / sum(population),
		pupilteachratio = sum(pupilteachratio * population) / sum(population),
		povrate = sum(povrate * population) / sum(population),
		corruptconvictrate = sum(corruptconvictrate * population) / sum(population),
		BAlaborforce = sum(BAlaborforce * population) / sum(population),
		patents = sum(patents * population) / sum(population),
		robrate = sum(robrate * population) / sum(population),
		cartheftrate = sum(cartheftrate * population) / sum(population),
		murderrate = sum(murderrate * population) / sum(population),
		propcrimerate = sum(propcrimerate * population) / sum(population))

capacity_by_2020_election <- state_capacity %>% 
	group_by(election %in% c("Presidential","Biennial")) %>%
	summarize(SC = mean(SC),
		MeanExcess = mean(MeanExcess),
		infantmortality2018 = sum(infantmortality2018 * population) / sum(population), 
		illiteracy = sum(illiteracy * population) / sum(population),
		innumeracy = sum(innumeracy * population) / sum(population),
		gradrate = sum(gradrate * population) / sum(population),
		op_ratio = sum(op_ratio * population) / sum(population),
		pupilteachratio = sum(pupilteachratio * population) / sum(population),
		povrate = sum(povrate * population) / sum(population),
		corruptconvictrate = sum(corruptconvictrate * population) / sum(population),
		BAlaborforce = sum(BAlaborforce * population) / sum(population),
		patents = sum(patents * population) / sum(population),
		robrate = sum(robrate * population) / sum(population),
		cartheftrate = sum(cartheftrate * population) / sum(population),
		murderrate = sum(murderrate * population) / sum(population),
		propcrimerate = sum(propcrimerate * population) / sum(population))