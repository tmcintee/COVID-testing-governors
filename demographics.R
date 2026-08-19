require(tidycensus)
require(dplyr)
require(tidyr)

# Variable dictionary: one row per variable
var_dict <- tibble::tribble(~var_name, ~variable,
	"population",         "B01003_001",
	# Race / ethnicity counts
	"white_nonhisp",      "B03002_003",
	"black_nonhisp",      "B03002_004",
	"native_nonhisp",     "B03002_005",
	"asian_nonhisp",      "B03002_006",
	"pacific_nonhisp",    "B03002_007",
	"other_nonhisp",      "B03002_008",
	"multiracial_nonhisp","B03002_009",
	"hispanic",           "B03002_012",
	"age65plus",          "S0101_C01_030",
	"foreign_born",         "B05006_001",
	"bachelors_plus",       "S1501_C01_015E",
	"pop25plus",            "S1501_C01_006E")
# strip trailing E/P/PE suffixes from both ACS output and lookup table

acs <- get_acs(geography = "state",
		variables = var_dict$variable,
		survey = "acs5",
		year = 2020) %>% 
	mutate(variable_base = sub("(PE|P|E)$", "", variable))

var_dict <- var_dict %>% mutate(variable_base = sub("(PE|P|E)$", "", variable))

state_demo <- acs %>%
	left_join(var_dict %>% select(variable_base, var_name),by = "variable_base") %>%
	select(NAME, var_name, estimate) %>%
	pivot_wider(names_from = var_name, values_from = estimate) %>%
	rename(state_name = NAME) %>%
	mutate(state = state.abb[match(state_name, state.name)])

state_demo_groups <- state_demo %>%
	inner_join(state_elec)

demo_4group <- state_demo_groups %>%
	group_by(election) %>%
	summarize(pct_white_nonhispanic = 100*sum(white_nonhisp)/sum(population),
		pct_black = 100*sum(black_nonhisp)/sum(population),
		pct_hispanic = 100*sum(hispanic)/sum(population),
		pct_over_65 = 100*sum(age65plus)/sum(population),
		pct_degree = 100*sum(bachelors_plus)/sum(pop25plus), # Degree is measured for 25+.
		pct_foreign_born = 100*sum(foreign_born)/sum(population))


demo_2group <- state_demo_groups %>%
	group_by(election %in% c("Presidential","Biennial")) %>%
	summarize(pct_white_nonhispanic = 100*sum(white_nonhisp)/sum(population),
		pct_black = 100*sum(black_nonhisp)/sum(population),
		pct_hispanic = 100*sum(hispanic)/sum(population),
		pct_over_65 = 100*sum(age65plus)/sum(population),
		pct_degree = 100*sum(bachelors_plus)/sum(pop25plus), # Degree is measured for 25+.
		pct_foreign_born = 100*sum(foreign_born)/sum(population))

demos <- bind_rows(demo_2group,demo_4group)
write_csv(demos,"demographic_summary.csv")