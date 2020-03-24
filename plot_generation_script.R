require(tidyverse)
require(ggrepel)
require(gganimate)
require(gifski)
#current snapshot


state_infec_base <- read_csv(url("https://covidtracking.com/api/states.csv"))
state_pop <- read_csv("state populations.csv")
state_elec <- read_csv("state governors.csv")
state_area <- read_csv("state areas.csv")
state_urbanization <- read_csv("state urbanization.csv")
state_turnout <- read_csv("governors_turnout.csv")
state_infec <- state_infec_base %>%
  inner_join(state_elec) %>%
  inner_join(state_area) %>%
  inner_join(state_urbanization) %>%
  inner_join(state_pop) %>%
  inner_join(state_turnout)
state_infec[is.na(state_infec)] <- 0
maximax <- max(state_infec$total,na.rm = TRUE)
state_infec$pos_rate <- state_infec$positive/(state_infec$positive+state_infec$negative)
ave_pos_rate <- sum(state_infec$positive)/sum(state_infec$positive + state_infec$negative)
max_pos_rate <- max(state_infec$pos_rate)
state_infec$density <- state_infec$population/state_infec$area
ggplot(state_infec,aes(y = 100000*positive/population,x = 100000*(positive + negative)/population,size = population,fill = pos_rate,label = state))+
  geom_point(shape = 21)+
  geom_text_repel(size = 4)+
  scale_x_log10()+
  scale_y_log10()+
  #expand_limits(x = c(1,maximax), y = c(1,maximax))+
  scale_fill_gradientn(colors = c("green","yellow","red"),values = c(0,ave_pos_rate,1))+
  labs(x = "Compeleted COVID-19 tests per 100,000",
       y = "Positive COVID-19 tests per 100,000",
       fill = "Share of test results positive",
       title = "COVID-19 testing and results by state, gubernatorial election, and party of current governor",
       size = "Population",
       subtitle = paste0("Data from covidtracking.com; plot generated ",date()))+
  facet_grid(rows = vars(current),cols = vars(election))+
  theme_bw()+
  guides()

#

weighted_lm <- lm(data = state_infec, pos_rate ~ density + urbanization + election + current,weights = population)
high_accountable <- state_infec %>% filter(election %in% c("Biennial","Presidential"))
low_accountable <- state_infec %>% filter(election %in% c("Midterm","Off-year"))


ggplot(state_infec,aes(y = positive/(positive + negative),x = Last_gov_turnout, label = state, size = population,
                       fill = positive / (positive + negative)))+
  geom_point(shape = 21)+
  geom_text_repel(size = 4)+
  #expand_limits(x = c(1,maximax), y = c(1,maximax))+
  scale_fill_gradientn(colors = c("green","yellow","red"),values = c(0,ave_pos_rate,1))+
  scale_y_log10()+
  labs(x = "Turnout in last gubernatorial election",
       y = "Positive rate on COVID-19 tests")+
  geom_smooth(method = "lm")+
  theme_bw()+
  guides()

state_summary <- state_infec %>%
  group_by(election) %>%
  filter(state != "WA" & state != "NY") %>%
  summarise(confirmed_cases_per_100000 = 100000*sum(positive)/sum(population),
                                                                          testing_rate = 100000*sum(positive+negative)/sum(population),
                                                                          share_positive = sum(positive)/sum(positive + negative))

#Under construction
state_infec_time <- read.csv(url("http://covidtracking.com/api/states/daily.csv"))
state_infec_time <- inner_join(state_infec_time,state_pop)
state_infec_time[is.na(state_infec_time)] <- 0

g_basic <- ggplot(state_infec_time,aes(y = positive,x = negative))+
  geom_point()+
  transition_states(states = date)

g_anim <- ggplot(state_infec_time,aes(y = positive,x = negative,size = 100000*positive/population,fill = positive/(positive + negative),label = state))+
  geom_label()+
  #geom_point(shape = 21)+
  scale_x_log10()+
  scale_y_log10()+
  coord_equal()+
  expand_limits(x = c(1,maximax), y = c(1,maximax))+
  scale_fill_gradientn(colors = c("green","yellow","red"),values = c(0,ave_pos_rate,1))+
  labs(x = "Negative COVID-19 tests",
       y = "Positive COVID-19 tests",
       size = "Confirmed cases per 100,000",
       fill = "Share of test results positive",
       title = "COVID-19 testing and results by state",
       subtitle = paste0("Data from covidtracking.com; plot generated ",date()))+
  transition_states(states = date)

animate(g_anim, duration = 20, fps = 30, width = 800, height = 600, renderer = gifski_renderer("covid_labels.gif"))
