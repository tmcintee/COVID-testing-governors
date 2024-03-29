require(tidyverse)
require(ggrepel)
require(gganimate)
require(gifski)
#current snapshot


#state_infec_base <- state_infec_time_mod %>% filter(numeric_date == max(numeric_date))
state_infec_base <- read.csv(file = "https://covidtracking.com/api/v1/states/current.csv")
state_pop <- read_csv("state populations.csv")
state_elec <- read_csv("state governors.csv")
state_area <- data.frame(state = state.abb, area = state.area)
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
ggplot(state_infec,
       aes(y = 100000*death/population,
           x = 100000*(positive + negative)/population,
           size = population,
           fill = pos_rate,
           label = state))+
  geom_point(shape = 21)+
  geom_text_repel(size = 4, alpha = 1)+
  scale_x_log10()+
  scale_y_log10()+
  #expand_limits(x = c(1,maximax), y = c(1,maximax))+
  scale_fill_gradientn(colors = c("green","yellow","red"),values = c(0,ave_pos_rate,1))+
  labs(x = "Completed COVID-19 tests per 100,000",
       y = "COVID-19 confirmed deaths per 100,000",
       fill = "Share of test results positive",
       title = "COVID-19 testing and results by state, gubernatorial election, and party of current governor",
       size = "Population",
       subtitle = paste0("Data from covidtracking.com; plot generated ",date()))+
  facet_grid(rows = vars(current),cols = vars(election))+
  theme_bw()+
  guides()


ggplot(state_infec,
       aes(y = death,
           x = positive,
           fill = pos_rate,
           label = state))+
  geom_point(shape = 21)+
  geom_text_repel(size = 4, alpha = 1)+
  scale_x_log10()+
  scale_y_log10()+
  #expand_limits(x = c(1,maximax), y = c(1,maximax))+
  scale_fill_gradientn(colors = c("green","yellow","red"),values = c(0,ave_pos_rate,1))+
  labs(x = "Confirmed COVID-19 cases",
       y = "Deaths",
       fill = "Share of test results positive",
       title = "COVID-19 deaths and confirmed cases",
       size = "Hospitalizations",
       subtitle = paste0("Data from covidtracking.com; plot generated ",date()))+
  geom_smooth(method = "lm",se = FALSE)+
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
  #filter(state != "WA" & state != "NY") %>%
  summarise(Total_population = sum(population),
            Total_deaths = sum(death),
            Total_hospitalized = sum(hospitalized),
            Total_confirmed_cases = sum(positive),
            Confirmed_cases_per_100000 = 100000*Total_confirmed_cases/Total_population,
            Testing_rate = 100000*sum(positive+negative)/sum(population),
            share_positive = sum(positive)/sum(positive + negative))

