library(sars2pack)
library(tidyverse)
state_infec_time <- covidtracker_data()
state_pop <- read_csv("state populations.csv")
state_elec <- read_csv("state governors.csv")
state_area <- data.frame(state = state.abb, area = state.area)
state_urbanization <- read_csv("state urbanization.csv")
state_turnout <- read_csv("governors_turnout.csv")
state_infec_time_mod <- state_infec_time %>%
  inner_join(state_elec) %>%
  inner_join(state_area) %>%
  inner_join(state_urbanization) %>%
  inner_join(state_pop) %>%
  inner_join(state_turnout)
state_infec_time_mod[is.na(state_infec_time_mod)] <- 0
state_infec_time_mod <- state_infec_time_mod %>%
  mutate(numeric_date = as.numeric(date)-min(as.numeric(date)))
states_group_election <- state_infec_time_mod %>%
  group_by(date,election) %>%
  summarise(Population = sum(population),
            Deaths = sum(death),
            Confirmed_cases = sum(positive),
            Total_tests = sum(positive + negative),
            Deaths_per = 100000*Deaths/Population,
            Cases_per = 100000*Confirmed_cases/Population,
            Tests_per = 100000*Total_tests/Population)
ggplot(states_group_election %>% filter(Deaths > 0),aes(x = date, y = Deaths, color = election))+
  geom_point()+
  geom_smooth()+
  scale_x_date()+
  scale_y_log10()+
  labs(x = "Date",
       y = "COVID-19 deaths",
       color = "Gubernatorial election schedule")

my_states <- c("CA","NC","VA","OH")
states_group_me <- state_infec_time_mod %>%
  mutate(me_live = state %in% my_states) %>%
  group_by(date,me_live) %>%
  summarise(Population = sum(population),
            Deaths = sum(death),
            Confirmed_cases = sum(positive),
            Total_tests = sum(positive + negative),
            Deaths_per = 100000*Deaths/Population,
            Cases_per = 100000*Confirmed_cases/Population,
            Tests_per = 100000*Total_tests/Population)


national_time <- state_infec_time_mod %>%
  mutate(numeric_date = as.numeric(date)-min(as.numeric(date))) %>%
  group_by(date,numeric_date) %>%
  summarise(Population = sum(population),
            Deaths = sum(death),
            Confirmed_cases = sum(positive),
            Total_tests = sum(positive + negative),
            Deaths_per = 100000*Deaths/Population,
            Cases_per = 100000*Confirmed_cases/Population,
            Tests_per = 100000*Total_tests/Population)


ggplot(states_group_me %>% filter(Deaths > 0),
       aes(x = date, y = Deaths_per, color = factor(me_live,labels = c("States I have not lived in recently",
                                                                       "States I have lived in recently"))))+
  geom_point()+
  geom_smooth()+
  scale_x_date()+
  scale_y_log10()+
  labs(x = "Date",
       y = "COVID-19 deaths per 100000",
       color = "")

my_states <- c("CA","NC","VA","OH")
states_group_me <- state_infec_time_mod %>%
  mutate(me_live = state %in% my_states) %>%
  group_by(date,me_live) %>%
  summarise(Population = sum(population),
            Deaths = sum(death),
            Confirmed_cases = sum(positive),
            Total_tests = sum(positive + negative),
            Deaths_per = 100000*Deaths/Population,
            Cases_per = 100000*Confirmed_cases/Population,
            Tests_per = 100000*Total_tests/Population)

ggplot(states_group_me %>% filter(Deaths > 0),
       aes(x = date, y = Deaths_per, color = factor(me_live,labels = c("States I have not lived in recently",
                                                                       "States I have lived in recently"))))+
  geom_point()+
  geom_smooth()+
  scale_x_date()+
  scale_y_log10()+
  labs(x = "Date",
       y = "COVID-19 deaths per 100000",
       color = "")

confederate_states <- c("SC",
                        "NC",
                        "VA",
                        "TN",
                        "GA",
                        "MS",
                        "AL",
                        "FL",
                        "LA",
                        "TX",
                        "AR")
border_states <- c("KY",
                   "MO",
                   "DE",
                   "MD")
new_england <- c("ME",
                 "MA",
                 "RI",
                 "CT",
                 "VT",
                 "NH")
other_union_states <- c("IL",
                        "IN",
                        "IA",
                        "KS",
                        "MI",
                        "MN",
                        "OH",
                        "NJ",
                        "NY",
                        "OR",
                        "CA")
union_states <- c(new_england,other_union_states)
states_group_civil_war <- state_infec_time_mod %>%
  mutate(status_1861 = fct_collapse(state,
                                    Confederate = confederate_states,
                                    Union = union_states,
                                    Border = border_states,
                                    group_other = TRUE)) %>%
  group_by(date,status_1861) %>%
  summarise(Population = sum(population),
            Deaths = sum(death),
            Confirmed_cases = sum(positive),
            Total_tests = sum(positive + negative),
            Deaths_per = 100000*Deaths/Population,
            Cases_per = 100000*Confirmed_cases/Population,
            Tests_per = 100000*Total_tests/Population)


ggplot(states_group_civil_war %>% filter(Deaths > 0),
       aes(x = date, y = Deaths_per, color = status_1861))+
  geom_point()+
  geom_smooth()+
  scale_x_date()+
  labs(x = "Date",
       y = "COVID-19 deaths per 100000",
       color = "Status in 1861")


fit <- nls(death ~ SSlogis(numeric_date, Asym, xmid, scal),
           data = state_infec_time_mod %>% filter(state == "NY" )
           )
newData <- data.frame(numeric_date = c(1:100))
newData$death <- predict(fit,newData)
ggplot(NY_frame,aes(x = numeric_date,y = death))+geom_point()+
  stat_function(fun=fitfun)

fun_list_logistic <- list()
fun_list_exp <- list()
state_count <- length(unique(state_infec_time_mod$state))
parameters_estimated <- data.frame(State = unique(state_infec_time_mod$state),
                                   Growth = numeric(length = 50),
                                   Initial = numeric(length = 50),
                                   Logistic_rate = numeric(length = 50),
                                   Logistic_cap = numeric(length = 50),
                                   Logistic_midpoint = numeric(length = 50)
                                   )
for(i in 1:state_count)
{
  each_state <- unique(state_infec_time_mod$state)[[i]]
  smallFrame <- state_infec_time_mod %>%
    filter(state == each_state & death > 0)
  try(fun_list_logistic[[i]] <- log_fit_function(smallFrame,"death","numeric_date"))
  try(names(fun_list_logistic)[[i]] <- each_state)
  try(fun_list_exp[[i]] <- exp_fit_function(smallFrame,"death","numeric_date"))
  try(names(fun_list_exp)[[i]] <- each_state)
  try(
  if(is.function(fun_list_logistic[[i]]))
  {
    params <- log_fit_function(smallFrame,"death","numeric_date",mode = "coefficients")
    parameters_estimated[[i,"Logistic_rate"]] <- 1/params[["scal"]]
    parameters_estimated[[i,"Logistic_cap"]] <- params[["Asym"]]
    parameters_estimated[[i,"Logistic_midpoint"]] <- params[["xmid"]]
  })
  if(is.function(fun_list_exp[[i]]))
  {
    params <- exp_fit_function(smallFrame,"death","numeric_date",mode = "coefficients")
    parameters_estimated[[i,"Growth"]] <- params[["Growth"]]
    parameters_estimated[[i,"Initial"]] <- params[["Initial"]]
  }
}