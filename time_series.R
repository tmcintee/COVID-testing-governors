require(tidyverse)
state_infec_daily <- read_csv(url("https://covidtracking.com/api/states/daily.csv"))
state_infec_daily$pos_rate <- state_infec_daily$positive/(state_infec_daily$positive+state_infec_daily$negative)
state_infec_daily$pos_rate_instant <- state_infec_daily$positiveIncrease/(state_infec_daily$positiveIncrease+state_infec_daily$negativeIncrease)
state_tests <- state_infec_daily %>% group_by(state) %>% summarise(tests = max(totalTestResults)) %>% arrange(-tests)
key_states <- state_tests$state[1:10]
ggplot(state_infec_daily,aes(x = date, y = pos_rate, label = state))+
  geom_line(data = state_infec_daily %>% filter(state %in% key_states), aes(group = state, color = state))+
  geom_text(alpha = 0.2)+
  geom_text(data = state_infec_daily %>% filter(state %in% key_states), aes(group = state, color = state))+
  scale_color_brewer(palette = "Paired")
