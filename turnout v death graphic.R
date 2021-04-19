require(electoralanalysis)
max_DR = max(state_infec$death/state_infec$population)
max_TR = max(state_infec$Last_gov_turnout)

elec_cov <- state_infec %>%
  group_by(state) %>%
  summarise(Elec.Vote.D = (death/population)/max_DR)
elec_cov$State <- ""
for(i in 1:nrow(elec_cov)){elec_cov$State[[i]] = state.name[state.abb == elec_cov$state[[i]]]}
g_cov <- MapSimple(elec_cov,fiftystater::fifty_states,colorVector = c("black"))

elec_turnout <- state_infec %>%
  group_by(state) %>%
  summarise(Elec.Vote.T = (1-Last_gov_turnout)/max_TR)
elec_turnout$State <- ""
for(i in 1:nrow(elec_turnout)){elec_turnout$State[[i]] = state.name[state.abb == elec_turnout$state[[i]]]}
g_turnout <- MapSimple(elec_turnout,fiftystater::fifty_states,colorVector = c("black","white"))

g_cor <- ggplot(state_infec,aes(x = Last_gov_turnout, y = 100000*death/population, size = population, label = state, alpha = 0.5))+
  geom_text()+
  labs(x = "Turnout in last gubernatorial election",
       y = "COVID-19 deaths per 100,000") +
  guides(size = FALSE, alpha = FALSE)+
  geom_smooth(method = "lm")+
  theme_bw()

ggpubr::ggarrange(nrow = 1,g_cov + labs(title = "COVID-19 deaths per capita"),g_cor,g_turnout + labs(title = "Turnout in last gubernatorial election"))

cov_model = lm((death/population) ~ Last_gov_turnout +  density, data = state_infec)
dens_model = lm((death/population) ~ density, data = state_infec)
turn_model = lm((death/population) ~ Last_gov_turnout, data = state_infec)
elec_model = lm((death/population) ~ election, data = state_infec)
pol_model = lm((death/population) ~ election + Last_gov_turnout, data = state_infec)
everything_model = lm(death/population ~ election + Last_gov_turnout + urbanization + density + current,data = state_infec)
everything_but_sched = lm(death / population ~ Last_gov_turnout +urbanization + density + current, data = state_infec)
everything_but_turnout = lm(death / population ~ election +urbanization + density + current, data = state_infec)
