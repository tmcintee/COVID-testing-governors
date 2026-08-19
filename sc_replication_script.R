# Replication Script for Auerbach, Lerner, and Ridge "State Capacity and Covid-19 Responses: Comparing US States"
# Script run on R version 4.1.3
# McIntee replication notes: Commented out a number of lines that error out until the script ran. Otherwise, did not notice any major differences poking at this using R 4.4.2.


library(psych)
library(tidyverse)
library(viridis)
library(usmap)
library(sjPlot)
library(sjmisc)
library(ggplot2)
library(foreign)
library(betareg)
library(stargazer)
library(xtable)
library(cspp)

load("statecapacitydata_final.RData")



# McIntee note: I'm skeptical of grad rate and teacher / pupil ratio - too much gaming of those metrics in dysfunctional districts without corresponding improvements in innumeracy and illiteracy. However, in replication, I found that they had relatively little effect on the measures or predictions.
fa_vars = total %>%
	dplyr::select("infantmortality2018", 
		"illiteracy", 
		"innumeracy", 
		"gradrate", 
		"op_ratio",
		"pupilteachratio", 
		"povrate", 
		"corruptconvictrate", 
		"BAlaborforce", 
		"patents",
		"robrate", 
		"cartheftrate", 
		"murderrate", 
		"propcrimerate") 
fa_vars = as.data.frame(fa_vars)
row.names(fa_vars) = total$st.abb

fa_vars = fa_vars%>%
	mutate_all( ~(scale(.) %>% as.vector))

row.names(fa_vars) = total$st.abb
fa_vars = fa_vars[is.na(fa_vars$infantmortality2018) == F,]


#Factor analysis
sc.fa_1 = fa(fa_vars)
summary(sc.fa_1)



# create dataset with factor scores and state data from library(usmap) data
sc_data = as.data.frame(sc.fa_1$scores)
sc_data$abbr = NA
sc_data$abbr = row.names(sc_data)
sc_data = left_join(statepop, sc_data)
sc_data$SC = -sc_data$MR1 # this is our state capacity factor


sc_data = rename(sc_data, "state" = "full") 
# read in DVs
outcomes  = read_csv("Outcome Variables.csv")


sc_data_master = left_join(sc_data, outcomes)
sc_data_master = left_join(sc_data_master, total)

sc_data_master$vaxpct <- sc_data_master$Vax_Series_Complete_Pop_Pct/100
sc_data_master$vaxnumb <- sc_data_master$Vax_Admin_Per_100K/100000

sc_data_master$density = gsub(",","" ,sc_data_master$density)



plot_usmap(data = sc_data_master, values = "SC", color = "black") + 
	scale_fill_viridis(name = "sc.fa_1") +
	theme(legend.position = "right")





# Regressions from the Paper



sc_m1 = betareg(MeanExcessLow~SC + repgov + popage + percentwhite, data=sc_data_master)
sc_m3 = betareg(vaxnumb~SC+ repgov + popage + percentwhite, data=sc_data_master)
sc_m4 = betareg(vaxpct~SC + repgov + popage + percentwhite, data=sc_data_master)
summary(sc_m1)
summary(sc_m3)
summary(sc_m4)

policies <- lm(policyindex ~ repgov + popage + percentwhite + latitude+ longitude  + as.numeric(density) + pollib_median, data=sc_data_master)
summary(policies)

sc_data_master$highcap <- ifelse(sc_data_master$SC > .7, "High Capacity", "Low Capacity")
sc_data_master$highcap = as.factor(sc_data_master$highcap)
colnames(sc_data_master)
sc_m1_pol = betareg(MeanExcessLow~SC + policyindex + repgov + popage + percentwhite, data=sc_data_master)
sc_m1_pol_int = betareg(MeanExcessLow~ highcap * policyindex + repgov + popage + percentwhite, data=sc_data_master)
sc_m1_pol_int_a = betareg(MeanExcessLow~ SC * policyindex + repgov + popage + percentwhite, data=sc_data_master)
sc_m1_school = betareg(MeanExcessLow~SC * SchoolClose + repgov + popage + percentwhite, data=sc_data_master) #not sig
# sc_m1_gath = betareg(MeanExcessLow~SC * GathRestrict + repgov + popage + percentwhite, data=sc_data_master) #model does not run because only ND did not use it
# sc_m1_rest = betareg(MeanExcessLow~SC * RestaurantRestrict + repgov + popage + percentwhite, data=sc_data_master) #not possible, everyone had this
sc_m1_home = betareg(MeanExcessLow~SC * StayAtHome + repgov + popage + percentwhite, data=sc_data_master) 
sc_m1_neb = betareg(MeanExcessLow~SC * NEBusinessClose + repgov + popage + percentwhite, data=sc_data_master)#not sig
summary(sc_m1_pol) #robust to adding
summary(sc_m1_pol_int_a) #p=0.07, policies lower death rates when SC is high. they don't when SC is low.
summary(sc_m1_school) #p=0.13
summary(sc_m1_home) #p=0.06
summary(sc_m1_neb) #p=0.36

intplot <- plot_model(sc_m1_pol_int, type = "pred", terms = c("policyindex", "highcap"))

# Regression Plots
intplot + theme_classic() + xlab("Covid Mitigation Policy Index") + ylab("Mean Excess Deaths") +ggtitle("") +
	theme(legend.title = element_blank(), legend.position = 'bottom')


# Regression Table
stargazer(sc_m1_pol, sc_m1_pol_int_a, sc_m1_school, sc_m1_home, sc_m1_neb, title = "State Capacity, State Policies, and COVID Outcomes", no.space = T)
table(sc_data_master$highcap)


# Appendix models TBW

#regression model with latitude
sc_m1_lat = betareg(MeanExcessLow~SC + repgov + popage + percentwhite + latitude, data=sc_data_master)
sc_m3_lat = betareg(vaxnumb~SC+ repgov + popage + percentwhite + latitude, data=sc_data_master)
sc_m4_lat = betareg(vaxpct~SC + repgov + popage + percentwhite + latitude, data=sc_data_master)
summary(sc_m1_lat)
summary(sc_m3_lat)
summary(sc_m4_lat)

#regression model with longitude
sc_m1_long = betareg(MeanExcessLow~SC + repgov + popage + percentwhite + longitude, data=sc_data_master)
sc_m3_long = betareg(vaxnumb~SC+ repgov + popage + percentwhite + longitude, data=sc_data_master)
sc_m4_long = betareg(vaxpct~SC + repgov + popage + percentwhite + longitude, data=sc_data_master)
summary(sc_m1_long)
summary(sc_m3_long)
summary(sc_m4_long)

#regression model with lat and long
sc_m1_latlong = betareg(MeanExcessLow~SC + repgov + popage + percentwhite + latitude + longitude, data=sc_data_master)
sc_m3_latlong = betareg(vaxnumb~SC+ repgov + popage + percentwhite+ latitude + longitude, data=sc_data_master)
sc_m4_latlong = betareg(vaxpct~SC + repgov + popage + percentwhite + latitude+ longitude, data=sc_data_master)
summary(sc_m1_latlong)
summary(sc_m3_latlong)
summary(sc_m4_latlong)

stargazer(sc_m1_lat, sc_m3_lat, sc_m4_lat, title = "State Capacity Factor and COVID Outcomes", no.space = T)
stargazer(sc_m1_long, sc_m3_long, sc_m4_long, title = "State Capacity Factor and COVID Outcomes", no.space = T)
stargazer(sc_m1_latlong, sc_m3_latlong, sc_m4_latlong, title = "State Capacity Factor and COVID Outcomes", no.space = T)
#stargazer(sc_m1_ind, sc_m3_ind, sc_m4_ind, title = "State Capacity Factor and COVID Outcomes", no.space = T)

tesdata <- subset(sc_data_master, sc_data_master$abbr != "ND")

#regression model with individualism
sc_m1_ind = betareg(MeanExcessLow~SC + repgov + popage + percentwhite + individualism, data=sc_data_master)
sc_m3_ind = betareg(vaxnumb~SC+ repgov + popage + percentwhite + individualism, data=sc_data_master)
sc_m4_ind = betareg(vaxpct~SC + repgov + popage + percentwhite + individualism, data=sc_data_master)
summary(sc_m1_ind)
summary(sc_m3_ind)
summary(sc_m4_ind)

#OLS the regressions in the appendix
sc_m1_lm = lm(MeanExcessLow~SC + repgov +popage +percentwhite, data=sc_data_master)
sc_m2_lm = lm(rentdispersed~SC + repgov +popage  +percentwhite, data=sc_data_master)
sc_m3_lm = lm(Vax_Series_Complete_Pop_Pct~SC+ repgov +popage +percentwhite, data=sc_data_master)
sc_m4_lm = lm(Vax_Admin_Per_100K~SC+ repgov +popage +percentwhite, data=sc_data_master)
summary(sc_m1_lm)
#summary(sc_m2_lm)
summary(sc_m3_lm)
summary(sc_m4_lm)

#MEANEXCESS low in main paper and high in the appendix

sc_m1_high = betareg(MeanExcess~SC + repgov +popage  +percentwhite, data=sc_data_master)
summary(sc_m1_high)

#Trump vote (2016) in appendix and Rep gov in body
sc_m1_a = betareg(MeanExcessLow~SC + trumpvote2016 +popage +percentwhite, data=sc_data_master)
sc_m2_a = betareg(rentdispersed~SC + trumpvote2016 +popage  +percentwhite, data=sc_data_master)
sc_m3_a = betareg(vaxnumb~SC+ trumpvote2016 +popage +percentwhite, data=sc_data_master)
sc_m4_a = betareg(vaxpct~SC+ trumpvote2016 +popage +percentwhite, data=sc_data_master)
summary(sc_m1_a)
summary(sc_m2_a)
summary(sc_m3_a)
summary(sc_m4_a)

#rep leg control in appendix and Rep gov in body
sc_m1_a2 = betareg(MeanExcessLow~SC + repleg +popage +percentwhite, data=sc_data_master)
#sc_m2_a2 = betareg(rentdispersed~SC + repleg +popage  +percentwhite, data=sc_data_master)
sc_m3_a2 = betareg(vaxnumb~SC+ repleg +popage +percentwhite, data=sc_data_master)
sc_m4_a2 = betareg(vaxpct~SC+ repleg +popage +percentwhite, data=sc_data_master)
summary(sc_m1_a2)
#summary(sc_m2_a2)
summary(sc_m3_a2)
summary(sc_m4_a2)

#unified control in appendix and Rep gov in body
sc_m1_a3 = betareg(MeanExcessLow~SC + repcontrol +popage +percentwhite, data=sc_data_master)
#sc_m2_a3 = betareg(rentdispersed~SC + repcontrol +popage  +percentwhite, data=sc_data_master)
sc_m3_a3 = betareg(vaxnumb~SC+ repcontrol +popage +percentwhite, data=sc_data_master)
sc_m4_a3 = betareg(vaxpct~SC+ repcontrol +popage +percentwhite, data=sc_data_master)
summary(sc_m1_a3)
#summary(sc_m2_a3)
summary(sc_m3_a3)
summary(sc_m4_a3)


#include both gov and rep in appendix and Rep gov in body
sc_m1_a4 = betareg(MeanExcessLow~SC + repleg*repgov +popage +percentwhite, data=sc_data_master)
#sc_m2_a3 = betareg(rentdispersed~SC + repcontrol +popage  +percentwhite, data=sc_data_master)
sc_m3_a4 = betareg(vaxnumb~SC+ repleg*repgov +popage +percentwhite, data=sc_data_master)
sc_m4_a4 = betareg(vaxpct~SC+ repleg*repgov +popage +percentwhite, data=sc_data_master)
summary(sc_m1_a4)
#summary(sc_m2_a3)
summary(sc_m3_a4)
summary(sc_m4_a4)




#Flu vaccine included in vax models in appendix
sc_m3_fl = betareg(vaxnumb~SC+ repgov +popage +percentwhite + fluvax, data=sc_data_master)
sc_m4_fl = betareg(vaxpct~SC+ repgov +popage +percentwhite + fluvax, data=sc_data_master)
summary(sc_m3_fl)
summary(sc_m4_fl)

#robust to density
sc_m1_b = betareg(MeanExcessLow~SC + repgov +popage  +percentwhite + as.numeric(density), data=sc_data_master)
#sc_m2_b = lm(rentdispersed~SC + repgov +popage  +percentwhite + as.numeric(density), data=sc_data_master)
sc_m3_b = betareg(vaxnumb~SC+ repgov +popage +percentwhite + as.numeric(density), data=sc_data_master)
sc_m4_b = betareg(vaxpct~SC+ repgov +popage +percentwhite + as.numeric(density), data=sc_data_master)
summary(sc_m1_b)
#summary(sc_m2_b)
summary(sc_m3_b)
summary(sc_m4_b)

#robust to gdppc
sc_m1_g = betareg(MeanExcessLow~SC*gdppc + repgov +popage  +percentwhite + gdppc, data=sc_data_master)
#sc_m2_b = lm(rentdispersed~SC + repgov +popage  +percentwhite + as.numeric(GDPpc), data=sc_data_master)
sc_m3_g = betareg(vaxnumb~SC*gdppc+ repgov +popage +percentwhite + gdppc, data=sc_data_master)
sc_m4_g = betareg(vaxpct~SC*gdppc+ repgov +popage +percentwhite + gdppc, data=sc_data_master)
summary(sc_m1_g)
#summary(sc_m2_g)
summary(sc_m3_g)
summary(sc_m4_g)

#robust to social capital
sc_m1_sc = betareg(MeanExcessLow~SC + repgov +popage  +percentwhite + soc_capital, data=sc_data_master)
#sc_m2_sc = lm(rentdispersed~SC + repgov +popage  +percentwhite + soc_capital, data=sc_data_master)
sc_m3_sc = betareg(vaxnumb~SC+ repgov +popage +percentwhite + soc_capital, data=sc_data_master)
sc_m4_sc = betareg(vaxpct~SC+ repgov +popage +percentwhite + soc_capital, data=sc_data_master)
summary(sc_m1_sc)
#summary(sc_m2_sc)
summary(sc_m3_sc)
summary(sc_m4_sc)

#robust to policy liberalism
sc_m1_l = betareg(MeanExcessLow~SC  +popage  +percentwhite + pollib_median, data=sc_data_master)
#sc_m2_l = lm(rentdispersed~SC  +popage  +percentwhite + pollib_median, data=sc_data_master)
sc_m3_l = betareg(vaxnumb~SC +popage +percentwhite + pollib_median, data=sc_data_master)
sc_m4_l = betareg(vaxpct~SC +popage +percentwhite + pollib_median, data=sc_data_master)
summary(sc_m1_l)
#summary(sc_m2_b)
summary(sc_m3_l)
summary(sc_m4_l)

#robust to census region (midwest is reference category because it defaults to alphabetical)
sc_m1_r = betareg(MeanExcessLow~SC  +repgov+popage  +percentwhite + region, data=sc_data_master)
#sc_m2_r = lm(rentdispersed~SC  +popage  +percentwhite + region, data=sc_data_master)
sc_m3_r = betareg(vaxnumb~SC +repgov+popage +percentwhite + region, data=sc_data_master)
sc_m4_r = betareg(vaxpct~SC+repgov +popage +percentwhite + region, data=sc_data_master)
summary(sc_m1_r)#this one ends up with p=0.11
#summary(sc_m2_b)
summary(sc_m3_r)
summary(sc_m4_r) #this one ends up with p=0.10

#robust to days since the first US case (which by the way has no effect)
sc_m1_d = betareg(MeanExcessLow~SC  +repgov +popage + percentwhite + days, data=sc_data_master)
#sc_m2_r = lm(rentdispersed~SC +repgov +popage  +percentwhite + region, data=sc_data_master)
sc_m3_d = betareg(vaxnumb~SC +repgov+popage +percentwhite + days, data=sc_data_master)
sc_m4_d = betareg(vaxpct~SC+repgov +popage +percentwhite + days, data=sc_data_master)
summary(sc_m1_d)
#summary(sc_m2_b)
summary(sc_m3_d)
summary(sc_m4_d)


industrydat <- read.csv(file = 'SQGDP2__ALL_AREAS_2005_2019_rec.csv')
industrydat2 <- read.csv(file = 'SQGDP2__ALL_AREAS_2005_2019_service.csv')
sc_data_master = sc_data_master[sc_data_master$abbr != "PR",]
sc_data_master$rec <- ifelse(industrydat$state == total$state, industrydat$X2019.Q3, 999)
sc_data_master$service <- ifelse(industrydat2$GeoName == total$state, industrydat2$X2019.Q3, 999)




#robust to including gdp total from arts, entertainment, and recreation in 2019Q3 (also no interaction)
sc_m1_d = betareg(MeanExcessLow~SC+rec +repgov +popage + percentwhite, data=sc_data_master)
#sc_m2_r = lm(rentdispersed~SC  +popage  +percentwhite + region, data=sc_data_master)
sc_m3_d = betareg(vaxnumb~SC+rec+repgov +popage +percentwhite , data=sc_data_master)
sc_m4_d = betareg(vaxpct~SC+rec +repgov+popage +percentwhite , data=sc_data_master)
summary(sc_m1_d)
#summary(sc_m2_b)
summary(sc_m3_d)
summary(sc_m4_d)
sc_data_master$recrate <- sc_data_master$rec/sc_data_master$gdppc*sc_data_master$poptotal
sc_m1_d = betareg(MeanExcessLow~SC+recrate +repgov +popage + percentwhite, data=sc_data_master)
#in dirct model, death goes up as share from recreation goes up, but the SC effect is robust
sc_m3_d = betareg(vaxnumb~SC+recrate +repgov+popage +percentwhite , data=sc_data_master)
sc_m4_d = betareg(vaxpct~SC+recrate+repgov +popage +percentwhite , data=sc_data_master)
summary(sc_m1_d)
#summary(sc_m2_b)
summary(sc_m3_d)
summary(sc_m4_d)

#robust to including gdp total from arts, entertainment, and recreation in 2019Q3 (also no interaction)
sc_m1_rec = betareg(MeanExcessLow~SC+rec +repgov +popage + percentwhite, data=sc_data_master)
#sc_m2_r = lm(rentdispersed~SC  +popage  +percentwhite + region, data=sc_data_master)
sc_m3_rec = betareg(vaxnumb~SC+rec+repgov +popage +percentwhite , data=sc_data_master)
sc_m4_rec = betareg(vaxpct~SC+rec +repgov+popage +percentwhite , data=sc_data_master)
summary(sc_m1_rec)
summary(sc_m3_rec)
summary(sc_m4_rec)
sc_m1_rec_int = betareg(MeanExcessLow~SC*rec +repgov +popage + percentwhite, data=sc_data_master)
#sc_m2_r = lm(rentdispersed~SC  +popage  +percentwhite + region, data=sc_data_master)
sc_m3_rec_int = betareg(vaxnumb~SC*rec+repgov +popage +percentwhite , data=sc_data_master)
sc_m4_rec_int = betareg(vaxpct~SC*rec +repgov+popage +percentwhite , data=sc_data_master)
summary(sc_m1_rec_int)
summary(sc_m3_rec_int)
summary(sc_m4_rec_int)

table(sc_data_master$rec)
sc_data_master$recrate <- 1000000*4*(sc_data_master$rec)/(sc_data_master$gdppc*sc_data_master$poptotal) #times 4 to account for this is 1 quarter
sc_m1_rr = betareg(MeanExcessLow~SC+recrate +repgov +popage + percentwhite, data=sc_data_master)
#in dirct model, death goes up as share from recreation goes up, but the SC effect is robust
sc_m3_rr = betareg(vaxnumb~SC+recrate +repgov+popage +percentwhite , data=sc_data_master)
sc_m4_rr = betareg(vaxpct~SC+recrate+repgov +popage +percentwhite , data=sc_data_master)
summary(sc_m1_rr)
#summary(sc_m2_b)
summary(sc_m3_rr)
summary(sc_m4_rr)
table(sc_data_master$recrate)
sc_m1_rr_int = betareg(MeanExcessLow~SC*recrate +repgov +popage + percentwhite, data=sc_data_master)
#in dirct model, death goes up as share from recreation goes up, but the SC effect is robust
sc_m3_rr_int = betareg(vaxnumb~SC*recrate +repgov+popage +percentwhite , data=sc_data_master)
sc_m4_rr_int = betareg(vaxpct~SC*recrate+repgov +popage +percentwhite , data=sc_data_master)
summary(sc_m1_rr_int)
#summary(sc_m2_b)
summary(sc_m3_rr_int)
summary(sc_m4_rr_int)

#stargazer(sc_m1_rr, sc_m3_rr, sc_m4_rr,sc_m1_rr_int, sc_m3_rr_int, sc_m4_rr_int, title = "State Capacity Factor and COVID Outcomes + Recreation Income", no.space = T)
p1 <- plot_model(sc_m4_rr_int, type = "pred", terms = c("recrate", "SC"),   show.legend = T)
table(sc_data_master$recrate)

#robust to including gdp total from accommodations and food services in 2019Q3 (also no interaction)
sc_data_master$servrate <- 1000000*4*(sc_data_master$service)/(sc_data_master$gdppc*sc_data_master$poptotal) #times 4 to account for this is 1 quarter
sc_m1_sr = betareg(MeanExcessLow~SC+servrate +repgov +popage + percentwhite, data=sc_data_master)
#in dirct model, death goes up as share from recreation goes up, but the SC effect is robust
sc_m3_sr = betareg(vaxnumb~SC+servrate +repgov+popage +percentwhite , data=sc_data_master)
sc_m4_sr = betareg(vaxpct~SC+servrate+repgov +popage +percentwhite , data=sc_data_master)
summary(sc_m1_rr)
#summary(sc_m2_b)
summary(sc_m3_rr)
summary(sc_m4_rr)

sc_m1_sr_int = betareg(MeanExcessLow~SC*servrate +repgov +popage + percentwhite, data=sc_data_master)
#in dirct model, death goes up as share from recreation goes up, but the SC effect is robust
sc_m3_sr_int = betareg(vaxnumb~SC*servrate +repgov+popage +percentwhite , data=sc_data_master)
sc_m4_sr_int = betareg(vaxpct~SC*servrate+repgov +popage +percentwhite , data=sc_data_master)
summary(sc_m1_sr_int)
#summary(sc_m2_b)
summary(sc_m3_sr_int)
summary(sc_m4_sr_int)

# stargazer(sc_m1_sr, sc_m3_sr, sc_m4_sr,sc_m1_sr_int, sc_m3_sr_int, sc_m4_sr_int, title = "State Capacity Factor and COVID Outcomes + Services Income", no.space = T)
p1 <- plot_model(sc_m1_sr_int, type = "pred", terms = c("servrate", "SC"),   show.legend = T)
table(sc_data_master$SC)

table(sc_data_master$st.abb, sc_data_master$GDPpc)


stargazer(sc_m1, sc_m3, sc_m4, title = "State Capacity Factor and COVID Outcomes", no.space = T)
stargazer(sc_m1_lm, sc_m3_lm, sc_m4_lm, title = "State Capacity Factor and COVID Outcomes", no.space = T)
stargazer(sc_m1_a, sc_m3_a, sc_m4_a, title = "State Capacity Factor and COVID Outcomes", no.space = T)
stargazer(sc_m1_b, sc_m3_b, sc_m4_b, title = "State Capacity Factor and COVID Outcomes", no.space = T)

stargazer(sc_m1_high, sc_m3_fl, sc_m4_fl, title = "State Capacity Factor and COVID Outcomes", no.space = T)


stargazer(sc_m1_a2, sc_m3_a2, sc_m4_a2, title = "State Capacity Factor and COVID Outcomes", no.space = T)
stargazer(sc_m1_a3, sc_m3_a3, sc_m4_a3, title = "State Capacity Factor and COVID Outcomes", no.space = T)
stargazer(sc_m1_a4, sc_m3_a4, sc_m4_a4, title = "State Capacity Factor and COVID Outcomes", no.space = T)

stargazer(sc_m1_d, sc_m3_d, sc_m4_d, title = "State Capacity Factor and COVID Outcomes + addtional controls", no.space = T)
stargazer(sc_m1_g, sc_m3_g, sc_m4_g, title = "State Capacity Factor and COVID Outcomes + addtional controls", no.space = T)
stargazer(sc_m1_sc, sc_m3_sc, sc_m4_sc, title = "State Capacity Factor and COVID Outcomes + addtional controls", no.space = T)
stargazer(sc_m1_l, sc_m3_l, sc_m4_l, title = "State Capacity Factor and COVID Outcomes + addtional controls", no.space = T)

stargazer(sc_m1_r, sc_m3_r, sc_m4_r, title = "State Capacity Factor and COVID Outcomes + addtional controls", no.space = T)
stargazer(sc_m1_d, sc_m3_d, sc_m4_d, title = "State Capacity Factor and COVID Outcomes + addtional controls", no.space = T)


ggplot(sc_data_master, aes(x=SC)) + geom_density(fill="grey", color="grey") + theme_classic() + xlab('State Capcity Factor') +ylab("Density")
-sc.fa_1$loadings


# Table 1
xtable(unclass(-sc.fa_1$loadings))



# Figures 
plot_usmap(data = sc_data_master, values = "Vax_Admin_Per_100K", color = "black") + 
	scale_fill_viridis(name = "Vax_Admin_Per_100K",  direction = 1) +
	theme(legend.position = "right")

plot_usmap(data = sc_data_master, values = "MeanExcessLow", color = "black") + 
	scale_fill_viridis(name = "Mean Excess Deaths: 2020", option = "magma", direction = -1) +
	theme(legend.position = "right")

plot_usmap(data = sc_data_master, values = "rentdispersed", color = "black") + 
	scale_fill_viridis(name = "Rent Dispersion",  direction = 1) +
	theme(legend.position = "right")

plot_usmap(data = sc_data_master, values = "Vax_Series_Complete_Pop_Pct", color = "black") + 
	scale_fill_viridis(name = "% Vaccine Series Complete",  direction = 1) +
	theme(legend.position = "right")

plot_usmap(data = sc_data_master, values = "SC", color = "black") + 
	scale_fill_viridis(name = "State Capacity Factor") + 
	theme(legend.position = "right")


plot( sc_data_master$trumpvote2016, sc_data_master$SC, xlim = c(0,100), xlab="Trump Vote Share - 2016", ylab="State Capacity", pch = 20)
abline(lm(sc_data_master$SC ~ sc_data_master$trumpvote2016))
text(sc_data_master$SC ~ sc_data_master$trumpvote2016, labels=sc_data_master$state,data=sc_data_master)

plot(sc_data_master$percentwhite, sc_data_master$SC, xlim = c(0,100), xlab="Percent White", ylab="State Capacity", pch = 20)
abline(lm(sc_data_master$SC ~ sc_data_master$percentwhite))
text(sc_data_master$SC ~ sc_data_master$percentwhite, labels=sc_data_master$state,data=sc_data_master)

plot(sc_data_master$popage, sc_data_master$SC, xlab="Average Population Age", ylab="State Capacity", pch = 20)
abline(lm(sc_data_master$SC ~ sc_data_master$popage))
text(sc_data_master$SC ~ sc_data_master$popage, labels=sc_data_master$state,data=sc_data_master)


plot(log(as.numeric(sc_data_master$gdppc)), sc_data_master$SC, xlab="Log GDP per Capita", ylab="State Capacity", pch = 20)
abline(lm(sc_data_master$SC ~ log(as.numeric(sc_data_master$gdppc))))
text(sc_data_master$SC ~ log(as.numeric(sc_data_master$gdppc)), labels=sc_data_master$state,data=sc_data_master)


plot(sc_data_master$gdppc, sc_data_master$SC, xlab="GDP per Capita", ylab="State Capacity", pch = 20, xlim = c(33000, 80000))
abline(lm(sc_data_master$SC ~ sc_data_master$gdppc))
text(sc_data_master$SC ~ sc_data_master$gdppc, labels=sc_data_master$state,data=sc_data_master)

# Table Summary
# Appendix tables on factor reliability
sc_data_master[c("abbr", "SC", "MeanExcessLow", "Vax_Admin_Per_100K", "Vax_Series_Complete_Pop_Pct", "rentdispersed")] %>%
	filter(abbr != "DC") %>%
	xtable()


fa_vars = total %>%
	dplyr::select( "infantmortality2018", "illiteracy", "innumeracy", "gradrate", "op_ratio",
		"pupilteachratio", "povrate", "corruptconvictrate", "BAlaborforce", "patents",
		"robrate", "cartheftrate", "murderrate", "propcrimerate") 

fa_vars = total %>%
	dplyr::select( "infantmortality2018", "illiteracy", "innumeracy", "gradrate", "op_ratio", 
		"pupilteachratio",  "povrate", "corruptconvictrate", "BAlaborforce", "patents",
		"robrate", "cartheftrate", "murderrate") 
row.names(fa_vars) = total$st.abb
fa_vars = fa_vars%>%
	mutate_all( ~(scale(.) %>% as.vector))
row.names(fa_vars) = total$st.abb
fa_vars = fa_vars[is.na(fa_vars$infantmortality2018) == F,]

#Factor analysis
sc.fa_1 = fa(fa_vars) # the core variable
sc.fa_2 = fa(fa_vars) #minus infant mortality
cor.test(sc.fa_1$scores, sc.fa_2$scores) #0.987
sc.fa_3 = fa(fa_vars) #minus illiteracy
cor.test(sc.fa_1$scores, sc.fa_3$scores) #0.960
sc.fa_4 = fa(fa_vars) #minus innuneracy
cor.test(sc.fa_1$scores, sc.fa_4$scores) #0.954
sc.fa_5 = fa(fa_vars) #minus gradrate
cor.test(sc.fa_1$scores, sc.fa_5$scores) #0.999
sc.fa_6 = fa(fa_vars) #minus op_ratio
cor.test(sc.fa_1$scores, sc.fa_6$scores) #0.996
sc.fa_7 = fa(fa_vars) #minus pupilteachratio
cor.test(sc.fa_1$scores, sc.fa_7$scores) #0.996
sc.fa_8 = fa(fa_vars) #minus povrate
cor.test(sc.fa_1$scores, sc.fa_8$scores) #0.993
sc.fa_9 = fa(fa_vars) #minus corruptconvictrate
cor.test(sc.fa_1$scores, sc.fa_9$scores) #0.997
sc.fa_10 = fa(fa_vars) #minus balaborforce
cor.test(sc.fa_1$scores, sc.fa_10$scores) #0.991
sc.fa_11 = fa(fa_vars) #minus patents
cor.test(sc.fa_1$scores, sc.fa_11$scores) #0.996
sc.fa_12 = fa(fa_vars) #minus robrate
cor.test(sc.fa_1$scores, sc.fa_12$scores) #0.989
sc.fa_13 = fa(fa_vars) #minus cartheftrate
cor.test(sc.fa_1$scores, sc.fa_13$scores) #0.982
sc.fa_14 = fa(fa_vars) #minus murderrate
cor.test(sc.fa_1$scores, sc.fa_14$scores) #0.993
sc.fa_15 = fa(fa_vars) #minus propcrimerate
cor.test(sc.fa_1$scores, sc.fa_15$scores) #0.992

# reliability 
cr_alpha = psych::alpha(fa_vars, check.keys = T)
summary(cr_alpha) #cronbach's alpha on our items == 0.82, which is good

##########################################

# squire and bowen indicies of legislative professionalism for robustness checks

squire = get_cspp_data(vars = "legprofscore", years = 2003)
bowen  = get_cspp_data(vars = "bowen_legprof_firstdim", years = 2013)
squire = squire %>% 
	dplyr::select(-year)
bowen = bowen %>% 
	dplyr::select(-year)


legprof = left_join(squire, bowen)

sc_data_master_prof = left_join(sc_data_master, bowen)
sc_data_master_prof = left_join(sc_data_master_prof, squire)


cor(sc_data_master_prof$SC, sc_data_master_prof$legprofscore, use = "complete.obs")
cor(sc_data_master_prof$SC, sc_data_master_prof$bowen_legprof_firstdim, use = "complete.obs")

sc_m1_legprof = betareg(MeanExcessLow~legprofscore + repgov + popage + percentwhite, data=sc_data_master_prof)
sc_m3_legprof = betareg(vaxnumb~legprofscore+ repgov + popage + percentwhite, data=sc_data_master_prof)
sc_m4_legprof = betareg(vaxpct~legprofscore + repgov + popage + percentwhite, data=sc_data_master_prof)
summary(sc_m1_legprof)
summary(sc_m3_legprof)
summary(sc_m4_legprof)

sc_m1_legprof_b = betareg(MeanExcessLow~bowen_legprof_firstdim + repgov + popage + percentwhite, data=sc_data_master_prof)
sc_m3_legprof_b = betareg(vaxnumb~bowen_legprof_firstdim+ repgov + popage + percentwhite, data=sc_data_master_prof)
sc_m4_legprof_b = betareg(vaxpct~bowen_legprof_firstdim + repgov + popage + percentwhite, data=sc_data_master_prof)
summary(sc_m1_legprof)
summary(sc_m3_legprof)
summary(sc_m4_legprof)

# stargazer(sc_m1_legprof, sc_m3_legprof, sc_m4_legprof, 
#	sc_m1_legprof_b, sc_m3_legprof_b, sc_m4_legprof_b, 
#	title = "Legislative Professionalism and COVID Outcomes", no.space = T)


############################

#### SVI data work 

svi_2020 = read_csv("SVI2020_US_COUNTY.csv")

svi_2020 = svi_2020 %>% 
	dplyr::select(RPL_THEMES, STATE, ST_ABBR)

svi_2020 = svi_2020 %>% 
	group_by(ST_ABBR) %>% 
	summarize(svi = mean(RPL_THEMES))
svi_2020 = svi_2020 %>% 
	rename('abbr' = "ST_ABBR")

sc_data_master_prof_svi = left_join(sc_data_master_prof, svi_2020)

sc_m1_st_svi = betareg(MeanExcessLow~SC + svi  + repgov , data=sc_data_master_prof_svi)
sc_m3_st_svi = betareg(vaxnumb~ SC + svi + repgov , data=sc_data_master_prof_svi)
sc_m4_st_svi = betareg(vaxpct~ SC + svi  + repgov , data=sc_data_master_prof_svi)
summary(sc_m1_st_svi)
summary(sc_m3_st_svi)
summary(sc_m4_st_svi)

cor(sc_data_master_prof_svi$SC, sc_data_master_prof_svi$svi, use = "complete.obs")

stargazer(sc_m1_st_svi, sc_m3_st_svi, sc_m4_st_svi, 
	title = "Social Vulnerability Index, State Capcity, and COVID Outcomes", no.space = T)


################## update liberalism scores
write_lib2019 = read_csv("lib_2019.csv")
write_lib2019 = write_lib2019 %>% 
	dplyr::select(-year)

sc_data_master_prof_svi = left_join(sc_data_master_prof_svi, write_lib2019, by = "st.abb")

sc_m1_l_up = betareg(MeanExcessLow~SC  +popage  +percentwhite + policy_updated, data=sc_data_master_prof_svi)
sc_m3_l_up = betareg(vaxnumb~SC +popage +percentwhite + policy_updated, data=sc_data_master_prof_svi)
sc_m4_l_up = betareg(vaxpct~SC +popage +percentwhite + policy_updated, data=sc_data_master_prof_svi)
summary(sc_m1_l_up)
summary(sc_m3_l_up)
summary(sc_m4_l_up)

stargazer(sc_m1_l_up, sc_m3_l_up, sc_m4_l_up, 
	title = "State Capacity Factor and Covid Outcomes with State Policy Liberalism", no.space = T)
cor(sc_data_master_prof_svi$pollib_median, sc_data_master_prof_svi$policy_updated, use="complete.obs")

################


stateemploy  = get_cspp_data(vars = "atotemp", years = 2016)

stateemploy = stateemploy %>% 
	select(-year)
sc_data_master_prof = left_join(sc_data_master_prof, stateemploy)
sc_data_master_prof$stateemploy
cor(sc_data_master_prof$SC, sc_data_master_prof$atotemp,  use = "complete.obs")
sc_data_master_prof$stateemploy_percent= (sc_data_master_prof$atotemp/ sc_data_master_prof$poptotal)*100

cor(sc_data_master_prof$SC, 
	sc_data_master_prof$stateemploy_percent,  
	use = "complete.obs")

sc_m1_st_emp = betareg(MeanExcessLow~log(atotemp) + repgov + popage + percentwhite, data=sc_data_master_prof)
sc_m3_st_emp = betareg(vaxnumb~log(atotemp)+ repgov + popage + percentwhite, data=sc_data_master_prof)
sc_m4_st_emp = betareg(vaxpct~log(atotemp) + repgov + popage + percentwhite, data=sc_data_master_prof)
summary(sc_m1_st_emp)
summary(sc_m3_st_emp)
summary(sc_m4_st_emp)

sc_m1_st_empr = betareg(MeanExcessLow~stateemploy_percent + repgov + popage + percentwhite, data=sc_data_master_prof)
sc_m3_st_empr = betareg(vaxnumb~stateemploy_percent+ repgov + popage + percentwhite, data=sc_data_master_prof)
sc_m4_st_empr = betareg(vaxpct~stateemploy_percent + repgov + popage + percentwhite, data=sc_data_master_prof)
summary(sc_m1_st_empr)
summary(sc_m3_st_empr)
summary(sc_m4_st_empr)

#stargazer(sc_m1_st_emp, sc_m3_st_emp, sc_m4_st_emp, 
#	sc_m1_st_empr, sc_m3_st_empr, sc_m4_st_empr, 
#	title = "Number of State Employees and COVID Outcomes", no.space = T)
