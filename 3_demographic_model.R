rm(list=ls())
library(tidyverse)
library(readxl)
library(lubridate)
library(zoo)
library(cowplot)
library(flextable)
library(cdcfluview)
library(ggpubr)
library(magick)
library(grid)
library(officer)
library(RColorBrewer)



# Initial Version  --------------------------------------------------------
#From original pre-print 


conditions = c("all cancer",
               "pancreatic cancer",
               "lung cancer",
               "colorectal cancer",
               "breast cancer",
               "hematologic cancer",
               "diabetes",
               "Alzheimer's",
               "kidney disease",
               "ischemic heart disease")

population_at_risk = c(18000000,
                       90000,
                       541000,
                       1545000,
                       3800000,
                       550000,
                       34200000,
                       6500000,
                       37000000,
                       20000000)

deaths_2019 = c(546453,
                39798,
                123622,
                49053,
                43519,
                57892,
                229326,
                118993,
                189938,
                440225)

IFR_adjusted = c(2.272999,
                 2.636392,
                 2.703841,
                 2.034357,
                 1.813096,
                 2.888777,
                 2.105315,
                 9.289791,
                 2.864266,
                 3.903738)

data = data.frame(conditions = conditions,
                  population_at_risk = population_at_risk,
                  deaths_2019 = deaths_2019,
                  IFR_adjusted = IFR_adjusted/100) %>% 
  mutate(expected_infections = population_at_risk * 0.09,
         expected_deaths_null = expected_infections*IFR_adjusted,
         excess_null = round(expected_deaths_null/deaths_2019*100),
         expected_deaths_OR2 = expected_infections*IFR_adjusted*2,
         excess_OR2 = round(expected_deaths_OR2/deaths_2019*100),
         expected_deaths_OR5 = expected_infections*IFR_adjusted*5,
         excess_OR5 = round(expected_deaths_OR5/deaths_2019*100))



# Revised version ---------------------------------------------------------

dat = read_excel("data/demographic_model_data.xlsx") %>% 
  mutate(`Chronic Condition`=outcome,
         covid_ifr = covid_ifr/100,
         ifr_lower = ifr_lower/100,
         ifr_upper = ifr_upper/100,
         attack_rate = attack_rate/100,
         attack_lower=attack_lower/100,
         attack_upper=attack_upper/100,
         excess_null = (pop_at_risk*attack_rate*covid_ifr)/observed_deaths*100,
         excess_null_lower = (pop_at_risk*attack_lower*ifr_lower)/observed_deaths*100,
         excess_null_upper = (pop_at_risk*attack_upper*ifr_upper)/observed_deaths*100,
         excess_OR2= (pop_at_risk*attack_rate*(covid_ifr*2))/observed_deaths*100,
         excess_OR2_lower = (pop_at_risk*attack_lower*(ifr_lower*2))/observed_deaths*100,
         excess_OR2_upper = (pop_at_risk*attack_upper*(ifr_upper*2))/observed_deaths*100,
         excess_OR5= (pop_at_risk*attack_rate*(covid_ifr*5))/observed_deaths*100,
         excess_OR5_lower = (pop_at_risk*attack_lower*(ifr_lower*5))/observed_deaths*100,
         excess_OR5_upper = (pop_at_risk*attack_upper*(ifr_upper*5))/observed_deaths*100)



demo_data = dat %>% 
  mutate(outcome = factor(outcome,levels=c("All cancers","Pancreatic cancer","Lung cancer","Hematological cancer","Colorectal cancer",
                          "Breast cancer","Diabetes","Alzheimer's")),
         observed_excess = paste0(round(observed)," (",round(observed_lower),"-",round(observed_upper),")"),
           null = paste0(round(excess_null)," (",round(excess_null_lower),"-",round(excess_null_upper),")"),
         OR2 = paste0(round(excess_OR2)," (",round(excess_OR2_lower),"-",round(excess_OR2_upper),")"),
         OR5 = paste0(round(excess_OR5)," (",round(excess_OR5_lower),"-",round(excess_OR5_upper),")")) %>% 
  select(`Chronic condition`=outcome,
         `State`=state,
    `Population at risk` = pop_at_risk,
         `Mean age`=age_at_risk,
    `Wave` = period,
    `Observed deaths over same period in 2019`=observed_deaths,
    `Observed excess in 2020` = observed_excess,
    `Expected excess (null)`=null,
    `Expected excess (OR=2)`=OR2,
    `Expected excess (OR=5)`=OR5) %>% 
  arrange(`Chronic condition`)
write.csv(demo_data,"tables/demo_table.csv")

demo_table = flextable::as_flextable(demo_data) %>% set_table_properties(layout = "autofit")
demo_table
save_as_docx(demo_table, path="tables/demo_table.docx")



# Competing risks figures  ------------------------------------------------
library(RColorBrewer)
library(stats)
dat = dat %>% 
  mutate(baseline_risk = observed_deaths/pop_at_risk*100,
         outcome = factor(outcome,levels=c("All cancers","Pancreatic cancer","Lung cancer","Hematological cancer","Colorectal cancer",
                                           "Breast cancer","Diabetes","Alzheimer's")),
         State = factor(state,levels=c("National","New York","Texas","California")),
         covid_rate = covid_ifr*attack_rate*pop_at_risk/pop_at_risk*100)

plot1 = ggplot(data=dat)+
  theme_bw()+
  geom_point(aes(x=log(baseline_risk), y=log(excess_null),color=outcome,shape=State),size=10)+
  scale_color_manual(name="Chronic Condition",values=c("royalblue4","steelblue2","paleturquoise2","darkcyan",
                                                       "salmon3","peachpuff2","palevioletred","maroon4"))+
  geom_smooth(method = "lm", aes(x=log(baseline_risk),y=log(excess_null)))+
  labs(x="Log(Baseline risk of death from chronic condition)",y="Log(Expected excess due to COVID-19)", title= "Figure 5. Expected excess mortality vs. baseline risk of death")+
  theme(axis.title = element_text(size=20),
        axis.text = element_text(size=20),
        legend.text = element_text(size=15),
        legend.title = element_text(size=15),
        title = element_text(size=25),
       legend.direction="vertical",legend.box="vertical")
plot1
ggsave(plot=plot1, "poster figure 4a.png",height=7,width=16,units="in")
dat2 = dat %>% 
  pivot_longer(cols=c(baseline_risk, covid_rate),names_to="cause",values_to="deaths")
plot2 =ggplot(data=dat %>% filter(State=="National"))+
  theme_bw()+
#  geom_bar(aes(y=outcome, x=deaths*10,group=cause,fill=cause),stat="identity",position="dodge")+
  geom_point(aes(y=outcome, x=baseline_risk,color="Chronic condition itself"),size=10)+
  geom_point(aes(y=outcome, x=covid_rate,color="COVID-19"),size=8)+
  #scale_fill_manual(name=NULL,values=c("plum1"))+
  scale_color_manual(name=NULL,values=c("salmon3","royalblue4"))+
  labs(y=NULL, title ="Figure 4. Expected deaths per 100 persons at risk", x=NULL)+
  theme(axis.title = element_text(size=18),
        axis.text = element_text(size=20),
        legend.text = element_text(size=20),
        title = element_text(size=25),
        legend.position="bottom", legend.direction="horizontal"
        )+
  theme(
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )+
  theme(legend.box.background  = element_rect(fill = "white", colour = "black"))
plot2
ggsave(plot=plot2, "competing risks figure4b.png",height=5.5,width=16,units="in")
plot3 = plot_grid(plot1, plot2, ncol=2,rel_widths = c(.7,1))

plot3
ggsave(plot=plot3, "competing risks figure.png",height=4,width=16,units="in")



# Sensitivity Analysis 1 --------------------------------------------------
#using attack rates for adults 50-64 
dat_sens1 = read_excel("data/demographic_model_data.xlsx") %>% 
  mutate(`Chronic Condition`=outcome,
         covid_ifr = covid_ifr/100,
         ifr_lower = ifr_lower/100,
         ifr_upper = ifr_upper/100,
         attack_rate = attack_rate_5064/100,
         attack_lower=attack_lower_5064/100,
         attack_upper=attack_upper_5064/100,
         excess_null = (pop_at_risk*attack_rate*covid_ifr)/observed_deaths*100,
         excess_null_lower = (pop_at_risk*attack_lower*ifr_lower)/observed_deaths*100,
         excess_null_upper = (pop_at_risk*attack_upper*ifr_upper)/observed_deaths*100,
         excess_OR2= (pop_at_risk*attack_rate*(covid_ifr*2))/observed_deaths*100,
         excess_OR2_lower = (pop_at_risk*attack_lower*(ifr_lower*2))/observed_deaths*100,
         excess_OR2_upper = (pop_at_risk*attack_upper*(ifr_upper*2))/observed_deaths*100,
         excess_OR5= (pop_at_risk*attack_rate*(covid_ifr*5))/observed_deaths*100,
         excess_OR5_lower = (pop_at_risk*attack_lower*(ifr_lower*5))/observed_deaths*100,
         excess_OR5_upper = (pop_at_risk*attack_upper*(ifr_upper*5))/observed_deaths*100)



demo_data_sens1 = dat_sens1 %>% 
  mutate(outcome = factor(outcome,levels=c("All cancers","Pancreatic cancer","Lung cancer","Hematological cancer","Colorectal cancer",
                                           "Breast cancer","Diabetes","Alzheimer's")),
         observed_excess = paste0(round(observed)," (",round(observed_lower),"-",round(observed_upper),")"),
         null = paste0(round(excess_null)," (",round(excess_null_lower),"-",round(excess_null_upper),")"),
         OR2 = paste0(round(excess_OR2)," (",round(excess_OR2_lower),"-",round(excess_OR2_upper),")"),
         OR5 = paste0(round(excess_OR5)," (",round(excess_OR5_lower),"-",round(excess_OR5_upper),")")) %>% 
  select(`Chronic condition`=outcome,
         `State`=state,
         `Population at risk` = pop_at_risk,
         `Mean age`=age_at_risk,
         `Wave` = period,
         `Observed deaths over same period in 2019`=observed_deaths,
         `Observed excess in 2020` = observed_excess,
         `Expected excess (null)`=null,
         `Expected excess (OR=2)`=OR2,
         `Expected excess (OR=5)`=OR5) %>% 
  arrange(`Chronic condition`)

#decrease attack rate by 20%
dat_sens2 = read_excel("data/demographic_model_data.xlsx") %>% 
  mutate(`Chronic Condition`=outcome,
         covid_ifr = covid_ifr/100,
         ifr_lower = ifr_lower/100,
         ifr_upper = ifr_upper/100,
         attack_rate = attack_rate*.8/100,
         attack_lower=attack_lower*.8/100,
         attack_upper=attack_upper*.8/100,
         excess_null = (pop_at_risk*attack_rate*covid_ifr)/observed_deaths*100,
         excess_null_lower = (pop_at_risk*attack_lower*ifr_lower)/observed_deaths*100,
         excess_null_upper = (pop_at_risk*attack_upper*ifr_upper)/observed_deaths*100,
         excess_OR2= (pop_at_risk*attack_rate*(covid_ifr*2))/observed_deaths*100,
         excess_OR2_lower = (pop_at_risk*attack_lower*(ifr_lower*2))/observed_deaths*100,
         excess_OR2_upper = (pop_at_risk*attack_upper*(ifr_upper*2))/observed_deaths*100,
         excess_OR5= (pop_at_risk*attack_rate*(covid_ifr*5))/observed_deaths*100,
         excess_OR5_lower = (pop_at_risk*attack_lower*(ifr_lower*5))/observed_deaths*100,
         excess_OR5_upper = (pop_at_risk*attack_upper*(ifr_upper*5))/observed_deaths*100)



demo_data_sens2 = dat_sens2 %>% 
  mutate(outcome = factor(outcome,levels=c("All cancers","Pancreatic cancer","Lung cancer","Hematological cancer","Colorectal cancer",
                                           "Breast cancer","Diabetes","Alzheimer's")),
         observed_excess = paste0(round(observed)," (",round(observed_lower),"-",round(observed_upper),")"),
         null = paste0(round(excess_null)," (",round(excess_null_lower),"-",round(excess_null_upper),")"),
         OR2 = paste0(round(excess_OR2)," (",round(excess_OR2_lower),"-",round(excess_OR2_upper),")"),
         OR5 = paste0(round(excess_OR5)," (",round(excess_OR5_lower),"-",round(excess_OR5_upper),")")) %>% 
  select(`Chronic condition`=outcome,
         `State`=state,
         `Population at risk` = pop_at_risk,
         `Mean age`=age_at_risk,
         `Wave` = period,
         `Observed deaths over same period in 2019`=observed_deaths,
         `Observed excess in 2020` = observed_excess,
         `Expected excess (null)`=null,
         `Expected excess (OR=2)`=OR2,
         `Expected excess (OR=5)`=OR5) %>% 
  arrange(`Chronic condition`)


#decrease attack rate by 30%
dat_sens3 = read_excel("data/demographic_model_data.xlsx") %>% 
  mutate(`Chronic Condition`=outcome,
         covid_ifr = covid_ifr/100,
         ifr_lower = ifr_lower/100,
         ifr_upper = ifr_upper/100,
         attack_rate = attack_rate*.7/100,
         attack_lower=attack_lower*.7/100,
         attack_upper=attack_upper*.7/100,
         excess_null = (pop_at_risk*attack_rate*covid_ifr)/observed_deaths*100,
         excess_null_lower = (pop_at_risk*attack_lower*ifr_lower)/observed_deaths*100,
         excess_null_upper = (pop_at_risk*attack_upper*ifr_upper)/observed_deaths*100,
         excess_OR2= (pop_at_risk*attack_rate*(covid_ifr*2))/observed_deaths*100,
         excess_OR2_lower = (pop_at_risk*attack_lower*(ifr_lower*2))/observed_deaths*100,
         excess_OR2_upper = (pop_at_risk*attack_upper*(ifr_upper*2))/observed_deaths*100,
         excess_OR5= (pop_at_risk*attack_rate*(covid_ifr*5))/observed_deaths*100,
         excess_OR5_lower = (pop_at_risk*attack_lower*(ifr_lower*5))/observed_deaths*100,
         excess_OR5_upper = (pop_at_risk*attack_upper*(ifr_upper*5))/observed_deaths*100)

demo_data_sens3 = dat_sens3 %>% 
  mutate(outcome = factor(outcome,levels=c("All cancers","Pancreatic cancer","Lung cancer","Hematological cancer","Colorectal cancer",
                                           "Breast cancer","Diabetes","Alzheimer's")),
         observed_excess = paste0(round(observed)," (",round(observed_lower),"-",round(observed_upper),")"),
         null = paste0(round(excess_null)," (",round(excess_null_lower),"-",round(excess_null_upper),")"),
         OR2 = paste0(round(excess_OR2)," (",round(excess_OR2_lower),"-",round(excess_OR2_upper),")"),
         OR5 = paste0(round(excess_OR5)," (",round(excess_OR5_lower),"-",round(excess_OR5_upper),")")) %>% 
  select(`Chronic condition`=outcome,
         `State`=state,
         `Population at risk` = pop_at_risk,
         `Mean age`=age_at_risk,
         `Wave` = period,
         `Observed deaths over same period in 2019`=observed_deaths,
         `Observed excess in 2020` = observed_excess,
         `Expected excess (null)`=null,
         `Expected excess (OR=2)`=OR2,
         `Expected excess (OR=5)`=OR5) %>% 
  arrange(`Chronic condition`)


