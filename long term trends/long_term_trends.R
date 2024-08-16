rm(list=ls())
library(tidyverse)
library(zoo)
library(MMWRweek)
library(imputeTS)


pop1 = read.delim("long term trends/data through 2023/pop_2010_2020.txt") %>% 
  filter(!is.na(Population)) %>% 
  select("year" = Yearly.July.1st.Estimates, Population)

pop2 = read.delim("long term trends/data through 2023/pop_2020_2022.txt") %>% 
  filter(!is.na(Population))%>% 
  select("year" = Yearly.July.1st.Estimates, Population) %>% 
  mutate(year = year+1)

pop3 = rbind(pop1, pop2)

dat_nat = readRDS("Data/weekly_mort_us.rds") %>% 
  filter(MMWRdate<'2020-12-20')%>% 
  pivot_longer(cols=c(cancer_uc,cancer_mc,
                      diabetes_uc,diabetes_mc,
                      alzheimer_uc,alzheimer_mc),
               names_to="cod",values_to="deaths") %>% 
  select("date"=MMWRdate, cod,deaths) %>% 
  group_by(cod) %>% 
  mutate(deaths_smooth = rollmean(deaths, k=5,fill="extend"),
         date=as.Date(date),
         MMWRweek(date)) %>% 
  left_join(pop3, by=c("MMWRyear"="year"))


dat_nat2 = read.delim("long term trends/data through 2023/national_MC.txt") %>% 
   mutate(week = as.numeric(substr(MMWR.Week.Code,6,7)),
         year = as.numeric(MMWR.Year.Code)) %>% 
  filter(week<99) %>% 
  mutate(date = MMWRweek2Date(MMWRweek = week, MMWRyear=year),
         cod = case_when(MCD...ICD.10.113.Cause.List.Code=="GR113-019"~"cancer_mc",
                         MCD...ICD.10.113.Cause.List.Code=="GR113-046"~"diabetes_mc",
                         MCD...ICD.10.113.Cause.List.Code=="GR113-052"~"alzheimer_mc")) %>% 
  filter(date<'2024-02-25') %>% 
  select(date, cod, "deaths"=Deaths) %>% 
  group_by(cod) %>% 
  mutate(deaths_smooth = rollmean(deaths, k=5,fill="extend"))

dat_nat3 = read.delim("long term trends/data through 2023/national_UC.txt") %>% 
  mutate(week = as.numeric(substr(MMWR.Week.Code,6,7)),
         year = as.numeric(MMWR.Year.Code)) %>% 
  filter(week<99) %>% 
  mutate(date = MMWRweek2Date(MMWRweek = week, MMWRyear=year),
         cod = case_when(UCD...ICD.10.113.Cause.List.Code=="GR113-019"~"cancer_uc",
                         UCD...ICD.10.113.Cause.List.Code=="GR113-046"~"diabetes_uc",
                         UCD...ICD.10.113.Cause.List.Code=="GR113-052"~"alzheimer_uc")) %>% 
  filter(date<'2024-02-25') %>% 
  select(date, cod, "deaths"=Deaths) %>% 
  group_by(cod) %>% 
  mutate(deaths_smooth = rollmean(deaths, k=5,fill="extend"))

dat_nat4 = rbind(dat_nat2, dat_nat3) %>% 
  mutate(date=as.Date(date),
         MMWRweek(date))%>% 
  left_join(pop3, by=c("MMWRyear"="year")) %>% 
  mutate(Population = na_locf(Population))

plot_counts = ggplot()+
  geom_line(data=dat_nat, aes(x=date, y=deaths_smooth),color="blue")+
  geom_line(data=dat_nat4, aes(x=date, y=deaths_smooth),color="black")+
  facet_wrap(~cod,scales="free",ncol=2)+
  labs(x=NULL, y="Deaths",title="National")
plot_counts
ggsave(plot=plot_counts, "national_counts.png",height=7,width=13,units="in")

plot_rates = ggplot()+
  theme_bw()+
  geom_line(data=dat_nat4, aes(x=date, y=deaths_smooth/Population*100000),color="blue")+
  geom_line(data=dat_nat, aes(x=date, y=deaths_smooth/Population*100000),color="black")+
  geom_vline(xintercept=as.Date('2020-12-06'),linetype="dashed")+
  facet_wrap(~cod,scales="free",ncol=2)+
  labs(x=NULL, y="Mortality Rate",title="National")
plot_rates 
ggsave(plot=plot_rates, "national_rates.png",height=7,width=13,units="in")

# State -level data -------------------------------------------------------
spop1 = read.delim("long term trends/data through 2023/state_pop_2010_2020.txt") %>% 
  filter(!is.na(Population)) %>% 
  select(States, "year" = Yearly.July.1st.Estimates, Population)

spop2 = read.delim("long term trends/data through 2023/state_pop_2020_2022.txt") %>% 
  filter(!is.na(Population))%>% 
  select(States, "year" = Yearly.July.1st.Estimates, Population) %>% 
  mutate(year = year+1)

spop3 = rbind(spop1, spop2)

dat_state = readRDS("Data/weekly_mort_states.rds") %>% 
  filter(MMWRdate<'2020-12-20')%>% 
  pivot_longer(cols=c(cancer_uc,cancer_mc,
                      diabetes_uc,diabetes_mc,
                      alzheimer_uc,alzheimer_mc),
               names_to="cod",values_to="deaths") %>% 
  select(staters, "date"=MMWRdate, cod,deaths) %>% 
  group_by(staters, cod) %>% 
  mutate(deaths_smooth = rollmean(deaths, k=5,fill="extend"),
         date=as.Date(date),
         MMWRweek(date),
       States = case_when(staters=="CA"~"California",
                         staters=="NY"~"New York",
                         staters=="TX"~"Texas")) %>% 
  left_join(spop3, by=c("MMWRyear"="year","States"))


dat_state2 = read.delim("long term trends/data through 2023/states_MC.txt") %>% 
  mutate(week = as.numeric(substr(MMWR.Week.Code,6,7)),
         year = as.numeric(MMWR.Year.Code)) %>% 
  filter(week<99) %>% 
  mutate(date = MMWRweek2Date(MMWRweek = week, MMWRyear=year),
         cod = case_when(MCD...ICD.10.113.Cause.List.Code=="GR113-019"~"cancer_mc",
                         MCD...ICD.10.113.Cause.List.Code=="GR113-046"~"diabetes_mc",
                         MCD...ICD.10.113.Cause.List.Code=="GR113-052"~"alzheimer_mc")) %>% 
  filter(date<'2024-02-25') %>% 
  select("States"=Residence.State, date, cod, "deaths"=Deaths) %>% 
  group_by(States, cod) %>% 
  mutate(deaths_smooth = rollmean(deaths, k=5,fill="extend"))

dat_state3 = read.delim("long term trends/data through 2023/states_UC.txt") %>% 
  mutate(week = as.numeric(substr(MMWR.Week.Code,6,7)),
         year = as.numeric(MMWR.Year.Code)) %>% 
  filter(week<99) %>% 
  mutate(date = MMWRweek2Date(MMWRweek = week, MMWRyear=year),
         cod = case_when(UCD...ICD.10.113.Cause.List.Code=="GR113-019"~"cancer_uc",
                         UCD...ICD.10.113.Cause.List.Code=="GR113-046"~"diabetes_uc",
                         UCD...ICD.10.113.Cause.List.Code=="GR113-052"~"alzheimer_uc")) %>% 
  filter(date<'2024-02-25') %>% 
  select("States"=Residence.State,date, cod, "deaths"=Deaths) %>% 
  group_by(States, cod) %>% 
  mutate(deaths_smooth = rollmean(deaths, k=5,fill="extend"))

dat_state4 = rbind(dat_state2, dat_state3) %>% 
  mutate(date=as.Date(date),
         MMWRweek(date))%>% 
  left_join(spop3, by=c("MMWRyear"="year","States")) %>% 
  mutate(Population = na_locf(Population))

plot_counts_CA = ggplot()+
  theme_bw()+
  geom_line(data=dat_state4%>% filter(States=="California"), aes(x=date, y=deaths_smooth),color="blue")+
  geom_line(data=dat_state %>% filter(States=="California"), aes(x=date, y=deaths_smooth),color="black")+
  geom_vline(xintercept=as.Date('2020-12-06'),linetype="dashed")+
  facet_wrap(~cod,scales="free",ncol=2)+
  labs(x=NULL, y="Deaths",title="California")
plot_counts_CA
ggsave(plot=plot_counts_CA, "national_counts_CA.png",height=7,width=13,units="in")

plot_rates_CA = ggplot()+
  theme_bw()+
  geom_line(data=dat_state4 %>% filter(States=="California"), aes(x=date, y=deaths_smooth/Population*100000),color="blue")+
  geom_line(data=dat_state %>% filter(States=="California"), aes(x=date, y=deaths_smooth/Population*100000),color="black")+
  geom_vline(xintercept=as.Date('2020-12-06'),linetype="dashed")+
  facet_wrap(~cod,scales="free",ncol=2)+
  labs(x=NULL, y="Mortality Rate",title="California")
plot_rates_CA
ggsave(plot=plot_rates_CA, "national_rates_CA.png",height=7,width=13,units="in")


plot_counts_NY = ggplot()+
  geom_line(data=dat_state %>% filter(States=="New York"), aes(x=date, y=deaths_smooth),color="blue")+
  geom_line(data=dat_state4%>% filter(States=="New York"), aes(x=date, y=deaths_smooth),color="black")+
  facet_wrap(~cod,scales="free",ncol=2)+
  labs(x=NULL, y="Deaths",title="New York")
plot_counts_NY
ggsave(plot=plot_counts_NY, "national_counts_NY.png",height=7,width=13,units="in")

plot_rates_NY = ggplot()+
  geom_line(data=dat_state %>% filter(States=="New York"), aes(x=date, y=deaths_smooth/Population*100000),color="blue")+
  geom_line(data=dat_state4 %>% filter(States=="New York"), aes(x=date, y=deaths_smooth/Population*100000),color="black")+
  facet_wrap(~cod,scales="free",ncol=2)+
  labs(x=NULL, y="Mortality Rate",title="New York")
plot_rates_NY
ggsave(plot=plot_rates_NY, "national_rates_NY.png",height=7,width=13,units="in")

plot_counts_TX = ggplot()+
  geom_line(data=dat_state %>% filter(States=="Texas"), aes(x=date, y=deaths_smooth),color="blue")+
  geom_line(data=dat_state4%>% filter(States=="Texas"), aes(x=date, y=deaths_smooth),color="black")+
  facet_wrap(~cod,scales="free",ncol=2)+
  labs(x=NULL, y="Deaths",title="Texas")
plot_counts_TX
ggsave(plot=plot_counts_TX, "national_counts_TX.png",height=7,width=13,units="in")

plot_rates_TX = ggplot()+
  geom_line(data=dat_state %>% filter(States=="Texas"), aes(x=date, y=deaths_smooth/Population*100000),color="blue")+
  geom_line(data=dat_state4 %>% filter(States=="Texas"), aes(x=date, y=deaths_smooth/Population*100000),color="black")+
  facet_wrap(~cod,scales="free",ncol=2)+
  labs(x=NULL, y="Mortality Rate",title="Texas")
plot_rates_TX
ggsave(plot=plot_rates_TX, "national_rates_TX.png",height=7,width=13,units="in")


# Combine all data --------------------------------------------------------

old_dat = rbind(dat_nat %>% mutate(States="National"),dat_state) %>% 
  mutate(cod = factor(cod, levels=c("cancer_uc","cancer_mc","diabetes_uc","diabetes_mc","alzheimer_uc","alzheimer_mc"),
                      labels=c("Cancer (Underlying)","Cancer (Multiple)","Diabetes (Underlying)","Diabetes (Multiple)",
                               "Alzheimer's (Underlying)","Alzheimer's (Multiple)")),
         States = factor(States,levels=c("National","New York","Texas","California")))
new_dat = rbind(dat_nat4 %>% mutate(States="National"),dat_state4)%>% 
  mutate(cod = factor(cod, levels=c("cancer_uc","cancer_mc","diabetes_uc","diabetes_mc","alzheimer_uc","alzheimer_mc"),
                      labels=c("Cancer (Underlying)","Cancer (Multiple)","Diabetes (Underlying)","Diabetes (Multiple)",
                               "Alzheimer's (Underlying)","Alzheimer's (Multiple)")),
         States = factor(States,levels=c("National","New York","Texas","California")))

#post_2020 = new_dat %>% select(date,"state" =States, cod, deaths, deaths_smooth, "population_size"=Population)
#saveRDS(post_2020,"Data/post_2020_data.rds")

plot_rates = ggplot()+
  theme_bw()+
  geom_line(data=new_dat, aes(x=date, y=deaths_smooth/Population*100000),color="blue")+
  geom_line(data=old_dat, aes(x=date, y=deaths_smooth/Population*100000),color="black")+
  geom_vline(xintercept=as.Date('2020-12-06'),linetype="dashed")+
  facet_wrap(~cod+States,scales="free",ncol=4)+
  labs(x=NULL, y="Mortality rate per 100,000 population")
plot_rates 
ggsave(plot=plot_rates,"long term mortality rates.png",height=8,width=13,units="in")

plot_counts = ggplot()+
  theme_bw()+
  geom_line(data=new_dat, aes(x=date, y=deaths_smooth),color="blue")+
  geom_line(data=old_dat, aes(x=date, y=deaths_smooth),color="black")+
  geom_vline(xintercept=as.Date('2020-12-06'),linetype="dashed")+
  facet_wrap(~cod+States,scales="free",ncol=4)+
  labs(x=NULL, y="Mortality rate per 100,000 population")
plot_counts
