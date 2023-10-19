rm(list=ls())

library(tidyverse)
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

'%notin%' = Negate('%in%')


#order for supplemental figures 
cause = c("cancer_mc_ma","diabetes_mc_ma","alzheimer_mc_ma","ihd_mc_ma","kidney_mc_ma",
          "cancer_pancreas_mc_ma","cancer_lung_mc_ma","cancer_breast_mc_ma","cancer_colorectal_mc_ma","cancer_blood_mc_ma")

cause_uc = c("cancer_uc_ma","diabetes_uc_ma","alzheimer_uc_ma","ihd_uc_ma","kidney_uc_ma",
             "cancer_pancreas_uc_ma","cancer_lung_uc_ma","cancer_breast_uc_ma","cancer_colorectal_uc_ma","cancer_blood_uc_ma")

format = c("Cancer","Diabetes","Alzheimer's","Ischemic Heart Disease","Kidney Disease",
           "Pancreatic Cancer","Lung Cancer","Breast Cancer","Colorectal Cancer","Blood Cancer")


excess_mort <- list.files(path = "./model_estimates", pattern=".csv")
excess_mort <- paste("model_estimates/",excess_mort, sep="")
excess_mort <- sapply(excess_mort, read.csv, simplify=FALSE) %>% bind_rows() %>%
  filter(!is.na(state)) %>% 
  mutate(date=as.Date(date),
         cod_format = factor(cod_format, levels=c(format)))
states=c("National","New York","Texas","California")
type = c("Multiple Cause","Underlying Cause")

excess_mort = excess_mort %>% filter(!is.na(cod_format))


# Supplemental Materials --------------------------------------------------

#table 1 - see main text 

# Table 2: Excess deaths  ----------------------------------------------

table_dat = excess_mort %>% 
  filter(date>='2020-03-01',!is.na(cod_format)) %>% 
  mutate(wave1 = ifelse(date<'2020-06-28',1,0),
         wave2 = ifelse(date>='2020-06-28' & date<'2020-10-04',1,0),
         wave3 = ifelse(date>='2020-10-04',1,0)) %>% 
  mutate(time_period = ifelse(date>='2020-03-01'&date<'2020-06-28',"Wave 1 (Mar 1-Jun 27)",
                              ifelse(date>='2020-06-28'&date<'2020-10-04',"Wave 2 (Jun 28-Oct 3)",
                                     ifelse(date>='2020-10-04',"Wave 3 (Oct 4-Dec 6)",NA))),
         cod_format = factor(cod_format,
                             levels=c("Cancer","Pancreatic Cancer","Lung Cancer","Breast Cancer","Colorectal Cancer","Blood Cancer",
                                      "Diabetes","Alzheimer's","Ischemic Heart Disease","Kidney Disease"))) %>% 
  group_by(state, time_period, cod_format, cod_type) %>% 
  summarize(pop=unique(population),
            Observed = sum(obs),
            Expected = round(sum(pred)),
            Lower = sum(lwr),
            Upper = sum(upr),
            Excess = Observed - Expected,
            Excess_lower = Observed - Lower,
            Excess_upper = Observed - Upper, 
            significant = between(0,Excess_upper, Excess_lower),
            significant = ifelse(significant==TRUE,"","*"),
            Over_base=round((Observed/Expected)*100-100),
            Over_lower=round((Observed/Upper)*100-100),
            Over_upper=round((Observed/Lower)*100-100),
            excess_rate=round(Excess/pop*100000,1)) %>% 
  mutate(State=factor(state,levels=c("National","New York","Texas","California")),
         `Cause of Death`=cod_format, 
         Obs=as.integer(Observed),Exp=as.integer(Expected),Excess=as.integer(Excess),
         Excess=paste0(Excess,significant),
         `Time Period` = factor(time_period, levels=c("Overall (Mar 1-Dec 6)","Wave 1 (Mar 1-Jun 27)", "Wave 2 (Jun 28-Oct 3)","Wave 3 (Oct 4-Dec 6)"))) %>% 
  ungroup() %>% 
  select(State, `Cause of Death`, cod_type, Excess, Over_base,Over_lower,Over_upper, `Time Period`)

table_dat2 = excess_mort %>% 
  #table_dat2 = blood %>% 
  filter(date>='2020-03-01') %>% 
  group_by(state, cod_format, cod_type) %>% 
  summarize(pop=unique(population),
            Observed = sum(obs),
            Expected = round(sum(pred)),
            Lower = sum(lwr),
            Upper = sum(upr),
            Excess = Observed - Expected,
            Excess_lower = Observed - Lower,
            Excess_upper = Observed - Upper, 
            significant = between(0,Excess_upper, Excess_lower),
            significant = ifelse(significant==TRUE,"","*"),
            Over_base=round((Observed/Expected)*100-100),
            Over_lower=round((Observed/Upper)*100-100),
            Over_upper=round((Observed/Lower)*100-100),
            excess_rate=round(Excess/pop*100000,1)) %>% 
  mutate(time_period = "Overall (Mar 1-Dec 6)",
         State=factor(state,levels=c("National","New York","Texas","California")),
         `Cause of Death`=cod_format, 
         `Time Period` = time_period,
         Obs=as.integer(Observed),Exp=as.integer(Expected),Excess=as.integer(Excess),Excess=paste0(Excess,significant)) %>% 
  ungroup() %>% 
  select(State,  `Cause of Death`, cod_type, Excess, Over_base,Over_lower,Over_upper,`Time Period`)

table_dat3 = rbind(table_dat, table_dat2) %>% 
  mutate(`Time Period` = factor(`Time Period`, levels=c("Overall (Mar 1-Dec 6)","Wave 1 (Mar 1-Jun 27)", "Wave 2 (Jun 28-Oct 3)","Wave 3 (Oct 4-Dec 6)")))

for(s in 1:length(states)){
  table <- tabulator(
    table_dat3 %>% filter(State==states[s]),
    rows = c("State","Cause of Death","Time Period"),
    columns = "cod_type",
    `Excess Deaths`=as_paragraph(as_chunk(Excess)),
    `% Over Baseline`=as_paragraph(as_chunk(Over_base)))

  table = flextable::as_flextable(table)
  table = flextable::autofit(table)
  save_as_docx(table, path=paste0("Appendix_Table",s+1,"_",states[s],".docx"))
 # table =  footnote(x=table, i=1,j=3,ref_symbols = c("*"),value = as_paragraph(c("confidence interval does not include zero")))%>% as_raster
  
  #gg_tab <- ggplot() + 
    #theme_void() + 
    #annotation_custom(rasterGrob(table), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)
  #gg_tab
 # ggsave(plot=gg_tab, paste0("tables/appendix_Table",s+1,"_",states[s],".pdf"),height=11,width=9, units="in")
}


# Figures 1-4: baseline and excess time series -------------------------------------------------------------

for(s in 1:length(states)){
  cancer = ggplot(data=excess_mort %>% filter(state==states[s],cod_format=="Cancer",date>='2017-01-01'))+
    theme_bw()+
    geom_ribbon(aes(x=date, ymin=lwr, ymax=upr),alpha=0.2,fill="orange")+
    geom_line(aes(x=date, y=obs, group=1),color="black")+
    geom_line(aes(x=date, y=pred, group=1),color="orange",size=1)+
    geom_vline(xintercept=as.Date('2020-04-12'),linetype='dashed',color='red')+
    geom_vline(xintercept=as.Date('2020-07-26'),linetype='dashed',color='red')+
    geom_vline(xintercept=as.Date('2020-12-13'),linetype='dashed',color='red')+
    facet_grid(rows=vars(factor(cod_type, levels=c('Underlying Cause',"Multiple Cause"))),scales="free")+
    labs(x=NULL, y=NULL,title="Cancer")
  pancreatic = ggplot(data=excess_mort %>% filter(state==states[s],cod_format=="Pancreatic Cancer",date>='2017-01-01'))+
    theme_bw()+
    geom_ribbon(aes(x=date, ymin=lwr, ymax=upr),alpha=0.2,fill="orange")+
    geom_line(aes(x=date, y=obs, group=1),color="black")+
    geom_line(aes(x=date, y=pred, group=1),color="orange",size=1)+
    geom_vline(xintercept=as.Date('2020-04-12'),linetype='dashed',color='red')+
    geom_vline(xintercept=as.Date('2020-07-26'),linetype='dashed',color='red')+
    geom_vline(xintercept=as.Date('2020-12-13'),linetype='dashed',color='red')+
    facet_grid(rows=vars(factor(cod_type, levels=c('Underlying Cause',"Multiple Cause"))),scales="free")+
    labs(x=NULL, y=NULL,title="Pancreatic Cancer")
  lung = ggplot(data=excess_mort %>% filter(state==states[s],cod_format=="Lung Cancer",date>='2017-01-01'))+
    theme_bw()+
    geom_ribbon(aes(x=date, ymin=lwr, ymax=upr),alpha=0.2,fill="orange")+
    geom_line(aes(x=date, y=obs, group=1),color="black")+
    geom_line(aes(x=date, y=pred, group=1),color="orange",size=1)+
    geom_vline(xintercept=as.Date('2020-04-12'),linetype='dashed',color='red')+
    geom_vline(xintercept=as.Date('2020-07-26'),linetype='dashed',color='red')+
    geom_vline(xintercept=as.Date('2020-12-13'),linetype='dashed',color='red')+
    facet_grid(rows=vars(factor(cod_type, levels=c('Underlying Cause',"Multiple Cause"))),scales="free")+
    labs(x=NULL, y=NULL,title="Lung Cancer")
  colorectal = ggplot(data=excess_mort %>% filter(state==states[s],cod_format=="Colorectal Cancer",date>='2017-01-01'))+
    theme_bw()+
    geom_ribbon(aes(x=date, ymin=lwr, ymax=upr),alpha=0.2,fill="orange")+
    geom_line(aes(x=date, y=obs, group=1),color="black")+
    geom_line(aes(x=date, y=pred, group=1),color="orange",size=1)+
    geom_vline(xintercept=as.Date('2020-04-12'),linetype='dashed',color='red')+
    geom_vline(xintercept=as.Date('2020-07-26'),linetype='dashed',color='red')+
    geom_vline(xintercept=as.Date('2020-12-13'),linetype='dashed',color='red')+
    facet_grid(rows=vars(factor(cod_type, levels=c('Underlying Cause',"Multiple Cause"))),scales="free")+
    labs(x=NULL, y=NULL,title="Colorectal Cancer")
  breast = ggplot(data=excess_mort %>% filter(state==states[s],cod_format=="Breast Cancer",date>='2017-01-01'))+
    theme_bw()+
    geom_ribbon(aes(x=date, ymin=lwr, ymax=upr),alpha=0.2,fill="orange")+
    geom_line(aes(x=date, y=obs, group=1),color="black")+
    geom_line(aes(x=date, y=pred, group=1),color="orange",size=1)+
    geom_vline(xintercept=as.Date('2020-04-12'),linetype='dashed',color='red')+
    geom_vline(xintercept=as.Date('2020-07-26'),linetype='dashed',color='red')+
    geom_vline(xintercept=as.Date('2020-12-13'),linetype='dashed',color='red')+
    facet_grid(rows=vars(factor(cod_type, levels=c('Underlying Cause',"Multiple Cause"))),scales="free")+
    labs(x=NULL, y=NULL,title="Breast Cancer")
  blood = ggplot(data=excess_mort %>% filter(state==states[s],cod_format=="Blood Cancer",date>='2017-01-01'))+
    theme_bw()+
    geom_ribbon(aes(x=date, ymin=lwr, ymax=upr),alpha=0.2,fill="orange")+
    geom_line(aes(x=date, y=obs, group=1),color="black")+
    geom_line(aes(x=date, y=pred, group=1),color="orange",size=1)+
    geom_vline(xintercept=as.Date('2020-04-12'),linetype='dashed',color='red')+
    geom_vline(xintercept=as.Date('2020-07-26'),linetype='dashed',color='red')+
    geom_vline(xintercept=as.Date('2020-12-13'),linetype='dashed',color='red')+
    facet_grid(rows=vars(factor(cod_type, levels=c('Underlying Cause',"Multiple Cause"))),scales="free")+
    labs(x=NULL, y=NULL,title="Blood Cancer")
  diabetes = ggplot(data=excess_mort %>% filter(state==states[s],cod_format=="Diabetes",date>='2017-01-01'))+
    theme_bw()+
    geom_ribbon(aes(x=date, ymin=lwr, ymax=upr),alpha=0.2,fill="orange")+
    geom_line(aes(x=date, y=obs, group=1),color="black")+
    geom_line(aes(x=date, y=pred, group=1),color="orange",size=1)+
    geom_vline(xintercept=as.Date('2020-04-12'),linetype='dashed',color='red')+
    geom_vline(xintercept=as.Date('2020-07-26'),linetype='dashed',color='red')+
    geom_vline(xintercept=as.Date('2020-12-13'),linetype='dashed',color='red')+
    facet_grid(rows=vars(factor(cod_type, levels=c('Underlying Cause',"Multiple Cause"))),scales="free")+
    labs(x=NULL, y=NULL,title="Diabetes")
  alzheimer = ggplot(data=excess_mort %>% filter(state==states[s],cod_format=="Alzheimer's",date>='2017-01-01'))+
    theme_bw()+
    geom_ribbon(aes(x=date, ymin=lwr, ymax=upr),alpha=0.2,fill="orange")+
    geom_line(aes(x=date, y=obs, group=1),color="black")+
    geom_line(aes(x=date, y=pred, group=1),color="orange",size=1)+
    geom_vline(xintercept=as.Date('2020-04-12'),linetype='dashed',color='red')+
    geom_vline(xintercept=as.Date('2020-07-26'),linetype='dashed',color='red')+
    geom_vline(xintercept=as.Date('2020-12-13'),linetype='dashed',color='red')+
    facet_grid(rows=vars(factor(cod_type, levels=c('Underlying Cause',"Multiple Cause"))),scales="free")+
    labs(x=NULL, y=NULL,title="Alzheimer's")
  ihd= ggplot(data=excess_mort %>% filter(state==states[s],cod_format=="Ischemic Heart Disease",date>='2017-01-01'))+
    theme_bw()+
    geom_ribbon(aes(x=date, ymin=lwr, ymax=upr),alpha=0.2,fill="orange")+
    geom_line(aes(x=date, y=obs, group=1),color="black")+
    geom_line(aes(x=date, y=pred, group=1),color="orange",size=1)+
    geom_vline(xintercept=as.Date('2020-04-12'),linetype='dashed',color='red')+
    geom_vline(xintercept=as.Date('2020-07-26'),linetype='dashed',color='red')+
    geom_vline(xintercept=as.Date('2020-12-13'),linetype='dashed',color='red')+
    facet_grid(rows=vars(factor(cod_type, levels=c('Underlying Cause',"Multiple Cause"))),scales="free")+
    labs(x=NULL, y=NULL,title="Ischemic Heart Disease")
  kidney = ggplot(data=excess_mort %>% filter(state==states[s],cod_format=="Kidney Disease",date>='2017-01-01'))+
    theme_bw()+
    geom_ribbon(aes(x=date, ymin=lwr, ymax=upr),alpha=0.2,fill="orange")+
    geom_line(aes(x=date, y=obs, group=1),color="black")+
    geom_line(aes(x=date, y=pred, group=1),color="orange",size=1)+
    geom_vline(xintercept=as.Date('2020-04-12'),linetype='dashed',color='red')+
    geom_vline(xintercept=as.Date('2020-07-26'),linetype='dashed',color='red')+
    geom_vline(xintercept=as.Date('2020-12-13'),linetype='dashed',color='red')+
    facet_grid(rows=vars(factor(cod_type, levels=c('Underlying Cause',"Multiple Cause"))),scales="free")+
    labs(x=NULL, y=NULL,title="Kidney Disease") 
  figure=plot_grid(cancer, diabetes, alzheimer,ihd, kidney, pancreatic,lung,colorectal,breast,blood, ncol=5,labels = "auto")
  title <- ggdraw() + 
    draw_label(
      states[s],
      fontface = 'bold',
      size=20,
      x = 0,
      hjust = 0
    ) +
    theme(
      plot.margin = margin(0, 0, 0, 7)
    )
  figure_w_title=plot_grid(
    title, figure,
    ncol = 1,
    rel_heights = c(0.1, 1)
  )
  
   ggsave(plot=figure_w_title, paste0("figures/appendix_Figure",s,"_",states[s],".pdf"),height=8,width=15,units="in")
}

# Figure 5: Correlations ------------------------------------------------------------
excess_counts = excess_mort %>% 
  filter(!is.na(cod_format)) %>% 
  drop_na() %>% 
  select(state, cod_format, cod_type, date, excess) %>% 
  filter(date>='2020-03-01')

cov_death = readRDS("weekly_mort_states.rds") %>% 
  filter(staters %in% c("NY","TX","CA"),MMWRdate>='2020-03-01') %>% 
  select(MMWRdate, staters, covid19_uc, covid19_mc)

nat_cov = readRDS("weekly_mort_US.rds") %>% 
  filter(MMWRdate>='2020-03-01') %>% 
  mutate(staters="National") %>% 
  select(MMWRdate, staters, covid19_uc, covid19_mc)

cov_death2 = rbind(cov_death, nat_cov) %>% 
  mutate(state=factor(staters,levels=c("National","NY","TX","CA"),labels=c("National","New York","Texas","California"))) %>% 
  group_by(staters) %>% 
  mutate_at(vars(covid19_uc:covid19_mc),
            list(ma = ~ zoo::rollapply(., 5, FUN=function(x) round(mean(x, na.rm=TRUE)),fill=NA))) %>% 
  drop_na() %>% 
  ungroup() %>% 
  mutate(MMWRdate = as.Date(MMWRdate),
         wave = ifelse(MMWRdate<'2020-06-14',"Wave 1",
                       ifelse(MMWRdate<='2020-09-12',"Wave 2", "Wave 3")))


covid_deaths = cov_death2 %>% 
  select(MMWRdate, state, covid19_uc_ma,wave)

cases_with_excess = excess_counts %>% 
  left_join(covid_deaths, by=c("state","date"="MMWRdate")) %>% 
  filter(date>='2020-03-10',cod_format %notin% c("Cardiorespiratory + COVID-19"))

for(s in 1:length(states)){
  for(t in 1:length(type)){
  
    plot4 = ggplot(data=cases_with_excess %>% filter(state==states[s],cod_type==type[t],date>='2020-03-01'))+
      theme_bw()+
      geom_point(aes(x=covid19_uc_ma, y=excess,color=wave))+
      scale_color_brewer(name=NULL, palette="Accent")+
      facet_wrap(~cod_format,ncol=5,scales="free")+
      geom_smooth(aes(x=covid19_uc_ma, y=excess), method="lm",se=FALSE)+
      stat_cor(aes(x=covid19_uc_ma, y=excess), method="pearson",size=5)+
      labs(y="Excess Deaths",x="Coded COVID-19 Deaths (UC)",title=paste0(states[s]," - ",type[t]))
    ggsave(plot=plot4, paste0("figures/correlations/appendix_correlation_figures",states[s],"_",type[t],".pdf"),height=8,width=14,units="in")
    
  }}

# Correlations by wave ----------------------------------------------------
for(s in 1:length(states)){
  for(t in 1:length(type)){
    
    plot4 = ggplot(data=cases_with_excess %>% filter(state==states[s],cod_type==type[t],date>='2020-03-01'))+
      theme_bw()+
      geom_point(aes(x=covid19_uc_ma, y=excess,color=wave))+
      scale_color_brewer(name=NULL, palette="Accent")+
      facet_wrap(~cod_format+wave,scales="free")+
      geom_smooth(aes(x=covid19_uc_ma, y=excess), method="lm",se=FALSE)+
      stat_cor(aes(x=covid19_uc_ma, y=excess), method="pearson",size=3)+
      labs(y="Excess Deaths",x="Coded COVID-19 Deaths (UC)",title=paste0(states[s]," - ",type[t]))
    ggsave(plot=plot4, paste0("figures/correlations/appendix_correlation_bywave",states[s],"_",type[t],".pdf"),height=8,width=14,units="in")
    
  }}


# Check UC/MC overlap - for independence in correlations -----------------------------------------------------
indiv_mort = readRDS("Indiv_Mort_wCancer.rds") #indicidual-level data not publicly available 

recent = indiv_mort %>% filter((MMWRdate >='2019-03-01'&MMWRdate<='2019-12-31')|(MMWRdate >='2020-03-01' & MMWRdate <='2020-12-31'))%>% 
  mutate(nursing_home = ifelse(placdth=="nursing home",1,0),
         out_hosp = ifelse(placdth %in% c("DOA","home","outpatient/ER"),1,0),
         age_years = ifelse(ageunits !="years",0,age),
         all =1,
         broad_letter = substr(ucod,1,1),
         cancer_contribute = ifelse(cancer_mc==1 & cancer_uc==0,1,0),
         diabetes_contribute = ifelse(diabetes_mc==1 & diabetes_uc==0,1,0))

cancer = recent %>% filter(MMWRyear==2020, cancer_mc==1) %>% 
  group_by(covid19_uc) %>% 
  tally() %>% 
  mutate(cod="Cancer")

diabetes = recent %>% filter(MMWRyear==2020, diabetes_mc==1) %>% 
  group_by(covid19_uc) %>% 
  tally() %>% 
  mutate(cod="Diabetes")

alzheimers = recent %>% filter(MMWRyear==2020, alzheimer_mc==1) %>% 
  group_by(covid19_uc) %>% 
  tally() %>% 
  mutate(cod="Alheimer's")

cardioresp = recent %>% filter(MMWRyear==2020, cardio_resp_mc==1) %>% 
  group_by(covid19_uc) %>% 
  tally() %>% 
  mutate(cod="Cardiorespiratory")

all = rbind(cancer, diabetes, alzheimers, cardioresp) %>% 
  pivot_wider(names_from=covid19_uc, values_from = n) %>% 
  mutate(total = `1`+`0`,
         percent = `1`/total*100)

table = flextable(all) %>% as_raster()
table
gg_tab <- ggplot() + 
  theme_void() + 
  annotation_custom(rasterGrob(table), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)
gg_tab
ggsave(plot=gg_tab, "correlations/extra_mc deaths with underlying covid.pdf",height=6,width=8, units="in")


# Covid not underlying ----------------------------------------------------


cov_mc = indiv_mort %>% filter(covid19_mc==1, covid19_uc==0) %>% 
  mutate(broad_letter = substr(ucod,1,1))

cov_uc = indiv_mort %>% filter(covid19_mc==1, covid19_uc==1) %>% 
  mutate(broad_letter = substr(ucod,1,1))

cov_mc = cov_mc %>% filter(ageunits=="years")
summary(cov_mc$age)


cov_uc = cov_uc %>% filter(ageunits=="years")
summary(cov_uc$age)

table(cov_mc$broad_letter)
underlying = as.data.frame(table(cov_mc$ucod))


plot_covmc = ggplot(data=cov_mc)+
  geom_bar(aes(x=broad_letter),stat="count")
plot_covmc
ggsave(plot=plot_covmc, "covid not underlying.png",height=8, width=14, units="in")
getwd()



# Figure 9: diabetes and cancer contributing deaths -----------------------



#total cancer deaths by UC/MC 

cancer_deaths = recent %>% filter(cancer_mc==1) %>% 
  group_by(yeardth, cancer_uc) %>%
  tally()

cancer_covid = recent %>% filter(yeardth==2020, (cancer_u_covid19_mc==1|covid19_u_cancer_mc==1))

table(cancer_covid$cancer_u_covid19_mc)
table(cancer_covid$covid19_u_cancer_mc)
13434/16579

diabetes_deaths = recent %>% filter(diabetes_mc==1) %>% 
  group_by(yeardth, diabetes_uc) %>%
  tally()

diabetes_covid = recent %>% filter(yeardth==2020, (diabetes_u_covid19_mc==1|covid19_u_diabetes_mc==1))


table(diabetes_covid$diabetes_u_covid19_mc)
table(diabetes_covid$covid19_u_diabetes_mc)
56749/58505


contributing_cancer = recent %>% filter(cancer_contribute==1)
contributing_diabetes = recent %>% filter(diabetes_contribute==1)

cancer_code = contributing_cancer %>% 
  group_by(MMWRyear, broad_letter) %>% 
  tally() %>% 
  mutate(cod = "Contributing Cancer")
cancer_code

diabetes_code = contributing_diabetes %>% 
  group_by(MMWRyear, broad_letter) %>% 
  tally() %>% 
  mutate(cod = "Contributing Diabetes")
diabetes_code

by_code_year = rbind(cancer_code, diabetes_code)

plot3 = ggplot(by_code_year)+
  theme_bw()+
  geom_bar(aes(x=broad_letter, y=n),stat="identity", color="black",fill="cornflowerblue")+
  facet_grid(cols=vars(MMWRyear),rows=vars(cod),scales="free")+
  labs(y="Deaths",x="ICD-10 Letter",title="Underlying cause when cancer or diabetes are contributing")
plot3
ggsave(plot=plot3, "figures/appendix_Figure9_underlying causes when cancer or diabetes is contributing.png",height=8, width=14, units="in")
ggsave(plot=plot3, "Figure6_underlying causes when cancer or diabetes is contributing.tiff",height=8, width=14, units="in")

