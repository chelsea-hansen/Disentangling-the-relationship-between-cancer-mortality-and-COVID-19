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


#Upload data 

cause = c("cancer_mc_ma","cancer_pancreas_mc_ma","cancer_lung_mc_ma","cancer_breast_mc_ma","cancer_colorectal_mc_ma","cancer_blood_mc_ma","cancer_stomach_mc_ma",
          "diabetes_mc_ma","alzheimer_mc_ma","ihd_mc_ma","cvd_mc_ma","lung_mc_ma","kidney_mc_ma","hiv_mc_ma")

cause_uc = c("cancer_uc_ma","cancer_pancreas_uc_ma","cancer_lung_uc_ma","cancer_breast_uc_ma","cancer_colorectal_uc_ma","cancer_blood_uc_ma","cancer_stomach_uc_ma",
             "diabetes_uc_ma","alzheimer_uc_ma","ihd_uc_ma","cvd_uc_ma","lung_uc_ma","kidney_uc_ma","hiv_uc_ma")

format = c("Cancer","Pancreatic Cancer","Lung Cancer","Breast Cancer","Colorectal Cancer","Blood Cancer","Stomach Cancer",
           "Diabetes","Alzheimer's","Ischemic Heart Disease","Cerebrovascular Disease","Chronic Lower Respiratory Diseases","Kidney Disease","HIV")

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



# Main text  --------------------------------------------------------------
# Table 1 - Descriptive ---------------------------------------------------
#This is actually the same code for the supplemental table
#just take the underlying columns for main text 

cods = c("cancer_uc", "cancer_mc", 
         "cancer_pancreas_uc", "cancer_pancreas_mc",
         "cancer_lung_uc", "cancer_lung_mc",
         "cancer_colorectal_uc", "cancer_colorectal_mc",
         "cancer_breast_uc", "cancer_breast_mc",
         "cancer_blood_uc", "cancer_blood_mc",
         "diabetes_uc", "diabetes_mc",
         "alzheimer_uc", "alzheimer_mc",
         "ihd_uc","ihd_mc",
         "kidney_uc", "kidney_mc"
         )

formats = c(rep("Cancer (C00-C99)",2),rep("Pancreatic Cancer (C25)",2),rep("Lung Cancer (C34)",2),rep("Colorectal Cancer (C18-C20)",2),rep("Breast Cancer (C50)",2),rep("Blood Cancer (C81-C96)",2),
            rep("Diabetes (E10-E14)",2),rep("Alzheimer's (G30)",2),rep("Ischemic Heart Disease (I20-I25)",2),rep("Kidney Disease (N00-N07,N17-N19,N25-N28)",2))

indiv_mort = readRDS("Indiv_Mort_wCancer.rds") #inidivdual-level data, not publicly available

recent = indiv_mort %>% filter((MMWRdate >='2019-03-01'&MMWRdate<='2019-12-31')|(MMWRdate >='2020-03-01' & MMWRdate <='2020-12-31'))%>% 
  mutate(nursing_home = ifelse(placdth=="nursing home",1,0),
         out_hosp = ifelse(placdth %in% c("DOA","home","outpatient/ER"),1,0),
         age_years = ifelse(ageunits !="years",0,age),
         all =1)

recent2 = recent %>% 
  pivot_longer(cols=c(cancer_uc, cancer_mc, 
                      cancer_pancreas_uc, cancer_pancreas_mc,
                      cancer_lung_uc, cancer_lung_mc,
                      cancer_colorectal_uc, cancer_colorectal_mc,
                      cancer_breast_uc, cancer_breast_mc,
                      cancer_blood_uc, cancer_blood_mc,
                      diabetes_uc, diabetes_mc,
                      alzheimer_uc, alzheimer_mc,
                      ihd_uc, ihd_mc,
                      kidney_uc, kidney_mc
                     ),
               names_to="cod",values_to="cod_ind") %>% 
  select(staters, MMWRyear, age_years, nursing_home, out_hosp, cod, cod_ind)

recent3 = recent2 %>% 
  filter(cod_ind==1) %>% 
  group_by(MMWRyear, cod) %>% 
  summarize(deaths = sum(cod_ind),
            mean_age = round(mean(age_years)),
            Q1 = quantile(age_years, prob=c(.25)),
            Q3 = quantile(age_years, prob=c(.75)))

recent3_home = recent2 %>% 
  filter(cod_ind==1) %>% 
  group_by(MMWRyear, cod, out_hosp) %>% 
  summarize(deaths = sum(cod_ind)) %>% 
  pivot_wider(names_from=out_hosp, values_from=deaths) %>% 
  ungroup() %>% 
  mutate(total = `0`+`1`,
         Percent = round(`1`/total*100,1)) %>% 
  select(MMWRyear, cod, "% out-of-hospital"=Percent)

recent3_nursing = recent2 %>% 
  filter(cod_ind==1) %>% 
  group_by(MMWRyear, cod, nursing_home) %>% 
  summarize(deaths = sum(cod_ind)) %>% 
  pivot_wider(names_from=nursing_home, values_from=deaths) %>% 
  ungroup() %>% 
  mutate(total = `0`+`1`,
         Percent = round(`1`/total*100,1)) %>% 
  select(MMWRyear, cod, "% nursing home"=Percent)


desc_table_dat = recent3 %>% 
  left_join(recent3_home, by=c("MMWRyear","cod")) %>% 
  left_join(recent3_nursing, by=c("MMWRyear","cod")) %>% 
  mutate(`Cause of Death (ICD-10)` = factor(cod, levels=c(cods),labels=c(formats)),
         cod_type = ifelse(grepl("_mc",cod),"Multiple Cause","Underlying Cause"),
         cod_type = factor(cod_type, levels=c("Underlying Cause","Multiple Cause")),
         Year = as.character(MMWRyear))


desc_table <- flextable::tabulator(
  desc_table_dat,
  rows = c("Year","Cause of Death (ICD-10)"),
  columns = "cod_type",
  `No. Deaths`=as_paragraph(as_chunk(as.integer(deaths))),
  `Mean age, years (IQR)`=as_paragraph(as_chunk(paste0(mean_age," (",Q1,"-",Q3,")"))),
  `%Home/ER` = as_paragraph(as_chunk(as.integer(`% out-of-hospital`))),
  `%Nursing Home`=as_paragraph(as_chunk(as.integer(`% nursing home`))))


table = flextable::as_flextable(desc_table) %>% set_table_properties(layout = "autofit")
table
#save_as_docx(table, path="table1.docx")
table = table%>% as_raster()

gg_tab <- ggplot() + 
  theme_void() + 
  annotation_custom(rasterGrob(table), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)
gg_tab
ggsave(plot=gg_tab,"tables/Table1.pdf",height=8,width=16, units="in")



# Table 2 - Excess Mortality  ---------------------------------------------
table_dat = excess_mort %>% 
  filter(date>='2020-03-01', 
         cod_format %notin% c("Lung Cancer","Breast Cancer","Kidney Disease","HIV","Ischemic Heart Disease")) %>% 
  mutate(wave1 = ifelse(date<'2020-06-28',1,0),
         wave2 = ifelse(date>='2020-06-28' & date<'2020-10-04',1,0),
         wave3 = ifelse(date>='2020-10-04',1,0)) %>% 
  filter((state=="New York" & wave1==1)|
           (state=="Texas" & wave2==1)|
           (state=="California"&wave3==1)|
           (state=="National")) %>% 
  mutate(time_period = ifelse(state=="New York","Wave 1 (Mar 1-Jun 27)",
                              ifelse(state=="Texas","Wave 2 (Jun 28-Oct 3)",
                                     ifelse(state=="California","Wave 3 (Oct 4-Dec 6)","Overall (Mar 1-Dec 6)")))) %>% 
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
            Over_lower=round((Observed/Lower)*100-100),
            Over_upper=round((Observed/Upper)*100-100),
            excess_rate=round(Excess/pop*100000,1)) %>% 
  mutate(State=factor(state,levels=c("National","New York","Texas","California")),
         `Cause of Death`=cod_format, 
         Obs=as.integer(Observed),Exp=as.integer(Expected),Excess=as.integer(Excess),
         Excess=paste0(Excess,significant),
         `Time Period` = factor(time_period, levels=c("Overall (Mar 1-Dec 6)","Wave 1 (Mar 1-Jun 27)", "Wave 2 (Jun 28-Oct 3)","Wave 3 (Oct 4-Dec 6)"))) %>% 
  ungroup() %>% 
  select(State, `Cause of Death`, cod_type, Excess,Excess_lower, Excess_upper, Over_base,Over_lower,Over_upper, `Time Period`)


  table <- tabulator(
    table_dat,
    rows = c("Cause of Death","State","Time Period"),
    columns = "cod_type",
    `Excess Deaths`=as_paragraph(as_chunk(Excess)),
    `% Over Baseline`=as_paragraph(as_chunk(Over_base)))
  
  table = flextable::as_flextable(table)
  table = flextable::autofit(table)
  #save_as_docx(table, path="table2.docx")
  table =  footnote(x=table, i=1,j=3,ref_symbols = c("*"),value = as_paragraph(c("confidence interval does not include zero")))%>% as_raster()
  
  gg_tab <- ggplot() + 
    theme_void() + 
    annotation_custom(rasterGrob(table), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)
  gg_tab
  ggsave(plot=gg_tab,"tables/main_Table2.pdf",height=8,width=14, units="in")


# Figure 1: COVID deaths  -----------------------------------------------------------

cov_death = readRDS("Weekly_Mort_wCancer.rds") %>% 
  filter(staters %in% c("NY","TX","CA"),MMWRdate>='2020-03-01') %>% 
  select(MMWRdate, staters, covid19_uc, covid19_mc)

nat_cov = readRDS("Weekly_Mort_wCancer.rds") %>% 
  filter(MMWRdate>='2020-03-01') %>% 
  group_by(MMWRdate) %>% 
  summarize(covid19_uc = sum(covid19_uc),
            covid19_mc = sum(covid19_mc)) %>% 
  ungroup() %>% 
  mutate(staters="National") %>% 
  select(MMWRdate, staters, covid19_uc, covid19_mc)

cov_death2 = rbind(cov_death, nat_cov) %>% 
  #mutate(state=factor(staters,levels=c("National","NY","TX","CA"),labels=c("National","New York","Texas","California"))) %>% 
  group_by(staters) %>% 
  mutate_at(vars(covid19_uc:covid19_mc),
            list(ma = ~ zoo::rollapply(., 5, FUN=function(x) round(mean(x, na.rm=TRUE)),fill=NA))) %>% 
  drop_na() %>% 
  ungroup() %>% 
  mutate(MMWRdate = as.Date(MMWRdate),
         wave = ifelse(MMWRdate<'2020-06-14',"Wave 1",
                       ifelse(MMWRdate<='2020-09-12',"Wave 2", "Wave 3")))


wave_times = data.frame(date1 = c("2020-03-01","2020-06-14","2020-09-27"), 
                   date2=c("2020-06-14","2020-09-27","2020-12-13"),
                   wave=c("Wave 1","Wave 2", "Wave 3")) %>% 
  mutate(date1=as.Date(date1),date2=as.Date(date2))


#cases 
case_data_2020_states <- read.csv("Weekly_United_States_COVID-19_Cases_and_Deaths_by_State_-_ARCHIVED.csv") %>% 
  filter(state %in% c("NY","TX","CA")) %>% 
  mutate(MMWRdate = as.Date(date_updated, format="%m/%d/%Y")+3) %>% 
  select(MMWRdate, state, new_cases) %>% 
  filter(MMWRdate >='2020-03-15', MMWRdate <='2020-12-13')

case_data_2020_all <- read.csv("Weekly_United_States_COVID-19_Cases_and_Deaths_by_State_-_ARCHIVED.csv") %>% 
  mutate(MMWRdate = as.Date(date_updated, format="%m/%d/%Y")+3) %>% 
  group_by(MMWRdate) %>% 
  summarize(new_cases=sum(new_cases)) %>% 
  ungroup() %>% 
  mutate(state="National") %>% 
  select(MMWRdate, state, new_cases) %>% 
  filter(MMWRdate >='2020-03-15', MMWRdate <='2020-12-13')


case_data = rbind(case_data_2020_states, case_data_2020_all) #%>% 
 # mutate(state=factor(state,levels=c("National","NY","TX","CA"),labels=c("National","New York","Texas","California"))) %>% 
 # arrange(state)

cov_death2 = cov_death2 %>% 
  left_join(case_data, by=c("MMWRdate","staters"="state")) %>% 
  mutate(state=factor(staters, levels=c("National","NY","TX","CA"),labels=c("National","New York","Texas","California")))

waves = ggplot(data=cov_death2)+
  theme_bw()+
  geom_rect(data=wave_times,aes(xmin=date1, xmax = date2, 
                           ymin = -Inf, ymax = Inf, 
                           fill = wave),alpha=0.3)+
  scale_fill_brewer(name = NULL, palette="YlOrBr")+
  geom_line(aes(x=MMWRdate, y=covid19_mc_ma/1000, color="Multiple Cause",linetype="Multiple Cause"),linewidth=1.5)+
  geom_line(aes(x=MMWRdate, y=covid19_uc_ma/1000, color="Underlying Cause",linetype="Underlying Cause"),linewidth=1)+
  geom_line(aes(x=MMWRdate, y=new_cases/100000,linetype="Reported Cases",color="Reported Cases"),size=1)+
  scale_y_continuous(
    name = "Deaths (in thousands)",
    sec.axis = sec_axis( trans=~.*.1, name="Cases (in millions)",labels=scales::comma),labels=scales::comma) +
  facet_wrap(~state, nrow=2, scales="free")+
  scale_color_manual(name=NULL, breaks = c("Multiple Cause","Underlying Cause","Reported Cases"),values=c("skyblue3","black","black"))+
  scale_linetype_manual(name=NULL, breaks = c("Multiple Cause","Underlying Cause","Reported Cases"), values=c('solid','solid','dotted'))+
  geom_vline(xintercept=as.Date('2020-04-12'),linetype='dashed',color='red',size=1)+
  geom_vline(xintercept=as.Date('2020-07-26'),linetype='dashed',color='red',size=1)+
  geom_vline(xintercept=as.Date('2020-12-13'),linetype='dashed',color='red',size=1)+
  theme(panel.grid = element_blank())+
  labs(x=NULL, y="COVID-19 Deaths")+
  theme(legend.text = element_text(size=12),
        strip.text.x = element_text(size = 15),
        axis.title.y = element_text(size=15))
waves
ggsave(plot=waves,"figures/Figure1.png",height=8,width=14, units="in")



# Figure 2-5: baseline and excess time series -----------------------------

for(s in 1:length(states)){
  cancer = ggplot(data=excess_mort %>% filter(state==states[s],cod_format=="Cancer"))+
    theme_bw()+
    geom_ribbon(aes(x=date, ymin=lwr, ymax=upr),alpha=0.2,fill="orange")+
    geom_line(aes(x=date, y=obs, group=1),color="black")+
    geom_line(aes(x=date, y=pred, group=1),color="orange",size=1)+
    geom_vline(xintercept=as.Date('2020-04-12'),linetype='dashed',color='red')+
    geom_vline(xintercept=as.Date('2020-07-26'),linetype='dashed',color='red')+
    geom_vline(xintercept=as.Date('2020-12-13'),linetype='dashed',color='red')+
    facet_grid(rows=vars(factor(cod_type, levels=c('Underlying Cause',"Multiple Cause"))),scales="free")+
    labs(x=NULL, y=NULL,title="Cancer")
  pancreatic = ggplot(data=excess_mort %>% filter(state==states[s],cod_format=="Pancreatic Cancer"))+
    theme_bw()+
    geom_ribbon(aes(x=date, ymin=lwr, ymax=upr),alpha=0.2,fill="orange")+
    geom_line(aes(x=date, y=obs, group=1),color="black")+
    geom_line(aes(x=date, y=pred, group=1),color="orange",size=1)+
    geom_vline(xintercept=as.Date('2020-04-12'),linetype='dashed',color='red')+
    geom_vline(xintercept=as.Date('2020-07-26'),linetype='dashed',color='red')+
    geom_vline(xintercept=as.Date('2020-12-13'),linetype='dashed',color='red')+
    facet_grid(rows=vars(factor(cod_type, levels=c('Underlying Cause',"Multiple Cause"))),scales="free")+
    labs(x=NULL, y=NULL,title="Pancreatic Cancer")
  colorectal = ggplot(data=excess_mort %>% filter(state==states[s],cod_format=="Colorectal Cancer"))+
    theme_bw()+
    geom_ribbon(aes(x=date, ymin=lwr, ymax=upr),alpha=0.2,fill="orange")+
    geom_line(aes(x=date, y=obs, group=1),color="black")+
    geom_line(aes(x=date, y=pred, group=1),color="orange",size=1)+
    geom_vline(xintercept=as.Date('2020-04-12'),linetype='dashed',color='red')+
    geom_vline(xintercept=as.Date('2020-07-26'),linetype='dashed',color='red')+
    geom_vline(xintercept=as.Date('2020-12-13'),linetype='dashed',color='red')+
    facet_grid(rows=vars(factor(cod_type, levels=c('Underlying Cause',"Multiple Cause"))),scales="free")+
    labs(x=NULL, y=NULL,title="Colorectal Cancer")
  diabetes = ggplot(data=excess_mort %>% filter(state==states[s],cod_format=="Diabetes"))+
    theme_bw()+
    geom_ribbon(aes(x=date, ymin=lwr, ymax=upr),alpha=0.2,fill="orange")+
    geom_line(aes(x=date, y=obs, group=1),color="black")+
    geom_line(aes(x=date, y=pred, group=1),color="orange",size=1)+
    geom_vline(xintercept=as.Date('2020-04-12'),linetype='dashed',color='red')+
    geom_vline(xintercept=as.Date('2020-07-26'),linetype='dashed',color='red')+
    geom_vline(xintercept=as.Date('2020-12-13'),linetype='dashed',color='red')+
    facet_grid(rows=vars(factor(cod_type, levels=c('Underlying Cause',"Multiple Cause"))),scales="free")+
    labs(x=NULL, y=NULL,title="Diabetes")
  alzheimer = ggplot(data=excess_mort %>% filter(state==states[s],cod_format=="Alzheimer's"))+
    theme_bw()+
    geom_ribbon(aes(x=date, ymin=lwr, ymax=upr),alpha=0.2,fill="orange")+
    geom_line(aes(x=date, y=obs, group=1),color="black")+
    geom_line(aes(x=date, y=pred, group=1),color="orange",size=1)+
    geom_vline(xintercept=as.Date('2020-04-12'),linetype='dashed',color='red')+
    geom_vline(xintercept=as.Date('2020-07-26'),linetype='dashed',color='red')+
    geom_vline(xintercept=as.Date('2020-12-13'),linetype='dashed',color='red')+
    facet_grid(rows=vars(factor(cod_type, levels=c('Underlying Cause',"Multiple Cause"))),scales="free")+
    labs(x=NULL, y=NULL,title="Alzheimer's")
  blood = ggplot(data=excess_mort %>% filter(state==states[s],cod_format=="Blood Cancer"))+
    theme_bw()+
    geom_ribbon(aes(x=date, ymin=lwr, ymax=upr),alpha=0.2,fill="orange")+
    geom_line(aes(x=date, y=obs, group=1),color="black")+
    geom_line(aes(x=date, y=pred, group=1),color="orange",size=1)+
    geom_vline(xintercept=as.Date('2020-04-12'),linetype='dashed',color='red')+
    geom_vline(xintercept=as.Date('2020-07-26'),linetype='dashed',color='red')+
    geom_vline(xintercept=as.Date('2020-12-13'),linetype='dashed',color='red')+
    facet_grid(rows=vars(factor(cod_type, levels=c('Underlying Cause',"Multiple Cause"))),scales="free")+
    labs(x=NULL, y=NULL,title="Hematologic Cancer")
  figure=plot_grid(cancer, diabetes, alzheimer, pancreatic, colorectal, blood, ncol=3,labels = "auto")
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
  ggsave(plot=figure_w_title, paste0("figures/Figure",s+1,"_",states[s],".png"),height=8,width=14,units="in")
  ggsave(plot=figure_w_title, paste0("figures/Figure",s+1,"_",states[s],".tiff"),height=8,width=14,units="in")
}


