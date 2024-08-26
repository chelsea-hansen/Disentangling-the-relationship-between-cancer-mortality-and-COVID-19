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



dat_national =  readRDS("data/weekly_mort_US.rds")%>% 
  mutate_at(vars(covid19_uc:kidney_mc),
            list(ma = ~ zoo::rollapply(., 5, FUN=function(x) round(mean(x, na.rm=TRUE)),fill=NA))) %>% 
  drop_na()
 

dat_ny =  readRDS("data/weekly_mort_states.rds")%>% 
  filter(staters=="NY") %>% 
  ungroup() %>% 
  mutate_at(vars(covid19_uc:kidney_mc),
            list(ma = ~ zoo::rollapply(., 5, FUN=function(x) round(mean(x, na.rm=TRUE)),fill=NA))) %>% 
  drop_na()

dat_ca = readRDS("data/weekly_mort_states.rds")%>% 
  filter(staters=="CA") %>% 
  ungroup() %>% 
  mutate_at(vars(covid19_uc:kidney_mc),
            list(ma = ~ zoo::rollapply(., 5, FUN=function(x) round(mean(x, na.rm=TRUE)),fill=NA))) %>% 
  drop_na()

dat_tx = readRDS("data/weekly_mort_states.rds")%>% 
  filter(staters=="TX") %>% 
  ungroup() %>% 
  mutate_at(vars(covid19_uc:kidney_mc),
            list(ma = ~ zoo::rollapply(., 5, FUN=function(x) round(mean(x, na.rm=TRUE)),fill=NA))) %>% 
  drop_na()


#function for estimating excess mortality using negative binomial regression 

excess_model = function(cod, data_set,state,cod_format,cod_type){
  data = data_set %>% 
  mutate(time=row_number(),
         time_sq = time^2,
         time_cub = time^3,
         cos1= cos(2*pi/52.17*time),
         sin1= sin(2*pi/52.17*time))

data_filter = data %>% filter(MMWRdate<='2020-03-01')

mod1 <- MASS::glm.nb(data_filter[[cod]] ~ time  + cos1 + sin1 +offset(log(Population)), data=data_filter)
mod2 <- MASS::glm.nb(data_filter[[cod]] ~ time + time_sq + cos1 + sin1 +offset(log(Population)), data=data_filter)
mod3 <- MASS::glm.nb(data_filter[[cod]] ~ time + time_sq + time_cub + cos1 + sin1 +offset(log(Population)), data=data_filter)



## we are sampling from multivariate normal distribution
## whose mean corresponds to their estimated values
## and using their corresponding variance-covariance matrix
nsim <- 10000
mm1 <- MASS::mvrnorm(nsim, mu=coef(mod1), Sigma=vcov(mod1),empirical=TRUE)

# use pre-March data to predict forward
pred_samp1 <- apply(mm1, 1, function(x) {
  ## propagating uncertainty in the estimated mean
  pp <- exp(x[1] + 
              x[2] * data$time +
              x[3] * data$cos1 +
              x[4] * data$sin1+
              log(data$Population))
  ## adding negative binomial noise
rnbinom(length(pp), mu=pp, size=mod1$theta)

})

#find lwr and upr estimates of prediction
lwr1 <- apply(pred_samp1, 1, quantile, 0.025, na.rm=TRUE)
upr1 <- apply(pred_samp1, 1, quantile, 0.975, na.rm=TRUE)
pred1 <- exp(predict(mod1, newdata=data))

results1 = data.frame(
  state=state,
  cod=cod,
  cod_format = cod_format,
  cod_type=cod_type,
  date=as.Date(data$MMWRdate),
  obs=data[[cod]],
  population=data$Population,
  pred=pred1,
  lwr=lwr1,
  upr=upr1) %>% 
  mutate(excess= obs-pred,
         obs_rate = obs/population*100000,
         pred_rate = pred/population*100000,
         lwr_rate = lwr/population*100000,
         upr_rate = upr/population*100000,
         excess_rate = excess/population*100000,
         model="model 1")


mm2 <- MASS::mvrnorm(nsim, mu=coef(mod2), Sigma=vcov(mod2),empirical=TRUE)

# use pre-March data to predict forward
pred_samp2 <- apply(mm2, 1, function(x) {
  ## propagating uncertainty in the estimated mean
  pp <- exp(x[1] + 
              x[2] * data$time +
              x[3] * data$time_sq +
              x[4] * data$cos1 +
              x[5] * data$sin1+
              log(data$Population))
  ## adding negative binomial noise
  rnbinom(length(pp), mu=pp, size=mod2$theta)
  
})

#find lwr and upr estimates of prediction
lwr2 <- apply(pred_samp2, 1, quantile, 0.025, na.rm=TRUE)
upr2 <- apply(pred_samp2, 1, quantile, 0.975, na.rm=TRUE)
pred2 <- exp(predict(mod2, newdata=data))

results2 = data.frame(
  state=state,
  cod=cod,
  cod_format = cod_format,
  cod_type=cod_type,
  date=as.Date(data$MMWRdate),
  obs=data[[cod]],
  population=data$Population,
  pred=pred2,
  lwr=lwr2,
  upr=upr2) %>% 
  mutate(excess= obs-pred,
         obs_rate = obs/population*100000,
         pred_rate = pred/population*100000,
         lwr_rate = lwr/population*100000,
         upr_rate = upr/population*100000,
         excess_rate = excess/population*100000,
         model="model 2")


mm3 <- MASS::mvrnorm(nsim, mu=coef(mod3), Sigma=vcov(mod3),empirical=TRUE)

# use pre-March data to predict forward
pred_samp3 <- apply(mm3, 1, function(x) {
  ## propagating uncertainty in the estimated mean
  pp <- exp(x[1] + 
              x[2] * data$time +
              x[3] * data$time_sq +
              x[4] * data$time_cub +
              x[5] * data$cos1 +
              x[6] * data$sin1+
              log(data$Population))
  ## adding negative binomial noise
  rnbinom(length(pp), mu=pp, size=mod3$theta)
  
})

#find lwr and upr estimates of prediction
lwr3 <- apply(pred_samp3, 1, quantile, 0.025, na.rm=TRUE)
upr3 <- apply(pred_samp3, 1, quantile, 0.975, na.rm=TRUE)
pred3 <- exp(predict(mod3, newdata=data))

results3 = data.frame(
  state=state,
  cod=cod,
  cod_format = cod_format,
  cod_type=cod_type,
  date=as.Date(data$MMWRdate),
  obs=data[[cod]],
  population=data$Population,
  pred=pred3,
  lwr=lwr3,
  upr=upr3) %>% 
  mutate(excess= obs-pred,
         obs_rate = obs/population*100000,
         pred_rate = pred/population*100000,
         lwr_rate = lwr/population*100000,
         upr_rate = upr/population*100000,
         excess_rate = excess/population*100000,
         model="model 3")


results = rbind(results1, results2, results3)
AIC = data.frame(model1=AIC(mod1),model2=AIC(mod2), model3=AIC(mod3),cause = cod, place=state)

write.csv(results,paste0("model_selection/excess_",cod,"_",state,".csv"))
write.csv(AIC,paste0("model_selection/aic_",cod,"_",state,".csv"))

}




# National Models ---------------------------------------------------

cause = c("cancer_mc_ma","cancer_pancreas_mc_ma","cancer_lung_mc_ma","cancer_breast_mc_ma","cancer_colorectal_mc_ma","cancer_blood_mc_ma",
          "diabetes_mc_ma","alzheimer_mc_ma","ihd_mc_ma","kidney_mc_ma")

cause_uc = c("cancer_uc_ma","cancer_pancreas_uc_ma","cancer_lung_uc_ma","cancer_breast_uc_ma","cancer_colorectal_uc_ma","cancer_blood_uc_ma",
          "diabetes_uc_ma","alzheimer_uc_ma","ihd_uc_ma","kidney_uc_ma")

format = c("Cancer","Pancreatic Cancer","Lung Cancer","Breast Cancer","Colorectal Cancer","Blood Cancer",
           "Diabetes","Alzheimer's","Ischemic Heart Disease","Kidney Disease")



for(c in 1:length(cause)){
  excess_model(cause[c],dat_national,"National",format[c],"Multiple Cause")
}

for(c in 1:length(cause_uc)){
  excess_model(cause_uc[c],dat_national,"National",format[c],"Underlying Cause")
}


# New York ----------------------------------------------------------------
for(c in 1:length(cause)){
  excess_model(cause[c],dat_ny,"New York",format[c],"Multiple Cause")
}

for(c in 1:length(cause_uc)){
  excess_model(cause_uc[c],dat_ny,"New York",format[c],"Underlying Cause")
}


# California --------------------------------------------------------------
for(c in 1:length(cause)){
  excess_model(cause[c],dat_ca,"California",format[c],"Multiple Cause")
}

for(c in 1:length(cause_uc)){
  excess_model(cause_uc[c],dat_ca,"California",format[c],"Underlying Cause")
}


# Texas -------------------------------------------------------------------
for(c in 1:length(cause)){
  excess_model(cause[c],dat_tx,"Texas",format[c],"Multiple Cause")
}

for(c in 1:length(cause_uc)){
  excess_model(cause_uc[c],dat_tx,"Texas",format[c],"Underlying Cause")
}



# Plot AIC results  -------------------------------------------------------



cause2 = c("cancer_mc_ma","cancer_uc_ma","cancer_pancreas_mc_ma","cancer_pancreas_uc_ma",
           "cancer_lung_mc_ma","cancer_lung_uc_ma","cancer_breast_mc_ma","cancer_breast_uc_ma",
           "cancer_colorectal_mc_ma","cancer_colorectal_uc_ma","cancer_blood_mc_ma","cancer_blood_uc_ma",
          "diabetes_mc_ma","diabetes_uc_ma","alzheimer_mc_ma","alzheimer_uc_ma",
          "ihd_mc_ma","ihd_uc_ma","kidney_mc_ma","kidney_uc_ma")

format2 = c("Cancer (Multiple)","Cancer (Underlying)","Pancreatic (Multiple)","Pancreatic (Underlying)",
           "Lung (Multiple)","Lung (Underlying)","Breast (Multiple)","Breast (Underlying)",
           "Colorectal (Multiple)","Colorectal (Underlying)","Hematological (Multiple)","Hematological (Underlying)",
           "Diabetes (Multiple)","Diabetes (Underlying)","Alzheimer's (Multiple)","Alzheimer's (Underlying)",
           "IHD (Multiple)","IHD (Underlying)","Kidney (Multiple)","Kidney (Underlying)")


dat_aic = list.files(path=file.path(getwd(), "model_selection"),pattern=".csv")
dat_aic = dat_aic[grepl("aic",dat_aic)]
dat_aic <- sapply(dat_aic, read.csv, simplify=FALSE) %>% bind_rows() 
dat_step=dat_aic

dat_aic = dat_aic %>% 
  mutate(cod = factor(cause,levels=cause2,labels=format2),
         state = factor(place, levels=c("National","New York","California","Texas")),
         min_aic = case_when(pmin(model1,model2, model3)==model1~"model1",
                             pmin(model1,model2, model3)==model2~"model2",
                             pmin(model1,model2, model3)==model3~"model3"))%>% 
  pivot_longer(cols=c(model1,model2,model3),names_to="model",values_to="aic") %>% 
  mutate(min = ifelse(model==min_aic,"yes","no"))


aic_plot = ggplot(data=dat_aic)+
  geom_tile(aes(x=model, y=cod,fill=min),alpha=0.5)+
  geom_text(aes(x=model, y=cod,label=round(aic)))+
  facet_grid(cols=vars(state))+
  labs(x=NULL, y=NULL)
aic_plot
ggsave(plot=aic_plot,"aic plot.png",height=8,width=14,units="in")
getwd()


dat_step = dat_step %>% 
  mutate(cod = factor(cause,levels=cause2,labels=format2),
         state = factor(place, levels=c("National","New York","California","Texas")),
         step1 = case_when(model2+2<model1~"model 2",
                           model2+2>=model1~"model 1"),
         step2 = case_when(step1=="model 1"~"model 1",
                           step1=="model 2"&model3+2>=model2~"model 2",
                           step1=="model 2"&model3+2<model2~"model 3"))

dat_step = dat_step %>% select(state, cod, cause,"final_model"=step2)
write.csv(dat_step,"final model selection.csv")


coverage = read.csv("cross_validation/coverage proportion - state.csv")%>% 
  mutate(Coverage=round(prop,2)) %>% 
  select(cod, model,state, Coverage)

dat_step = dat_step %>% 
  left_join(coverage, by=c("cause"="cod","final_model" ="model","state")) %>% 
  mutate(state = factor(state, levels=c("National","New York","Texas","California")),
         final_model = factor(final_model,levels=c("model 1","model 2","model 3"),
                        labels=c("Model1", "Model2","Model3")))

aic_plot = ggplot(data=dat_step)+
  theme_bw()+
  geom_tile(aes(x=final_model, y=reorder(cod,desc(cod)),fill="Selected Model"))+
  geom_text(aes(x=final_model, y=reorder(cod,desc(cod)),label=Coverage),color="white")+
  facet_grid(cols=vars(state))+
  scale_fill_manual(name=NULL,values="navy")+
  labs(x=NULL,y=NULL)+
  theme(axis.text=element_text(size=15),
        strip.text.x=element_text(size=15),
        legend.text=element_text(size=15),
        legend.position="top")
aic_plot
ggsave(plot=aic_plot, "model selection based with coverage proportion.png",height=8,width=13)
