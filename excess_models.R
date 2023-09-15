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


dat = readRDS("Weekly_Mort_wCancer.rds") %>% 
  filter(!is.na(Population),MMWRdate!='2020-12-27') #remove incomplete week of data 
table(dat$staters,useNA='always')

dat_national = dat %>% 
  select(-staters, -year) %>% 
  group_by(MMWRdate) %>% 
  summarise_all(sum) %>% 
  ungroup () %>% 
  mutate_at(vars(resp_uc:hiv_mc),
            list(ma = ~ zoo::rollapply(., 5, FUN=function(x) round(mean(x, na.rm=TRUE)),fill=NA))) %>% 
  drop_na()
 

dat_ny = dat %>% 
  filter(staters=="NY") %>% 
  ungroup() %>% 
  mutate_at(vars(resp_uc:hiv_mc),
            list(ma = ~ zoo::rollapply(., 5, FUN=function(x) round(mean(x, na.rm=TRUE)),fill=NA))) %>% 
  drop_na()

dat_ca = dat %>% 
  filter(staters=="CA") %>% 
  ungroup() %>% 
  mutate_at(vars(resp_uc:hiv_mc),
            list(ma = ~ zoo::rollapply(., 5, FUN=function(x) round(mean(x, na.rm=TRUE)),fill=NA))) %>% 
  drop_na()

dat_tx = dat %>% 
  filter(staters=="TX") %>% 
  ungroup() %>% 
  mutate_at(vars(resp_uc:hiv_mc),
            list(ma = ~ zoo::rollapply(., 5, FUN=function(x) round(mean(x, na.rm=TRUE)),fill=NA))) %>% 
  drop_na()


#function for estimating excess mortality using negative binomial regression 

excess_model = function(cod, data_set,state,cod_format,cod_type){
  data = data_set %>% 
  mutate(time=row_number(),
         time_sq = time^2,
         cos1= cos(2*pi/52.17*time),
         sin1= sin(2*pi/52.17*time))

data_filter = data %>% filter(MMWRdate<='2020-03-01')

gfit <- MASS::glm.nb(data_filter[[cod]] ~ time + time_sq + cos1 + sin1 +offset(log(Population)), data=data_filter)



## we are sampling from multivariate normal distribution
## whose mean corresponds to their estimated values
## and using their corresponding variance-covariance matrix
nsim <- 10000
mm <- MASS::mvrnorm(nsim, mu=coef(gfit), Sigma=vcov(gfit),empirical=TRUE)

# use pre-March data to predict forward
pred_samp <- apply(mm, 1, function(x) {
  ## propagating uncertainty in the estimated mean
  pp <- exp(x[1] + 
              x[2] * data$time +
              x[3] * data$time_sq +
              x[4] * data$cos1 +
              x[5] * data$sin1+
              log(data$Population))
  ## adding negative binomial noise
rnbinom(length(pp), mu=pp, size=gfit$theta)

})

#find lwr and upr estimates of prediction
lwr <- apply(pred_samp, 1, quantile, 0.025, na.rm=TRUE)
upr <- apply(pred_samp, 1, quantile, 0.975, na.rm=TRUE)
pred <- exp(predict(gfit, newdata=data))

results = data.frame(
  state=state,
  cod=cod,
  cod_format = cod_format,
  cod_type=cod_type,
  date=as.Date(data$MMWRdate),
  obs=data[[cod]],
  population=data$Population,
  pred=pred,
  lwr=lwr,
  upr=upr) %>% 
  mutate(excess= obs-pred,
         obs_rate = obs/population*100000,
         pred_rate = pred/population*100000,
         lwr_rate = lwr/population*100000,
         upr_rate = upr/population*100000,
         excess_rate = excess/population*100000)

write.csv(results,paste0("model_estimates/excess_",cod,"_",state,".csv"))
}




# National Models ---------------------------------------------------

cause = c("cancer_mc_ma","cancer_pancreas_mc_ma","cancer_lung_mc_ma","cancer_breast_mc_ma","cancer_colorectal_mc_ma","cancer_blood_mc_ma","cancer_stomach_mc_ma",
          "diabetes_mc_ma","alzheimer_mc_ma","ihd_mc_ma","cvd_mc_ma","lung_mc_ma","kidney_mc_ma","hiv_mc_ma")

cause_uc = c("cancer_uc_ma","cancer_pancreas_uc_ma","cancer_lung_uc_ma","cancer_breast_uc_ma","cancer_colorectal_uc_ma","cancer_blood_uc_ma","cancer_stomach_uc_ma",
          "diabetes_uc_ma","alzheimer_uc_ma","ihd_uc_ma","cvd_uc_ma","lung_uc_ma","kidney_uc_ma","hiv_uc_ma")

format = c("Cancer","Pancreatic Cancer","Lung Cancer","Breast Cancer","Colorectal Cancer","Blood Cancer","Stomach Cancer",
           "Diabetes","Alzheimer's","Ischemic Heart Disease","Cerebrovascular Disease","Chronic Lower Respiratory Diseases","Kidney Disease","HIV")

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



