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

'%not_in%' = Negate('%in%')


dat_national =  readRDS("data/weekly_mort_US.rds")%>% 
  mutate_at(vars(covid19_uc:kidney_mc),
            list(ma = ~ zoo::rollapply(., 5, FUN=function(x) round(mean(x, na.rm=TRUE)),fill=NA))) %>% 
  drop_na() 

 #dat_national2 =  readRDS("weekly_mort_US.rds")


dat_ny =  readRDS("data/weekly_mort_states.rds")%>% 
  filter(staters=="NY") %>% 
  ungroup() %>% 
  mutate_at(vars(covid19_uc:kidney_mc),
            list(ma = ~ zoo::rollapply(., 5, FUN=function(x) round(mean(x, na.rm=TRUE)),fill=NA))) %>% 
  drop_na() 


#dat_ny2 =  readRDS("weekly_mort_states.rds")%>% 
  #filter(staters=="NY") 


dat_ca = readRDS("data/weekly_mort_states.rds")%>% 
  filter(staters=="CA") %>% 
  ungroup() %>% 
  mutate_at(vars(covid19_uc:kidney_mc),
            list(ma = ~ zoo::rollapply(., 5, FUN=function(x) round(mean(x, na.rm=TRUE)),fill=NA))) %>% 
  drop_na() 

#dat_ca2 = readRDS("weekly_mort_states.rds")%>% 
  #filter(staters=="CA") 


dat_tx = readRDS("data/weekly_mort_states.rds")%>% 
  filter(staters=="TX") %>% 
  ungroup() %>% 
  mutate_at(vars(covid19_uc:kidney_mc),
            list(ma = ~ zoo::rollapply(., 5, FUN=function(x) round(mean(x, na.rm=TRUE)),fill=NA))) %>% 
  drop_na() 

#dat_tx2 = readRDS("weekly_mort_states.rds")%>% 
  #filter(staters=="TX") 

#cross validation model - only fit data through 2018 and then evaluate 2019 

excess_model = function(cod, data_set,state,cod_format,cod_type){
  data = data_set %>% 
    mutate(time=row_number(),
           time_sq = time^2,
           time_cub = time^3,
           cos1= cos(2*pi/52.17*time),
           sin1= sin(2*pi/52.17*time))
  
  data_filter = data %>% filter(MMWRdate<='2018-12-31')
  
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
  
  write.csv(results,paste0("cross_validation/all_versions/excess_",cod,"_",state,".csv"))
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


# Coverage Proportions  ---------------------------------------------------

cross_val <- list.files(path = "./cross_validation/all_versions", pattern=".csv")
cross_val <- paste("cross_validation/all_versions/",cross_val, sep="")
cross_val <- sapply(cross_val, read.csv, simplify=FALSE) %>% bind_rows()

cov_prop = cross_val %>% 
  filter(date>='2019-01-01',date<='2019-12-31') %>% 
  mutate(date=as.Date(date),
         denominator=1,
         numerator = ifelse(obs>=lwr & obs<=upr,1,0)) %>% 
  group_by(cod,model) %>% 
  summarize(denominator = sum(denominator),
            numerator = sum(numerator)) %>% 
  mutate(prop = numerator/denominator)

write.csv(cov_prop,"cross_validation/coverage proportion - overall.csv")

cov_prop_state = cross_val %>% 
  filter(date>='2019-01-01',date<='2019-12-31') %>% 
  mutate(denominator=1,
         numerator = ifelse(obs>=lwr & obs<=upr,1,0)) %>% 
  group_by(cod,state,model) %>% 
  summarize(denominator = sum(denominator),
            numerator = sum(numerator)) %>% 
  mutate(prop = numerator/denominator)

write.csv(cov_prop_state,"cross_validation/coverage proportion - state.csv")




# Test model on raw data  -------------------------------------------------

excess_model2 = function(cod, data_set,state,cod_format,cod_type){
  data = data_set %>% 
    ungroup() %>% 
    mutate(time=row_number(),
           time_sq = time^2,
           cos1= cos(2*pi/52.17*time),
           sin1= sin(2*pi/52.17*time))
  
  data_filter = data %>% filter(MMWRdate<='2018-12-31')
  
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
  
  write.csv(results,paste0("cross_validation/raw data/excess_",cod,"_",state,".csv"))
}




# National Models ---------------------------------------------------

cause2 = c("cancer_mc","diabetes_mc","alzheimer_mc")

cause_uc2 = c("cancer_uc","diabetes_uc","alzheimer_uc")

format = c("Cancer","Diabetes","Alzheimer's")

for(c in 1:length(cause2)){
  excess_model2(cause2[c],dat_national2,"National",format[c],"Multiple Cause")
}

for(c in 1:length(cause_uc2)){
  excess_model2(cause_uc2[c],dat_national2,"National",format[c],"Underlying Cause")
}


# New York ----------------------------------------------------------------
for(c in 1:length(cause2)){
  excess_model2(cause2[c],dat_ny2,"New York",format[c],"Multiple Cause")
}

for(c in 1:length(cause_uc2)){
  excess_model2(cause_uc2[c],dat_ny2,"New York",format[c],"Underlying Cause")
}


# California --------------------------------------------------------------
for(c in 1:length(cause2)){
  excess_model2(cause2[c],dat_ca2,"California",format[c],"Multiple Cause")
}

for(c in 1:length(cause_uc2)){
  excess_model2(cause_uc2[c],dat_ca2,"California",format[c],"Underlying Cause")
}


# Texas -------------------------------------------------------------------
for(c in 1:length(cause2)){
  excess_model2(cause2[c],dat_tx2,"Texas",format[c],"Multiple Cause")
}

for(c in 1:length(cause_uc2)){
  excess_model2(cause_uc2[c],dat_tx2,"Texas",format[c],"Underlying Cause")
}

raw_mod <- list.files(path = "./cross_validation/raw data", pattern=".csv")
raw_mod <- paste("cross_validation/raw data/",raw_mod, sep="")
raw_mod <- sapply(raw_mod, read.csv, simplify=FALSE) %>% bind_rows()

raw_mod = raw_mod %>% 
  filter(date>='2020-03-01') %>% 
  mutate(wave = ifelse(date<'2020-06-28',"wave1",
                       ifelse(date>='2020-06-28' & date<'2020-10-04',"wave2","wave3")))



raw_table =raw_mod%>% 
  group_by(state, wave, cod) %>% 
  summarize(Observed = sum(obs),
            Expected = round(sum(pred)),
            Lower = sum(lwr),
            Upper = sum(upr),
            Excess = Observed - Expected,
            Excess_lower = Observed - Upper,
            Excess_upper = Observed - Lower,
            Over_base=round((Observed/Expected)*100-100),
            Over_lower=round((Observed/Lower)*100-100),
            Over_upper=round((Observed/Upper)*100-100)) %>% 
  mutate(flag = ifelse((state=="New York"&wave=="wave1")|
                         (state=="Texas"&wave=="wave2")|
                         (state=="California"&wave=="wave3"),1,0)) %>% 
  filter(flag==1)
write.csv(raw_table,"cross_validation/raw data/senstivity with raw data-states.csv")

national = raw_mod%>% 
  group_by(state, cod) %>% 
  summarize(Observed = sum(obs),
            Expected = round(sum(pred)),
            Lower = sum(lwr),
            Upper = sum(upr),
            Excess = Observed - Expected,
            Excess_lower = Observed - Upper,
            Excess_upper = Observed - Lower,
            Over_base=round((Observed/Expected)*100-100),
            Over_lower=round((Observed/Lower)*100-100),
            Over_upper=round((Observed/Upper)*100-100)) %>% 
  filter(state=="National")
write.csv(national,"cross_validation/raw data/senstivity with raw data-national.csv")

