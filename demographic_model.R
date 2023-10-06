rm(list=ls())
library(tidyverse)

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
