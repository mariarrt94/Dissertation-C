##### Dissertation Maria Reyes Retana - Code for creating main databases: regression and graphs 

##### Libraries #####

library(tidyverse)
library(readxl)
library(xlsx)
library(lubridate)

rm(list = ls())
options(scipen = 999)

##### Read data #####

load('Outputs/Data_tidy_dissertation.RData')

load('Outputs/Data_women_index.RData')

hfa_girls_05 <- read_xlsx("Inputs/hfa-girls-z-who-2007-exp.xlsx", sheet = "hfa 0-5") %>% 
  select(Month, M, S, StDev)

hfa_girls_519 <- read_xlsx("Inputs/hfa-girls-z-who-2007-exp.xlsx", sheet = "hfa_girls_z_WHO 2007_exp") %>% 
  select(Month, M, S, StDev)

hfa_boys_05 <- read_xlsx("Inputs/hfa-boys-z-who-2007-exp.xlsx", sheet = "hfa 0-5") %>% 
  select(Month, M, S, StDev)

hfa_boys_519 <- read_xlsx("Inputs/hfa-boys-z-who-2007-exp.xlsx", sheet = "hfa_boys_z_WHO 2007_exp") %>% 
  select(Month, M, S, StDev)

##### Create general anthopometric measures WHO #####

hfa_girls <- hfa_girls_05 %>% 
  rbind(hfa_girls_519)

hfa_boys <- hfa_boys_05 %>% 
  rbind(hfa_boys_519)

##### Create base with children and parents

mom_base <- aux_adul %>% filter(sex == 0) %>% 
  left_join(women_index_res, by = c("year", "folio", "folio_uni", "pid_link_uni")) %>% 
  rename(age_mom = ls02_2, spanish_mom = spanish, indigenous_mom = indigenous, worked_12_mom = worked_12, 
         edu_mom = ed06, married_mom = married, income_c_mom = income_c, height_mom = sa07_21, weight_mom = sa08_21, 
         test_score_mom = test_score, decision_mom = decision_points, HH_mom = HH, pid_link_mom = pid_link_uni, index_mom = PC1) %>% 
  select(year, pid_link_mom, age_mom, spanish_mom, indigenous_mom, worked_12_mom, edu_mom, married_mom, income_c_mom, 
         height_mom, weight_mom, test_score_mom, decision_mom, HH_mom, index_mom, dummy_div) %>% 
  distinct()
  
dad_base <- aux_adul %>% filter(sex == 1) %>% 
  left_join(women_index_res, by = c("year", "folio", "folio_uni", "pid_link_uni")) %>% 
  rename(age_dad = ls02_2, spanish_dad = spanish, indigenous_dad = indigenous, worked_12_dad = worked_12, 
         edu_dad = ed06, married_dad = married, income_c_dad = income_c, height_dad = sa07_21, weight_dad = sa08_21, 
         test_score_dad = test_score, decision_dad = decision_points, HH_dad = HH, pid_link_dad = pid_link_uni, index_dad = PC1) %>% 
  select(year, pid_link_dad, age_dad, spanish_dad, indigenous_dad, worked_12_dad, edu_dad, married_dad, income_c_dad, 
         height_dad, weight_dad, test_score_dad, decision_dad, HH_dad, index_dad, dummy_div) %>% 
  distinct()

# here we need to merge the data from the parents to the children and create anthropocentric indicators, also scale test.

base_child <- aux_child %>% 
  left_join(mom_base, by = c("year", "pid_link_mom")) %>% 
  left_join(dad_base, by = c("year", "pid_link_dad")) %>% 
  mutate(decision_women = decision_mom - decision_dad) %>% 
  distinct()