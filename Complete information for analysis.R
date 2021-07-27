##### Dissertation Maria Reyes Retana - 
# This code merges tidy data and the women index, also adds the height for age information to 
# have full information needed for the anaylisis

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
  select(Month, M, Median, S, StDev)

hfa_girls_519 <- read_xlsx("Inputs/hfa-girls-z-who-2007-exp.xlsx", sheet = "hfa_girls_z_WHO 2007_exp") %>% 
  rename(Median = SD0) %>% 
  select(Month, M, Median, S, StDev)

hfa_boys_05 <- read_xlsx("Inputs/hfa-boys-z-who-2007-exp.xlsx", sheet = "hfa 0-5") %>% 
  select(Month, M, Median, S, StDev)

hfa_boys_519 <- read_xlsx("Inputs/hfa-boys-z-who-2007-exp.xlsx", sheet = "hfa_boys_z_WHO 2007_exp") %>% 
  rename(Median = SD0) %>% 
  select(Month, M, Median, S, StDev)

##### Create general anthopometric measures WHO #####

hfa_girls <- hfa_girls_05 %>% 
  rbind(hfa_girls_519) %>% 
  mutate(sex = 0)

hfa_boys <- hfa_boys_05 %>% 
  rbind(hfa_boys_519) %>% 
  mutate(sex = 1)

hfa <- hfa_girls %>% 
  rbind(hfa_boys)

##### Create base with children and parents

mom_base <- aux_adul %>% filter(sex == 0) %>% 
  left_join(women_index_res, by = c("year", "folio", "folio_uni", "pid_link_uni")) %>% 
  rename(age_mom = ls02_2, spanish_mom = spanish, indigenous_mom = indigenous, worked_12_mom = worked_12, 
         edu_mom = ed06, married_mom = married, income_c_mom = income_c, height_mom = sa07_21, weight_mom = sa08_21, 
         test_score_mom = test_score, decision_mom = decision_points, HH_mom = HH, pid_link_mom = pid_link_uni) %>% 
  select(year, pid_link_mom, age_mom, spanish_mom, indigenous_mom, worked_12_mom, edu_mom, married_mom, income_c_mom, 
         height_mom, weight_mom, test_score_mom, decision_mom, HH_mom, dummy_div, PC1tot, PC1work, PC1house, PC1agency) %>% 
  distinct()
  
dad_base <- aux_adul %>% filter(sex == 1) %>% 
  left_join(women_index_res, by = c("year", "folio", "folio_uni", "pid_link_uni")) %>% 
  rename(age_dad = ls02_2, spanish_dad = spanish, indigenous_dad = indigenous, worked_12_dad = worked_12, 
         edu_dad = ed06, married_dad = married, income_c_dad = income_c, height_dad = sa07_21, weight_dad = sa08_21, 
         test_score_dad = test_score, decision_dad = decision_points, HH_dad = HH, pid_link_dad = pid_link_uni, 
         PC1tot_dad = PC1tot, PC1work_dad = PC1work, PC1house_dad = PC1house, PC1agency_dad = PC1agency) %>% 
  select(year, pid_link_dad, age_dad, spanish_dad, indigenous_dad, worked_12_dad, edu_dad, married_dad, income_c_dad, 
         height_dad, weight_dad, test_score_dad, decision_dad, HH_dad, PC1tot_dad, PC1work_dad, PC1house_dad, PC1agency_dad) %>% 
  distinct()

# here we need to merge the data from the parents to the children and create anthropocentric indicators, also scale test.

base_child <- aux_child %>%
  filter(ls02_2 <16) %>% 
  left_join(mom_base, by = c("year", "pid_link_mom")) %>% 
  left_join(dad_base, by = c("year", "pid_link_dad")) %>% 
  mutate(decision_women = decision_mom - decision_dad) %>% 
  distinct() %>% 
  left_join(hfa, by = c("Month", "sex")) %>%
  rename(height = sa07_21, weight = sa08_21) %>% 
  mutate(hfa_z = (height - Median)/StDev, 
         test_score_z = scale(test_score, center = TRUE, scale = TRUE))

##### Select necessary databases #####

rm(list=setdiff(ls(), c("base_child", "mom_base", "dad_base", "hfa", "decisions", "summary_dec", "aux_adul")))

base_child_p <- panel_data(base_child, id = pid_link_uni, wave = year) %>% 
  ungroup() 

##### Panel #####

base_factor_dec <- base_child_p %>% 
  filter(!is.na(decision_mom)) %>% 
  filter(ls02_2 > 4) %>% 
  filter(ls02_2 < 16) %>% 
  mutate(decision_mom = factor(decision_mom, ordered = TRUE, levels = c(0, 1, 2, 3, 4, 5, 6, 7, 8,
                                                                        9, 10, 11, 12)))

mom_panel <- panel_data(mom_base, id = pid_link_mom, wave = year) %>% 
  ungroup() %>% 
  mutate(log_income_mom = log(income_c_mom))

dad_panel <- panel_data(dad_base, id = pid_link_dad, wave = year) %>% 
  ungroup() %>% 
  mutate(log_income_dad = log(income_c_dad)) 
