##### Dissertation Maria Reyes Retana - 
# This code merges tidy data and the women index, also adds the height for age information to 
# have full information needed for the anaylisis

##### Libraries #####

library(tidyverse)
library(readxl)
library(xlsx)
library(lubridate)
library(panelr)
library(foreign)

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
         test_score_mom = test_score, decision_mom = decision_points, HH_mom = HH, pid_link_mom = pid_link_uni, rel_hh_mom = ls05_1, 
         PC1tot_mom = PC1tot, PC2tot_mom = PC2tot, PC1money_mom = PC1money, PC1ch_mom = PC1ch) %>% 
  select(year, pid_link_mom, age_mom, spanish_mom, indigenous_mom, worked_12_mom, edu_mom, married_mom, income_c_mom, 
         height_mom, weight_mom, test_score_mom, decision_mom, rel_hh_mom, HH_mom, dummy_div, PC1tot_mom, PC2tot_mom, PC1money_mom, 
         PC1ch_mom) %>% 
  distinct()
  
dad_base <- aux_adul %>% filter(sex == 1) %>% 
  left_join(women_index_res, by = c("year", "folio", "folio_uni", "pid_link_uni")) %>% 
  rename(age_dad = ls02_2, spanish_dad = spanish, indigenous_dad = indigenous, worked_12_dad = worked_12, 
         edu_dad = ed06, married_dad = married, income_c_dad = income_c, height_dad = sa07_21, weight_dad = sa08_21, 
         test_score_dad = test_score, decision_dad = decision_points, HH_dad = HH, pid_link_dad = pid_link_uni, 
         PC1tot_dad = PC1tot, PC2tot_dad = PC2tot, rel_hh_dad = ls05_1,  PC1money_dad = PC1money, PC1ch_dad = PC1ch) %>% 
  select(year, pid_link_dad, age_dad, spanish_dad, indigenous_dad, worked_12_dad, edu_dad, married_dad, income_c_dad, 
         height_dad, weight_dad, test_score_dad, decision_dad,rel_hh_dad, HH_dad, PC1tot_dad, PC2tot_dad, PC1money_dad, 
         PC1ch_dad) %>% 
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
  group_by(ls02_2, year) %>% 
  mutate(test_score_z = scale(test_score)) %>% 
  ungroup() %>% 
  group_by(ls02_2, year, sex) %>% 
  mutate(height_z = scale(height)) %>% 
  mutate(hfa_z = (height - Median)/StDev) %>% 
  filter(rel_hh_mom == 1 | rel_hh_mom == 2 | rel_hh_dad == 1 | rel_hh_dad == 2) %>% 
  select(-c("ls03_21", "ls03_22", "date_born")) %>% 
  unique() %>% 
  left_join(summary_household, by = c("folio_uni", "year")) %>% 
  mutate(income_crea = case_when(income_com != 0 ~ income_com, 
                                 income_com == 0 & !is.na(income_c_mom) & !is.na(income_c_dad) ~ income_c_mom + income_c_dad,
                                 income_com == 0 & !is.na(income_c_mom) & is.na(income_c_dad) ~ income_c_mom, 
                                 income_com == 0 & is.na(income_c_mom) &!is.na(income_c_dad) ~ income_c_dad, 
                                 TRUE ~ NA_real_), 
         income_crea_pc = income_crea/number_persons,
         log_income_crea = case_when(income_crea == 0 | income_crea < 0 ~ income_crea,
                                     income_crea != 0 & !is.na(income_crea) ~ log(income_crea),
                                     is.na(income_crea)~ NA_real_, 
                                     TRUE ~ NA_real_),
         log_income_crea_pc = case_when(income_crea_pc == 0 | income_crea_pc < 0 ~ income_crea_pc,
                                        income_crea_pc != 0 & !is.na(income_crea_pc) ~ log(income_crea_pc),
                                        is.na(income_crea_pc)~ NA_real_, 
                                        TRUE ~ NA_real_)) %>% 
  select(year, ent, folio_uni, pid_link_uni, sex, ls02_2, spanish, school_att, worked_12, edn09, 
         height, test_score, test_score_z, hfa_z, height_z, pid_link_mom, pid_link_dad, age_mom, spanish_mom, worked_12_mom, edu_mom, married_mom, 
         test_score_mom, decision_mom, height_mom, rel_hh_mom, HH_mom, dummy_div, PC1tot_mom, PC2tot_mom, PC1money_mom, 
         PC1ch_mom, age_dad, spanish_dad, worked_12_dad, edu_dad, height_dad, test_score_dad, income_c_dad,
         decision_dad, hfa_z, test_score_z, income_crea, log_income_crea, income_crea_pc, log_income_crea_pc, children, number_persons)

##### Select necessary databases #####

rm(list=setdiff(ls(), c("base_child", "mom_base", "dad_base", "hfa", "decisions", "summary_dec", "aux_adul", "summary_household")))

# base_child_sta <- base_child %>% 
#   mutate(letter = substr(pid_link_uni, 7, 7), 
#          pid_link = case_when(letter == "A" ~ paste(substr(pid_link_uni, 1, 6), "1", substr(pid_link_uni, 8, 11), sep = ""),
#                    letter == "B" ~ paste(substr(pid_link_uni, 1, 6), "2", substr(pid_link_uni, 8, 11), sep = ""),
#                    letter == "C" ~ paste(substr(pid_link_uni, 1, 6), "2", substr(pid_link_uni, 8, 11), sep = ""),
#                    TRUE ~ NA_character_),
#          folio = case_when(letter == "A" ~ paste(substr(folio_uni, 1, 6), "1", substr(folio_uni, 8, 9), sep = ""),
#                            letter == "B" ~ paste(substr(folio_uni, 1, 6), "2", substr(folio_uni, 8, 9), sep = ""),
#                            letter == "C" ~ paste(substr(folio_uni, 1, 6), "2", substr(folio_uni, 8, 9), sep = ""),
#                            TRUE ~ NA_character_)) %>% 
#   select(-c("letter"))
# 
# write.dta(base_child_sta, "Outputs/base_child.dta")
