##### Dissertation Maria Reyes Retana - Code for creating summary statistics and main databases 

##### Libraries #####

library(tidyverse)
library(readxl)
library(psych)
library(xlsx)
library(lubridate)

rm(list = ls())
options(scipen = 999)

##### Load required information #####

# Raw data sets
load('Outputs/Datasets_dissertation.RData')

# Inflation code to adjust income and spending
source('Inflation.R')

##### Book EA and EN: Cognitive skills #### 

# EN: Children, we have to read the correct answers to assign a rate. 

answer_c <- read_xlsx("Inputs/Codes.xlsx", sheet = "c_cognitive")

cog_chil_proc <- cog_ch %>% 
  gather("question", "answer", ecn01:ecn18) %>% 
  left_join(answer_c) %>% 
  mutate(point = case_when(year == 2002 & answer == correct ~ 1,
                           year == 2005 & answer == correct ~ 1,
                           year == 2009 & answer == correct09 ~ 1,
                           TRUE ~ 0)) %>% 
  group_by(year, folio, ls, pid_link_uni, date_int) %>% 
  summarise(point = sum(point)) %>% 
  mutate(test_score = point/18)

# EA: Adult

answer_a <- read_xlsx("Inputs/Codes.xlsx", sheet = "a_cognitive")

cog_adul_proc <- cog_ad %>%
  gather("question", "answer", eca01:eca12) %>% 
  left_join(answer_a) %>% 
  mutate(point = case_when(year == 2002 & answer == correct ~ 1,
                           year == 2005 & answer == correct ~ 1,
                           year == 2009 & answer == correct09 ~ 1,
                           TRUE ~ 0)) %>% 
  group_by(year, folio, ls, pid_link_uni, date_int) %>% 
  summarise(point = sum(point)) %>% 
  mutate(test_score = point/12) %>% 
  filter(!point>12)
  
##### Book:IIIA Decisions #####

# Here I need to create a new variable containing who takes each decision from 1-12, 
# I will classify them in "own", "spouse", "both", "other"

decisions <- dec_base %>% 
  mutate(decision_maker = case_when(dh02a_1 == "A" & dh02b_1 == "" ~ "Own",
                                    dh02a_1 == "A" & dh02b_1 == "B" ~ "Both",
                                    dh02a_1 == "" & dh02b_1 == "B" ~ "Spouse",
                                    TRUE ~ "Other"))%>%
  select(folio, ls, secuencia, year, decision_maker) %>% 
  filter(!is.na(secuencia)) %>% 
  mutate(decision_key = case_when(secuencia == 1 | secuencia == 2 | secuencia == 6 | secuencia == 7 | secuencia == 8 ~ 1,
                                  TRUE ~ 0),
         decision_points = case_when(decision_maker == "Own" ~ 1, 
                                     decision_maker == "Both" ~ 1, 
                                     decision_maker == "Spouse" ~ 0, 
                                     TRUE ~ 0),
         decision_alone = case_when(decision_maker == "Own" ~ 1,
                                    TRUE ~ 0)) %>% 
  group_by(folio, ls, year) %>% 
  select(-decision_key) %>% 
  mutate(decision_points = sum(decision_points), decision_alone = sum(decision_alone, na.rm = TRUE)) %>% 
  spread(secuencia, decision_maker) %>% 
  left_join(basic_ind) %>% 
  #left_join(weight_b3a) %>% 
  #left_join(weight_b3al) %>% 
  left_join(cog_adul_proc)

dec_points <- decisions %>% 
  select(year, folio, ls, pid_link, decision_points, decision_alone)

#####  Summary statistics: Household #####

out <- boxplot(basic_ind$ls13_2, plot=FALSE)$out

summary_h <- basic_ind %>% 
  filter(!ls13_2 %in% out) %>% 
  left_join(INF) %>% 
  group_by(ls04, ls05_1, year) %>%
  mutate(ls12 = case_when(ls12 == 1 ~ 1,
                          ls12 == 3 ~ 0,
                          TRUE ~ ls12)) %>% 
#  filter(!is.na(fac_libc)) %>%
  filter(!is.na(ls04)) %>% 
  summarise(worked = mean(ls12, na.rm = TRUE), income = mean(ls13_2, na.rm = TRUE), 
            income_com = mean(ls13_2, na.rm = TRUE)*fact_infl, education = mean(ls14, na.rm = TRUE), 
            age = mean(ls02_2, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(ls05_1 == 1 | ls05_1 == 2)

summary_household <- basic_ind %>% 
  left_join(INF) %>% 
  filter(!ls13_2 %in% out) %>% 
  mutate(children = case_when(ls05_1 == 3 | ls05_1 == 4 ~ 1, 
                              TRUE ~ 0)) %>% 
  group_by(folio_uni, year) %>% 
  summarise(income = sum(ls13_2, na.rm = TRUE), income_com = sum(ls13_2, na.rm = TRUE)*fact_infl, education = mean(ls14, na.rm = TRUE),
            children = sum(children), number_persons = n(),  income_pc = sum(ls13_2, na.rm = TRUE)/n(),
            income_pc_com = (sum(ls13_2, na.rm = TRUE)/n())*fact_infl) %>% 
  distinct() %>% 
  left_join(basic_folio)
  
summary_house <- describeBy(summary_household, group = summary_household$year)

summary_dec <- decisions %>% 
  filter(!is.na(ls04)) %>% 
  filter(!is.na(ls05_1)) %>%
#  filter(!is.na(fac_3a)) %>%
  group_by(ls04, ls05_1, year) %>% 
  summarise(decision_points = mean(decision_points, na.rm = TRUE), decision_alone = mean(decision_alone, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(ls05_1 == 1 | ls05_1 == 2)

#####  Summary statistics: Adults summary #####

aux_adul <- basic_ind %>% 
  left_join(basic_folio) %>% 
  filter(ls02_2>15) %>%
  left_join(ed_base) %>% 
  filter(!ls13_2 %in% out) %>% 
  left_join(bio_base) %>% 
  left_join(cog_adul_proc) %>% 
  left_join(INF) %>% 
  mutate(sex = case_when(ls04 == 1 ~ 1, 
                         ls04 == 3 ~ 0,
                         TRUE ~ NA_real_), 
         spanish = case_when(ed01 == 1 ~ 1, 
                             ed01 == 3 ~ 0,
                             TRUE ~ NA_real_), 
         indigenous = case_when(ed03 == 1 ~ 1, 
                                ed03 == 3 ~ 0, 
                                TRUE ~ NA_real_), 
         school_att = case_when(ed05 ==1 ~ 1, 
                                ed05 == 3 ~ 0, 
                                TRUE ~ NA_real_), 
         worked_12 = case_when(ls12 == 1 ~ 1, 
                               ls12 == 3 ~ 0, 
                               TRUE ~ NA_real_),
         income_c = ls13_2*fact_infl, 
         married = case_when(ls10 == 1 ~ 1, 
                             ls10 == 5 ~ 1, 
                             TRUE ~ 0), 
         HH = case_when(ls05_1 == 1 ~ 1, 
                        TRUE ~ 0)) %>% 
  left_join(dec_points) %>% 
  select(year, ent, state_name, mpio, date_int, dummy_div, folio, pid_link, folio_uni, pid_link_uni,  sex, ls05_1, ls02_2, ls03_21, ls03_22, spanish, indigenous, school_att, 
         worked_12, ed06, ed07_1, married, ls04, ls13_2, income_c, ls14, sa07_21, sa08_21, point, test_score, decision_points, decision_alone, HH)

aux_muj <- aux_adul %>% filter(sex == 0) #%>% 
  #filter(ls05_1 == 1 | ls05_1 == 2)

aux_hom <- aux_adul %>% filter(sex == 1)# %>% 
 # filter(ls05_1 == 1 | ls05_1 == 2)

summary_muj <- describeBy(aux_muj, group = aux_muj$year)

summary_hom <- describeBy(aux_hom, group = aux_hom$year)

#####  Summary statistics: Children summary #####

aux_child <- edna_base %>% 
  left_join(bio_base) %>% 
  left_join(cog_chil_proc) %>% 
  left_join(basic_ind) %>% 
  mutate(sex = case_when(ls04 == 1 ~ 1, 
                         ls04 == 3 ~ 0,
                         TRUE ~ NA_real_), 
         spanish = case_when(edn02 == 1 ~ 1, 
                             edn02 == 3 ~ 0,
                             TRUE ~ NA_real_), 
         indigenous = case_when(edn01 == 1 ~ 1, 
                                edn01 == 3 ~ 0, 
                                TRUE ~ NA_real_), 
         school_att = case_when(edn03 ==1 ~ 1, 
                                edn03 == 3 ~ 0, 
                                TRUE ~ NA_real_), 
         worked_12 = case_when(ls12 == 1 ~ 1, 
                               ls12 == 3 ~ 0, 
                               TRUE ~ NA_real_)) %>% 
  select(year, ent, state_name, mpio, date_int, dummy_div, folio, pid_link, folio_uni, pid_link_uni,  sex, ls02_2, ls03_21, ls03_22, spanish, indigenous, school_att, 
         worked_12, edn09, sa07_21, sa08_21, point, test_score, pid_link_mom, pid_link_dad) %>% 
  mutate(date_born = make_date(ls03_22, ls03_21),
         age_months = interval(date_born, date_int)%/% months(1),
         age_aux = interval(date_born, date_int)%/% years(1), 
         months_aux = case_when(!is.na(age_months)  ~ age_months, 
                            is.na(age_months) & !is.na(ls02_2) ~ ls02_2*12,
                            TRUE ~ NA_real_),
         Months = case_when(months_aux < 0 ~ ls02_2*12, 
                            TRUE ~ months_aux),
         age_years = case_when(!is.na(age_aux) ~ age_aux, 
                             is.na(age_aux) & !is.na(ls02_2) ~ ls02_2,
                             TRUE ~ NA_real_), 
         Years = case_when(age_years < 0 ~ ls02_2, 
                            TRUE ~ age_years)) %>% 
  filter(!Years>15) %>%
  select(-c("age_months", "age_aux", "months_aux", "age_years"))
  
summary_child <- describeBy(aux_child, group = aux_child$year)

###### Save summary #####

write.xlsx2(summary_muj[["2002"]], "Outputs/summary.xlsx", sheetName = "2002_muj", overwrite = TRUE)
write.xlsx2(summary_muj[["2005"]], "Outputs/summary.xlsx", sheetName = "2005_muj", append = TRUE, overwrite = TRUE)
write.xlsx2(summary_muj[["2009"]], "Outputs/summary.xlsx", sheetName = "2009_muj", append = TRUE, overwrite = TRUE)


write.xlsx2(summary_hom[["2002"]], "Outputs/summary.xlsx", sheetName = "2002_hom", append = TRUE, overwrite = TRUE)
write.xlsx2(summary_hom[["2005"]], "Outputs/summary.xlsx", sheetName = "2005_hom", append = TRUE, overwrite = TRUE)
write.xlsx2(summary_hom[["2009"]], "Outputs/summary.xlsx", sheetName = "2009_hom", append = TRUE, overwrite = TRUE)


write.xlsx2(summary_child[["2002"]], "Outputs/summary.xlsx", sheetName = "2002_child", append = TRUE, overwrite = TRUE)
write.xlsx2(summary_child[["2005"]], "Outputs/summary.xlsx", sheetName = "2005_child", append = TRUE, overwrite = TRUE)
write.xlsx2(summary_child[["2009"]], "Outputs/summary.xlsx", sheetName = "2009_child", append = TRUE, overwrite = TRUE)

write.xlsx2(summary_house[["2002"]], "Outputs/summary.xlsx", sheetName = "2002_house", append = TRUE, overwrite = TRUE)
write.xlsx2(summary_house[["2005"]], "Outputs/summary.xlsx", sheetName = "2005_house", append = TRUE, overwrite = TRUE)
write.xlsx2(summary_house[["2009"]], "Outputs/summary.xlsx", sheetName = "2009_house", append = TRUE, overwrite = TRUE)

##### Maintain and save databases needed for analysis and graphs and erase the rest 


rm(list=setdiff(ls(), c("summary_dec", "summary_household", "summary_child", "summary_h","decisions", "aux_adul", "aux_child", 
                        "decisions", "basic_folio")))

save.image(file = 'Outputs/Data_tidy_dissertation.RData')
