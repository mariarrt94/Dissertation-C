##### Dissertation Maria Reyes Retana - 
# This code generates summaries for children and women with different 
# level of decision-making for the data section

##### Read data #####

source('Complete information for analysis.R')

##### Libraries #####

library(gtsummary)
library(flextable)
library(tidyverse)

##### Summary for children considered #####

data_child_sum <- base_child %>% 
  select(year,  school_att, worked_12, worked_12_mom, worked_12_dad, HH_mom, sex, ls02_2,
         edn09, test_score, height, income_crea_pc, children, number_persons, decision_mom, decision_dad) %>% 
  filter(!(decision_mom == 0 & decision_dad == 0)) %>% 
  mutate(decisions_female = case_when(decision_mom >= decision_dad ~ "Mom > dad",
                                      is.na(decision_dad) & !is.na(decision_mom) ~ "Mom > dad", 
                                      decision_dad > decision_mom ~ "Dad > mom", 
                                      !is.na(decision_dad) & is.na(decision_mom) ~ "Dad > mom",
                                      TRUE ~ "x")) %>% 
  select(-c("decision_mom", "decision_dad")) %>% 
  rename('Boys (%)' = sex,  'Attends school? (%)' = school_att, 'Worked last 12 months? (%)'= worked_12, 'Mom works? (%)' = worked_12_mom, 
         'Dad works? (%)' = worked_12_dad, 'Mom household head (%)' = HH_mom, 'Education level' = edn09, 'Raven test-score (0%-100%)' = test_score, 
          Age = ls02_2, 'Height (cm)' = height, 'Household per capita income' = income_crea_pc, 
         'Number of children'= children, 'Number of persons' = number_persons, 
         'More decisions by mom' = decisions_female)

fmt_pvalue_with_stars <- function(x) {
  dplyr::case_when(
    x < 0.01 ~ paste0(style_pvalue(x), "***"),
    x < 0.05 ~ paste0(style_pvalue(x), "**"),
    x < 0.1 ~ paste0(style_pvalue(x), "*"),
    TRUE ~ style_pvalue(x)
  )
}

# manually calculate differences in means

data_stats_m <- data_child_sum %>% 
  filter(`More decisions by mom` == "Dad > mom")

data_stats_d <- data_child_sum %>% 
  filter(`More decisions by mom` == "Mom > dad")

t.test(data_stats_d$`Number of children`, data_stats_m$`Number of children`)


table_child <- data_child_sum %>%
  ungroup() %>% 
  select(-c(year)) %>% 
  tbl_summary(by = 'More decisions by mom', statistic = list(all_continuous() ~ "{mean} ({sd})"),type = list(c('Number of persons', 'Education level') ~ "continuous"),  missing = "no") %>% 
  add_n() %>% # add column with total number of non-missing observations
  add_difference(pvalue_fun = fmt_pvalue_with_stars) %>% # test for a difference between groups
  modify_header(label = "**Variable**") %>% # update the column header
  bold_labels() %>% 
  modify_footnote(p.value ~ "*p<0.1; **p<0.05; ***p<0.01") %>% 
  modify_footnote(label ~ "The first 6 variables ending with (%) are dichotomous") %>% 
  as_flex_table() 

save_as_docx(table_child, path = "Outputs/table_child.docx")

##### Summary for mothers with different decision power #####

mom_sum <- mom_base %>%
  left_join(base_child) %>% 
  filter(!(decision_mom == 0 & decision_dad == 0)) %>% 
  mutate(decisions_female = case_when(decision_mom >= decision_dad ~ "Mom > dad",
                                      is.na(decision_dad) & !is.na(decision_mom) ~ "Mom > dad", 
                                      decision_dad > decision_mom ~ "Dad > mom", 
                                      !is.na(decision_dad) & is.na(decision_mom) ~ "Dad > mom",
                                      TRUE ~ "x")) %>% 
  select(worked_12_mom, worked_12_dad, married_mom, HH_mom, edu_mom, edu_dad, income_c_mom, income_c_dad, test_score_mom, 
         test_score_dad, decisions_female) %>% 
  rename('She works? (%)' = worked_12_mom, "Spouse works? (%)" = worked_12_dad, 'Married (%)' = married_mom, 
         'She is household head (%)' = HH_mom, 'Her education level' = edu_mom, 'Spouse education level' = edu_dad, 
         'Her income'= income_c_mom, 'Spouse income' = income_c_dad, 'Her test score' = test_score_mom, 
          'Spouse test score' = test_score_dad, 'More decisions by mom' = decisions_female)
  
table_mom <- mom_sum %>%
  ungroup() %>% 
  tbl_summary(by = 'More decisions by mom', 
              statistic = list(all_continuous() ~ "{mean} ({sd})"),
              type = list(c('Her education level', 'Spouse education level') ~ "continuous"),  
              missing = "no") %>% 
  add_n() %>% # add column with total number of non-missing observations
  add_difference(pvalue_fun = fmt_pvalue_with_stars) %>% 
  modify_header(label = "**Variable**") %>% # update the column header
  bold_labels() %>% 
  modify_footnote(p.value ~ "*p<0.1; **p<0.05; ***p<0.01") %>% 
  modify_footnote(label ~ "Variables ending with (%) are dichotomous") %>% 
  as_flex_table() 

save_as_docx(table_mom, path = "Outputs/table_momp.docx")

#### mom sum por decision ----

# Define a vector of decision numbers

duplicados <- aux_adul %>%
  filter(ls05_1 == 1 | ls05_1 == 2) %>% 
  arrange(folio_uni) %>% 
  distinct() %>% 
  dplyr::group_by(year, state_name, folio_uni, sex) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(n > 1L) 

sum_by_decision <- aux_adul %>% 
  filter(ls05_1 == 1 | ls05_1 == 2) %>% 
  distinct() %>% 
  arrange(folio_uni) %>% 
  select(year, state_name, folio_uni, pid_link_uni, sex, ls05_1, ls02_2, worked_12, ed06, ed07_1, married, income_c, ls14, 
         PC1tot, PC1work, PC1ch,
         test_score, decision_alone, HH, `1`:`12`) %>% 
  filter(!(folio_uni %in% duplicados$folio_uni)) %>% 
  group_by(folio_uni) %>% 
  select(-pid_link_uni) %>% 
  mutate(sex = ifelse(sex == 0, "female", "male")) %>% 
  pivot_wider(names_from = sex, values_from = c(ls05_1, ls02_2, worked_12, ed06, ed07_1, married, income_c, ls14, 
                                                   test_score, decision_alone, HH, `1`:`12`)) %>% 
  mutate(across(c(`1_female`,`2_female`,`3_female`,`4_female`,`5_female`, `6_female`, 
                  `7_female`,`8_female`,`9_female`,`10_female`,`11_female`, `12_female`), 
                ~case_when(. == "Own" | . == "Both" ~ "Own or Both",
                . == "Spouse" ~ "Partner",
                                                  TRUE ~ NA_character_), 
                .names = "decision_{str_remove(.col, '_female')}")) %>% 
  rename('She works? (%)' = worked_12_female, "Spouse works? (%)" = worked_12_male, 'Married (%)' = married_female, 
         'She is household head (%)' = HH_female, 'Her education level' = ed06_female, 'Spouse education level' = ed06_male, 
         'Her income'= income_c_female, 'Spouse income' = income_c_male, 'Her test score' = test_score_female, 
         'Spouse test score' = test_score_male) %>% 
  ungroup()

# women_summary 
women_sumary <- aux_adul %>% 
  filter(ls05_1 == 1 | ls05_1 == 2) %>% 
  filter(sex==0) %>% 
  distinct() %>% 
  left_join(women_index_res, by = c("year", "folio","folio_uni", "pid_link_uni")) %>% 
  select(folio_uni, pid_link_uni, year, folio_uni, pid_link_uni, PC1tot) %>% 
  arrange(pid_link_uni) %>% 
  pivot_wider(names_from = year, values_from = c(PC1tot)) %>% 
  
  
  
  complete.cases()

# own +both and partner

decision_cols_m <- c("year", 
                     "She works? (%)", "Spouse works? (%)", 
                     "She is household head (%)", 
                     "Her education level","Spouse education level", 
                     "Spouse income", "Her income", 
                     "Her test score", "Spouse test score", "decision_1", "decision_2", "decision_3", 
                     "decision_4", "decision_5", "decision_6", "decision_7", 
                     "decision_8", "decision_9", "decision_10", "decision_11","decision_12")


sum_by_decision_long <- sum_by_decision %>% 
  select(all_of(decision_cols_m)) %>% 
  pivot_longer(names_to = "decision", values_to = "type", cols = c(decision_1:decision_12)) %>% 
  filter(type ==  "Own or Both" | type == "Partner")
  

table_mom_dec1 <- sum_by_decision_long %>%
  select(-year) %>% 
  ungroup() %>% 
  tbl_strata(
    strata = decision, 
    .tbl_fun = 
      ~.x %>% 
      tbl_summary(by = type, 
                  statistic = list(all_continuous() ~ "{mean} ({sd})"),
                  type = list(c('Her education level', 'Spouse education level', 
                                'Spouse test score', 'Her test score', 
                                'Spouse income', 'Her income') ~ "continuous", 
                              c('She works? (%)', 'Spouse works? (%)',
                                'She is household head (%)') ~ "categorical"),
                  missing = "no") %>% 
      add_n(),
    add_difference(pvalue_fun = fmt_pvalue_with_stars),
    .header = "**{strata}**, N = {n}" ) %>% 
  as_flex_table() 

save_as_docx(table_mom_dec1, path = "Outputs/table_mom_dec.docx")

# ahora own y both separados 

decision_cols_sep <- c("year", 
                     "She works? (%)", "Spouse works? (%)", 
                     "She is household head (%)", 
                     "Her education level","Spouse education level", 
                     "Spouse income", "Her income",
                     "Her test score", "Spouse test score", "1_female","2_female","3_female",
                     "4_female","5_female", "6_female", "7_female", "8_female" ,"9_female","10_female",
                     "11_female", "12_female")


sum_by_decision_long_sep <- sum_by_decision %>% 
  select(all_of(decision_cols_sep)) %>% 
  pivot_longer(names_to = "decision", values_to = "type", cols = c(`1_female`:`12_female`)) %>% 
  filter(type ==  "Own" | type == "Both")


table_mom_dec_sep <- sum_by_decision_long_sep %>%
  select(-year) %>% 
  ungroup() %>% 
  tbl_strata(
    strata = decision, 
    .tbl_fun = 
      ~.x %>% 
      tbl_summary(by = type, 
                  statistic = list(all_continuous() ~ "{mean} ({sd})"),
                  type = list(c('Her education level', 'Spouse education level', 
                                'Spouse test score', 'Her test score', 
                                'Spouse income', 'Her income') ~ "continuous", 
                              c('She works? (%)', 'Spouse works? (%)',
                                'She is household head (%)') ~ "categorical"),
                  missing = "no") %>% 
      add_n(),
    add_difference(pvalue_fun = fmt_pvalue_with_stars),
    .header = "**{strata}**, N = {n}" ) %>% 
  as_flex_table() 

save_as_docx(table_mom_dec_sep, path = "Outputs/table_mom_dec_sep.docx")
