##### Dissertation Maria Reyes Retana - 
# This code generates summaries for children and women with different 
# level of decision-making for the data section

##### Read data #####

source('Complete information for analysis.R')

##### Libraries #####

library(gtsummary)
library(flextable)

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
  tbl_summary(by = 'More decisions by mom', statistic = list(all_continuous() ~ "{mean} ({sd})"),type = list(c('Her education level', 'Spouse education level') ~ "continuous"),  missing = "no") %>% 
  add_n() %>% # add column with total number of non-missing observations
  add_difference(pvalue_fun = fmt_pvalue_with_stars) %>% 
  modify_header(label = "**Variable**") %>% # update the column header
  bold_labels() %>% 
  modify_footnote(p.value ~ "*p<0.1; **p<0.05; ***p<0.01") %>% 
  modify_footnote(label ~ "Variables ending with (%) are dichotomous") %>% 
  as_flex_table() 

save_as_docx(table_mom, path = "Outputs/table_mom.docx")