##### Dissertation Maria Reyes Retana - 
# This code generates comparison tables for attrited children

##### Read data #####

source('5_complete_data.R')

##### Libraries #####

library(gtsummary)
library(flextable)
library(ggpubr)

##### Summary for children considered #####

year_2002 <- base_child %>% 
  ungroup() %>% 
  select(year, pid_link_uni) %>% 
  filter(year == "2002") 

year_2005 <- base_child %>% 
  ungroup() %>% 
  select(year, pid_link_uni) %>% 
  filter(year == "2005")

data_att <- base_child %>% 
  select(year, pid_link_uni,  school_att, worked_12, worked_12_mom, worked_12_dad, HH_mom, sex, ls02_2,
         edn09, test_score_z, height_z, income_crea_pc, children, number_persons, decision_mom, decision_dad) %>% 
  group_by(pid_link_uni) %>% 
  mutate(count = n()) %>% 
  mutate(attrition = case_when(count == 3 ~ "Three waves", 
                               count == 1 & year == "2002" ~ "Attrited",
                               count == 1 & year == "2005" ~ "Attrited",
                               count == 1 & year == "2009" ~ "Third wave", 
                               count == 2 & pid_link_uni %in% year_2002$pid_link_uni ~ "Attrited", 
                               count == 2 & pid_link_uni %in% year_2005$pid_link_uni ~ "Second and third wave",
                               TRUE ~ "A")) %>% 
  mutate(decisions_female = case_when(decision_mom >= decision_dad ~ "Mom > dad",
                                      is.na(decision_dad) & !is.na(decision_mom) ~ "Mom > dad", 
                                      decision_dad > decision_mom ~ "Dad > mom", 
                                      !is.na(decision_dad) & is.na(decision_mom) ~ "Dad > mom",
                                      TRUE ~ "x")) %>% 
  rename('Boys (%)' = sex,  'Attends school? (%)' = school_att, 'Worked last 12 months? (%)'= worked_12, 'Mom works? (%)' = worked_12_mom, 
         'Dad works? (%)' = worked_12_dad, 'Mom household head (%)' = HH_mom, 'Education level' = edn09, 'Raven test-score (0%-100%)' = test_score_z, 
         Age = ls02_2, 'Height (cm)' = height_z, 'Household per capita income' = income_crea_pc, 
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

treat_att <- data_att %>%
  ungroup() %>% 
  filter(!(decision_mom == 0 & decision_dad == 0)) %>%
  select(-c(decision_dad)) %>% 
  filter(`More decisions by mom` == "Mom > dad") %>% 
  select(-c("year", "pid_link_uni", "count", "More decisions by mom")) %>% 
  filter(attrition == "Three waves" | attrition == "Attrited") %>% 
  tbl_summary(by = attrition, statistic = list(all_continuous() ~ "{mean}", all_categorical()~"{p}%"), 
              type = list(c('Number of persons', 'Education level', 'Raven test-score (0%-100%)', 'Height (cm)') ~ "continuous"),  missing = "no") %>%
  add_difference(pvalue_fun = fmt_pvalue_with_stars) %>% 
  modify_header(label = "**Variable**") %>% # update the column header
  bold_labels() %>% 
  modify_footnote(p.value ~ "*p<0.1; **p<0.05; ***p<0.01") %>% 
  as_flex_table() 

control_att <- data_att %>%
  ungroup() %>% 
  filter(!(decision_mom == 0 & decision_dad == 0)) %>%
  select(-c(decision_dad)) %>% 
  filter(`More decisions by mom` != "Mom > dad") %>% 
  select(-c("year", "pid_link_uni", "count", "More decisions by mom")) %>% 
  filter(attrition == "Three waves" | attrition == "Attrited") %>% 
  tbl_summary(by = attrition, statistic = list(all_continuous() ~ "{mean}", all_categorical()~"{p}%"),  
              type = list(c('Number of persons', 'Education level', 'Raven test-score (0%-100%)', 'Height (cm)') ~ "continuous"),  missing = "no") %>%
  add_difference(pvalue_fun = fmt_pvalue_with_stars) %>% 
  modify_header(label = "**Variable**") %>% # update the column header
  bold_labels() %>% 
  modify_footnote(p.value ~ "*p<0.1; **p<0.05; ***p<0.01") %>% 
  as_flex_table() 

 t.first <- data_att[match(unique(data_att$pid_link_uni), data_att$pid_link_uni),] # identify the first appearance not going to use it

att_not <- data_att %>% 
  ungroup() %>% 
  filter(!(decision_mom == 0 & decision_dad == 0)) %>%
  select(-c(decision_dad)) %>%
  filter(attrition == "Three waves" | attrition == "Attrited") %>% 
 # filter(`More decisions by mom` == "Mom > dad" | `More decisions by mom` == "Dad > mom") %>% 
 # filter(attrition == "Attrited") %>% 
  select(-c("year", "pid_link_uni", "count", "More decisions by mom")) %>% 
    tbl_summary(by = attrition, statistic = list(all_continuous() ~ "{mean}", all_categorical()~"{p}%"), 
                type = list(c('Number of persons', 'Education level', 'Raven test-score (0%-100%)', 'Height (cm)') ~ "continuous"),  missing = "no") %>%
  add_difference(pvalue_fun = fmt_pvalue_with_stars) %>% 
  modify_header(label = "**Variable**") %>% # update the column header
  bold_labels() %>% 
  modify_footnote(p.value ~ "*p<0.1; **p<0.05; ***p<0.01") %>% 
  as_flex_table() 

save_as_docx(`Attrition in control group` = control_att, `Attrition in treatment group` = treat_att,
             `Comparison between attrited` = att_att, path = "Outputs/tables/table_attrition.docx")

# manually calculate differences in means: it is consistent with table above

data_stats_a <- data_att %>% 
  filter(`More decisions by mom` == "Mom > dad") %>% 
  filter(attrition == "Attrited") 

data_stats_p <- data_att %>% 
  filter(`More decisions by mom` == "Mom > dad") %>% 
  filter(attrition == "Three waves")

t.test(data_stats_a$decision_dad, data_stats_p$decision_dad)
