##### Dissertation Maria Reyes Retana - 
# CODE to create summary statistics for women and children

##### Libraries #####

library(tidyverse)
library(readxl)
library(xlsx)
library(plm)
library(pglm)
library(margins)
library(lmtest)
library(stargazer)
library(clubSandwich)
library(skimr)
library(kableExtra)

##### Read data ##### 

source('5_complete_data.R')

shift_sector <- read.csv("Outputs/shocks_base_sec.csv") %>% 
  mutate(ent = as.numeric(ent), mpio = as.numeric(mpio)) %>% 
  select(-c(X))

##### Data for shift analysis ######

base_child <- base_child %>% ungroup() %>% 
  left_join(shift_sector) %>% 
  mutate(muni_com = paste(ent, mpio, sep = "")) %>% 
  filter(married_mom ==1) 

pdata_child_t <- pdata.frame(base_child, index = c("pid_link_uni", "year")) %>% 
  filter(!(decision_mom == 0 & decision_dad == 0)) %>% 
  filter(!is.na(test_score))

pdata_child_h <- pdata.frame(base_child, index = c("pid_link_uni", "year")) %>% 
  filter(!(decision_mom == 0 & decision_dad == 0)) %>% 
  filter(!is.na(hfa_z))

mom_base <- mom_base %>% 
  left_join(shift_sector) %>% 
  mutate(income_c_mom = ifelse(worked_12_mom ==0 & is.na(income_c_mom), 0, income_c_mom),
         log_income_mom = case_when(income_c_mom > 0 ~ log(income_c_mom), 
                                    TRUE ~ income_c_mom)) %>% 
  filter(married_mom ==1) %>% 
  filter(!is.na(PC1money_mom))

p_data_women <- pdata.frame(mom_base, index = c("pid_link_mom", "year")) %>% 
  # we are assigning an income of zero when women does not work and income is missing, as we loose a lot of women
  # if we do not consider incomes equal to zero 
  mutate(income_c_mom = ifelse(worked_12_mom ==0 & is.na(income_c_mom), 0, income_c_mom),
         log_income_mom = case_when(income_c_mom > 0 ~ log(income_c_mom), 
                                    TRUE ~ income_c_mom)) %>% 
  filter(married_mom ==1) %>% 
  filter(!is.na(PC1money_mom))


##### Select relevant variables for summary statistics 

base_child_summary <- base_child %>% 
  rename(Age = ls02_2, Sex = sex,
         `School attendance (%)` = school_att, 
         `Works` = worked_12, 
         `Education level` = edn09, 
          `Height` = height, `Test score` = test_score, 
         `Mom's age` = age_mom, `Dad's age` = age_dad, 
         `Mom works? (%)` = worked_12_mom, `Dad works? (%)` = worked_12_dad, 
         `Mom's education` = edu_mom, `Dad's education` = edu_dad, 
         `Mom's test score` = test_score_mom, `Dad's test score` = test_score_dad,
         `# Mom's decisions` = decision_mom, `# Dad's decisions` = decision_dad, 
         `# Mom's financial decisions` = decision_finan_mom, `# Dad's financial decisions` = decision_finan_dad, 
          `# Children` = children, 
         `HH Mom` = HH_mom) %>% 
  # select needed variables for summary statistic tables
  select(year, Age, Sex, `School attendance (%)`, Works, `Education level`, 
         Height, `Test score`, `Mom's age`, `Dad's age`, `Mom works? (%)`, 
         `Dad works? (%)`, `Mom's education`, `Dad's education`, 
         `Mom's test score`, `Dad's test score`, `# Mom's decisions`, 
         `# Dad's decisions`, `# Mom's financial decisions`, 
         `# Dad's financial decisions`, `# Children`, `HH Mom`)

summary_stats_child <- base_child_summary %>% 
  skim()

# add summary by sex of children

sample_size <- base_child_summary %>% 
  group_by(Sex) %>% 
  summarise(across(everything(), ~sum(!is.na(.)))) %>% 
  pivot_longer(cols = c(year:`# Children`)) %>% 
  rename(skim_variable = name, Sample = value)

summary_stats_child_group <- base_child_summary %>% 
  group_by(Sex) %>% 
  skim() %>% 
  left_join(sample_size)

# Convert to a data frame


summary_stats_child %>%
  rename(Variable = skim_variable, Mean = numeric.mean, 
         SD = numeric.sd, Histogram = numeric.hist) %>% 
  filter(Variable != "year") %>% 
  select(Variable, n_missing, Mean, SD, Histogram) %>% 
  kable("html", digits = 2) %>%
  kable_classic(full_width = F, html_font = "Cambria") %>% 
 # add_header_above(c(" " = 1, "Group 1" = 2, "Group 2" = 1, "Group 3" = 1)) %>% 
  save_kable("Outputs/tables/summary_stats_child.html")
  
summary_stats_child_group %>%
  rename(Variable = skim_variable, Mean = numeric.mean, 
         SD = numeric.sd, Histogram = numeric.hist) %>% 
  filter(Variable != "year") %>% 
  mutate(Sex = ifelse(Sex == 0, "Girl", "Boy")) %>% 
  select(Variable, n_missing, Mean, SD, Histogram, Sample, Sex) %>% 
  pivot_wider(names_from = Sex, values_from = c(n_missing, Mean, SD, Histogram, Sample)) %>% 
  select(Variable, n_missing_Girl, Sample_Girl, Mean_Girl, SD_Girl, n_missing_Boy, Sample_Boy, Mean_Boy, SD_Boy) %>% 
  kable("html", digits = 2, col.names = c("Variable","Missing","Sample", "Mean", "SD", "Missing", "Sample","Mean", "SD")) %>%
  kable_classic(full_width = F, html_font = "LM Roman") %>% 
 add_header_above(c(" " = 1 ,"Girls" = 4, "Boys" = 4)) %>% 
  footnote(general_title = "Note:", 
           number = c("Variables ending with (%) are dichotomous.", 
           "Education level is classified into ten categories. 1. No education, 2. Preschool, 3. Elementary,
           4. Secondary, 5. Open secondary, 6. High school, 7. Open high school, 8. Normal basic, 9. College, 
           10. Postgraduate.")) %>%
  save_kable("Outputs/tables/summary_stats_child_sex.html")

# CREATE SUMMARY TABLE for women from children sample

women_summary <- base_child_summary %>% 
  select(-c(Age, Sex, `School attendance (%)`, Works, `Education level`,Height, `Test score`)) %>% 
  mutate(financial_type = ifelse(`# Mom's financial decisions` >2.93955839, "More", "Less")) %>% 
  filter(!is.na(financial_type))

sample_size_mom <- women_summary %>% 
  group_by(financial_type) %>% 
  summarise(across(everything(), ~sum(!is.na(.)))) %>% 
  pivot_longer(cols = c(year:`HH Mom`)) %>% 
  rename(Variable = name, Sample = value) %>% 
  filter(!is.na(financial_type))

statistics_type_mom <- women_summary %>% 
  group_by(financial_type) %>% 
  #create summary 
  skim() %>% 
  # rename variables for tables
  rename(Variable = skim_variable, Mean = numeric.mean, 
         SD = numeric.sd, Histogram = numeric.hist) %>% 
  filter(Variable != "year") %>% 
  # join sample size
  left_join(sample_size_mom) %>% 
  # filter if they do not have decision making
  filter(!is.na(financial_type)) %>% 
  select(Variable, n_missing, Mean, SD, Histogram, Sample, financial_type) %>% 
  # pivot to create table format
  pivot_wider(names_from = financial_type, values_from = c(n_missing, Mean, SD, Histogram, Sample)) %>% 
  select(Variable, Sample_More, Mean_More, SD_More, Sample_Less, Mean_Less, SD_Less) %>% 
  mutate(t_stat = (Mean_More - Mean_Less) / sqrt((SD_More^2/Sample_More) + (SD_Less^2/Sample_Less)),
         df = Sample_More + Sample_Less - 2) %>%
  mutate(p_value = 2 * pt(-abs(t_stat), df)) %>% 
  mutate(significance = case_when(
    p_value < 0.01 ~ "***",
    p_value < 0.05 ~ "**",
    p_value < 0.1  ~ "*",
    TRUE ~ "")) %>% 
  select(-c(t_stat, p_value, df, SD_More, SD_Less)) %>% 
  # save as kable for dissertation
  kable("html", digits = 2, col.names = c("Variable","Sample", "Mean","Sample","Mean", "P-value")) %>%
  kable_classic(full_width = F, html_font = "LM Roman") %>% 
  add_header_above(c(" " = 1 ,"More than average" = 2, "Less than average" = 2, " " =1)) %>% 
  footnote(general_title = "Note:", 
           number = c("Variables ending with (%) are dichotomous.", 
                      "Education level is classified into ten categories. 1. No education, 2. Preschool, 3. Elementary,
           4. Secondary, 5. Open secondary, 6. High school, 7. Open high school, 8. Normal basic, 9. College, 
           10. Postgraduate.", "*p<0.1; **p<0.05; ***p<0.01")) %>%
  save_kable("Outputs/tables/summary_stats_women.html")

# Now doing the same but with women sample only


women_summary_s <- mom_base %>% 
  rename(Age = age_mom, `She works? (%)` = worked_12_mom, Income = income_c_mom, 
         `Education level` = edu_mom, Height = height_mom, `Test score` = test_score_mom, 
         `# of decisions` = decision_mom, `# of financial decisions` = decision_finan_mom) %>% 
  select(year, Age, `She works? (%)`, Income, `Education level`, Height, `Test score`, 
         `# of decisions`, `# of financial decisions`) %>% 
  mutate(financial_type = ifelse(`# of financial decisions` >2.93955839, "More", "Less")) %>% 
  filter(!is.na(financial_type))

sample_size_mom_s <- women_summary_s %>% 
  group_by(financial_type) %>% 
  summarise(across(everything(), ~sum(!is.na(.)))) %>% 
  pivot_longer(cols = c(year:`# of financial decisions`)) %>% 
  rename(Variable = name, Sample = value) %>% 
  filter(!is.na(financial_type))

statistics_type_mom_s <- women_summary_s %>% 
  group_by(financial_type) %>% 
  #create summary 
  skim() %>% 
  # rename variables for tables
  rename(Variable = skim_variable, Mean = numeric.mean, 
         SD = numeric.sd, Histogram = numeric.hist) %>% 
  filter(Variable != "year") %>% 
  # join sample size
  left_join(sample_size_mom_s) %>% 
  # filter if they do not have decision making
  filter(!is.na(financial_type)) %>% 
  select(Variable, n_missing, Mean, SD, Histogram, Sample, financial_type) %>% 
  # pivot to create table format
  pivot_wider(names_from = financial_type, values_from = c(n_missing, Mean, SD, Histogram, Sample)) %>% 
  select(Variable, Sample_More, Mean_More, SD_More, Sample_Less, Mean_Less, SD_Less) %>% 
  mutate(t_stat = (Mean_More - Mean_Less) / sqrt((SD_More^2/Sample_More) + (SD_Less^2/Sample_Less)),
         df = Sample_More + Sample_Less - 2) %>%
  mutate(p_value = 2 * pt(-abs(t_stat), df)) %>% 
  mutate(significance = case_when(
    p_value < 0.01 ~ "***",
    p_value < 0.05 ~ "**",
    p_value < 0.1  ~ "*",
    TRUE ~ "")) %>% 
  select(-c(t_stat, p_value, df, SD_More, SD_Less)) %>% 
  # save as kable for dissertation
  kable("html", digits = 2, col.names = c("Variable","Sample", "Mean","Sample","Mean", "P-value")) %>%
  kable_classic(full_width = F, html_font = "LM Roman") %>% 
  add_header_above(c(" " = 1 ,"More than average" = 2, "Less than average" = 2, " " =1)) %>% 
  footnote(general = c("Source: MxFLS-1, MxFLS-2, MxFLS-3."),
           number = c("Variables ending with (%) are dichotomous.", 
                      "Education level is classified into ten categories. 1. No education, 2. Preschool, 3. Elementary,
           4. Secondary, 5. Open secondary, 6. High school, 7. Open high school, 8. Normal basic, 9. College, 
           10. Postgraduate.", "*p<0.1; **p<0.05; ***p<0.01")) %>%
  save_kable("summary_stats_women_s.html")

# Read the HTML file and replace "Note: " with an empty string
html_content <- readLines("Outputs/tables/summary_stats_women_s.html")
html_content <- gsub("Note: ", "", html_content, fixed = TRUE)

# Write the modified HTML content back to the file
writeLines(html_content, "Outputs/tables/summary_stats_women_s.html")
