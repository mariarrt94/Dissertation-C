##### Dissertation Maria Reyes Retana - 
# This code does the diff in diff

##### Read data #####

source('Complete information for analysis.R')

##### Libraries #####

library(gtsummary)
library(flextable)
library(ggpubr)
library(lmtest)
library(plm)

##### Generate databases with treated and time #####

did_mom <- mom_base %>% 
  mutate(time =  year == "2009", 
        treated = ent == 9) %>% 
     #    did = time*treated) %>% 
  filter(!is.na(PC1money_mom)) %>% 
  filter(married_mom == 1) %>% 
  filter(!is.na(ent)) %>% 
  mutate(log_income_mom= case_when(income_c_mom == 0 | income_c_mom < 0 ~ income_c_mom,
                                   income_c_mom != 0 & !is.na(income_c_mom) ~ log(income_c_mom),
                                 is.na(income_c_mom)~ NA_real_, 
                                 TRUE ~ NA_real_))

did_child <- base_child %>% 
  ungroup() %>% 
  mutate(time =  year == "2009", 
         treated = ent == 9) %>% 
  filter(ls02_2 > 4) %>% 
  filter(married_mom == 1)

pdata_child_t <- pdata.frame(did_child, index = c("pid_link_uni", "year")) %>% 
  filter(!(decision_mom == 0 & decision_dad == 0)) %>% 
  filter(ls02_2 > 4)

###### General effect of law on mother bargaining power #####

did_model_mom <- lm(decision_finan ~ treated*time + edu_mom + worked_12_mom +age_mom + c(age_mom*age_mom) 
                    + test_score_mom + log_income_mom + HH_mom + married_mom, data = did_mom)

summary(did_model_mom)

# parallel trend assumption

ggplot(did_mom, aes(year, PC1money_mom, color = treated)) + 
  stat_summary(geom = 'line') +
  geom_vline(xintercept = 2005) + 
  theme_minimal()

# positive effect on bargaining power but not significant

##### General Effect on children outcomes #####

did_model_test <- lm(test_score_z ~ treated*time + ls02_2 + c(ls02_2*ls02_2) + sex +  school_att + edn09 + worked_12_mom + HH_mom + test_score_mom 
                     + test_score_dad  +  log_income_crea_pc + children, data = did_child)

summary(did_model_test)

ggplot(did_child, aes(year, test_score_z, color = treated)) + 
  stat_summary(geom = 'line') +
  geom_vline(xintercept = 2005) + 
  theme_minimal()

did_model_height <- lm(height_z ~ treated*time + ls02_2 + c(ls02_2*ls02_2) + sex + school_att + edn09 + worked_12_mom + HH_mom + height_mom 
                     + height_dad  +  log_income_crea_pc + children, data = did_child)

summary(did_model_height)

ggplot(did_child, aes(year, height_z, color = treated)) + 
  stat_summary(geom = 'line') +
  geom_vline(xintercept = 2005) + 
  theme_minimal()
