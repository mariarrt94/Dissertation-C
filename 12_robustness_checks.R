##### Dissertation Maria Reyes Retana - 
# This code runs the robustness tests for the dissertation

##### Libraries #####

library(tidyverse)
library(readxl)
library(xlsx)
library(lubridate)
library(plm)
library(lmtest)
library(gmm)
library(momentfit)
library(skimr)
library(stargazer)
library(gtsummary)
library(flextable)
library(scales)
library(panelr)
library(oglmx)

##### Read data #####

source('5_complete_data.R')

##### Generates data panel indi #####

base_child <- base_child %>% ungroup()

pdata_child_t <- pdata.frame(base_child, index = c("pid_link_uni", "year")) %>% 
  filter(!(decision_mom == 0 & decision_dad == 0)) %>% 
  filter(ls02_2 > 4 ) %>% 
  filter(!is.na(test_score))

panel_data_test <- panel_data(base_child, id = pid_link_uni, wave = year)

pdata_child_h <- pdata.frame(base_child, index = c("pid_link_uni", "year")) %>% 
  filter(!(decision_mom == 0 & decision_dad == 0)) %>% 
  filter(!is.na(hfa_z))

pdata_child_hem <- pdata.frame(base_child, index = c("pid_link_uni", "year")) %>% 
  filter(!(decision_mom == 0 & decision_dad == 0)) %>% 
  filter(!is.na(hemo_z))

##### Hemoglobin separated ######

hemo_mod1_s <- plm(hemo_z ~ PC1ch_mom + PC1money_mom + ls02_2 + school_att + edn09 + worked_12_mom + HH_mom + hemo_mom 
                   + hemo_dad  + edu_mom + edu_dad + log_income_crea_pc + children,
                   data = pdata_child_hem, model = "pooling")

hemo_mod2_s <- plm(hemo_z ~ PC1ch_mom + PC1money_mom + ls02_2 + school_att + edn09 + worked_12_mom + HH_mom + hemo_mom 
                   + hemo_dad  + edu_mom + edu_dad + log_income_crea_pc + children,
                   data = pdata_child_hem, model = "within", effect = "time")
pFtest(hemo_mod2_s, hemo_mod1_s)# F test for time effects: OLS vs pooled F = 2.2301, df1 = 2, df2 = 4617, p-value = 0.1076 i cannot reject

hemo_mod3_s <- plm(hemo_z ~ PC1ch_mom + PC1money_mom + ls02_2 + school_att + edn09 + worked_12_mom + HH_mom + hemo_mom 
                   + hemo_dad  + edu_mom + edu_dad + log_income_crea_pc + children,
                   data = pdata_child_hem, model = "within", effect = "individual")
pFtest(hemo_mod3_s, hemo_mod1_s)# F = 1.3813, df1 = 3736, df2 = 883, p-value = 0.00000000194 reject

hemo_mod4_s <- plm(hemo_z ~ PC1ch_mom + PC1money_mom + ls02_2 + school_att + edn09 + worked_12_mom + HH_mom + hemo_mom 
                   + hemo_dad  + edu_mom + edu_dad + log_income_crea_pc + children,
                   data = pdata_child_hem, model = "within", effect = "twoways")
pFtest(hemo_mod4_s, hemo_mod1_s)# F = 1.3813, df1 = 3736, df2 = 883, p-value = 0.00000000194

hemo_mod5_s <- plm(hemo_z ~ PC1ch_mom + PC1money_mom + ls02_2 + school_att + edn09 + worked_12_mom + HH_mom + hemo_mom 
                   + hemo_dad  + edu_mom + edu_dad + log_income_crea_pc + children,
                   data = pdata_child_hem, model = "random")
phtest(hemo_mod4_s, hemo_mod5_s) #Hausman test  under H0 random is preferred 0.00376 reject preferred is fixed

# generate clustered standard errors in a list v

rob_se_hemo_s <- list(sqrt(diag(vcovHC(hemo_mod1_s, type = "HC1"))),
                      sqrt(diag(vcovHC(hemo_mod2_s, type = "HC1"))),
                      sqrt(diag(vcovHC(hemo_mod3_s, type = "HC1"))),
                      sqrt(diag(vcovHC(hemo_mod4_s, type = "HC1"))))

stargazer(hemo_mod1_s, hemo_mod2_s, hemo_mod3_s, hemo_mod4_s, 
          digits = 3, header = FALSE, type = "html", dep.var.labels=c("Hemoglobin Z-Score"),
          covariate.labels=c("Bargaining Index C", "Bargaining Index F", "Age" ,"School attendance", 
                             "Education level", "Mom works?", "Mom is household head", 
                             "Hemoglobin mom", "Hemoglobin dad", "Mom education", "Dad education",
                             "Log household income pc", "# Children"),
          no.space = TRUE, align = TRUE,
          se = rob_se_hemo_s, model.numbers = FALSE,  column.labels = c("(1)", "(2)", "(3)", "(4)"), 
          add.lines = list(c('Individual Effects', 'No', 'No', 'Yes', 'Yes'),
                           c('Time Effects', 'No', 'Yes', 'No', 'Yes'),
                           c('F test fixed effects', '', '2.230', '1.381', '1.386')),
          column.sep.width = "5pt", # Well... you can tweak this,
          omit.stat = c("adj.rsq"),
          out = "Outputs/robustness_checks/hemo_models_sep.htm")

##### Girls/Boys hemo #####

# as all models containing fixed effects are preferred above those which don't I present Girls and Boys in one table.
hemo_mod1_g <- plm(hemo_z ~ PC1ch_mom + PC1money_mom  + school_att + edn09 + worked_12_mom + HH_mom + hemo_mom 
                   + hemo_dad  + edu_mom + edu_dad +  log_income_crea_pc + children,
                   data = pdata_child_hem, model = "pooling", subset = sex == 0)

hemo_mod2_g <- plm(hemo_z ~ PC1ch_mom + PC1money_mom  + school_att + edn09 + worked_12_mom + HH_mom + hemo_mom 
                   + hemo_dad  + edu_mom + edu_dad +  log_income_crea_pc + children,
                   data = pdata_child_hem, model = "within", effect = "time", subset = sex == 0)
pFtest(hemo_mod2_g, hemo_mod1_g)# F test for time effects: OLS vs pooled F = 0.64343, df1 = 2, df2 = 2419, p-value = 0.5256 I cannot reject

hemo_mod3_g <- plm(hemo_z ~ PC1ch_mom + PC1money_mom  + school_att + edn09 + worked_12_mom + HH_mom + hemo_mom 
                   + hemo_dad  + edu_mom + edu_dad +  log_income_crea_pc + children,
                   data = pdata_child_hem, model = "within", effect = "individual", subset = sex == 0)
pFtest(hemo_mod3_g, hemo_mod1_g)# F = 1.4949, df1 = 1954, df2 = 467, p-value = 0.00000006627 reject FE preferred

hemo_mod4_g <- plm(hemo_z ~ PC1ch_mom + PC1money_mom  + school_att + edn09 + worked_12_mom + HH_mom + hemo_mom 
                   + hemo_dad  + edu_mom + edu_dad +  log_income_crea_pc + children,
                   data = pdata_child_hem, model = "within", effect = "twoways", subset = sex == 0)
pFtest(hemo_mod4_g, hemo_mod1_g)# F = 1.5104, df1 = 1956, df2 = 465, p-value = 0.00000003411, FE preferred

hemo_mod1_b <- plm(hemo_z ~ PC1ch_mom + PC1money_mom  + school_att + edn09 + worked_12_mom + HH_mom + hemo_mom 
                   + hemo_dad  + edu_mom + edu_dad +  log_income_crea_pc + children,
                   data = pdata_child_hem, model = "pooling", subset = sex == 1)

hemo_mod2_b <- plm(hemo_z ~ PC1ch_mom + PC1money_mom  + school_att + edn09 + worked_12_mom + HH_mom + hemo_mom 
                   + hemo_dad  + edu_mom + edu_dad +  log_income_crea_pc + children,
                   data = pdata_child_hem, model = "within", effect = "time", subset = sex == 1)
pFtest(hemo_mod2_b, hemo_mod1_b)# F = 1.7384, df1 = 2, df2 = 2477, p-value = 0.176, I cannot reject OLS

hemo_mod3_b <- plm(hemo_z ~ PC1ch_mom + PC1money_mom  + school_att + edn09 + worked_12_mom + HH_mom + hemo_mom 
                   + hemo_dad  + edu_mom + edu_dad +  log_income_crea_pc + children,
                   data = pdata_child_hem, model = "within", effect = "individual", subset = sex == 1)
pFtest(hemo_mod3_b, hemo_mod1_b)# F = 1.3153, df1 = 2005, df2 = 474, p-value = 0.0001206 reject FE preferred

hemo_mod4_b <- plm(hemo_z ~ PC1ch_mom + PC1money_mom  + school_att + edn09 + worked_12_mom + HH_mom + hemo_mom 
                   + hemo_dad  + edu_mom + edu_dad +  log_income_crea_pc + children,
                   data = pdata_child_hem, model = "within", effect = "twoways", subset = sex == 1)
pFtest(hemo_mod4_b, hemo_mod1_b)#F = 1.3321, df1 = 2007, df2 = 472, p-value = 0.00006398, FE preferred

rob_se_bg_h <- list(sqrt(diag(vcovHC(hemo_mod3_g, type = "HC1"))),
                    sqrt(diag(vcovHC(hemo_mod4_g, type = "HC1"))),
                    sqrt(diag(vcovHC(hemo_mod3_b, type = "HC1"))),
                    sqrt(diag(vcovHC(hemo_mod4_b, type = "HC1"))))

stargazer(hemo_mod3_g, hemo_mod4_g, hemo_mod3_b, hemo_mod4_b, 
          digits = 3, header = FALSE, type = "html", dep.var.labels=c("Haemoglobin Z-score"),
          covariate.labels=c("Bargaining Index C", "Bargaining Index F", "School attendance", 
                             "Education level", "Mom works?", "Mom is household head", 
                             "Haemoglobin mom", "Haemoglobin dad", "Mom education", "Dad education",
                             "Household income pc", "# Children"),
          no.space = TRUE, align = TRUE,
          se = rob_se_bg_h, model.numbers = FALSE,  column.labels = c("Girls (3)", "Girls (4)", "Boys (3)", "Boys (4)"), 
          add.lines = list(c('Time Fixed Effects', 'No', 'Yes', 'No', 'Yes')),
          column.sep.width = "5pt", # Well... you can tweak this,
          omit.stat = c("adj.rsq"),
          out = "Outputs/robustness_checks/hemo_models_GB.htm")

test_mod1_s <- plm(test_score_z ~ PC1ch_mom + PC1money_mom + ls02_2 + c(ls02_2*ls02_2)+  school_att + edn09 + worked_12_mom + HH_mom + test_score_mom 
                   + test_score_dad  + edu_mom + edu_dad +  log_income_crea_pc + children,
                   data = pdata_child_t, model = "pooling")

test_mod2_s <- plm(test_score_z ~ PC1ch_mom + PC1money_mom + ls02_2 + c(ls02_2*ls02_2) + school_att + edn09 + worked_12_mom + HH_mom + test_score_mom 
                   + test_score_dad  + edu_mom + edu_dad +  log_income_crea_pc + children,
                   data = pdata_child_t, model = "within", effect = "time")
pFtest(test_mod2_s, test_mod1_s)# F test for time effects: OLS vs pooled 0.8935 I cannot reject OLS F = 0.11265, df1 = 2, df2 = 3988

test_mod3_s <- plm(test_score_z ~ PC1ch_mom + PC1money_mom + ls02_2 + c(ls02_2*ls02_2)+ school_att + edn09 + worked_12_mom + HH_mom + test_score_mom 
                   + test_score_dad  + edu_mom + edu_dad +  log_income_crea_pc + children,
                   data = pdata_child_t, model = "within", effect = "individual")
pFtest(test_mod3_s, test_mod1_s)# F test for time effects: OLS vs pooled reject OLS in favour of individual effects F = 1.2099, df1 = 3015, df2 = 976, p-value = 0.0001682
test_mod4_s <- plm(test_score_z ~ PC1ch_mom + PC1money_mom + ls02_2 + c(ls02_2*ls02_2)+ school_att + edn09 + worked_12_mom + HH_mom + test_score_mom 
                   + test_score_dad  + edu_mom + edu_dad +  log_income_crea_pc + children,
                   data = pdata_child_t, model = "within", effect = "twoways")
pFtest(test_mod4_s, test_mod1_s)# F test for time effects: OLS vs pooled  0.0001567 reject OLS 1.2099, df1 = 3017, df2 = 974, p-value = 0.0001732

test_mod5_s <- plm(test_score_z ~ PC1ch_mom + PC1money_mom + ls02_2 + c(ls02_2*ls02_2)+ school_att + edn09 + worked_12_mom + HH_mom + test_score_mom 
                   + test_score_dad  + edu_mom + edu_dad +  log_income_crea_pc + children,
                   data = pdata_child_t, model = "random")

phtest(test_mod4_s, test_mod5_s) #Hausman test  under H0 random is preferred .008 reject preferred is fixed

test_mod6_s <- wbm(test_score_z ~ PC1ch_mom + PC1money_mom + ls02_2+ c(ls02_2*ls02_2) + school_att + edn09 + worked_12_mom + HH_mom + test_score_mom 
                   + test_score_dad  +  log_income_crea_pc + children | sex + edu_mom + edu_dad, data = panel_data_test)

summary(test_mod6_s)

# dummy fixed effects including only household FE
test_mod7_s <- lm(test_score_z ~ PC1ch_mom + PC1money_mom + ls02_2 + school_att + edn09 + worked_12_mom + HH_mom + test_score_mom 
                  + test_score_dad  + edu_mom + edu_dad +  log_income_crea_pc + children + factor(year) + factor(folio_uni),
                  data = pdata_child_t)

# without any controls 

test_mod6_s <- plm(test_score_z ~ PC1ch_mom + PC1money_mom, data = pdata_child_t, model = "within", effect = "individual")

test_mod7_s <- plm(test_score_z ~ PC1ch_mom + PC1money_mom, data = pdata_child_t, model = "within", effect = "twoways")

test_mod8_s <- plm(test_score_z ~ PC1ch_mom + PC1money_mom, data = pdata_child_t, model = "pooling")

coeftest(test_mod6_s)
coeftest(test_mod7_s)
coeftest(test_mod8_s)

# generate clustered standard errors in a list v

rob_se_ts <- list(sqrt(diag(vcovHC(test_mod1_s, type = "HC1"))),
                  sqrt(diag(vcovHC(test_mod2_s, type = "HC1"))),
                  sqrt(diag(vcovHC(test_mod3_s, type = "HC1"))),
                  sqrt(diag(vcovHC(test_mod4_s, type = "HC1"))))

stargazer(test_mod1_s, test_mod2_s, test_mod3_s, test_mod4_s, 
          digits = 3, header = FALSE, type = "html", dep.var.labels=c("Raven Test Z-Score"),
          covariate.labels=c("Bargaining Index C", "Bargaining Index F", "Age", "Age squared", "School attendance", 
                             "Education level", "Mom works?", "Mom is household head", 
                             "Test score mom", "Test score dad", "Mom education", "Dad education",
                             "Household income pc", "# Children"),
          no.space = TRUE, align = TRUE,
          se = rob_se_ts, model.numbers = FALSE,  column.labels = c("(1)", "(2)", "(3)", "(4)"), 
          add.lines = list(c('Individual Effects', 'No', 'No', 'Yes', 'Yes'),
                           c('Time Effects', 'No', 'Yes', 'No', 'Yes'),
                           c('F test fixed effects', '', '0.113', '1.210', '1.209')),
          column.sep.width = "5pt", # Well... you can tweak this,
          omit.stat = c("adj.rsq"),
          out = "Outputs/robustness_checks/test_models_sep.htm")

##### Reported on dissertation: Height separated adding health ######

hfa_mod1_s <- plm(height_z ~ PC1ch_mom + PC1money_mom + ls02_2 + c(ls02_2*ls02_2) + school_att + edn09 +factor(health_stat) + factor(worked_12_mom) + factor(HH_mom) + height_mom 
                  + height_dad  + edu_mom + edu_dad + log_income_crea_pc + children,
                  data = pdata_child_h, model = "pooling")

hfa_mod3_s <- plm(height_z ~ PC1ch_mom + PC1money_mom + ls02_2 + c(ls02_2*ls02_2) + school_att + edn09 +factor(health_stat) + factor(worked_12_mom) + factor(HH_mom) + height_mom 
                  + height_dad  + edu_mom + edu_dad + log_income_crea_pc + children,
                  data = pdata_child_h, model = "within", effect = "individual")
pFtest(hfa_mod3_s, hfa_mod1_s)# F = 1.4456, df1 = 4766, df2 = 1601, p-value < 0.00000000000000022

hfa_mod4_s <- plm(height_z ~ PC1ch_mom + PC1money_mom + ls02_2 + c(ls02_2*ls02_2) + school_att + edn09 + factor(health_stat) + factor(worked_12_mom) + factor(HH_mom) + height_mom 
                  + height_dad  + edu_mom + edu_dad + log_income_crea_pc + children,
                  data = pdata_child_h, model = "within", effect = "twoways")
pFtest(hfa_mod4_s, hfa_mod1_s)# F = 1.4469, df1 = 4768, df2 = 1599, p-value < 0.00000000000000022

##### Reported on dissertation: Test separated #####

test_mod1_s <- plm(test_score_z ~ PC1ch_mom + PC1money_mom + ls02_2 + c(ls02_2*ls02_2)+ school_att + edn09 + factor(health_stat) +factor(worked_12_mom) + factor(HH_mom) + test_score_mom 
                   + test_score_dad  + edu_mom + edu_dad +  log_income_crea_pc + children,
                   data = pdata_child_t, model = "pooling")

test_mod3_s <- plm(test_score_z ~ PC1ch_mom + PC1money_mom + ls02_2 + c(ls02_2*ls02_2)+ school_att + edn09 + factor(health_stat) +factor(worked_12_mom) + factor(HH_mom) + test_score_mom 
                   + test_score_dad  + edu_mom + edu_dad +  log_income_crea_pc + children,
                   data = pdata_child_t, model = "within", effect = "individual")
pFtest(test_mod3_s, test_mod1_s)# F test for time effects: OLS vs pooled reject OLS in favour of individual effects F = 1.2099, df1 = 3015, df2 = 976, p-value = 0.0001682
test_mod4_s <- plm(test_score_z ~ PC1ch_mom + PC1money_mom + ls02_2 + c(ls02_2*ls02_2)+ school_att + edn09 + factor(health_stat)+ factor(worked_12_mom) + factor(HH_mom) + test_score_mom 
                   + test_score_dad  + edu_mom + edu_dad +  log_income_crea_pc + children,
                   data = pdata_child_t, model = "within", effect = "twoways")
pFtest(test_mod4_s, test_mod1_s)# F test for time effects: OLS vs pooled  0.0001567 reject OLS 1.2099, df1 = 3017, df2 = 974, p-value = 0.0001732

test_mod5_s <- plm(test_score_z ~ PC1ch_mom + PC1money_mom + ls02_2 + c(ls02_2*ls02_2)+ school_att + edn09 + +factor(health_stat) +factor(worked_12_mom) + factor(HH_mom)+ test_score_mom 
                   + test_score_dad  + edu_mom + edu_dad +  log_income_crea_pc + children,
                   data = pdata_child_t, model = "random")

phtest(test_mod4_s, test_mod5_s) #Hausman test  under H0 random is preferred .008 reject preferred is fixed

# generate clustered standard errors in a list v

rob_se_ts <- list(sqrt(diag(vcovHC(test_mod3_s, type = "HC1"))),
                  sqrt(diag(vcovHC(test_mod4_s, type = "HC1"))),
                  sqrt(diag(vcovHC(hfa_mod3_s, type = "HC1"))),
                  sqrt(diag(vcovHC(hfa_mod4_s, type = "HC1"))))

stargazer(test_mod3_s, test_mod4_s, hfa_mod3_s, hfa_mod4_s, 
          digits = 3, header = FALSE, type = "html", dep.var.labels=c("Raven Test Z-Score", "Height Z-score"),
          covariate.labels=c("Bargaining Index C", "Bargaining Index F", "Age", "Age squared", "School attendance", 
                             "Education level", "Health Good", "Health Regular", "Health Bad", "Mom works?", "Mom is household head", 
                             "Test score mom", "Test score dad", "Height mom", "Height dad","Mom education", "Dad education",
                             "Household income pc", "# Children"),
          no.space = TRUE, align = TRUE,
          se = rob_se_ts, model.numbers = FALSE,  column.labels = c("FE (3)", "FE (4)", "FE (3)", " FE (4)"), 
          add.lines = list(c('Time Effects', 'No', 'Yes', 'No', 'Yes')),
          column.sep.width = "5pt", # Well... you can tweak this,
          omit.stat = c("adj.rsq"),
          out = "Outputs/robustness_checks/test_height_rob.htm")

