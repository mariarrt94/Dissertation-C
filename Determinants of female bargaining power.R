##### Dissertation Maria Reyes Retana - 
# This code runs the regressions to study the determinants of female bargaining power.
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

##### Read data #####

source('Complete information for analysis.R')

##### Generates data panel indi #####

p_data_women <- pdata.frame(mom_base, index = c("pid_link_mom", "year")) %>% 
  mutate(log_income_mom = case_when(income_c_mom > 0 ~ log(income_c_mom), 
                                    TRUE ~ income_c_mom))

##### Runs regressions ######

fin_mod1 <- plm(PC1money_mom ~  age_mom + c(age_mom*age_mom) + edu_mom  + HH_mom + test_score_mom + married_mom + log_income_mom,
                   data = p_data_women, model = "pooling")

fin_mod2 <- plm(PC1money_mom ~ age_mom + c(age_mom*age_mom) + edu_mom  + HH_mom + test_score_mom 
                + married_mom + log_income_mom,
                   data = p_data_women, model = "within", effect = "time")

summary(fin_mod2)

coeftest(fin_mod1)

coeftest(fin_mod2)

# tests: pooled or time fe?

# 1. Test FE time versus pooled OLS: p-value is  0.01812 ha is significant effects, we reject OLS in favour of Time FE

pFtest(fin_mod2, fin_mod1)

ch_mod1 <- plm(PC1ch_mom ~ age_mom + c(age_mom*age_mom) + edu_mom  + HH_mom + test_score_mom 
                + married_mom + log_income_mom,
                   data = p_data_women, model = "pooling")

ch_mod2 <- plm(PC1ch_mom ~ age_mom + c(age_mom*age_mom) + edu_mom + HH_mom + test_score_mom 
                + married_mom + log_income_mom,
                   data = p_data_women, model = "within", effect = "time")

# 1. Test FE time versus pooled OLS: p-value is  0.00103  ha is significant effects, we reject OLS in favour of Time FE

pFtest(ch_mod2, ch_mod1)

# generate clustered standard errors in a list v

fin_rob_se <- list(sqrt(diag(vcovHC(ch_mod1, type = "HC1"))),
                  sqrt(diag(vcovHC(ch_mod2, type = "HC1"))),
                  sqrt(diag(vcovHC(fin_mod1, type = "HC1"))),
                  sqrt(diag(vcovHC(fin_mod2, type = "HC1"))))

stargazer(ch_mod1, ch_mod2, fin_mod1, fin_mod2, 
          digits = 3, header = FALSE, type = "html", dep.var.labels=c("Children bargaining index", "Financial bargaining index"),
          covariate.labels=c("Age", "Age squared", "Education level", "Household head", "Test score", 
                             "Married", "Log income"),
          no.space = TRUE, align = TRUE,
          se = fin_rob_se, model.numbers = FALSE,  column.labels = c(" POLS (1)", "Time FE (2)", " POLS (3)", "Time FE (4)"), 
          add.lines = list(c('Time effects', 'No', 'Yes', 'No', 'Yes'),
                           c('F test time effects', '', '4.096', '', '4.025')),
          column.sep.width = "5pt", # Well... you can tweak this,
        #  omit.stat = c("f"),
          out = "Outputs/mom_models.htm")
##### Runs regressions: robust check PC1 ######

fin_mod1_rob <- plm(PC2tot_mom ~  age_mom + c(age_mom*age_mom) + edu_mom + worked_12_mom + HH_mom + test_score_mom + dummy_div + log_income_mom,
                data = p_data_women, model = "pooling")

fin_mod2_rob <- plm(PC2tot_mom ~ age_mom + c(age_mom*age_mom) + edu_mom + worked_12_mom + HH_mom + test_score_mom 
                + dummy_div + log_income_mom,
                data = p_data_women, model = "within", effect = "time")

# tests: pooled or time fe?

# 1. Test FE time versus pooled OLS: p-value is  0.2316 i cannot reject OLS

pFtest(fin_mod2_rob, fin_mod1_rob)

ch_mod1_rob <- plm(PC1tot_mom ~ age_mom + c(age_mom*age_mom)+ edu_mom + worked_12_mom + HH_mom + test_score_mom 
               + dummy_div + log_income_mom,
               data = p_data_women, model = "pooling")

ch_mod2_rob <- plm(PC1tot_mom ~ age_mom + c(age_mom*age_mom) + edu_mom + worked_12_mom + HH_mom + test_score_mom 
               + dummy_div + log_income_mom,
               data = p_data_women, model = "within", effect = "time")

# 1. Test FE time versus pooled OLS: p-value is  0.00003211  ha is significant effects, we reject OLS in favour of Time FE

pFtest(ch_mod2_rob, ch_mod1_rob)

# generate clustered standard errors in a list v

fin_rob_se_rob <- list(sqrt(diag(vcovHC(ch_mod1_rob, type = "HC1"))),
                   sqrt(diag(vcovHC(ch_mod2_rob, type = "HC1"))),
                   sqrt(diag(vcovHC(fin_mod1_rob, type = "HC1"))),
                   sqrt(diag(vcovHC(fin_mod2_rob, type = "HC1"))))

stargazer(ch_mod1_rob, ch_mod2_rob, fin_mod1_rob, fin_mod2_rob, 
          digits = 3, header = FALSE, type = "html", dep.var.labels=c("PC1", "PC2"),
          covariate.labels=c("Age", "Age squared", "Education level", "Household head", "Test score", 
                             "Divorced", "Income"),
          no.space = TRUE, align = TRUE,
          se = fin_rob_se_rob, model.numbers = FALSE,  column.labels = c(" POLS (1)", "POLS (2)", " POLS (3)", " POLS (4)"), 
          add.lines = list(c('Time effects', 'No', 'Yes', 'No', 'Yes')),
          column.sep.width = "5pt", # Well... you can tweak this,
          #  omit.stat = c("f"),
          out = "Outputs/Robust/mom_models_rob.htm")