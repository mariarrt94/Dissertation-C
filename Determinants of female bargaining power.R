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

fin_mod1 <- plm(PC1money_mom ~  age_mom + edu_mom + worked_12_mom + HH_mom + test_score_mom + dummy_div + log_income_mom,
                   data = p_data_women, model = "pooling")

fin_mod2 <- plm(PC1money_mom ~ age_mom + edu_mom + worked_12_mom + HH_mom + test_score_mom 
                + dummy_div + log_income_mom,
                   data = p_data_women, model = "within", effect = "time")

# tests: pooled or time fe?

# 1. Test FE time versus pooled OLS: p-value is  0.02716 ha is significant effects, we reject OLS in favour of Time FE

pFtest(fin_mod2, fin_mod1)

ch_mod1 <- plm(PC1ch_mom ~ age_mom + edu_mom + worked_12_mom + HH_mom + test_score_mom 
                + dummy_div + log_income_mom,
                   data = p_data_women, model = "pooling")

ch_mod2 <- plm(PC1ch_mom ~ age_mom + edu_mom + worked_12_mom + HH_mom + test_score_mom 
                + dummy_div + log_income_mom,
                   data = p_data_women, model = "within", effect = "time")

# 1. Test FE time versus pooled OLS: p-value is  0.00103  ha is significant effects, we reject OLS in favour of Time FE

pFtest(ch_mod2, ch_mod1)

# generate clustered standard errors in a list v

fin_rob_se <- list(sqrt(diag(vcovHC(ch_mod1, type = "HC1"))),
                  sqrt(diag(vcovHC(ch_mod2, type = "HC1"))),
                  sqrt(diag(vcovHC(fin_mod1, type = "HC1"))),
                  sqrt(diag(vcovHC(fin_mod2, type = "HC1"))))

stargazer(ch_mod1, ch_mod2, fin_mod1, fin_mod2, 
          digits = 3, header = FALSE, type = "html", dep.var.labels=c("Children-related bargaining index", "Financial-Related bargaining index"),
          covariate.labels=c("Age", "Education level", "Household head", "Test score", 
                             "Divorced", "Income"),
          no.space = TRUE, align = TRUE,
          se = fin_rob_se, model.numbers = FALSE,  column.labels = c("(1)", "(2)", "(3)", "(4)"), 
          add.lines = list(c('Time effects', 'No', 'Yes', 'No', 'Yes'),
                           c('F test time effects', '', '3.611', '', '6.896')),
          column.sep.width = "5pt", # Well... you can tweak this,
        #  omit.stat = c("f"),
          out = "Outputs/mom_models.htm")