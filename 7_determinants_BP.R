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

source('5_complete_data.R')

##### Generates data panel #####

# created data base for women
p_data_women <- pdata.frame(mom_base, index = c("pid_link_mom", "year")) %>% 
  mutate(income_c_mom = ifelse(worked_12_mom ==0 & is.na(income_c_mom), 0, income_c_mom),
    log_income_mom = case_when(income_c_mom > 0 ~ log(income_c_mom), 
                                    TRUE ~ income_c_mom)) %>% 
  # only married women
  filter(married_mom ==1) %>% 
  # only non empty values for the index
  filter(!is.na(PC1money_mom)) 


##### Financial model ----

# financial index POLS

fin_mod1 <- plm(PC1money_mom ~  age_mom + c(age_mom*age_mom) + edu_mom  + HH_mom + test_score_mom + log_income_mom,
                   data = p_data_women, model = "pooling")

# Financial index FE
fin_mod2 <- plm(PC1money_mom ~ age_mom + c(age_mom*age_mom) + edu_mom  + HH_mom + test_score_mom + log_income_mom,
                   data = p_data_women, model = "within", effect = "time")


fin_mod22_randomeffects <- plm(PC1money_mom~ age_mom + c(age_mom*age_mom) + edu_mom + HH_mom + test_score_mom 
                             + log_income_mom,
                             data = p_data_women, model = "random", effect = "time", random.method = "amemiya")


# 1. Hausman Test

hausman_test <- phtest(fin_mod2, fin_mod22_randomeffects)
print(hausman_test)

## #Here, the p-value is 0.9755, which is greater than the typical significance level of 0.05.
#Therefore, we fail to reject the null hypothesis. This means there's insufficient evidence to suggest that 
#the random effects model is inconsistent. 
#So, based on this test, it would be more appropriate to use the random effects model over the fixed effects model.

# 2. Breusch-Pagan test
bp_test <- plmtest(fin_mod22_randomeffects, type=c("bp"))
print(bp_test)
# The null hypothesis of this test is that variances across entities (like individuals, firms, etc.) are zero, 
#which means that there are no random effects. In this context, rejecting the null hypothesis would provide evidence 
#in favor of a random effects model.
#In this case, the p-value is extremely small (0.00004711), which is well below the common significance level of 0.05. 
#This means that we would reject the null hypothesis and conclude that there are significant random effects in the model.


# 3. Breusch-Godfrey/Wooldridge

bg_test <- pbgtest(fin_mod22_randomeffects)

#  chi-squared statistic of 51.099 with a p-value extremely close to zero (0.0000000000008783). 
#This p-value is well below conventional significance levels (such as 0.05 or 0.01), so we would reject the null hypothesis.
# for this reason we use robust standard errors

# conclusion: we will use the random effects model with robust


#  Children model -----

# first old estimations

ch_mod1 <- plm(PC1ch_new_mom ~ age_mom + c(age_mom*age_mom) + edu_mom  + HH_mom + test_score_mom 
                + log_income_mom,
                   data = p_data_women, model = "pooling")

ch_mod2<- plm(PC1ch_new_mom ~ age_mom + c(age_mom*age_mom) + edu_mom + HH_mom + test_score_mom 
                + log_income_mom,
                   data = p_data_women, model = "within", effect = "time")

ch_mod2_randomeffects <- plm(PC1ch_new_mom ~ age_mom + c(age_mom*age_mom) + edu_mom + HH_mom + test_score_mom 
                             + log_income_mom,
                             data = p_data_women, model = "random", effect = "time", random.method = "amemiya")

# estimations with edits by Andy


# 1. Test FE time versus pooled OLS: p-value is  0.72  cannot reject OLS in favour of Time FE

pFtest(ch_mod2, ch_mod1) 

# 2. Hausman

hausman_test_ch <- phtest(ch_mod2, ch_mod2_randomeffects)

print(hausman_test_ch)

#the Hausman test yields a p-value of 0.9978, which is above the commonly used significance level thresholds.
#Hence, we fail to reject the null hypothesis. 
#This suggests that the random effects model is consistent and efficient, and it would be appropriate.

# 3. Breusch-Pagan test
bp_test_ch <- plmtest(ch_mod2_randomeffects, type=c("bp"))
print(bp_test_ch)

# There is presence of random effects. 

bg_test_ch <- pbgtest(ch_mod2_randomeffects)
print(bg_test_ch)

# Presence of serial correlation, therefore we need to use
# robust standard errors. 


# conclusion the most appropiate for this is random with robust standard erros

##### Save results -----
# generate clustered standard errors in a list v

fin_rob_se <- list(
                  sqrt(diag(vcovHC(ch_mod2_randomeffects, type = "HC1"))),
                  sqrt(diag(vcovHC(fin_mod22_randomeffects, type = "HC1")))
              )

stargazer(ch_mod2_randomeffects, fin_mod22_randomeffects, 
          digits = 3, header = FALSE, type = "html", dep.var.labels=c("Children bargaining index", "Financial bargaining index"),
          covariate.labels=c("Age", "Age squared", "Education level", "Household head", "Test score", 
                             "Log income"),
          no.space = TRUE, align = TRUE,
          se = fin_rob_se, model.numbers = TRUE,  column.labels = c("Random Effects", "Random Effects"), 
          add.lines = list(c('Time effects', 'Yes','Yes'),
                           c('Hausman test p value','0.998', '0.976')),
          column.sep.width = "50pt", # Well... you can tweak this,
          notes = c("Source: MXFLS", "Robust standard errors in parenthesis."),
        #  omit.stat = c("f"),
          out = "Outputs/models/mom_models_final.htm")


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
          out = "Outputs/robustness_checks/mom_models_rob.htm")