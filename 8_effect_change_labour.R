##### Dissertation Maria Reyes Retana - 
# This code runs the regressions to determine the effect of bargaining power in children outcomes using shift-share
# This is the code that runs the main result of the analysis.

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

##### Read data ##### 

source('5_complete_data.R')

shift_sector <- read.csv("Inputs/shocks_base_sec.csv") %>% 
  mutate(ent = as.numeric(ent), mpio = as.numeric(mpio)) %>% 
  select(-c(X))

##### Data for shift analysis ######

base_child <- base_child %>% ungroup() %>% 
  left_join(shift_sector) %>% 
  mutate(muni_com = paste(ent, mpio, sep = ""))

pdata_child_t <- pdata.frame(base_child, index = c("pid_link_uni", "year")) %>% 
  filter(!(decision_mom == 0 & decision_dad == 0)) %>% 
  filter(!is.na(test_score)) %>% 
  # only married / living with partner
  filter(married_mom ==1) 

pdata_child_h <- pdata.frame(base_child, index = c("pid_link_uni", "year")) %>% 
  filter(!(decision_mom == 0 & decision_dad == 0)) %>% 
  filter(!is.na(hfa_z)) %>% 
  # only married / living with partner
  filter(married_mom ==1) 

mom_base <- mom_base %>% 
  left_join(shift_sector)

p_data_women <- pdata.frame(mom_base, index = c("pid_link_mom", "year")) %>% 
  # we are assigning an income of zero when women does not work and income is missing, as we loose a lot of women
  # if we do not consider incomes equal to zero 
  mutate(income_c_mom = ifelse(worked_12_mom ==0 & is.na(income_c_mom), 0, income_c_mom),
         log_income_mom = case_when(income_c_mom > 0 ~ log(income_c_mom), 
                                    TRUE ~ income_c_mom)) %>% 
  # only married / living with partner
  filter(married_mom ==1) %>% 
  filter(!is.na(PC1money_mom))


##### First step as relevant for mom #####

### POLS

# decisions financial
mod_fb <- plm(PC1money_mom ~ female + male + age_mom + c(age_mom*age_mom) + edu_mom + HH_mom + test_score_mom,  data = p_data_women, model =  "pooling")

summary(mod_fb)

# decisions children with edits
mod_ch_new <- plm(PC1ch_new_mom ~ female + male + age_mom + c(age_mom*age_mom) + edu_mom + HH_mom + test_score_mom,  data = p_data_women,  model = "pooling")

summary(mod_ch_new)

# # of decisions 
mod_dec_fin <- plm(decision_finan_mom ~ female + male + age_mom + c(age_mom*age_mom) + edu_mom + HH_mom + test_score_mom, data = p_data_women, model = "pooling")

coeftest(mod_dec_fin)

# income

mod_la <- plm(log_income_mom ~ female +age_mom + c(age_mom*age_mom) + edu_mom + HH_mom + test_score_mom, data = p_data_women, model = "pooling")

summary(mod_la)

### random 

# decisions financial
mod_fb_random <- plm(PC1money_mom ~ female + male + age_mom + c(age_mom*age_mom) + edu_mom + HH_mom + test_score_mom,  data = p_data_women, model = "random", random.method = "amemiya")

summary(mod_fb_random)

# decisions children with edits
mod_ch_new_random <- plm(PC1ch_new_mom ~ female + male + age_mom + c(age_mom*age_mom) + edu_mom + HH_mom + test_score_mom,  data = p_data_women, model = "random", random.method = "amemiya")

summary(mod_ch_new_random)

# # of decisions 
mod_dec_fin_random <- plm(decision_finan_mom ~ female + male + age_mom + c(age_mom*age_mom) + edu_mom + HH_mom + test_score_mom, data = p_data_women, model = "random", random.method = "amemiya")

coeftest(mod_dec_fin_random)

# income
mod_la_random <- plm(log_income_mom ~ female +age_mom + c(age_mom*age_mom) + edu_mom + HH_mom + test_score_mom, data = p_data_women, model = "random", random.method = "amemiya")

summary(mod_la_random)

### Test which model random or POLS

# 1. Perform the Hausman test
hausman_test_bp <- phtest(mod_fb, mod_fb_random)
print(hausman_test_bp)

## the p-value from the Hausman test is 0.09471, which is higher than the commonly 
#used significance level of 0.05. This means that you fail to reject the null hypothesis
#at the 5% significance level, implying that the difference in coefficients between the POLS model 
#and the random effects model is not statistically significant.

#Therefore, the Hausman test suggests that the random effects model is 
#appropriate for the data. This result suggests that,  the unobserved, time-invariant 
#individual-specific effects are not correlated with the regressors, 
#and that the RE estimator is both consistent and efficient.

# 2. Breusch-Pagan test
bp_test_fin<- plmtest(mod_fb_random, type=c("bp"))
print(bp_test_fin)

## USE robust standard erros

# save table for pols

mom_se <- list(sqrt(diag(vcovHC(mod_fb, type = "HC1"))),
               sqrt(diag(vcovHC(mod_ch_new, type = "HC1"))),
                  sqrt(diag(vcovHC(mod_dec_fin, type = "HC1"))),
                  sqrt(diag(vcovHC(mod_la, type = "HC1"))))

stargazer(mod_fb, mod_ch_new, mod_dec_fin, mod_la, 
          digits = 3, header = FALSE, type = "html", dep.var.labels=c("Financial bargaining index","Children Bargaining index", "# Financial decisions", "Log income"),
          covariate.labels=c("Change female labour demand", "Change male labour demand", "Age", "Age squared", "Education level", "Household head",
                             "Test score"),
          no.space = TRUE, align = TRUE,
          se = mom_se, model.numbers = TRUE,  column.labels = c("POLS", "POLS", "POLS", "POLS"), 
          column.sep.width = "5pt", # Well... you can tweak this,
       #   omit.stat = c("adj.rsq"),
          out = "Outputs/models/model_barg.htm")


# save table for RANDOM

mom_se_random <- list(sqrt(diag(vcovHC(mod_fb_random, type = "HC1"))),
               sqrt(diag(vcovHC(mod_ch_new_random, type = "HC1"))),
               sqrt(diag(vcovHC(mod_dec_fin_random, type = "HC1"))),
               sqrt(diag(vcovHC(mod_la_random, type = "HC1"))))

stargazer(mod_fb_random, mod_ch_new_random, mod_dec_fin_random, mod_la_random, 
          digits = 3, header = FALSE, type = "html", dep.var.labels=c("Financial bargaining index","Children Bargaining index", "# Financial decisions", "Log income"),
          covariate.labels=c("Change female labour demand", "Change male labour demand", "Age", "Age squared", "Education level", "Household head",
                             "Test score"),
          no.space = TRUE, align = TRUE,
          se = mom_se_random, model.numbers = TRUE,  column.labels = c("Random Effects", "Random Effects", "Random Effects", "Random Effects"), 
          column.sep.width = "5pt", # Well... you can tweak this,
          #   omit.stat = c("adj.rsq"),
          out = "Outputs/models/model_barg_random.htm")

# bargainign new with modifications

##### Reduced form: shift-share as proxy for bargaining power #####

iv_mod1_t <- plm(test_score_z ~ female + ls02_2 + c(ls02_2*ls02_2) + school_att  + edn09 + worked_12_mom + HH_mom + test_score_mom 
                 + test_score_dad  + edu_mom + edu_dad +  log_income_crea_pc + children + factor(year), data = pdata_child_t, model = "pooling")

coeftest(iv_mod1_t)

iv_mod1_h <- plm(height_z ~ female + ls02_2 + c(ls02_2*ls02_2)+  factor(sex) + edn09 + worked_12_mom + HH_mom + height_mom 
                 + height_dad  + edu_mom + edu_dad +  log_income_crea_pc + children + factor(year), data = pdata_child_h, model = "pooling")

coeftest(iv_mod1_h)

## subset with girls ##

iv_mod1_t_g <- plm(test_score_z ~ female + ls02_2 + c(ls02_2*ls02_2)+  school_att + edn09 + worked_12_mom + HH_mom + test_score_mom 
                   + test_score_dad  + edu_mom + edu_dad +  log_income_crea_pc + children + factor(year), data = pdata_child_t, model = "pooling", subset = sex == 0)

coeftest(iv_mod1_t_g)

iv_mod1_h_g <- plm(height_z ~ female + ls02_2 + c(ls02_2*ls02_2)+  school_att + edn09 + worked_12_mom + HH_mom + height_mom 
                   + height_dad  + edu_mom + edu_dad +  log_income_crea_pc + children + factor(year), data = pdata_child_h, model = "pooling", subset = sex == 0)

coeftest(iv_mod1_h_g)

# with bargainign index OLS

ols_mod1_t_g <- plm(test_score_z ~ PC1money_mom + ls02_2 + c(ls02_2*ls02_2)+  school_att + edn09 + worked_12_mom + HH_mom + test_score_mom
                    + test_score_dad  + edu_mom + edu_dad +  log_income_crea_pc + children + factor(year), data = pdata_child_t, model = "pooling", subset = sex == 0)

coeftest(ols_mod1_t_g)

ols_mod1_h_g <- plm(height_z ~ PC1money_mom + ls02_2 + c(ls02_2*ls02_2)+  school_att + edn09 + worked_12_mom + HH_mom  + height_mom 
                    + height_dad + edu_mom + edu_dad +  log_income_crea_pc + children + factor(year), data = pdata_child_t, model = "pooling", subset = sex == 0)

coeftest(ols_mod1_h_g)

# with bargaining index fixed effects

fe_mod1_t_g <- plm(test_score_z ~ PC1money_mom + ls02_2 + c(ls02_2*ls02_2)+  school_att + edn09 + worked_12_mom + HH_mom + test_score_mom
                    + test_score_dad  + edu_mom + edu_dad +  log_income_crea_pc + children + factor(year), data = pdata_child_t, model = "within", effect = "time",
                   subset = sex == 0)

coeftest(fe_mod1_t_g)

fe_mod1_h_g <- plm(height_z ~ PC1money_mom + ls02_2 + c(ls02_2*ls02_2)+  school_att + edn09 + worked_12_mom + HH_mom + height_mom 
                   + height_dad + edu_mom + edu_dad +  log_income_crea_pc + children + factor(year), data = pdata_child_t,  model = "within", effect = "time", 
                   subset = sex == 0)

coeftest(fe_mod1_h_g)

# with bargaining index random effects 

re_mod1_t_g <- plm(test_score_z ~ PC1money_mom + ls02_2 + c(ls02_2*ls02_2)+  school_att + edn09 + worked_12_mom + HH_mom + test_score_mom
                   + test_score_dad  + edu_mom + edu_dad +  log_income_crea_pc + children + factor(year), data = pdata_child_t, model = "random", effect = "time",
                   subset = sex == 0, random.method = "amemiya")

coeftest(re_mod1_t_g)

re_mod1_h_g <- plm(height_z ~ PC1money_mom + ls02_2 + c(ls02_2*ls02_2)+  school_att + edn09 + worked_12_mom + HH_mom + height_mom 
                   + height_dad  + edu_mom + edu_dad +  log_income_crea_pc + children + factor(year), data = pdata_child_t, model = "random", effect = "time",
                   subset = sex == 0, random.method = "amemiya")

coeftest(re_mod1_h_g)


## subset with boys ##

iv_mod1_t_b <- plm(test_score_z ~ female + ls02_2 + c(ls02_2*ls02_2)+  school_att + edn09 + worked_12_mom + HH_mom + test_score_mom 
                   + test_score_dad  + edu_mom + edu_dad +  log_income_crea_pc + children + factor(year), data = pdata_child_t, model = "pooling", subset = sex == 1)

summary(iv_mod1_t_b)

iv_mod1_h_b <- plm(height_z ~ female + ls02_2 + c(ls02_2*ls02_2)+  school_att + edn09 + worked_12_mom + HH_mom + height_mom 
                   + height_dad  + edu_mom + edu_dad +  log_income_crea_pc + children + factor(year), data = pdata_child_h, model = "pooling", subset = sex == 1)

coeftest(iv_mod1_h_b)

# with OLS bargaining index 

ols_mod1_t_g <- plm(test_score_z ~ PC1money_mom + ls02_2 + c(ls02_2*ls02_2)+  school_att + edn09 + worked_12_mom + HH_mom + test_score_mom
                    + test_score_dad  + edu_mom + edu_dad +  log_income_crea_pc + children + factor(year), data = pdata_child_t, model = "pooling", subset = sex == 0)

coeftest(ols_mod1_t_g)

# with bargainign index OLS

ols_mod1_t_b <- plm(test_score_z ~ PC1money_mom + ls02_2 + c(ls02_2*ls02_2)+  school_att + edn09 + worked_12_mom + HH_mom + test_score_mom
                    + test_score_dad  + edu_mom + edu_dad +  log_income_crea_pc + children + factor(year), data = pdata_child_t, model = "pooling", subset = sex == 1)

coeftest(ols_mod1_t_b)

ols_mod1_h_b <- plm(height_z ~ PC1money_mom + ls02_2 + c(ls02_2*ls02_2)+  school_att + edn09 + worked_12_mom + HH_mom  + height_mom 
                    + height_dad  + edu_mom + edu_dad +  log_income_crea_pc + children + factor(year), data = pdata_child_t, model = "pooling", subset = sex == 1)

coeftest(ols_mod1_h_b)

# with bargaining index fixed effects

fe_mod1_t_b <- plm(test_score_z ~ PC1money_mom + ls02_2 + c(ls02_2*ls02_2)+  school_att + edn09 + worked_12_mom + HH_mom + test_score_mom
                   + test_score_dad  + edu_mom + edu_dad +  log_income_crea_pc + children + factor(year), data = pdata_child_t, model = "within", effect = "time",
                   subset = sex == 1)

coeftest(fe_mod1_t_b)


fe_mod1_h_b <- plm(height_z ~ PC1money_mom + ls02_2 + c(ls02_2*ls02_2)+  school_att + edn09 + worked_12_mom + HH_mom + height_mom 
                   + height_dad  + edu_mom + edu_dad +  log_income_crea_pc + children + factor(year), data = pdata_child_t,  model = "within", effect = "time", 
                   subset = sex == 1)

coeftest(fe_mod1_h_b)

### random 

re_mod1_t_b <- plm(test_score_z ~ PC1money_mom + ls02_2 + c(ls02_2*ls02_2)+  school_att + edn09 + worked_12_mom + HH_mom + test_score_mom
                   + test_score_dad  + edu_mom + edu_dad +  log_income_crea_pc + children + factor(year), data = pdata_child_t, model = "random", effect = "time",
                   subset = sex == 1, random.method = "amemiya")

coeftest(re_mod1_t_b)

re_mod1_h_b <- plm(height_z ~ PC1money_mom + ls02_2 + c(ls02_2*ls02_2)+  school_att + edn09 + worked_12_mom + HH_mom + height_mom 
                   + height_dad  + edu_mom + edu_dad +  log_income_crea_pc + children + factor(year), data = pdata_child_t, model = "random", effect = "time",
                   subset = sex == 1, random.method = "amemiya")

coeftest(re_mod1_h_b)


## write table Raven ##
# not used in final study

shift_se <- list(sqrt(diag(vcovCR(iv_mod1_t, pdata_child_t$ent, type = "CR2"))),
                 sqrt(diag(vcovCR(iv_mod1_t_g, pdata_child_t$ent,type = "CR2"))),
                 sqrt(diag(vcovCR(iv_mod1_t_b, pdata_child_t$ent, type = "CR2"))))

stargazer(iv_mod1_t, iv_mod1_t_g, iv_mod1_t_b, omit = "year",
          digits = 3, header = FALSE, type = "html", dep.var.labels=c("Raven Test Z-Score"),
          covariate.labels=c("Demand for female labour", "Age", "Age squared", "Gender = boy", "School attendance", 
                             "Education level", "Mom works?", "Mom is household head", 
                             "Test score mom", "Test score dad", "Mom education", "Dad education",
                             "Household income pc", "# Children"),
          no.space = FALSE, 
          align = TRUE,
          single.row = TRUE,
          se = shift_se, model.numbers = FALSE,  column.labels = c("Full sample POLS (1)", "Girls sample POLS (2)", "Boys sample POLS (3)"), 
          column.sep.width = "5pt", # Well... you can tweak this,
          omit.labels = "Year dummies?",
          out = "Outputs/models/test_shift_edit.htm")

## write table height  ##

shift_se_h <- list(sqrt(diag(vcovCR(iv_mod1_h, pdata_child_t$mpio, type = "CR2"))),
                 sqrt(diag(vcovCR(iv_mod1_h_g, pdata_child_t$mpio,type = "CR2"))),
                 sqrt(diag(vcovCR(iv_mod1_h_b, pdata_child_t$mpio, type = "CR2"))))

stargazer(iv_mod1_h, iv_mod1_h_g, iv_mod1_h_b, omit = "year",
          digits = 3, header = FALSE, type = "html", dep.var.labels=c("Height Z-Score"),
          covariate.labels=c("Demand for female labour", "Age", "Age squared", "Gender = boy", 
                             "Education level", "Mom works?", "Mom is household head", 
                             "Height mom", "Height dad", "Mom education", "Dad education",
                             "Household income pc", "# Children"),
          no.space = FALSE,
          align = TRUE,
          single.row = TRUE,
          se = shift_se_h, model.numbers = FALSE,  column.labels = c("Full sample POLS (1)", "Girls sample POLS (2)", "Boys sample POLS (3)"), 
          column.sep.width = "5pt", # Well... you can tweak this,
          omit.labels = "Year dummies?",
          out = "Outputs/models/height_shift_edit.htm")

###### Raven with POLS FE #####

### boy and girls

# Fixed Effects

shift_se_gb <- list(
                    sqrt(diag(vcovCR(iv_mod1_t_g, pdata_child_t$mpio, type = "CR2"))),
                    sqrt(diag(vcovHC(fe_mod1_t_g, cluster = "group", type = "HC0"))),
                    sqrt(diag(vcovHC(iv_mod1_t_b, cluster = "group", type = "HC0"))),
                    sqrt(diag(vcovHC(fe_mod1_t_b, cluster = "group", type = "HC0")))
                    )

stargazer(iv_mod1_t_g, fe_mod1_t_g, iv_mod1_t_b, fe_mod1_t_b, omit = "year",
          digits = 3, header = FALSE, type = "html", dep.var.labels=c("Raven Test Z-Score"),
          covariate.labels=c("Demand for female labour", "Financial BP", "Age", "Age squared", "School attendance",
                             "Education level", "Mom works?", "Mom is household head",
                             "Test score mom", "Test score dad", "Mom education", "Dad education",
                             "Household income pc", "# Children"),
          no.space = FALSE,
          align = TRUE,
          single.row = TRUE,
          se = shift_se_gb, model.numbers = TRUE,  column.labels = c("Girls Shift-share", "Girls FE", "Boys Shift-share", "Boys FE"),
          column.sep.width = "5pt", # Well... you can tweak this,
          omit.labels = "Year dummies?",
          out = "Outputs/models/test_shift_gb.htm")

# Random Effects

shift_se_gb_re <- list(
  sqrt(diag(vcovCR(iv_mod1_t_g, pdata_child_t$mpio, type = "CR2"))),
  sqrt(diag(vcovHC(re_mod1_t_g, cluster = "group", type = "HC0"))),
  sqrt(diag(vcovHC(iv_mod1_t_b, cluster = "group", type = "HC0"))),
  sqrt(diag(vcovHC(re_mod1_t_b, cluster = "group", type = "HC0")))
)

stargazer(iv_mod1_t_g, re_mod1_t_g, iv_mod1_t_b, re_mod1_t_b, omit = "year",
          digits = 3, header = FALSE, type = "html", dep.var.labels=c("Raven Test Z-Score"),
          covariate.labels=c("Demand for female labour", "Financial BP", "Age", "Age squared", "School attendance",
                             "Education level", "Mom works?", "Mom is household head",
                             "Test score mom", "Test score dad", "Mom education", "Dad education",
                             "Household income pc", "# Children"),
          no.space = FALSE,
          align = TRUE,
          single.row = TRUE,
          se = shift_se_gb_re, model.numbers = TRUE,  column.labels = c("Girls Shift-share", "Girls RE", "Boys Shift-share", "Boys RE"),
          column.sep.width = "5pt", # Well... you can tweak this,
          omit.labels = "Year dummies?",
          out = "Outputs/models/test_shift_re_gb.htm")

###### Table height  #####

#boys and girls

# Fixed-Effects

shift_he_gb <- list(
                   sqrt(diag(vcovHC(iv_mod1_h_g, cluster = "group", type = "HC0"))),
                   sqrt(diag(vcovHC(fe_mod1_h_g, cluster = "group", type = "HC0"))),
                   sqrt(diag(vcovHC(iv_mod1_h_b, cluster = "group", type = "HC0"))),
                   sqrt(diag(vcovHC(fe_mod1_h_b, cluster = "group", type = "HC0")))
                   )

stargazer(iv_mod1_h_g, fe_mod1_h_g, iv_mod1_h_b, fe_mod1_h_b, omit = "year",
          digits = 3, header = FALSE, type = "html", dep.var.labels=c("Height Z-Score"),
          covariate.labels=c("Demand for female labour", "Financial BP", "Age", "Age squared", "School attendance",
                             "Education level", "Mom works?", "Mom is household head",
                             "Height mom", "Height dad", "Mom education", "Dad education",
                             "Household income pc", "# Children"),
          no.space = FALSE,
          align = TRUE,
          single.row = TRUE,
          se = shift_he_gb, model.numbers = TRUE,  column.labels = c("Girls Shift-share", "Girls FE", "Boys Shift-share", "Boys FE"),
          column.sep.width = "5pt", # Well... you can tweak this,
          omit.labels = "Year dummies?",
          out = "Outputs/models/height_shift_gb.htm")

# Random-Effects

shift_he_gb_re <- list(
  sqrt(diag(vcovHC(iv_mod1_h_g, cluster = "group", type = "HC0"))),
  sqrt(diag(vcovHC(re_mod1_h_g, cluster = "group", type = "HC0"))),
  sqrt(diag(vcovHC(iv_mod1_h_b, cluster = "group", type = "HC0"))),
  sqrt(diag(vcovHC(re_mod1_h_b, cluster = "group", type = "HC0")))
)

stargazer(iv_mod1_h_g, re_mod1_h_g, iv_mod1_h_b, re_mod1_h_b, omit = "year",
          digits = 3, header = FALSE, type = "html", dep.var.labels=c("Height Z-Score"),
          covariate.labels=c("Demand for female labour", "Financial BP", "Age", "Age squared", "School attendance",
                             "Education level", "Mom works?", "Mom is household head",
                             "Height mom", "Height dad", "Mom education", "Dad education",
                             "Household income pc", "# Children"),
          no.space = FALSE,
          align = TRUE,
          single.row = TRUE,
          se = shift_he_gb_re, model.numbers = TRUE,  column.labels = c("Girls Shift-share", "Girls RE", "Boys Shift-share", "Boys RE"),
          column.sep.width = "5pt", # Well... you can tweak this,
          omit.labels = "Year dummies?",
          out = "Outputs/models/height_shift_re_gb.htm")

