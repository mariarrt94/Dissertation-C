##### Dissertation Maria Reyes Retana - 
# This code runs the regressions to determine the effect of bargaining power in children outcomes using FE

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
library(sandwich)

##### Read data #####

source('Complete information for analysis.R')

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

base_child_02 <- base_child %>% filter(year == "2002") %>% filter(!is.na(health_stat))
base_child_05 <- base_child %>% filter(year == "2005") %>% filter(!is.na(health_stat))
base_child_09 <- base_child %>% filter(year == "2009") %>% filter(!is.na(health_stat))

##### Reported on dissertation: Test separated #####

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

rob_se_ts <- list(sqrt(diag(vcovHC(test_mod1_s, type = "HC1", cluster = "group"))),
                  sqrt(diag(vcovHC(test_mod2_s, type = "HC1", cluster = "group"))),
                  sqrt(diag(vcovHC(test_mod3_s, type = "HC1", cluster = "group"))),
                  sqrt(diag(vcovHC(test_mod4_s, type = "HC1", cluster = "group"))))

stargazer(test_mod1_s, test_mod2_s, test_mod3_s, test_mod4_s, 
          digits = 3, header = FALSE, type = "html", dep.var.labels=c("Raven Test Z-Score"),
          covariate.labels=c("Children bargaining Index", "Financial bargaining Index", "Age", "Age squared", "School attendance", 
                             "Education level", "Mom works?", "Mom is household head", 
                             "Test score mom", "Test score dad", "Mom education", "Dad education",
                             "Household income pc", "# Children"),
          no.space = FALSE, align = TRUE,
          single.row = TRUE,
          se = rob_se_ts, model.numbers = FALSE,  column.labels = c("POLS (1)", "Time FE (2)", "FE (3)", "FE (4)"), 
          add.lines = list(c('Individual Effects', 'No', 'No', 'Yes', 'Yes'),
                           c('Time Effects', 'No', 'Yes', 'No', 'Yes'),
                           c('F test fixed effects', '', '0.113', '1.210', '1.209')),
          column.sep.width = "5pt", # Well... you can tweak this,
          omit.stat = c("adj.rsq"),
          out = "Outputs/test_models_sep.htm")

##### Reported on dissertation: Height separated ######

hfa_mod1_s <- plm(height_z ~ PC1ch_mom + PC1money_mom + ls02_2 + c(ls02_2*ls02_2) + edn09 + worked_12_mom + HH_mom + height_mom 
                  + height_dad  + edu_mom + edu_dad + log_income_crea_pc + children,
                  data = pdata_child_h, model = "pooling")

hfa_mod2_s <- plm(height_z ~ PC1ch_mom + PC1money_mom + ls02_2 + c(ls02_2*ls02_2) + edn09 + worked_12_mom + HH_mom + height_mom 
                  + height_dad  + edu_mom + edu_dad + log_income_crea_pc + children,
                  data = pdata_child_h, model = "within", effect = "time")
pFtest(hfa_mod2_s, hfa_mod1_s)# F test for time effects: OLS vs pooled F = 0.62361, df1 = 2, df2 = 6365, p-value = 0.536

hfa_mod3_s <- plm(height_z ~ PC1ch_mom + PC1money_mom + ls02_2 + c(ls02_2*ls02_2) + edn09 + worked_12_mom + HH_mom + height_mom 
                  + height_dad  + edu_mom + edu_dad + log_income_crea_pc + children,
                  data = pdata_child_h, model = "within", effect = "individual")
pFtest(hfa_mod3_s, hfa_mod1_s)# F = 1.4456, df1 = 4766, df2 = 1601, p-value < 0.00000000000000022

hfa_mod4_s <- plm(height_z ~ PC1ch_mom + PC1money_mom + ls02_2 + c(ls02_2*ls02_2) + edn09 + worked_12_mom + HH_mom + height_mom 
                  + height_dad  + edu_mom + edu_dad + log_income_crea_pc + children,
                  data = pdata_child_h, model = "within", effect = "twoways")
pFtest(hfa_mod4_s, hfa_mod1_s)# F = 1.4469, df1 = 4768, df2 = 1599, p-value < 0.00000000000000022

hfa_mod5_s <- plm(height_z ~ PC1ch_mom + PC1money_mom + ls02_2 + c(ls02_2*ls02_2) + edn09 + worked_12_mom + HH_mom + height_mom 
                  + height_dad  + edu_mom + edu_dad + log_income_crea_pc + children,
                  data = pdata_child_h, model = "random")
phtest(hfa_mod4_s, hfa_mod5_s) #Hausman test  under H0 random is preferred 0.02 reject preferred is fixed

# generate clustered standard errors in a list v

rob_se_h_s <- list(sqrt(diag(vcovHC(hfa_mod1_s, type = "HC1", cluster = "group"))),
                   sqrt(diag(vcovHC(hfa_mod2_s, type = "HC1", cluster = "group"))),
                   sqrt(diag(vcovHC(hfa_mod3_s, type = "HC1", cluster = "group"))),
                   sqrt(diag(vcovHC(hfa_mod4_s, type = "HC1", cluster = "group"))))

stargazer(hfa_mod1_s, hfa_mod2_s, hfa_mod3_s, hfa_mod4_s, 
          digits = 3, header = FALSE, type = "html", dep.var.labels=c("Height Z-Score"),
          covariate.labels=c("Bargaining Index C", "Bargaining Index F", "Age", "Age squared", 
                             "Education level", "Mom works?", "Mom is household head", 
                             "Height mom", "Height dad", "Mom education", "Dad education",
                             "Log household income pc", "# Children"),
          no.space = FALSE, align = TRUE,
          single.row = TRUE,
          se = rob_se_h_s, model.numbers = FALSE,  column.labels = c("POLS (1)", "Time FE (2)", "FE (3)", "FE (4)"), 
          add.lines = list(c('Individual Effects', 'No', 'No', 'Yes', 'Yes'),
                           c('Time Effects', 'No', 'Yes', 'No', 'Yes'),
                           c('F test fixed effects', '', '0.624', '1.446', '1.447')),
          column.sep.width = "5pt", # Well... you can tweak this,
          omit.stat = c("adj.rsq"),
          out = "Outputs/hfa_models_sep.htm")

##### Health ordered probit not reported ######

health_mod1_s <- oglmx(health_stat ~ PC1ch_mom + PC1money_mom + ls02_2 + school_att + edn09 + worked_12_mom + HH_mom + height_mom 
                   + height_dad  + edu_mom + edu_dad + log_income_crea_pc + children,
                   data = base_child_02, link = "probit", constantMEAN = FALSE, constantSD = FALSE,
                   delta=0,threshparam = NULL)

health_mod2_s <- oglmx(health_stat ~ PC1ch_mom + PC1money_mom + ls02_2 + school_att + edn09 + worked_12_mom + HH_mom + height_mom 
                       + height_dad  + edu_mom + edu_dad + log_income_crea_pc + children,
                       data = base_child_05, link = "probit", constantMEAN = FALSE, constantSD = FALSE,
                       delta=0,threshparam = NULL)

health_mod3_s <- oglmx(health_stat ~ PC1ch_mom + PC1money_mom + ls02_2 + school_att + edn09 + worked_12_mom + HH_mom + height_mom 
                       + height_dad  + edu_mom + edu_dad + log_income_crea_pc + children,
                       data = base_child_09, link = "probit", constantMEAN = FALSE, constantSD = FALSE,
                       delta=0,threshparam = NULL)



# generate clustered standard errors in a list v

rob_se_health_s <- list(sqrt(diag(vcovHC(health_mod1_s, type = "HC1"))),
                      sqrt(diag(vcovHC(health_mod2_s, type = "HC1"))),
                      sqrt(diag(vcovHC(health_mod3_s, type = "HC1"))),
                      sqrt(diag(vcovHC(health_mod4_s, type = "HC1"))))

stargazer(health_mod1_s, health_mod2_s, health_mod3_s, health_mod4_s, 
          digits = 3, header = FALSE, type = "html", dep.var.labels=c("healthglobin Z-Score"),
          covariate.labels=c("Bargaining Index C", "Bargaining Index F", "Age" ,"School attendance", 
                             "Education level", "Mom works?", "Mom is household head", 
                             "Height mom", "Height dad", "Mom education", "Dad education",
                             "Log household income pc", "# Children"),
          no.space = TRUE, align = TRUE,
          se = rob_se_health_s, model.numbers = FALSE,  column.labels = c("(1)", "(2)", "(3)", "(4)"), 
          add.lines = list(c('Individual Effects', 'No', 'No', 'Yes', 'Yes'),
                           c('Time Effects', 'No', 'Yes', 'No', 'Yes'),
                           c('F test fixed effects', '', '2.230', '1.381', '1.386')),
          column.sep.width = "5pt", # Well... you can tweak this,
          omit.stat = c("adj.rsq"),
          out = "Outputs/health_models_sep.htm")


##### Total score models #####

test_mod1 <- plm(test_score_z ~ PC1tot_mom + PC2tot_mom + sex  + school_att + edn09 + worked_12_mom + HH_mom + test_score_mom 
                   + test_score_dad  + edu_mom + edu_dad + log_income_crea_pc + children,
                 data = pdata_child_t, model = "pooling")

test_mod2 <- plm(test_score_z ~ PC1tot_mom + PC2tot_mom + sex   + school_att + edn09 + worked_12_mom + HH_mom + test_score_mom 
                 + test_score_dad  + edu_mom + edu_dad +  log_income_crea_pc + children,
                        data = pdata_child_t, model = "within", effect = "time")

test_mod3 <- plm(test_score_z ~ PC1tot_mom + PC2tot_mom + sex  + school_att + edn09 + worked_12_mom + HH_mom + test_score_mom 
                 + test_score_dad  + edu_mom + edu_dad +  log_income_crea_pc + children,
                      data = pdata_child_t, model = "within", effect = "individual")

test_mod4 <- plm(test_score_z ~ PC1tot_mom + PC2tot_mom + sex  + school_att + edn09 + worked_12_mom + HH_mom + test_score_mom 
                 + test_score_dad  + edu_mom + edu_dad +  log_income_crea_pc + children,
                      data = pdata_child_t, model = "within", effect = "twoways")

test_mod5 <- plm(test_score_z ~ PC1tot_mom + PC2tot_mom + sex + school_att + edn09 + worked_12_mom + HH_mom + test_score_mom 
                 + test_score_dad  + edu_mom + edu_dad +  log_income_crea_pc + children,
                      data = pdata_child_t, model = "random")

# generate clustered standard errors in a list v

rob_se <- list(sqrt(diag(vcovHC(test_mod1, type = "HC1"))),
               sqrt(diag(vcovHC(test_mod2, type = "HC1"))),
               sqrt(diag(vcovHC(test_mod3, type = "HC1"))),
               sqrt(diag(vcovHC(test_mod4, type = "HC1"))))

stargazer(test_mod1, test_mod2, test_mod3, test_mod4, 
          digits = 3, header = FALSE, type = "html", dep.var.labels=c("Raven Test Score z PC12"),
          covariate.labels=c("Bargaining Index C", "Bargaining Index F", "Gender = boy", "Age", "School attendance", 
                             "Education level", "Mom works?", "Mom is household head", 
                             "Test score mom", "Test score dad", "Mom education", "Dad education",
                             "Household income pc", "# Children"),
          no.space = TRUE, align = TRUE,
          se = rob_se, model.numbers = FALSE,  column.labels = c("(1)", "(2)", "(3)", "(4)"), 
          add.lines = list(c('Individual Fixed Effects', 'No', 'No', 'Yes', 'Yes'),
                           c('Time Fixed Effects', 'No', 'Yes', 'No', 'Yes')),
          column.sep.width = "5pt", # Well... you can tweak this,
        # omit.stat = c("f"),
          out = "Outputs/test_models.htm")

##### Girls and boys test score same #####
# as in almost every category FE with individual are preferred we will only show FE in one table

test_mod1_g <- plm(test_score_z ~ PC1ch_mom + PC1money_mom + ls02_2 + c(ls02_2*ls02_2)  + school_att + edn09 + worked_12_mom + HH_mom + test_score_mom 
                 + test_score_dad  + edu_mom + edu_dad +  log_income_crea_pc + children,
                 data = pdata_child_t, model = "pooling", subset = sex == 0)

test_mod2_g <- plm(test_score_z ~ PC1ch_mom + PC1money_mom +ls02_2 + c(ls02_2*ls02_2) + school_att + edn09 + worked_12_mom + HH_mom + test_score_mom 
                 + test_score_dad  + edu_mom + edu_dad +  log_income_crea_pc + children,
                 data = pdata_child_t, model = "within", effect = "time", subset = sex == 0)
pFtest(test_mod2_g, test_mod1_g)# F test for time effects: OLS vs pooled F = 0.50483, df1 = 2, df2 = 1948, p-value = 0.6037 I cannot reject

test_mod3_g <- plm(test_score_z ~ PC1ch_mom + PC1money_mom +ls02_2 + c(ls02_2*ls02_2) + school_att + edn09 + worked_12_mom + HH_mom + test_score_mom 
                 + test_score_dad  + edu_mom + edu_dad +  log_income_crea_pc + children,
                 data = pdata_child_t, model = "within", effect = "individual", subset = sex == 0)
pFtest(test_mod3_g, test_mod1_g)# F test for ind effects: F = 0.97492, df1 = 1675, df2 = 275, p-value = 0.6174 I cannot reject

test_mod4_g <- plm(test_score_z ~ PC1ch_mom + PC1money_mom +ls02_2 + c(ls02_2*ls02_2) + school_att + edn09 + worked_12_mom + HH_mom  + test_score_mom 
                 + test_score_dad  + edu_mom + edu_dad +  log_income_crea_pc + children,
                 data = pdata_child_t, model = "within", effect = "twoways", subset = sex == 0)
pFtest(test_mod4_g, test_mod1_g)# F = 0.97344, df1 = 1677, df2 = 273, p-value = 0.6235 I cannot reject

test_mod1_b <- plm(test_score_z ~ PC1ch_mom + PC1money_mom  +ls02_2 + c(ls02_2*ls02_2)+ school_att + edn09 + worked_12_mom + HH_mom  +  test_score_mom 
                   + test_score_dad  + edu_mom + edu_dad +  log_income_crea_pc + children,
                   data = pdata_child_t, model = "pooling", subset = sex == 1)

test_mod2_b <- plm(test_score_z ~ PC1ch_mom + PC1money_mom +ls02_2 + c(ls02_2*ls02_2) + school_att + edn09 + worked_12_mom + HH_mom + test_score_mom 
                   + test_score_dad  + edu_mom + edu_dad +  log_income_crea_pc + children,
                   data = pdata_child_t, model = "within", effect = "time", subset = sex == 1)
pFtest(test_mod2_b, test_mod1_b) # OLS vs time F = 0.073547, df1 = 2, df2 = 2027, p-value = 0.9291

test_mod3_b <- plm(test_score_z ~ PC1ch_mom + PC1money_mom  +ls02_2 + c(ls02_2*ls02_2)+ school_att + edn09 + worked_12_mom + HH_mom  + test_score_mom 
                   + test_score_dad  + edu_mom + edu_dad +  log_income_crea_pc + children,
                   data = pdata_child_t, model = "within", effect = "individual", subset = sex == 1)
pFtest(test_mod3_b, test_mod1_b) # OLS vs Ind: reject F = 1.2727, df1 = 1737, df2 = 292, p-value = 0.004699 preferred FE

test_mod4_b <- plm(test_score_z ~ PC1ch_mom + PC1money_mom + ls02_2 + c(ls02_2*ls02_2) + school_att + edn09 + worked_12_mom + HH_mom + test_score_mom 
                   + test_score_dad  + edu_mom + edu_dad +  log_income_crea_pc + children,
                   data = pdata_child_t, model = "within", effect = "twoways", subset = sex == 1)
pFtest(test_mod4_b, test_mod1_b) # OLS vs Ind: reject F = 1.2679, df1 = 1739, df2 = 290, p-value = 0.005386

rob_se_bg <- list(sqrt(diag(vcovHC(test_mod3_g, type = "HC1", cluster = "group"))),
                 sqrt(diag(vcovHC(test_mod4_g, type = "HC1", cluster = "group"))),
                 sqrt(diag(vcovHC(test_mod3_b, type = "HC1", cluster = "group"))),
                 sqrt(diag(vcovHC(test_mod4_b, type = "HC1", cluster = "group"))))

stargazer(test_mod3_g, test_mod4_g, test_mod3_b, test_mod4_b, 
          digits = 3, header = FALSE, type = "html", dep.var.labels=c("Raven Test Z-Score"),
          covariate.labels=c("Bargaining Index C", "Bargaining Index F", "Age", "Age squared" ,"School attendance", 
                             "Education level", "Mom works?", "Mom is household head", 
                             "Test score mom", "Test score dad", "Mom education", "Dad education",
                             "Household income pc", "# Children"),
          no.space = FALSE, align = TRUE,
          single.row = TRUE,
          se = rob_se_bg, model.numbers = FALSE,  column.labels = c("FE girls (1)", "FE girls (2)", "FE boys (3)", "FE boys (4)"), 
          add.lines = list(c('Time Fixed Effects', 'No', 'Yes', 'No', 'Yes')),
          column.sep.width = "5pt", # Well... you can tweak this,
           omit.stat = c("adj.rsq"),
          out = "Outputs/test_models_b.htm")

##### Height girls and boys #####
# as all models containing fixed effects are preferred above those which don't I present Girls and Boys in one table.
hfa_mod1_g <- plm(height_z ~ PC1ch_mom + PC1money_mom + ls02_2 + c(ls02_2*ls02_2) + edn09  + worked_12_mom + HH_mom + height_mom 
                   + height_dad  + edu_mom + edu_dad +  log_income_crea_pc + children,
                   data = pdata_child_h, model = "pooling", subset = sex == 0)

hfa_mod2_g <- plm(height_z ~ PC1ch_mom + PC1money_mom + ls02_2 + c(ls02_2*ls02_2) + edn09  + worked_12_mom + HH_mom + height_mom 
                   + height_dad  + edu_mom + edu_dad +  log_income_crea_pc + children,
                   data = pdata_child_h, model = "within", effect = "time", subset = sex == 0)
pFtest(hfa_mod2_g, hfa_mod1_g)# F test for time effects: OLS vs pooled F = 0.64343, df1 = 2, df2 = 2419, p-value = 0.5256 I cannot reject

hfa_mod3_g <- plm(height_z ~ PC1ch_mom + PC1money_mom +ls02_2 + c(ls02_2*ls02_2) + edn09  + worked_12_mom + HH_mom + height_mom 
                   + height_dad  + edu_mom + edu_dad +  log_income_crea_pc + children,
                   data = pdata_child_h, model = "within", effect = "individual", subset = sex == 0)
pFtest(hfa_mod3_g, hfa_mod1_g)# F = 1.4949, df1 = 1954, df2 = 467, p-value = 0.00000006627 reject FE preferred

hfa_mod4_g <- plm(height_z ~ PC1ch_mom + PC1money_mom + ls02_2 + c(ls02_2*ls02_2) + edn09  + worked_12_mom + HH_mom + height_mom 
                   + height_dad  + edu_mom + edu_dad +  log_income_crea_pc + children,
                   data = pdata_child_h, model = "within", effect = "twoways", subset = sex == 0)
pFtest(hfa_mod4_g, hfa_mod1_g)# F = 1.5104, df1 = 1956, df2 = 465, p-value = 0.00000003411, FE preferred

hfa_mod1_b <- plm(height_z ~ PC1ch_mom + PC1money_mom  +ls02_2 + c(ls02_2*ls02_2) + edn09  + worked_12_mom + HH_mom + height_mom 
                  + height_dad  + edu_mom + edu_dad +  log_income_crea_pc + children,
                  data = pdata_child_h, model = "pooling", subset = sex == 1)

hfa_mod2_b <- plm(height_z ~ PC1ch_mom + PC1money_mom  + ls02_2 + c(ls02_2*ls02_2) + edn09  + worked_12_mom + HH_mom + height_mom 
                  + height_dad  + edu_mom + edu_dad +  log_income_crea_pc + children,
                  data = pdata_child_h, model = "within", effect = "time", subset = sex == 1)
pFtest(hfa_mod2_b, hfa_mod1_b)# F = 1.7384, df1 = 2, df2 = 2477, p-value = 0.176, I cannot reject OLS

hfa_mod3_b <- plm(height_z ~ PC1ch_mom + PC1money_mom +ls02_2 + c(ls02_2*ls02_2) + edn09  + worked_12_mom + HH_mom + height_mom 
                  + height_dad  + edu_mom + edu_dad +  log_income_crea_pc + children,
                  data = pdata_child_h, model = "within", effect = "individual", subset = sex == 1)
pFtest(hfa_mod3_b, hfa_mod1_b)# F = 1.3153, df1 = 2005, df2 = 474, p-value = 0.0001206 reject FE preferred

hfa_mod4_b <- plm(height_z ~ PC1ch_mom + PC1money_mom  + ls02_2 + c(ls02_2*ls02_2) + edn09 + worked_12_mom + HH_mom + height_mom 
                  + height_dad  + edu_mom + edu_dad +  log_income_crea_pc + children,
                  data = pdata_child_h, model = "within", effect = "twoways", subset = sex == 1)
pFtest(hfa_mod4_b, hfa_mod1_b)#F = 1.3321, df1 = 2007, df2 = 472, p-value = 0.00006398, FE preferred

rob_se_bg_h <- list(sqrt(diag(vcovHC(hfa_mod3_g, type = "HC1", cluster = "group"))),
                   sqrt(diag(vcovHC(hfa_mod4_g, type = "HC1", cluster = "group"))),
                   sqrt(diag(vcovHC(hfa_mod3_b, type = "HC1", cluster = "group"))),
                   sqrt(diag(vcovHC(hfa_mod4_b, type = "HC1", cluster = "group"))))

stargazer(hfa_mod3_g, hfa_mod4_g, hfa_mod3_b, hfa_mod4_b, 
          digits = 3, header = FALSE, type = "html", dep.var.labels=c("Height Z-score"),
          covariate.labels=c("Bargaining Index C", "Bargaining Index F", "Age", "Age squared",
                             "Education level" ,"Mom works?", "Mom is household head", 
                             "Height mom", "Height dad", "Mom education", "Dad education",
                             "Household income pc", "# Children"),
          single.row = TRUE,
          no.space = FALSE, align = TRUE,
          se = rob_se_bg_h, model.numbers = FALSE,  column.labels = c("FE girls (1)", "FE girls (2)", "FE boys (3)", "FE boys (4)"), 
          add.lines = list(c('Time Fixed Effects', 'No', 'Yes', 'No', 'Yes')),
          column.sep.width = "5pt", # Well... you can tweak this,
          omit.stat = c("adj.rsq"),
          out = "Outputs/height_models_GB.htm")

##### Total score tests #####
# 1. Test FE time versus pooled OLS: p-value is 0.21 ha is significant effects, we cannot reject then pool

pFtest(test_mod2, test_mod1)

# 2. Test FE individual versus pooled OLS: p-value is 0.0002 ha is significant effects, we reject pool

pFtest(test_mod3, test_mod1)

# 3. Test FE twoways versus pooled OLS: p-value is 0.00001 ha is significant effects, we reject pool

pFtest(test_mod4, test_mod1)

# 4. Hausman test, H0: is that the preferred model is random effects

phtest(test_mod4, test_mod5)

# p-value 0.09  we cannot reject the null at 5%, we should use the random effects.
# This test whether the unique errors are correlated with the regressors, the null is they are not, in which 
# the assumption for the random effects is valid. 

# 5. Breusch Pagan test for heresokedasticity

bptest(test_mod1) # p-value = .0194 there is presence so we correct using robust matrix

##### Total height #####

hfa_mod1 <- plm(height_z ~ PC1tot_mom + PC2tot_mom + sex + school_att + edn09 + worked_12_mom + HH_mom + height_mom 
                 + height_dad  + edu_mom + edu_dad + log_income_crea_pc + children,
                 data = pdata_child_t, model = "pooling")

hfa_mod2 <- plm(height_z ~ PC1tot_mom + PC2tot_mom + sex + school_att + edn09 + worked_12_mom + HH_mom + height_mom 
                 + height_dad  + edu_mom + edu_dad + log_income_crea_pc + children,
                 data = pdata_child_t, model = "within", effect = "time")

hfa_mod3 <- plm(height_z ~ PC1tot_mom + PC2tot_mom + sex + school_att + edn09 + worked_12_mom + HH_mom + height_mom 
                 + height_dad  + edu_mom + edu_dad + log_income_crea_pc + children,
                 data = pdata_child_t, model = "within", effect = "individual")

hfa_mod4 <- plm(height_z ~ PC1tot_mom + PC2tot_mom + sex + school_att + edn09 + worked_12_mom + HH_mom + height_mom 
                 + height_dad  + edu_mom + edu_dad + log_income_crea_pc + children,
                 data = pdata_child_t, model = "within", effect = "twoways")

hfa_mod5 <- plm(height_z ~ PC1tot_mom + PC2tot_mom + sex + school_att + edn09 + worked_12_mom + HH_mom + height_mom 
                 + height_dad  + edu_mom + edu_dad + log_income_crea_pc + children,
                 data = pdata_child_t, model = "random")

# generate clustered standard errors in a list v

rob_se_h <- list(sqrt(diag(vcovHC(hfa_mod1, type = "HC1"))),
               sqrt(diag(vcovHC(hfa_mod2, type = "HC1"))),
               sqrt(diag(vcovHC(hfa_mod3, type = "HC1"))),
               sqrt(diag(vcovHC(hfa_mod4, type = "HC1"))))

stargazer(hfa_mod1, hfa_mod2, hfa_mod3, hfa_mod4, 
          digits = 3, header = FALSE, type = "html", dep.var.labels=c("Height Z-Score"),
          covariate.labels=c("Bargaining Index C", "Bargaining Index F", "Gender", "School attendance", 
                             "Education level", "Mom works?", "Mom is household head", 
                             "Height mom", "Height dad", "Mom education", "Dad education",
                             "Log household income pc", "# Children"),
          no.space = TRUE, align = TRUE,
          se = rob_se_h, model.numbers = FALSE,  column.labels = c("(1)", "(2)", "(3)", "(4)"), 
          add.lines = list(c('Individual Fixed Effects', 'No', 'No', 'Yes', 'Yes'),
                           c('Time Fixed Effects', 'No', 'Yes', 'No', 'Yes')),
          column.sep.width = "5pt", # Well... you can tweak this,
          # omit.stat = c("f"),
          out = "Outputs/hfa_models.htm")

# Test FE versus pooled OLS

pFtest(height_female_fe, height_female_pool)

# p-value is 0.39 ha is significant effects, we cannot reject OLS pooled better choice

# Hausman test, H0: is that the preferred model is random effects

phtest(height_female_fe, height_female_random)

# p-value .00034 we reject random in favour of fixed effects, then random effects is not valid in this case. 


#test balancedness of the sample, 1 indicates balanced

punbalancedness(height_female_random)

