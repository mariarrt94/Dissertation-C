##### Dissertation Maria Reyes Retana - 
# This code runs the regressions to determine the effect of bargaining power in children outcomes using shift-share

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

source('Complete information for analysis.R')

shift_sector <- read.csv("Outputs/shocks_base_sec.csv") %>% 
  mutate(ent = as.numeric(ent), mpio = as.numeric(mpio)) %>% 
  select(-c(X))

##### Data for shift analysis ######

base_child <- base_child %>% ungroup() %>% 
  left_join(shift_sector) %>% 
  mutate(muni_com = paste(ent, mpio, sep = ""))

pdata_child_t <- pdata.frame(base_child, index = c("pid_link_uni", "year")) %>% 
  filter(!(decision_mom == 0 & decision_dad == 0)) %>% 
  filter(ls02_2 > 4 ) %>% 
  filter(!is.na(test_score))

panel_data_test <- panel_data(base_child, id = pid_link_uni, wave = year)

pdata_child_h <- pdata.frame(base_child, index = c("pid_link_uni", "year")) %>% 
  filter(!(decision_mom == 0 & decision_dad == 0)) %>% 
  filter(!is.na(hfa_z))

mom_base <- mom_base %>% left_join(shift_sector)

p_data_women <- pdata.frame(mom_base, index = c("pid_link_mom", "year")) %>% 
  mutate(log_income_mom = case_when(income_c_mom > 0 ~ log(income_c_mom), 
                                    TRUE ~ income_c_mom))

##### First step as relevant for mom #####

mod_fb <- plm(PC1money_mom ~ female + male + age_mom + c(age_mom*age_mom) + edu_mom + HH_mom + test_score_mom + married_mom,  data = p_data_women, model = "pooling")

summary(mod_fb)

mod_dec_fin <- plm(decision_finan ~ female + male + age_mom + c(age_mom*age_mom) + edu_mom + HH_mom + test_score_mom + married_mom, data = p_data_women, model = "pooling")

coeftest(mod_dec_fin)

mod_la <- plm(log_income_mom ~ female + age_mom + c(age_mom*age_mom) + edu_mom + HH_mom + test_score_mom + married_mom , data = p_data_women, model = "pooling")

summary(mod_la)

### this not reported for now, according with the context children index does not reflect

mod_dec_ch <- plm(PC1ch_mom ~ female + male + age_mom + c(age_mom*age_mom) + edu_mom + HH_mom + test_score_mom + married_mom, data = p_data_women, model = "pooling")

coeftest(mod_dec_ch)

# save table 

mom_se <- list(sqrt(diag(vcovHC(mod_fb, type = "HC1"))),
                  sqrt(diag(vcovHC(mod_dec_fin, type = "HC1"))),
                  sqrt(diag(vcovHC(mod_la, type = "HC1"))))

stargazer(mod_fb, mod_dec_fin, mod_la, 
          digits = 3, header = FALSE, type = "html", dep.var.labels=c("Financial bargaining index", "# Financial decisions", "Log income"),
          covariate.labels=c("Change female labour demand", "Change male labour demand", "Age", "Age squared", "Education level", "Household head",
                             "Test score", "Married"),
          no.space = TRUE, align = TRUE,
          se = mom_se, model.numbers = FALSE,  column.labels = c("POLS (1)", "POLS (2)", "POLS (3)"), 
          column.sep.width = "5pt", # Well... you can tweak this,
       #   omit.stat = c("adj.rsq"),
          out = "Outputs/model_barg.htm")

##### Reduced form: shift-share as proxy for bargaining power #####

iv_mod1_t <- plm(test_score_z ~ female + ls02_2 + c(ls02_2*ls02_2) + factor(sex) + school_att + edn09 + worked_12_mom + HH_mom + test_score_mom 
                 + test_score_dad  + edu_mom + edu_dad +  log_income_crea_pc + children + factor(year), data = pdata_child_t, model = "pooling")

coeftest(iv_mod1_t)

iv_mod1_h <- plm(height_z ~ female + ls02_2 + c(ls02_2*ls02_2)+  factor(sex) + edn09 + worked_12_mom + HH_mom + height_mom 
                 + height_dad  + edu_mom + edu_dad +  log_income_crea_pc + children + factor(year), data = pdata_child_h, model = "pooling")

coeftest(iv_mod1_h)

## subset with girls ##

iv_mod1_t_g <- plm(test_score_z ~ female + ls02_2 + c(ls02_2*ls02_2)+  school_att + edn09 + worked_12_mom + HH_mom + test_score_mom 
                   + test_score_dad  + edu_mom + edu_dad +  log_income_crea_pc + children + factor(year), data = pdata_child_t, model = "pooling", subset = sex == 0)

coeftest(iv_mod1_t_g)

iv_mod1_h_g <- plm(height_z ~ female + ls02_2 + c(ls02_2*ls02_2)+  edn09 + worked_12_mom + HH_mom + height_mom 
                   + height_dad  + edu_mom + edu_dad +  log_income_crea_pc + children + factor(year), data = pdata_child_h, model = "pooling", subset = sex == 0)

coeftest(iv_mod1_h_g)

## subset with boys ##

iv_mod1_t_b <- plm(test_score_z ~ female + ls02_2 + c(ls02_2*ls02_2)+  school_att + edn09 + worked_12_mom + HH_mom + test_score_mom 
                   + test_score_dad  + edu_mom + edu_dad +  log_income_crea_pc + children + factor(year), data = pdata_child_t, model = "pooling", subset = sex == 1)

summary(iv_mod1_t_b)

iv_mod1_h_b <- plm(height_z ~ female + ls02_2 + c(ls02_2*ls02_2)+  edn09 + worked_12_mom + HH_mom + height_mom 
                   + height_dad  + edu_mom + edu_dad +  log_income_crea_pc + children + factor(year), data = pdata_child_h, model = "pooling", subset = sex == 1)

coeftest(iv_mod1_h_b)

## write table Raven ##

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
          out = "Outputs/test_shift.htm")

## write table height  ##

shift_se_h <- list(sqrt(diag(vcovCR(iv_mod1_h, pdata_child_t$ent, type = "CR2"))),
                 sqrt(diag(vcovCR(iv_mod1_h_g, pdata_child_t$ent,type = "CR2"))),
                 sqrt(diag(vcovCR(iv_mod1_h_b, pdata_child_t$ent, type = "CR2"))))

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
          out = "Outputs/height_shift.htm")

###### In stata for probit panel #####

base_child_sta <- base_child %>%
  mutate(letter = substr(pid_link_uni, 7, 7),
         pid_link = case_when(letter == "A" ~ paste(substr(pid_link_uni, 1, 6), "1", substr(pid_link_uni, 8, 11), sep = ""),
                   letter == "B" ~ paste(substr(pid_link_uni, 1, 6), "2", substr(pid_link_uni, 8, 11), sep = ""),
                   letter == "C" ~ paste(substr(pid_link_uni, 1, 6), "2", substr(pid_link_uni, 8, 11), sep = ""),
                   TRUE ~ NA_character_),
         folio = case_when(letter == "A" ~ paste(substr(folio_uni, 1, 6), "1", substr(folio_uni, 8, 9), sep = ""),
                           letter == "B" ~ paste(substr(folio_uni, 1, 6), "2", substr(folio_uni, 8, 9), sep = ""),
                           letter == "C" ~ paste(substr(folio_uni, 1, 6), "2", substr(folio_uni, 8, 9), sep = ""),
                           TRUE ~ NA_character_)) %>%
  select(-c("letter"))

base_mom_sta <- mom_base %>%
  mutate(letter = substr(pid_link_mom, 7, 7),
         pid_link_mom = case_when(letter == "A" ~ paste(substr(pid_link_mom, 1, 6), "1", substr(pid_link_mom, 8, 11), sep = ""),
                              letter == "B" ~ paste(substr(pid_link_mom, 1, 6), "2", substr(pid_link_mom, 8, 11), sep = ""),
                              letter == "C" ~ paste(substr(pid_link_mom, 1, 6), "2", substr(pid_link_mom, 8, 11), sep = ""),
                              TRUE ~ NA_character_)) %>%
  select(-c("letter"))

write.dta(base_child_sta, "Outputs/base_child.dta")

write.dta(base_mom_sta, "Outputs/base_mom.dta")

###### IV: not for thesis ##### 

iv_mod_t2 <- plm(test_score_z ~ PC1money_mom + ls02_2 + c(ls02_2*ls02_2)+  school_att + edn09 + HH_mom + test_score_mom 
                 + test_score_dad  +  log_income_crea_pc + children + factor(year) | ls02_2 + c(ls02_2*ls02_2)+  school_att + edn09 + HH_mom + test_score_mom 
                 + test_score_dad  + log_income_crea_pc + children + factor(year) + female
                   , data = pdata_child_t, model = "fd")

summary(iv_mod_t2, diagnostics = TRUE)

