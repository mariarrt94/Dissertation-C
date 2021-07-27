##### Dissertation Maria Reyes Retana 
# This code contains the principal component analysis to create the index for women's bargaining power

##### Libraries #####

library(tidyverse)
library(readxl)
library(FactoMineR)
library(factoextra)
library(psych)

options(scipen=999)

##### Import data bases from Rdta #####

load('Outputs/Data_tidy_dissertation.RData')

codes_dec <- read_xlsx("Inputs/Codes.xlsx", sheet = "sequence") %>% 
  mutate(dec = as.character(dec))

##### Create women's agency index #####

women_index <- decisions %>%
  select(-c(date_int)) %>% 
  distinct() %>% 
  ungroup() %>% 
  filter(!is.na(pid_link)) %>% 
  gather("dec", "person", `1`:`12`) %>% 
  filter(ls05_1 == 1 |ls05_1 == 2) %>% 
  left_join(codes_dec) %>% 
  mutate(points_dec = case_when(person == "Spouse" ~  0, 
                                person == "Both" ~ 1, 
                                person == "Other" ~ 0, 
                                person == "Own" ~ 2, 
                                TRUE ~ NA_real_)) %>% 
  select(year, folio, folio_uni, pid_link_uni, decision, points_dec, decision_points) %>% 
  spread(decision, points_dec) %>% 
  drop_na()
 
  # select active variables: all decisions

  PCA_G <- PCA(women_index[c(6:17)], scale.unit = TRUE, ncp = 5, graph = TRUE)
  
  index_tot <- prcomp(women_index[c(6:17)], scale = TRUE, center = TRUE)
  
  summary(index_tot)
  
  # money-work decisions 7, 8, 9, 10, 11, 12
  
  PCA_work <- PCA(women_index[c("money_relatives", "money_spo_relatives", "own_work", 
                             "spouse_work", "strong_expenditure")], scale.unit = TRUE, ncp = 5, graph = TRUE)
  
  index_work <- prcomp(women_index[c("money_relatives", "money_spo_relatives", "own_work", 
                                     "spouse_work", "strong_expenditure")], scale = TRUE, center = TRUE)
  summary(index_work)
  
  # Agency index
  
  PCA_agency <- PCA(women_index[c("strong_expenditure", "own_clothes", "money_relatives", "children_health", "food_house")], scale.unit = TRUE, ncp = 5, graph = TRUE)
  
  index_agency <- prcomp(women_index[c("strong_expenditure", "own_clothes", "money_relatives", "children_health", "food_house")], scale = TRUE, center = TRUE)
 
   summary(index_agency)
  
  # household decisions 1:6, 12
  
  PCA_house <- PCA(women_index[c("children_clothes", "children_education", "children_health", 
                                 "contraceptives", "food_house", "own_clothes", "spouse_clothes")], scale.unit = TRUE, ncp = 5, graph = TRUE)
  
  index_house <- prcomp(women_index[c("children_clothes", "children_education", "children_health", 
                                      "contraceptives", "food_house", "own_clothes", "spouse_clothes")], scale = TRUE, center = TRUE)
  summary(index_house)
  
  ##### Graphs with PCA #####
  
  screeplot(compo_women, type = "l", npcs = 15, main = "Screeplot of the first 10 PCs")
  abline(h = 1, col="red", lty=5)
  legend("topright", legend=c("Eigenvalue = 1"),
         col=c("red"), lty=5, cex=0.6)
  
  cumpro <- cumsum(compo_women$sdev^2 / sum(compo_women$sdev^2))
  plot(cumpro[0:12], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot")
  abline(v = 9, col="blue", lty=5)
  abline(h = 0.89391, col="blue", lty=5)
  legend("topleft", legend=c("Cut-off @ PC9"),
         col=c("blue"), lty=5, cex=0.6)
  
  # include PCA into our original data base
  
  women_index$PC1tot <- index_tot$x[,1] 
  
  women_index$PC1work <- index_work$x[,1]
  
  women_index$PC1house <- index_house$x[,1]
  
  women_index$PC1agency <- index_agency$x[,1]
    
  women_index_res <- women_index %>% 
    select(year, folio, folio_uni, pid_link_uni, PC1tot, PC1work, PC1house, PC1agency)
  
  rm(list=setdiff(ls(), c("women_index", "women_index_res")))
  
  save.image(file = 'Outputs/Data_women_index.RData')
  
  
  
  