##### Dissertation Maria Reyes Retana - Code for women's agency index

##### Libraries #####

library(tidyverse)
library(readxl)
library(FactoMineR)
library(factoextra)

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
 #filter(sex == 0) %>% 
  left_join(codes_dec) %>% 
  mutate(points_dec = case_when(person == "Spouse" ~  0, 
                                person == "Both" ~ 1, 
                                person == "Other" ~ 0, 
                                person == "Own" ~ 2, 
                                TRUE ~ NA_real_)) %>% 
  select(year, folio, folio_uni, pid_link_uni, decision, points_dec, decision_points) %>% 
  spread(decision, points_dec) %>% 
  drop_na()
  
  women_dec <- women_index %>% 
  select(-c("folio", "folio_uni", "year", "pid_link_uni", "decision_points"))
 
  # select active variables: only decisions
  
  dec_active <- women_index[, 5:16]

  prue <-PCA(dec_active, scale.unit = TRUE, ncp = 5, graph = TRUE)
  
  ind <- get_pca_ind(prue)
  
  pca_p <- prcomp(women_dec[], scale = TRUE, center = TRUE)
  
  compo_women <- prcomp(women_dec, scale = TRUE)
  
  # include PCA into our original data base
  
  women_index$PC1 <- compo_women$x[,1]
  
  women_index_res <- women_index %>% 
    select(year, folio, folio_uni, pid_link_uni, PC1)
  
  rm(list=setdiff(ls(), c("women_index", "women_index_res")))
  
  save.image(file = 'Outputs/Data_women_index.RData')
  
  
  
  