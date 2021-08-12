##### Dissertation Maria Reyes Retana 
# This code contains the principal component analysis to create the index for women's bargaining power

##### Libraries #####

library(tidyverse)
library(readxl)
library(FactoMineR)
library(factoextra)
library(psych)
library(corrplot)
library(extrafont)

options(scipen=999)

##### Import data bases from Rdta #####

load('Outputs/Data_tidy_dissertation.RData')

codes_dec <- read_xlsx("Inputs/Codes.xlsx", sheet = "sequence") %>% 
  mutate(dec = as.character(dec))

##### Create women's agency index #####

women_index <- decisions %>%
  select(-c(date_int)) %>% 
 # filter(ls04 == 3) %>% 
  distinct() %>% 
  ungroup() %>% 
  filter(!is.na(pid_link_uni)) %>% 
  gather("dec", "person", `1`:`12`) %>% 
  filter(ls05_1 == 1 |ls05_1 == 2) %>% 
  left_join(codes_dec) %>% 
  mutate(points_dec = case_when(person == "Spouse" ~  0, 
                                person == "Both" ~ 1, 
                                person == "Other" ~ 0, 
                                person == "Own" ~ 1, 
                                TRUE ~ NA_real_)) %>% 
  select(year, folio, folio_uni, pid_link_uni, decision, points_dec, decision_points) %>% 
  spread(decision, points_dec) %>% 
  drop_na()
 
  # select active variables: all decisions

  PCA_G <- PCA(women_index[c(6:17)], scale.unit = TRUE, ncp = 5, graph = TRUE)
  
  index_tot <- prcomp(women_index[c(6:17)], scale. = TRUE, center = TRUE)
  
  summary(index_tot)
  
  # only when filtering ls04 == 3 before doing the PCA
  # The x and rotation signs are negative which makes the interpretation less straightforward. 
  # Originally, a lower PC1 means a higher decisions making power being the more negative the "better" 
  # For that reason, I will change the sign of the eigenvalues and the rotation. This does not change results
  # Here a discussion on the matter: https://stats.stackexchange.com/questions/88880/does-the-sign-of-scores-or-of-loadings-in-pca-or-fa-have-a-meaning-may-i-revers
  # 
  # index_tot$x[,1] <- -index_tot$x[,1]
  # 
  # index_tot$rotation[,1] <- -index_tot$rotation[,1]
  # 
  # index_tot$x[,2] <- -index_tot$x[,2]
  # 
  # index_tot$rotation[,2] <- -index_tot$rotation[,2]
  
  ##### Tests and graphs #####
  
  # Plot of eigenvalues: variance explained by each principal component
  
 eigenv_tot <- fviz_eig(index_tot, addlabels = TRUE, ylim = c(0,30), ncp = 12) +
    theme(axis.text = element_text(size = 12, color = "#000f1c", face = "bold", family = 'Calibri'),
          plot.caption = element_text(hjust = 0, size = 8),
          text = element_text(size = 12, color = "#000f1c", face = "bold", family = 'Calibri')) +
    labs(title = "",
         x = "Principal Components", y = "% of explained variances", 
         caption = "Source: MxFLS-1, MxFLS-2, MxFLS-3.")
  
  ggsave("Outputs/Graphs/Number of decisions by gender and position.jpg",device = "jpeg",plot = eigenv_tot, width = 18, height = 10, units = "cm")
  
  # Plot of first 2 components
  
  graph_dir_pca <- fviz_pca_var(index_tot,
               col.var = "contrib", # Color by contributions to the PC
               gradient.cols = c(	"#3792cb", "#005b96", "#03396c", "#011f4b", "#000a14"),
               repel = TRUE,     # Avoid text overlapping,
               labelsize = 2) +
    theme(axis.text = element_text(size = 10, color = "#000f1c", face = "bold", family = 'Calibri'),
          plot.caption = element_text(hjust = 0, size = 10),
          text = element_text(size = 9, color = "#000f1c", face = "bold", family = 'Calibri')) + 
    labs(title = "",
         caption = "Source: MxFLS-1, MxFLS-2, MxFLS-3.")
  
  ggsave("Outputs/Graphs/Position of the first 2 components.jpg",device = "jpeg",plot = graph_dir_pca , width = 12, height = 12, units = "cm")
  
  # Contributions of variables to PC1
  contrib_pc1 <- fviz_contrib(index_tot, choice = "var", axes = 1, top = 10) +
    theme(axis.text = element_text(size = 10, color = "#000f1c", face = "bold", family = 'Calibri'),
          plot.caption = element_text(hjust = 0, size = 10),
          text = element_text(size = 9, color = "#000f1c", face = "bold", family = 'Calibri')) +
    labs(title = "",
         caption = "Source: MxFLS-1, MxFLS-2, MxFLS-3.")
  
  ggsave("Outputs/Graphs/Contribution PC1.jpg",device = "jpeg",plot = contrib_pc1, width = 17, height = 11, units = "cm")
  
  # Contributions of variables to PC2
  contrib_pc2 <- fviz_contrib(index_tot, choice = "var", axes = 2, top = 10) +
    theme(axis.text = element_text(size = 10, color = "#000f1c", face = "bold", family = 'Calibri'),
          plot.caption = element_text(hjust = 0, size = 10),
          text = element_text(size = 9, color = "#000f1c", face = "bold", family = 'Calibri')) +
    labs(title = "",
         caption = "Source: MxFLS-1, MxFLS-2, MxFLS-3.")
  
  ggsave("Outputs/Graphs/Contribution PC2.jpg",device = "jpeg",plot = contrib_pc2, width = 17, height = 11, units = "cm")
  
  ##### How many PC #####
  
  # by component SD
  
  sd <- index_tot$sdev
  var <- sd^2
  varPercent <- var/sum(var) * 100
  dev.new()
  
  barplot(varPercent, xlab='PC', ylab='Percent Variance', names.arg=1:length(varPercent), las=1, ylim=c(0, max(varPercent)), col='gray')
  abline(h=1/ncol(women_index[c(6:17)])*100, col='red')
  
  # According to tihs we should include 3 components in the analysis
  
  ##### Table for dissertation #####
  
  eig <- get_eig(index_tot)
  
  var <- get_pca_var(index_tot)
  
  pca_var <- var$cor[,1:2]
  
  corrplot(var$cos2, is.corr=FALSE)
  
  # by individual x = scores PCA specific for persons
  
  scores <- index_tot$x
  
  #by var
  
  loadings <- index_tot$rotation
  
  corr <- t(loadings)*sd
  
  #another way of computing correlation
  
  corr2 <- cor(scores, women_index[c(6:17)])
  
  # loadings
  
  table_pca <- as.data.frame(index_tot$rotation[,1:2]) %>% 
    arrange(desc(PC1))
  
  sqrt(1/ncol(women_index[c(6:17)])) #cut off for important loadings = 0.2886751
  
  table_pca$SD <- index_tot$sdev
  
  table_pca <- table_pca %>% 
    mutate(weight1 = PC1*SD, weight2 = PC2*SD)
  
  write.csv(table_pca, "Outputs/table_pca.csv")

  ##### Other decisions by group: main #####
  #money-work decisions 7, 8, 9, 10 most relevant in PC2 tot 

  PCA_work <- PCA(women_index[c(17,16,14,11,12)], scale.unit = TRUE, ncp = 5, graph = TRUE)

  index_work <- prcomp(women_index[c(17,16,14,11,12)], scale = TRUE, center = TRUE)
  summary(index_work)
  
  # corre
  
  var_work <- get_pca_var(index_work)
  
  pca_work <- var_work$cor[,1:2]
  
  corrplot(var_work$cos2, is.corr=FALSE)
  
  # graph 2 fisrt
  
  index_work$x[,1] <- - index_work$x[,1]
  index_work$x[,2] <- - index_work$x[,2]
  index_work$rotation[,1] <- - index_work$rotation[,1]
  index_work$rotation[,2] <- - index_work$rotation[,2]
  
  
  graph_dir_money <- fviz_pca_var(index_work, axes = c(1,2), 
                                col.var = "contrib", # Color by contributions to the PC
                                gradient.cols = c(	"#3792cb", "#005b96", "#03396c", "#011f4b", "#000a14"),
                                repel = TRUE,     # Avoid text overlapping,
                                labelsize = 2) +
    theme(axis.text = element_text(size = 10, color = "#000f1c", face = "bold", family = 'Calibri'),
          plot.caption = element_text(hjust = 0, size = 10),
          text = element_text(size = 9, color = "#000f1c", face = "bold", family = 'Calibri')) + 
    labs(title = "",
         caption = "Source: MxFLS-1, MxFLS-2, MxFLS-3.")
  
  ggsave("Outputs/Graphs/Position of the first 2 components work.jpg",device = "jpeg",plot = graph_dir_money , width = 12, height = 12, units = "cm")

  # # Agency index
  # 
  # PCA_agency <- PCA(women_index[c(17, 6, 9, 10, 11)], scale.unit = TRUE, ncp = 5, graph = TRUE)
  # 
  # index_agency <- prcomp(women_index[c(17, 6, 9, 10, 11)], scale = TRUE, center = TRUE)
  # 
  #  summary(index_agency)
   
   # correlogram
   
   # var_agency <- get_pca_var(index_agency)
   # 
   # pca_agency <- var_agency$cor[,1:2]
   # 
   # corrplot(var_agency$cos2, is.corr=FALSE)

  # children decisions relevant: 5,6,4,12

  PCA_ch <- PCA(women_index[c(6:10)], scale.unit = TRUE, ncp = 5, graph = TRUE)

  index_ch <- prcomp(women_index[c(6:10)], scale = TRUE, center = TRUE)
  
  summary(index_ch)
  
  ##### Table for dissertation #####
  
  eig_w <- get_eig(index_work)
  
  var_w <- get_pca_var(index_work)
  
  pca_var_w <- var_w$cor[,1:2]
  
  corrplot(var_w$cos2, is.corr=FALSE)
  
  # by individual x = scores PCA specific for persons
  
  scores_w <- index_work$x
  
  #by var
  
  loadings_w <- index_work$rotation
  
  corr <- t(loadings_w)*sd
  
  #another way of computing correlation
  
  corr2 <- cor(scores_w, women_index[c(6:17)])
  
  # loadings
  
  table_pca_w <- as.data.frame(index_work$rotation[,1:2]) %>% 
    arrange(desc(PC1)) %>% 
    select(PC1)%>% 
    mutate(Index = "Financial")
  
  table_pca_w$SD <- index_work$sdev
  
  sqrt(1/ncol(women_index[c(17,16,14,11,12)])) #cut off for important loadings = 0.447
  
  table_pca_c <- as.data.frame(index_ch$rotation[,1:2]) %>% 
    arrange(desc(PC1)) %>% 
    select(PC1) %>% 
    mutate(Index = "Children")
  
  table_pca_c$SD <- index_ch$sdev
  
  table_pca_sep <- table_pca_w %>% 
    rbind(table_pca_c) %>% 
    mutate(weight1 = PC1*SD)
  
  write.csv(table_pca_sep, "Outputs/table_pca_sep.csv")
  
  # correlogram
  
  var_ch <- get_pca_var(index_ch)
  
  pca_ch <- var_ch$cor[,1:2]
  
  corrplot(var_ch$cos2, is.corr=FALSE)
  
  graph_dir_ch <- fviz_pca_var(index_ch, axes = c(1,2), 
                                  col.var = "contrib", # Color by contributions to the PC
                                  gradient.cols = c(	"#3792cb", "#005b96", "#03396c", "#011f4b", "#000a14"),
                                  repel = TRUE,     # Avoid text overlapping,
                                  labelsize = 2) +
    theme(axis.text = element_text(size = 10, color = "#000f1c", face = "bold", family = 'Calibri'),
          plot.caption = element_text(hjust = 0, size = 10),
          text = element_text(size = 9, color = "#000f1c", face = "bold", family = 'Calibri')) + 
    labs(title = "",
         caption = "Source: MxFLS-1, MxFLS-2, MxFLS-3.")
  
  ggsave("Outputs/Graphs/Position of the first 2 components ch.jpg",device = "jpeg",plot = graph_dir_ch , width = 12, height = 12, units = "cm")
  

  # include PCA into our original data base
  
  women_index$PC1tot <- index_tot$x[,1] 
  
  women_index$PC2tot <- index_tot$x[,2] 
  
  # For money and agency index The x and rotation signs are negative which makes the interpretation less straightforward. 
  # Originally, a lower PC1 means a higher decisions making power being the more negative the "better" 
  # For that reason, I will change the sign of the eigenvalues and the rotation

  women_index$PC1money <- index_work$x[,1]

  women_index$PC1ch <- index_ch$x[,1]

 # women_index$PC1agency <- - index_agency$x[,1]
    
  women_index_res <- women_index %>% 
    select(year, folio, folio_uni, pid_link_uni, PC1tot, PC2tot, PC1money, PC1ch)
  
  rm(list=setdiff(ls(), c("women_index", "women_index_res")))
  
  save.image(file = 'Outputs/Data_women_index.RData')
  
  
  
  