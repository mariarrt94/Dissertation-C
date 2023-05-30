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
  library(tidyr)

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
#  filter(ls05_1 == 1 |ls05_1 == 2) %>% 
  left_join(codes_dec) %>% 
  mutate(points_dec = case_when(person == "Spouse" ~  0, 
                                person == "Both" ~ 1, 
                                person == "Other" ~ 0, 
                                person == "Own" ~ 1, 
                                TRUE ~ NA_real_)) %>% 
  select(year, folio, folio_uni, pid_link_uni, decision, points_dec, decision_points, ls04, ls10) %>% 
  spread(decision, points_dec) %>% 
  drop_na() %>% 
  mutate(sex = ls04) %>% 
  filter(ls10 == 1 |  ls10==5) %>% # married or in union
  select(-c(ls04,ls10)) 
 
 
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
  
  ggsave("Outputs/graphs/Number of decisions by gender and position.jpg",device = "jpeg",plot = eigenv_tot, width = 18, height = 10, units = "cm")
  
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
  
  ggsave("Outputs/graphs/Position of the first 2 components.jpg",device = "jpeg",plot = graph_dir_pca , width = 12, height = 12, units = "cm")
  
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

  PCA_work <- PCA(women_index[c(17,16,14,11,12)], scale.unit = TRUE, ncp = 1000, graph = TRUE)

  index_work <- prcomp(women_index[c(17,16,14,11,12)], scale = TRUE, center = TRUE)
  summary(index_work)
  
  ###### # Extract the eigenvalues, explained variance, and cumulative explained variance
  eigenvalues <- index_work$sdev^2
  explained_variance <- eigenvalues / sum(eigenvalues)
  cumulative_variance <- cumsum(explained_variance)
  
  # Create a data frame with the eigenvalues, explained variance, and cumulative explained variance
  summary_table <- data.frame(
    Component = 1:length(eigenvalues),
    Eigenvalue = eigenvalues,
    ExplainedVariance = explained_variance,
    CumulativeVariance = cumulative_variance
  )
  
  # Transpose the loadings matrix
  transposed_loadings <- t(index_work$rotation)
  
  # Convert the transposed loadings matrix into a data frame
  loadings_df <- as.data.frame(transposed_loadings)
  
  # Add row names to the loadings data frame
  rownames(loadings_df) <- rownames(transposed_loadings)
  
  # Combine the summary table with the loadings data frame
  summary_table_with_loadings <- cbind(summary_table, loadings_df)
  
  # Print the summary table with loadings
  print(summary_table_with_loadings)
    
    
    #### regular scree plot 
    
    explained_variance <- index_work$sdev^2 / sum(index_work$sdev^2)    
    explained_variance_df <- data.frame(
      Component = 1:length(explained_variance),
      ExplainedVariance = explained_variance
    )
    
    
    scree_plot <- ggplot(explained_variance_df, aes(x = Component, y = ExplainedVariance)) +
      geom_point() +
      geom_line() +
      theme_minimal() +
      labs(
        title = "Scree Plot",
        x = "Principal Component",
        y = "Explained Variance"
      )
    
    print(scree_plot)
    
    #### scree plot with eigenvalues
    
    eigenvalues <- index_work$sdev^2
    eigenvalues_df <- data.frame(
      Component = 1:length(eigenvalues),
      Eigenvalue = eigenvalues
    )
    
    scree_plot_eigenvalues <- ggplot(eigenvalues_df, aes(x = Component, y = Eigenvalue)) +
      geom_point() +
      geom_line() +
      geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
      theme_minimal() +
      labs(
        title = "Scree Plot with Eigenvalues and Kaiser Criterion",
        x = "Principal Component",
        y = "Eigenvalue"
      )
    
    print(scree_plot_eigenvalues)

  # corre, solo variables
  
  work_variables <- women_index[c(17,16,14,11,12)]
  
  var_work <- get_pca_var(index_work)
  
  pca_work <- var_work$cor[,1:2]
  
  corrplot(var_work$cor, method = "number")
  
  cor_work = cor(work_variables)
  write.csv(cor_work, "Outputs/tables/cor_work.csv")
  corrplot(cor_work, method = "number")
  
  # table work: to see which variables have more weight 
  
  table_pca_work <- as.data.frame(index_work$rotation[,1:2]) %>% 
    arrange(desc(PC1))
  
  sqrt(1/ncol(women_index[c(6:17)])) #cut off for important loadings = 0.2886751
  
  table_pca_work $SD <- index_work$sdev
  
  table_pca_work  <- table_pca_work  %>% 
    mutate(weight1 = PC1*SD, weight2 = PC2*SD)
  
  write.csv(table_pca_work, "Outputs/tables/pca_work.csv", row.names = FALSE)
  
  # ## edit march 2023, work only strong expenditure, own work spouse's work, money relatives
  #  it does not work -commented
  # 
  # PCA_money <- PCA(women_index[c(17,16,14,12)], scale.unit = TRUE, ncp = 5, graph = TRUE)
  # 
  # index_money <- prcomp(women_index[c(17,16,14,12)], scale = TRUE, center = TRUE)
  # 
  # summary(index_money)
  # 
  # money_variables <- women_index[c(14,16,17,12)]
  # 
  # var_money <- get_pca_var(index_money)
  # 
  # pca_money <- var_money$cor[,1:2]
  # 
  # corrplot(var_money$cor, method = "number")
  # 
  # cor_money = cor(money_variables)
  # write.csv(cor_money, "Outputs/cor_money.csv", row.names = FALSE)
  # corrplot(cor_money, method = "number")
  
  #### table money
  
  table_pca_money <- as.data.frame(index_money$rotation[,1:2]) %>% 
    arrange(desc(PC1))
  
  sqrt(1/ncol(women_index[c(6:17)])) #cut off for important loadings = 0.2886751
  
  table_pca$SD <- index_tot$sdev
  
  table_pca <- table_pca %>% 
    mutate(weight1 = PC1*SD, weight2 = PC2*SD)
  
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
  
  
  index_money$x[,1] <- - index_money$x[,1]
  index_money$x[,2] <- - index_money$x[,2]
  index_money$rotation[,1] <- - index_money$rotation[,1]
  index_money$rotation[,2] <- - index_money$rotation[,2]
  
  
  graph_dir_money_2 <- fviz_pca_var(index_money, axes = c(1,2), 
                                  col.var = "contrib", # Color by contributions to the PC
                                  gradient.cols = c(	"#3792cb", "#005b96", "#03396c", "#011f4b", "#000a14"),
                                  repel = TRUE,     # Avoid text overlapping,
                                  labelsize = 2) +
    theme(axis.text = element_text(size = 10, color = "#000f1c", face = "bold", family = 'Calibri'),
          plot.caption = element_text(hjust = 0, size = 10),
          text = element_text(size = 9, color = "#000f1c", face = "bold", family = 'Calibri')) + 
    labs(title = "",
         caption = "Source: MxFLS-1, MxFLS-2, MxFLS-3.")
  
  ggsave("Outputs/graphs/Position of the first 2 components work.jpg",device = "jpeg",plot = graph_dir_money , width = 12, height = 12, units = "cm")
  
  ggsave("Outputs/graphs/Position of the first 2 components work - limited.jpg",device = "jpeg",plot = graph_dir_money_2 , width = 12, height = 12, units = "cm")
  

  # # Agency index
  # 
  #PCA_agency <- PCA(women_index[c(17, 6, 9, 10, 11)], scale.unit = TRUE, ncp = 5, graph = TRUE)
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

  PCA_ch <- PCA(women_index[c(6:9)], scale.unit = TRUE, ncp = 5, graph = TRUE)

  index_ch <- prcomp(women_index[c(6:9)], scale = TRUE, center = TRUE)
  
  summary(index_ch)
  
  # just corr
  ch_variables <- women_index[c(6:9)]
  cor_ch = cor(ch_variables)
  write.csv(cor_ch, "Outputs/cor_ch.csv")
  corrplot(cor_ch, method = "number")
  
  # todas 
  todas_variables <- women_index[c(6:10, 17,16,14,11,12,13,15)]
  cor_todas = cor(todas_variables)
  write.csv(cor_todas, "Outputs/cor_todas.csv")
  corrplot(cor_todas, method = "number")
  
  # children decisions removing contraception
  
  
  PCA_ch_new <- PCA(women_index[c(6,7,8)], scale.unit = TRUE, ncp = 5, graph = TRUE)
  
  index_ch_new <- prcomp(women_index[c(6,7,8)], scale = TRUE, center = TRUE)
  
  summary(index_ch_new)
  
  # just corr
  ch_new_variables <- women_index[c(6,7,8)]
  cor_ch_new = cor(ch_new_variables)
  write.csv(cor_ch_new, "Outputs/tables/cor_ch_new.csv")
  corrplot(cor_ch_new, method = "number")
  
  # todas 
  todas_variables <- women_index[c(6:10, 17,16,14,11,12,13,15)]
  cor_todas = cor(todas_variables)
  write.csv(cor_todas, "Outputs/tables/cor_todas.csv")
  corrplot(cor_todas, method = "number")
  
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
  
  table_pca_c <- as.data.frame(index_ch_new$rotation[,1:2]) %>% 
    arrange(desc(PC1)) %>% 
    select(PC1) %>% 
    mutate(Index = "Children")
  
  table_pca_c$SD <- index_ch_new$sdev
  
  table_pca_sep <- table_pca_w %>% 
    rbind(table_pca_c) %>% 
    mutate(weight1 = PC1*SD)
  
  write.csv(table_pca_sep, "Outputs/tables/table_pca_sep.csv")
  
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
  
  ggsave("Outputs/graphs/Position of the first 2 components ch.jpg",device = "jpeg",plot = graph_dir_ch , width = 12, height = 12, units = "cm")
  
  # now graph with children new 
  
  var_ch_new <- get_pca_var(index_ch_new)
  
  pca_ch_new <- var_ch_new$cor[,1:2]
  
  corrplot(var_ch_new$cos2, is.corr=FALSE)
  
  graph_dir_ch_new <- fviz_pca_var(index_ch_new, axes = c(1,2), 
                               col.var = "contrib", # Color by contributions to the PC
                               gradient.cols = c(	"#3792cb", "#005b96", "#03396c", "#011f4b", "#000a14"),
                               repel = TRUE,     # Avoid text overlapping,
                               labelsize = 2) +
    theme(axis.text = element_text(size = 10, color = "#000f1c", face = "bold", family = 'Calibri'),
          plot.caption = element_text(hjust = 0, size = 10),
          text = element_text(size = 9, color = "#000f1c", face = "bold", family = 'Calibri')) + 
    labs(title = "",
         caption = "Source: MxFLS-1, MxFLS-2, MxFLS-3.")
  
  ggsave("Outputs/graphs/Children PCA_new.jpg",device = "jpeg",plot = graph_dir_ch_new , width = 12, height = 12, units = "cm")
  

  # include PCA into our original data base
  
  women_index$PC1tot <- index_tot$x[,1] 
  
  women_index$PC2tot <- index_tot$x[,2] 
  
  # For money and agency index The x and rotation signs are negative which makes the interpretation less straightforward. 
  # Originally, a lower PC1 means a higher decisions making power being the more negative the "better" 
  # For that reason, I will change the sign of the eigenvalues and the rotation

  women_index$PC1money <- index_work$x[,1]

  women_index$PC1ch <- index_ch$x[,1]
  
  # include also the two new variables, money related and children without contraceptives
  
  women_index$PC1financial_new <- index_money$x[,1]
  
  women_index$PC1ch_new <- index_ch_new$x[,1]

 # women_index$PC1agency <- - index_agency$x[,1]
    
  women_index_res <- women_index %>% 
    select(year, folio, folio_uni, pid_link_uni, PC1tot, PC2tot, PC1money, PC1ch, PC1financial_new, PC1ch_new, sex)
  
  rm(list=setdiff(ls(), c("women_index", "women_index_res")))
  
  save.image(file = 'Outputs/Data_women_index.RData')
  
  ##### Evaluate repetition ----
  
  count <- women_index_res %>% 
    group_by(pid_link_uni,sex) %>% 
    summarise(count = n()) %>% 
    ungroup() %>% 
    group_by(count,sex) %>% 
    summarise(number_of_waves = n()) %>% 
    mutate(sex = ifelse(sex==3,"woman", "man"))
  
  # 1. with PCA money decisions
  repetition_money <- women_index_res %>% 
    select(folio_uni, pid_link_uni, year, folio_uni, pid_link_uni, PC1money) %>% 
    arrange(pid_link_uni) %>% 
    pivot_wider(names_from = year, values_from = c(PC1money)) %>% 
    filter(!((is.na(`2002`) & is.na(`2005`)) | (is.na(`2002`) & is.na(`2009`) | (is.na(`2009`) & is.na(`2005`))))) %>% 
    mutate(repetition1 = `2002`/`2005`, 
           repetition2 = `2005`/`2009`,
           repetition3 = `2002`/`2009`) %>% 
    mutate(grado1 = case_when(repetition1 == 1| repetition2 ==1 | repetition3 ==1 ~ "repetition or",
                              TRUE ~ "no repetition"), 
           grado2 = case_when(repetition1 == 1 & repetition2 ==1 & repetition3 ==1 ~ "repetition and",
                              TRUE ~ "no repetition")) %>% 
    mutate(level_repetition = case_when(grado1 == "no repetition" ~ "no repetition", 
                                        grado2 == "repetition and" ~ "repetition and", 
                                        TRUE ~ "repetition or")) %>% 
    group_by(level_repetition) %>% 
    summarise(count = n()) %>% 
    mutate(tipo = "financial original")
  
  # 1.2 with OCA money but new
  
  repetition_money_new <- women_index_res %>% 
    select(folio_uni, pid_link_uni, year, folio_uni, pid_link_uni, PC1financial_new) %>% 
    arrange(pid_link_uni) %>% 
    pivot_wider(names_from = year, values_from = c(PC1financial_new)) %>% 
    filter(!((is.na(`2002`) & is.na(`2005`)) | (is.na(`2002`) & is.na(`2009`) | (is.na(`2009`) & is.na(`2005`))))) %>% 
    mutate(repetition1 = `2002`/`2005`, 
           repetition2 = `2005`/`2009`,
           repetition3 = `2002`/`2009`) %>% 
    mutate(grado1 = case_when(repetition1 == 1| repetition2 ==1 | repetition3 ==1 ~ "repetition or",
                              TRUE ~ "no repetition"), 
           grado2 = case_when(repetition1 == 1 & repetition2 ==1 & repetition3 ==1 ~ "repetition and",
                              TRUE ~ "no repetition")) %>% 
    mutate(level_repetition = case_when(grado1 == "no repetition" ~ "no repetition", 
                                        grado2 == "repetition and" ~ "repetition and", 
                                        TRUE ~ "repetition or")) %>% 
    group_by(level_repetition) %>% 
    summarise(count = n()) %>% 
    mutate(tipo = "financial new")
  
  
 # 2. children decisions
  repetition_ch <- women_index_res %>% 
    select(folio_uni, pid_link_uni, year, folio_uni, pid_link_uni, PC1ch) %>% 
    arrange(pid_link_uni) %>% 
    pivot_wider(names_from = year, values_from = c(PC1ch)) %>% 
    mutate(repetition1 = `2002`/`2005`, 
           repetition2 = `2005`/`2009`,
           repetition3 = `2002`/`2009`) %>% 
    mutate(grado1 = case_when(repetition1 == 1| repetition2 ==1 | repetition3 ==1 ~ "repetition or",
                              TRUE ~ "no repetition"), 
           grado2 = case_when(repetition1 == 1 & repetition2 ==1 & repetition3 ==1 ~ "repetition and",
                              TRUE ~ "no repetition")) %>% 
    mutate(level_repetition = case_when(grado1 == "no repetition" ~ "no repetition", 
                                        grado2 == "repetition and" ~ "repetition and", 
                                        TRUE ~ "repetition or")) %>% 
    group_by(level_repetition) %>% 
    summarise(count = n()) %>% 
    mutate(tipo = "children original")
  
  # 2.2 children decisions but new
  
  repetition_ch_new <- women_index_res %>% 
    select(folio_uni, pid_link_uni, year, folio_uni, pid_link_uni, PC1ch_new) %>% 
    arrange(pid_link_uni) %>% 
    pivot_wider(names_from = year, values_from = c(PC1ch_new)) %>% 
    mutate(repetition1 = `2002`/`2005`, 
           repetition2 = `2005`/`2009`,
           repetition3 = `2002`/`2009`) %>% 
    mutate(grado1 = case_when(repetition1 == 1| repetition2 ==1 | repetition3 ==1 ~ "repetition or",
                              TRUE ~ "no repetition"), 
           grado2 = case_when(repetition1 == 1 & repetition2 ==1 & repetition3 ==1 ~ "repetition and",
                              TRUE ~ "no repetition")) %>% 
    mutate(level_repetition = case_when(grado1 == "no repetition" ~ "no repetition", 
                                        grado2 == "repetition and" ~ "repetition and", 
                                        TRUE ~ "repetition or")) %>% 
    group_by(level_repetition) %>% 
    summarise(count = n()) %>% 
    mutate(tipo = "children new")
  
  
# data of repetition 
  
  repetition <- repetition_ch_new %>% 
    rbind(repetition_ch) %>% 
    rbind(repetition_money) %>% 
    rbind(repetition_money_new) %>% 
    pivot_wider(names_from = level_repetition, values_from = count) %>% 
    mutate(percentage_some = (`repetition or` + `repetition and`)/(`repetition or`+`no repetition` + `repetition and`), 
           percentage_all = (`repetition and`)/(`repetition or`+`no repetition` + `repetition and`))
  # save excel with repetition and count of cases
list <- list(repetition, count)

writexl::write_xlsx(list,"Outputs/old/repetition_counts.xlsx")
