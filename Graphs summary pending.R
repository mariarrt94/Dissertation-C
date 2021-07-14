##### Graphs decisions/children/mom 

##### Libraries #####

library(tidyverse)
library(ggridges)
library(extrafont)
library(scales)
library(ggplot2)
library(hrbrthemes)
library(plm)
library(panelr)
library(outliers)

##### Read data and set fond and colors #####

load('Outputs/Data_tidy_dissertation.RData')

colors_pal <- c('#17406D','#0F6FC6','#009DD9','#176A7B','#0BD0D9',
                '#10CF9B','#5FF3CB','#A5C249','#C8DA92','#CC0066',
                '#FE001A','#FA5F00','#FEA300')

colores <- colors_pal[c(1,2,3, 4,5,6,7,8,9,10,11,12,13)]

fontcolor <- '#000f1c'

# The next command is to specify the font, it only has to be done once.
#loadfonts(device = "win")

##### Generate panel data for databases #####

base_child_p <- panel_data(base_child, id = pid_link_uni, wave = year) %>% 
  ungroup() 

base_factor_dec <- base_child_p %>% 
  filter(!is.na(decision_mom)) %>% 
  mutate(decision_mom = factor(decision_mom, ordered = TRUE, levels = c(0, 1, 2, 3, 4, 5, 6, 7, 8,
                                                                        9, 10, 11, 12)))

mom_panel <- panel_data(mom_base, id = pid_link_mom, wave = year) %>% 
  ungroup() %>% 
  mutate(log_income_mom = log(income_c_mom))

dad_panel <- panel_data(dad_base, id = pid_link_dad, wave = year) %>% 
  ungroup() %>% 
  mutate(log_income_dad = log(income_c_dad)) 

##### Graph decisions #####

graph_dec <- summary_dec %>% 
  mutate(gender = case_when(ls04 == 1 ~ "Male",
                            TRUE ~ "Female"),
         relation_HH = case_when(ls05_1 == 1 ~ "Household Head",
                                 TRUE ~ "Spouse of HH")) %>%
  ggplot(aes(x = gender, y = decision_points, fill = relation_HH)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ year) +
  labs(title = element_blank(),
       y = "Number of decisions taken (total 12)",
       x = "Gender") + 
  theme(text = element_text(size = 15, color = fontcolor, face = "bold", family = 'Century Gothic'),
        axis.text=element_text(size = 12, color = fontcolor, face = "bold"),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        plot.caption = element_text(hjust = 0),
        panel.grid.minor = element_blank(),
        legend.title = element_blank()) + 
  scale_fill_manual(values = c(5, 7))

graph_dec_tot <- decisions %>% 
  filter(!is.na(`7`)) %>% 
  mutate(gender = case_when(ls04 == 1 ~ "Male",
                            TRUE ~ "Female"),
         relation_HH = case_when(ls05_1 == 1 ~ "Household Head",
                                 TRUE ~ "Spouse of HH")) %>%
  ggplot(aes(x = gender,  fill = `7`)) +
  geom_bar(stat = "count") +
  facet_wrap(~ year) +
  labs(title = element_blank(),
       y = "Who takes the decision os selling important assets",
       x = "Gender") + 
  theme(text = element_text(size = 15, color = fontcolor, face = "bold", family = 'Century Gothic'),
        axis.text=element_text(size = 12, color = fontcolor, face = "bold"),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        plot.caption = element_text(hjust = 0),
        panel.grid.minor = element_blank(),
        legend.title = element_blank()) + 
  scale_fill_manual(values = colores)

graph_ds <- decisions %>% 
  #left_join(aux_adul, by = c("folio", "year", "pid_link", "pid_link_uni", "folio_uni")) %>%
  filter(ls05_1 == 1 | ls05_1 == 2) %>% 
  mutate(gender_hh = case_when(ls04 == 1 & ls05_1 == 1 ~ "M and HH",
                               ls04 == 1 & ls05_1 == 2 ~ "M and S",
                               ls04 == 3 & ls05_1 == 1 ~ "F and HH",
                               ls04 == 3 & ls05_1 == 2 ~ "F and S",
                            TRUE ~ NA_character_)) %>% 
  filter(!is.na(ls04)) %>% 
  filter(!is.na(test_score)) %>% 
  ggplot(aes(x = reorder(gender_hh, decision_points, .desc = TRUE),
             y = test_score, 
             fill = decision_points)) +
  geom_tile() + 
  facet_wrap(~ year) +
  scale_fill_gradient(low = "floralwhite", high = "#0BD0D9") +
  theme(legend.position="bottom", axis.title.y = element_blank(),axis.title.x = element_blank(), 
        legend.title=element_blank(),panel.background = element_blank(),
        axis.text = element_text(size = 14, color = "#000f1c", face = "bold", family = 'Century Gothic'),
        plot.caption = element_text(hjust = 0, size = 13),
        legend.text = element_text(size = 10, color = "#000f1c", face = "bold", family = 'Century Gothic'),
        text = element_text(size = 14, color = "#000f1c", face = "bold", family = 'Century Gothic'),
        axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(labels = percent, limits = c(0, 1))

# Distribution #

base_child_p %>%
  ungroup() %>% 
  filter(!is.na(test_score)) %>% 
  filter(!is.na(decision_mom)) %>% 
  ggplot(aes(x = test_score, color = as.factor(decision_mom))) +
  geom_density() +
  facet_wrap(~ year) +
  scale_fill_manual(values = colores) +
  theme(legend.position="bottom", axis.title.y = element_blank(),axis.title.x = element_blank(), 
        legend.title=element_blank(),panel.background = element_blank(),
        axis.text = element_text(size = 14, color = "#000f1c", face = "bold", family = 'Century Gothic'),
        plot.caption = element_text(hjust = 0, size = 13),
        legend.text = element_text(size = 10, color = "#000f1c", face = "bold", family = 'Century Gothic'),
        text = element_text(size = 14, color = "#000f1c", face = "bold", family = 'Century Gothic'),
        axis.text.x = element_text(angle = 90))

out <- boxplot.stats(base_child$test_score)$out
  
  line_plot(base_child_p, test_score, add.mean = TRUE)
  
  dec_graph <- decisions %>% 
    ungroup() %>% 
    filter(!is.na(pid_link)) %>% 
    mutate(gender_hh = case_when(ls04 == 1 & ls05_1 == 1 ~ "M and HH",
                                 ls04 == 1 & ls05_1 == 2 ~ "M and S",
                                 ls04 == 3 & ls05_1 == 1 ~ "F and HH",
                                 ls04 == 3 & ls05_1 == 2 ~ "F and S",
                                 TRUE ~ NA_character_)) %>%
    gather("dec", "person", `1`:`12`) %>% 
    filter(!person == "NA") %>%
    select(year, pid_link_uni, dec, person, test_score, gender_hh) %>%
    ggplot() + 
    geom_bar(aes(x = factor(dec, ordered = TRUE, levels = c("1", "2", "3", "4", "5", "6", "7",
                                                            "8", "9", "10", "11", "12")), fill = person), 
             position = "fill") +
    facet_wrap(~ year) +
    scale_fill_manual(values = colores) +
    theme(legend.position="bottom", axis.title.y = element_blank(), axis.title.x = element_blank(), 
          legend.title=element_blank(),panel.background = element_blank(),
          axis.text = element_text(size = 10, color = "#000f1c", face = "bold", family = 'Century Gothic'),
          plot.caption = element_text(hjust = 0, size = 13),
          legend.text = element_text(size = 10, color = "#000f1c", face = "bold", family = 'Century Gothic'),
          text = element_text(size = 10, color = "#000f1c", face = "bold", family = 'Century Gothic')) +
    scale_x_discrete(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) 
 
##### Determinants of decision-making power #####

 # greater income, greater decision power kind of
 plot(mom_panel$decision_mom ~ mom_panel$log_income_mom)
  
  mom_panel %>%
    filter(!is.na(decision_mom)) %>% 
    ggplot(aes(x = factor(decision_mom, ordered = TRUE, levels = c("0", "1", "2", "3", "4", "5", "6", "7",
                                                                   "8", "9", "10", "11", "12")) , y = income_c_mom, fill = factor(year))) +
    geom_boxplot() + 
    stat_summary(fun = mean,
                 geom = "point", size = 2, show.legend = TRUE) + 
    facet_wrap(~ year) +
  #  geom_text(data = dec_label, aes(label = paste(mean*100, "%", sep = ""), y = mean +.05),
   #           fontface = "bold", family = 'Century Gothic', size = 4, angle = 90) + 
    scale_fill_manual(values = colores) +
    theme(legend.position ="none", 
          legend.title = element_blank(),
          axis.text = element_text(size = 10, color = "#000f1c", face = "bold", family = 'Century Gothic'),
          plot.caption = element_text(hjust = 0, size = 9),
          legend.text = element_blank(),
          text = element_text(size = 10, color = "#000f1c", face = "bold", family = 'Century Gothic')) +
    ylab('Income of the mother') +
    xlab('Number of decisions taken') +
    labs(title = 'Relation between the number of decisions taken by mother and the test score of the child',
         caption = '*The points and values inside the boxes represent the mean value.')
  
  mom_panel %>% 
      drop_na(index_mom) %>% 
    ggplot(aes(x = decision_mom, y = log_income_mom)) +
    geom_smooth(method = lm) +
    facet_wrap(~ year) +
    scale_fill_manual(values = c(colores)) +
    theme(legend.position="bottom", axis.title.y = element_blank(), axis.title.x = element_blank(), 
          legend.title=element_blank(),
          axis.text = element_text(size = 10, color = "#000f1c", face = "bold", family = 'Century Gothic'),
          plot.caption = element_text(hjust = 0, size = 13),
          legend.text = element_text(size = 10, color = "#000f1c", face = "bold", family = 'Century Gothic'),
          text = element_text(size = 10, color = "#000f1c", face = "bold", family = 'Century Gothic')) 
  
  
  reg_mom <- lm(decision_mom ~ as.factor(dummy_div), mom_panel)
  
  summary(reg_mom)
 
##### Effect of decision on children outcomes #####
 
#test score against decision mom: idea CHECAR COMPONENTES PRINCIPALES
 
 dec_label <- base_factor_dec %>%
   group_by(decision_mom, year) %>% 
   summarise(mean = round(mean(test_score, na.rm = TRUE), digits = 3), 
             median = round(median(test_score, na.rm = TRUE), digits = 3))
  
  dec_label %>% 
    ggplot(aes(x = decision_mom, y = mean)) +
    geom_point() +
    facet_wrap(~ year) +
 
 dec_test <- base_factor_dec %>%
   ggplot(aes(x = decision_mom , y = test_score, fill = factor(year))) +
   geom_boxplot() + 
   stat_summary(fun = mean,
                geom = "point", size = 2, show.legend = TRUE) + 
    facet_wrap(~ year) +
    geom_text(data = dec_label, aes(label = paste(mean*100, "%", sep = ""), y = mean +.06),
             fontface = "bold", family = 'Century Gothic', size = 4, angle = 90) + 
   scale_fill_manual(values = colores) +
   theme(legend.position ="none", 
         legend.title = element_blank(),
         axis.text = element_text(size = 10, color = "#000f1c", face = "bold", family = 'Century Gothic'),
         plot.caption = element_text(hjust = 0, size = 9),
         legend.text = element_blank(),
         text = element_text(size = 10, color = "#000f1c", face = "bold", family = 'Century Gothic')) +
   scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
   ylab('Test score of the child') +
   xlab('Number of decisions taken by the mother') +
   labs(title = 'Relation between the number of decisions taken by mother and the test score of the child',
        caption = '*The points and values inside the boxes represent the mean value.')
 
 dec_test_jitter <- base_child_p %>%
   ggplot(aes(x = decision_mom, y = test_score, color = year)) +
   geom_point(position = "jitter") +
   facet_wrap(~ year) +
   scale_fill_manual(values = c(colores)) +
   theme(legend.position="bottom", axis.title.y = element_blank(), axis.title.x = element_blank(), 
         legend.title=element_blank(),
         axis.text = element_text(size = 10, color = "#000f1c", face = "bold", family = 'Century Gothic'),
         plot.caption = element_text(hjust = 0, size = 9),
         legend.text = element_text(size = 10, color = "#000f1c", face = "bold", family = 'Century Gothic'),
         text = element_text(size = 10, color = "#000f1c", face = "bold", family = 'Century Gothic')) +
   scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
   
 
 #remove outliers, there is something weird in 2009
 
 out_test <- outlier(base_child_p$test_score)
 
 out_women <- outlier(base_child_p$decision_women)
 
 # number of decisions woman - man: all positive relation
 
 test_smooth_women <- base_child_p %>% 
   drop_na(test_score) %>%
   filter(!test_score %in% out_test) %>% 
   filter(!decision_women %in% out_women) %>% 
   ggplot(aes(x = decision_women, y = test_score)) +
   geom_smooth(method = lm) +
   facet_wrap(~ year) +
   scale_fill_manual(values = c(colores)) +
   theme(legend.position="bottom", 
         legend.title=element_blank(),
         axis.text = element_text(size = 10, color = "#000f1c", face = "bold", family = 'Century Gothic'),
         plot.caption = element_text(hjust = 0, size = 9),
         legend.text = element_text(size = 10, color = "#000f1c", face = "bold", family = 'Century Gothic'),
         text = element_text(size = 10, color = "#000f1c", face = "bold", family = 'Century Gothic')) +
   scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
   ylab('Test score of the child') +
   xlab('Number of decisions taken by the mother minus decisions taken by the father') +
   labs(title = 'Relation between bargaining power of the mother and the test score of the child',
        caption = '*The bargaining power of the mother is computed as the number of decisions taken by the mother minus decisions taken by the father.')
 
 # number of decisions mom
 
 out_mom <- outlier(base_child_p$decision_mom)
 
 test_smooth_mom <- base_child_p %>% 
   drop_na(test_score) %>%
   filter(!test_score %in% out_test) %>% 
   filter(!decision_mom %in% out_mom) %>% 
   ggplot(aes(x = decision_mom, y = test_score)) +
   geom_smooth(method = lm) +
   facet_wrap(~ year) +
   scale_fill_manual(values = c(colores)) +
   theme(legend.position="bottom", axis.title.y = element_blank(), axis.title.x = element_blank(), 
         legend.title=element_blank(),
         axis.text = element_text(size = 10, color = "#000f1c", face = "bold", family = 'Century Gothic'),
         plot.caption = element_text(hjust = 0, size = 13),
         legend.text = element_text(size = 10, color = "#000f1c", face = "bold", family = 'Century Gothic'),
         text = element_text(size = 10, color = "#000f1c", face = "bold", family = 'Century Gothic')) +
   scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
   ylab('Test score of the child') +
   xlab('Number of decisions taken by the mother') +
   labs(title = 'Relation between number of decisions taken by the mother and the test score of the child')
 
 # index mom: weird not present for the moment
 
 out_index_mom <- outlier(base_child_p$index_mom)
 
 test_smooth_index <- base_child_p %>% 
   drop_na(test_score) %>%
   filter(!test_score %in% out_test) %>% 
   filter(!index_mom %in% out_index_mom) %>% 
   ggplot(aes(x = index_mom, y = test_score)) +
   geom_smooth(method = lm) +
   facet_wrap(~ year) +
   scale_fill_manual(values = c(colores)) +
   theme(legend.position="bottom", axis.title.y = element_blank(), axis.title.x = element_blank(), 
         legend.title=element_blank(),
         axis.text = element_text(size = 10, color = "#000f1c", face = "bold", family = 'Century Gothic'),
         plot.caption = element_text(hjust = 0, size = 13),
         legend.text = element_text(size = 10, color = "#000f1c", face = "bold", family = 'Century Gothic'),
         text = element_text(size = 10, color = "#000f1c", face = "bold", family = 'Century Gothic')) +
   scale_y_continuous(labels = scales::percent_format(accuracy = 1))
 
 reg <- lm(test_score ~ decision_women, base_child_p)
 
 summary(reg)
 