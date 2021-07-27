##### Dissertation Maria Reyes Retana
#This code creates the summary statistics graphs and tables needed. 

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

source('Main information for regression.R')

colors_pal <- c('#17406D','#0F6FC6','#009DD9','#176A7B','#0BD0D9',
                '#10CF9B','#5FF3CB','#A5C249','#C8DA92','#CC0066',
                '#FE001A','#FA5F00','#FEA300')

colores <- colors_pal[c(1,2,3, 4,5,6,7,8,9,10,11,12,13)]

show_col(colores)

fontcolor <- '#000f1c'

codes_sec <- read_xlsx("Inputs/Codes.xlsx", sheet = "sequence")

# The next command is to specify the font, it only has to be done once.
#loadfonts(device = "win")

# This code has summary statistics about: 1) decision making; 2) decision making and determinants; 3) children characteristics.

##### 1) Decision Making #####

graph_dec <- summary_dec %>% 
  mutate(gender = case_when(ls04 == 1 ~ "Male",
                            ls04 == 3  ~ "Female", 
                            TRUE ~ NA_character_),
         relation_HH = case_when(ls05_1 == 1 ~ "Household Head",
                                 TRUE ~ "Spouse of HH"))

mean_dec <- graph_dec %>% 
  group_by(year, gender, relation_HH) %>% 
  summarise(mean_dec = mean(decision_points))

# bar_dec <- graph_dec %>% 
#   ggplot(aes(x = gender, y = decision_points, fill = gender)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   facet_wrap(~ year) +
#   # geom_text(data = mean_dec, aes(label = round(mean_dec, 1), y = mean_dec +.06),
#   #           fontface = "bold", family = 'Century Gothic', size = 5) +
#   labs(title = element_blank(),
#        y = "Number of decisions taken (total 12)",
#        x = "Gender") + 
#   theme(text = element_text(size = 15, color = fontcolor, face = "bold", family = 'Century Gothic'),
#         axis.text=element_text(size = 12, color = fontcolor, face = "bold"),
#         axis.ticks = element_blank(),
#         panel.background = element_blank(),
#         panel.grid.major = element_blank(), 
#         plot.caption = element_text(hjust = 0),
#         panel.grid.minor = element_blank(),
#         legend.title = element_blank()) + 
#   scale_fill_manual(values = c(5, 3)) 

dec_summary_g <- decisions %>% 
  mutate(gender = case_when(ls04 == 1 ~ "Male",
                            ls04 == 3  ~ "Female", 
                            TRUE ~ NA_character_)) %>% 
  ungroup() %>% 
  filter(ls05_1 == 1 | ls05_1 == 2) %>% 
  select(year, gender, `1`:`12`) %>% 
  drop_na() %>% 
  gather(decision, decision_maker, `1`:`12`) %>% 
  group_by(year, decision, gender, decision_maker) %>% 
  summarise(tot = n()) %>% 
  ungroup() %>% 
  group_by(year, decision, gender) %>% 
  mutate(gen = sum(tot), porc = tot/gen) %>% 
  ungroup() %>% 
  mutate(gender_year = paste(year, gender, sep = "-"), dec = as.numeric(decision)) %>%
  select(gender_year, dec, decision_maker, porc, year, gender) %>% 
  left_join(codes_sec) %>% 
  filter(decision_maker == "Own") %>% 
  ggplot(aes(x = reorder(decision, -porc), y = porc, fill = as.factor(gender))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(x = reorder(decision, -porc), y = porc + .1, label = sprintf("%2.1f%%", porc*100), fontface = "bold", family = 'Calibri'), 
            position = position_dodge(width = 1)) + 
  facet_wrap(~ year) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  scale_fill_manual(values = c(5, 3)) +
  theme(legend.position ="none", 
        legend.title = element_blank(), axis.title.y = element_blank(),
        axis.text = element_text(size = 9, color = "#000f1c", face = "bold", family = 'Calibri'),
        plot.caption = element_text(hjust = 0, size = 9),
        legend.text = element_blank(),
        text = element_text(size = 9, color = "#000f1c", face = "bold", family = 'Calibri')) +
  ylab('Percentage of persons who reported making the decision alone') +
  labs(caption = 'Source: MxFLS-1, MxFLS-2, MxFLS-3.') +
  coord_flip()

# graph_dec_assets <- decisions %>% 
#   filter(!is.na(`7`)) %>% 
#   mutate(gender = case_when(ls04 == 1 ~ "Male",
#                             TRUE ~ "Female"),
#          relation_HH = case_when(ls05_1 == 1 ~ "Household Head",
#                                  TRUE ~ "Spouse of HH")) %>%
#   ggplot(aes(x = gender,  fill = `7`)) +
#   geom_bar(stat = "count") +
#   facet_wrap(~ year) +
#   labs(title = element_blank(),
#        y = "Who takes the decision os selling important assets",
#        x = "Gender") + 
#   theme(text = element_text(size = 15, color = fontcolor, face = "bold", family = 'Century Gothic'),
#         axis.text=element_text(size = 12, color = fontcolor, face = "bold"),
#         axis.ticks = element_blank(),
#         panel.background = element_blank(),
#         panel.grid.major = element_blank(), 
#         plot.caption = element_text(hjust = 0),
#         panel.grid.minor = element_blank(),
#         legend.title = element_blank()) + 
#   scale_fill_manual(values = colores)

# dec_heat <- decisions %>% 
#   #left_join(aux_adul, by = c("folio", "year", "pid_link", "pid_link_uni", "folio_uni")) %>%
#   filter(ls05_1 == 1 | ls05_1 == 2) %>% 
#   mutate(gender_hh = case_when(ls04 == 1 & ls05_1 == 1 ~ "M and HH",
#                                ls04 == 1 & ls05_1 == 2 ~ "M and S",
#                                ls04 == 3 & ls05_1 == 1 ~ "F and HH",
#                                ls04 == 3 & ls05_1 == 2 ~ "F and S",
#                             TRUE ~ NA_character_)) %>% 
#   filter(!is.na(ls04)) %>% 
#   filter(!is.na(test_score)) %>% 
#   ggplot(aes(x = reorder(gender_hh, decision_points, .desc = TRUE),
#              y = test_score, 
#              fill = decision_points)) +
#   geom_tile() + 
#   facet_wrap(~ year) +
#   scale_fill_gradient(low = "floralwhite", high = "#0BD0D9") +
#   theme(legend.position="bottom", axis.title.y = element_blank(),axis.title.x = element_blank(), 
#         legend.title=element_blank(),panel.background = element_blank(),
#         axis.text = element_text(size = 14, color = "#000f1c", face = "bold", family = 'Century Gothic'),
#         plot.caption = element_text(hjust = 0, size = 13),
#         legend.text = element_text(size = 10, color = "#000f1c", face = "bold", family = 'Century Gothic'),
#         text = element_text(size = 14, color = "#000f1c", face = "bold", family = 'Century Gothic'),
#         axis.text.x = element_text(angle = 90)) +
#   scale_y_continuous(labels = percent, limits = c(0, 1))

#   dec_graph_muj <- decisions %>% 
#     filter(ls04 == 3) %>% 
#     ungroup() %>% 
#     filter(!is.na(pid_link)) %>% 
#     mutate(gender_hh = case_when(ls04 == 1 & ls05_1 == 1 ~ "M and HH",
#                                  ls04 == 1 & ls05_1 == 2 ~ "M and S",
#                                  ls04 == 3 & ls05_1 == 1 ~ "F and HH",
#                                  ls04 == 3 & ls05_1 == 2 ~ "F and S",
#                                  TRUE ~ NA_character_)) %>%
#     gather("dec", "person", `1`:`12`) %>% 
#     filter(!person == "NA") %>%
#     select(year, pid_link_uni, dec, person, test_score, gender_hh) %>%
#     ggplot() + 
#     geom_bar(aes(x = factor(dec, ordered = TRUE, levels = c("1", "2", "3", "4", "5", "6", "7",
#                                                             "8", "9", "10", "11", "12")), fill = person), 
#              position = "fill") +
#     facet_wrap(~ year) +
#     labs(title = element_blank(),
#          y = "Percentage",
#          x = "Decision") + 
#     scale_fill_manual(values = colores) +
#     theme(legend.position="bottom", 
#           panel.background = element_blank(),
#           axis.text = element_text(size = 8, color = "#000f1c", face = "bold", family = 'Century Gothic'),
#           plot.caption = element_text(hjust = 0, size = 13),
#           legend.text = element_text(size = 10, color = "#000f1c", face = "bold", family = 'Century Gothic'),
#           text = element_text(size = 10, color = "#000f1c", face = "bold", family = 'Century Gothic')) +
#     scale_x_discrete(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")) +
#     scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1))
#  
# ##### 2) Determinants of decision-making power #####
  
 inc_dec_box <- mom_panel %>%
    filter(!is.na(decision_mom)) %>% 
   filter(!is.na(income_c_mom)) %>% 
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
   scale_y_continuous(labels = scales::comma_format()) +
    ylab('Income of the mother (Mexican Pesos)') +
    xlab('Number of decisions taken by the mother') +
    labs(title = 'Relation between the number of decisions taken by mother and her income',
         caption = '*The points inside the boxes represent the mean value.')
 
 # mean income vs decision:
 
 dec_income_mean <- mom_panel %>% 
   drop_na(decision_mom) %>% 
   group_by(year, decision_mom) %>% 
   summarise(income_c_mom = mean(income_c_mom, na.rm = TRUE)) %>% 
   ggplot(aes(x = factor(decision_mom, ordered = TRUE, levels = c("0", "1", "2", "3", "4", "5", "6", "7",
                                                                  "8", "9", "10", "11", "12")), y = income_c_mom)) + 
   geom_point(aes(colour = factor(year),size = .7)) + 
   facet_wrap(~year) +
   scale_color_manual(values = c("#0F6FC6","#009DD9","#176A7B")) +
   theme(legend.position ="none", 
         legend.title = element_blank(),
         axis.text = element_text(size = 10, color = "#000f1c", face = "bold", family = 'Calibri'),
         plot.caption = element_text(hjust = 0, size = 9),
         legend.text = element_blank(),
         text = element_text(size = 10, color = "#000f1c", face = "bold", family = 'Calibri')) +
   scale_y_continuous(labels = scales::comma_format()) +
   ylab('Annual income of the mother (Mexican Pesos)') +
   xlab('Number of decisions taken by the mother') +
   labs(caption = 'Source: MxFLS-1, MxFLS-2, MxFLS-3.')
 
##### 3) Children caracteristics  #####
  
 score_age <- base_child_p %>% 
    drop_na(test_score) %>% 
    filter(ls02_2 > 4) %>% 
    filter(ls02_2 < 13) %>% 
    group_by(year, ls02_2) %>% 
    summarise(test_score = mean(test_score, na.rm = TRUE), count = n()) %>% 
    ggplot(aes(x = factor(ls02_2, ordered = TRUE), y = test_score)) + 
    geom_point(aes(colour = factor(year),size = .7)) + 
    facet_wrap(~year) +
    scale_color_manual(values = c("#0F6FC6","#009DD9","#176A7B")) +
    theme(legend.position ="none", 
          legend.title = element_blank(),
          axis.text = element_text(size = 10, color = "#000f1c", face = "bold", family = 'Calibri'),
          plot.caption = element_text(hjust = 0, size = 9),
          legend.text = element_blank(),
          text = element_text(size = 10, color = "#000f1c", face = "bold", family = 'Calibri')) +
    scale_y_continuous(labels = scales::percent_format()) +
    ylab('Test Score') +
    xlab('Age') +
    labs(caption = 'Source: MxFLS-1, MxFLS-2, MxFLS-3.') 
  
##### Save plots #####
 
 ggsave("Outputs/Graphs/Number of decisions by gender and position.jpg",device = "jpeg",plot = bar_dec, width = 20, height = 10, units = "cm")
 
 ggsave("Outputs/Graphs/Number of decisions against income box.jpg",device = "jpeg",plot = inc_dec_box, width = 20, height = 15, units = "cm")
 
 ggsave("Outputs/Graphs/Decision by gender and role.jpg",device = "jpeg",plot = dec_graph_muj, width = 20, height = 10, units = "cm")
 
 ggsave("Outputs/Graphs/Number of decisions against income point.jpg",device = "jpeg",plot = dec_income_mean, width = 19, height = 10, units = "cm")
 
 ggsave("Outputs/Graphs/Test score vs. age.jpg",device = "jpeg",plot = score_age, width = 19, height = 10, units = "cm")
 
 ggsave("Outputs/Graphs/Percentage of decisions own.jpg",device = "jpeg",plot = dec_summary_g, width = 28, height = 20, units = "cm")
 