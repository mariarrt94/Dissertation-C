##### Dissertation Maria Reyes Retana
# This code creates the graphs to start analysing visually the effect of women bargaining power in 
# children outcomes

##### Libraries #####

library(tidyverse)
library(ggridges)
library(extrafont)
library(scales)
library(ggplot2)
library(hrbrthemes)
library(outliers)

##### Read data and set fond and colors #####

source('5_complete_data.R')

colors_pal <- c('#17406D','#0F6FC6','#009DD9','#176A7B','#0BD0D9',
                '#10CF9B','#5FF3CB','#A5C249','#C8DA92','#CC0066',
                '#FE001A','#FA5F00','#FEA300')

colores <- colors_pal[c(1,2,3, 4,5,6,7,8,9,10,11,12,13)]

show_col(colores)

fontcolor <- '#000f1c'

##### Income versus index #####

# for results part

inc_dec_smooth <- mom_base %>% 
  drop_na(PC1money_mom) %>% 
  ggplot(aes(x = PC1money_mom, y = income_c_mom)) +
  geom_smooth(method = lm) +
  facet_wrap(~ year) +
  scale_fill_manual(values = c(colores)) +
  theme(legend.position="bottom", 
        legend.title=element_blank(),
        axis.text = element_text(size = 10, color = "#000f1c", face = "bold", family = 'Century Gothic'),
        plot.caption = element_text(hjust = 0, size = 13),
        legend.text = element_text(size = 10, color = "#000f1c", face = "bold", family = 'Century Gothic'),
        text = element_text(size = 10, color = "#000f1c", face = "bold", family = 'Century Gothic')) +
  scale_x_continuous(labels = scales::comma_format(accuracy = 1), breaks = seq(-4, 6, by = 1)) +
  ylab('Log income of the mother') +
  xlab('Index of bargaining power') +
  labs(title = 'Relation between female bargaining power and her income')

##### Effect of decision on children outcomes #####

# Distribution #

base_child_p %>%
  ungroup() %>% 
  filter(ls02_2 > 4) %>% 
  filter(ls02_2 < 13) %>% 
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

#test score and hfa against decision mom box

dec_label <- base_factor_dec %>%
  filter(ls02_2 < 13) %>% 
  group_by(decision_mom, year) %>% 
  summarise(mean = round(mean(test_score, na.rm = TRUE), digits = 3), 
            median = round(median(test_score, na.rm = TRUE), digits = 3))

dec_test_box <- base_factor_dec %>%
  filter(ls02_2 < 13) %>% 
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

dec_label_hfa <- base_factor_dec %>%
  group_by(decision_mom, year) %>% 
  summarise(mean = round(mean(hfa_z, na.rm = TRUE), digits = 3), 
            median = round(median(hfa_z, na.rm = TRUE), digits = 3))

dec_hfa_box <- base_factor_dec %>%
  ggplot(aes(x = decision_mom , y = hfa_z, fill = factor(year))) +
  geom_boxplot() + 
  stat_summary(fun = mean,
               geom = "point", size = 2, show.legend = TRUE) + 
  facet_wrap(~ year) +
  geom_text(data = dec_label_hfa, aes(label = mean, y = mean +.06),
            fontface = "bold", family = 'Century Gothic', size = 4, angle = 90) + 
  scale_fill_manual(values = colores) +
  theme(legend.position ="none", 
        legend.title = element_blank(),
        axis.text = element_text(size = 10, color = "#000f1c", face = "bold", family = 'Century Gothic'),
        plot.caption = element_text(hjust = 0, size = 9),
        legend.text = element_blank(),
        text = element_text(size = 10, color = "#000f1c", face = "bold", family = 'Century Gothic')) +
  scale_y_continuous(labels = scales::comma_format()) +
  ylab('Height for age z-score of the child') +
  xlab('Number of decisions taken by the mother') +
  labs(title = 'Relation between the number of decisions taken by mother and the test score of the child',
       caption = '*The points and values inside the boxes represent the mean value.')

#remove outliers, there is something weird in 2009

out_test <- outlier(base_child_p$test_score)

out_hfa <- outlier(base_child_p$hfa_z)

out_women <- outlier(base_child_p$decision_women)

# number of decisions woman - man: all positive relation

test_smooth_women <- base_child_p %>% 
  drop_na(test_score) %>%
  filter(ls02_2 < 13) %>%filter(ls02_2 < 13) %>%
  #  filter(!test_score %in% out_test) %>% 
  #  filter(!decision_women %in% out_women) %>% 
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
  drop_na(decision_mom) %>%
  filter(ls02_2 < 13) %>%
  ggplot(aes(x = decision_mom, y = test_score)) +
  geom_smooth(method = lm) +
  facet_wrap(~ year) +
  scale_fill_manual(values = c(colores)) +
  theme(legend.position="bottom", 
        legend.title=element_blank(),
        axis.text = element_text(size = 10, color = "#000f1c", face = "bold", family = 'Century Gothic'),
        plot.caption = element_text(hjust = 0, size = 13),
        legend.text = element_text(size = 10, color = "#000f1c", face = "bold", family = 'Century Gothic'),
        text = element_text(size = 10, color = "#000f1c", face = "bold", family = 'Century Gothic')) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(labels = scales::comma_format(accuracy = 1), breaks = seq(0, 12, by = 1)) +
  ylab('Test score of the child') +
  xlab('Number of decisions taken by the mother') +
  labs(title = 'Relation between number of decisions taken by the mother and the test score of the child')

hfa_smooth_mom <- base_child_p %>% 
  drop_na(hfa_z) %>%
  drop_na(decision_mom) %>%
  ggplot(aes(x = decision_mom, y = hfa_z)) +
  geom_smooth(method = lm) +
  facet_wrap(~ year) +
  scale_fill_manual(values = c(colores)) +
  theme(legend.position="bottom", 
        title = element_text(hjust = 0.5),
        axis.text = element_text(size = 10, color = "#000f1c", face = "bold", family = 'Century Gothic'),
        plot.caption = element_text(hjust = 0, size = 9),
        legend.text = element_text(size = 10, color = "#000f1c", face = "bold", family = 'Century Gothic'),
        text = element_text(size = 10, color = "#000f1c", face = "bold", family = 'Century Gothic')) +
  scale_y_continuous(labels = scales::comma_format(accuracy = .2)) +
  scale_x_continuous(labels = scales::comma_format(accuracy = 1), breaks = seq(0, 12, by = 1)) +
  ylab('Height for age z-score of the child') +
  xlab('Number of decisions taken by the mother') +
  labs(title = 'Relation between number of decisions taken by the mother and the height for age z-score of the child',
       caption = '*The z-score was computed by substracting the mean and dividing by the standard deviationin the corresponding 
        age and sex group in the reference population established by the U.S. National Center for health Statistics a group of 
        well-nourished U.S children.')

# index mom: weird not present for the moment

test_smooth_index <- base_child_p %>% 
  filter(ls02_2 < 13) %>%
  drop_na(test_score) %>%
  drop_na(PC1tot) %>%
  ggplot(aes(x = PC1tot, y = test_score_z)) +
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
  labs(title = 'Relation between number of decisions taken by the mother and the test-score of the child')

test_smooth_index <- base_child_p %>% 
  drop_na(hfa_z) %>% 
  drop_na(PC1tot) %>%
  ggplot(aes(x = PC1tot, y = hfa_z)) +
  geom_smooth(method = lm) +
  facet_wrap(~ year) +
  scale_fill_manual(values = c(colores)) +
  theme(legend.position="bottom", axis.title.y = element_blank(), axis.title.x = element_blank(), 
        legend.title=element_blank(),
        axis.text = element_text(size = 10, color = "#000f1c", face = "bold", family = 'Century Gothic'),
        plot.caption = element_text(hjust = 0, size = 13),
        legend.text = element_text(size = 10, color = "#000f1c", face = "bold", family = 'Century Gothic'),
        text = element_text(size = 10, color = "#000f1c", face = "bold", family = 'Century Gothic')) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  labs(title = 'Relation between maternal bargaining index (PCA) and the height for age z-score of the child')

mom_panel %>% 
  filter(!is.na(PC1tot)) %>% 
  ggplot(aes(x = PC1tot, y = decision_mom))+
  geom_smooth(method = lm)

##### Save plots #####

ggsave("Outputs/graphs/Number of decisions against income smooth.jpg",device = "jpeg",plot = inc_dec_smooth, width = 20, height = 10, units = "cm")

ggsave("Outputs/graphs/Number of decisions against test smooth.jpg",device = "jpeg",plot = test_smooth_mom, width = 20, height = 10, units = "cm")

ggsave("Outputs/graphs/Number of decisions against hfa smooth.jpg",device = "jpeg",plot = hfa_smooth_mom, width = 20, height = 10, units = "cm")