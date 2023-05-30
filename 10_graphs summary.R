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
library(readxl)

##### Read data and set fond and colors #####

load('Outputs/Data_tidy_dissertation.RData')

colors_pal <- c('#17406D','#0F6FC6','#009DD9','#176A7B','#0BD0D9',
                '#00D0A8','#5FF3CB','#70AD47','#C8DA92','#CC0066',
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


#Gráficar por año porcentaje de gombres/mujeres que toman la decision solos
dec_summary_g <- decisions %>% 
  mutate(gender = case_when(ls04 == 1 ~ "Male",
                            ls04 == 3  ~ "Female", 
                            TRUE ~ NA_character_)) %>% 
  ungroup() %>% 
  # filtering only spouse and partner
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
  filter(decision_maker == "Own" | decision_maker == "Both") %>% 
  group_by(gender_year, dec, year, gender, decision) %>% 
  summarise(porc = sum(porc)) %>% 
  ggplot(aes(x = reorder(decision, -porc), y = porc, fill = as.factor(gender))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(x = reorder(decision, -porc), y = porc + .1, label = sprintf("%2.1f%%", porc*100), fontface = "bold", family = 'Calibri'), 
            position = position_dodge(width = 1)) + 
  facet_wrap(~ year) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = c('#176A7B', '#0F6FC6')) +
  theme(legend.position ="bottom", 
        legend.title = element_blank(), axis.title.y = element_blank(),
        strip.text.x = element_text(size = 10, color = "#000f1c", face = "bold", family = 'Calibri'),
        axis.text = element_text(size = 10, color = "#000f1c", face = "bold", family = 'Calibri'),
        plot.caption = element_text(hjust = 0, size = 9),
        legend.text = element_text(size = 9, color = "#000f1c", face = "bold", family = 'Calibri'),
        text = element_text(size = 9, color = "#000f1c", face = "bold", family = 'Calibri')) +
  ylab('Percentage of persons who reported some control in a decision') +
  labs(caption = 'Source: MxFLS-1, MxFLS-2, MxFLS-3.') +
  coord_flip()

# Tabla de personas que toman la decisión solos o con la pareja (considerados como un solo valor) por quintil de ingreso
dec_summary_g_table <- decisions %>% 
  mutate(gender = case_when(ls04 == 1 ~ "Male",
                            ls04 == 3  ~ "Female", 
                            TRUE ~ NA_character_)) %>% 
  ungroup() %>% 
  filter(ls05_1 == 1 | ls05_1 == 2) %>% # either head or spouse of hh
  left_join(aux_adul %>% 
              select(pid_link_uni, folio_uni, year, test_score, decision_points, decision_alone, income_c)) %>% 
  select(year, gender, `1`:`12`, income_c) %>% 
  mutate(quintile = ntile(income_c, 5)) %>% 
  drop_na() %>% 
  pivot_longer(names_to = "decisions", values_to = "decision_maker", cols = -c(year, gender, income_c, quintile)) %>% 
  #\ mutate(some = case_when(decision_maker == "Own" | decision_maker == "Both" ~ "some", 
  #                         TRUE ~ "no")) %>% 
  group_by(year, decisions, gender, decision_maker, quintile) %>% 
  summarise(tot = n()) %>% 
  ungroup() %>% 
  group_by(year, decisions, gender, quintile) %>% 
  mutate(gen = sum(tot), porc = tot/gen) %>% 
  mutate(decisions_label = case_when(decisions == 1 ~ "Food eaten in this house",
                                     decisions == 2 ~ "Your clothes", 
                                     decisions == 3 ~ "Your partner's clothes", 
                                     decisions == 4 ~ "Children's clothes", 
                                     decisions == 5 ~ "Children's education", 
                                     decisions == 6 ~ "Children's health",
                                     decisions == 7 ~ "Important household expenditures", 
                                     decisions == 8 ~ "Money to your relatives", 
                                     decisions == 9  ~ "Money to your partner's relatives", 
                                     decisions == 10 ~ "If you should work", 
                                     decisions == 11 ~ "If your partner should work", 
                                     decisions == 12 ~ "Birth-control",
                                     TRUE ~ NA_character_)) %>%  
select(year, decisions, decisions_label, gender, decision_maker, quintile, gen, porc) %>% 
mutate(some_power = ifelse(decision_maker == "Both"| decision_maker == "Own",sum(porc[decision_maker == "Both"| decision_maker == "Own"]), NA))

# Tabla de personas que toman la decisión solos o con la pareja 
dec_summary_g_table_sep <- decisions %>% 
  mutate(gender = case_when(ls04 == 1 ~ "Male",
                            ls04 == 3  ~ "Female", 
                            TRUE ~ NA_character_)) %>% 
  ungroup() %>% 
  filter(ls05_1 == 1 | ls05_1 == 2) %>% # either head or spouse of hh
  select(year, gender, `1`:`12`) %>% 
  drop_na() %>% 
  pivot_longer(names_to = "decisions", values_to = "decision_maker", cols = -c(year, gender)) %>% 
  group_by(year, decisions, gender, decision_maker) %>% 
  summarise(tot = n()) %>% 
  ungroup() %>% 
  group_by(year, decisions, gender) %>% 
  mutate(gen = sum(tot), porc = tot/gen) %>% 
  mutate(decisions_label = case_when(decisions == 1 ~ "Food eaten in this house",
                                     decisions == 2 ~ "Your clothes", 
                                     decisions == 3 ~ "Your partner's clothes", 
                                     decisions == 4 ~ "Children's clothes", 
                                     decisions == 5 ~ "Children's education", 
                                     decisions == 6 ~ "Children's health",
                                     decisions == 7 ~ "Important household expenditures", 
                                     decisions == 8 ~ "Money to your relatives", 
                                     decisions == 9  ~ "Money to your partner's relatives", 
                                     decisions == 10 ~ "If you should work", 
                                     decisions == 11 ~ "If your partner should work", 
                                     decisions == 12 ~ "Birth-control",
                                     TRUE ~ NA_character_)) %>%  
  select(year, decisions, decisions_label, gender, decision_maker, porc,gen) %>% 
  group_by(year, decisions_label, gender) %>% 
  mutate(some_power = ifelse(decision_maker == "Both"| decision_maker == "Own",sum(porc[decision_maker == "Both"| decision_maker == "Own"]), NA))
  
  table_export <- dec_summary_g_table_sep %>% 
  select(year, decisions, decisions_label, gender, decision_maker, porc, gen) %>% 
  pivot_wider(names_from = decision_maker, values_from = c(porc, gen)) %>%
  ungroup() %>% 
  rename(observations = gen_Both) %>% 
  select(-c(decisions, gen_Other, gen_Own, gen_Spouse, `gen_Doesn't know/answer`)) %>% 
  arrange(year, decisions_label) %>% 
  pivot_wider(names_from = year, values_from = c(porc_Both:observations))
  
  write.csv(table_export, "Outputs/old/decision_gender_type_andy.csv")
  
#### descriptive statistics for different decisions 

    
# Gráfica de línea de quien tiene cierto poder de decisión 
# en decisiones monetarias en la casa por sexo
money_graph <- dec_summary_g_table %>% 
  filter(decisions %in% c(7,8,9,10,11)) %>% 
  filter(year == 2009 & (!is.na(some_power))) %>% 
  select(-decision_maker) %>% 
  dplyr::distinct() %>% 
  ggplot(aes(x = quintile, y = some_power, color = gender)) +
  geom_line(size = 1.2) +
  geom_point() +
  facet_grid(~decisions_label) +
  theme(legend.position ="bottom", 
        legend.title = element_blank(),
        strip.text.x = element_text(size = 7, color = "#000f1c", face = "bold", family = 'Century Gothic'),
        axis.text = element_text(size = 8, color = "#000f1c", face = "bold", family = 'Century Gothic'),
        plot.caption = element_text(hjust = 0, size = 8),
        legend.text = element_text(size = 8, color = "#000f1c", face = "bold", family = 'Century Gothic'),
        text = element_text(size = 8, color = "#000f1c", face = "bold", family = 'Century Gothic')) +
  ylab('Percentage of people with some control over decisions') +
  labs(caption = 'Source: MxFLS-3 (wave 2009-2012).', 
       title = "Money-related decisions") +
  scale_color_manual(values = c('#176A7B', '#0F6FC6'))

# gráfica lollipop de diferencia en porcentage de algún poder en la deción 
# por género y quintil
money_quintile_diff <- dec_summary_g_table %>% 
  # filtrar decisiones monetarias
  filter(decisions %in% c(7,8,9,10,11)) %>% 
  # filtrar 2009 y some power
  filter(year == 2009 & (!is.na(some_power))) %>% 
  # quitar variables no usadas
  select(-c(decision_maker, porc, gen)) %>% 
  dplyr::distinct() %>% 
  # cambiar nombres para labels
  mutate(gender = ifelse(gender == "Female", "woman", "man")) %>% 
  # reshape para tener start y end point
  pivot_wider(names_from = gender, values_from = some_power) %>% 
  # crear gap y posotion gap para labels de las gráficas
  mutate(gap = (man - woman)*100,
         gap_pos = woman +(gap/200)) %>% 
  # empezar gráfica
  ggplot() +
  # segmento para lollipop que toma como inicio y fin
  geom_segment(mapping = aes(x = quintile, xend=quintile, y=man, yend = woman), color = "dark grey") +
  # definir los puntos para los hombres
  geom_point(mapping = aes(x=quintile, y=man, color = "man"),  size=2, alpha = .8) +
  # poner labels con la diferencia en porcentajes hombres y mujeres
  geom_text(aes(x=quintile, y=gap_pos, label=comma(gap,accuracy = .1)),
            family = 'Century Gothic', 
            nudge_x = .2,
            size = 3) +
  # ponerr puntos para las mujeres
  geom_point(mapping = aes(x=quintile, y=woman, color = "woman"),  size=2, alpha = .8) +
  # crear paneles para cada decisión
  facet_wrap(~decisions_label)+
  #poner títulos
  labs(title = "Gap in percentage of some power in money related decisions, by income quintile",
       caption = "Source: MxFLS-3 (wave 2009-2012). 
                  Gap is computed as the difference between male's and female's % in percentage points.", 
       x = "Quintile (1 = lowest income quintile)") +
  # cambiar formatos y temas para la gráfica
  theme(legend.position="bottom", 
      #  axis.title.y = element_blank(),
       # axis.title.x = element_blank(), 
        legend.title=element_blank(),panel.background = element_blank(),
        axis.text.x = element_text(angle = 90),
        axis.text = element_text(size = 7, color = "#000f1c", face = "bold", family = 'Century Gothic'),
        legend.text = element_text(size = 7, color = "#000f1c", face = "bold", family = 'Century Gothic'),
        panel.grid.major = element_line(colour = "#D3D3D3"),
        text = element_text(size = 7, color = "#000f1c", face = "bold", family = 'Century Gothic')) +
  # colores hombres y mujeres
  scale_color_manual(values = c('#176A7B', '#0F6FC6')) +
  # escala de y como porcentaje
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  # voltear grráfica
  coord_flip()

ggsave(money_quintile_diff, file = "Outputs/graphs/money_quintile_diff.jpg", width = 19, height = 11, units = 'cm')

dec_summary_g_table %>% 
  filter(decisions %in% c(7,8,9,10,11)) %>% 
  filter(year == 2009 & (!is.na(some_power))) %>% 
  ggplot(aes(x = quintile, y = some_power, color = gender)) +
  geom_line(size = 1.2) +
  facet_grid(~decisions_label+decision_maker) +
  theme(legend.position ="bottom", 
        legend.title = element_blank(),
        strip.text.x = element_text(size = 7, color = "#000f1c", face = "bold", family = 'Century Gothic'),
        axis.text = element_text(size = 8, color = "#000f1c", face = "bold", family = 'Century Gothic'),
        plot.caption = element_text(hjust = 0, size = 8),
        legend.text = element_text(size = 8, color = "#000f1c", face = "bold", family = 'Century Gothic'),
        text = element_text(size = 8, color = "#000f1c", face = "bold", family = 'Century Gothic')) +
  ylab('Percentage of people with some control over decisions') +
  labs(caption = 'Source: MxFLS-3 (wave 2009-2012).', 
       title = "Money-related decisions") +
  scale_color_manual(values = c('#176A7B', '#0F6FC6')) 

ggsave(money_graph, file = "Outputs/graphs/money_graph.jpg", width = 24, height = 11, units = 'cm')
  
money_graph_sep <- dec_summary_g_table_sep %>% 
  #filtrar decisiones que tienen que ver con dinero
    filter(decisions %in% c(7,8,9,10,11)) %>% 
  # solo personas que reportan tomar la decision solos o con su pareja
    filter(year == 2009 & (decision_maker == "Own" | decision_maker == "Both")) %>% 
    ggplot(aes(x = gender, y = porc, fill = decision_maker)) + 
    geom_bar(stat = "identity") + # agregar capa de barra
    geom_text(aes(x = gender, y = some_power, label = scales::percent(some_power, accuracy = .1)),
              hjust = +.9,
             # angle = 270,
           # position = position_dodge(width = 1), 
            family = 'Century Gothic', 
            fontface = "bold", 
           size = 3) + # label en línea con barra
    facet_grid(~decisions_label, labeller = labeller(decisions_label = label_wrap_gen(15))) + # hacer paneles de la decisión
  # Cambiar aspecto físico de la gráfica 
   theme(legend.position ="bottom", 
          legend.title = element_blank(),
          strip.text.x = element_text(size = 7, color = "#000f1c", face = "bold", family = 'Century Gothic'),
          axis.text = element_text(size = 8, color = "#000f1c", face = "bold", family = 'Century Gothic'),
          plot.caption = element_text(hjust = 0, size = 8),
          legend.text = element_text(size = 8, color = "#000f1c", face = "bold", family = 'Century Gothic'),
          text = element_text(size = 8, color = "#000f1c", face = "bold", family = 'Century Gothic')) +
    ylab('Percentage of people with some control over decisions') +
    labs(caption = 'Source: MxFLS-3 (wave 2009-2012).
Label refers to the percentage of people who have certain control over the decision.', 
         title = "Money-related decisions") +
  scale_fill_manual(values = c('#176A7B', '#0F6FC6')) +
  scale_y_percent() +
  coord_flip()

ggsave(money_graph_sep, file = "Outputs/graphs/money_graph_sep.jpg", width = 22, height = 12, units = 'cm')

# gráfica de linea de cierto poder en las decisiones relacionadas con niños por quintil
children_graph <- dec_summary_g_table %>% 
  filter(decisions %in% c(1,4,5,6,12)) %>% 
  filter(year == 2009 & some == "some") %>% 
  ggplot(aes(x = quintile, y = porc, color = gender)) +
  geom_line(size = 1) +
  geom_point() +
  facet_grid(~decisions_label) +
  theme(legend.position ="bottom", 
        legend.title = element_blank(),
        strip.text.x = element_text(size = 8, color = "#000f1c", face = "bold", family = 'Century Gothic'),
        axis.text = element_text(size = 8, color = "#000f1c", face = "bold", family = 'Century Gothic'),
        plot.caption = element_text(hjust = 0, size = 8),
        legend.text = element_text(size = 8, color = "#000f1c", face = "bold", family = 'Century Gothic'),
        text = element_text(size = 8, color = "#000f1c", face = "bold", family = 'Century Gothic')) +
  ylab('Percentage of people with some control over decisions') +
  labs(caption = 'Source: MxFLS-3 (wave 2009-2012).', 
       title = "Household/Children-related decisions") +
  scale_color_manual(values = c('#176A7B', '#0F6FC6')) 
  
ggsave(children_graph, file = "Outputs/graphs/children_graph.jpg", width = 17, height = 10, units = 'cm')

# gráfica lollipop de diferencia en porcentage de algún poder en la decisión 
# por género y quintil - children
children_quintile_diff <- dec_summary_g_table %>% 
  # filtrar decisiones de los niños
  filter(decisions %in% c(1,4,5,6,12)) %>%  
  # filtrar 2009 y some power
  filter(year == 2009 & (!is.na(some_power))) %>% 
  # quitar variables no usadas
  select(-c(decision_maker, porc, gen)) %>% 
  dplyr::distinct() %>% 
  # cambiar nombres para labels
  mutate(gender = ifelse(gender == "Female", "woman", "man")) %>% 
  # reshape para tener start y end point
  pivot_wider(names_from = gender, values_from = some_power) %>% 
  # crear gap y posotion gap para labels de las gráficas
  mutate(gap = (man - woman)*100,
         gap_pos = woman +(gap/200)) %>% 
  # empezar gráfica
  ggplot() +
  # segmento para lollipop que toma como inicio y fin
  geom_segment(mapping = aes(x = quintile, xend=quintile, y=man, yend = woman), color = "dark grey") +
  # definir los puntos para los hombres
  geom_point(mapping = aes(x=quintile, y=man, color = "man"),  size=2, alpha = .8) +
  # poner labels con la diferencia en porcentajes hombres y mujeres
  geom_text(aes(x=quintile, y=gap_pos, label=comma(gap,accuracy = .1)),
            family = 'Century Gothic', 
            nudge_x = .2,
            size = 2) +
  # ponerr puntos para las mujeres
  geom_point(mapping = aes(x=quintile, y=woman, color = "woman"),  size=2, alpha = .8) +
  # crear paneles para cada decisión
  facet_wrap(~decisions_label)+
  #poner títulos
  labs(title = "Gap in percentage of decision power in children related decisions by income quintile",
       caption = "Source: MxFLS-3 (wave 2009-2012). 
                  Gap is computed as the difference between male's and female's % in percentage points.",
       x = "Quintile (1 = lowest income quintile)") +
  # cambiar formatos y temas para la gráfica
  theme(legend.position="bottom", 
        legend.title=element_blank(),panel.background = element_blank(),
        axis.text.x = element_text(angle = 90),
        axis.text = element_text(size = 6, color = "#000f1c", face = "bold", family = 'Century Gothic'),
        legend.text = element_text(size = 6, color = "#000f1c", face = "bold", family = 'Century Gothic'),
        panel.grid.major = element_line(colour = "#D3D3D3"),
        text = element_text(size = 6, color = "#000f1c", face = "bold", family = 'Century Gothic')) +
  # colores hombres y mujeres
  scale_color_manual(values = c('#176A7B', '#0F6FC6')) +
  # escala de y como porcentaje
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  # voltear gráfica
  coord_flip()

ggsave(children_quintile_diff, file = "Outputs/graphs/children_quintile_diff.jpg", width = 20, height = 12, units = 'cm')

# gráfica de barra, decisiones de niños por género y por quién toma la decisión
children_graph_sep <- dec_summary_g_table_sep %>% 
  # filtrar decisiones que tienen que ver con niños
  filter(decisions %in% c(1,4,5,6, 12)) %>%  
  # último año y solo aquellos que reportan Own o Both
  filter(year == 2009 & (decision_maker == "Own" | decision_maker == "Both")) %>%
  # gráfica x = género y y es el porcentaje de personas, se rellena por quien toma la decisión
  ggplot(aes(x = gender, y = porc, fill = decision_maker)) +
  geom_bar(stat = "identity") + # agregar capa de barra
  geom_text(aes(x = gender, y = some_power, label = scales::percent(some_power, accuracy = .1)),
            hjust = +.9,
            # angle = 270,
            # position = position_dodge(width = 1), 
            family = 'Century Gothic', 
            fontface = "bold", 
            size = 3) + # label en línea con barra
  facet_grid(~decisions_label, labeller = labeller(decisions_label = label_wrap_gen(15))) + # hacer paneles de la decisión
  # Cambiar aspecto físico de la gráfica 
  theme(legend.position ="bottom", 
        legend.title = element_blank(),
        strip.text.x = element_text(size = 7, color = "#000f1c", face = "bold", family = 'Century Gothic'),
        axis.text = element_text(size = 8, color = "#000f1c", face = "bold", family = 'Century Gothic'),
        plot.caption = element_text(hjust = 0, size = 8),
        legend.text = element_text(size = 8, color = "#000f1c", face = "bold", family = 'Century Gothic'),
        text = element_text(size = 8, color = "#000f1c", face = "bold", family = 'Century Gothic')) +
  ylab('Percentage of people with some control over decisions') +
  labs(caption = 'Source: MxFLS-3 (wave 2009-2012).
Label refers to the percentage of people who have certain control over the decision.', 
       title = "Children-related decisions") +
  scale_fill_manual(values = c('#176A7B', '#0F6FC6')) +
  scale_y_percent() +
  coord_flip()

ggsave(children_graph_sep, file = "Outputs/graphs/children_graph_sep.jpg", width = 21, height = 12, units = 'cm')

##### 2) Determinants of decision-making power #####
  
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
 
 ggsave("Outputs/graphs/Number of decisions by gender and position.jpg",device = "jpeg",plot = bar_dec, width = 20, height = 10, units = "cm")
 
 ggsave("Outputs/graphs/Number of decisions against income box.jpg",device = "jpeg",plot = inc_dec_box, width = 20, height = 15, units = "cm")
 
 ggsave("Outputs/graphs/Decision by gender and role.jpg",device = "jpeg",plot = dec_graph_muj, width = 20, height = 10, units = "cm")
 
 ggsave("Outputs/graphs/Number of decisions against income point.jpg",device = "jpeg",plot = dec_income_mean, width = 19, height = 10, units = "cm")
 
 ggsave("Outputs/graphs/Test score vs. age.jpg",device = "jpeg",plot = score_age, width = 19, height = 10, units = "cm")
 
 ggsave("Outputs/graphs/Percentage of decisions some.jpg",device = "jpeg",plot = dec_summary_g, width = 26, height = 18, units = "cm")
 