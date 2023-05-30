##### Dissertation Maria Reyes Retana 
# This code contains the code to do the graphs for histograms

##### Libraries #####

library(tidyverse)
library(readxl)
library(FactoMineR)
library(factoextra)
library(psych)
library(corrplot)
library(extrafont)
library(tidyr)
library(ggplot2)

options(scipen=999)

##### Import data bases from Rdta #####

##### Read data #####

source('Complete information for analysis.R')

mom_base_filtered <- mom_base %>% 
  mutate(income_c_mom = ifelse(worked_12_mom ==0 & is.na(income_c_mom), 0, income_c_mom),
         log_income_mom = case_when(income_c_mom > 0 ~ log(income_c_mom), 
                                    TRUE ~ income_c_mom)) %>% 
  #filter(married_mom ==1) %>% 
  filter(!is.na(PC1money_mom))
  #filter(age_mom >=18 & age_mom<=64)


# información de repetición

# 1. with PCA money decisions
repetition_money <- mom_base %>%
  select(pid_link_mom, year, PC1money_mom) %>%
  arrange(pid_link_mom) %>%
  pivot_wider(names_from = year, values_from = c(PC1money_mom)) %>%
  filter(!((is.na(`2002`) & is.na(`2005`)) | (is.na(`2002`) & is.na(`2009`) | (is.na(`2009`) & is.na(`2005`))))) %>%
  mutate(delta_2009_2005 = `2009`/`2005`,
         delta_2005_2002 = `2005`/`2002`,
         delta_2009_2002 = `2009`/`2002`) %>%
  pivot_longer(values_to = "ratio", names_to = "delta", cols = c(delta_2005_2002, delta_2009_2005, delta_2009_2002)) %>%
  group_by(delta) %>%
  mutate(prop_at_one = sum(ratio==1, na.rm = TRUE)/n())

prop_at_one <- repetition_money %>% 
  select(delta, prop_at_one) %>% 
  distinct()

# Create the histogram
PCA_money <- ggplot(repetition_money, aes(x = ratio, fill = delta)) +
  geom_histogram(binwidth = 0.1, color = "black", alpha = 0.7) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1") +
  labs(
    title = "Histogram of Value Ratios - Old Index",
    x = "Value Ratio",
    y = "Frequency"
  ) +
  geom_vline(aes(xintercept = 1), color = "blue", linetype = "dashed", size = .3) +
  annotate("text", x = 1.05, y = Inf, label = "Ratio = 1", hjust = 0, vjust = 1, color = "blue", size = 4, fontface = "bold") +
  facet_wrap(~ delta, scales = "free_y") +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(size = 12, color = "black", face = "bold"),
    axis.text = element_text(size = 12, color = "black", face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    legend.position = "none"
  )+
  geom_text(data = repetition_money %>% group_by(delta) %>% summarise(prop_at_one = mean(prop_at_one)),
            aes(x = Inf, y = Inf, label = sprintf("Share at one: %.2f", prop_at_one*100)),
            hjust = 1.1, vjust = 4, size = 3.4, color = "darkred", fontface = "bold")


ggsave("Outputs/Graphs/Histogram_ratio.jpg" ,device = "jpeg",
       plot = PCA_money, width = 20, height = 10, units = "cm")

###### Información de repetición para nuevo índice -----

# información de repetición

# 2. with PCA money decisions new index
repetition_money_new <- mom_base %>%
  select(pid_link_mom, year, PC1financial_new_mom) %>%
  arrange(pid_link_mom) %>%
  pivot_wider(names_from = year, values_from = c(PC1financial_new_mom)) %>%
  filter(!((is.na(`2002`) & is.na(`2005`)) | (is.na(`2002`) & is.na(`2009`) | (is.na(`2009`) & is.na(`2005`))))) %>%
  mutate(delta_2009_2005 = `2009`/`2005`,
         delta_2005_2002 = `2005`/`2002`,
         delta_2009_2002 = `2009`/`2002`) %>%
  pivot_longer(values_to = "ratio", names_to = "delta", cols = c(delta_2005_2002, delta_2009_2005, delta_2009_2002)) %>%
  group_by(delta) %>%
  mutate(prop_at_one = sum(ratio==1, na.rm = TRUE)/n())

prop_at_one_new <- repetition_money_new %>% 
  select(delta, prop_at_one) %>% 
  distinct()

# Create the histogram
PCA_money_new <- ggplot(repetition_money_new, aes(x = ratio, fill = delta)) +
  geom_histogram(binwidth = 0.1, color = "black", alpha = 0.7) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1") +
  labs(
    title = "Histogram of Value Ratios - New Index",
    x = "Value Ratio",
    y = "Frequency"
  ) +
  geom_vline(aes(xintercept = 1), color = "blue", linetype = "dashed", size = .3) +
  annotate("text", x = 1.05, y = Inf, label = "Ratio = 1", hjust = 1.1, vjust = 1, color = "blue", size = 4, fontface = "bold") +
  facet_wrap(~ delta, scales = "free_y") +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(size = 12, color = "black", face = "bold"),
    axis.text = element_text(size = 12, color = "black", face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    legend.position = "none"
  )+
  geom_text(data = repetition_money_new %>% 
              group_by(delta) %>% 
              summarise(prop_at_one = mean(prop_at_one)),
            aes(x = Inf, y = Inf, label = sprintf("Share at one: %.2f", prop_at_one*100)),
            hjust = 1.7, vjust = 4, size = 3.4, color = "darkred", fontface = "bold")


ggsave("Outputs/Graphs/Histogram_ratio_new.jpg" ,device = "jpeg",
       plot = PCA_money_new, width = 22, height = 10, units = "cm")





