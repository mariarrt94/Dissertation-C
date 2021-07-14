# Generate base to transform prices to prices of 2012

library(readxl)
library(tidyverse)

#### Import data #####

INPC <- read_xlsx("Inputs/BIE_BIE20210628095854.xlsx", sheet = "Pagina 1")

##### Generate Inflation #####
# We need incomes to be comparable between years, I am making the reference year the last one i.e 2012

INF <- INPC %>% 
  rename(date = Periodo, index = `Indice general`) %>% 
  filter(!is.na(index)) %>% 
  mutate(date = as.character(date),
         month = as.numeric(str_sub(date,6,7)),
         year = as.numeric(str_sub(date,1,4))) %>% 
  group_by(year) %>% 
  summarise(index = mean(index, na.rm = TRUE)) %>% 
  mutate(CPI_REF = index[year == 2012],
         fact_infl = CPI_REF/index) %>% 
  select(year, fact_infl)