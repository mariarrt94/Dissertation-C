##### Dissertation Maria Reyes Retana - 
# This code constructs the shift share instrument

##### Libraries ####

library(zoo)
library(tidyverse)
options(scipen = 999)
detach("package:plm", unload=TRUE)

##### Read data #####

sec_1999 <- read.csv("Inputs/SAIC__1999_sec.csv", skip = 4) %>% 
  select(-c(X)) %>% filter(!is.na(POPT.Personal.ocupado.total)) %>% 
  mutate(year_cen = 1999) %>% 
  rename(H001A.Personal.ocupado.total = POPT.Personal.ocupado.total, 
         H001C.Personal.ocupado.total..mujeres = POPM.Personal.ocupado.total.mujeres,
         H001B.Personal.ocupado.total..hombres = POPH.Personal.ocupado.total.hombres)

man_1999 <- read.csv("Inputs/SAIC__1999_man.csv", skip = 4) %>% 
  select(-c(X)) %>% filter(!is.na(POPT.Personal.ocupado.total))%>% 
  mutate(year_cen = 1999) %>% 
  rename(H001A.Personal.ocupado.total = POPT.Personal.ocupado.total, 
         H001C.Personal.ocupado.total..mujeres = POPM.Personal.ocupado.total.mujeres,
         H001B.Personal.ocupado.total..hombres = POPH.Personal.ocupado.total.hombres)

sec_2004 <- read.csv("Inputs/SAIC__2004_sec.csv", skip = 4) %>% 
  select(-c("X", "UE.UNIDADES.ECON?MICAS")) %>% filter(!is.na(H001A.Personal.ocupado.total))%>% 
  mutate(year_cen = 2004)

man_2004 <- read.csv("Inputs/SAIC__2004_man.csv", skip = 4) %>% 
  select(-c("X", "UE.UNIDADES.ECON?MICAS")) %>% filter(!is.na(H001A.Personal.ocupado.total))%>% 
  mutate(year_cen = 2004)

sec_2009 <- read.csv("Inputs/SAIC__2009_sec.csv", skip = 4) %>% 
  select(-c(X)) %>% filter(!is.na(H001A.Personal.ocupado.total)) %>% 
  mutate(year_cen = 2009)

man_2009 <- read.csv("Inputs/SAIC__2009_man.csv", skip = 4) %>% 
  select(-c(X)) %>% filter(!is.na(H001A.Personal.ocupado.total)) %>% 
  mutate(year_cen = 2009)

sec_tot <- read.csv("Inputs/SAIC2004-2013_sec.csv", skip = 4) %>% 
  select(-c(X)) %>% filter(!is.na(H001A.Personal.ocupado.total)) %>% 
  rename(year_cen = A?o.Censal)

man_tot <- read.csv("Inputs/SAIC2004-2013_man.csv", skip = 4) %>% 
  select(-c(X)) %>% filter(!is.na(H001A.Personal.ocupado.total)) %>% 
  rename(year_cen = A?o.Censal)

##### Tot tidy year by year #####

sec_sep <- sec_tot %>% 
  filter(year_cen == 2014) %>% 
  rbind(sec_2009, sec_2004, sec_1999)

##### Construct shift share instruments sector #####

# first we need the share of female work for each sector by municipality 

regexp <- "[[:digit:]]+"

sec_tot_tidy <- sec_sep %>% 
  rename(total = H001A.Personal.ocupado.total , female = H001C.Personal.ocupado.total..mujeres, 
         male = H001B.Personal.ocupado.total..hombres, sec = Actividad.Econ?mica) %>% 
  mutate(ent = str_sub(Entidad, 1L, 2L), mpio = str_sub(Municipio, 1L, 3L), year_cen = as.numeric(year_cen)) %>% 
  gather("gender", "workers", total:female) %>% 
  mutate(sec1 = str_extract(sec, regexp),
         sec = case_when(is.na(sec1) ~ "SC", 
                         TRUE ~ sec1))

sec_tot_year <- sec_tot_tidy %>% 
  ungroup() %>% 
  group_by(year_cen, gender) %>% 
  summarise(workers =sum(workers))

sec_share <- sec_tot_tidy %>%
  mutate(mpio_ent = paste(ent, mpio)) %>% 
  filter(year_cen == 1999) %>% 
  group_by(year_cen, mpio_ent, gender) %>% 
  mutate(tot_mpio = sum(workers),
         share = workers/tot_mpio) %>% 
  mutate(year_base = 2000) %>% 
  ungroup() %>% 
  select(-c(year_cen)) %>% 
  select(Entidad, Municipio, sec, mpio_ent, year_base, gender, share)

proof <- sec_share %>% 
  group_by(year_base, mpio_ent, gender) %>% 
  summarise(share = sum(share))

# Now we need the national growth rate in each sector, without considering the municipality to avoid biases

sec_grow <- sec_tot_tidy %>% 
  mutate(mpio_ent = paste(ent, mpio)) %>% 
  group_by(year_cen, gender, sec) %>%
  mutate(tot = sum(workers), 
         tot_without = map_dbl(mpio_ent, ~sum(workers[mpio_ent!=.x]))) %>%  # this eliminates the consideration of each municipality
  ungroup() %>% 
  arrange(sec, mpio_ent, gender, year_cen) %>% 
  group_by(gender, sec, mpio_ent) %>% 
  mutate(diff_emp = (tot_without - lag(tot_without))/lag(tot_without),
         per = workers/tot) %>% 
  filter(year_cen != 1999)

# now we add share and shocks 

sec_shock <- sec_grow %>%
  mutate(year_base = 2000) %>% 
  left_join(sec_share) %>% 
  mutate(shock = share*diff_emp)
  
shock_base <- sec_shock %>% 
  ungroup() %>% 
  group_by(year_cen, ent, mpio, gender) %>% 
  summarise(bi = sum(shock, na.rm = TRUE)) %>% 
  spread("gender", "bi") %>% 
  mutate(year = case_when(year_cen == 2004 ~ "2002", 
                          year_cen == 2009 ~ "2005", 
                          year_cen == 2014 ~ "2009"))

write.csv(shock_base, "Inputs/shocks_base_sec.csv")
  

