##### Dissertation Maria Reyes Retana - 
# This code does the matching of municipalities

##### Libraries #####

library(readxl)
library(dplyr)
library(MatchIt)
library(optmatch)
library(glm2)

##### Read data #####

edo_con <- read_xls("Inputs/Estados_y_Municipios1.xls", skip = 3) %>% 
  filter(!is.na(`Clave de la entidad federativa`)) %>% 
  select(-c("...4", "...8")) %>% 
  rename(ent = `Clave de la entidad federativa`)

muni_con <- read_xls("Inputs/Estados_y_Municipios1.xls", skip = 3, sheet = "Municipios") %>% 
  filter(!is.na(`Clave de la entidad federativa`)) %>% 
  select(-c("...6", "...10", `Grado de rezago social`, `Lugar que ocupa en el contexto nacional`)) %>% 
  rename(ent = `Clave de la entidad federativa`, mpio = `Clave del municipio`)
  
edo_base <- read_xlsx("Inputs/2016-ICE_2016-Base_datos.xlsx", sheet = "Ind (05)", skip = 5) %>% 
  rename(ent = Unidades,  `Entidad federativa` = ...2 ) %>% 
  filter(!is.na(`Entidad federativa`))

##### Merge data #####

edo_tidy <-  edo_base %>% 
  rename(fem_work = `Mujeres económicamente activas como porcentaje de la PEA`, dif_wage = `Diferencia porcentual de los ingresos entre hombres y mujeres`,
         year_ed = `Años promedio`, same_marr = `Índice (0=No se considera el matrimonio igualitario, 1=Sí se considera el matrimonio igualitario)`, 
         pib_pc = `Pesos por persona`) %>% 
  select(ent, fem_work, dif_wage, year_ed, same_marr, pib_pc) 

muni_tidy <- muni_con %>% 
  left_join(edo_tidy, by = c("ent")) %>% 
  mutate(ent_uni = ent == "09" | ent == "15" |ent == "31",
         same_marr = same_marr == 1 ) %>% 
  mutate(fem_work = as.numeric(fem_work), dif_wage = as.numeric(dif_wage), year_ed = as.numeric(year_ed))

edo_psm <- edo_con %>% 
  left_join(edo_tidy, by = c("ent")) %>% 
  mutate(ent_uni = ent == "09" | ent == "15" |ent == "31",
         same_marr = same_marr == 1 ) %>% 
  mutate(fem_work = as.numeric(fem_work), dif_wage = as.numeric(dif_wage), year_ed = as.numeric(year_ed))

##### Logit and PS #####

muni_ps <- glm2(ent_uni ~ `Población total` + `Pobreza alimentaria  (%)` +
               `% de población de 15 años o más analfabeta` + `Promedio de ocupantes por cuarto` +
                fem_work + dif_wage + year_ed, family = binomial(), data = muni_tidy, maxit = 100)

summary(muni_ps)

ps_muni <- data.frame(pr_score = predict(muni_ps, type = "response"),
                       ent_uni = muni_ps$model$ent_uni)

head(ps_muni)

##### Matching #####

mod_match_mu <- matchit(ent_uni ~ `Población total` + `Pobreza alimentaria  (%)` +
                       `% de población de 15 años o más analfabeta` + `Promedio de ocupantes por cuarto` +  
                        fem_work + dif_wage + year_ed + factor(same_marr) +`Índice de rezago social` + pib_pc, data = muni_tidy, method = "optimal", distance = "glm" , link = "probit", maxit = 100)

dta_m_mu <- match.data(mod_match_mu)

mod_match_edo <- matchit(ent_uni ~ `Población total` + `Pobreza alimentaria  (%)` +
                       `% de población de 15 años o más analfabeta` + `Promedio de ocupantes por cuarto` +  
                       fem_work + dif_wage + year_ed + factor(same_marr) +`Índice de rezago social`+ pib_pc, data = edo_psm, method = "nearest", distance = "glm" , link = "probit", maxit = 100)

dta_m_edo <- match.data(mod_match_edo)


contrl_muni <- dta_m %>% 
  select(ent) %>% 
  unique()

plot(mod_match)

mod_cov <- c('Población total', 'Pobreza alimentaria  (%)', '% de población de 15 años o más analfabeta',
             'Promedio de ocupantes por cuarto', 'fem_work', 'dif_wage', 'year_ed')

mean_match <- dta_m %>%
  group_by(ent_uni) %>% 
  select(one_of(mod_cov)) %>% 
  summarise_all(funs(mean(.,na.rm = TRUE)))

summary(mod_match)
