##### Dissertation Maria Reyes Retana - Code for creating databases 

##### Libraries #####

library(tidyverse)
library(readxl)
library(lubridate)
rm(list = ls())
options(scipen = 999)

##### Load reading code #####

source('Data Reading.R')

##### General data base: generate unified folio and pid_link: individual level #####

# Basic information: it is necessary to generate a unified identifier for every individual and household across all databases
# we will generate that identifier in this section for the three waves

basic_ind02 <- c_ls_02 %>%
  mutate(year = 2002,
         folio_aux = str_sub(folio, 1L, -3L), 
         term_h = "00",
         round_i = "A",
         round_h = "A",
         length = str_length(folio_aux),
         folio_2 = case_when(length == 2 ~ paste("0000", folio_aux, sep = ""),
                                length == 3 ~ paste("000", folio_aux, sep = ""),
                                length == 4 ~ paste("00", folio_aux, sep = ""),
                                length == 5 ~ paste("0", folio_aux, sep = ""),
                                TRUE ~ folio_aux),
         ls_aux = as.character(ls),
         ls = case_when(ls<10 ~ paste("0", ls_aux, sep = ""),
                              TRUE ~ ls_aux),
         folio = paste(folio_2, term_h, sep = ""),
         pid_link = paste(folio_2, term_h, ls, sep = ""), 
         pid_link_aux = pid_link) %>% 
  select(year, folio, folio_2, term_h, round_i, round_h, ls, ls00, pid_link, pid_link_aux, ls02_2, ls03_21, ls03_22, ls04, ls05_1, ls06, ls07, ls10, ls11, ls12, ls13_1, ls13_2, ls14, ls15_1, ls16)

folio_i_2002 <- basic_ind02 %>% 
  select(pid_link_aux) %>% distinct()

folio_h_2002 <- basic_ind02 %>% 
  select(folio) %>% distinct() %>% 
  mutate(house_term = str_sub(folio, 1L, 6L))

basic_ind05 <- c_ls_05 %>% 
  mutate(year = 2005, folio_2 = str_sub(folio, 1L, -3L), term_h = str_sub(folio, -2L), 
         pid_link_aux = pid_link,
         term_i5 =  str_sub(pid_link, 7L,8L),
         folio_ih = paste(folio_2, term_i5, sep = ""),
         round_h = case_when(folio %in% folio_h_2002$folio ~ "A", 
                           TRUE ~ "B"),
         round_i = case_when(folio_ih %in% folio_h_2002$folio ~ "A", 
                             TRUE ~ "B")) %>% 
  select(year, folio, folio_2, term_h, round_i, round_h, ls, ls00, pid_link, pid_link_aux, ls02_2, ls03_21, ls03_22, ls04, ls05_1, ls06, ls07, ls10, ls11, ls12, ls13_1, ls13_2, ls14, ls15_1, ls16) %>% 
  rbind(basic_ind02) %>% 
  arrange(folio_2, ls, ls00, year)

folio_i_2005 <- basic_ind05 %>% 
  select(pid_link_aux, round_i) %>% 
  filter(round_i == "B") %>% 
  distinct()

folio_h_2005 <- basic_ind05 %>% 
  select(folio, round_h) %>%
  filter(round_h == "B") %>% 
  distinct()

basic_ind09 <- c_ls_09 %>% 
  mutate(year = 2009, ls_aux = str_sub(pid_link, -2L), term_h = str_sub(folio, -2L), term_i = str_sub(pid_link, 9L,10L), 
         folio_2 = paste(str_sub(folio, 1L, 6L), term_h, sep = ""), 
         pid_link_aux = paste(str_sub(pid_link, 1L,6L), str_sub(pid_link, -4L), sep = ""),
         round = str_sub(folio, 7L, 7L), folio_ih = paste(str_sub(folio, 1L, 6L),term_i, sep = ""),
         round_h = case_when(folio_2 %in% folio_h_2002$folio ~ "A", 
                             folio_2 %in% folio_h_2005$folio ~ "B",
                             TRUE ~ "C"),
         round_i = case_when(folio_ih %in% folio_h_2002$folio ~ "A", 
                             folio_ih  %in% folio_h_2005$folio ~ "B",
                             TRUE ~ "C")) %>% 
  select(year, folio, folio_2, term_h, round_i, round_h, ls, ls00, pid_link, pid_link_aux, ls02_2, ls03_21, ls03_22, ls04, ls05_1, ls06, ls07, ls10, ls11, ls12, ls13_1, ls13_2, ls14, ls15_1, ls16) %>% 
  rbind(basic_ind05) %>% 
  mutate(pid_link_uni = paste(str_sub(pid_link_aux, 1L, 6L), round_i, str_sub(pid_link_aux, -4L), sep = ""), 
         pid_link_uni = case_when(pid_link_uni == "B" ~ paste(folio_2, round_i, term_h, ls, sep = ""),
                                    TRUE ~ pid_link_uni),
         folio_uni = paste(str_sub(folio, 1L, 6L), round_h, str_sub(folio, -2L), sep = "")) %>% 
  select(year, folio, ls, pid_link, pid_link_uni, folio_uni, 
         ls02_2, ls03_21, ls03_22, ls04, ls05_1, ls06, ls07, ls10, ls11, ls12, ls13_1, ls13_2, ls14, ls15_1, ls16) %>% 
  mutate(pid_link_mom = case_when(ls07<10 ~ paste(folio_uni, "0", ls07, sep = ""), 
                                  ls07>=10  & ls07<50 ~ paste(folio_uni, ls07, sep = ""), 
                                  ls07 == 51 ~ "does not live in household", 
                                  ls07 == 53 ~ "deceased", 
                                  TRUE ~ NA_character_), 
         pid_link_dad = case_when(ls06<10 ~ paste(folio_uni, "0", ls06, sep = ""), 
                                  ls06>=10  & ls06<50 ~ paste(folio_uni, ls06, sep = ""), 
                                  ls06 == 51 ~ "does not live in household", 
                                  ls06 == 53 ~ "deceased", 
                                  TRUE ~ NA_character_))

##### State: Household level #####

base_portad_02 <- c_portad_02 %>% 
  mutate(folio_aux = str_sub(folio, 1L, -3L),
         length = str_length(folio_aux),
         folio_2 = case_when(length == 2 ~ paste("0000", folio_aux, sep = ""),
                             length == 3 ~ paste("000", folio_aux, sep = ""),
                             length == 4 ~ paste("00", folio_aux, sep = ""),
                             length == 5 ~ paste("0", folio_aux, sep = ""),
                             TRUE ~ folio_aux),
         folio = paste(folio_2, "00", sep = ""),
         ls_aux = as.character(ls),
         ls = case_when(ls<10 ~ paste("0", ls_aux, sep = ""),
                        TRUE ~ ls_aux),
         pid_link = paste(folio_2, "00", ls, sep = ""),
         year = 2002) %>% 
  rename(ent = edo) %>% 
  select(year, folio, ent, mpio, loc)

base_portad_05 <- c_portad_05 %>% 
  mutate(year = 2005) %>% 
  select(year, folio, ent, mpio, loc)

base_portad_09 <- c_portad_09 %>% 
  mutate(year = 2009) %>% 
  select(year, folio, ent, mpio, loc)

base_portad <- base_portad_02 %>% 
  rbind(base_portad_05, base_portad_09)

##### Date of interview: household level #####

base_con_02 <- c_conpor_02 %>% 
  mutate(folio_aux = str_sub(folio, 1L, -3L),
         length = str_length(folio_aux),
         folio_2 = case_when(length == 2 ~ paste("0000", folio_aux, sep = ""),
                             length == 3 ~ paste("000", folio_aux, sep = ""),
                             length == 4 ~ paste("00", folio_aux, sep = ""),
                             length == 5 ~ paste("0", folio_aux, sep = ""),
                             TRUE ~ folio_aux),
         folio = paste(folio_2, "00", sep = ""),
         year = 2002, 
         anio = 2002, 
         mes = 12) %>% 
  select(year, folio, anio, mes) %>% 
  distinct()

base_con_05 <- c_conpor_05 %>% 
  mutate(year = 2005) %>% 
  select(year, folio, anio, mes) %>% 
  distinct()

base_con_09 <- c_conpor_09 %>% 
  mutate(year = 2009) %>% 
  select(year, folio, anio, mes) %>% 
  distinct()

base_con <- base_con_02 %>% 
  rbind(base_con_05, base_con_09)

##### Create base with unified folio, ls, pid_link #####

# Here we will also use the divorce law dates in Mexico, to generate a 0 1 dummy in divorce law
div_law <- read_xlsx("Inputs/Codes.xlsx", sheet = "state") %>% 
  mutate(date_law  = make_date(law_divorce_year, law_divorce_month))

basic_folio <- basic_ind09 %>% 
  select(year, folio, ls, pid_link, folio_uni, pid_link_uni, pid_link_mom, pid_link_dad) %>% 
  left_join(base_portad) %>% 
  left_join(base_con) %>% 
  mutate(year_int = case_when(anio == 2002 ~ "2002", 
                          anio < 10 ~ paste("200", anio, sep = ""), 
                          anio > 9 & anio < 20 ~ paste("20", anio, sep = ""), 
                          TRUE ~ NA_character_), 
         date_int = make_date(year_int, mes)) %>%  
  left_join(div_law, by = "ent") %>% 
  mutate(dummy_div = case_when(date_int > date_law ~ 1, 
                                TRUE ~ 0))

##### Book IIIA: Characteristics of household members #####

# weights general #

weightb3a_02 <- hh02w_b3a_02 %>% 
  mutate(year = 2002, fac_3a = factor_b3a, 
         ls_aux = as.character(ls),
         ls = case_when(ls<10 ~ paste("0", ls_aux, sep = ""),
                        TRUE ~ ls_aux)) %>%
  select(folio, ls, fac_3a, year)

weightb3a_05 <- ehh05w_b3a_05 %>% 
  mutate(year = 2005)

weightb3a_09 <- hh09w_b3a_09 %>% 
  mutate(year = 2009)

weight_b3a <- weightb3a_02%>% 
  rbind(weightb3a_05) %>% 
  rbind(weightb3a_09)

# other weights #

weight_b3al_05 <- ehh05w_b3al_05 %>% 
  mutate(year = 2005)

weight_b3al_09 <- hh09_lw_b3a_09 %>% 
  mutate(year = 2009)

weight_b3al <- weight_b3al_05 %>% 
  rbind(weight_b3al_09)

# IIIA:DH Household decision making

dec_02 <- iiia_dh_02 %>% 
  mutate(folio_aux = str_sub(folio, 1L, -3L),
         length = str_length(folio_aux),
         folio_2 = case_when(length == 2 ~ paste("0000", folio_aux, sep = ""),
                                            length == 3 ~ paste("000", folio_aux, sep = ""),
                                            length == 4 ~ paste("00", folio_aux, sep = ""),
                                            length == 5 ~ paste("0", folio_aux, sep = ""),
                                            TRUE ~ folio_aux),
         folio = paste(folio_2, "00", sep = ""),
         ls_aux = as.character(ls),
         ls = case_when(ls<10 ~ paste("0", ls_aux, sep = ""),
                        TRUE ~ ls_aux),
          pid_link = paste(folio_2, "00", ls, sep = ""),
         year = 2002) %>% 
  select(-c(folio_2, folio_aux, ls_aux, length))

dec_05 <- iiia_dh_05 %>% 
  mutate(year = 2005)

dec_09 <- iiia_dh_09 %>% 
  mutate(year = 2009)

dec_base <- dec_02 %>% 
  rbind(dec_05, dec_09) 

# IIIA 

ed_02 <- iiia_ed_02 %>% 
  mutate(folio_aux = str_sub(folio, 1L, -3L),
         length = str_length(folio_aux),
         folio_2 = case_when(length == 2 ~ paste("0000", folio_aux, sep = ""),
                             length == 3 ~ paste("000", folio_aux, sep = ""),
                             length == 4 ~ paste("00", folio_aux, sep = ""),
                             length == 5 ~ paste("0", folio_aux, sep = ""),
                             TRUE ~ folio_aux),
         folio = paste(folio_2, "00", sep = ""),
         ls_aux = as.character(ls),
         ls = case_when(ls<10 ~ paste("0", ls_aux, sep = ""),
                        TRUE ~ ls_aux),
         pid_link = paste(folio_2, "00", ls, sep = ""),
         year = 2002) %>% 
  select(year, folio, pid_link, ed01, ed03, ed05, ed06, ed07_1)

ed_05 <- iiia_ed_05 %>% 
  mutate(year = 2005) %>% 
  select(year, folio, pid_link, ed01, ed03, ed05, ed06, ed07_1)

ed_09 <- iiia_ed_09 %>% 
  mutate(year = 2009) %>% 
  select(year, folio, pid_link, ed01, ed03, ed05, ed06, ed07_1)

ed_base <- ed_02 %>% 
  rbind(ed_05, ed_09) 

##### Book: C - Control Book #####

weight_c_02 <- hh02w_bc_02 %>% 
  mutate(year = 2002)

weight_c_05 <- hh05w_bc_05 %>% 
  mutate(year = 2005)

weight_c_09 <- hh09w_bc_09 %>% 
  mutate(fac_libc = fac_c, year = 2009) %>% 
  select(-c(fac_c))

weight_c <- weight_c_02 %>% 
  rbind(weight_c_05) %>% 
  rbind(weight_c_09)

# other weights 

weight_cl_05 <- hh05w_bcl_05 %>% 
  mutate(fac_cl = fac_libcl, year = 2005) %>% 
  select(-c(fac_libcl))

weight_cl_09 <- hh09_lw_bc_09 %>% 
  mutate(year = 2009)

weight_cl <- weight_cl_05 %>% 
  rbind(weight_cl_09)

basic_ind <- basic_ind09 #%>% 
 # left_join(weight_c) %>% 
#  left_join(weight_cl)

##### Book V: Children #####

# Weights

weight_5_02 <- hh02w_b5_02 %>% 
  mutate(ls_aux = as.character(ls),
         ls = case_when(ls<10 ~ paste("0", ls_aux, sep = ""),
                        TRUE ~ ls_aux), fac_5 = factor_b5, year = 2002) %>% 
  select(-c(factor_b5, ls_aux))
  
weight_5_05 <- ehh05w_b5_05 %>% 
  mutate(year = 2005)

weight_5_09 <- hh09w_b5_09 %>% 
  mutate(year = 2009)

weight_5 <- weight_5_02 %>% 
  rbind(weight_5_05) %>% 
  rbind(weight_5_09)

# Longitudinal weights

weight_5l_05 <- ehh05w_b5l_05 %>% 
  mutate(year = 2005)

weight_5l_09 <- hh09_lw_b5_09 %>% 
  mutate(year = 2009)

weight_5l <- weight_5l_05 %>% 
  rbind(weight_5l_09)

# Book V edna 

edna_02 <- v_edna_02 %>% 
  left_join(v_portad_02) %>% 
  mutate(folio_aux = str_sub(folio, 1L, -3L),
         length = str_length(folio_aux),
         folio_2 = case_when(length == 2 ~ paste("0000", folio_aux, sep = ""),
                             length == 3 ~ paste("000", folio_aux, sep = ""),
                             length == 4 ~ paste("00", folio_aux, sep = ""),
                             length == 5 ~ paste("0", folio_aux, sep = ""),
                             TRUE ~ folio_aux),
         folio = paste(folio_2, "00", sep = ""),
         ls_aux = as.character(ls),
         ls = case_when(ls<10 ~ paste("0", ls_aux, sep = ""),
                        TRUE ~ ls_aux),
         pid_link = paste(folio_2, "00", ls, sep = ""),
         year = 2002) %>% 
  select(year, folio, ls, pid_link, ent, edn01, edn02, edn03, edn09, edn10_1)

edna_05 <- v_edna_05 %>% 
  left_join(v_portad_05) %>% 
  mutate(year = 2005) %>% 
  select(year, folio, ls, pid_link, ent, edn01, edn02, edn03, edn09, edn10_1)

edna_09 <- v_edna_09 %>% 
  left_join(v_portad_09) %>% 
  mutate(year = 2009) %>%
  select(year, folio, ls, pid_link, ent, edn01, edn02, edn03, edn09, edn10_1)

edna_base <- edna_02 %>% 
  rbind(edna_05, edna_09) %>% 
  left_join(basic_folio)# %>% 
 # left_join(weight_5) %>% 
#  left_join(weight_5l)

##### Book S: Anthropometrics and Biomarkers #####

# Weights

weight_s_02 <- hh02w_bs_02 %>% 
  mutate(ls_aux = as.character(ls),
         ls = case_when(ls<10 ~ paste("0", ls_aux, sep = ""),
                        TRUE ~ ls_aux), fac_s = factor_bs, year = 2002) %>% 
  select(-c(factor_bs, ls_aux))

weight_s_05 <- ehh05w_bs_05 %>% 
  mutate(year = 2005)

weight_s_09 <- hh09w_bs_09 %>% 
  mutate(year = 2009)

weight_s <- weight_s_02 %>% 
  rbind(weight_s_05) %>% 
  rbind(weight_s_09)

# Longitudinal weights

weight_sl_05 <- ehh05w_bsl_05 %>% 
  mutate(year = 2005)

weight_sl_09 <- hh09_lw_bs_09 %>% 
  mutate(year = 2009)

weight_sl <- weight_sl_05 %>% 
  rbind(weight_sl_09)

# Book S antro 

bio_02 <- s_sa_02 %>% 
  mutate(folio_aux = str_sub(folio, 1L, -3L),
         length = str_length(folio_aux),
         folio_2 = case_when(length == 2 ~ paste("0000", folio_aux, sep = ""),
                             length == 3 ~ paste("000", folio_aux, sep = ""),
                             length == 4 ~ paste("00", folio_aux, sep = ""),
                             length == 5 ~ paste("0", folio_aux, sep = ""),
                             TRUE ~ folio_aux),
         folio = paste(folio_2, "00", sep = ""),
         ls_aux = as.character(ls00),
         ls = case_when(ls00<10 ~ paste("0", ls_aux, sep = ""),
                        TRUE ~ ls_aux),
         pid_link = paste(folio_2, "00", ls, sep = ""),
         year = 2002,
         sa07_21 = sa07_2, 
         sa08_21 = sa09_2) %>% 
  select(year, folio, pid_link, ls, sa01, sa03, sa07_21, sa08_21) 

bio_05 <- s_sa_05 %>% 
  mutate(year = 2005) %>% 
  select(year, folio, pid_link, ls, sa01, sa03, sa07_21, sa08_21) 

bio_09 <- s_sa_09 %>% 
  mutate(year = 2009) %>%
  select(year, folio, pid_link, ls, sa01, sa03, sa07_21, sa08_21) 

bio_base <- bio_02 %>% 
  rbind(bio_05, bio_09) %>% 
  left_join(basic_folio)# %>% 
#  left_join(weight_s) %>% 
#  left_join(weight_sl)

##### BooK EA: Adult cognitive #####

weight_ea_02 <- hh02w_bea_02 %>% 
  mutate(ls_aux = as.character(ls),
         ls = case_when(ls<10 ~ paste("0", ls_aux, sep = ""),
                        TRUE ~ ls_aux), fac_ea = factor_bea, year = 2002) %>% 
  select(-c(factor_bea, ls_aux))

weight_ea_05 <- ehh05w_bea_05 %>% 
  mutate(year = 2005)

weight_ea_09 <- hh09w_bea_09 %>% 
  mutate(year = 2009)

weight_ea <- weight_ea_02 %>% 
  rbind(weight_ea_05) %>% 
  rbind(weight_ea_09)

# Longitudinal weights

weight_eal_05 <- ehh05w_beal_05 %>% 
  mutate(year = 2005)

weight_eal_09 <- hh09_lw_bea_09 %>% 
  mutate(year = 2009)

weight_eal <- weight_eal_05 %>% 
  rbind(weight_eal_09)

# Book EA: Adult cognitive

cog_ad_02 <- ea_eca_02 %>% 
  mutate(folio_aux = str_sub(folio, 1L, -3L),
         length = str_length(folio_aux),
         folio_2 = case_when(length == 2 ~ paste("0000", folio_aux, sep = ""),
                             length == 3 ~ paste("000", folio_aux, sep = ""),
                             length == 4 ~ paste("00", folio_aux, sep = ""),
                             length == 5 ~ paste("0", folio_aux, sep = ""),
                             TRUE ~ folio_aux),
         folio = paste(folio_2, "00", sep = ""),
         ls_aux = as.character(ls),
         ls = case_when(ls<10 ~ paste("0", ls_aux, sep = ""),
                        TRUE ~ ls_aux),
         pid_link = paste(folio_2, "00", ls, sep = ""),
         year = 2002) %>% 
  select(year, folio, ls, pid_link, eca01:eca12) 

cog_ad_05 <- ea_eca_05 %>% 
  mutate(year = 2005) %>% 
  select(year, folio, ls, pid_link, eca01:eca12) 

cog_ad_09 <- ea_eca_09 %>% 
  mutate(year = 2009) %>%
  select(year, folio, ls, pid_link, eca01:eca12) 

cog_ad <- cog_ad_02 %>% 
  rbind(cog_ad_05, cog_ad_09) %>% 
  left_join(basic_folio)# %>% 
#  left_join(weight_ea) %>% 
#  left_join(weight_eal)

##### Book EN: Child cognitive #####

weight_en_02 <- hh02w_ben_02 %>% 
  mutate(ls_aux = as.character(ls),
         ls = case_when(ls<10 ~ paste("0", ls_aux, sep = ""),
                        TRUE ~ ls_aux), fac_en = factor_ben, year = 2002) %>% 
  select(-c(factor_ben, ls_aux))

weight_en_05 <- ehh05w_ben_05 %>% 
  mutate(year = 2005)

weight_en_09 <- hh09w_ben_09 %>% 
  mutate(year = 2009)

weight_en <- weight_en_02 %>% 
  rbind(weight_en_05) %>% 
  rbind(weight_en_09)

# Longitudinal weights

weight_enl_05 <- ehh05w_benl_05 %>% 
  mutate(year = 2005)

weight_enl_09 <- hh09_lw_ben_09 %>% 
  mutate(year = 2009)

weight_enl <- weight_enl_05 %>% 
  rbind(weight_enl_09)

# Book EN: Children cognitive

cog_ch_02 <- en_ecn_02 %>% 
  mutate(folio_aux = str_sub(folio, 1L, -3L),
         length = str_length(folio_aux),
         folio_2 = case_when(length == 2 ~ paste("0000", folio_aux, sep = ""),
                             length == 3 ~ paste("000", folio_aux, sep = ""),
                             length == 4 ~ paste("00", folio_aux, sep = ""),
                             length == 5 ~ paste("0", folio_aux, sep = ""),
                             TRUE ~ folio_aux),
         folio = paste(folio_2, "00", sep = ""),
         ls_aux = as.character(ls),
         ls = case_when(ls<10 ~ paste("0", ls_aux, sep = ""),
                        TRUE ~ ls_aux),
         pid_link = paste(folio_2, "00", ls, sep = ""),
         year = 2002) %>% 
  select(year, folio, ls, pid_link, ecn01:ecn18) 

cog_ch_05 <- en_ecn_05 %>% 
  mutate(year = 2005) %>% 
  select(year, folio, ls, pid_link, ecn01:ecn18) 

cog_ch_09 <- en_ecn_09 %>% 
  mutate(year = 2009) %>%
  select(year, folio, ls, pid_link, ecn01:ecn18) 

cog_ch <- cog_ch_02 %>% 
  rbind(cog_ch_05, cog_ch_09) %>% 
  left_join(basic_folio)# %>% 
 # left_join(weight_en) %>% 
#  left_join(weight_enl)

##### Select necessary databases and generate RData #####

rm(list=setdiff(ls(), c("basic_folio", "bio_base", "weight_s", "weight_sl", "basic_ind", "ed_base",
                        "weight_c", "weight_cl", "cog_ad", "weight_ea", "weight_eal", "cog_ch", 
                        "weight_en", "weight_enl", "dec_base", "weight_b3a", "weight_b3al", 
                        "edna_base", "weight_5", "weight_5l")))

save.image(file = 'Outputs/Datasets_dissertation.RData')


