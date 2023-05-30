##### Dissertation Maria Reyes Retana 
# This code reads the raw data base from the Mexican Family Life Survey which can be found here:
# http://www.ennvih-mxfls.org/english/ennvih-1.html 

##### Libraries #####

library(tidyverse)
library(foreign)
library(haven)
library(purrr)

# Read data from different waves

##### 2002 #####

# select all the variables we need from each book
# Book control
 
 names <- c("c_portad", "c_ls", "hh02w_bc", "c_conpor")

 for(i in names){
   filepath <- file.path("Data/2002/hh02dta_bc", paste(i,".dta",sep=""))
   assign(paste(i,"_02", sep = ""), read_dta(filepath))
 }
 
 # Book IIIA - Characteristics of adult household members
 
 names <- c("iiia_dh", "iiia_ed", "iiia_tb", "hh02w_b3a", "iiia_ata")
 
 for(i in names){
   filepath <- file.path("Data/2002/hh02dta_b3a", paste(i,".dta",sep=""))
   assign(paste(i,"_02", sep = ""), read_dta(filepath))
 }
 
 # Book V - Characteristics of Children younger than 15 years old 
 
 names <- c("v_portad", "v_edna", "hh02w_b5", "v_conpor", "v_esn")
 
 for(i in names){
   filepath <- file.path("Data/2002/hh02dta_b5", paste(i,".dta",sep=""))
   assign(paste(i,"_02", sep = ""), read_dta(filepath))
 }
 
 # Book S - anthropometrics and biomarkers
 
 names <- c("s_portad", "s_sa", "hh02w_bs")
 
 for(i in names){
   filepath <- file.path("Data/2002/hh02dta_bs", paste(i,".dta",sep=""))
   assign(paste(i,"_02", sep = ""), read_dta(filepath))
 }
 
 # Book EA - Adult cognitive ability
 
 names <- c("ea_portad", "ea_eca", "hh02w_bea")
 
 for(i in names){
   filepath <- file.path("Data/2002/hh02dta_bea", paste(i,".dta",sep=""))
   assign(paste(i,"_02", sep = ""), read_dta(filepath))
 }
 
 # Book EN - Child cognitive ability
 
 names <- c("en_portad", "en_ecn", "hh02w_ben")
 
 for(i in names){
   filepath <- file.path("Data/2002/hh02dta_ben", paste(i,".dta",sep=""))
   assign(paste(i,"_02", sep = ""), read_dta(filepath))
 }
##### 2005 ####
 
 # Book control 
 
 names <- c("c_portad", "c_ls", "hh05w_bc", "hh05w_bcl", "c_conpor")
 
 for(i in names){
    filepath <- file.path("Data/2005/hh05dta_bc", paste(i,".dta",sep=""))
    assign(paste(i,"_05", sep = ""), read_dta(filepath))
 }
 
 # Book IIIA - Characteristics of adult household members
 
 names <- c("iiia_dh", "iiia_ed", "iiia_tb", "ehh05w_b3a", "ehh05w_b3al", "iiia_ata")
 
 for(i in names){
    filepath <- file.path("Data/2005/hh05dta_b3a", paste(i,".dta",sep=""))
    assign(paste(i,"_05", sep = ""), read_dta(filepath))
 }
 
 # Book V - Characteristics of Children younger than 15 years old 
 
 names <- c("v_portad", "v_edna", "ehh05w_b5", "ehh05w_b5l", "v_conpor", "v_esn")
 
 for(i in names){
    filepath <- file.path("Data/2005/hh05dta_b5", paste(i,".dta",sep=""))
    assign(paste(i,"_05", sep = ""), read_dta(filepath))
 }
 
 # Book S - anthropometrics and biomarkers
 
 names <- c("s_portad", "s_sa", "ehh05w_bs", "ehh05w_bsl")
 
 for(i in names){
    filepath <- file.path("Data/2005/hh05dta_bs", paste(i,".dta",sep=""))
    assign(paste(i,"_05", sep = ""), read_dta(filepath))
 }
 
 # Book EA - Adult cognitive ability
 
 names <- c("ea_portad", "ea_eca", "ehh05w_bea", "ehh05w_beal")
 
 for(i in names){
    filepath <- file.path("Data/2005/hh05dta_bea", paste(i,".dta",sep=""))
    assign(paste(i,"_05", sep = ""), read_dta(filepath))
 }
 
 # Book EN - Child cognitive ability
 
 names <- c("en_portad", "en_ecn", "ehh05w_ben", "ehh05w_benl")
 
 for(i in names){
    filepath <- file.path("Data/2005/hh05dta_ben", paste(i,".dta",sep=""))
    assign(paste(i,"_05", sep = ""), read_dta(filepath))
 } 

##### 2009 ####

 # Book control 
 
 names <- c("c_portad", "c_ls", "hh09w_bc", "hh09_lw_bc", "c_conpor")
 
 for(i in names){
    filepath <- file.path("Data/2009/hh09dta_bc", paste(i,".dta",sep=""))
    assign(paste(i,"_09", sep = ""), read_dta(filepath))
 }
 
 # Book IIIA - Characteristics of adult household members
 
 names <- c("iiia_dh", "iiia_ed", "iiia_tb", "hh09w_b3a", "hh09_lw_b3a", "iiia_ata")
 
 for(i in names){
    filepath <- file.path("Data/2009/hh09dta_b3a", paste(i,".dta",sep=""))
    assign(paste(i,"_09", sep = ""), read_dta(filepath))
 }
 
 # Book V - Characteristics of Children younger than 15 years old 
 
 names <- c("v_portad", "v_edna", "hh09w_b5", "hh09_lw_b5", "v_conpor", "v_esn")
 
 for(i in names){
    filepath <- file.path("Data/2009/hh09dta_b5", paste(i,".dta",sep=""))
    assign(paste(i,"_09", sep = ""), read_dta(filepath))
 }
 
 # Book S - anthropometrics and biomarkers
 
 names <- c("s_portad", "s_sa", "hh09w_bs", "hh09_lw_bs")
 
 for(i in names){
    filepath <- file.path("Data/2009/hh09dta_bs", paste(i,".dta",sep=""))
    assign(paste(i,"_09", sep = ""), read_dta(filepath))
 }
 
 # Book EA - Adult cognitive ability
 
 names <- c("ea_portad", "ea_eca", "hh09w_bea", "hh09_lw_bea")
 
 for(i in names){
    filepath <- file.path("Data/2009/hh09dta_bea", paste(i,".dta",sep=""))
    assign(paste(i,"_09", sep = ""), read_dta(filepath))
 }
 
 # Book EN - Child cognitive ability
 
 names <- c("en_portad", "en_ecn", "hh09w_ben", "hh09_lw_ben")
 
 for(i in names){
    filepath <- file.path("Data/2009/hh09dta_ben", paste(i,".dta",sep=""))
    assign(paste(i,"_09", sep = ""), read_dta(filepath))
 } 
