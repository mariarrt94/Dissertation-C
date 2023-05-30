

# UNVEILING FEMALE BARGAINING POWER: DECODING ITS DETERMINANTS AND POTENTIAL IMPACTS


The purpose of this project is to undertand how gender-specific changes in labour
demand affect female bargaining power. In addition, it seeks to unveil its potential 
effects on children cognitive skills and health. 

## Data 

I use the Mexican Family Life Survey which consists in three waves of household and individual information. 

## Structure of the code

The code contains all the scripts used to produce this work. Paper forecoming. 

1. Data reading: reads needed information from Mexican Family Life Survey
2. Creating database: uses the previous code, and constructs complete databases.
3. Tidying database: cleans database and constructs basic summary statistics. 
   (creates Rdata Data_tidy_dissertation that will be used to perform final analysis)
4. Women index: performs PCA and constructs bargaining power index from decisions within the household.
5. Complete data: joins the previous databases and joins the final dabases for 
   the analysis (adults and children, including bargaining index). 
6. Shift share - read INEGI information and construct shift share instruments. 
7. Determinants BP: studies the variables which seem related with our Bargaining Power Index. 
8. Effect change in labour: This produces the main results of the study. 
   Changed in female labour demand and how they affect the Bargaining power index, 
   lastly it analyses the effect of said shifts in children's outcomes. 
9. Summary statistics: This code could automatically be run after step 5. 
10. Graphs summary statistics: This code could automatically be run after step 5. 
11. Attrition: revision of the differences of attrited children and women vs non-attrited. 
12. Robustness checks: This code runs certain robustness checks for the analysis. 
13. extra: aux inflation is a code needed to put income of the three waves on the same 
units


