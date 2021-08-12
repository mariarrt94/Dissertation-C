** stata
use "C:/Users/maria/OneDrive/Desktop/Development Economics MSc/Dissertation/R/Outputs/base_child.dta"

destring, replace
tsset pid_link year

xtreg test_score_z sex school_att worked_12_mom HH_mom test_score_mom test_score_dad   PC1tot_mom PC2tot_mom income_crea_pc children, vce(robust) fe i(pid_link)

xtabond2 test_score_z sex school_att test_score_mom test_score_dad income_crea_pc children, gmm(income_crea_pc) robust

