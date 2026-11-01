				*** Aggregation of SCM series - Specification Annual ***

*Authors: Ricardo Duque Gabriel and Ana Sofia Pessoa 
*Start Date: 13/12/2018
*Last Update: 14/11/2019

/*
The file Aggregation_Annual.do aggregates the dataset Analysis_Annual.dta 
with the datasets created after the SCM with the synthetic economic series 
generated. The new dataset is saved as LP_Annual.dta 
*/

clear
set more off


use "${Data}Auxi\SCM_1_Annual", clear

//appending the SCM generated datasets
foreach x in 3 7 8 9 10 12 14 16 18 21 22{
	append using "${Data}Auxi\SCM_`x'_Annual"
}
//save the new SCM dataset
save "${Data}Auxi\SCM", replace

use "${Data}Analysis_Annual", clear
//merging analysis.dta with SCM
merge m:1 code date using "${Data}Auxi\SCM"

drop _merge




*Due to the residual component that we ignore, the shares do not always sum
*to 100%, so we must normalize these valuees to obtain interpreteable results
*that impute the small measurement error into all shares equally. Both, to the
*doppelganger as well as the actual series
gen csh_residual = (1 - (csh_c + csh_g + csh_i + csh_x - csh_m)) / 5
replace csh_c = csh_c + csh_residual
replace csh_g = csh_g + csh_residual
replace csh_i = csh_i + csh_residual
replace csh_x = csh_x + csh_residual
replace csh_m = csh_m - csh_residual

gen csh_residualcg = (1 - (csh_ccg + csh_gcg + csh_icg + csh_xcg - csh_mcg)) / 5
replace csh_ccg = csh_ccg + csh_residualcg
replace csh_gcg = csh_gcg + csh_residualcg
replace csh_icg = csh_icg + csh_residualcg
replace csh_xcg = csh_xcg + csh_residualcg
replace csh_mcg = csh_mcg - csh_residualcg

gen csh_nx =csh_x-csh_m
gen csh_nxcg=csh_xcg-csh_mcg

gen g=.
gen gcg=.

save "${Data}LP_Annual", replace




/*
********************************************************************************
*Inequality and ltrate graphs


use `"${PATH_OUT_ANALYSIS}/SCM_1_Annual_Inequality"'

//appending the SCM generated datasets
foreach x in 3 7 8 9 10 12 14 17 19 22 23{
	append using `"${PATH_OUT_ANALYSIS}/SCM_`x'_Annual_Inequality"'
}
//save the new SCM dataset
save "SCM", replace

use `"${PATH_OUT_DATA}/Analysis_Annual"', clear
//merging analysis.dta with SCM
merge m:1 code date using SCM

drop _merge

save `"${PATH_OUT_ANALYSIS}/LP_Annual_Inequality"', replace
