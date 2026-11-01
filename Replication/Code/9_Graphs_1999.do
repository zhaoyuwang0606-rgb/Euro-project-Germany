 				*** Save Graphs  - Specification Annual ***

/*
The file Graphs.do combines all the individual countries graphs into one figure.
*/


clear all
set more off
set scheme s1color
graph set window fontface default
capture graph set window fontface Helvetica 
use "${Data}LP_Annual", clear

foreach x in $countries {

keep if id==`x'

local s = country 
label var $outcome "Country"
label var $outcomecg "Synthetic Country"
if (code=="GRC") {
	*get standard deviation (in percent)
	gen dif = $outcome - $outcomecg
	sum dif if id==`x' & year<$treatment_GRC
	gen std=100*(r(sd))*2
	

		*present as "deviation from base year (percent)"
		foreach var in $outcome $outcomecg {
			local f=`var'[1]
			replace `var' = (`var'-`f')*100
		}
		
	*generate upper and lower bound
	gen fup=$outcomecg +std
	gen flow=$outcomecg -std
	*draw figure
	twoway (rarea fup flow date if id== `x', color(gs13)) (line $outcomecg $outcome date if id == `x', lwidth(thick ..)  legend(off) ylabel(0(100)500) xtitle(" ") ti("`s'") lcolor(blue) lpa("-") tline($treatment_GRC, lcolor(gray)) tlabel("$begin"(6)"$end"))
}
else {
	*get standard deviation (in percent)
	gen dif = $outcome - $outcomecg 
	sum dif if id==`x' & year<$treatment
	gen std=100*(r(sd))*2
	
		*present as "deviation from base year (percent)"
		foreach var in $outcome $outcomecg {
			local f=`var'[1]
			replace `var' = (`var'-`f')*100
		}

	
	*generate upper and lower bound
	gen fup=$outcomecg +std
	gen flow=$outcomecg -std
	*draw figure
	twoway (rarea fup flow date if id== `x', color(gs13)) (line $outcomecg $outcome date if id == `x', lwidth(thick ..)  legend(off) ylabel(0(100)500) xtitle(" ") ti("`s'") lcolor(blue) lpa("-") tline($treatment, lcolor(gray)) tlabel("$begin"(6)"$end"))
}
graph save "${Fig}Auxi\SCM_gdp_`x'_Annual.gph", replace
graph export "${Fig}Auxi\SCM_gdp_`x'_Annual.eps", replace

use "${Data}LP_Annual", clear
}

graph combine "${Fig}Auxi\SCM_gdp_1_Annual.gph" "${Fig}Auxi\SCM_gdp_3_Annual.gph" "${Fig}Auxi\SCM_gdp_7_Annual.gph" "${Fig}Auxi\SCM_gdp_8_Annual.gph" "${Fig}Auxi\SCM_gdp_9_Annual.gph" "${Fig}Auxi\SCM_gdp_10_Annual.gph" "${Fig}Auxi\SCM_gdp_12_Annual.gph" "${Fig}Auxi\SCM_gdp_14_Annual.gph" "${Fig}Auxi\SCM_gdp_16_Annual.gph" "${Fig}Auxi\SCM_gdp_18_Annual.gph" "${Fig}Auxi\SCM_gdp_21_Annual.gph" "${Fig}Auxi\SCM_gdp_22_Annual.gph", l2title("deviation from $begin (percent)", size(small)) iscale(*0.8) imargin(tiny) com 
graph export "${Fig}SCM_gdp_Annual_1999.eps", replace
graph export "${Fig}SCM_gdp_Annual_1999.pdf", replace



********************************************************************************
* Graphs for GDP components (C + G + I + X + M) and emp share and lab prod growth  *
use "${Data}LP_Annual", clear

**Construct matrix for store the contribution of each GDP component
local p=1
local l=1
matrix weights = J(12 ,6 ,0)
matrix rownames weights = "Austria" "Belgium" "Finland" "France" "Germany" "Greece" "Ireland" "Italy" "Luxembourg" "Netherlands" "Portugal" "Spain"
matrix colnames weights = "Private_Consumption" "Government_Consumption" "Investment" "Net_Exports" "GDP" "Euro_per_capita"

matrix weightss = J(12 ,3 ,0)
matrix rownames weightss = $countries_name
matrix colnames weightss = "Net_Exports" "Exports" "Imports"

foreach x in $countries {


foreach var in csh_c csh_g csh_i csh_m csh_x csh_nx{
	
keep if id==`x'
local s = country 

*get real gdp
replace g = (1 + normgdp_s) * rgdpna[1]
replace gcg = (1 + normgdp_scg) * rgdpna[1]

	*Store series for each series to compute its contribution in a Table
	gen `var'_`p' = `var' * g 
	gen `var'cg_`p' = `var'cg * gcg 
	
	*kjhkhkj
	replace `var' = `var' * g 
	replace `var'cg = `var'cg * gcg 
	
	
	*present as "deviation from the actual series base year (percent)"
	local f=`var'[1]
	replace `var' = ((`var'/`f')-1)*100
	*uncomment this next line if we want to see the "deviation from each series base year (percent)" instead
	local f=`var'cg[1]
	replace `var'cg = ((`var'cg/`f')-1)*100


	
	*Computation of net exports as the difference between the series (to answer: which series is growing more?)
	if (`var'==csh_nx){
		replace csh_nx = csh_x-csh_m
		replace csh_nxcg = csh_xcg-csh_mcg
	}



}



*Compute the contribution of each component to GDP gap
*GDP
gen csh_5 = (g[29] - gcg[29])/ gcg[29] *100
gen csh_1 = (csh_c_`p'[29] - csh_ccg_`p'[29]) / (g[29] - gcg[29]) *csh_5
gen csh_2 = (csh_g_`p'[29] - csh_gcg_`p'[29]) / (g[29] - gcg[29]) *csh_5
gen csh_3 = (csh_i_`p'[29] - csh_icg_`p'[29]) / (g[29] - gcg[29]) *csh_5
gen csh_6 = (csh_x_`p'[29] - csh_xcg_`p'[29]) / (g[29] - gcg[29]) *csh_5
gen csh_7 = (csh_m_`p'[29] - csh_mcg_`p'[29]) / (g[29] - gcg[29]) *csh_5
gen csh_4 = csh_6 - csh_7

gen csh1 = csh_1 /csh_5 *100
gen csh2 = csh_2 /csh_5 *100
gen csh3 = csh_3 /csh_5 *100
gen csh5 = csh_5 /csh_5 *100
gen csh6 = csh_6 /csh_5 *100
gen csh7 = csh_7 /csh_5 *100
gen csh4 = csh6 - csh7



forvalues c=1/5{
	local flag = csh_`c'
	matrix weights[`p',`c']= `flag'
}

* To compute euro per capita results in 2011 EUR divide by population in the year
* of the gap 2007 and multiply by the exchange rate of the time from PWT 0.719355
local flag = (g[29] - gcg[29])/pop[29] * 1.02
matrix weights[`p',6]= `flag'

local p=`p'+1

matrix weightss[`l',1]=csh_4[1]
matrix weightss[`l',2]=csh_6[1]
matrix weightss[`l',3]=csh_7[1]


local l=`l'+1


use "${Data}LP_Annual", clear

}



outtable using "${Tab}Weights_Decomposition", mat(weights) nolabel replace nobox asis center f(%9.2f)
outtable using "${Tab}Net_Exports_Decomposition", mat(weightss) nolabel replace nobox asis center f(%9.2f)

putexcel set `"${hp}\Output\Decomposition_Maastricht.xlsx"', sheet("Decomposition") modify
putexcel A1=matrix(weights)

