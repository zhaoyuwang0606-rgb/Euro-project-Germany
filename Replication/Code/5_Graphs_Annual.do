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
graph export "${Fig}SCM_gdp_Annual.eps", replace
graph export "${Fig}SCM_gdp_Annual.pdf", replace



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

if (code=="GRC") {
	*get standard deviation (in percent)
	gen dif = `var' - `var'cg
	sum dif if id==`x' & year<$treatment_GRC
	gen `var'std=(r(sd))*2

	*generate upper and lower bound
	gen fup=`var'cg +`var'std
	gen flow=`var'cg -`var'std
	*draw figure
	twoway (rarea fup flow date if id== `x', color(gs13)) (line `var'cg `var' date if id == `x', lwidth(thick ..) ylabel(-100(200)900, labsize(large)) legend(off) xtitle(" ") lcolor(blue) lpa("-") tline($treatment_GRC, lcolor(gray)) xlabel(1970(6)2007, labsize(large)) tlabel("$begin"(6)"$end"))
	drop dif fup flow
}
else if (code=="IRL") {
	*get standard deviation (in percent)
	gen dif = `var' - `var'cg 
	sum dif if id==`x' & year<$treatment
	gen `var'std=(r(sd))*2

	
	*generate upper and lower bound
	gen fup=`var'cg +`var'std
	gen flow=`var'cg -`var'std
	*draw figure
	twoway (rarea fup flow date if id== `x', color(gs13)) (line `var'cg `var' date if id == `x', lwidth(thick ..) ylabel(-100(300)1200, labsize(large)) legend(off) xtitle(" ") lcolor(blue) lpa("-") tline($treatment, lcolor(gray)) xlabel(1970(6)2007, labsize(large)) tlabel("$begin"(6)"$end"))
	drop dif fup flow
}
else if (code=="LUX") {
	*get standard deviation (in percent)
	gen dif = `var' - `var'cg 
	sum dif if id==`x' & year<$treatment
	gen `var'std=(r(sd))*2

	
	*generate upper and lower bound
	gen fup=`var'cg +`var'std
	gen flow=`var'cg -`var'std
	*draw figure
	twoway (rarea fup flow date if id== `x', color(gs13)) (line `var'cg `var' date if id == `x', lwidth(thick ..) ylabel(-100(200)900, labsize(large)) legend(off) xtitle(" ") lcolor(blue) lpa("-") tline($treatment, lcolor(gray)) xlabel(1970(6)2007, labsize(large)) tlabel("$begin"(6)"$end"))
	drop dif fup flow
}
else {
	*get standard deviation (in percent)
	gen dif = `var' - `var'cg 
	sum dif if id==`x' & year<$treatment
	gen `var'std=(r(sd))*2

	
	*generate upper and lower bound
	gen fup=`var'cg +`var'std
	gen flow=`var'cg -`var'std
	*draw figure
	twoway (rarea fup flow date if id== `x', color(gs13)) (line `var'cg `var' date if id == `x', lwidth(thick ..) ylabel(-100(200)700, labsize(large)) legend(off) xtitle(" ") lcolor(blue) lpa("-") tline($treatment, lcolor(gray)) xlabel(1970(6)2007, labsize(large)) tlabel("$begin"(6)"$end"))
	*twoway (rarea fup flow date if id== `x', color(gs13)) (line `var'cg `var' date if id == `x', lwidth(thick ..) legend(off) xtitle(" ") lcolor(blue) lpa("-") tline($treatment, lcolor(gray)) xlabel(1970(6)2007, labsize(large)) tlabel("$begin"(6)"$end"))
	drop dif fup flow
}

*graph save "${Fig}Auxi\SCM_`var'_`x'_Annual.gph", replace
graph export "${Fig}Auxi\SCM_`var'_`x'_Annual.eps", replace

*use `"${PATH_OUT_ANALYSIS}/LP_Annual"', clear

*global SPECIFICATION Annual
*global COMPONENTS On
*upload correspondent global variables
*do `"${PATH_IN_MASTER}/Master_EMU.do"'

}

/*
*New Table / Graph of GDP decomposition through time
gen ggvar = (g-gcg)/gcg * 100
gen cvar = (csh_c_`p' - csh_ccg_`p') / (g - gcg) *ggvar
gen gvar = (csh_g_`p' - csh_gcg_`p') / (g - gcg) *ggvar
gen ivar = (csh_i_`p' - csh_icg_`p') / (g - gcg) *ggvar
gen xvar = (csh_x_`p' - csh_xcg_`p') / (g - gcg) *ggvar
gen mvar = (csh_m_`p' - csh_mcg_`p') / (g - gcg) *ggvar
gen nxvar = xvar - mvar

twoway (line ggvar year if id== `x', lwidth(thick ..)) (line cvar gvar ivar nxvar year if id == `x',  xtitle(" ") lcolor(blue) lpa("-") xlabel(1970(6)2007, labsize(large)) tlabel(1970(6)2007))
graph save `"${PATH_OUT_ANALYSIS}/Decomposition_`x'.gph"', replace
graph export `"${PATH_OUT_FIGURES}/Decomposition_`x'.eps"', replace
*/



*Compute the contribution of each component to GDP gap
*GDP
gen csh_5 = (g[38] - gcg[38])/ gcg[38] *100
gen csh_1 = (csh_c_`p'[38] - csh_ccg_`p'[38]) / (g[38] - gcg[38]) *csh_5
gen csh_2 = (csh_g_`p'[38] - csh_gcg_`p'[38]) / (g[38] - gcg[38]) *csh_5
gen csh_3 = (csh_i_`p'[38] - csh_icg_`p'[38]) / (g[38] - gcg[38]) *csh_5
gen csh_6 = (csh_x_`p'[38] - csh_xcg_`p'[38]) / (g[38] - gcg[38]) *csh_5
gen csh_7 = (csh_m_`p'[38] - csh_mcg_`p'[38]) / (g[38] - gcg[38]) *csh_5
gen csh_4 = csh_6 - csh_7

gen csh1 = csh_1 /csh_5 *100
gen csh2 = csh_2 /csh_5 *100
gen csh3 = csh_3 /csh_5 *100
gen csh5 = csh_5 /csh_5 *100
gen csh6 = csh_6 /csh_5 *100
gen csh7 = csh_7 /csh_5 *100
gen csh4 = csh6 - csh7

/*
*Compute the contribution of each component to GDP gap  --  GROWTH!!
*GDP
		foreach var in $outcome $outcomecg {
			local f=`var'[1]
			replace `var' = (`var'-`f')*100
		}
gen csh_5 = ($outcome[38] - $outcomecg[38])/ $outcomecg[38] *100
gen csh_1 = (csh_c[38] - csh_ccg[38]) / ($outcome[38] - $outcomecg[38]) *csh_5
gen csh_2 = (csh_g[38] - csh_gcg[38]) / ($outcome[38] - $outcomecg[38]) *csh_5
gen csh_3 = (csh_i[38] - csh_icg[38]) / ($outcome[38] - $outcomecg[38]) *csh_5
gen csh_6 = (csh_x[38] - csh_xcg[38]) / ($outcome[38] - $outcomecg[38]) *csh_5
gen csh_7 = (csh_m[38] - csh_mcg[38]) / ($outcome[38] - $outcomecg[38]) *csh_5
gen csh_4 = csh_6 - csh_7

gen csh1 = csh_1 /csh_5 *100
gen csh2 = csh_2 /csh_5 *100
gen csh3 = csh_3 /csh_5 *100
gen csh5 = csh_5 /csh_5 *100
gen csh6 = csh_6 /csh_5 *100
gen csh7 = csh_7 /csh_5 *100
gen csh4 = csh6 - csh7
*/


/*
gen csh_1 = 100*wb_1*(((csh_c[38]-csh_ccg[38])/csh_c[38] )/abs(csh_5))
gen csh_2 = 100*wb_2*(((csh_g[38]-csh_gcg[38])/csh_g[38] )/abs(csh_5))
gen csh_3 = 100*wb_3*(((csh_i[38]-csh_icg[38])/csh_i[38] )/abs(csh_5))
gen csh_4 = 100*wb_4*(((csh_nx[38]-csh_nxcg[38])/csh_nx[38] )/abs(csh_5))
*/

forvalues c=1/5{
	local flag = csh_`c'
	matrix weights[`p',`c']= `flag'
}

* To compute euro per capita results in 2011 EUR divide by population in the year
* of the gap 2007 and multiply by the exchange rate of the time from PWT 0.719355
local flag = (g[38] - gcg[38])/pop[38] * 0.719355225563049
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

putexcel set `"${hp}\Output\Decomposition.xlsx"', sheet("Decomposition") modify
putexcel A1=matrix(weights)


