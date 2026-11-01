/*
The file Graphs.do combines all the individual countries graphs into one figure
for long term bond rates, inflation, gdp, etc...
*/


clear all
set more off
set scheme s1color
graph set window fontface default
capture graph set window fontface Helvetica 
use "${Data}LP_Annual", clear


if ("$outcome" == "inflation") {

foreach x in $countries {

keep if id==`x'

local s = country 
label var $outcome "Country"
label var $outcomecg "Synthetic Country"
if (code=="GRC") {
	*get standard deviation (in percent)
	gen dif = $outcome - $outcomecg
	sum dif if id==`x' & year<$treatment_GRC
	gen std=(r(sd))*2
	

		*present as "deviation from base year (percent)"
		*foreach var in $outcome $outcomecg {
		*	local f=`var'[1]
		*	replace `var' = (`var'-`f')
		*}
		
	*generate upper and lower bound
	gen fup=$outcomecg +std
	gen flow=$outcomecg -std
	*draw figure
	twoway (rarea fup flow date if id== `x', color(gs13)) (line $outcomecg $outcome date if id == `x', lwidth(thick ..)  legend(off) ylabel(-4(4)36) xtitle(" ") ti("`s'") lcolor(blue) lpa("-") tline($treatment_GRC, lcolor(gray)) tlabel("$begin"(6)"$end"))
}
else {
	*get standard deviation (in percent)
	gen dif = $outcome - $outcomecg 
	sum dif if id==`x' & year<$treatment
	gen std=(r(sd))*2
	
		*present as "deviation from base year (percent)"
		*foreach var in $outcome $outcomecg {
		*	local f=`var'[1]
		*	replace `var' = (`var'-`f')
		*}

	
	*generate upper and lower bound
	gen fup=$outcomecg +std
	gen flow=$outcomecg -std
	*draw figure
	twoway (rarea fup flow date if id== `x', color(gs13)) (line $outcomecg $outcome date if id == `x', lwidth(thick ..)  legend(off) ylabel(-4(4)36) xtitle(" ") ti("`s'") lcolor(blue) lpa("-") tline($treatment, lcolor(gray)) tlabel("$begin"(6)"$end"))
}
graph save "${Fig}Auxi\SCM_gdp_`x'_Annual.gph", replace

use "${Data}LP_Annual", clear
}

graph combine "${Fig}Auxi\SCM_gdp_1_Annual.gph" "${Fig}Auxi\SCM_gdp_3_Annual.gph" "${Fig}Auxi\SCM_gdp_7_Annual.gph" "${Fig}Auxi\SCM_gdp_8_Annual.gph" "${Fig}Auxi\SCM_gdp_9_Annual.gph" "${Fig}Auxi\SCM_gdp_10_Annual.gph" "${Fig}Auxi\SCM_gdp_12_Annual.gph" "${Fig}Auxi\SCM_gdp_14_Annual.gph" "${Fig}Auxi\SCM_gdp_16_Annual.gph" "${Fig}Auxi\SCM_gdp_18_Annual.gph" "${Fig}Auxi\SCM_gdp_21_Annual.gph" "${Fig}Auxi\SCM_gdp_22_Annual.gph", l2title("Inflation rates (%)", size(small)) iscale(*0.8) imargin(tiny) com 
graph export "${Fig}SCM_Inflation_Annual.pdf", replace

}
else if ("$outcome" == "Bond") {
foreach x in 1 3 7 8 9 12 14 16 18 21 22 {

keep if id==`x'

local s = country 
label var $outcome "Country"
label var $outcomecg "Synthetic Country"
if (code=="GRC") {
	*get standard deviation (in percent)
	gen dif = $outcome - $outcomecg
	sum dif if id==`x' & year<$treatment_GRC
	gen std=(r(sd))*2
	

		*present as "deviation from base year (percent)"
		*foreach var in $outcome $outcomecg {
		*	local f=`var'[1]
		*	replace `var' = (`var'-`f')
		*}
		
	*generate upper and lower bound
	gen fup=$outcomecg +std
	gen flow=$outcomecg -std
	*draw figure
	twoway (rarea fup flow date if id== `x', color(gs13)) (line $outcomecg $outcome date if id == `x', lwidth(thick ..)  legend(off) ylabel(0(4)20) xtitle(" ") ti("`s'") lcolor(blue) lpa("-") tline($treatment_GRC, lcolor(gray)) tlabel("$begin"(6)"$end"))
}
else {
	*get standard deviation (in percent)
	gen dif = $outcome - $outcomecg 
	sum dif if id==`x' & year<$treatment
	gen std=(r(sd))*2
	
		*present as "deviation from base year (percent)"
		*foreach var in $outcome $outcomecg {
		*	local f=`var'[1]
		*	replace `var' = (`var'-`f')
		*}

	
	*generate upper and lower bound
	gen fup=$outcomecg +std
	gen flow=$outcomecg -std
	*draw figure
	twoway (rarea fup flow date if id== `x', color(gs13)) (line $outcomecg $outcome date if id == `x', lwidth(thick ..)  legend(off) ylabel(0(4)20) xtitle(" ") ti("`s'") lcolor(blue) lpa("-") tline($treatment, lcolor(gray)) tlabel("$begin"(6)"$end"))
}
graph save "${Fig}Auxi\SCM_gdp_`x'_Annual.gph", replace

use "${Data}LP_Annual", clear
}

graph combine "${Fig}Auxi\SCM_gdp_1_Annual.gph" "${Fig}Auxi\SCM_gdp_3_Annual.gph" "${Fig}Auxi\SCM_gdp_7_Annual.gph" "${Fig}Auxi\SCM_gdp_8_Annual.gph" "${Fig}Auxi\SCM_gdp_9_Annual.gph" "${Fig}Auxi\SCM_gdp_12_Annual.gph" "${Fig}Auxi\SCM_gdp_14_Annual.gph" "${Fig}Auxi\SCM_gdp_16_Annual.gph" "${Fig}Auxi\SCM_gdp_18_Annual.gph" "${Fig}Auxi\SCM_gdp_21_Annual.gph" "${Fig}Auxi\SCM_gdp_22_Annual.gph", l2title("Long-term interest rates (%)", size(small)) iscale(*0.8) imargin(tiny) com 
graph export "${Fig}SCM_Bond_Annual.pdf", replace
}
else if ("$outcome" == "rgdpnapc") {
foreach x in 1 3 7 8 9 10 12 14 16 18 21 {

keep if id==`x'

local s = country 
label var $outcome "Country"
label var $outcomecg "Synthetic Country"
if (code=="GRC") {
	
	*present as "thousands"
		foreach var in $outcome $outcomecg {
			*local f=`var'[1]
			replace `var' = `var'/1000
		}

		
	*get standard deviation (in percent)
	gen dif = $outcome - $outcomecg
	sum dif if id==`x' & year<$treatment_GRC
	gen std=(r(sd))*2

		
	*generate upper and lower bound
	gen fup=$outcomecg +std
	gen flow=$outcomecg -std
	*draw figure
	twoway (rarea fup flow date if id== `x', color(gs13)) (line $outcomecg $outcome date if id == `x', lwidth(thick ..)  legend(off) ylabel(10(10)60) xtitle(" ") ti("`s'") lcolor(blue) lpa("-") tline($treatment_GRC, lcolor(gray)) tlabel("$begin"(6)"$end"))
}
else {
	*present as "thousands"
		foreach var in $outcome $outcomecg {
			*local f=`var'[1]
			replace `var' = `var'/1000
		}

	*get standard deviation (in percent)
	gen dif = $outcome - $outcomecg 
	sum dif if id==`x' & year<$treatment
	gen std=(r(sd))*2
	

	*generate upper and lower bound
	gen fup=$outcomecg +std
	gen flow=$outcomecg -std
	*draw figure
	twoway (rarea fup flow date if id== `x', color(gs13)) (line $outcomecg $outcome date if id == `x', lwidth(thick ..)  legend(off) ylabel(10(10)60) xtitle(" ") ti("`s'") lcolor(blue) lpa("-") tline($treatment, lcolor(gray)) tlabel("$begin"(6)"$end"))
}
graph save "${Fig}Auxi\SCM_gdp_`x'_Annual.gph", replace

use "${Data}LP_Annual", clear
}

graph combine "${Fig}Auxi\SCM_gdp_1_Annual.gph" "${Fig}Auxi\SCM_gdp_3_Annual.gph" "${Fig}Auxi\SCM_gdp_7_Annual.gph" "${Fig}Auxi\SCM_gdp_8_Annual.gph" "${Fig}Auxi\SCM_gdp_9_Annual.gph" "${Fig}Auxi\SCM_gdp_10_Annual.gph" "${Fig}Auxi\SCM_gdp_12_Annual.gph" "${Fig}Auxi\SCM_gdp_14_Annual.gph" "${Fig}Auxi\SCM_gdp_16_Annual.gph" "${Fig}Auxi\SCM_gdp_18_Annual.gph" "${Fig}Auxi\SCM_gdp_21_Annual.gph", l2title("Real GDP per capita in thousands (2011 USD)", size(small)) iscale(*0.8) imargin(tiny) com 
graph export "${Fig}SCM_rgdpnapc_Annual.pdf", replace
}
else if ("$outcome" == "normgdp_ppp_s") {
foreach x in 1 3 7 8 9 10 12 14 16 18 21 22{

keep if id==`x'

local s = country 
label var $outcome "Country"
label var $outcomecg "Synthetic Country"
if (code=="GRC") {

		
	*get standard deviation (in percent)
	gen dif = $outcome - $outcomecg
	sum dif if id==`x' & year<$treatment_GRC
	gen std=(r(sd))*2*100

			*present as "deviation from base year (percent)"
		foreach var in $outcome $outcomecg {
			local f=`var'[1]
			replace `var' = (`var'-`f')*100
		}	
		
	*generate upper and lower bound
	gen fup=$outcomecg +std
	gen flow=$outcomecg -std
	*draw figure
	twoway (rarea fup flow date if id== `x', color(gs13)) (line $outcomecg $outcome date if id == `x', lwidth(thick ..)  legend(off) ylabel(0(100)600) xtitle(" ") ti("`s'") lcolor(blue) lpa("-") tline($treatment_GRC, lcolor(gray)) tlabel("$begin"(6)"$end"))
}
else {

	*get standard deviation (in percent)
	gen dif = $outcome - $outcomecg 
	sum dif if id==`x' & year<$treatment
	gen std=(r(sd))*2*100
	
		*present as "deviation from base year (percent)"
		foreach var in $outcome $outcomecg {
			local f=`var'[1]
			replace `var' = (`var'-`f')*100
		}
	
	*generate upper and lower bound
	gen fup=$outcomecg +std
	gen flow=$outcomecg -std
	*draw figure
	twoway (rarea fup flow date if id== `x', color(gs13)) (line $outcomecg $outcome date if id == `x', lwidth(thick ..)  legend(off) ylabel(0(100)600) xtitle(" ") ti("`s'") lcolor(blue) lpa("-") tline($treatment, lcolor(gray)) tlabel("$begin"(6)"$end"))
}
graph save "${Fig}Auxi\SCM_gdp_`x'_Annual.gph", replace

use "${Data}LP_Annual", clear
}

graph combine "${Fig}Auxi\SCM_gdp_1_Annual.gph" "${Fig}Auxi\SCM_gdp_3_Annual.gph" "${Fig}Auxi\SCM_gdp_7_Annual.gph" "${Fig}Auxi\SCM_gdp_8_Annual.gph" "${Fig}Auxi\SCM_gdp_9_Annual.gph" "${Fig}Auxi\SCM_gdp_10_Annual.gph" "${Fig}Auxi\SCM_gdp_12_Annual.gph" "${Fig}Auxi\SCM_gdp_14_Annual.gph" "${Fig}Auxi\SCM_gdp_16_Annual.gph" "${Fig}Auxi\SCM_gdp_18_Annual.gph" "${Fig}Auxi\SCM_gdp_21_Annual.gph" "${Fig}Auxi\SCM_gdp_22_Annual.gph", l2title("Real GDP, 2011 USD, PPP adjusted, deviation from 1970 (%)", size(small)) iscale(*0.8) imargin(tiny) com 
graph export "${Fig}SCM_normgdp_ppp_s_Annual.pdf", replace
}

else if ("$outcome" == "normgdppc_s") {
foreach x in 1 3 7 8 9 10 12 14 16 18 21 22{

keep if id==`x'

local s = country 
label var $outcome "Country"
label var $outcomecg "Synthetic Country"
if (code=="GRC") {

		
	*get standard deviation (in percent)
	gen dif = $outcome - $outcomecg
	sum dif if id==`x' & year<$treatment_GRC
	gen std=(r(sd))*2*100

			*present as "deviation from base year (percent)"
		foreach var in $outcome $outcomecg {
			local f=`var'[1]
			replace `var' = (`var'-`f')*100
		}	
		
	*generate upper and lower bound
	gen fup=$outcomecg +std
	gen flow=$outcomecg -std
	*draw figure
	twoway (rarea fup flow date if id== `x', color(gs13)) (line $outcomecg $outcome date if id == `x', lwidth(thick ..)  legend(off) ylabel(0(100)400) xtitle(" ") ti("`s'") lcolor(blue) lpa("-") tline($treatment_GRC, lcolor(gray)) tlabel("$begin"(6)"$end"))
}
else {

	*get standard deviation (in percent)
	gen dif = $outcome - $outcomecg 
	sum dif if id==`x' & year<$treatment
	gen std=(r(sd))*2*100
	
		*present as "deviation from base year (percent)"
		foreach var in $outcome $outcomecg {
			local f=`var'[1]
			replace `var' = (`var'-`f')*100
		}
	
	*generate upper and lower bound
	gen fup=$outcomecg +std
	gen flow=$outcomecg -std
	*draw figure
	twoway (rarea fup flow date if id== `x', color(gs13)) (line $outcomecg $outcome date if id == `x', lwidth(thick ..)  legend(off) ylabel(0(100)400) xtitle(" ") ti("`s'") lcolor(blue) lpa("-") tline($treatment, lcolor(gray)) tlabel("$begin"(6)"$end"))
}
graph save "${Fig}Auxi\SCM_gdp_`x'_Annual.gph", replace

use "${Data}LP_Annual", clear
}

graph combine "${Fig}Auxi\SCM_gdp_1_Annual.gph" "${Fig}Auxi\SCM_gdp_3_Annual.gph" "${Fig}Auxi\SCM_gdp_7_Annual.gph" "${Fig}Auxi\SCM_gdp_8_Annual.gph" "${Fig}Auxi\SCM_gdp_9_Annual.gph" "${Fig}Auxi\SCM_gdp_10_Annual.gph" "${Fig}Auxi\SCM_gdp_12_Annual.gph" "${Fig}Auxi\SCM_gdp_14_Annual.gph" "${Fig}Auxi\SCM_gdp_16_Annual.gph" "${Fig}Auxi\SCM_gdp_18_Annual.gph" "${Fig}Auxi\SCM_gdp_21_Annual.gph" "${Fig}Auxi\SCM_gdp_22_Annual.gph", l2title("Real GDP per capita, deviation from 1970 (%)", size(small)) iscale(*0.8) imargin(tiny) com 
graph export "${Fig}SCM_normgdppc_s_Annual.pdf", replace
}



*Baseline variable
else if ("$outcome" == "normgdp_s") {
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
	twoway (rarea fup flow date if id== `x', color(gs13)) (line $outcomecg $outcome date if id == `x', lwidth(thick ..)  legend(off) ylabel(0(100)500) xtitle(" ") ti("`s'") lcolor(blue) lpa("-") tline($treatment_GRC, lcolor(gray)) tline(1999, lpattern("-") lcolor(gray)) tlabel("$begin"(6)"$end"))
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
	twoway (rarea fup flow date if id== `x', color(gs13)) (line $outcomecg $outcome date if id == `x', lwidth(thick ..)  legend(off) ylabel(0(100)500) xtitle(" ") ti("`s'") lcolor(blue) lpa("-") tline($treatment, lcolor(gray)) tline(1999, lpattern("-") lcolor(gray)) tlabel("$begin"(6)"$end"))
}
graph save "${Fig}Auxi\SCM_gdp_`x'_Annual.gph", replace

use "${Data}LP_Annual", clear
}
local ty = $treatment_choice
graph combine "${Fig}Auxi\SCM_gdp_1_Annual.gph" "${Fig}Auxi\SCM_gdp_3_Annual.gph" "${Fig}Auxi\SCM_gdp_7_Annual.gph" "${Fig}Auxi\SCM_gdp_8_Annual.gph" "${Fig}Auxi\SCM_gdp_9_Annual.gph" "${Fig}Auxi\SCM_gdp_10_Annual.gph" "${Fig}Auxi\SCM_gdp_12_Annual.gph" "${Fig}Auxi\SCM_gdp_14_Annual.gph" "${Fig}Auxi\SCM_gdp_16_Annual.gph" "${Fig}Auxi\SCM_gdp_18_Annual.gph" "${Fig}Auxi\SCM_gdp_21_Annual.gph" "${Fig}Auxi\SCM_gdp_22_Annual.gph", l2title("deviation from $begin (percent)", size(small)) iscale(*0.8) imargin(tiny) com 
graph export "${Fig}SCM_gdp_Rob_`ty'_Annual.pdf", replace
}
else if ("$outcome" == "normgnipc_s") {
foreach x in $countries {

keep if id==`x'

local s = country 
label var $outcome "Country"
label var $outcomecg "Synthetic Country"
if (code=="GRC") {
		*present as "deviation from base year (percent)"
		foreach var in $outcome $outcomecg {
			local f=`var'[1]
			replace `var' = (`var'-`f')*100
		}
		
	*get standard deviation (in percent)
	gen dif = $outcome - $outcomecg
	sum dif if id==`x' & year<$treatment_GRC
	gen std=(r(sd))*2
	


		
	*generate upper and lower bound
	gen fup=$outcomecg +std
	gen flow=$outcomecg -std
	*draw figure
	twoway (rarea fup flow date if id== `x', color(gs13)) (line $outcomecg $outcome date if id == `x', lwidth(thick ..)  legend(off) ylabel(0(10)50) xtitle(" ") ti("`s'") lcolor(blue) lpa("-") tline($treatment_GRC, lcolor(gray)) tlabel("$begin"(6)"$end"))
}
else {
		*present as "deviation from base year (percent)"
		foreach var in $outcome $outcomecg {
			local f=`var'[1]
			replace `var' = (`var'-`f')*100
		}

	*get standard deviation (in percent)
	gen dif = $outcome - $outcomecg 
	sum dif if id==`x' & year<$treatment
	gen std=(r(sd))*2
	
	
	*generate upper and lower bound
	gen fup=$outcomecg +std
	gen flow=$outcomecg -std
	*draw figure
	twoway (rarea fup flow date if id== `x', color(gs13)) (line $outcomecg $outcome date if id == `x', lwidth(thick ..)  legend(off) ylabel(0(10)50) xtitle(" ") ti("`s'") lcolor(blue) lpa("-") tline($treatment, lcolor(gray)) tlabel("$begin"(6)"$end"))
}
graph save "${Fig}Auxi\SCM_gdp_`x'_Annual.gph", replace

use "${Data}LP_Annual", clear
}

graph combine "${Fig}Auxi\SCM_gdp_1_Annual.gph" "${Fig}Auxi\SCM_gdp_3_Annual.gph" "${Fig}Auxi\SCM_gdp_7_Annual.gph" "${Fig}Auxi\SCM_gdp_8_Annual.gph" "${Fig}Auxi\SCM_gdp_9_Annual.gph" "${Fig}Auxi\SCM_gdp_10_Annual.gph" "${Fig}Auxi\SCM_gdp_12_Annual.gph" "${Fig}Auxi\SCM_gdp_16_Annual.gph" "${Fig}Auxi\SCM_gdp_18_Annual.gph" "${Fig}Auxi\SCM_gdp_21_Annual.gph" "${Fig}Auxi\SCM_gdp_22_Annual.gph", l2title("GNI per capita deviation from $begin (percent)", size(small)) iscale(*0.8) imargin(tiny) com 
graph export "${Fig}SCM_normgnipc_Annual.pdf", replace
}
else if ("$outcome" == "GNI") {
foreach x in $countries {

keep if id==`x'

local s = country 
label var $outcome "Country"
label var $outcomecg "Synthetic Country"
if (code=="GRC") {
		*present as "deviation from base year (percent)"
		foreach var in $outcome $outcomecg {
			*local f=`var'[1]
			replace `var' = (`var')/1000
		}
		
	*get standard deviation (in percent)
	gen dif = $outcome - $outcomecg
	sum dif if id==`x' & year<$treatment_GRC
	gen std=(r(sd))*2
	


		
	*generate upper and lower bound
	gen fup=$outcomecg +std
	gen flow=$outcomecg -std
	*draw figure
	twoway (rarea fup flow date if id== `x', color(gs13)) (line $outcomecg $outcome date if id == `x', lwidth(thick ..)  legend(off) ylabel(0(10)50) xtitle(" ") ti("`s'") lcolor(blue) lpa("-") tline($treatment_GRC, lcolor(gray)) tlabel("$begin"(6)"$end"))
}
else {
		*present as "deviation from base year (percent)"
		foreach var in $outcome $outcomecg {
			replace `var' = (`var'/1000)
		}

	*get standard deviation (in percent)
	gen dif = $outcome - $outcomecg 
	sum dif if id==`x' & year<$treatment
	gen std=(r(sd))*2
	
	
	*generate upper and lower bound
	gen fup=$outcomecg +std
	gen flow=$outcomecg -std
	*draw figure
	twoway (rarea fup flow date if id== `x', color(gs13)) (line $outcomecg $outcome date if id == `x', lwidth(thick ..)  legend(off) ylabel(0(10)50) xtitle(" ") ti("`s'") lcolor(blue) lpa("-") tline($treatment, lcolor(gray)) tlabel("$begin"(6)"$end"))
}
graph save "${Fig}Auxi\SCM_gdp_`x'_Annual.gph", replace

use "${Data}LP_Annual", clear
}

graph combine "${Fig}Auxi\SCM_gdp_1_Annual.gph" "${Fig}Auxi\SCM_gdp_3_Annual.gph" "${Fig}Auxi\SCM_gdp_7_Annual.gph" "${Fig}Auxi\SCM_gdp_8_Annual.gph" "${Fig}Auxi\SCM_gdp_9_Annual.gph" "${Fig}Auxi\SCM_gdp_12_Annual.gph" "${Fig}Auxi\SCM_gdp_14_Annual.gph" "${Fig}Auxi\SCM_gdp_18_Annual.gph" "${Fig}Auxi\SCM_gdp_21_Annual.gph" "${Fig}Auxi\SCM_gdp_22_Annual.gph", l2title("GNI per capita deviation from $begin (percent)", size(small)) iscale(*0.8) imargin(tiny) com 
graph export "${Fig}SCM_gni_Annual.pdf", replace

keep if year ==2007 & mu == 1
gen GNIdif = GNI - GNIcg

list GNIdif country
}
