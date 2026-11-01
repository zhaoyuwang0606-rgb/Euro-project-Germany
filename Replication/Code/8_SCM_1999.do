   			*** Synthetic Control Method - Specification Annual ***
 
/*
The file SCM_Annual.do runs the synthetic control method (SCM) for each of the treated 
countries: Austria, Belgium, Finland, France, Germany, Ireland, Italy, 
Luxembourg, The Netherlands, Portugal and Spain.

First, it runs the synth package to mimic each countries' normalized 
log of real GDP per capita using as predictors the log real GPD per capita 
itself and its lags, the shares of its components: private and government 
consumption, investment, and net exports, the employment share and the labor
productivity growth. To do that one needs the synth package easily obtained 
by running: ssc install synth

Second, we construct series for each doppelgangers' log real GDP per capita 
by using the weights computed by the SCM. 

Third, we construct a matrix to save the weights attributed to each 
OECD and non-eurozone countries. 

Finally, we save the weights as a tex document: Weights, and the series in a new
.dta file for each country which we then aggregate in the aggregation do file.
*/


clear
set more off

* run paths do file
do Paths.do


* Decide on Specification (Annual or Quarterly) and whether 
global SPECIFICATION Annual
*global COMPONENTS On

*upload correspondent global variables
do Master_EMU.do



********************************************************************************
                                      *SCM*
********************************************************************************

********************************************************************************
                                      *SCM*
********************************************************************************


**Construct matrix for weights used for SCG construction
local a = 1
local p= $number_donor - $remove_nr
matrix weights = J(`p' ,$number_countries ,0)

*still needs to be automatized
matrix rownames weights = $donor_names
matrix colnames weights = "Austria"  "Belgium" "Finland" "France" "Germany" ///
"Greece" "Ireland" "Italy" "Luxembourg" "Netherlands" "Portugal" "Spain"

 
foreach x in $countries {

*___________________1. Run the  Synthetic Control _____________________________* allopt

use "${Data}Analysis_Annual", clear

set more off
replace donor=0 if country=="$remove_1"
replace donor=0 if country=="$remove_2"
replace donor=0 if country=="$remove_3"
keep if id==`x' | donor==1
replace id=1 if id == `x'
xtset id date
xtset id h


if (code=="GRC") {
	local treatment_h = ($treatment_GRC-$begin)+1
	local max = `treatment_h'-1
	synth $outcome $covariates $outcome $outcome(2(1)`max'), trunit(1) trperiod(`treatment_h') nested allopt
}
else{
	local treatment_h = ($treatment-$begin)+1
	local max = `treatment_h'-1
	synth $outcome $covariates $outcome $outcome(2(1)`max'), trunit(1) trperiod(`treatment_h') nested allopt
}


*______________________2. Obtain economic series for SCG_______________________*


foreach var in $outcome $covariates {
	gen `var'cg=.
}   

*Get weights from SCM
matrix w = e(W_weights)



*For each pair id-weight compute contribution to SCG's variables of interest
foreach var in $outcome $covariates{
	forval c = 1/`p' {
		replace `var'cg = w[`c',2]*`var' if id == w[`c',1]
	}
}

/*
Sum to compute SCG variables of interest - trick to easily compute the 
weighted average - [_n+max{h}] only times the amount of donor countries
*/

if ("$begin"=="1970") {

	if ("$remove_nr"=="0") {
		foreach var in $outcome $covariates {
			replace `var'cg = `var'cg[_n+29] + `var'cg[_n+58] + `var'cg[_n+87] + `var'cg[_n+116] + `var'cg[_n+145] + `var'cg[_n+174] + `var'cg[_n+203] +`var'cg[_n+232] + `var'cg[_n+261] + `var'cg[_n+290] +`var'cg[_n+319] + `var'cg[_n+348] + `var'cg[_n+377] + `var'cg[_n+406]
		}
	}
	else if ("$remove_nr"=="1") {
		foreach var in $outcome $covariates {
			replace `var'cg = `var'cg[_n+38] + `var'cg[_n+76] + `var'cg[_n+114] + `var'cg[_n+152] + `var'cg[_n+190] + `var'cg[_n+228] + `var'cg[_n+266] +`var'cg[_n+304] + `var'cg[_n+342] + `var'cg[_n+380] +`var'cg[_n+418] + `var'cg[_n+456] + `var'cg[_n+494]
		}	
	}
	else if ("$remove_nr"=="2") {
		foreach var in $outcome $covariates {
			replace `var'cg = `var'cg[_n+38] + `var'cg[_n+76] + `var'cg[_n+114] + `var'cg[_n+152] + `var'cg[_n+190] + `var'cg[_n+228] + `var'cg[_n+266] +`var'cg[_n+304] + `var'cg[_n+342] + `var'cg[_n+380] +`var'cg[_n+418] + `var'cg[_n+456]
		}
	}
	else if ("$remove_nr"=="3") {
		foreach var in $outcome $covariates {
			replace `var'cg = `var'cg[_n+38] + `var'cg[_n+76] + `var'cg[_n+114] + `var'cg[_n+152] + `var'cg[_n+190] + `var'cg[_n+228] + `var'cg[_n+266] +`var'cg[_n+304] + `var'cg[_n+342] + `var'cg[_n+380] +`var'cg[_n+418]
		}
	}
}
 
	
*______________3. Contruct matrix with the weights used in the SCM_____________*

forvalues c=1/`p' {
	local flag = 100*(w[`c',2])
	matrix weights[`c',`a']= `flag' 
}
local a=`a'+1

//Save the dataset with the contructed SCM series
local c = country
keep if id==1
keep code date $outcomecg $covariatescg 
save "${Data}Auxi\SCM_`x'_Annual", replace
}


** Saving the weights table in a tex document - still needs to be automatized
foreach vari in $donor_names{
gen `vari'=0
label var `vari' "`vari'"
}


outtable using "${Tab}\Weights_Annual_1993", mat(weights) label replace nobox asis center f(%9.1f) caption("Weights used to contruct each doppelganger - Annual Specification")
