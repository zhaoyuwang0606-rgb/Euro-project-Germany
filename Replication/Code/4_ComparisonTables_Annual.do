 		*** Comparison Tables  - Specification Annual ***

/*
The file ComparisonTables.do constructs comparison tables which summarize each 
countries and their doppelganger for the variables of interest before the 
Monetary Union acession. It is useful to observe whether the doppelganger 
can mimic the country well in the pretreatement period across all variables. 
*/


clear all
set more off
graph drop _all
use "${Data}LP_Annual", clear


*row size
local r=2

*column size
local ce =(1+$number_covariates)
local c= `ce'*$number_countries

**Generate the matrix to store the values 
matrix comp = J(`c',`r',0)

**some trick to  get empty cells in the rows with only the countries names	
forvalues a=1/`r'{
	forval b = 1(`ce')`c'{
		matrix comp[`b',`a']= .
	}
}

foreach x in $countries {
use "${Data}LP_Annual", clear

set more off
keep if donor==1 | id==`x'
replace id=1 if id == `x'
xtset id date


foreach vari in $countries_name{
gen `vari'=0
label var `vari' "`vari'"
}


*Matching of covariates Table
matrix colnames comp = "Country" "Synthetic" 						
matrix rownames comp = "Austria" $covariates "Belgium" $covariates "Finland" ///
$covariates "France" $covariates "Germany" $covariates "Greece" $covariates ///
"Ireland" $covariates "Italy" $covariates "Luxembourg" $covariates ///
"Netherlands" $covariates "Portugal" $covariates "Spain" $covariates


*computing the mean for each variable for the country - correct this to capture only the pre-treatment period according to the treatment date - different for Greece
local c = 1
foreach var in $covariates{
	replace `var' = `var'*100
	if "`var'" == "inflation" {
		replace inflation = inflation/100
	}
	sum `var' if id==1 & year <= 1998
	matrix comp[`r',`c']=r(mean)
	local r = `r' + 1
}

*computing the mean for each variable for the synthetic country
local c = 2
* come back #number of covariates +1
local r = `r' - $number_covariates
foreach var in $covariatescg{
	replace `var' = `var'*100
	if "`var'" == "inflationcg" {
		replace inflationcg = inflationcg/100
	}
	sum `var' if id==1 & year <= 1998
	matrix comp[`r',`c']=r(mean)
	local r = `r' + 1
}

local r = `r' + 1
}

label var csh_c "Share of Priv. Consumption"
label var inflation "Inflation Rate"
*save the table as tex document
outtable using "${Tab}Comp_Annual", mat(comp) replace label nobox asis center nodots longtable f(%9.2f) caption("Predictors' means for each country")
