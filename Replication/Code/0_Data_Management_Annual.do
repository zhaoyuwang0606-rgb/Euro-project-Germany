 	  *** Data Management - Specification Annual ***
 
/*
Stata15 file
The file Data_Management.do is used to drop countries and variables 
which are not used, rescale/generate the outcome variable and predictors,
generating the donor pools.
It uses a .dta file compiled with information from the Penn World Tables 9.1
and the World Bank. 
*/
 
clear
set more off


********************************************************************************
* 								DATA CLEANING
********************************************************************************
* PENN TABLES *
import excel "$Data\Source_Annual\PennTable\PennTable.xlsx", sheet("Data") firstrow clear
rename countrycode c
merge 1:1 year c using "$Data\Source_Annual\World Bank\WDI.dta"
*keep if c=="AUS" | c=="AUT" | c=="BEL" | c=="CAN" | c=="CZE" | c=="CHL" | c=="DNK" | c=="EST" | c=="FIN" | c=="FRA" | c=="DEU" | c=="GRC" | c=="HUN" | c=="ISL" | c=="IRL" | c=="ITA" | c=="ISR" | c=="JPN" | c=="KOR" | c=="LVA" | c=="LTU" | c=="LUX" | c=="MEX" | c=="NLD" | c=="NZL" | c=="NOR" | c=="POL" | c=="PRT" | c=="SVK" | c=="SVN" | c=="SWE" | c=="ESP" | c=="CHE" | c=="TUR" | c=="GBR" | c=="USA" | c=="ARG" | c=="BRA" | c=="BGR" | c=="CPV" | c=="COL" | c=="CRI" | c=="HRV" | c=="CYP" | c=="IND" | c=="IDN" | c=="MLT" | c=="MKD" | c=="PER" | c=="ROU" | c=="RUS" | c=="ZAF"
keep if c=="AUS" | c=="AUT" | c=="BEL" | c=="CAN" | c=="CHL" | c=="DNK" | c=="FIN" | c=="FRA" | c=="DEU" | c=="GRC" | c=="ISL" | c=="IRL" | c=="ITA" | c=="ISR" | c=="JPN" | c=="KOR" | c=="LUX" | c=="MEX" | c=="NLD" | c=="NZL" | c=="NOR" | c=="PRT" | c=="SWE" | c=="ESP" | c=="CHE" | c=="GBR" | c=="USA" 
keep c country currency_unit year rgdpe rgdpo pop emp avh ccon cda cgdpe cn ck ctfp rgdpna rconna rdana rnna rkna rtfpna csh_c csh_i csh_g csh_x csh_m
keep if year>1969
save "$Data\Source_Annual\Clean_PennTable.dta", replace


* World Bank *
import excel "$Data\Source_Annual\World Bank\pcGNI_Real.xls", sheet("Sheet1") firstrow clear
reshape long YR , i(CountryCode CountryName) j(year)
rename YR GNI
label var GNI " Gross national income per capita (constant 2015 USD)"
rename CountryCode c
rename CountryName country
replace country = "Republic of Korea" if country=="Korea, Rep."
save "$Data\Source_Annual\World_Bank_GNI.dta", replace

import excel "$Data\Source_Annual\World Bank\GNI.xls", sheet("Sheet1") firstrow clear
reshape long YR , i(CountryCode CountryName) j(year)
rename YR GNIc
label var GNIc " Gross national income per capita (current USD)"
rename CountryCode c
rename CountryName country
replace country = "Republic of Korea" if country=="Korea, Rep."
save "$Data\Source_Annual\World_Bank_GNIc.dta", replace


import excel "$Data\Source_Annual\World Bank\WorldBank_GDPshares.xlsx", sheet("Sheet1") firstrow clear
drop SeriesCode // SeriesName	CountryName
reshape long YR , i(SeriesName CountryCode CountryName) j(year)
replace SeriesName = "wb_c" if SeriesName == "Final consumption expenditure (% of GDP)"
replace SeriesName = "wb_g" if SeriesName == "General government final consumption expenditure (% of GDP)"
replace SeriesName = "wb_i" if SeriesName == "Gross fixed capital formation (% of GDP)"
replace SeriesName = "wb_x" if SeriesName == "Exports of goods and services (% of GDP)"
replace SeriesName = "wb_m" if SeriesName == "Imports of goods and services (% of GDP)"
reshape wide YR, i(CountryCode CountryName year) j(SeriesName) string
foreach var in wb_c wb_g wb_i wb_x wb_m{
	rename YR`var' `var'
}
rename CountryCode c
rename CountryName country
replace country = "Republic of Korea" if country=="Korea, Rep."
drop if year == 2018
save "$Data\Source_Annual\World_Bank.dta", replace






* merge both datasets
merge 1:1 year country using "$Data\Source_Annual\World_Bank_GNI.dta"
drop if _merge == 2
drop _merge

merge 1:1 year country using "$Data\Source_Annual\World_Bank_GNIc.dta"
drop if _merge == 2
drop _merge


merge 1:1 year country using "$Data\Source_Annual\Clean_PennTable.dta"
drop _merge

merge 1:1 year country using "$Data\Source_Annual\Arvai_Gabriel_inflation_bonds.dta"
drop if _merge == 2
drop _merge



rename c code
replace country ="Korea"  if country=="Republic of Korea"
save "${Data}Data_Management_Annual_WB", replace

*complete data gaps for Germany
replace GNIc = 3600 if country == "Germany" & GNIc==.
replace GNIc = 2070 if country == "Ireland" & GNIc==.

* complete data gaps for Bond information for Austria, Korea, and Luxembourg
replace Bond = 7.70666666575 if Bond == . & country == "Austria"
replace Bond = 16.80555555556 if Bond == . & country == "Korea"
replace Bond = 6.583333332333 if Bond == . & country == "Luxembourg"

* correct this shotcut to allow exercise for Bond to run, currently only exclude Greece from the final picture
replace Bond = -999 if Bond==.

*use "${Data}Data_Management_Annual_WB_pre_revision", clear

			*** Removing not needed data and countries ***

keep code country year rgdpe rgdpna rconna rdana rnna emp pop csh_* wb_* inflation CPI Bills Bond GNI GNIc

*Keep OECD countries for which we have all the data and did not joined
* the European MU between 2002 - 2008, i.e, Slovenia(2007) and Cyprus(2008)
keep if code == "AUS" | code=="AUT" | code=="BEL" | code=="CAN" | code=="CHL" | code=="DNK" | code=="FIN" | code=="FRA" | code=="DEU" | code=="GRC" | code=="ISL" | code=="IRL" | code=="ISR" | code=="ITA" | code=="KOR" | code=="LUX" | code=="MEX" | code=="NLD" | code=="NZL" | code=="NOR" | code=="PRT" | code=="ESP" | code=="SWE" | code=="CHE" | code=="GBR" | code=="USA"


*Specification Annual
global SPECIFICATION Annual
do Master_EMU.do

					*** Date and ID ***
				
*Generating quarter date
gen date = year
format date %ty
				
*Declare as panel dataset using xtset panelvar timevar
sort country date				
egen id = group(country)
xtset id date

*Strategic change for synth package
replace id=1 if country == "Austria"
replace id=2 if country == "Australia"


				*** Generating variables ***
				

*extend backwards GNI using GNIc growth rates
order GNI* year code
bys id: gen time = 2008-year
xtset id time
replace GNI = l.GNI*(1+d.GNIc/l.GNIc) if GNI==.
drop time GNIc
* correct this shotcut to allow exercise for Bond to run, currently only exclude Greece from the final picture
replace GNI=-99999 if GNI==.

xtset id date				
				
*Generate Monetary Union dummy
gen mu=0
foreach x in $countries{
	replace mu=1 if id==`x'	
}				

*Construct predictors for SCG: gdp components share, employment share and labor prod. growth
xtset id date
/*
gen csh_c = rconna/rgdpna
gen csh_i = (rdana-rconna)/rgdpna
gen csh_nx = (rgdpna-rdana)/rgdpna
*/

gen csh_emp = emp/pop
gen csh_prod = ln(rgdpna/l.rgdpna)-ln(emp/l.emp)


replace csh_c = (wb_c-wb_g)/100
replace csh_i = wb_i/100
replace csh_g = wb_g/100
replace csh_x = wb_x/100
replace csh_m = wb_m/100

*Create donor groups according to data availability
*Donor pool for Specification Annual (with all predictors since 1970) - 30 donors
gen donor=0
foreach x in $donor_countries{
	replace donor=1 if id==`x'	
}

*Removing periods after the financial crisis
drop if year > $end | year < $begin

*Construct log normalized real gdp per capita
*get gdp per capita
gen rgdpnapc=rgdpna/pop


* trick to delete just for the sake of running the referee exercise
*replace rgdpnapc = rgdpnapc/2 if country =="Luxembourg"

*get normalized gdp 
xtset id date
bys id: gen normgdp_s = rgdpna/rgdpna[1]
bys id: gen normgdppc_s = rgdpnapc/rgdpnapc[1]
bys id: gen normgdp_ppp_s = rgdpe/rgdpe[1]
bys id: gen normgnipc_s = GNI/GNI[1]

*get growth rate normalized gdp per capita (interpretation: growth of the real gdp per capita relative to 1970)
replace normgdp_s=normgdp_s-1
replace normgdppc_s=normgdppc_s-1
replace normgdp_ppp_s=normgdp_ppp_s-1
replace normgnipc_s = normgnipc_s-1

*construct linear trend 
xtset id date
bys id: gen h = _n

*drop due to stagnation
drop if code=="JPN"

**labeling the variables
label var csh_g "Share of Gov. Consumption"
label var csh_c "Share of Consumption"
label var csh_i  "Share of Investment"
label var csh_m "Share of Imports"
label var csh_x "Share of Exports"
*label var csh_nx "Share of Net Exports"
label var csh_emp  "Employment Share"
label var csh_prod  "Labor productivity growth"

 
*gen ids for in-space placebo
gen id_placebo=.
local j=1
foreach x in $donor_countries_R{
	replace id_placebo=`j' if id==`x'
	local j = `j'+1
}
foreach x in $countries{
	replace id_placebo=`j' if id==`x'
	local j = `j'+1
}
       
*Save file
save "${Data}Analysis_Annual.dta", replace
saveold "${Data}Analysis_Annual_12_$end_choice.dta", version(12) replace
