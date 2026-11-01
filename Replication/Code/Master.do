* Master File that runs all codes
/*

Results not directly produced in Stata
- Figure 3: In-space placebo using R Code file "Placebo_Graph_Annual"
- Figure 4 and Table A.4 are ad-hoc computations using the output from the previous code

*/

********************************************************************************
* ids of treated units: 1 3 7 8 9 10 12 14 17 19 22 23
* Austria: 1
* Belgium: 3
* Finland: 7
* France: 8
* Germany: 9
* Greece: 10 - take into account that Greece has a different treatment date 
* Ireland: 12
* Italy: 14
* Luxembourg : 16 - leave it out of the analysis when using (non-normalized) gni or real gdp per capita - because its high level can't be matched.
* Netherlands : 18
* Portugal : 21
* Spain : 22

global countries_name Austria Belgium Finland France Germany Greece Ireland Italy Luxembourg Netherlands Portugal Spain
global countries_code AUT BEL FIN FRA DEU GRC IRL ITA LUX NLD PRT ESP
global countries 1 3 7 8 9 10 12 14 16 18 21 22
global number_countries = 12

*choose outcome variable (baseline: normgdp_s)
global outcome normgdp_s


*choose predictors to the following list and tell stata how many are you using (e.g. inflation)
global covariates csh_c csh_i csh_g csh_m csh_x csh_emp csh_prod inflation
global number_covariates = 8

*change also for doppelganger series the choice of the outcome and covariates (ending with "cg" for control group)
global outcomecg normgdp_scg
global covariatescg csh_ccg csh_icg csh_gcg csh_xcg csh_mcg csh_empcg csh_prodcg inflationcg


*treatment dates
global treatment = 1999
global treatment_GRC = 2001

*starting and ending years of the sample
global begin = 1970
global end = 2007

* Default Globals for rob checks changes in donor pool
global remove_1 
global remove_2 
global remove_3 
global remove_nr = 0

*Donor countries
global donor_names Australia Canada Chile Denmark Iceland Israel Korea Mexico NewZealand Norway Sweden Switzerland UnitedKingdom UnitedStates
global donor_countries 2 4 5 6 11 13 15 17 19 20 23 24 25 26
global number_donor = 14


********************************************************************************





* Baseline Results
* Figure 1, Figure 7 (Weights_Decomposition.tex)
* Table 1 (Weights_Decomposition.tex), 
* Table A.2 (Comp_Annual.tex); Table A.3 (Weights_Annual.tex), 
* Table A.6 (Weights_Decomposition.tex); Table A.7 (Net_Exports__Decomposition.tex)
* 
/*
do Paths
do 0_Data_Management_Annual
do 1_SCM_Annual
do 2_Aggregation_Annual

* Table
do 3_Descriptives_Annual

* Table 
do 4_ComparisonTables_Annual

* Figures
do 5_Graphs_Annual

*/


* In-time placebo test
* Figure 2 (and any other where the end of sample is 2007 and a placebo treatment date 
* is between 1972 and 2006)
/*
* Run in time placebo
global treatment = 1992
global treatment_GRC = 1992

do Paths
do 0_Data_Management_Annual
do 1_SCM_Annual
do 2_Aggregation_Annual
do 6_Graphs_Other
*/




* Leave countries out of the donor pool placebo

/*
For performing robustness tests change here: remove the country you want to test 
from the list donor_names and write it into the remove global up to 3 countries
including spaces (e.g. United Kingdom, or Chile) 
*/

/*
global donor_names Australia Canada Chile Iceland Israel Korea Mexico NewZealand Norway Switzerland UnitedStates
global remove_1 Denmark
global remove_2 Sweden
global remove_3 United Kingdom
global remove_nr = 3

do Paths
do 0_Data_Management_Annual
do 1_SCM_Annual
do 2_Aggregation_Annual
do 6_Graphs_Other
*/




/*
* Run rob check Referee 1 (normgdp_ppp_s, normgdppc_s, Bond, inflation)
* when running inflation (remove it from predictors)
global outcome normgdp_ppp_s
global outcomecg normgdp_ppp_scg
global countries_choice 1 3 7 8 9 10 12 14 16 18 21 22

do Paths
do 0_Data_Management_Annual
do 1_SCM_Annual
do 2_Aggregation_Annual
do 6_Graphs_Other
*/


* Run rob check for only real per capita gni (GNI) - does not work for Greece (no data) and Luxembourg 
global outcome GNI
global outcomecg GNIcg
global countries 1 3 7 8 9 12 14 18 21 22

do Paths
do 0_Data_Management_Annual
do 1_SCM_Annual
do 2_Aggregation_Annual
do 6_Graphs_Other
*/


/*
* Run rob for only real gdp per capita - does not work for Spain
global outcome rgdpnapc
global outcomecg rgdpnapccg
global countries_choice 1 3 7 8 9 10 12 14 16 18 21

do Paths
do 0_Data_Management_Annual
do 1_SCM_Annual
do 2_Aggregation_Annual
do 6_Graphs_Other
*/


/*

* Table annual avg GDP growth by country (3 and 10 years before and after Great Recession - 2008)
* compute standard deviation of avg growth rates for both EMU and donor countries
* document heterogeneity of 

do Paths
global end = 2018
do 0_Data_Management_Annual
* Table
preserve
use "${Data}Analysis_Annual", clear
keep if inrange(year,1999,2016)
gen gdpg = (rgdpna-L8.rgdpna)/L8.rgdpna/8*100
keep if year == 2007 | year == 2016
bys mu year: tabstat gdpg, stat(mean sd min max)
restore

*/


/*
* Referee Check - Maastricht Treaty decomposition - change global end to 1999
global treatment_choice = 1992
global treatment_GRC_choice = 1992
global outcome_choice normgdp_s
global outcome_choicecg normgdp_scg
global countries_choice 1 3 7 8 9 10 12 14 16 18 21 22
global end_choice = 1998

do Paths
do 0_Data_Management_Annual
do 8_SCM_1999
do 2_Aggregation_Annual

* Table
*do 3_Descriptives_Annual

* Table 
*do 4_ComparisonTables_Annual

* Figures
do 9_Graphs_1999

*/
