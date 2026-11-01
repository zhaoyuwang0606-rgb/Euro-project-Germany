/*
*Authors: Ricardo Duque Gabriel and Ana Sofia Pessoa 
*Start Date: 10/24/2019
*Last Update: 01/27/2024


Master code for changing the kind of exercise you want to perform by 
changing the global variables in the project. Annual data (1970-2007)
*/

/*
* ids of treated units: 1 3 7 8 9 10 12 14 17 19 22 23
* Austria: 1
* Belgium: 3
* Finland: 7
* France: 8
* Germany: 9
* Greece: 10 - take into account that Greece has a different treatment date 
* Ireland: 12 - careful when analysing normalized gdp - remove the match in 1997Q3-1998Q4 because Ireland is impossible to match, it is the country with highest gdp growth in the pre-treatment sample!
* Italy: 14
* Luxembourg : 16 - leave it out of the analysis when using (non-normalized) gni or real gdp per capita - because its high level can't be matched.
* Netherlands : 18
* Portugal : 21
* Spain : 22
global countries_name Austria Belgium Finland France Germany Greece Ireland Italy Luxembourg Netherlands Portugal Spain
global countries_code AUT BEL FIN FRA DEU GRC IRL ITA LUX NLD PRT ESP
global countries $countries_choice
*remove first from previous list id to easily aggregate datasets in Aggregation.do
*global countries_agg 3 7 8 9 10 12 14 16 18 21 22
global number_countries = 12

*Donor  countries (R version)
global donor_countries_R 2 4 5 6 11 13 15 17 19 20 23 24 25 26

*choose outcome variable (normgdp_s or lrpcgdp_s)
global outcome $outcome_choice


*choose predictors to the following list and tell stata how many are you using (e.g. inflation)
global covariates csh_c csh_i csh_g csh_m csh_x csh_emp csh_prod inflation
global number_covariates = 8

*change also for doppelganger series the choice of the outcome and covariates (ending with "cg" for control group)
global outcomecg $outcome_choicecg

global covariatescg csh_ccg csh_icg csh_gcg csh_xcg csh_mcg csh_empcg csh_prodcg inflationcg


*treatment dates - for the anticipation test change here to 1992 (when the Maastricht Treaty was signed) or 1995 or 1998
global treatment = $treatment_choice
global treatment_GRC = $treatment_GRC_choice

*starting and ending years of the sample
global begin = 1970
global end = $end_choice
