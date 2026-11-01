* Stata script to run the SCM replication for Germany only in user folder

clear all
set more off

* Install SCM packages if missing
cap ssc install synth
cap ssc install moremata
cap ssc install outtable

* Define working path
global hp = "c:\Users\22984\Desktop\Leuven slide\Applied econometrics\Applied econometrics\Replication"
global Fig = "$hp\Output\"
global Tab = "$hp\Output\"
global Data = "$hp\Data\"

* Set SCM parameters for Germany only (id == 9)
global countries_name Germany
global countries_code DEU
global countries 9
global number_countries = 12  // Set to 12 to avoid conformability error in 1_SCM_Annual.do

* Outcome and Covariates
global outcome normgdp_s
global outcomecg normgdp_scg

global covariates csh_c csh_i csh_g csh_x csh_m csh_emp csh_prod inflation
global number_covariates = 8
global covariatescg csh_ccg csh_icg csh_gcg csh_xcg csh_mcg csh_empcg csh_prodcg inflationcg

* Treatment dates
global treatment = 1999
global treatment_GRC = 2001

* Sample years
global begin = 1970
global end = 2007

* Rob checks
global remove_1 
global remove_2 
global remove_3 
global remove_nr = 0

* Donor countries
global donor_names Australia Canada Chile Denmark Iceland Israel Korea Mexico NewZealand Norway Sweden Switzerland UnitedKingdom UnitedStates
global donor_countries 2 4 5 6 11 13 15 17 19 20 23 24 25 26
global number_donor = 14

* Run the SCM script
do 1_SCM_Annual.do

* Print weights and first few rows of outputs
use "$Data/Auxi/SCM_9_Annual.dta", clear
list date normgdp_s normgdp_scg in 1/15
