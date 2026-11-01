* Stata script to run extended SCM replication for Germany (Baseline, Ex-UK, LOO, Placebos)
clear all
set more off

* Define paths
global hp = "c:\Users\22984\Desktop\Leuven slide\Applied econometrics\Applied econometrics\Replication"
global Fig = "$hp\Output\"
global Tab = "$hp\Output\"
global Data = "$hp\Data\"

* Create Stata output directories
cap mkdir "$hp\Output\Baseline_Stata"
cap mkdir "$hp\Output\ExUK_Stata"

* 1. Baseline Model SCM (with placebos)
* ==============================================================================
display "== Running Baseline SCM in Stata (nested allopt) =="

use "$Data/Analysis_Annual.dta", clear
* Germany (id == 9) + all 14 donor countries
keep if id == 9 | donor == 1
replace id = 1 if id == 9
xtset id h

* Run baseline synth
synth normgdp_s csh_c csh_i csh_g csh_x csh_m csh_emp csh_prod inflation normgdp_s normgdp_s(2(1)29), trunit(1) trperiod(30) nested allopt

* Save W-weights (unit weights)
matrix w = e(W_weights)
local p = rowsof(w)
tempname myfile
file open `myfile' using "$hp\Output\Baseline_Stata\SCM_Weights.csv", write replace
file write `myfile' "unit,weight" _n
forval i = 1/`p' {
    local cid = w[`i', 1]
    local wval = w[`i', 2]
    preserve
    use "$Data/Analysis_Annual.dta", clear
    keep if id == `cid'
    local cname = country[1]
    restore
    file write `myfile' `"`cname',`wval'"' _n
}
file close `myfile'

* Save V-weights (predictor weights)
matrix v = e(V_matrix)
local K = rowsof(v)
local vnames : rownames v
tempname myfile
file open `myfile' using "$hp\Output\Baseline_Stata\SCM_Predictor_Weights.csv", write replace
file write `myfile' "variable,weight" _n
forval i = 1/`K' {
    local vname : word `i' of `vnames'
    local vval = v[`i', `i']
    file write `myfile' "`vname',`vval'" _n
}
file close `myfile'

* Save balance table
matrix b = e(X_balance)
local K_b = rowsof(b)
local bnames : rownames b
tempname myfile
file open `myfile' using "$hp\Output\Baseline_Stata\SCM_Balance.csv", write replace
file write `myfile' "variable,Treated,Synthetic" _n
forval i = 1/`K_b' {
    local bname : word `i' of `bnames'
    local tval = b[`i', 1]
    local sval = b[`i', 2]
    file write `myfile' "`bname',`tval',`sval'" _n
}
file close `myfile'

* Obtain Baseline Trend and Gap series
use "$Data/Analysis_Annual.dta", clear
keep if id == 9 | donor == 1
replace id = 1 if id == 9
xtset id h

foreach var in normgdp_s {
    gen `var'cg = .
}
forval c = 1/`p' {
    replace normgdp_scg = w[`c',2]*normgdp_s if id == w[`c',1]
}

preserve
keep if id != 1
collapse (sum) normgdp_scg, by(year)
tempfile temp_scg
save `temp_scg'
restore

preserve
keep if id == 1
drop normgdp_scg  // Drop all-missing variable to allow merge to overwrite it
merge 1:1 year using `temp_scg', keep(match) nogenerate
gen gap = normgdp_s - normgdp_scg
gen pct_gap = gap / normgdp_scg * 100
keep year normgdp_s normgdp_scg gap pct_gap
rename year time_unit
rename normgdp_s real_y
rename normgdp_scg synth_y
export delimited using "$hp\Output\Baseline_Stata\Baseline_YearByYear_Gap.csv", replace

* Compute aggregated effect
gen pre = (time_unit < 1999)
sum gap if pre == 1
local pre_avg = r(mean)
sum gap if pre == 0
local post_avg = r(mean)
local post_cum = r(sum)
sum pct_gap if pre == 0
local post_pct = r(mean)
local post_n = r(N)

clear
set obs 1
gen Specification = "Stata SCM - Germany Baseline"
gen Pre_Avg_Gap = `pre_avg'
gen Post_Avg_Gap = `post_avg'
gen Post_Cumul_Gap = `post_cum'
gen Post_Avg_Pct = `post_pct'
gen Post_N_Years = `post_n'
export delimited using "$hp\Output\Baseline_Stata\Aggregated_Treatment_Effect.csv", replace
restore

* 2. Ex-UK Model SCM
* ==============================================================================
display "== Running Ex-UK SCM in Stata (nested allopt) =="

use "$Data/Analysis_Annual.dta", clear
* Germany (id == 9) + all donor countries except United Kingdom (id == 25)
keep if id == 9 | (donor == 1 & id != 25)
replace id = 1 if id == 9
xtset id h

* Run ex-UK synth
synth normgdp_s csh_c csh_i csh_g csh_x csh_m csh_emp csh_prod inflation normgdp_s normgdp_s(2(1)29), trunit(1) trperiod(30) nested allopt

* Save W-weights (unit weights)
matrix w_ex = e(W_weights)
local p_ex = rowsof(w_ex)
tempname myfile
file open `myfile' using "$hp\Output\ExUK_Stata\SCM_Weights.csv", write replace
file write `myfile' "unit,weight" _n
forval i = 1/`p_ex' {
    local cid = w_ex[`i', 1]
    local wval = w_ex[`i', 2]
    preserve
    use "$Data/Analysis_Annual.dta", clear
    keep if id == `cid'
    local cname = country[1]
    restore
    file write `myfile' `"`cname',`wval'"' _n
}
file close `myfile'

* Save V-weights (predictor weights)
matrix v_ex = e(V_matrix)
local K_ex = rowsof(v_ex)
local vnames_ex : rownames v_ex
tempname myfile
file open `myfile' using "$hp\Output\ExUK_Stata\SCM_Predictor_Weights.csv", write replace
file write `myfile' "variable,weight" _n
forval i = 1/`K_ex' {
    local vname : word `i' of `vnames_ex'
    local vval = v_ex[`i', `i']
    file write `myfile' "`vname',`vval'" _n
}
file close `myfile'

* Save balance table
matrix b_ex = e(X_balance)
local K_b_ex = rowsof(b_ex)
local bnames_ex : rownames b_ex
tempname myfile
file open `myfile' using "$hp\Output\ExUK_Stata\SCM_Balance.csv", write replace
file write `myfile' "variable,Treated,Synthetic" _n
forval i = 1/`K_b_ex' {
    local bname : word `i' of `bnames_ex'
    local tval = b_ex[`i', 1]
    local sval = b_ex[`i', 2]
    file write `myfile' "`bname',`tval',`sval'" _n
}
file close `myfile'

* Obtain Ex-UK Trend and Gap series
use "$Data/Analysis_Annual.dta", clear
keep if id == 9 | (donor == 1 & id != 25)
replace id = 1 if id == 9
xtset id h

foreach var in normgdp_s {
    gen `var'cg = .
}
forval c = 1/`p_ex' {
    replace normgdp_scg = w_ex[`c',2]*normgdp_s if id == w_ex[`c',1]
}

preserve
keep if id != 1
collapse (sum) normgdp_scg, by(year)
tempfile temp_scg_ex
save `temp_scg_ex'
restore

preserve
keep if id == 1
drop normgdp_scg  // Drop all-missing variable to allow merge to overwrite it
merge 1:1 year using `temp_scg_ex', keep(match) nogenerate
gen gap = normgdp_s - normgdp_scg
gen pct_gap = gap / normgdp_scg * 100
keep year normgdp_s normgdp_scg gap pct_gap
rename year time_unit
rename normgdp_s real_y
rename normgdp_scg synth_y
export delimited using "$hp\Output\ExUK_Stata\Baseline_YearByYear_Gap.csv", replace

* Compute aggregated effect
gen pre = (time_unit < 1999)
sum gap if pre == 1
local pre_avg = r(mean)
sum gap if pre == 0
local post_avg = r(mean)
local post_cum = r(sum)
sum pct_gap if pre == 0
local post_pct = r(mean)
local post_n = r(N)

clear
set obs 1
gen Specification = "Stata SCM - Germany Ex-UK"
gen Pre_Avg_Gap = `pre_avg'
gen Post_Avg_Gap = `post_avg'
gen Post_Cumul_Gap = `post_cum'
gen Post_Avg_Pct = `post_pct'
gen Post_N_Years = `post_n'
export delimited using "$hp\Output\ExUK_Stata\Aggregated_Treatment_Effect.csv", replace
restore

* 3. Leave-One-Out (LOO) checks in Stata
* ==============================================================================
display "== Running LOO in Stata (nested allopt) =="
* We loop over the top donors for Ex-UK model in Stata.
* Based on the weights we get: USA (id 26), Sweden (id 23), Switzerland (id 24), Canada (id 4), Denmark (id 6)
local top_donors 26 23 24 4 6
local donor_names_loo UnitedStates Sweden Switzerland Canada Denmark

* We will save the synthetic paths for each LOO run in a single dataset
tempfile loo_all_trends
local first = 1

foreach d of local top_donors {
    preserve
    use "$Data/Analysis_Annual.dta", clear
    keep if id == `d'
    local dname = country[1]
    restore
    
    preserve
    use "$Data/Analysis_Annual.dta", clear
    keep if id == 9 | (donor == 1 & id != 25 & id != `d')
    replace id = 1 if id == 9
    xtset id h
    
    synth normgdp_s csh_c csh_i csh_g csh_x csh_m csh_emp csh_prod inflation normgdp_s normgdp_s(2(1)29), trunit(1) trperiod(30) nested allopt
    
    matrix w_loo = e(W_weights)
    local p_loo = rowsof(w_loo)
    
    foreach var in normgdp_s {
        gen `var'cg = .
    }
    forval c = 1/`p_loo' {
        replace normgdp_scg = w_loo[`c',2]*normgdp_s if id == w_loo[`c',1]
    }
    
    keep if id != 1
    collapse (sum) normgdp_scg, by(year)
    
    gen type = "Drop `dname'"
    
    if `first' == 1 {
        save `loo_all_trends', replace
        local first = 0
    }
    else {
        append using `loo_all_trends'
        save `loo_all_trends', replace
    }
    restore
}

* Export the LOO trends to CSV so that we can plot them in R
preserve
use `loo_all_trends', clear
rename year time_unit
rename normgdp_scg value
export delimited using "$hp\Output\ExUK_Stata\SCM_LOO_Trends.csv", replace
restore

display "== All SCM runs in Stata finished successfully! =="
