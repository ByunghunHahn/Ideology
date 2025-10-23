********************************************************************************
* Generate Summary Table for Outcome Variables
* Written by Ariel Gelrud 5/20/25
* Latest updated 5/20/25
* Generate Summary Statistics Table using file write (no texdoc needed)
* Alternative approach that doesn't require texdoc package
********************************************************************************

set more off
set matsize 5000
set emptycells drop

* Set directory and globals
cd "~/Library/CloudStorage/Dropbox/California Election Data/Code"
global logpath "~/Library/CloudStorage/Dropbox/California Election Data/Logs"
global tab_opt = "dec(2) pdec(2) label"
global mindta "~/Library/CloudStorage/Dropbox/California Election Data/Minutes/data/dta"
global missing = "with_missing"

* Define outcome variables
global y_fiscal = "exp_total_per_stu"
global y_score = "test ecd_test hsp_test"
global y_enroll = "enrollment"
global y_behv = "sus_share"
global class = "entry ba30 ba60"

* Create Results/Tex directory if it doesn't exist
capture mkdir "Results"
capture mkdir "Results/Tex"

* Open file for writing
file open myfile using "Results/Tex/summary_stats_table.tex", write replace

* Write table header
// file write myfile "\begin{table}[H]" _n
// file write myfile "\caption{Summary Statistics} \label{tab:summ_stats_table}" _n
// file write myfile "\center" _n
// file write myfile "\resizebox{\textwidth}{!}{" _n
// file write myfile "    \renewcommand{\arraystretch}{1.1}" _n

file write myfile "   {@{}l@{\hspace{6.5pt}}c@{\hspace{6pt}}c@{\hspace{7pt}}c@{\hspace{6pt}}c@{\hspace{6pt}}c@{}} \toprule \midrule" _n
file write myfile "     & All & \multicolumn{4}{c}{Close Elections ($<$ 5 pp)}  \\\cline{3-6}" _n
file write myfile "     &    Elections    & Any & Fiscally Conservative & Equity & Hispanic \\\cline{2-2}\cline{3-3}\cline{4-4}\cline{5-5}\cline{6-6}" _n
file write myfile "     & (1) & (2) & (3) & (4) & (5) \\ \midrule" _n

********************************************************************************
* Panel A: Candidate Characteristics
********************************************************************************

file write myfile "    \multicolumn{6}{l}{\textit{\textbf{Panel A: Candidate Characteristics}}} \\" _n

* Panel A variables and labels
local panel_a_vars " hisp female occ_teacher democrat valid_platform"

* Process Panel A variables
local i = 1
foreach var of local panel_a_vars {
    if `i' == 5 local label "Share Having a Platform"
    if `i' == 1 local label "Share Hispanic"
    if `i' == 2 local label "Share Female"
    if `i' == 3 local label "Share Teacher"
    if `i' == 4 local label "Share Registered Democrat"
    
    * Column 1: All elections
    use "candidates_all_$missing.dta" if offedu==0, clear
    collapse `var', by(multi_race)
    quietly sum `var'
    local col1 = string(r(mean), "%4.2f")
    
    * Column 2: Close elections (any)
    use "candidates_close_election_$missing.dta" if offedu==0, clear
    collapse `var', by(multi_race)
    quietly sum `var'
    local col2 = string(r(mean), "%4.2f")
    
    * Column 3: Close elections - Budget Hawk
    use "data_for_rd/with_missing/dist_budget_hawk.dta" if dist_cnty~=1, clear
    keep multi_race
    duplicates drop
    destring multi_race, replace
    merge 1:m multi_race using "candidates_all_with_missing.dta", gen(_mm) keep(3)
    collapse `var', by(multi_race)
    quietly sum `var'
    local col3 = string(r(mean), "%4.2f")
    
    * Column 4: Close elections - Equity Prior
    use "data_for_rd/with_missing/dist_equity_prior.dta" if dist_cnty~=1, clear
    keep multi_race
    duplicates drop
    destring multi_race, replace
    merge 1:m multi_race using "candidates_all_with_missing.dta", gen(_mm) keep(3)
    collapse `var', by(multi_race)
    quietly sum `var'
    local col4 = string(r(mean), "%4.2f")
    
    * Column 5: Close elections - Hispanic
    use "data_for_rd/with_missing/dist_hisp.dta" if dist_cnty~=1, clear
    keep multi_race
    duplicates drop
    destring multi_race, replace
    merge 1:m multi_race using "candidates_all_with_missing.dta", gen(_mm) keep(3)
    collapse `var', by(multi_race)
    quietly sum `var'
    local col5 = string(r(mean), "%4.2f")
    
    * Write row to file
    file write myfile "    `label' & `col1' & `col2' & `col3' & `col4' & `col5' \\" _n
    
    local i = `i' + 1
}

* N Elections row for Panel A
use "candidates_all_$missing.dta" if offedu==0, clear
unique multi_race
local n1 : display %12.0fc r(unique)
unique multi_candid
local nc1 : display %12.0fc r(unique)


use "candidates_close_election_$missing.dta" if offedu==0, clear
unique multi_race
local n2 : display %12.0fc r(unique)
unique multi_candid
local nc2 : display %12.0fc r(unique)


use "data_for_rd/with_missing/dist_budget_hawk.dta" if dist_cnty~=1, clear
keep multi_race
duplicates drop
destring multi_race, replace
merge 1:m multi_race using "candidates_all_with_missing.dta", gen(_mm) keep(3)
unique multi_race
local n3 : display %12.0fc r(unique)
unique multi_candid
local nc3 : display %12.0fc r(unique)


use "data_for_rd/with_missing/dist_equity_prior.dta" if dist_cnty~=1, clear
keep multi_race
duplicates drop
destring multi_race, replace
merge 1:m multi_race using "candidates_all_with_missing.dta", gen(_mm) keep(3)
unique multi_race
local n4 : display %12.0fc r(unique)
unique multi_candid
local nc4 : display %12.0fc r(unique)

use "data_for_rd/with_missing/dist_hisp.dta" if dist_cnty~=1, clear
keep multi_race
duplicates drop
destring multi_race, replace
merge 1:m multi_race using "candidates_all_with_missing.dta", gen(_mm) keep(3)
unique multi_race
local n5 : display %12.0fc r(unique)
unique multi_candid
local nc5 : display %12.0fc r(unique)

file write myfile "    N Candidates & `nc1' & `nc2' & `nc3' & `nc4' & `nc5' \\" _n
file write myfile "    N Elections & `n1' & `n2' & `n3' & `n4' & `n5' \\" _n
file write myfile "       & & & & & \\" _n
********************************************************************************
* Panel B: Educational Outcomes in the Year Before the Election
********************************************************************************

file write myfile "    \multicolumn{6}{l}{\textit{\textbf{Panel B: Educational Outcomes in the Year Before the Election}}} \\" _n

* Function to process outcomes data
program define process_outcomes_data
    replace year = year-1
    destring leaid, replace
    duplicates drop leaid year, force
    merge 1:1 leaid year using "outcomes.dta", gen(_platform) force keep(master matched)
    
    * Process salary data
    ren id_dist_state cds
    merge m:1 year cds using "../CDE/Certificated Salaries & Benefits/TSAL Outcome.dta", gen(_tsal) force keep(matched)
    merge m:1 year using "cpi.dta", keep(matched) nogen
    
    ds *sal*
    local varlist = r(varlist)
    foreach x in `varlist' {
        replace `x'=. if `x'<0
        replace `x' = `x' * cpi
    }
    
    * Generate salary variables
    foreach x in $class {
        gen `x'_totalfte = 0
        forvalues i = 1/40 {
            replace `x'_totalfte = `x'_totalfte + `x'_fte`i' if !missing(`x'_fte`i')
        }
        gen `x'_sal_Bcomp = 0
        forvalues i = 1/40 {
            replace `x'_sal_Bcomp = `x'_sal_Bcomp + (`x'_salary`i' * `x'_fte`i')/`x'_totalfte if !missing(`x'_fte`i') & !missing(`x'_salary`i')
        }
        replace `x'_sal_Bcomp=. if `x'_sal_Bcomp==0
        replace `x'_sal_Bcomp=`x'_sal_Bcomp/1000
    }
end

* CCD Spending Per Pupil - all columns
* Column 1: All elections
use "candidates_all_$missing.dta" if offedu==0, clear
process_outcomes_data
quietly sum exp_total_per_stu
local col1 = string(r(mean), "%4.2f")
quietly count if !missing(exp_total_per_stu)
local n1 : display %12.0fc r(N)

* Column 2: Close elections
use "candidates_close_election_$missing.dta" if offedu==0, clear
process_outcomes_data
quietly sum exp_total_per_stu
local col2 = string(r(mean), "%4.2f")
quietly count if !missing(exp_total_per_stu)
local n2 : display %12.0fc r(N)

* Column 3: Budget Hawk
use "data_for_rd/with_missing/dist_budget_hawk.dta" if dist_cnty~=1, clear
keep multi_race
duplicates drop
destring multi_race, replace
merge 1:m multi_race using "candidates_all_with_missing.dta", gen(_mm) keep(3)
process_outcomes_data
quietly sum exp_total_per_stu
local col3 = string(r(mean), "%4.2f")
quietly count if !missing(exp_total_per_stu)
local n3 : display %12.0fc r(N)

* Column 4: Equity Prior
use "data_for_rd/with_missing/dist_equity_prior.dta" if dist_cnty~=1, clear
keep multi_race
duplicates drop
destring multi_race, replace
merge 1:m multi_race using "candidates_all_with_missing.dta", gen(_mm) keep(3)
process_outcomes_data
quietly sum exp_total_per_stu
local col4 = string(r(mean), "%4.2f")
quietly count if !missing(exp_total_per_stu)
local n4 : display %12.0fc r(N)

* Column 5: Hispanic
use "data_for_rd/with_missing/dist_hisp.dta" if dist_cnty~=1, clear
keep multi_race
duplicates drop
destring multi_race, replace
merge 1:m multi_race using "candidates_all_with_missing.dta", gen(_mm) keep(3)
process_outcomes_data
quietly sum exp_total_per_stu
local col5 = string(r(mean), "%4.2f")
quietly count if !missing(exp_total_per_stu)
local n5 : display %12.0fc r(N)

file write myfile "    \textbf{CCD} Spending Per Pupil (\\$1k) (1998-) & `col1' & `col2' & `col3' & `col4' & `col5' \\" _n
file write myfile "    N Districts x Years & `n1' & `n2' & `n3' & `n4' & `n5' \\" _n
file write myfile "       & & & & & \\" _n

* Enrollment
local enroll_vars "enrollment enrollment_white_share enrollment_hisp_share"

local i = 1
foreach var of local enroll_vars {
    if `i' == 1 local label "\textbf{CCD} Total Enrollment (1998-)"
    if `i' == 2 local label "Share White (1998-)"
    if `i' == 3 local label "Share Hispanic (1998-)"
    
    * Column 1: All elections
    use "candidates_all_$missing.dta" if offedu==0, clear
    process_outcomes_data
    quietly sum `var'
    local col1 = string(r(mean), "%4.2f")
    
    * Column 2: Close elections
    use "candidates_close_election_$missing.dta" if offedu==0, clear
    process_outcomes_data
    quietly sum `var'
    local col2 = string(r(mean), "%4.2f")
    
    * Column 3: Budget Hawk
    use "data_for_rd/with_missing/dist_budget_hawk.dta" if dist_cnty~=1, clear
    keep multi_race
    duplicates drop
    destring multi_race, replace
    merge 1:m multi_race using "candidates_all_with_missing.dta", gen(_mm) keep(3)
    process_outcomes_data
    quietly sum `var'
    local col3 = string(r(mean), "%4.2f")
    
    * Column 4: Equity Prior
    use "data_for_rd/with_missing/dist_equity_prior.dta" if dist_cnty~=1, clear
    keep multi_race
    duplicates drop
    destring multi_race, replace
    merge 1:m multi_race using "candidates_all_with_missing.dta", gen(_mm) keep(3)
    process_outcomes_data
    quietly sum `var'
    local col4 = string(r(mean), "%4.2f")
    
    * Column 5: Hispanic
    use "data_for_rd/with_missing/dist_hisp.dta" if dist_cnty~=1, clear
    keep multi_race
    duplicates drop
    destring multi_race, replace
    merge 1:m multi_race using "candidates_all_with_missing.dta", gen(_mm) keep(3)
    process_outcomes_data
    quietly sum `var'
    local col5 = string(r(mean), "%4.2f")
    
    file write myfile "    `label' & `col1' & `col2' & `col3' & `col4' & `col5' \\" _n
    
    local i = `i' + 1
}

* N row for enrollment - calculate actual N for first test variable
use "candidates_all_$missing.dta" if offedu==0, clear
process_outcomes_data
quietly count if !missing(test)
local n1 : display %12.0fc r(N)

use "candidates_close_election_$missing.dta" if offedu==0, clear
process_outcomes_data
quietly count if !missing(test)
local n2 : display %12.0fc r(N)

use "data_for_rd/with_missing/dist_budget_hawk.dta" if dist_cnty~=1, clear
keep multi_race
duplicates drop
destring multi_race, replace
merge 1:m multi_race using "candidates_all_with_missing.dta", gen(_mm) keep(3)
process_outcomes_data
quietly count if !missing(test)
local n3 : display %12.0fc r(N)

use "data_for_rd/with_missing/dist_equity_prior.dta" if dist_cnty~=1, clear
keep multi_race
duplicates drop
destring multi_race, replace
merge 1:m multi_race using "candidates_all_with_missing.dta", gen(_mm) keep(3)
process_outcomes_data
quietly count if !missing(test)
local n4 : display %12.0fc r(N)

use "data_for_rd/with_missing/dist_hisp.dta" if dist_cnty~=1, clear
keep multi_race
duplicates drop
destring multi_race, replace
merge 1:m multi_race using "candidates_all_with_missing.dta", gen(_mm) keep(3)
process_outcomes_data
quietly count if !missing(test)
local n5 : display %12.0fc r(N)

file write myfile "     N Districts x Years & `n1' & `n2' & `n3' & `n4' & `n5' \\" _n

* Test scores
local test_vars "test ecd_test hsp_test perecd"

local i = 1
foreach var of local test_vars {
    if `i' == 1 local label "\textbf{SEDA} Test Scores (2009-)"
    if `i' == 2 local label "Hispanic Test Scores (2009-)"
    if `i' == 3 local label "ECD Test Scores (2009-)"
    if `i' == 4 local label "Share ECD Students (2009-)"
    
    * Column 1: All elections
    use "candidates_all_$missing.dta" if offedu==0, clear
    process_outcomes_data
    quietly sum `var'
    local col1 = string(r(mean), "%4.2f")
    
    * Column 2: Close elections
    use "candidates_close_election_$missing.dta" if offedu==0, clear
    process_outcomes_data
    quietly sum `var'
    local col2 = string(r(mean), "%4.2f")
    
    * Column 3: Budget Hawk
    use "data_for_rd/with_missing/dist_budget_hawk.dta" if dist_cnty~=1, clear
    keep multi_race
    duplicates drop
    destring multi_race, replace
    merge 1:m multi_race using "candidates_all_with_missing.dta", gen(_mm) keep(3)
    process_outcomes_data
    quietly sum `var'
    local col3 = string(r(mean), "%4.2f")
    
    * Column 4: Equity Prior
    use "data_for_rd/with_missing/dist_equity_prior.dta" if dist_cnty~=1, clear
    keep multi_race
    duplicates drop
    destring multi_race, replace
    merge 1:m multi_race using "candidates_all_with_missing.dta", gen(_mm) keep(3)
    process_outcomes_data
    quietly sum `var'
    local col4 = string(r(mean), "%4.2f")
    
    * Column 5: Hispanic
    use "data_for_rd/with_missing/dist_hisp.dta" if dist_cnty~=1, clear
    keep multi_race
    duplicates drop
    destring multi_race, replace
    merge 1:m multi_race using "candidates_all_with_missing.dta", gen(_mm) keep(3)
    process_outcomes_data
    quietly sum `var'
    local col5 = string(r(mean), "%4.2f")
    
    file write myfile "    `label' & `col1' & `col2' & `col3' & `col4' & `col5' \\" _n
    
    local i = `i' + 1
}

* N row for test scores - calculate actual N for first test variable
use "candidates_all_$missing.dta" if offedu==0, clear
process_outcomes_data
quietly count if !missing(test)
local n1 : display %12.0fc r(N)

use "candidates_close_election_$missing.dta" if offedu==0, clear
process_outcomes_data
quietly count if !missing(test)
local n2 : display %12.0fc r(N)

use "data_for_rd/with_missing/dist_budget_hawk.dta" if dist_cnty~=1, clear
keep multi_race
duplicates drop
destring multi_race, replace
merge 1:m multi_race using "candidates_all_with_missing.dta", gen(_mm) keep(3)
process_outcomes_data
quietly count if !missing(test)
local n3 : display %12.0fc r(N)

use "data_for_rd/with_missing/dist_equity_prior.dta" if dist_cnty~=1, clear
keep multi_race
duplicates drop
destring multi_race, replace
merge 1:m multi_race using "candidates_all_with_missing.dta", gen(_mm) keep(3)
process_outcomes_data
quietly count if !missing(test)
local n4 : display %12.0fc r(N)

use "data_for_rd/with_missing/dist_hisp.dta" if dist_cnty~=1, clear
keep multi_race
duplicates drop
destring multi_race, replace
merge 1:m multi_race using "candidates_all_with_missing.dta", gen(_mm) keep(3)
process_outcomes_data
quietly count if !missing(test)
local n5 : display %12.0fc r(N)

file write myfile "     N Districts x Years & `n1' & `n2' & `n3' & `n4' & `n5' \\" _n


********************************************************************************
* Panel C: Meeting Characteristics in the Year Before the Election
********************************************************************************

file write myfile "    \multicolumn{6}{l}{\textit{\textbf{Panel C: Meeting Characteristics in the Year Before the Election}}} \\" _n

* Meeting characteristics
local meeting_vars "n_meetings num_present n_mtn n_pass"

local i = 1
foreach var of local meeting_vars {
    if `i' == 1 local label "Total Number of Meetings"
    if `i' == 2 local label "Present Members per Meeting"
    if `i' == 3 local label "Proposed Motions per Year"
    if `i' == 4 local label "Passed Motions per Year"
    
    * Column 1: All elections
    use "candidates_all_$missing.dta" if offedu==0, clear
    keep leaid year multi_raceid
    duplicates drop
    replace year = year-1
    merge m:1 leaid year using "$mindta/minutes_final_year_level.dta", keep(master matched)
	gen n_mtn_yr = n_mtn*n_meetings
	gen n_pass_yr = n_pass*n_meetings
    quietly sum `var'
    local col1 = string(r(mean), "%4.2f")
    
    * Column 2: Close elections
    use "candidates_close_election_$missing.dta" if offedu==0, clear
    keep leaid year multi_raceid
    duplicates drop
    replace year = year-1
    merge m:1 leaid year using "$mindta/minutes_final_year_level.dta", keep(master matched)
	gen n_mtn_yr = n_mtn*n_meetings
	gen n_pass_yr = n_pass*n_meetings
    quietly sum `var'
    local col2 = string(r(mean), "%4.2f")
    
    * Column 3: Budget Hawk
    use "data_for_rd/with_missing/dist_budget_hawk.dta" if dist_cnty~=1, clear
    keep multi_race
    duplicates drop
    destring multi_race, replace
    merge 1:m multi_race using "candidates_all_with_missing.dta", gen(_mm) keep(3)
    keep leaid year multi_raceid
    duplicates drop
    replace year = year-1
    merge m:1 leaid year using "$mindta/minutes_final_year_level.dta", keep(3)
	gen n_mtn_yr = n_mtn*n_meetings
	gen n_pass_yr = n_pass*n_meetings
    quietly sum `var'
    local col3 = string(r(mean), "%4.2f")
    
    * Column 4: Equity Prior
    use "data_for_rd/with_missing/dist_equity_prior.dta" if dist_cnty~=1, clear
    keep multi_race
    duplicates drop
    destring multi_race, replace
    merge 1:m multi_race using "candidates_all_with_missing.dta", gen(_mm) keep(3)
    keep leaid year multi_raceid
    duplicates drop
    replace year = year-1
    merge m:1 leaid year using "$mindta/minutes_final_year_level.dta", keep(3)
	gen n_mtn_yr = n_mtn*n_meetings
	gen n_pass_yr = n_pass*n_meetings
    quietly sum `var'
    local col4 = string(r(mean), "%4.2f")
    
    * Column 5: Hispanic
    use "data_for_rd/with_missing/dist_hisp.dta" if dist_cnty~=1, clear
    keep multi_race
    duplicates drop
    destring multi_race, replace
    merge 1:m multi_race using "candidates_all_with_missing.dta", gen(_mm) keep(3)
    keep leaid year multi_raceid
    duplicates drop
    replace year = year-1
    merge m:1 leaid year using "$mindta/minutes_final_year_level.dta", keep(3)
	gen n_mtn_yr = n_mtn*n_meetings
	gen n_pass_yr = n_pass*n_meetings
    quietly sum `var'
    local col5 = string(r(mean), "%4.2f")
    
    file write myfile "    `label' & `col1' & `col2' & `col3' & `col4' & `col5' \\" _n
    
    local i = `i' + 1
}

* N Districts x Years - calculate actual N for first meeting variable
use "candidates_all_$missing.dta" if offedu==0, clear
keep leaid year multi_raceid
duplicates drop
replace year = year-1
merge m:1 leaid year using "$mindta/minutes_final_year_level.dta", keep(master matched)
quietly count if !missing(n_meetings)
local n1 : display %12.0fc r(N)

use "candidates_close_election_$missing.dta" if offedu==0, clear
keep leaid year multi_raceid
duplicates drop
replace year = year-1
merge m:1 leaid year using "$mindta/minutes_final_year_level.dta", keep(master matched)
quietly count if !missing(n_meetings)
local n2 : display %12.0fc r(N)

use "data_for_rd/with_missing/dist_budget_hawk.dta" if dist_cnty~=1, clear
keep multi_race
duplicates drop
destring multi_race, replace
merge 1:m multi_race using "candidates_all_with_missing.dta", gen(_mm) keep(3)
keep leaid year multi_raceid
duplicates drop
replace year = year-1
merge m:1 leaid year using "$mindta/minutes_final_year_level.dta", keep(3)
quietly count if !missing(n_meetings)
local n3 : display %12.0fc r(N)

use "data_for_rd/with_missing/dist_equity_prior.dta" if dist_cnty~=1, clear
keep multi_race
duplicates drop
destring multi_race, replace
merge 1:m multi_race using "candidates_all_with_missing.dta", gen(_mm) keep(3)
keep leaid year multi_raceid
duplicates drop
replace year = year-1
merge m:1 leaid year using "$mindta/minutes_final_year_level.dta", keep(3)
quietly count if !missing(n_meetings)
local n4 : display %12.0fc r(N)

use "data_for_rd/with_missing/dist_hisp.dta" if dist_cnty~=1, clear
keep multi_race
duplicates drop
destring multi_race, replace
merge 1:m multi_race using "candidates_all_with_missing.dta", gen(_mm) keep(3)
keep leaid year multi_raceid
duplicates drop
replace year = year-1
merge m:1 leaid year using "$mindta/minutes_final_year_level.dta", keep(3)
quietly count if !missing(n_meetings)
local n5 : display %12.0fc r(N)

file write myfile "    N Districts x Years & `n1' & `n2' & `n3' & `n4' & `n5' \\" _n

* Close table
file write myfile "    \bottomrule" _n
file write myfile "    \end{tabular}" _n
// file write myfile "}" _n
// file write myfile "\end{table}" _n

* Close file
file close myfile

* Drop the program
program drop process_outcomes_data

display "LaTeX table written to Results/Tex/summary_stats_table.tex"

* * * 
use "candidates_all_$missing.dta" if offedu==0, clear
    keep leaid year multi_raceid
    duplicates drop
    replace year = year-1
    merge m:1 leaid year using "$mindta/minutes_final_year_level.dta", keep(master matched)
	
	
	