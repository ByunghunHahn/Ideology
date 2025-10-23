********************************************************************************
* School Level Outcome
*  Won't be merged to the main outcome.dta, since it blows up the N of obs
*  School outcome data sets are separately stored, and merged only to 
* 	a handful of RD data sets when needed
* Written by Minseon Park 09/22/2024
* Latest version by Minseon Park 11/05/2024
* I. CCD- CRDC merge (CRDC: 2009-2017, biannual)
* II. Then ESSA (ESSA: 2018-2022, annual)
* III. Generate Variables
********************************************************************************

clear all 
set more off
set emptycells drop
set maxvar 32000

* set directory 
cd "~/Library/CloudStorage/Dropbox/California Election Data/Code"
global logpath "~/Library/CloudStorage/Dropbox/California Election Data/Logs"

local today : di %td date("$S_DATE", "DMY")

cap log close
log using "$logpath/99_outcome_sch_`today'.log", replace


********************************************************************************
* I. CCD- CRDC merge (CRDC: 2009-2017, biannual)
********************************************************************************

*** 1) crosswalk prepared by Ariel
use "../CRDC/dta/crdc_school_finance.dta", clear
drop if substr(combokey,1,2)=="04"
replace combokey = "0" + combokey if length(combokey) == 11
merge m:1 combokey using "../CRDC/dta/crdc_crosswalk_schools.dta", gen(_crosswalk)
compress 

tab _crosswalk year 
 /*             merge |      2009       2011       2013       2015       2017 |     Total
----------------------+-------------------------------------------------------+----------
      Master only (1) |       477        108        281        219          0 |     1,085 
          Matched (3) |     7,499      9,756      9,602      9,919     10,121 |    46,897 
----------------------+-------------------------------------------------------+----------
                Total |     7,976      9,864      9,883     10,138     10,121 |    47,982 */

g ncessch = nces_sch1516 if year==2015
replace ncessch= nces_sch1718 if year==2017
replace ncessch= nces_sch1112 if year==2011 // precisely match on year
replace ncessch= nces_sch1415 if year==2013
replace ncessch= nces_sch1112 if year==2009 // these two just nearest year

foreach yr in 1112 1415 1516 1617 1718 1819 {
	replace ncessch = nces_sch`yr' if ncessch=="" & _crosswalk~=1	
}

foreach var of varlist _all {
    capture confirm string variable `var'
    if !_rc {
        replace `var' = lower(`var')
    }
}

sort sch_name year lea_zip

g nces_yes = ncessch~="" 
replace sch_name = strtrim(sch_name)
replace lea_zip = strtrim(lea_zip)
bysort sch_name lea_zip: egen nces_yes_ever = max(nces_yes)

count if nces_yes==0 & nces_yes_ever==1
count if ncessch=="" // 197/1206 ever exist under the same school name and same lea zip code

bysort sch_name lea_zip: replace ncessch =  ncessch[_n-1] if ncessch=="" & nces_yes==0 & nces_yes_ever==1
bysort sch_name lea_zip: replace ncessch =  ncessch[_n+1] if ncessch=="" & nces_yes==0 & nces_yes_ever==1

replace ncessch=combokey if ncessch==""
keep lea_state-expend fte_teachers_fin -lea_zip ncessch _crosswalk
order ncessch combokey year sch_name lea_name
drop lea_state schid


*** 2) generate two main variables --- total spending, total instructor salaries 
* 2009 & 2011; data has total salary, instructor salary (both teacher and aid), and nonsalary
foreach x in tot_salaries inst_salaries expend {
	replace `x'=. if `x'<0 
}
su tot_salarie expend
g exp_wfed = tot_salaries + expend
g sal_inst_wfed = inst_salaries
bysort year : su exp_wfed sal_inst_wfed

* 2015 & 2017; data has salary of different personnel + sourced by Local/state or Federal
foreach x in sch_npe_wfed sch_sal_totpers_wfed sch_sal_teach_wfed sch_sal_aid_wfed {
	replace `x'=. if `x'<0 
}
replace exp_wfed=sch_npe_wfed+sch_sal_totpers_wfed if exp_wfed==. 
replace sal_inst_wfed = sch_sal_teach_wfed + sch_sal_aid_wfed if sal_inst_wfed==.
bysort year : su exp_wfed sal_inst_wfed

* 2013 is the problem; salary and other expenditure sourced by LOCAL and STATE GOV ONLY
foreach x in sch_sal_totpers_wofed sch_npe_wofed sch_sal_instr_wofed sch_sal_teach_wofed sch_sal_aid_wofed {
	replace `x'=. if `x'<0 
}
g exp_wofed = sch_sal_totpers_wofed+sch_npe_wofed // 2013, 15, 17
g sal_inst_wofed = sch_sal_instr_wofed // 2013
replace sal_inst_wofed = sch_sal_teach_wofed + sch_sal_aid_wofed if sal_inst_wofed==.
bysort year: su exp_* sal_inst*

la var exp_wfed "CRDC: Spending with Federal Fund"
la var exp_wofed "CRDC: Spending without Federal Fund"
la var sal_inst_wfed "CRDC: Instructional Salary with Federal Fund"
la var sal_inst_wofed "CRDC: Instructional Salary without Federal Fund"

bysort ncessch year: g N=_N
br if N>=2

drop if N==2 & exp_wfed==0 & exp_wofed==0
duplicates drop ncessch year exp_wfed exp_wofed, force
duplicates r ncessch year 
// 89 obs our of ~48000 that has duplicates with different level of expenditure
// don't know what's going on
drop if N>=2 // 89


*** 3) some data cleaning // I think the original data is super messy
* only possible to check in 2015 & 2017 if w and w/t fed funding, which is higher
count if exp_wfed<exp_wofed & exp_wofed<. //3,270
count if exp_wfed>=exp_wofed & exp_wfed<. //16,661

replace sch_npe_wfed = sch_npe_wofed if sch_npe_wfed==0 & sch_npe_wofed~=.
replace sch_sal_totpers_wfed =sch_sal_totpers_wofed if sch_sal_totpers_wfed==0 & sch_sal_totpers_wofed ~=.

g exp_wfed_adj = tot_salaries + expend
replace exp_wfed_adj=sch_npe_wfed+sch_sal_totpers_wfed if exp_wfed_adj==. // 2015 + 2017

count if exp_wfed_adj<exp_wofed & exp_wofed<. // 2,944
count if exp_wfed_adj>exp_wofed & exp_wfed_adj<. // 15,355

keep ncessch year sch_name lea_name leaid combokey exp* sal_inst*

save "../CRDC/dta/crdc_school_finance_cleaned.dta", replace
 
 
use "../CCD/ccd_school_cleaned.dta", clear
merge 1:m ncessch year using "../CRDC/dta/crdc_school_finance_cleaned.dta", gen (_crdc)
tab year _crdc if inlist(year, 2009, 2011, 2013, 2015, 2017) 
/*	CCD School: Academic |    Matching result from merge
year (fall semester) | Master on  Using onl  Matched ( |     Total
---------------------+---------------------------------+----------
                2009 |     2,546        227      7,749 |    10,522 
                2011 |       613        121      9,741 |    10,475 
                2013 |       821        227      9,654 |    10,702 
                2015 |       500        159      9,973 |    10,632 
                2017 |       404         74     10,044 |    10,522 
---------------------+---------------------------------+----------
               Total |     4,884        808     47,161 |    52,853 
			   
what are this using-only schools? adult, pre-k, charter, special ed */
drop if _crdc==2
	 
	 
********************************************************************************
* II. Then ESSA (ESSA: 2018-2022, annual)
********************************************************************************

*** later years' seasch has the form of district id+school id; that of earlier years' school id only
gen after_dash = ""
gen dash_pos = strpos(seasch, "-")
replace after_dash = substr(seasch, dash_pos + 1, .) if dash_pos > 0
replace after_dash = seasch if dash_pos == 0
drop dash_pos seasch
ren after_dash seasch 

duplicates l id_district_state seasch year, c
bysort id_district_state seasch year: g N=_N
drop if _enroll==1 & N>=2 
drop N

egen schoolcdscode = concat(id_district_state seasch)
drop if year<=2008

duplicates r schoolcdscode year
duplicates r ncessch year

merge 1:1 schoolcdscode year using "../ESSA/dta/essa_school_expenditures.dta", gen(_essa) 
ren (ncessch schoolcdscode) (id_sch_nces id_sch_state)
order id_*

tab year _essa if year>=2018 
/* CCD School: Academic |    Matching result from merge
year (fall semester) | Master on  Using onl  Matched ( |     Total
---------------------+---------------------------------+----------
                2018 |     1,418          5      9,132 |    10,555 
                2019 |     1,454          8      9,024 |    10,486 
                2020 |     1,314         14      9,098 |    10,426 
                2021 |     1,358          6      9,098 |    10,462 
                2022 |     1,305         10      9,111 |    10,426 
---------------------+---------------------------------+----------
               Total |     6,849         43     45,463 |    52,355 */
drop if _essa==2
duplicates r id_sch_nces year
duplicates r id_sch_state year


********************************************************************************
* III. Then SFP (199899-200708, annual)
********************************************************************************
	
	gen school_code = substr(id_sch_state,8,.)
	duplicates r school_code year
	duplicates drop school_code year, force //16 --- same school but state change within an academic year
	
	g test_year = year+1
	merge 1:1 school_code test_year using "../SFP/nospending_replication/data/clean/clean_sfp_school.dta", gen(_sfp)
	drop test_year // SFP (Fischer)
	
	tab year _sfp // what are these using only cases?
 	
********************************************************************************
* IV. Generate Variables
********************************************************************************

	ren id_district* id_dist* 

	*** 1) enrollment 
	ds enrollment_*
	local varlist = r(varlist)
	foreach x in `varlist' enrollment {
		replace `x' =. if `x'<0
	}

	g hisp_share = enrollment_sch_hisp/enrollment 
	replace free_or_reduced_price_lunch=. if free_or_reduced_price_lunch<0
	g frl_share =  free_lunch/enrollment

	bysort id_dist_nces: egen enroll_total = sum(enrollment)
	bysort id_dist_nces: egen hisp_share_avg = sum(hisp_share * (enrollment/enroll_total))
	g hisp_high = cond(hisp_share>hisp_share_avg,1,0)
	bysort id_dist_nces: egen frl_share_avg = sum(frl_share * (enrollment/enroll_total))
	g frl_high = cond(frl_share>frl_share_avg,1,0)
	tab1 hisp_high frl_high

	
	*** 2) total expenditure
	g exp_per_stu = exp_wfed/enrollment
	replace exp_per_stu = school_expenditures_f+school_expenditures_sl if missing(exp_per_stu) 
	replace exp_per_stu=exp_per_stu
	la var exp_per_stu "CRDC & ESSA: total expenditure per pupil (w fed)"

	g exp_per_stu_v2 = cond(!missing(exp_per_stu),exp_per_stu,exp_wofed/(enrollment))
	la var exp_per_stu_v2 "CRDC & ESSA: total expenditure per pupil (w/t fed only in 2013)"

	g exp_per_stu_v3 = exp_wfed_adj/enrollment
	replace exp_per_stu_v3 = school_expenditures_f+school_expenditures_sl if missing(exp_per_stu_v3) 
	replace exp_per_stu_v3=exp_per_stu_v3
	la var exp_per_stu_v3 "CRDC & ESSA: total expenditure per pupil (w fed after data cleaning)"

	g sal_inst_per_stu = sal_inst_wfed/enrollment
	g sal_inst_per_stu_v2 = cond(!missing(sal_inst_per_stu), sal_inst_per_stu, sal_inst_wofed/enrollment)

	* gen per-pupil vars for funds and CPI adjust
	foreach var of varlist fund_* {
		gen `var'_pp = `var' / enrollment
	}
	
	merge m:1 year using  "cpi.dta", keep(matched) nogen

	ds exp_per_stu* sal_inst_per_stu* fund_*
 
	local varlist = r(varlist)
	foreach x in `varlist' {
		replace `x' = `x'* cpi
		g log_`x' = log(`x')
	}

	la var log_exp_per_stu "CRDC & ESSA: log total expenditure per pupil (w fed)"
	la var log_exp_per_stu_v2 "CRDC & ESSA: log total expenditure per pupil(w/t fed only in 2013)"
	la var log_exp_per_stu_v3 "CRDC & ESSA: log total expenditure per pupil (w fed after data cleaning)"
	la var log_sal_inst_per_stu "CRDC & ESSA: log total instructional salary per pupil (w fed)"
	la var log_sal_inst_per_stu_v2 "CRDC & ESSA: log  total instructional salary per pupil(w/t fed only in 2013)"

	destring id_dist_nces id_dist_state, replace

	replace school_name = lower(school_name)

	ssc install winsor2
	winsor2 exp_per_stu, cuts(1 99)  // Winsorizes at 1st and 99th percentiles
	winsor2 exp_per_stu_v2, cuts(1 99)  // Winsorizes at 1st and 99th percentiles
	winsor2 exp_per_stu_v3, cuts(1 99)  // Winsorizes at 1st and 99th percentiles
	winsor2 fund_total_pp, cuts(0 99)
	g log_exp_per_stu_w = log(exp_per_stu_w)
	g log_exp_per_stu_v2_w = log(exp_per_stu_v2_w)
	g log_exp_per_stu_v3_w = log(exp_per_stu_v3_w)

	su exp*per_stu enrollment
	
	count if id_dist_nces==.
	drop if id_dist_nces==. // basically only SFP observations
	save "outcomes_sch_finance.dta", replace

log c
