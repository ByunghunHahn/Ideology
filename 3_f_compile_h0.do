********************************************************************************
* In Many Analysis, we group cases into: H0 tested in prior studes, H0 based on 
* 	raw difference in priorities, H0 others.
* This dofile generates the list of such cases somewhat manually so that the rest 
* 	of dofiles can use this manual list.
* Written by Minseon Park 06/04/25
********************************************************************************

global list_demo = "hisp female occ_teacher democrat_v2"
global list_prio ="budget_hawk equity_prior safety_health_prior teacher_care agenda_bias parent_involvement score_concern facility_prior cte_prior dropout_prior enrollment_prior sup_concern"

global list_outcome ="rev_cte_per_stu exp_total_per_stu exp_capital_total_per_stu teacher_exit_dist entry_sal_Bcomp_log ba60_sal_Bcomp_log ecd_test hsp_test log_enrollment sus_share enrollment_charter_share n_charter_sch"

global list_outcome_main ="rev_cte_per_stu exp_total_per_stu bond_amount_per_stu exp_capital_total_per_stu teacher_exit_dist entry_sal_Bcomp_log ba60_sal_Bcomp_log ecd_test hsp_test test log_enrollment sus_share sup_change_cum" // main outcomes to be shown in stacked outcome RD results


********************************************************************************
* 						I. First Stage Hypothesis 
********************************************************************************

	clear all
	local n_prio : word count $list_prio
	local n_demo : word count $list_demo

	set obs `n_prio'

	* Generate "prio" variable from your global macro
	gen prio = ""
	forvalues n = 1/`n_prio' {
		local this_prio : word `n' of $list_prio
		replace prio = "`this_prio'" in `n'
	}

	* Expand each "prio" observation by number of demo types
	expand `n_demo'+1

	* Generate "demo" variable iterating through demo list
	bysort prio: gen demo = ""
	bysort prio: gen demo_index = _n

	forvalues i = 1/`n_demo' {
		local this_demo : word `i' of $list_demo
		replace demo = "`this_demo'" if demo_index == `i'
	}
	replace demo =prio if demo_index == `n_demo'+1
	
	drop demo_index
	order prio demo

	* Define hypothesis type (h_type)
	gen h_type_exact = 0
	merge 1:1 demo prio using Results/bar_reg, keepus(pvalue1 mean3) keep(master matched) nogen
	replace h_type = 2 if pvalue1 <= 0.05 & h_type == 0 // pvalue1 from t-test result
	ren (pvalue1 mean3) (pval_ttest coef_ttest)
	replace h_type_exact = 2 if pval_ttest <= 0.05

	merge 1:m demo prio using "Results/first_stage_with_missing", keepus(coef predicted pval) keep(master matched) nogen
	drop if predicted=="yes" 
	duplicates r demo prio
	drop predicted
	ren (coef pval) (coef_first_stg pval_first_stg)
	
	save "Temp/h0_list_first_stage.dta", replace

	
********************************************************************************
* 						II. Priorities -> Outcomes
********************************************************************************
		
	clear all
	local n_prio : word count $list_prio

	set obs `n_prio'

	* Generate "prio" variable
	gen prio = ""
	forvalues n = 1/`n_prio' {
		local this_prio : word `n' of $list_prio
		replace prio = "`this_prio'" in `n'
	}

	* Assign outcomes based on prio
	gen outcome = ""
	replace outcome = "exp_total_per_stu bond_amount_per_stu" if prio=="budget_hawk"
	replace outcome = "ecd_test hsp_test" if prio=="equity_prior"
	replace outcome = "sus_share" if prio=="safety_health_prior"
	replace outcome = "teacher_exit_dist teacher_tenure entry_sal_Bcomp_log ba60_sal_Bcomp_log enrollment_charter_share n_charter_sch" if prio=="teacher_care"
	replace outcome = "rev_cte_per_stu" if prio=="cte_prior"
	replace outcome = "log_enrollment " if prio=="dropout_prior"
	replace outcome = "log_enrollment" if prio=="enrollment_prior"
	replace outcome = "exp_capital_total_per_stu" if prio=="facility_prior"
	replace outcome = "test" if prio=="score_concern"
	replace outcome = "sup_change_cum" if prio=="sup_concern"
	
	* Expand rows so each outcome is in a separate row
	split outcome, gen(outcome_)
	drop outcome

	reshape long outcome_, i(prio) j(seq)
	drop if outcome_==""

	rename outcome_ outcome
	order prio outcome
	sort prio outcome
	drop seq
	
	gen subset = ""
	replace subset = "& year<=2017" if strpos("exp_total_per_stu exp_capital_total_per_stu rev_cte_per_stu", outcome)
	replace subset = "& year<=2017 & agency_charter_indicator~=1" if strpos("enrollment_charter_share n_charter_sch", outcome)
	
	g sch_level =0 
	replace sch_level=1 if strpos("fund_total_pp_w exp_per_stu_w", outcome)
		
	gen priolabel = "Equity" if prio=="equity_prior"
	replace priolabel = "Teacher Support" if prio=="teacher_care"
	replace priolabel = "Fiscal Conservatism" if prio=="budget_hawk"
	replace priolabel = "Safety" if prio=="safety_health_prior"
	replace priolabel = "Dropout" if prio=="dropout_prior"
	replace priolabel = "CTE" if prio=="cte_prior"
	replace priolabel = "Facility Improvement" if prio=="facility_prior"
	replace priolabel = "Test Score" if prio=="score_concern"

	gen outcome_main = 1 if strpos("$list_outcome_main", outcome)
	
	save "Temp/h0_list_prio_outcome.dta", replace
	

	use "Temp/h0_list_first_stage.dta", clear
	joinby prio using "Temp/h0_list_prio_outcome.dta", unmatched(master)
	duplicates r demo outcome // duplicates cases - when multiple priorities are matched to the same outcome
	
	replace h_type_exact=1 if (demo=="hisp" & outcome=="hsp_test")| (demo=="hisp" & outcome=="exp_total_per_stu") | (demo=="hisp" & outcome=="bond_amount_per_stu") | (demo=="occ_teacher" & outcome=="ba60_sal_Bcomp_log") | (demo=="occ_teacher" & outcome=="entry_sal_Bcomp_log") | (demo=="occ_teacher" & outcome=="teacher_exit_dist")
	*(demo=="occ_teacher" & outcome=="enrollment_charter_share") | (demo=="occ_teacher" & outcome=="n_charter_sch") |
	la de h_type_exact 0 "others" 1 "exact outcomes in previous studies" 2 "sig. raw diff"
	la val h_type_exact h_type_exact 
	
	g coef_sign = 1 
	replace coef_sign = -1 if coef_first_stg<0
	replace coef_sign = -coef_sign if inlist(outcome,"exp_total_per_stu", "teacher_exit_dist", "bond_amount_per_stu")
	replace coef_sign = 1 if h_type_exact==1 & inlist(outcome,"teacher_exit_dist")==0
	
	save "Temp/h0_list_demo_outcome.dta", replace

