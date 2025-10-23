	*** Run regressions with priorities
foreach x in $list_prio {	
    // Create treatment indicator and clean missing values
	gen T_`x' = (m_`x' >= 0 & !missing(m_`x'))
    gen m2_`x' = cond(missing(m_`x'), 0, m_`x')
	
	// Get the outcome(s) associated with this priority
	preserve
	use "Temp/prio_outcomes_list.dta", clear
	keep if strpos(prio, "`x'")
	levelsof y_list, local(yvals)
	levelsof fiscal, local(fiscal)
	if `fiscal'==1 {
		local yearcond="year>=year_elected & year<=2017"
	}
	if `fiscal'~=1 {
		local yearcond="year>=year_elected"
	}
	restore
	
	// Now loop over the extracted y values
	tokenize `yvals'
	while "`1'" != "" {
    local y `1'
    di "Running regression for outcome `y' using priority `x'"
	forvalues n=1/3 {
	// Generate RD sample indicator
    capture noisily rdrobust `y'_D m_`x' if `yearcond' & comp_`x'==`n', ///
    p(1) kernel(uniform) bwselect(mserd) covs(yr_*) vce(hc0)

	if _rc != 0 {
		// fallback: manually set bandwidth if rdrobust fails
		local bw = 5
	}
	else {
		local bw = e(h_l)
	}

	gen S_`y'_`x'`n' = (abs(m_`x') <= `bw')

    // Create interactions	
	g T_`y'_`x'_S`n' = T_`x' * S_`y'_`x'`n'
	g m_`y'_`x'_S`n' = m2_`x' * S_`y'_`x'`n'	
	g int_`y'_`x'`n' = T_`y'_`x'_S`n'*m_`y'_`x'_S`n'
	
    // Regression on full sample
    if strpos("log_exp_pp tch_exit_dist", "`y'") { // reverse for a subset of variables
		replace T_`y'_`x'_S`n' = -T_`y'_`x'_S`n'
		replace int_`y'_`x'`n' = -int_`y'_`x'`n'
		replace m_`y'_`x'_S`n' = -m_`y'_`x'_S`n'
		reg `y'_D T_`y'_`x'_S`n' m_`y'_`x'_S`n' int_`y'_`x'`n' S_`y'_`x'`n' ib(2014).year if `yearcond'& comp_`x'==`n'
		est store `y'_`x'`n'
	}
	else {
		reg `y'_D T_`y'_`x'_S`n' m_`y'_`x'_S`n' int_`y'_`x'`n' S_`y'_`x'`n' ib(2014).year if `yearcond'& comp_`x'==`n'
		est store `y'_`x'`n'
	}	
	}
    macro shift
}
}


	preserve 
	use "Results/first_stage_no_missing", clear
	drop if predicted=="yes"
	drop if balance=="yes"
	rename demo x // **** AG 05/12 edit 
	rename prio y // **** AG 05/12 edit 
	keep if strpos("female hisp occ_teacher democrat_v2", x) | y==x
	drop if y==""
	ren coef coef_p
	ren pval pval_p
	save "Temp/first_stage_no_missing.dta", replace
	restore
	
	*** Run regressions with identities
	foreach x in $list_demo {
    // Replace missing with zeros		
	gen T_`x' = (m_`x' >= 0 & !missing(m_`x'))
    gen m2_`x' = cond(missing(m_`x'),0,m_`x')
			
	// Now loop over the extracted y values
	foreach y in tch_exit_dist tch_tenure entry_sal_ln ba60_sal_ln ecd_test hsp_test log_enroll sus_share rev_cte_pp log_exp_pp exp_cptl_pp {
		di "Running regression for outcome `y' using priority `x'"
	if strpos("rev_cte_pp log_exp_pp exp_cptl_pp", "`y'")==1 {
		local yearcond="year>=year_elected & year<=2017"
	}
	if strpos("rev_cte_pp log_exp_pp exp_cptl_pp", "`y'")==0 {
		local yearcond="year>=year_elected"
	}

	// y-> prio, then prio-> iden
	preserve
	use "Temp/prio_outcomes_list.dta", replace
	levelsof prio_raw if strpos(y_list, "`y'"), local(prio)
	restore
	
	preserve
	use "Temp/first_stage_no_missing.dta", replace
	gen match = 0
	foreach p of local prio {
		replace match = 1 if y == "`p'" & strpos(x, "`x'")
	}
	su coef_p if match==1
	scalar b_T= r(mean)
	restore
		
	// Generate RD sample indicator
	forvalues n=1/3 {	
		
	// Generate RD sample indicator
    capture noisily rdrobust `y'_D m_`x' if `yearcond' & comp_`x'==`n', ///
    p(1) kernel(uniform) bwselect(mserd) covs(yr_*) vce(hc0)

	if _rc != 0 {
		// fallback: manually set bandwidth if rdrobust fails
		local bw = 5
	}
	else {
		local bw = e(h_l)
	}

	gen S_`y'_`x'`n' = (abs(m_`x') <= `bw')
	
    // Create interactions	
	g T_`y'_`x'_S`n' = T_`x' * S_`y'_`x'`n'
	g m_`y'_`x'_S`n' = m2_`x' * S_`y'_`x'`n'	
	g int_`y'_`x'`n' = T_`y'_`x'_S`n'*m_`y'_`x'_S`n'
			
	if strpos("log_exp_pp tch_exit_dist", "`y'") {
		replace T_`y'_`x'_S`n' = -T_`y'_`x'_S`n'
		replace int_`y'_`x'`n' = -int_`y'_`x'`n'
		replace m_`y'_`x'_S`n' = -m_`y'_`x'_S`n'
		reg `y'_D T_`y'_`x'_S`n' m_`y'_`x'_S`n' int_`y'_`x'`n' S_`y'_`x'`n' ib(2014).year if `yearcond' & comp_`x'==`n'
		est store `y'_`x'`n'
	}
	else {
		reg `y'_D T_`y'_`x'_S`n' m_`y'_`x'_S`n' int_`y'_`x'`n' S_`y'_`x'`n' ib(2014).year if `yearcond' & comp_`x'==`n'
		est store `y'_`x'`n'
	}	
	}
	}
} 

