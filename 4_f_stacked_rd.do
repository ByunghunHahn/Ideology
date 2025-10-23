capture program drop stacked_rd
program define stacked_rd
    syntax, yearcond(string) se_type(string) 
	
	*** Run regressions with priorities
foreach x in $list_prio {	
    // Create treatment indicator and clean missing values
	gen T_`x' = (m_`x' >= 0 & !missing(m_`x'))
    gen m2_`x' = cond(missing(m_`x'), 0, m_`x')
	
	// Get the outcome(s) associated with this priority
	preserve
	use "Temp/h0_list_demo_outcome.dta" if outcome_main==1, clear
	keep if prio=="`x'"
	levelsof outcome, local(raw_yvals)
	local yvals : subinstr local raw_yvals `"""' "", all
	local yvals : subinstr local yvals "'" "", all
	local yvals : subinstr local yvals "`" "", all
	levelsof subset, local(raw_subset)
	local subset : subinstr local raw_subset `"""' "", all
	restore

	// Now loop over the extracted y values
	foreach y of local yvals {
	local yy = substr("`y'", 1, 6)

	// Generate RD sample indicator
    rdrobust `y'_D m_`x' if `yearcond' `subset', p(1) kernel(uniform) bwselect(mserd) covs(yr_*) vce(`se_type')
    local bw = e(h_l)
	gen S_`yy'`x' = (abs(m_`x') <= `bw')
    
    // Create interactions	
	g T_`yy'`x'_S = T_`x' * S_`yy'`x'
	g m_`yy'`x'_S = m2_`x' * S_`yy'`x'	
	g int_`yy'`x' = T_`yy'`x'_S*m_`yy'`x'_S
	
    // Regression on full sample
    if strpos("exp_total_per_stu teacher_exit_dist", "`y'") { // reverse for a subset of variables
		replace T_`yy'`x'_S = -T_`yy'`x'_S
		replace m_`yy'`x'_S = -m_`yy'`x'_S
		replace int_`yy'`x' = -int_`yy'`x'
		
		reg `y'_D T_`yy'`x'_S m_`yy'`x'_S int_`yy'`x' S_`yy'`x' ib(2014).year if `yearcond' `subset'
		est store M_`yy'`x'
	}
	else {
		reg `y'_D T_`yy'`x'_S m_`yy'`x'_S int_`yy'`x' S_`yy'`x' ib(2014).year if `yearcond' `subset'
		est store M_`yy'`x'
	}	
    macro shift
}
}
	
	*** Run regressions with identities	
	foreach x in $list_demo {
    // Replace missing with zeros		
	gen T_`x' = (m_`x' >= 0 & !missing(m_`x'))
    gen m2_`x' = cond(missing(m_`x'),0,m_`x')
			
	// Now loop over the extracted y values
	foreach y in $y_rev $y_fiscal $y_teacher $y_score $y_enroll $y_behv {
	di "Running regression for outcome `y' using identity `x'"
	local yy = substr("`y'", 1, 6)
	
	// y-> prio, then prio-> iden
	preserve
	use "Temp/h0_list_demo_outcome.dta" if outcome_main==1 & strpos(demo, "`x'") & strpos(outcome,"`y'"), clear
	su coef_ttest  
	scalar b_T= r(mean)		
	if strpos("exp_total_per_stu teacher_exit_dist", "`y'") { // change the sign for two outcomes variables
		scalar b_T = -b_T
		display b_T
	}
	if coef_sign != . {
		scalar b_T = coef_sign // if tested in prior studies, fix sign
		display b_T
	}	
	levelsof subset, local(raw_subset)
	local subset : subinstr local raw_subset `"""' "", all	
	
	count if h_type_exact == 1
	local ht_suffix = cond(r(N) > 0, "_ht1", "")
	restore
		
	// Generate RD sample indicator
    rdrobust `y'_D m_`x' if `yearcond' `subset', p(1) kernel(uniform) bwselect(mserd) covs(yr_*) vce(`se_type')
    local bw = e(h_l)
	gen S_`yy'`x' = (abs(m_`x') <= `bw')
    
    // Create interactions	
	g T_`yy'`x'_S = T_`x' * S_`yy'`x'
	g m_`yy'`x'_S = m2_`x' * S_`yy'`x'	
	g int_`yy'`x' = T_`yy'`x'_S*m_`yy'`x'_S
	
    // Regression on full sample
	if (b_T < 0) { // reverse if b_T is negative --- otherwise var-cov matrix of abs(\beta) should be calculated, which is not possible because abs() is not a differentiable function.
		replace T_`yy'`x'_S = -T_`yy'`x'_S
		replace m_`yy'`x'_S = -m_`yy'`x'_S
		replace int_`yy'`x' = -int_`yy'`x'

		reg `y'_D T_`yy'`x'_S m_`yy'`x'_S int_`yy'`x' S_`yy'`x' ib(2014).year if `yearcond' `subset'
		est store M_`yy'`x'`ht_suffix'
	}
	else {
		reg `y'_D T_`yy'`x'_S m_`yy'`x'_S int_`yy'`x' S_`yy'`x' ib(2014).year if `yearcond' `subset'
		est store M_`yy'`x'`ht_suffix'
	}	
	}
}
end
