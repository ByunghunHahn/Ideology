	*** Run regressions with priorities
	foreach x in $list_prio {	
    // Replace missing with zeros		
	gen T_`x' = (m_`x' >= 0 & !missing(m_`x'))
    gen m2_`x' = cond(missing(m_`x'),0,m_`x')
	
	// Generate RD sample indicator
    rdrobust n_pass_`x' m_`x' if year>=year_elected+1, p(1) kernel(triangular) bwselect(mserd) covs(yr_*) vce(nncluster id_election)
    local bw = e(h_l)
    gen S_`x' = (abs(m_`x') <= `bw')
    
    // Create interactions	
	g T_`x'_S = T_`x' * S_`x'
	g m_`x'_S = m2_`x' * S_`x'	
	g int_`x' = T_`x'_S*m_`x'_S

    // Regression on full sample
    if strpos("agenda_bias budget_hawk", "`x'") { // reverse for a subset of variables
		replace T_`x'_S = -T_`x'_S
		replace int_`x' = -int_`x'
		replace m_`x'_S = -m_`x'_S
		reg n_pass_`x' T_`x'_S m_`x'_S int_`x' S_`x' ib(2014).year if year>=year_elected+1
		est store M_`x'
	}
	else {
		reg n_pass_`x' T_`x'_S m_`x'_S int_`x' S_`x' ib(2014).year if year>=year_elected+1
		est store M_`x'
	}	
}	


	preserve 
	use "Results/first_stage_no_missing", clear
	drop if predicted=="yes"
	drop if balance=="yes"
	keep if strpos("female hisp occ_teacher democrat_v2", demo) | prio==demo
	drop if prio==""
	ren coef coef_p
	ren pval pval_p
	save "Temp/first_stage_no_missing.dta", replace
	restore
		
	*** Run regressions with identities
	foreach x in $list_demo {
    // Replace missing with zeros		
	gen T_`x' = (m_`x' >= 0 & !missing(m_`x'))
    gen m2_`x' = cond(missing(m_`x'),0,m_`x')
			
		foreach y in $list_prio {
		rdrobust n_pass_`y' m_`x' if year>=year_elected+1, p(1) kernel(triangular) bwselect(mserd) covs(yr_*) vce(nncluster id_election)
	    local bw = e(h_l)	
		gen S_`y'_`x' = (abs(m_`x') <= `bw')
		
		// Create interactions	
		g T_`y'_`x'_S = T_`x' * S_`y'_`x'
		g m_`y'_`x'_S = m2_`x' * S_`y'_`x'	
		g int_`y'_`x' = T_`y'_`x'_S*m_`y'_`x'_S
		
		// Regression on full sample			
		preserve
		use "Temp/first_stage_no_missing.dta", replace
		su coef_p if strpos(prio, "`y'") & strpos(demo, "`x'")
		scalar b_T= r(mean)
		restore

			
		if strpos("budget_hawk", "`y'") & strpos("hisp", "`x'")  {
			scalar b_T = -1
		}
		if strpos("equity", "`y'") & strpos("hisp", "`x'")  {
			scalar b_T = 1
		}		
		if strpos("teacher", "`y'") & strpos("occ_teacher", "`x'")  {
			scalar b_T = 1
		}			
		
		if strpos("agenda_bias budget_hawk", "`y'") {
			scalar b_T = -b_T
		}

		if (b_T < 0) { // reverse if b_T is negative --- otherwise var-cov matrix of abs(\beta) should be calculated, which is not possible because abs() is not a differentiable function.
			replace T_`y'_`x'_S = -T_`y'_`x'_S
			replace int_`y'_`x' = -int_`y'_`x'
			replace m_`y'_`x'_S = -m_`y'_`x'_S
			reg n_pass_`y' T_`y'_`x'_S m_`y'_`x'_S int_`y'_`x' S_`y'_`x' ib(2014).year if year>=year_elected+1
			est store M_`y'_`x'
		}
		else {
			reg n_pass_`y' T_`y'_`x'_S m_`y'_`x'_S int_`y'_`x' S_`y'_`x' ib(2014).year if year>=year_elected+1
			est store M_`y'_`x'
		}
	}
	} 
