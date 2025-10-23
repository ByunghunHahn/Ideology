********************************************************************************
* Run Super-stacked RD and Draw Bar Graph
* Written By Minseon Park 04/06/2025
* Modified to split identity values based on existing literature
********************************************************************************

set seed 1234
clear all 
* set directory 
cd "~/Dropbox/California Election Data/Code"
global logpath "~/Dropbox/California Election Data/Logs"

global list_demo = "hisp female occ_teacher democrat_v2 "
global list_prio ="budget_hawk equity safety teacher_care involve facility cte dropout agenda_bias" // names of variables are shortend o.w. stata gives error that the model names are too long, enrollment is further dropped since current motion data do not have it as a keyword

global missing="with_missing"
global datapath = "data_for_rd/$missing"
global mindta "~/Dropbox/California Election Data/Minutes/data/dta"

global flag_abs = 2 // 1: get mean of abs(RD coeff); 2: get mean of RD coeff
// ver1 checks if identities/ideologies affect n of motions passed at all
// on the contrary, ver2 considers the case of "wrong sign". when the coeff has wrong signs, then the coefficient is negative and that negative value is used to calculate the mean

********************************************************************************
* I. Run Super-stacked RD and Bar Graph
********************************************************************************

	eststo clear
	use "$datapath/dist_stacked.dta", clear
	
	drop if dist_cnty==1
	
	tostring id_district_nces, replace  // Convert to string if not already	
	gen leaid = string(real(id_district_nces), "%07.0f")  // Adjust "8" to the required width
	tab year, g(yr_)
	egen id_election = group(leaid year_elected)
	
	merge m:1 leaid year using "$mindta/minutes_final_year_level.dta", keep(master matched) gen(_minute)
	
	ren *safety_health_prior* *safety_prior* // shorten names not to get an error
	ren *parent_involvement_prior* *involve_prior*
	ren *_prior* **
	ren n_pass_engagement n_pass_involve
	
	// want to squeeze these two?
	g n_pass_cte = n_pass_agenda_bias
	g n_pass_dropout = n_pass_academic
	
	/*foreach x in budget_hawk equity safety teacher_care involve facility cte dropout agenda_bias {
		bysort year: egen z=std(n_pass_`x' )
		replace n_pass_`x' = z
		drop z
	}*/
	
	
	
	if $flag_abs==1 {
	do 5_f_stacked_rd_ver1
	}
	if $flag_abs==2 {
	do 5_f_stacked_rd_ver2
	}

********************************************************************************
* II. Run Seemingly Unrelated Regression
********************************************************************************
	
	estimates dir
	local all_models `r(names)'
	
	local sum_iden_lit = ""    // Identity combinations from existing literature
	local sum_iden_other = ""  // Other identity combinations
	local sum_ideo = ""        // Ideology variables
	local treat_iden_lit = ""
	local treat_iden_other = ""
	local treat_ideo = ""
	local count_iden_lit = 0
	local count_iden_other = 0
	local count_ideo = 0

	foreach y in $list_prio {
		foreach x in $list_demo {
			local treat_this = "T_`y'_`x'_S"
			
			// Check if this combination is from existing literature
			local is_existing_lit = 0
			
			// Define combinations from existing literature
			if ("`y'" == "teacher_care" & "`x'" == "occ_teacher") {
				local is_existing_lit = 1
			}
			else if ("`y'" == "equity" & "`x'" == "hisp") {
				local is_existing_lit = 1
			}
			else if ("`y'" == "budget_hawk" & "`x'" == "hisp") {
				local is_existing_lit = 1
			}
			
			// Assign to appropriate category
			if (`is_existing_lit' == 1) {
				// Identity combinations from existing literature
				if "`sum_iden_lit'" == "" {
					local sum_iden_lit "[M_`y'_`x'_mean]`treat_this'"
				}
				else {
					local sum_iden_lit "`sum_iden_lit' + [M_`y'_`x'_mean]`treat_this'"
				}
				
				if "`treat_iden_lit'" == "" {
					local treat_iden_lit "M_`y'_`x'_mean:`treat_this'"
				}
				else {
					local treat_iden_lit "`treat_iden_lit' M_`y'_`x'_mean:`treat_this'"
				}
				
				local count_iden_lit = `count_iden_lit' + 1
			}
			else {
				// Other identity combinations
				if "`sum_iden_other'" == "" {
					local sum_iden_other "[M_`y'_`x'_mean]`treat_this'"
				}
				else {
					local sum_iden_other "`sum_iden_other' + [M_`y'_`x'_mean]`treat_this'"
				}
				
				if "`treat_iden_other'" == "" {
					local treat_iden_other "M_`y'_`x'_mean:`treat_this'"
				}
				else {
					local treat_iden_other "`treat_iden_other' M_`y'_`x'_mean:`treat_this'"
				}
				
				local count_iden_other = `count_iden_other' + 1
			}
		}
    
		local treat_this = "T_`y'_S"
		
		if "`sum_ideo'" == "" {
			local sum_ideo "[M_`y'_mean]`treat_this'"
		}
		else {
			local sum_ideo "`sum_ideo' + [M_`y'_mean]`treat_this'"
		}
		
		if "`treat_ideo'" == "" {
			local treat_ideo "M_`y'_mean:`treat_this'"
		}
		else {
			local treat_ideo "`treat_ideo' M_`y'_mean:`treat_this'"
		}
		
		local count_ideo = `count_ideo' + 1
	}
	
	suest `all_models', vce(cl id_election)
	
	lincom (`sum_iden_lit')/`count_iden_lit'
	scalar theta_unw_iden_lit = r(estimate)
	scalar theta_se_unw_iden_lit = r(se)
	
	lincom (`sum_iden_other')/`count_iden_other'
	scalar theta_unw_iden_other = r(estimate)
	scalar theta_se_unw_iden_other = r(se)
	
	lincom (`sum_ideo')/`count_ideo'
	scalar theta_unw_ideo = r(estimate)
	scalar theta_se_unw_ideo = r(se)
	
	
********************************************************************************
* III. Calculate Precision-weighted Means
********************************************************************************
	
	mata:
	b = st_matrix("e(b)")
	V = st_matrix("e(V)")
	names = st_matrixcolstripe("e(b)")

	iden_lit_treats = tokens("`treat_iden_lit'")
	iden_other_treats = tokens("`treat_iden_other'")
	ideo_treats = tokens("`treat_ideo'")

	iden_lit_indices = J(0,1,.)
	iden_other_indices = J(0,1,.)
	ideo_indices = J(0,1,.)

	for (i=1; i<=cols(b); i++) {
		for (j=1; j<=cols(iden_lit_treats); j++) {
			if (names[i,1] + ":" + names[i,2] == iden_lit_treats[j]) {
				iden_lit_indices = iden_lit_indices \ i
			}
		}
		for (j=1; j<=cols(iden_other_treats); j++) {
			if (names[i,1] + ":" + names[i,2] == iden_other_treats[j]) {
				iden_other_indices = iden_other_indices \ i
			}
		}
		for (j=1; j<=cols(ideo_treats); j++) {
			if (names[i,1] + ":" + names[i,2] == ideo_treats[j]) {
				ideo_indices = ideo_indices \ i
			}
		}
	}

	// Extract identity from literature, other identities, and ideology pieces
	b_iden_lit = b[1, iden_lit_indices]
	V_iden_lit = V[iden_lit_indices, iden_lit_indices]
	
	b_iden_other = b[1, iden_other_indices]
	V_iden_other = V[iden_other_indices, iden_other_indices]

	b_ideo = b[1, ideo_indices]
	V_ideo = V[ideo_indices, ideo_indices]
	
	// -------- WEIGHTED estimates --------
	// Identity from Literature
	one_iden_lit = J(cols(b_iden_lit),1,1)
	Vinv_iden_lit = invsym(V_iden_lit)
	theta_num_iden_lit = one_iden_lit' * Vinv_iden_lit * b_iden_lit'
	theta_denom_iden_lit = one_iden_lit' * Vinv_iden_lit * one_iden_lit
	theta_iden_lit = theta_num_iden_lit[1,1] / theta_denom_iden_lit[1,1]
	theta_var_iden_lit = 1 / theta_denom_iden_lit[1,1]
	theta_se_iden_lit = sqrt(theta_var_iden_lit)
	
	// Other Identities
	one_iden_other = J(cols(b_iden_other),1,1)
	Vinv_iden_other = invsym(V_iden_other)
	theta_num_iden_other = one_iden_other' * Vinv_iden_other * b_iden_other'
	theta_denom_iden_other = one_iden_other' * Vinv_iden_other * one_iden_other
	theta_iden_other = theta_num_iden_other[1,1] / theta_denom_iden_other[1,1]
	theta_var_iden_other = 1 / theta_denom_iden_other[1,1]
	theta_se_iden_other = sqrt(theta_var_iden_other)
		
	// Ideology
	one_ideo = J(cols(b_ideo),1,1)
	Vinv_ideo = invsym(V_ideo)
	theta_num_ideo = one_ideo' * Vinv_ideo * b_ideo'
	theta_denom_ideo = one_ideo' * Vinv_ideo * one_ideo
	theta_ideo = theta_num_ideo[1,1] / theta_denom_ideo[1,1]
	theta_var_ideo = 1 / theta_denom_ideo[1,1]
	theta_se_ideo = sqrt(theta_var_ideo)

	// -------- Bring back to Stata --------
	st_numscalar("theta_iden_lit", theta_iden_lit)
	st_numscalar("theta_se_iden_lit", theta_se_iden_lit)
	
	st_numscalar("theta_iden_other", theta_iden_other)
	st_numscalar("theta_se_iden_other", theta_se_iden_other)
	
	st_numscalar("theta_ideo", theta_ideo)
	st_numscalar("theta_se_ideo", theta_se_ideo)
	
	st_matrix("b_iden_lit", b_iden_lit)
	st_matrix("b_iden_other", b_iden_other)
	st_matrix("b_ideo", b_ideo)

	end
	
	display "Identity (Existing Literature): " theta_iden_lit
	display "Identity (Existing Literature) Unweighted: " theta_unw_iden_lit
	display "Identity (Other): " theta_iden_other
	display "Identity (Other) Unweighted: " theta_unw_iden_other
	display "Ideology: " theta_ideo	
	display "Ideology Unweighted: " theta_unw_ideo
	
	matlist b_iden_lit // all values should be positive
	matlist b_iden_other // all values should be positive
	matlist b_ideo // all values should be positive

********************************************************************************
* IV. Generate Bar Graph 
********************************************************************************
		
	* Step 1: Build matrix with 6 rows
	matrix graphdata = ( ///
    1, 1, theta_unw_iden_lit, theta_se_unw_iden_lit \ ///
    1, 2, theta_unw_iden_other, theta_se_unw_iden_other \ ///
    1, 3, theta_unw_ideo, theta_se_unw_ideo \ ///
    2, 1, theta_iden_lit, theta_se_iden_lit \ ///
    2, 2, theta_iden_other, theta_se_iden_other \ ///
    2, 3, theta_ideo, theta_se_ideo ///
	)

	* Step 2: Convert matrix to dataset
	clear
	svmat graphdata, names(col)

	rename c3 mean
	rename c4 se

	* Step 3: Add type and group
	gen type = .
	replace type = 1 in 1/3  // Unweighted
	replace type = 2 in 4/6  // Precision Weighted

	gen group = .
	replace group = 1 in 1  // Identity (Existing Literature)
	replace group = 1 in 4  // Identity (Existing Literature)
	replace group = 2 in 2  // Identity (Other)
	replace group = 2 in 5  // Identity (Other)
	replace group = 3 in 3  // Ideology
	replace group = 3 in 6  // Ideology

	label define type 1 "Unweighted" 2 "Precision Weighted"
	label define group 1 "Identities (existing literature)" 2 "Identities (other)" 3 "Ideologies"

	label values type type
	label values group group

	drop c1 c2
	g hi = mean+1.96*se
	g low = mean-1.96*se

	
	
	* save 
	save "Temp/temp_rd_min_bar_data.dta", replace 
	use "Temp/temp_rd_min_bar_data.dta", replace 
	
		twoway (bar mean group if group==1, barwidth(0.7) color(dkorange*2) ///
     mlabel(mean) mlabposition(1) mlabgap(0.5) mlabcolor(black) mlabformat(%9.2f) mlabsize(med)) ///
    (rcap hi low group if group==1, lc(black)) ///
    (bar mean group if group==2, barwidth(0.7) color(dkorange) ///
     mlabel(mean) mlabposition(1) mlabgap(0.5) mlabcolor(black) mlabformat(%9.2f) mlabsize(med)) ///
    (rcap hi low group if group==2, lc(black)) ///
    (bar mean group if group==3, barwidth(0.7) color(ebblue) ///
     mlabel(mean) mlabposition(1) mlabgap(0.5) mlabcolor(black) mlabformat(%9.2f) mlabsize(med)) ///
    (rcap hi low group if group==3, lc(black)) ///
, by(type, note("")) ///
  ytitle("Mean Effect on Number of Motions Passed", size(medlarge)) ///
  legend(order(1 "Identities (existing literature)" 3 "Identities (other)" 5 "Ideologies") ///
         position(6) rows(3) size(small)) ///
  xtitle("") xla(, labcolor(bg) tlength(0))		
  
  
	 * NO PRECISION WEIGHTS
	drop if type==2
	
	twoway (bar mean group if group==1, barwidth(0.7) color(dkorange*2) ///
     mlabel(mean) mlabposition(1) mlabgap(0.5) mlabcolor(black) mlabformat(%9.2f) mlabsize(med)) ///
    (rcap hi low group if group==1, lc(black)) ///
    (bar mean group if group==2, barwidth(0.7) color(dkorange) ///
     mlabel(mean) mlabposition(1) mlabgap(0.5) mlabcolor(black) mlabformat(%9.2f) mlabsize(med)) ///
    (rcap hi low group if group==2, lc(black)) ///
    (bar mean group if group==3, barwidth(0.7) color(ebblue) ///
     mlabel(mean) mlabposition(1) mlabgap(0.5) mlabcolor(black) mlabformat(%9.2f) mlabsize(med)) ///
    (rcap hi low group if group==3, lc(black)), ///
  ytitle("Mean Effect on Number of Motions Passed", size(medlarge)) ///
  legend(order(1 "Identities (existing literature)" 3 "Identities (other)" 5 "Ideologies") ///
         position(6) rows(1)  size(medlarge)) ///
  xtitle("") xla(, labcolor(bg) tlength(0))		
  
  if $flag_abs==1 {
	gr export "Results/Graph/rd_minute_bar_ag.png", replace
	}
	if $flag_abs==2 {
	gr export "Results/Graph/rd_minute_bar_sign_ag.png", replace
	}
	
  
	
	
  
 /*	if $flag_abs==1 {
	gr export "Results/Graph/rd_minute_bar_ag.png", replace
	}
	if $flag_abs==2 {
	gr export "Results/Graph/rd_minute_bar_sign_ag.png", replace
	}*/
