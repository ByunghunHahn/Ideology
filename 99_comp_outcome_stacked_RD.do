********************************************************************************
* Heterogeneity Analysis by Board Composition
* Written by Minseon Park 04/20/25
* Latest updated by Minseon Park 05/04/25
********************************************************************************

clear all
set seed 1234

* set directory 
cd "~/Dropbox/California Election Data/Code"
global logpath "~/Dropbox/California Election Data/Logs"

global list_demo = "hisp female occ_tch democrat_v2 "
global list_prio ="budget_hawk equity safety tch_care facility " //cte dropout enroll are dropped since there are too few cases where these are majority; names of variables are shortend o.w. stata gives error that the model names are too long
// TODO: Find a more systemtic way such as dropping the case if obs<100
global class = "entry ba60"

global missing="with_missing"
global datapath = "data_for_rd/$missing"	

global flag_balance =0
global flag_abs = 2 // 1: get mean of abs(RD coeff); 2: get mean of RD coeff
// ver1 checks if identities/ideologies affect n of motions passed at all
// on the contrary, ver2 considers the case of "wrong sign". when the coeff has wrong signs, then the coefficient is negative and that negative value is used to calculate the mean	.

********************************************************************************
* I. Run Super-stacked RD and Bar Graph
* Note: Section 0 of 4_4_outcome_stacked_RD.do should've been already run
********************************************************************************
	
	global y_rev ="rev_cte_pp"
	global y_fiscal= "log_exp_pp exp_cptl_pp"
	global y_teacher = "tch_exit_dist tch_tenure entry_sal_ln ba60_sal_ln"
	global y_score = "ecd_test hsp_test"
	global y_enroll = "log_enroll"
	global y_behv = "sus_share"	

	eststo clear
	use "$datapath/dist_stacked.dta", clear
	
	drop if dist_cnty==1
	
	tostring id_district_nces, replace  // Convert to string if not already	
	gen leaid = string(real(id_district_nces), "%07.0f")  // Adjust "8" to the required width

	*** merge outcomes
	keep leaid year m_* year_elected state_leaid multi_raceid id_district_nces
	destring leaid, replace
	merge m:1 leaid year using "Temp/outcomes_z.dta", gen(_outcome_z) force keep(master matched)
	
	do 4_f_merge_tsal 
	
	*** merge with composition
	// Ensure consistent width by adding leading zeros
	gen leaid_padded = string(leaid, "%07.0f")  // Adjust "8" to the required width
	drop leaid
	ren leaid_padded leaid
	
	merge m:1 leaid year using "board_composition/board_composition_elections.dta", keep(master matched) gen(_comp)
	
	*** shorten variable names not to get an error
	ren *per_stu* *pp*
	ren *exp_total* *exp*
	ren *_Bcomp* **
	ren *_log *_ln
	ren *teacher* *tch*
	ren *capital_total* *cptl*
	ren *enrollment* *enroll*		
	ren *safety_health_prior* *safety_prior*
	ren *parent_involvement_prior* *involve_prior*
	ren *_prior* **
	
	foreach y in entry_sal_ln ba60_sal_ln entry_sal_Bsal_ln ba60_sal_Bsal_ln {
		egen z=std(`y')
		replace `y'=z
		drop z
	}
	
	*** classification by baseline composition
	g size_estimate_b = size_estimate if year==year_elected-1 
	sort multi_raceid size_estimate_b
	bysort multi_raceid: replace size_estimate_b = size_estimate_b[1]
	
	foreach x in $list_demo $list_prio {
	g `x'_total = 0 
	forvalue n=1/14 {
		replace `x'_total = `x'_total+1 if `x'`n'==1
	}
	g `x'_total2 = 0
	forvalue n=1/14 {
		replace `x'_total2 = `x'_total2+1 if `x'`n'==0 // exclude if invalid platform
	}
	g `x'_share =`x'_total/(`x'_total + `x'_total2)
	
	g `x'_total_b = `x'_total if year==year_elected-1 
	sort multi_raceid `x'_total_b
	bysort multi_raceid: replace `x'_total_b = `x'_total_b[1]
	
	g `x'_total2_b = `x'_total2 if year==year_elected-1 
	sort multi_raceid `x'_total2_b
	bysort multi_raceid: replace `x'_total2_b = `x'_total2_b[1]
	
	g comp_`x' = cond(`x'_total_b==0, 3, cond(`x'_total_b<`x'_total2_b, 1, 2)) if !missing(size_estimate_b)

	la de comp_`x' 1 "Minority among non-Missing" 2 "Majority among non-Missing" 3 "First among non-Missing"
	la val comp_`x' comp_`x'			
	
}
 
	foreach x in $list_prio {
		tab comp_`x' if abs(m_`x')<5
	}
	
	do 4_f_prep_rd	
	do 6_f_stacked_rd_ver2
	

*******************************************************************************
* II. Run Seemingly Unrelated Regression
********************************************************************************
		
	estimates dir
	local all_models `r(names)'

	local iden_min ""
	local iden_maj ""
	local iden_one ""
	local ideo_min ""
	local ideo_maj ""
	local ideo_one ""

	foreach m of local all_models {
		local group = substr("`m'", -1, 1) // last char = 1 or 2

		if strpos("`m'", "hisp") | strpos("`m'", "female") | strpos("`m'", "democrat_v2") | strpos("`m'", "occ_tch") {
			if "`group'" == "1" local iden_min `iden_min' `m'
			else if "`group'" == "2" local iden_maj `iden_maj' `m'
			else if "`group'" == "3" local iden_one `iden_one' `m'
			
		}
		else {
			if "`group'" == "1" local ideo_min `ideo_min' `m'
			else if "`group'" == "2" local ideo_maj `ideo_maj' `m'
			else if "`group'" == "3" local ideo_one `ideo_one' `m'
		}
	}

	* Initialize all macro containers
	foreach prefix in sum treat {
		foreach group in iden_min iden_maj ideo_min ideo_maj {
			local `prefix'_`group' ""
		}
	}

	* Build sums and labels
	foreach group in iden_min ideo_min {
		local models ``group''
		foreach m of local models {
			local m_base = substr("`m'", 1, length("`m'") - 1)
			local treat_this = "T_`m_base'_S1" // treatment var
			local m_mean = "`m'_mean"   // coefficient name

			local sum_expr = "`sum_`group''"
			local treat_expr = "`treat_`group''"

			if "`sum_expr'" == "" {
				local sum_`group' "[`m_mean']`treat_this'"
				local treat_`group' "`m_mean':`treat_this'"
			}
			else {
				local sum_`group' "`sum_expr' + [`m_mean']`treat_this'"
				local treat_`group' "`treat_expr' `m_mean':`treat_this'"
			}
		}
		local count_`group' : word count ``group''
	}

	foreach group in iden_maj ideo_maj {
		local models ``group''
		foreach m of local models {
			local m_base = substr("`m'", 1, length("`m'") - 1)		
			local treat_this = "T_`m_base'_S2" // treatment var
			local m_mean = "`m'_mean"   // coefficient name

			local sum_expr = "`sum_`group''"
			local treat_expr = "`treat_`group''"

			if "`sum_expr'" == "" {
				local sum_`group' "[`m_mean']`treat_this'"
				local treat_`group' "`m_mean':`treat_this'"
			}
			else {
				local sum_`group' "`sum_expr' + [`m_mean']`treat_this'"
				local treat_`group' "`treat_expr' `m_mean':`treat_this'"
			}
		}
		local count_`group' : word count ``group''
	}
	
	foreach group in iden_one ideo_one {
		local models ``group''
		foreach m of local models {
			local m_base = substr("`m'", 1, length("`m'") - 1)		
			local treat_this = "T_`m_base'_S3" // treatment var
			local m_mean = "`m'_mean"   // coefficient name

			local sum_expr = "`sum_`group''"
			local treat_expr = "`treat_`group''"

			if "`sum_expr'" == "" {
				local sum_`group' "[`m_mean']`treat_this'"
				local treat_`group' "`m_mean':`treat_this'"
			}
			else {
				local sum_`group' "`sum_expr' + [`m_mean']`treat_this'"
				local treat_`group' "`treat_expr' `m_mean':`treat_this'"
			}
		}
		local count_`group' : word count ``group''
	}
	

	suest `all_models', vce(cl id_election) 

	foreach g in iden_min iden_maj ideo_min ideo_maj iden_one ideo_one {
		di "Testing: `g'"
		di "`sum_`g''"
		lincom (`sum_`g'')/`count_`g''
		scalar theta_unw_`g' = r(estimate)
		scalar theta_se_unw_`g' = r(se)
	}

********************************************************************************
* III. Calculate Precision-weighted Means
********************************************************************************
	
	mata:
	b = st_matrix("e(b)")
	V = st_matrix("e(V)")
	names = st_matrixcolstripe("e(b)")

	iden_min_treats = tokens("`treat_iden_min'")
	iden_maj_treats = tokens("`treat_iden_maj'")
	iden_one_treats = tokens("`treat_iden_one'")
	ideo_min_treats = tokens("`treat_ideo_min'")
	ideo_maj_treats = tokens("`treat_ideo_maj'")
	ideo_one_treats = tokens("`treat_ideo_one'")

	iden_min_idx = J(0,1,.)
	iden_maj_idx = J(0,1,.)
	iden_one_idx = J(0,1,.)
	ideo_min_idx = J(0,1,.)
	ideo_maj_idx = J(0,1,.)
	ideo_one_idx = J(0,1,.)

	for (i=1; i<=cols(b); i++) {
		coefname = names[i,1] + ":" + names[i,2]
		for (j=1; j<=length(iden_min_treats); j++) {
			if (coefname == iden_min_treats[j]) {
				iden_min_idx = iden_min_idx \ i
			}
		}
		for (j=1; j<=length(iden_maj_treats); j++) {
			if (coefname == iden_maj_treats[j]) {
				iden_maj_idx = iden_maj_idx \ i
			}
		}
		for (j=1; j<=length(iden_one_treats); j++) {
			if (coefname == iden_one_treats[j]) {
				iden_one_idx = iden_one_idx \ i
			}
		}
		for (j=1; j<=length(ideo_min_treats); j++) {
			if (coefname == ideo_min_treats[j]) {
				ideo_min_idx = ideo_min_idx \ i
			}
		}
		for (j=1; j<=length(ideo_maj_treats); j++) {
			if (coefname == ideo_maj_treats[j]) {
				ideo_maj_idx = ideo_maj_idx \ i
			}
		}
		for (j=1; j<=length(ideo_one_treats); j++) {
			if (coefname == ideo_one_treats[j]) {
				ideo_one_idx = ideo_one_idx \ i
			}
		}
	}

	// -------- WEIGHTED estimates --------

	// Identity MIN
	b_iden_min = b[1, iden_min_idx]
	V_iden_min = V[iden_min_idx, iden_min_idx]
	one_iden_min = J(cols(b_iden_min),1,1)
	Vinv_iden_min = invsym(V_iden_min)
	theta_num_iden_min = one_iden_min' * Vinv_iden_min * b_iden_min'
	theta_denom_iden_min = one_iden_min' * Vinv_iden_min * one_iden_min
	theta_iden_min = theta_num_iden_min[1,1] / theta_denom_iden_min[1,1]
	theta_var_iden_min = 1 / theta_denom_iden_min[1,1]
	theta_se_iden_min = sqrt(theta_var_iden_min)

	// Identity MAJ
	b_iden_maj = b[1, iden_maj_idx]
	V_iden_maj = V[iden_maj_idx, iden_maj_idx]
	one_iden_maj = J(cols(b_iden_maj),1,1)
	Vinv_iden_maj = invsym(V_iden_maj)
	theta_num_iden_maj = one_iden_maj' * Vinv_iden_maj * b_iden_maj'
	theta_denom_iden_maj = one_iden_maj' * Vinv_iden_maj * one_iden_maj
	theta_iden_maj = theta_num_iden_maj[1,1] / theta_denom_iden_maj[1,1]
	theta_var_iden_maj = 1 / theta_denom_iden_maj[1,1]
	theta_se_iden_maj = sqrt(theta_var_iden_maj)
	
	// Identity ONE
	b_iden_one = b[1, iden_one_idx]
	V_iden_one = V[iden_one_idx, iden_one_idx]
	one_iden_one = J(cols(b_iden_one),1,1)
	Vinv_iden_one = invsym(V_iden_one)
	theta_num_iden_one = one_iden_one' * Vinv_iden_one * b_iden_one'
	theta_denom_iden_one = one_iden_one' * Vinv_iden_one * one_iden_one
	theta_iden_one = theta_num_iden_one[1,1] / theta_denom_iden_one[1,1]
	theta_var_iden_one = 1 / theta_denom_iden_one[1,1]
	theta_se_iden_one = sqrt(theta_var_iden_one)

	// Ideology MIN
	b_ideo_min = b[1, ideo_min_idx]
	V_ideo_min = V[ideo_min_idx, ideo_min_idx]
	one_ideo_min = J(cols(b_ideo_min),1,1)
	Vinv_ideo_min = invsym(V_ideo_min)
	theta_num_ideo_min = one_ideo_min' * Vinv_ideo_min * b_ideo_min'
	theta_denom_ideo_min = one_ideo_min' * Vinv_ideo_min * one_ideo_min
	theta_ideo_min = theta_num_ideo_min[1,1] / theta_denom_ideo_min[1,1]
	theta_var_ideo_min = 1 / theta_denom_ideo_min[1,1]
	theta_se_ideo_min = sqrt(theta_var_ideo_min)

	// Ideology MAJ
	b_ideo_maj = b[1, ideo_maj_idx]
	V_ideo_maj = V[ideo_maj_idx, ideo_maj_idx]
	one_ideo_maj = J(cols(b_ideo_maj),1,1)
	Vinv_ideo_maj = invsym(V_ideo_maj)
	theta_num_ideo_maj = one_ideo_maj' * Vinv_ideo_maj * b_ideo_maj'
	theta_denom_ideo_maj = one_ideo_maj' * Vinv_ideo_maj * one_ideo_maj
	theta_ideo_maj = theta_num_ideo_maj[1,1] / theta_denom_ideo_maj[1,1]
	theta_var_ideo_maj = 1 / theta_denom_ideo_maj[1,1]
	theta_se_ideo_maj = sqrt(theta_var_ideo_maj)
	
	// Ideology ONE
	b_ideo_one = b[1, ideo_one_idx]
	V_ideo_one = V[ideo_one_idx, ideo_one_idx]
	one_ideo_one = J(cols(b_ideo_one),1,1)
	Vinv_ideo_one = invsym(V_ideo_one)
	theta_num_ideo_one = one_ideo_one' * Vinv_ideo_one * b_ideo_one'
	theta_denom_ideo_one = one_ideo_one' * Vinv_ideo_one * one_ideo_one
	theta_ideo_one = theta_num_ideo_one[1,1] / theta_denom_ideo_one[1,1]
	theta_var_ideo_one = 1 / theta_denom_ideo_one[1,1]
	theta_se_ideo_one = sqrt(theta_var_ideo_one)

	// -------- Bring back to Stata --------
	st_numscalar("theta_iden_min", theta_iden_min)
	st_numscalar("theta_se_iden_min", theta_se_iden_min)
	st_matrix("b_iden_min", b_iden_min)

	st_numscalar("theta_iden_maj", theta_iden_maj)
	st_numscalar("theta_se_iden_maj", theta_se_iden_maj)
	st_matrix("b_iden_maj", b_iden_maj)
	
	st_numscalar("theta_iden_one", theta_iden_one)
	st_numscalar("theta_se_iden_one", theta_se_iden_one)
	st_matrix("b_iden_one", b_iden_one)

	st_numscalar("theta_ideo_min", theta_ideo_min)
	st_numscalar("theta_se_ideo_min", theta_se_ideo_min)
	st_matrix("b_ideo_min", b_ideo_min)

	st_numscalar("theta_ideo_maj", theta_ideo_maj)
	st_numscalar("theta_se_ideo_maj", theta_se_ideo_maj)
	st_matrix("b_ideo_maj", b_ideo_maj)
	
	st_numscalar("theta_ideo_one", theta_ideo_one)
	st_numscalar("theta_se_ideo_one", theta_se_ideo_one)
	st_matrix("b_ideo_one", b_ideo_one)

	end

	display theta_iden_min
	matlist b_ideo_maj
	
	
********************************************************************************
* IV. Generate Bar Graph 
********************************************************************************
			 
	* Step 1: Build matrix with 12 rows (2 types × 6 groups)
	matrix graphdata = ( ///
    1, 1, theta_unw_iden_min, theta_se_unw_iden_min \ ///
    1, 3, theta_unw_ideo_min, theta_se_unw_ideo_min \ ///
    1, 2, theta_unw_iden_maj, theta_se_unw_iden_maj \ ///
    1, 4, theta_unw_ideo_maj, theta_se_unw_ideo_maj \ ///
    1, 5, theta_unw_iden_one, theta_se_unw_iden_one \ ///
    1, 6, theta_unw_ideo_one, theta_se_unw_ideo_one \ ///
    2, 1, theta_iden_min, theta_se_iden_min \ ///
    2, 3, theta_ideo_min, theta_se_ideo_min \ ///
    2, 2, theta_iden_maj, theta_se_iden_maj \ ///
    2, 4, theta_ideo_maj, theta_se_ideo_maj \ ///
    2, 5, theta_iden_one, theta_se_iden_one \ ///
    2, 6, theta_ideo_one, theta_se_ideo_one ///
	)

	* Step 2: Convert matrix to dataset
	clear
	svmat graphdata, names(col)
	rename c3 mean
	rename c4 se
	
	* Step 3: Add type and group
	label define type 1 "Unweighted" 2 "Precision Weighted"
	label define group 1 "Identities (Min)" 3 "Ideologies (Min)" 2 "Identities (Maj)" 4 "Ideologies (Maj)" 5 "Identities (One)" 6 "Ideologies (One)"
	label values c1 type
	label values c2 group
	rename c1 type
	rename c2 group
	drop if missing(mean)  // optional: skip missing bars
	gen hi = mean + 1.96 * se
	gen low = mean - 1.96 * se

	* order for graph
	* Create a new display order variable
	gen barorder = .
	replace barorder = 1 if group == 5  // Identities (Only)
	replace barorder = 2 if group == 1  // Identities (Minority) 
	replace barorder = 3 if group == 2  // Identities (Majority)
	replace barorder = 4 if group == 6  // Ideologies (Only)
	replace barorder = 5 if group == 3  // Ideologies (Minority)
	replace barorder = 6 if group == 4  // Ideologies (Majority)


	* save 
	save "Temp/temp_rd_comp_bar_data.dta", replace 
	
	twoway 	(bar mean barorder if group==5, barwidth(0.7) color(cranberry) mlabel(mean) mlabposition(1) mlabgap(1) mlabcolor(black) mlabformat(%9.2f) mlabsize(small)) ///
	    	(rcap hi low barorder if group==5, lc(black)) ///	
			(bar mean barorder if group==1, barwidth(0.7) color(ebblue) mlabel(mean) mlabposition(1) mlabgap(1) mlabcolor(black) mlabformat(%9.2f) mlabsize(small)) ///
	    	(rcap hi low barorder if group==1, lc(black)) ///
	    	(bar mean barorder if group==2, barwidth(0.7) color(dkgreen) mlabel(mean) mlabposition(1) mlabgap(1) mlabcolor(black) mlabformat(%9.2f) mlabsize(small)) ///
	    	(rcap hi low barorder if group==2, lc(black)) ///
	    	(bar mean barorder if group==6, barwidth(0.7) color(dkorange) mlabel(mean) mlabposition(1) mlabgap(1) mlabcolor(black) mlabformat(%9.2f) mlabsize(small)) ///
	    	(rcap hi low barorder if group==6, lc(black)) ///
			(bar mean barorder if group==3, barwidth(0.7) color(navy) mlabel(mean) mlabposition(1) mlabgap(1) mlabcolor(black) mlabformat(%9.2f) mlabsize(small)) ///
	    	(rcap hi low barorder if group==3, lc(black)) ///
			(bar mean barorder if group==4, barwidth(0.7) color(dkorange*2) mlabel(mean) mlabposition(1) mlabgap(1) mlabcolor(black) mlabformat(%9.2f) mlabsize(small)) ///
	    	(rcap hi low barorder if group==4, lc(black)) ///					
, by(type, note("") ///
legend(order(1 "Identities (Only)" 3 "Identities (Minority)" 5 "Identities (Majority)" 7 "Ideologies (Only)" 9 "Ideologies (Minority)" 11 "Ideologies (Majority)") col(6) pos(6) ring(0) size(small))) ///
  ytitle("Mean Effect on Standardized Outcomes", size(medlarge)) yla(-0.1(0.1)0.3) ///
  legend(order(1 "Identities (Only)" 3 "Identities (Minority)" 5 "Identities (Majority)" 7 "Ideologies (Only)" 9 "Ideologies (Minority)" 11 "Ideologies (Majority)") col(3) pos(6) ring(0) size(small)) ///
  xtitle("") xla(, labcolor(bg) tlength(0))			
  
  
 * NO PRECISION WEIGHTS
 drop if type==2
 
	twoway 	(bar mean barorder if group==5, barwidth(0.7) color(cranberry) mlabel(mean) mlabposition(1) mlabgap(1) mlabcolor(black) mlabformat(%9.2f) mlabsize(small)) ///
	    	(rcap hi low barorder if group==5, lc(black)) ///	
			(bar mean barorder if group==1, barwidth(0.7) color(dkorange) mlabel(mean) mlabposition(1) mlabgap(1) mlabcolor(black) mlabformat(%9.2f) mlabsize(small)) ///
	    	(rcap hi low barorder if group==1, lc(black)) ///
	    	(bar mean barorder if group==2, barwidth(0.7) color(navy) mlabel(mean) mlabposition(1) mlabgap(1) mlabcolor(black) mlabformat(%9.2f) mlabsize(small)) ///
	    	(rcap hi low barorder if group==2, lc(black)) ///
	    	(bar mean barorder if group==6, barwidth(0.7) fcolor(cranberry*.3) bcolor(cranberry) mlabel(mean) mlabposition(1) mlabgap(1) mlabcolor(black) mlabformat(%9.2f) mlabsize(small)) ///
	    	(rcap hi low barorder if group==6, lc(black)) ///
			(bar mean barorder if group==3, barwidth(0.7) fcolor(dkorange*.3) bcolor(dkorange) mlabel(mean) mlabposition(1) mlabgap(1) mlabcolor(black) mlabformat(%9.2f) mlabsize(small)) ///
	    	(rcap hi low barorder if group==3, lc(black)) ///
			(bar mean barorder if group==4, barwidth(0.7) fcolor(navy*.3) bcolor(navy) mlabel(mean) mlabposition(1) mlabgap(1) mlabcolor(black) mlabformat(%9.2f) mlabsize(small)) ///
	    	(rcap hi low barorder if group==4, lc(black)) ///					
, /// by(type, note("") 
legend(order(1 "Identities (Only)" 3 "Identities (Minority)" 5 "Identities (Majority)" 7 "Ideologies (Only)" 9 "Ideologies (Minority)" 11 "Ideologies (Majority)") col(6) pos(6) ring(0) size(small)) ///
  ytitle("Mean Effect on Standardized Outcomes", size(medlarge)) yla(-0.1(0.1)0.3) ///
  legend(order(1 "Identities (Only)" 3 "Identities (Minority)" 5 "Identities (Majority)" 7 "Ideologies (Only)" 9 "Ideologies (Minority)" 11 "Ideologies (Majority)") col(3) pos(6) ring(1) size(small)) ///
  xtitle("") xla(, labcolor(bg) tlength(0))			
  
  
  STOP 
 
	gr export "Results/Graph/rd_outcome_by_comp_bar.png", replace
	
	
	
	
	
	
