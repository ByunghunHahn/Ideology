********************************************************************************
* Run Super-stacked RD and Draw Bar Graph
* Written By Minseon Park 07/14/2025
********************************************************************************

	version 18.5
clear all
set seed 1234

* set directory 
cd "~/Library/CloudStorage/Dropbox/California Election Data/Code"
global logpath "~/Library/CloudStorage/Dropbox/California Election Data/Logs"

global y_rev =""
global y_fiscal= "rev_cte_per_stu exp_total_per_stu bond_amount_per_stu exp_capital_total_per_stu"
global y_teacher = "teacher_exit_dist entry_sal_Bcomp_log ba60_sal_Bcomp_log"
global y_score = "ecd_test hsp_test test"
global y_enroll = "log_enrollment"
global y_behv = "sus_share"	
global y_sup = ""
	
global class = "entry ba60"

global missing="with_missing"
global datapath = "data_for_rd/$missing"	

global list_demo = "hisp female occ_teacher democrat_v2"
global list_prio ="budget_hawk equity_prior safety_health_prior teacher_care score_concern facility_prior cte_prior dropout_prior enrollment_prior sup_concern"

global flag_balance =0


********************************************************************************
* I. Store Stacked Data
********************************************************************************

	use "$datapath/dist_stacked.dta", clear
	
	tostring id_district_nces, replace  // Convert to string if not already	
	gen leaid = string(real(id_district_nces), "%07.0f")  // Adjust "8" to the required width

	keep leaid year m_* year_elected state_leaid multi_raceid id_district_nces winner_*

	*** 1/ get outcomes
	* a) merged
	destring leaid, replace
	merge m:1 leaid year using "Temp/outcomes_z.dta", gen(_outcome_z) force keep(master matched)
	
	* b) TSAL // these are the variables with which I don't know how to do normalization within each year, since we define the outcome using the election year
	do 4_f_merge_tsal 
	
	foreach y in entry_sal_Bcomp_log ba60_sal_Bcomp_log { 
		egen z=std(`y')
		replace `y'=z
		drop z
	}
	
	do 4_f_prep_rd

	* c) super-intendent // this is not a diff variable but this way it's consistent with the rest of vars so makes our life easier
	egen multi_raceid_num= group(multi_raceid)
	xtset multi_raceid_num year
	keep if year>=year_elected
	tsegen double sup_change_cum_D = rowmax(L(0/4).sup_change) 
	
	foreach y in sup_change_cum_D { 
		egen z=std(`y')
		replace `y'=z
		drop z
	}
	
	
	*** 2/ pre-process outcome variables
	foreach y in $y_fiscal $y_rev { // fiscal variables after 2018 won't be used
		replace `y'=. if year>2017
	}

	
	keep if year>=year_elected
	save "Temp/data_for_rd_stacked.dta", replace
	
	
********************************************************************************
* II. Run Stacked Data
********************************************************************************	
	
	eststo clear
	
	do 0_f_rd_plots
	
	* Define categories
	local filters h_type_exact==1 prio~=demo&h_type_exact==2 prio==demo

	* Create empty matrix to store results
	mat results = J(4,10,.)
	local i = 1

	foreach f of local filters {
		* Step 1: Filter h0_list and set up pairs
		use Temp/h0_list_demo_outcome.dta if outcome_main==1 & ~(demo=="democrat_v2" & strpos(outcome,"sup")), clear
		// can't run the full code with (demo=="democrat_v2" & strpos(outcome,"sup")) somehow.
		// this combo is 1 pair out of 46 identities (other) pairs
		keep if `f'
		duplicates drop demo outcome, force 
		
		gen pair_id = _n
		local npairs = _N

		tempfile stacked
		save `stacked', emptyok replace

		forvalues j = 1/`npairs' {
			local xvar = demo[`j']
			local yvar = outcome[`j']
			local prio = prio[`j']
			local coef_sign = coef_sign[`j']

			preserve
				use Temp/data_for_rd_stacked.dta, clear
				gen outcome_D = `yvar'_D *`coef_sign'
				gen vote_margin  = m_`xvar'
				gen prio_diff = winner_`prio'
				gen prio_diff_p = winner_`prio'_p				
				gen pair_id = `j'
				keep outcome_D vote_margin year* pair_id yr_* id_election cds prio_diff*
				*keep if !missing(outcome_D, vote_margin)
				append using `stacked'
				save `stacked', replace
			restore
		}

		use `stacked', clear
		tab pair_id, gen(pair_id_)
		
		eststo: rdrobust outcome_D vote_margin, p(1) kernel(triangular) bwselect(mserd) covs(yr_* pair_id_*) vce(nncluster id_election)
		estadd scalar cl_l= e(ci_l_rb)
		estadd scalar cl_h= e(ci_r_rb)
		estadd scalar pval = e(pv_rb)
		estadd scalar bw = e(h_l)
		estadd scalar N_eff = e(N_h_l) + e(N_h_r)	
		mat results[`i',1] = e(tau_cl)
		mat results[`i',2] = e(ci_l_rb)
		mat results[`i',3] = e(ci_r_rb)
		
		cap eststo: rdrobust outcome_D vote_margin if prio_diff==0, p(1) kernel(triangular) bwselect(mserd) covs(yr_* pair_id_*) vce(nncluster id_election)
		cap estadd scalar cl_l= e(ci_l_rb)
		cap estadd scalar cl_h= e(ci_r_rb)
		cap estadd scalar pval = e(pv_rb)
		cap estadd scalar bw = e(h_l)
		cap estadd scalar N_eff = e(N_h_l) + e(N_h_r)
		mat results[`i',4] = e(tau_cl)
		mat results[`i',5] = e(ci_l_rb)
		mat results[`i',6] = e(ci_r_rb)
		
		cap eststo: rdrobust outcome_D vote_margin if abs(prio_diff_p)<0.01, p(1) kernel(triangular) bwselect(mserd) covs(yr_* pair_id_*) vce(nncluster id_election)
		cap estadd scalar cl_l= e(ci_l_rb)
		cap estadd scalar cl_h= e(ci_r_rb)
		cap estadd scalar pval = e(pv_rb)
		cap estadd scalar bw = e(h_l)
		cap estadd scalar N_eff = e(N_h_l) + e(N_h_r)
		mat results[`i',7] = e(tau_cl)
		mat results[`i',8] = e(ci_l_rb)
		mat results[`i',9] = e(ci_r_rb)
					
		* Extract point estimate and CIs
		mat results[`i',10] = `npairs'

		local ++i
	}
	global tab_opt = "b(a2) se(2) scalars(cl_l cl_h pval bw N_eff ymean) label star(+ 0.1 * 0.05 ** 0.01)"

	esttab, $tab_opt
	gr combine "Results/Graph/rd_stacked_h_type_exact==1.gph" "Results/Graph/rd_stacked_h_type==2.gph" "Results/Graph/rd_stacked_demo==prio.gph", col(3) imargin(0 0 0 0) xsize(5) ysize(2.5)
	
	
	* Save matrix as dataset for graphing
	clear
	svmat results, names(col)
	gen group = _n

	* Add group labels
	label define grouplab 1 "Identities (existing literature)" 2 "Identities (other)" 3 "Ideologies"
	label values group grouplab
	
	
	twoway (bar c1 group if group==2, barwidth(0.7) color(dkorange) ///
     mlabel(c1) mlabposition(1) mlabgap(0.5) mlabcolor(black) mlabformat(%9.2f) mlabsize(med)) ///
    (rcap c2 c3 group if group==2, lc(black)) ///
    (bar c1 group if group==3, barwidth(0.7) color(ebblue) ///
     mlabel(c1) mlabposition(1) mlabgap(0.5) mlabcolor(black) mlabformat(%9.2f) mlabsize(med)) ///
    (rcap c2 c3 group if group==3, lc(black)) ///
	(bar c1 group if group==1, barwidth(0.7) color(dkorange*2) ///
     mlabel(c1) mlabposition(1) mlabgap(0.5) mlabcolor(black) mlabformat(%9.2f) mlabsize(med)) ///
    (rcap c2 c3 group if group==1, lc(black)) ///
, ytitle("Mean Effect on Standardized Outcomes", size(medlarge)) ///
  legend(order( 5 "Identities (existing lit.)" 1 "Identities (augmented)" 3 "Ideologies") col(3) pos(6) size(medlarge)) ///
  xtitle("") xla(, labcolor(bg) tlength(0)) ylabel(-0.05(0.05)0.2)
	gr export "Results/Graph/rd_outcome_bar.png",replace
	
	
	twoway (bar c4 group if group==2, barwidth(0.7) color(dkorange) ///
     mlabel(c4) mlabposition(1) mlabgap(0.5) mlabcolor(black) mlabformat(%9.2f) mlabsize(med)) ///
    (rcap c5 c6 group if group==2, lc(black)) ///
    (bar c7 group if group==3, barwidth(0.7) color(ebblue) ///
     mlabel(c7) mlabposition(1) mlabgap(0.5) mlabcolor(black) mlabformat(%9.2f) mlabsize(med)) ///
    (rcap c8 c9 group if group==3, lc(black)) ///
	(bar c4 group if group==1, barwidth(0.7) color(dkorange*2) ///
     mlabel(c4) mlabposition(1) mlabgap(0.5) mlabcolor(black) mlabformat(%9.2f) mlabsize(med)) ///
    (rcap c5 c6 group if group==1, lc(black)) ///
, ytitle("Mean Effect on Standardized Outcomes", size(medlarge)) ///
  legend(order( 5 "Identities (existing lit.)" 1 "Identities (augmented)" 3 "Ideologies") col(3) pos(6) size(medlarge)) ///
  xtitle("") xla(, labcolor(bg) tlength(0)) ylabel(-0.05(0.05)0.2)
	gr export "Results/Graph/rd_outcome_bar_joint.png",replace	
	

	/* for presentation animation
	twoway (bar c1 group if group==2, barwidth(0.7) color(dkorange%50)) ///
    (bar c1 group if group==3, barwidth(0.7) color(ebblue%50)) ///
	(bar c1 group if group==1, barwidth(0.7) color(dkorange*2%50)) ///
	(bar c4 group if group==2, barwidth(0.7) lcolor(white) lw(thin) fcolor(none)) ///
	(bar c4 group if group==1, barwidth(0.7) lcolor(white) lw(thin) fcolor(none)) ///
, ytitle("Mean Effect on Standardized Outcomes", size(medlarge)) ///
  legend(order(6 "Identities (existing literature)"  ///
  4 "Identities (other)"  ///
  8 "Ideologies" ) col(3) pos(6) size(medlarge)) ///
  xtitle("") xla(, labcolor(bg) tlength(0)) ylabel(-0.05(0.05)0.2)
	gr export "Results/Graph/rd_outcome_bar_joint_0.png",replace	
	
	twoway (bar c1 group if group==2, barwidth(0.7) color(dkorange%50)) ///
    (bar c1 group if group==3, barwidth(0.7) color(ebblue%50)) ///
	(bar c1 group if group==1, barwidth(0.7) color(dkorange*2%50)) ///
	(bar c4 group if group==2, barwidth(0.7) lcolor(dkorange) lw(thick) fcolor(none) ///
     mlabel(c4) mlabposition(1) mlabgap(0.5) mlabcolor(black) mlabformat(%9.2f) mlabsize(med)) ///
    (rcap c5 c6 group if group==2, lc(black)) ///
	(bar c4 group if group==1, barwidth(0.7) lcolor(dkorange*2) lw(thick) fcolor(none) ///
     mlabel(c4) mlabposition(1) mlabgap(0.5) mlabcolor(black) mlabformat(%9.2f) mlabsize(med)) ///
    (rcap c5 c6 group if group==1, lc(black)) ///
, ytitle("Mean Effect on Standardized Outcomes", size(medlarge)) ///
  legend(order(6 "Identities (existing literature)"  ///
  4 "Identities (other)"  ///
  8 "Ideologies" ) col(3) pos(6) size(medlarge)) ///
  xtitle("") xla(, labcolor(bg) tlength(0)) ylabel(-0.05(0.05)0.2)
	gr export "Results/Graph/rd_outcome_bar_joint_1.png",replace	
	
	
	twoway (bar c1 group if group==2, barwidth(0.7) color(dkorange%50)) ///
    (bar c1 group if group==3, barwidth(0.7) color(ebblue%50)) ///
	(bar c1 group if group==1, barwidth(0.7) color(dkorange*2%50)) ///
	(bar c4 group if group==2, barwidth(0.7) lcolor(dkorange) lw(thick) fcolor(none) ///
     mlabel(c4) mlabposition(1) mlabgap(0.5) mlabcolor(black) mlabformat(%9.2f) mlabsize(med)) ///
    (rcap c5 c6 group if group==2, lc(black)) ///
    (bar c7 group if group==3, barwidth(0.7) lcolor(navy) lw(thick) fcolor(none) ///
     mlabel(c7) mlabposition(1) mlabgap(0.5) mlabcolor(black) mlabformat(%9.2f) mlabsize(med)) ///
    (rcap c8 c9 group if group==3, lc(black)) ///
	(bar c4 group if group==1, barwidth(0.7) lcolor(dkorange*2) lw(thick) fcolor(none) ///
     mlabel(c4) mlabposition(1) mlabgap(0.5) mlabcolor(black) mlabformat(%9.2f) mlabsize(med)) ///
    (rcap c5 c6 group if group==1, lc(black)) ///
, ytitle("Mean Effect on Standardized Outcomes", size(medlarge)) ///
  legend(order(8 "Identities (existing literature)"  ///
  4 "Identities (other)"  ///
  6 "Ideologies" ) col(3) pos(6) size(medlarge)) ///
  xtitle("") xla(, labcolor(bg) tlength(0)) ylabel(-0.05(0.05)0.2)
	gr export "Results/Graph/rd_outcome_bar_joint_2.png",replace	
	
	*/
********************************************************************************
* III. Run Stacked Data with Valid Platform Cases
********************************************************************************	
	use "data_for_rd/no_missing/dist_stacked.dta", clear
	
	tostring id_district_nces, replace  // Convert to string if not already	
	gen leaid = string(real(id_district_nces), "%07.0f")  // Adjust "8" to the required width

	keep leaid year m_* year_elected state_leaid multi_raceid id_district_nces

	*** 1/ get outcomes
	* a) merged
	destring leaid, replace
	merge m:1 leaid year using "Temp/outcomes_z.dta", gen(_outcome_z) force keep(master matched)
	
	* b) TSAL // these are the variables with which I don't know how to do normalization within each year, since we define the outcome using the election year
	do 4_f_merge_tsal 
	
	foreach y in entry_sal_Bcomp_log ba60_sal_Bcomp_log { 
		egen z=std(`y')
		replace `y'=z
		drop z
	}
	
	do 4_f_prep_rd

	* c) super-intendent // this is not a diff variable but this way it's consistent with the rest of vars so makes our life easier
	egen multi_raceid_num= group(multi_raceid)
	xtset multi_raceid_num year
	keep if year>=year_elected
	tsegen double sup_change_cum_D = rowmax(L(0/4).sup_change) 
	
	foreach y in sup_change_cum_D { 
		egen z=std(`y')
		replace `y'=z
		drop z
	}
	
	*** 2/ pre-process outcome variables
	foreach y in $y_fiscal $y_rev { // fiscal variables after 2018 won't be used
		replace `y'=. if year>2017
	}

	
	keep if year>=year_elected
	save "Temp/data_for_rd_stacked_nomissing.dta", replace
	

	* Define categories
	local filters prio==demo

	foreach f of local filters {
		* Step 1: Filter h0_list and set up pairs
		use Temp/h0_list_demo_outcome.dta if outcome_main==1 & ~(demo=="democrat_v2" & strpos(outcome,"sup")), clear
		// can't run the full code with (demo=="democrat_v2" & strpos(outcome,"sup")) somehow.
		// this combo is 1 pair out of 46 identities (other) pairs
		keep if `f'
		duplicates drop demo outcome, force 
		
		gen pair_id = _n
		local npairs = _N

		tempfile stacked
		save `stacked', emptyok replace

		forvalues j = 1/`npairs' {
			local xvar = demo[`j']
			local yvar = outcome[`j']
			local coef_sign = coef_sign[`j']

			preserve
				use Temp/data_for_rd_stacked_nomissing.dta, clear
				gen outcome_D = `yvar'_D * `coef_sign'
				gen vote_margin  = m_`xvar'
				gen pair_id = `j'
				keep outcome_D vote_margin year* pair_id yr_* id_election cds
				keep if !missing(outcome_D, vote_margin)
				append using `stacked'
				save `stacked', replace
			restore
		}

		use `stacked', clear
		tab pair_id, gen(pair_id_)
		
		rdrobust outcome_D vote_margin, p(1) kernel(triangular) bwselect(mserd) covs(yr_* pair_id_*) vce(nncluster id_election)
		
		* Extract point estimate and CIs
		mat results[`i',1] = e(tau_cl)
		mat results[`i',2] = e(ci_l_rb)
		mat results[`i',3] = e(ci_r_rb)
		mat results[`i',4] = `npairs'

		local ++i
	}
	
	* Save matrix as dataset for graphing
	clear
	svmat results, names(col)
	gen group = _n

	* Add group labels
	label define grouplab 1 "Identities (existing literature)" 2 "Identities (other)" 3 "Ideologies"
	label values group grouplab
	
	twoway (bar c1 group if group==3, barwidth(0.7) color(ebblue) ///
     mlabel(c1) mlabposition(1) mlabgap(0.5) mlabcolor(black) mlabformat(%9.2f) mlabsize(med)) ///
    (rcap c2 c3 group if group==3, lc(black)) ///
	(bar c1 group if group==4, barwidth(0.7) color(ebblue*2) ///
     mlabel(c1) mlabposition(1) mlabgap(0.5) mlabcolor(black) mlabformat(%9.2f) mlabsize(med)) ///
    (rcap c2 c3 group if group==4, lc(black)) ///
, ytitle("Mean Effect on Standardized Outcomes", size(medlarge)) ///
  legend(order( 1 "Ideologies"  3 "Ideologies (Only with Valid Platform)") col(3) pos(6) size(medlarge)) ///
  xtitle("") xla(, labcolor(bg) tlength(0)) ylabel(0(0)0.2)
	gr export "Results/Graph/rd_outcome_bar_valid.png",replace

*log c
