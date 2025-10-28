********************************************************************************
* Check Robustness of Stacked RD results
* Written By Minseon Park 07/16/2025
* Run after running 5_1_outcome_multi_stacked_RD.do which generate input data for this dofile
********************************************************************************


version 18.5
set seed 1234
clear all
eststo clear

	use "Temp/data_for_rd_stacked_new.dta", clear
	
	* Define categories
	local filters h_type_exact==1 prio~=demo&h_type_exact~=1 prio==demo

	foreach f of local filters {
		eststo clear
		
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
				use "Temp/data_for_rd_stacked_new.dta", clear
				gen outcome_D = `yvar'_D * `coef_sign'
				gen vote_margin = m_`xvar'
				gen outcome_l = `yvar'* `coef_sign'
				if strpos("`yvar'", "sup_change_cum")==0 {
					gen outcome_b = `yvar'_b * `coef_sign'
				}
				gen pair_id = `j'
				keep outcome_* vote_margin year* pair_id yr_* id_election cds 
				*keep if !missing(outcome_D, vote_margin)
				append using `stacked'
				save `stacked', replace
			restore
		}

		use `stacked', clear
		
		tab pair_id, gen(pair_id_)
		
		eststo: rdrobust outcome_D vote_margin, p(1) kernel(triangular) bwselect(mserd) covs(yr_* pair_id_*) vce(nncluster id_election)
		estadd scalar beta = round(tau_cl,0.001)
		estadd local CI = "[" + string(round(e(ci_l_rb),0.001),"%4.3f") + ", " + string(round(e(ci_r_rb),0.001),"%4.3f") + "]"
		estadd scalar cl_l= round(e(ci_l_rb),0.001)
		estadd scalar cl_h= round(e(ci_r_rb),0.001)
		estadd scalar Bandwidth = round(e(h_l),0.001)
		estadd scalar N_eff = round(e(N_h_l) + e(N_h_r)	,1)
		estadd scalar ymean = e(beta_Y_p_l)[1,1]
		
		eststo: rdrobust outcome_l vote_margin, p(1) kernel(triangular) bwselect(mserd) covs(yr_* outcome_b  pair_id_*) vce(nncluster id_election)
		estadd scalar beta = round(tau_cl,0.001)
		estadd local CI = "[" + string(round(e(ci_l_rb),0.001),"%4.3f") + ", " + string(round(e(ci_r_rb),0.001),"%4.3f") + "]"
		estadd scalar cl_l= round(e(ci_l_rb),0.001)
		estadd scalar cl_h= round(e(ci_r_rb),0.001)
		estadd scalar Bandwidth = round(e(h_l),0.001)
		estadd scalar N_eff = round(e(N_h_l) + e(N_h_r)	,1)
		estadd scalar ymean = e(beta_Y_p_l)[1,1]
	
		eststo: rdrobust outcome_D vote_margin, p(2) kernel(triangular) bwselect(mserd) covs(yr_*  pair_id_*) vce(nncluster id_election)
		estadd scalar beta = round(tau_cl,0.001)
		estadd local CI = "[" + string(round(e(ci_l_rb),0.001),"%4.3f") + ", " + string(round(e(ci_r_rb),0.001),"%4.3f") + "]"
		estadd scalar cl_l= round(e(ci_l_rb),0.001)
		estadd scalar cl_h= round(e(ci_r_rb),0.001)
		estadd scalar Bandwidth = round(e(h_l),0.001)
		estadd scalar N_eff = round(e(N_h_l) + e(N_h_r)	,1)
		estadd scalar ymean = e(beta_Y_p_l)[1,1]		
		
		eststo: rdrobust outcome_D vote_margin, p(1) kernel(uniform) bwselect(mserd) covs(yr_*  pair_id_*) vce(nncluster id_election)
		estadd scalar beta = round(tau_cl,0.001)
		estadd local CI = "[" + string(round(e(ci_l_rb),0.001),"%4.3f") + ", " + string(round(e(ci_r_rb),0.001),"%4.3f") + "]"
		estadd scalar cl_l= round(e(ci_l_rb),0.001)
		estadd scalar cl_h= round(e(ci_r_rb),0.001)
		estadd scalar Bandwidth = round(e(h_l),0.001)
		estadd scalar N_eff = round(e(N_h_l) + e(N_h_r)	,1)
		estadd scalar ymean = e(beta_Y_p_l)[1,1]	
		
		eststo: rdrobust outcome_D vote_margin, p(1) kernel(triangular) bwselect(mserd) covs(yr_*  pair_id_*) vce(nncluster id_election 4)
		estadd scalar beta = round(tau_cl,0.001)
		estadd local CI = "[" + string(round(e(ci_l_rb),0.001),"%4.3f") + ", " + string(round(e(ci_r_rb),0.001),"%4.3f") + "]"
		estadd scalar cl_l= round(e(ci_l_rb),0.001)
		estadd scalar cl_h= round(e(ci_r_rb),0.001)
		estadd scalar Bandwidth = round(e(h_l),0.001)
		estadd scalar N_eff = round(e(N_h_l) + e(N_h_r)	,1)
		estadd scalar ymean = e(beta_Y_p_l)[1,1]		
		
		eststo: rdrobust outcome_D vote_margin, p(1) kernel(triangular) bwselect(mserd) covs(yr_*  pair_id_*) vce(nncluster id_election 5)
		estadd scalar beta = round(tau_cl,0.001)
		estadd local CI = "[" + string(round(e(ci_l_rb),0.001),"%4.3f") + ", " + string(round(e(ci_r_rb),0.001),"%4.3f") + "]"
		estadd scalar cl_l= round(e(ci_l_rb),0.001)
		estadd scalar cl_h= round(e(ci_r_rb),0.001)
		estadd scalar Bandwidth = round(e(h_l),0.001)
		estadd scalar N_eff = round(e(N_h_l) + e(N_h_r)	,1)
		estadd scalar ymean = e(beta_Y_p_l)[1,1]		

		esttab using "Results/New_Tex/rd_robust_`f'.tex", replace ///
		cells("b(fmt(3))") ///
		stats(CI Bandwidth N_eff, labels("" "Bandwidth" "N") fmt("%s" 3 0 3)) ///
		coeflabels(RD_Estimate "Estimate") ///
		plain noobs nostar nogap nonumbers nomtitles nolines ///
		tex fragment collabels(none)
	
	}
	
