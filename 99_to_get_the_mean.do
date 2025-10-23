
	set seed 1234

	* set directory 
	cd "~/Dropbox/California Election Data/Code"
	global logpath "~/Dropbox/California Election Data/Logs"


	global list_prio =" budget_hawk equity_prior agenda_bias facility_prior parent_involvement_prior safety_health_prior teacher_care dropout_prior enrollment_prior cte_prior score_concern sup_concern"
	global list_demo = "hisp female occ_teacher democrat_v2"

	global missing = "with_missing" // "with_missing" or "no_missing"
	
	global y_rev =""
	global y_fiscal= "exp_total_per_stu exp_capital_total_per_stu"
	global y_teacher = ""
	global y_score = "ecd_test "
	global y_enroll = ""
	global y_behv = "sus_share"	
	global y_sup= ""

	*** 1/ first stage
	eststo clear
	foreach y in budget_hawk equity_prior hisp {
		use "data_for_rd/$missing/dist_`y'.dta" if dist_cnty~=1, clear
		
		* prep y vars 
		do 4_f_prep_rd
		
		* merge with board composition data 
		merge m:1 leaid year using "Temp/board_composition.dta", keep(master matched) gen(_comp)
		
		* prep share vars (unique to first-stage analysis)
		g `y'_share_b = `y'_share if year==year_elected-1
		sort multi_raceid `y'_share_b
		bysort multi_raceid: replace `y'_share_b = `y'_share_b[1]
		cap g `y'_share_D = `y'_share - `y'_share_b
		
		eststo: rdrobust `y'_share vote_margin if year>=year_elected & year<=year_elected+3, p(1) kernel(triangular) bwselect(mserd) covs(yr_*) vce(cluster id_election)
		estadd scalar beta = round(tau_cl,0.01)
		estadd scalar cl_l= round(e(ci_l_rb),0.01)
		estadd scalar cl_h= round(e(ci_r_rb),0.01)
		estadd scalar Bandwidth = round(e(h_l),0.01)
		estadd scalar N_eff = round(e(N_h_l) + e(N_h_r)	,1)
		estadd scalar ymean =  e(beta_p_l)[1,1]
		
}
		esttab using "Results/all_main_rd_level.csv", replace ///
		cells("b(fmt(2))") ///
		stats(cl_l cl_h Bandwidth N_eff ymean) ///
		coeflabels(RD_Estimate "$\beta$") nogap nolines label


		eststo clear
	*** 2/ univariate RD
	foreach y in budget_hawk equity_prior hisp {
		use "data_for_rd/$missing/dist_`y'.dta" if dist_cnty~=1, clear
		
		* prep y vars 
		do 4_f_prep_rd
		
		eststo: rdrobust exp_total_per_stu vote_margin if year>=year_elected & year<=2017, p(1) kernel(triangular) bwselect(mserd) covs(yr_*) vce(cluster id_election)
		estadd scalar beta = round(tau_cl,0.01)
		estadd scalar cl_l= round(e(ci_l_rb),0.01)
		estadd scalar cl_h= round(e(ci_r_rb),0.01)
		estadd scalar Bandwidth = round(e(h_l),0.01)
		estadd scalar N_eff = round(e(N_h_l) + e(N_h_r)	,1)
		estadd scalar ymean =  e(beta_p_l)[1,1]

		eststo: rdrobust ecd_test vote_margin if year>=year_elected, p(1) kernel(triangular) bwselect(mserd) covs(yr_*) vce(cluster id_election)
		estadd scalar beta = round(tau_cl,0.01)
		estadd scalar cl_l= round(e(ci_l_rb),0.01)
		estadd scalar cl_h= round(e(ci_r_rb),0.01)
		estadd scalar Bandwidth = round(e(h_l),0.01)
		estadd scalar N_eff = round(e(N_h_l) + e(N_h_r)	,1)
		estadd scalar ymean =  e(beta_p_l)[1,1]		
}
		esttab using "Results/all_main_rd_level.csv", append ///
		cells("b(fmt(2))") ///
		stats(cl_l cl_h Bandwidth N_eff ymean) ///
		coeflabels(RD_Estimate "$\beta$") nogap nolines label
		
	
	eststo clear
	*** 3/ minute
	foreach y in budget_hawk equity_prior {	
		
		use "data_for_rd/with_missing/dist_votes_`y'.dta" if dist_cnty~=1, clear
		 * year dummies and lags 
		tab year, g(yr_)
		egen id_election= group(leaid year_elected) // should be unique up to multi_raceid; multi_raceid is a string variable thus to be used for std err clustering, we need another numeric variable

		g year_lag = year-year_elected
		replace year_lag = year_lag+4
		cap g T = cond(vote_margin>=0,1,0) if !missing(vote_margin)
	
	
		eststo: rdrobust n_mtn_equity vote_margin if year>=year_elected & year<=2017, p(1) kernel(triangular) bwselect(mserd) covs(yr_*) vce(cluster id_election)
		estadd scalar beta = round(tau_cl,0.01)
		estadd scalar cl_l= round(e(ci_l_rb),0.01)
		estadd scalar cl_h= round(e(ci_r_rb),0.01)
		estadd scalar Bandwidth = round(e(h_l),0.01)
		estadd scalar N_eff = round(e(N_h_l) + e(N_h_r)	,1)
		estadd scalar ymean =  e(beta_p_l)[1,1]

		eststo: rdrobust n_mtn_budget vote_margin if year>=year_elected, p(1) kernel(triangular) bwselect(mserd) covs(yr_*) vce(cluster id_election)
		estadd scalar beta = round(tau_cl,0.01)
		estadd scalar cl_l= round(e(ci_l_rb),0.01)
		estadd scalar cl_h= round(e(ci_r_rb),0.01)
		estadd scalar Bandwidth = round(e(h_l),0.01)
		estadd scalar N_eff = round(e(N_h_l) + e(N_h_r)	,1)
		estadd scalar ymean =  e(beta_p_l)[1,1]			

		eststo: rdrobust n_mtn_total vote_margin if year>=year_elected, p(1) kernel(triangular) bwselect(mserd) covs(yr_*) vce(cluster id_election)
		estadd scalar beta = round(tau_cl,0.01)
		estadd scalar cl_l= round(e(ci_l_rb),0.01)
		estadd scalar cl_h= round(e(ci_r_rb),0.01)
		estadd scalar Bandwidth = round(e(h_l),0.01)
		estadd scalar N_eff = round(e(N_h_l) + e(N_h_r)	,1)
		estadd scalar ymean =  e(beta_p_l)[1,1]	
		
		eststo: rdrobust budget_yes_votes_mg1 vote_margin if year>=year_elected & year<=2017, p(1) kernel(triangular) bwselect(mserd) covs(yr_*) vce(cluster id_election)
		estadd scalar beta = round(tau_cl,0.01)
		estadd scalar cl_l= round(e(ci_l_rb),0.01)
		estadd scalar cl_h= round(e(ci_r_rb),0.01)
		estadd scalar Bandwidth = round(e(h_l),0.01)
		estadd scalar N_eff = round(e(N_h_l) + e(N_h_r)	,1)
		estadd scalar ymean =  e(beta_p_l)[1,1]

		eststo: rdrobust equity_yes_votes_mg1 vote_margin if year>=year_elected, p(1) kernel(triangular) bwselect(mserd) covs(yr_*) vce(cluster id_election)
		estadd scalar beta = round(tau_cl,0.01)
		estadd scalar cl_l= round(e(ci_l_rb),0.01)
		estadd scalar cl_h= round(e(ci_r_rb),0.01)
		estadd scalar Bandwidth = round(e(h_l),0.01)
		estadd scalar N_eff = round(e(N_h_l) + e(N_h_r)	,1)
		estadd scalar ymean =  e(beta_p_l)[1,1]		
		
		eststo: rdrobust total_yes_votes_mg1 vote_margin if year>=year_elected, p(1) kernel(triangular) bwselect(mserd) covs(yr_*) vce(cluster id_election)
		estadd scalar beta = round(tau_cl,0.01)
		estadd scalar cl_l= round(e(ci_l_rb),0.01)
		estadd scalar cl_h= round(e(ci_r_rb),0.01)
		estadd scalar Bandwidth = round(e(h_l),0.01)
		estadd scalar N_eff = round(e(N_h_l) + e(N_h_r)	,1)
		estadd scalar ymean =  e(beta_p_l)[1,1]		
				
	}
	
		esttab using "Results/all_main_rd_level.csv", append ///
		cells("b(fmt(2))") ///
		stats(cl_l cl_h Bandwidth N_eff ymean) ///
		coeflabels(RD_Estimate "$\beta$") nogap nolines label
		
		