********************************************************************************
* Check Robustness of Casestudy Results
* Written By Minseon Park 07/16/2025
********************************************************************************

version 18.5
clear all
set seed 1234

* set directory 
cd "~/Library/CloudStorage/Dropbox/California Election Data/Code"
global logpath "~/Library/CloudStorage/Dropbox/California Election Data/Logs"
	
global class = "entry ba60"

global missing="with_missing"
global datapath ="data_for_rd/$missing"	

global list_demo ="hisp female occ_teacher democrat_v2"
global list_prio ="budget_hawk equity_prior safety_health_prior teacher_care score_concern facility_prior cte_prior dropout_prior enrollment_prior sup_concern"

global flag_balance =0

global y_rev =""
global y_fiscal= "exp_total_per_stu exp_cap_const_per_stu exp_cap_land_per_stu"
global y_teacher = ""
global y_score = "test ecd_test hsp_test"
global y_enroll = ""
global y_behv = ""



	capture program drop rd_tab
	program define rd_tab
	  syntax, y(string) x(string) [yearcond(string)] [extra(int 0)]
			
		use "data_for_rd/with_missing/dist_`x'.dta", clear

		ren *vocational* *cte*
		g log_enrollment=log(enrollment)
		
		ren exp_capital_construction exp_cap_const
		ren exp_capital_land_structures exp_cap_land

		foreach yy in exp_cap_const exp_cap_land  {
			g `yy'_per_stu=`yy'/enrollment
			g log_`yy'_per_stu = log(`yy'_per_stu)
		}	
	
		do 4_f_prep_rd
	
		eststo: rdrobust `y'_D vote_margin if year>=year_elected `yearcond', p(1) kernel(triangular) bwselect(mserd) covs(yr_*) vce(nncluster id_election)
		estadd scalar beta = round(tau_cl,0.001)
		estadd local CI = "[" + string(round(e(ci_l_rb),0.001),"%4.3f") + ", " + string(round(e(ci_r_rb),0.001),"%4.3f") + "]"
		estadd scalar cl_l= round(e(ci_l_rb),0.001)
		estadd scalar cl_h= round(e(ci_r_rb),0.001)
		estadd scalar Bandwidth = round(e(h_l),0.001)
		estadd scalar N_eff = round(e(N_h_l) + e(N_h_r)	,1)
		estadd scalar ymean =  e(beta_Y_p_l)[1,1]
		
		eststo: rdrobust `y' vote_margin if year>=year_elected `yearcond', p(1) kernel(triangular) bwselect(mserd) covs(yr_* `y'_b) vce(nncluster id_election)
		estadd scalar beta = round(tau_cl,0.001)
		estadd local CI = "[" + string(round(e(ci_l_rb),0.001),"%4.3f") + ", " + string(round(e(ci_r_rb),0.001),"%4.3f") + "]"
		estadd scalar cl_l= round(e(ci_l_rb),0.001)
		estadd scalar cl_h= round(e(ci_r_rb),0.001)
		estadd scalar Bandwidth = round(e(h_l),0.001)
		estadd scalar N_eff = round(e(N_h_l) + e(N_h_r)	,1)
		estadd scalar ymean =  e(beta_Y_p_l)[1,1]
	
		eststo: rdrobust `y'_D vote_margin if year>=year_elected `yearcond', p(2) kernel(triangular) bwselect(mserd) covs(yr_*) vce(nncluster id_election)
		estadd scalar beta = round(tau_cl,0.001)
		estadd local CI = "[" + string(round(e(ci_l_rb),0.001),"%4.3f") + ", " + string(round(e(ci_r_rb),0.001),"%4.3f") + "]"
		estadd scalar cl_l= round(e(ci_l_rb),0.001)
		estadd scalar cl_h= round(e(ci_r_rb),0.001)
		estadd scalar Bandwidth = round(e(h_l),0.001)
		estadd scalar N_eff = round(e(N_h_l) + e(N_h_r)	,1)
		estadd scalar ymean =  e(beta_Y_p_l)[1,1]		
		
		eststo: rdrobust `y'_D vote_margin if year>=year_elected `yearcond', p(1) kernel(uniform) bwselect(mserd) covs(yr_*) vce(nncluster id_election)
		estadd scalar beta = round(tau_cl,0.001)
		estadd local CI = "[" + string(round(e(ci_l_rb),0.001),"%4.3f") + ", " + string(round(e(ci_r_rb),0.001),"%4.3f") + "]"
		estadd scalar cl_l= round(e(ci_l_rb),0.001)
		estadd scalar cl_h= round(e(ci_r_rb),0.001)
		estadd scalar Bandwidth = round(e(h_l),0.001)
		estadd scalar N_eff = round(e(N_h_l) + e(N_h_r)	,1)
		estadd scalar ymean =  e(beta_Y_p_l)[1,1]	
		
		eststo: rdrobust `y'_D vote_margin if year>=year_elected `yearcond', p(1) kernel(triangular) bwselect(mserd) covs(yr_*) vce(nncluster id_election 4)
		estadd scalar beta = round(tau_cl,0.001)
		estadd local CI = "[" + string(round(e(ci_l_rb),0.001),"%4.3f") + ", " + string(round(e(ci_r_rb),0.001),"%4.3f") + "]"
		estadd scalar cl_l= round(e(ci_l_rb),0.001)
		estadd scalar cl_h= round(e(ci_r_rb),0.001)
		estadd scalar Bandwidth = round(e(h_l),0.001)
		estadd scalar N_eff = round(e(N_h_l) + e(N_h_r)	,1)
		estadd scalar ymean =  e(beta_Y_p_l)[1,1]		
		
		eststo: rdrobust `y'_D vote_margin if year>=year_elected `yearcond', p(1) kernel(triangular) bwselect(mserd) covs(yr_*) vce(nncluster id_election 5)
		estadd scalar beta = round(tau_cl,0.001)
		estadd local CI = "[" + string(round(e(ci_l_rb),0.001),"%4.3f") + ", " + string(round(e(ci_r_rb),0.001),"%4.3f") + "]"
		estadd scalar cl_l= round(e(ci_l_rb),0.001)
		estadd scalar cl_h= round(e(ci_r_rb),0.001)
		estadd scalar Bandwidth = round(e(h_l),0.001)
		estadd scalar N_eff = round(e(N_h_l) + e(N_h_r)	,1)
		estadd scalar ymean =  e(beta_Y_p_l)[1,1]		
		
		if `extra' == 1 {
		eststo: rdrobust `y'_D vote_margin if year>=year_elected `yearcond' & ecd_test_D~=., p(1) kernel(triangular) bwselect(mserd) covs(yr_*) vce(nncluster id_election)
		estadd scalar beta = round(tau_cl,0.001)
		estadd local CI = "[" + string(round(e(ci_l_rb),0.001),"%4.3f") + ", " + string(round(e(ci_r_rb),0.001),"%4.3f") + "]"
		estadd scalar cl_l= round(e(ci_l_rb),0.001)
		estadd scalar cl_h= round(e(ci_r_rb),0.001)
		estadd scalar Bandwidth = round(e(h_l),0.001)
		estadd scalar N_eff = round(e(N_h_l) + e(N_h_r)	,1)
		estadd scalar ymean =  e(beta_Y_p_l)[1,1]		
		
		eststo: rdrobust `y'_D vote_margin if year>=year_elected `yearcond' & abs(exp_cap_const_per_stu_D)<6, p(1) kernel(triangular) bwselect(mserd) covs(yr_*) vce(nncluster id_election)
		estadd scalar beta = round(tau_cl,0.001)
		estadd local CI = "[" + string(round(e(ci_l_rb),0.001),"%4.3f") + ", " + string(round(e(ci_r_rb),0.001),"%4.3f") + "]"
		estadd scalar cl_l= round(e(ci_l_rb),0.001)
		estadd scalar cl_h= round(e(ci_r_rb),0.001)
		estadd scalar Bandwidth = round(e(h_l),0.001)
		estadd scalar N_eff = round(e(N_h_l) + e(N_h_r)	,1)
		estadd scalar ymean =  e(beta_Y_p_l)[1,1]		
		}
	end
	
	
	eststo clear 
	rd_tab, y("exp_total_per_stu") x("budget_hawk") yearcond("& year<=2017") extra(1)
	
	esttab using "Results/Tex/rd_spending_budget_extra.tex", replace ///
    cells("b(fmt(3))") ///
    stats(CI Bandwidth N_eff ymean, labels( " " "Bandwidth" "N" "Mean Dep. Var.") fmt("%s" 3 0 3)) ///
    coeflabels(RD_Estimate "Estimate") ///
    plain noobs nostar nogap nonumbers nomtitles nolines ///
    tex fragment collabels(none)
	
	eststo clear 
	rd_tab, y("exp_total_per_stu") x("equity_prior") yearcond("& year<=2017") extra(1)
	
	esttab using "Results/Tex/rd_spending_equity_extra.tex", replace ///
    cells("b(fmt(3))") ///
    stats(CI Bandwidth N_eff ymean, labels( " " "Bandwidth" "N" "Mean Dep. Var.") fmt("%s" 3 0 3)) ///
    coeflabels(RD_Estimate "Estimate") ///
    plain noobs nostar nogap nonumbers nomtitles nolines ///
    tex fragment collabels(none)
	
		
	eststo clear 
	rd_tab, y("exp_total_per_stu") x("budget_hawk") yearcond("& year<=2017") 
	
	esttab using "Results/Tex/rd_spending_budget.tex", replace ///
    cells("b(fmt(3))") ///
    stats(CI Bandwidth N_eff ymean, labels( " " "Bandwidth" "N" "Mean Dep. Var.") fmt("%s" 3 0 3)) ///
    coeflabels(RD_Estimate "Estimate") ///
    plain noobs nostar nogap nonumbers nomtitles nolines ///
    tex fragment collabels(none)

	eststo clear 
	rd_tab, y("exp_total_per_stu") x("equity_prior") yearcond("& year<=2017")
	
	esttab using "Results/Tex/rd_spending_equity.tex", replace ///
    cells("b(fmt(3))") ///
    stats(CI Bandwidth N_eff ymean, labels( " " "Bandwidth" "N" "Mean Dep. Var.") fmt("%s" 3 0 3)) ///
    coeflabels(RD_Estimate "Estimate") ///
    plain noobs nostar nogap nonumbers nomtitles nolines ///
    tex fragment collabels(none)
	
	eststo clear 
	rd_tab, y("ecd_test") x("equity_prior") 
	
	esttab using "Results/Tex/rd_ecd_test_equity.tex", replace ///
    cells("b(fmt(3))") ///
    stats(CI Bandwidth N_eff ymean, labels( " " "Bandwidth" "N" "Mean Dep. Var.") fmt("%s" 3 0 3)) ///
    coeflabels(RD_Estimate "Estimate") ///
    plain noobs nostar nogap nonumbers nomtitles nolines ///
    tex fragment collabels(none)
	
	
	eststo clear 
	rd_tab, y("exp_total_per_stu") x("hisp") yearcond("& year<=2017") 
	
	esttab using "Results/Tex/rd_spending_hisp.tex", replace ///
    cells("b(fmt(3))") ///
    stats(CI Bandwidth N_eff ymean, labels( " " "Bandwidth" "N" "Mean Dep. Var.") fmt("%s" 3 0 3)) ///
    coeflabels(RD_Estimate "Estimate") ///
    plain noobs nostar nogap nonumbers nomtitles nolines ///
    tex fragment collabels(none)	
	