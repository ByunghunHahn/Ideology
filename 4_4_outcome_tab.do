********************************************************************************
* Appendix tables with raw RD estimates
* Written by Minseon Park 05/04/25
********************************************************************************

version 18.5
clear all
set seed 1234

* set directory 
cd "~/Library/CloudStorage/Dropbox/California Election Data/Code"
global logpath "~/Library/CloudStorage/Dropbox/California Election Data/Logs"
global grpath ="Results/Graph"

global flag_balance =0

global y_rev ="rev_cte_per_stu"
global y_fiscal= "exp_total_per_stu exp_capital_total_per_stu"
global y_teacher = "teacher_exit_dist ba60_sal_Bcomp_log entry_sal_Bcomp_log"
global y_score = "ecd_test hsp_test test"
global y_enroll = "log_enrollment"
global y_behv = " sus_share"
global y_charter = ""
global y_sup = "sup_change"


global class "entry ba60"


// 15 - ideologies (dropout - enrollment + ecd test hisp test)
// 4 * 13 - identities 

	capture program drop rd_tab
	program define rd_tab
	  syntax, y(string) x(string) [yearcond(string)]
			
		use "data_for_rd/with_missing/dist_`x'.dta", clear

		ren *vocational* *cte*
		g log_enrollment=log(enrollment)
		
		do 4_f_merge_tsal
		do 4_f_prep_rd
	
		replace sup_change_D = sup_change
		
		foreach yy in `y' {	
		eststo: rdrobust `yy'_D vote_margin if year>=year_elected `yearcond', p(1) kernel(triangular) bwselect(mserd) covs(yr_*) vce(nncluster id_election)
		estadd scalar beta = round(tau_cl,0.001)
		estadd local CI = "[" + string(round(e(ci_l_rb),0.001),"%4.3f") + ", " + string(round(e(ci_r_rb),0.01),"%4.3f") + "]"
		estadd scalar cl_l= round(e(ci_l_rb),0.001)
		estadd scalar cl_h= round(e(ci_r_rb),0.001)
		estadd scalar Bandwidth = round(e(h_l),0.001)
		estadd scalar N_eff = round(e(N_h_l) + e(N_h_r)	,1)
		estadd scalar ymean =  e(beta_Y_p_l)[1,1]
		}		
	end
	
	foreach x in $list_demo {
	eststo clear 
	rd_tab, y("$y_fiscal $y_rev") x("`x'") yearcond("& year<=2017")
	rd_tab, y("$y_enroll $y_score $y_behv") x("`x'") 
	
	esttab using "Results/Tex/rd_uni_`x'.tex", replace ///
    cells("b(fmt(3))") ///
    stats(CI Bandwidth N_eff ymean, labels( " " "Bandwidth" "N" "Mean Dep. Var. ") fmt("%s" 3 0 3)) ///
    coeflabels(RD_Estimate "Estimate") ///
    plain noobs nostar nogap nonumbers nomtitles nolines ///
    tex fragment collabels(none)
	}
	
	foreach x in $list_demo {
	eststo clear 
	rd_tab, y("$y_teacher $y_sup") x("`x'") 
	
	esttab using "Results/Tex/rd_uni2_`x'.tex", replace ///
    cells("b(fmt(3))") ///
    stats(CI Bandwidth N_eff ymean, labels("" "Bandwidth" "N" "Mean Dep. Var. ") fmt("%s" 3 0 3)) ///
    coeflabels(RD_Estimate "Estimate") ///
    plain noobs nostar nogap nonumbers nomtitles nolines ///
    tex fragment collabels(none)
	}
	

	eststo clear 
	rd_tab, y("exp_total_per_stu") x("budget_hawk") yearcond("& year<=2017")
	rd_tab, y("exp_capital_total_per_stu") x("facility_prior") yearcond("& year<=2017")	
	rd_tab, y("rev_cte_per_stu") x("cte_prior") yearcond("& year<=2017")	
	rd_tab, y("log_enrollment") x("enrollment_prior") 
	rd_tab, y("log_enrollment") x("dropout_prior") 
	rd_tab, y("ecd_test hsp_test") x("equity_prior") 
	rd_tab, y("test") x("score_concern") 
	rd_tab, y("sus_share") x("safety_health_prior") 
			
	esttab using "Results/Tex/rd_uni_prio.tex", replace ///
    cells("b(fmt(3))") ///
    stats(CI Bandwidth N_eff ymean, labels( " " "Bandwidth" "N" "Mean Dep. Var. ") fmt("%s" 3 0 3)) ///
    coeflabels(RD_Estimate "Estimate") ///
    plain noobs nostar nogap nonumbers nomtitles nolines ///
    tex fragment collabels(none)
	
	
	eststo clear 
	rd_tab, y("$y_teacher") x("teacher_care") 
	rd_tab, y("$y_sup") x("sup_concern")
	
	esttab using "Results/Tex/rd_uni2_prio.tex", replace ///
    cells("b(fmt(3))") ///
    stats(CI Bandwidth N_eff ymean, labels( " " "Bandwidth" "N" "Mean Dep. Var. ") fmt("%s" 3 0 3)) ///
    coeflabels(RD_Estimate "Estimate") ///
    plain noobs nostar nogap nonumbers nomtitles nolines ///
    tex fragment collabels(none)	
