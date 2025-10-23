********************************************************************************
* RD with Indidividual Characteristics
* Written By Minseon Park 11/18/24
********************************************************************************

set seed 1234

* set directory 
cd "~/Library/CloudStorage/Dropbox/California Election Data/Code"
global logpath "~/Library/CloudStorage/Dropbox/California Election Data/Logs"

global y_rev ="rev_cte_per_stu rev_f_s_drug_free_per_stu rev_bilingual_ed_per_stu rev_s_gte_per_stu rev_lunch_per_stu rev_s_basic_ed_per_stu athletic    hvac        landpurch   class       elementary  middle      high        stem        safeall     infra"
global y_fiscal= "exp_total_per_stu exp_capital_total_per_stu bond_amount_per_stu"
global y_teacher = "ba60_sal_Bcomp_log entry_sal_Bcomp_log teacher_exit_dist "
global y_score = "test test_pred test_resid ecd_test ecd_test_pred ecd_test_resid"
global y_enroll = "enrollment enrollment_charter_share enrollment_charter n_charter charter_any charter_exp_share"
global y_behv = ""
global y_charter = "enrollment_charter_share charter_any n_charter_sch"
global y_sup = "sup_change"


global y_rev ="" 
global y_fiscal= ""
global y_teacher = "  "
global y_score = ""
global y_enroll = ""
global y_behv = ""
global y_charter = ""
global y_sup = ""
	
global tab_opt = "b(a2) se(2) scalars(cl_l cl_h pval bw N_eff ymean) label star(+ 0.1 * 0.05 ** 0.01)"

global list = " budget_hawk"
global class "entry ba60"

global flag_balance = 0 // 0 if main rd 1 if balance
if $flag_balance==0 {
 global year_cond0 = "year>=year_elected & year<=2017 "
 global year_cond1 = "year>=year_elected "
 global year_cond2 = "year>=year_elected "
  global year_cond3 = "year>=year_elected & year<=2017 & agency_charter_indicator~=1"
}	
else if $flag_balance==1 {
 global year_cond0 = "year<year_elected-1 & year<=2017"	
 global year_cond1 = "year==year_elected-2"
 global year_cond2 = "year<year_elected"
} 

 
foreach x in $list {

eststo clear

use "data_for_rd/with_missing/dist_`x'.dta" if dist_cnty~=1, clear

	g charter_any = cond(agency_charter_indicator==2,1,0) if agency_charter_indicator>=2 & agency_charter_indicator<=3
	ren *vocational* *cte*
	
	
	*** 1/ Generate more detailed spending variables
	g sal_oth_per_stu=(sal_total - sal_inst)/enrollment
	g sal_reg_per_stu=sal_tch_regular_prog/enrollment
	g sal_sped_per_stu=sal_tch_sped/enrollment
	g log_sal_oth_per_stu = log(sal_oth_per_stu)
	g exp_admin_per_stu = (exp_sch_admin+exp_general_admin)/enrollment
	g exp_enterprise_per_stu = exp_enterprise /enrollment
	g exp_operation_plant_per_stu =exp_operation_plant/enrollment
 
	ren exp_capital_construction exp_cap_const
	ren exp_capital_land_structures exp_cap_land
	g exp_capital_equip = exp_capital_instruc_equip + exp_capital_other_equip

	foreach y in exp_cap_const exp_cap_land exp_capital_equip {
		g `y'_per_stu=`y'/enrollment
		g log_`y'_per_stu = log(`y'_per_stu)
	}	
	
	g exp_nocapital_per_stu = exp_total_per_stu -exp_cap_const_per_stu
	g exp_noland_per_stu = exp_total_per_stu - exp_cap_land_per_stu
	g log_exp_nocapital_per_stu = log(exp_nocapital_per_stu)
	g log_exp_noland_per_stu = log(exp_noland_per_stu)
	
do 4_f_merge_tsal
*g ba60_share = ba60_totalfte/ts1_totfte
*g entry_share = entry_totalfte/ts1_totfte
g log_enrollment=log(enrollment)

do 4_f_prep_rd

	xtset id_election year
	keep if year>=year_elected
	tsegen double sup_change_cum = rowmax(L(0/4).sup_change) // or rowtotal


********************************************************************************
* I. Pooled RD 
********************************************************************************

foreach y in $y_rev $y_fiscal {	
	eststo: rdrobust `y'_D vote_margin if $year_cond0, p(1) kernel(triangular) bwselect(mserd) covs(yr_*) vce(nncluster id_election)
	estadd scalar cl_l= e(ci_l_rb) 
	estadd scalar cl_h= e(ci_r_rb)
	estadd scalar pval = e(pv_rb)
	estadd scalar bw = e(h_l)
	estadd scalar N_eff = e(N_h_l) + e(N_h_r)	
	qui estadd ysumm
}
foreach y in $y_score $y_teacher $y_enroll {	
	eststo: rdrobust `y'_D vote_margin if $year_cond1, p(1) kernel(triangular) bwselect(mserd) covs(yr_*) vce(nncluster id_election)
	estadd scalar cl_l= e(ci_l_rb)
	estadd scalar cl_h= e(ci_r_rb)
	estadd scalar pval = e(pv_rb)
	estadd scalar bw = e(h_l)
	estadd scalar N_eff = e(N_h_l) + e(N_h_r)
	qui estadd ysumm
}

foreach y in $y_behv {	
	eststo: rdrobust `y'_D vote_margin if $year_cond2, p(1) kernel(triangular) bwselect(mserd) covs(yr_*) vce(nncluster id_election)
	estadd scalar cl_l= e(ci_l_rb)
	estadd scalar cl_h= e(ci_r_rb)
	estadd scalar pval = e(pv_rb)
	estadd scalar bw = e(h_l)
	estadd scalar N_eff = e(N_h_l) + e(N_h_r)
	qui estadd ysumm
}
foreach y in $y_charter {	
	eststo: rdrobust `y'_D vote_margin if $year_cond3, p(1) kernel(triangular) bwselect(mserd) covs(yr_*) vce(nncluster id_election)
	estadd scalar cl_l= e(ci_l_rb)
	estadd scalar cl_h= e(ci_r_rb)
	estadd scalar pval = e(pv_rb)
	estadd scalar bw = e(h_l)
	estadd scalar N_eff = e(N_h_l) + e(N_h_r)		
	qui estadd ysumm
}

foreach y in $y_sup {	
	eststo: rdrobust `y'_D vote_margin if $year_cond1, p(1) kernel(triangular) bwselect(mserd) covs(yr_*) vce(nncluster id_election)
	estadd scalar cl_l= e(ci_l_rb)
	estadd scalar cl_h= e(ci_r_rb)
	estadd scalar pval = e(pv_rb)
	estadd scalar bw = e(h_l)
	estadd scalar N_eff = e(N_h_l) + e(N_h_r)
	estadd scalar ymean =  e(beta_p_l)[1,1]
	qui estadd ysumm
}

foreach y in $y_sup {	
	eststo: rdrobust `y' vote_margin if $year_cond1, p(1) kernel(triangular) bwselect(mserd) covs(yr_*) vce(nncluster id_election)
	estadd scalar cl_l= e(ci_l_rb)
	estadd scalar cl_h= e(ci_r_rb)
	estadd scalar pval = e(pv_rb)
	estadd scalar bw = e(h_l)
	estadd scalar N_eff = e(N_h_l) + e(N_h_r)
	qui estadd ysumm
}
esttab, $tab_opt
esttab using Results/results_temp.csv, $tab_opt append
}
