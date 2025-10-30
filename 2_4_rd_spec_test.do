********************************************************************************
* RD Specification Tests
* Written by Minseon Park 06/22/25
* Latest updated by Minseon Park 07/13/25
* I. McCrary Test
* II. Prep to Run Stacked RDs
* III. Selection into Outcome Sample
* IV. Balance Test
* V. Selection in Meeting Record
* Note: While this table shows up much earlier in the draft, we have to use all 
* 	prior dofiles to construct the table of this dofile.
********************************************************************************

version 18.5
clear all
set seed 1234

* set directory 
cd "C:\Users\hahn0\Desktop\Hahn_Park\Code"
global logpath "New_Logs"

global y_rev =""
global y_fiscal= "rev_cte_per_stu exp_total_per_stu exp_capital_total_per_stu bond_amount_per_stu"
global y_teacher = "teacher_exit_dist entry_sal_Bcomp_log ba60_sal_Bcomp_log"
global y_score = "ecd_test hsp_test test"
global y_enroll = "log_enrollment"
global y_behv = "sus_share"	
global y_sup = ""
	
global class = "entry ba60"

global missing="with_missing"
global datapath = "data_for_rd/$missing"	

global list_demo = "hisp female occ_teacher democrat_v2"
global list_prio ="equity_prior budget_hawk agenda_bias parent_involvement cte_prior dropout_prior enrollment_prior facility_prior safety_health_prior teacher_care sup_concern score_concern"

global list_outcome_main ="rev_cte_per_stu exp_total_per_stu exp_capital_total_per_stu teacher_exit_dist entry_sal_Bcomp_log ba60_sal_Bcomp_log ecd_test hsp_test test log_enrollment sus_share sup_change_cum" // main outcomes to be shown in stacked outcome RD results
	
global flag_balance =0

global graph_opt ="esll_opt(lcolor(ebblue*2) lwidth(med)) eslr_opt(lcolor(dkorange*1.2) lwidth(med)) cirl_opt(color(ebblue*2%30)) cirr_opt(color(dkorange*1.2%30)) histl_opt(color(ebblue%30)) histr_opt(color(dkorange%30))"


********************************************************************************
* I. McCrary Test
********************************************************************************
		
	* Fiscal Conservatism
	use data_for_rd/with_missing/dist_budget_hawk.dta if year == year_elected, clear
	rddensity vote_margin, c(0)
	scalar p_mc_fc = e(pv_q)
	scalar N_mc_fc = e(N_h_l) + e(N_h_r)

	* Equity
	use data_for_rd/with_missing/dist_equity_prior.dta if year == year_elected, clear
	rddensity vote_margin, c(0)
	scalar p_mc_eq = e(pv_q)
	scalar N_mc_eq = e(N_h_l) + e(N_h_r)

	* Hispanic
	use data_for_rd/with_missing/dist_hisp.dta if year == year_elected, clear
	rddensity vote_margin, c(0)
	scalar p_mc_hi = e(pv_q)
	scalar N_mc_hi = e(N_h_l) + e(N_h_r)


	use data_for_rd/with_missing/dist_stacked.dta if year == year_elected, clear

	* Identity - Existing Literature
	preserve
	keep m_hisp m_occ_teacher multi_raceid
	g id = _n
	
	ren (m_hisp m_occ_teacher) (m1 m2)
	reshape long m, i(id) j(treatment)
	
	rddensity m
	scalar p_mc_ex = e(pv_q)
	scalar N_mc_ex = r(N)
	restore
	
	* Identity - Others
	preserve
	keep m_hisp m_occ_teacher m_female m_democrat_v2 multi_raceid
	g id = _n
	
	ren (m_hisp m_occ_teacher m_female m_democrat_v2) (m1 m2 m3 m4)
	reshape long m, i(id) j(treatment)
	
	rddensity m
	scalar p_mc_iden = e(pv_q)
	scalar N_mc_iden = r(N)
	restore
	
	* Ideology - All
	preserve
	keep m_budget_hawk m_equity_prior m_safety_health_prior m_teacher_care m_score_concern m_facility_prior m_cte_prior m_dropout_prior m_enrollment_prior m_sup_concern m_agenda_bias m_parent_involvement multi_raceid
	g id = _n
	
	ren (m_budget_hawk m_equity_prior m_safety_health_prior m_teacher_care m_score_concern m_facility_prior m_cte_prior m_dropout_prior m_enrollment_prior m_sup_concern m_agenda_bias m_parent_involvement) (m1 m2 m3 m4 m5 m6 m7 m8 m9 m10 m11 m12)
	reshape long m, i(id) j(treatment)
	
	rddensity m
	scalar p_mc_ideo = e(pv_q)
	scalar N_mc_ideo = r(N)
	restore	

	* Now export the results
	file open myfile using "Results/New_Tex/rd_spec_test.tex", write replace
	file write myfile "\begin{tabular}{lcccccc}" _n
	file write myfile "\toprule \hline" _n
	file write myfile "& \multicolumn{3}{c}{Case-Study Characteristics} & \multicolumn{3}{c}{Aggregates Across All Characteristics}\\ \cmidrule(l{0.5cm}){2-4} \cmidrule(l{0.5cm}){5-7}" _n
	file write myfile "& Equity & Fiscally Cons. & Hispanic & Identity & Identity & Ideology \\" _n
	file write myfile "& & Conservative & & (Literature) & (Other) & \\" _n
	file write myfile "& (1) & (2) & (3) & (4) & (5) & (6) \\" _n
	file write myfile "\midrule" _n
	
	* Row 1: McCrary test p-values
	file write myfile "\multicolumn{7}{l}{\textit{\textbf{Panel A: McCrary Test}}} \\" _n
	foreach x in eq fc hi ex iden ideo {
		local `x'_pval = string(p_mc_`x', "%4.3f")
		local `x'_N = string(N_mc_`x', "%4.0f")
	}
	file write myfile "p-val & `eq_pval' & `fc_pval' & `hi_pval' & `ex_pval' & `iden_pval' & `ideo_pval' \\" _n
	file write myfile "N & `eq_N' & `fc_N' & `hi_N' & `ex_N' & `iden_N' & `ideo_N' \\" _n
	file write myfile " &  &  &  &  &  &  \\" _n	
	gen pvals = .

	local i = 1
	foreach var in $list_demo $list_prio {
		* Run McCrary
		rddensity m_`var', c(0)
		replace pvals = e(pv_q) in `i'
		local ++i
	}

	* Histogram of p-values
	histogram pvals, bin(10) frequency fcolor(purple*1.3) lcolor(black) xtitle("p-values")
	gr export "Results/New_Graph/mccrary_pvals.png", replace	

	
********************************************************************************
* II. Prep to Run Stacked RDs
********************************************************************************

	use "$datapath/stacked.dta", clear
	
	rddensity m_hisp if year==year_elected, pl plot_range(-25 25) plot_n(100 100)
    local pv = round(e(pv_q), .01)
    local h_l = -e(h_l)
    local h_r = e(h_r)
    local N = e(N_h_l) + e(N_h_r)
	
	rddensity m_hisp if year==year_elected, pl plot_range(-25 25) plot_n(100 100) ///
	graph_opt(xtitle("Vote Margin") title("Hispanic") ytitle("Density") legend(off) note("p-value = `pv',  N = `N'", ring(0) pos(1) size(*1.5)) xline(`h_l' `h_r')) $graph_opt 
	gr save "Results/New_Graph/density_test_hisp", replace
	
	rddensity m_budget_hawk if year==year_elected, pl plot_range(-25 25) plot_n(100 100)
    local pv = round(e(pv_q), .01)
    local h_l = -e(h_l)
    local h_r = e(h_r)
    local N = e(N_h_l) + e(N_h_r)
	
	rddensity m_budget_hawk if year==year_elected, pl plot_range(-25 25) plot_n(100 100) ///
	graph_opt(xtitle("Vote Margin") title("Fiscally Conservative") ytitle("Density") legend(off) note("p-value = `pv',  N = `N'", ring(0) pos(1) size(*1.5)) xline(`h_l' `h_r')) $graph_opt 
	gr save "Results/New_Graph/density_test_budget_hawk", replace
	
	rddensity m_equity_prior if year==year_elected, pl plot_range(-25 25) plot_n(100 100)
    local pv = round(e(pv_q), .01)
    local h_l = -e(h_l)
    local h_r = e(h_r)
    local N = e(N_h_l) + e(N_h_r)
	
	rddensity m_equity_prior if year==year_elected, pl plot_range(-25 25) plot_n(100 100) ///
	graph_opt(xtitle("Vote Margin") title("Equity") ytitle("Density") legend(off) note("p-value = `pv',  N = `N'", ring(0) pos(1) size(*1.5)) xline(`h_l' `h_r')) $graph_opt 
	gr save "Results/New_Graph/density_test_equity", replace
		
	gr combine "Results/New_Graph/density_test_budget_hawk" "Results/New_Graph/density_test_equity" "Results/New_Graph/density_test_hisp" , col(3) imargin(zero) ysize(2) iscale(1.2)
	   gr export "Results/New_Graph/density_test_$missing.png", replace
	   
	tostring id_district_nces, replace  // Convert to string if not already	
	gen leaid = string(real(id_district_nces), "%07.0f")  // Adjust "8" to the required width

	*** merge outcomes
	keep leaid year m_* year_elected state_leaid multi_raceid id_district_nces
	destring leaid, replace
	merge m:1 leaid year using "Temp/outcomes_z.dta", gen(_outcome_z) force keep(master matched)
	
	do 4_f_merge_tsal 
	
	foreach y in entry_sal_Bcomp_log ba60_sal_Bcomp_log { // these are the variables with which I don't know how to do normalization within each year, since we define the outcome using the election year
		egen z=std(`y')
		replace `y'=z
		drop z
	}
	
	do 4_f_prep_rd_new


********************************************************************************
* III. Selection into Outcome Sample
********************************************************************************
	
	ereturn clear
	estimates clear
	eststo clear
		
	*** 1/ Run RDs
	preserve 
	egen multi_raceid_num= group(multi_raceid)
	xtset multi_raceid_num year
	keep if year>=year_elected
	tsegen double sup_change_cum_D = rowmax(L(0/4).sup_change) 
	// this is not a diff variable but this way it's consistent with the rest of vars so makes our life easier

	foreach y in sup_change_cum_D { // these are the variables with which I don't know how to do normalization within each year, since we define the outcome using the election year
		egen z=std(`y')
		replace `y'=z
		drop z
	}
		
	foreach y in $list_outcome_main { // use missing as an outcome variable 
		forvalues t=1994/2026 {
			su `y'_D if year==`t'
			replace `y'_D = missing(`y'_D,0,1) if year==`t' & r(N)~=0
			replace `y'_D=. if year==`t' & r(N)==0
		}
	}
	
	do 4_f_stacked_rd // run individually
	stacked_rd, yearcond("year>=year_elected") se_type("cluster id_election")
	do 4_f_stacked_combine // stack and run suest
	scalar T1_N_ex = e(N)	
	scalar T1_N_iden = e(N)
	scalar T1_N_ideo = e(N)	
	
	*** 2/ Store Results
	* Block: Fiscal Conservatism
	rdrobust exp_total_per_stu_D m_budget_hawk if year>=year_elected & year<=2017, p(1) kernel(uniform) bwselect(mserd) covs(yr_*) vce(cluster id_election)
	scalar T1_beta_fc = e(tau_bc)
	scalar T1_ci_l_fc = e(ci_l_rb)
	scalar T1_ci_h_fc = e(ci_r_rb)
	scalar T1_N_fc =  e(N_h_l) + e(N_h_r)	
	scalar T1_ymean_fc =  e(beta_Y_p_l)[1,1]
	
	* Block: Equity
	rdrobust ecd_test_D m_equity_prior if year>=year_elected, p(1) kernel(uniform) bwselect(mserd) covs(yr_*) vce(cluster id_election)
	scalar T1_beta_eq = e(tau_bc)
	scalar T1_ci_l_eq = e(ci_l_rb)
	scalar T1_ci_h_eq = e(ci_r_rb)
	scalar T1_N_eq =  e(N_h_l) + e(N_h_r)	
	scalar T1_ymean_eq =  e(beta_Y_p_l)[1,1]
	
	* Block: Hispanic
	rdrobust exp_total_per_stu_D m_hisp if year>=year_elected, p(1) kernel(uniform) bwselect(mserd) covs(yr_*) vce(cluster id_election)
	scalar T1_beta_hi = e(tau_bc)
	scalar T1_ci_l_hi = e(ci_l_rb)
	scalar T1_ci_h_hi = e(ci_r_rb)
	scalar T1_N_hi =  e(N_h_l) + e(N_h_r)	
	scalar T1_ymean_hi =  e(beta_Y_p_l)[1,1]
	
	* Block: Aggregate
	scalar T1_beta_ex = theta_unw_sub
	scalar T1_ci_l_ex = theta_unw_sub - invnormal(0.975)*theta_se_unw_sub
	scalar T1_ci_h_ex = theta_unw_sub + invnormal(0.975)*theta_se_unw_sub
	
	scalar T1_beta_iden = theta_unw_iden
	scalar T1_ci_l_iden = theta_unw_iden - invnormal(0.975)*theta_se_unw_iden
	scalar T1_ci_h_iden = theta_unw_iden + invnormal(0.975)*theta_se_unw_iden
	
	scalar T1_beta_ideo= theta_unw_ideo
	scalar T1_ci_l_ideo = theta_unw_ideo - invnormal(0.975)*theta_se_unw_ideo
	scalar T1_ci_h_ideo = theta_unw_ideo + invnormal(0.975)*theta_se_unw_ideo
	restore
		
	* Row 2: Outcome sample selection (beta + CI)
	file write myfile "\multicolumn{7}{l}{\textit{\textbf{Panel B: Selection into Outcome Sample}}} \\" _n
	foreach x in eq fc hi ex iden ideo {
		local `x'_beta = string(T1_beta_`x', "%4.3f")
		local `x'_cl = string(T1_ci_l_`x', "%4.3f")
		local `x'_ch = string(T1_ci_h_`x', "%4.3f")
		local `x'_N = string(T1_N_`x', "%4.0f")
	}
	file write myfile "Estimate & `eq_beta' & `fc_beta' & `hi_beta' & `ex_beta' & `iden_beta' & `ideo_beta' \\" _n
	file write myfile "$ $ & [`eq_cl', `eq_ch'] & [`fc_cl', `fc_ch'] & [`hi_cl', `hi_ch'] & [`ex_cl', `ex_ch']  & [`iden_cl', `iden_ch']  & [`ideo_cl', `ideo_ch']  \\" _n
	file write myfile "N & `eq_N' & `fc_N' & `hi_N' & \multicolumn{3}{c}{`ex_N'} \\" _n
		
		
********************************************************************************
* IV. Balance Test
********************************************************************************
	
	ereturn clear
	estimates clear
	eststo clear
		
	*** 1/ Run RDs
	preserve 
	egen multi_raceid_num= group(multi_raceid)
	xtset multi_raceid_num year
	keep if year<=year_elected-1
	tsegen double sup_change_cum_D = rowmax(F(0/3).sup_change) // or rowtotal
		
	// this is not a diff variable but this way it's consistent with the rest of vars so makes our life easier

	foreach y in sup_change_cum_D { // these are the variables with which I don't know how to do normalization within each year, since we define the outcome using the election year
		egen z=std(`y')
		replace `y'=z
		drop z
	}
			
	do 4_f_stacked_rd // run individually
	stacked_rd, yearcond("year<=year_elected-2") se_type("cluster id_election")
	do 4_f_stacked_combine // stack and run suest
	scalar T2_N_ex = e(N)	
	scalar T2_N_iden = e(N)
	scalar T2_N_ideo = e(N)	
	
	*** 2/ Store Results
	* Block: Fiscal Conservatism
	rdrobust exp_total_per_stu_D m_budget_hawk if year<=year_elected-2 & year<=2017, p(1) kernel(uniform) bwselect(mserd) covs(yr_*) vce(nncluster id_election)
	scalar T2_beta_fc = e(tau_bc)
	scalar T2_ci_l_fc = e(ci_l_rb)
	scalar T2_ci_h_fc = e(ci_r_rb)
	scalar T2_N_fc =  e(N_h_l) + e(N_h_r)	
	scalar T2_ymean_fc =  -e(beta_Y_p_l)[1,1]
		
	* Block: Equity
	rdrobust ecd_test_D m_equity_prior if year<=year_elected-2, p(1) kernel(uniform) bwselect(mserd) covs(yr_*) vce(nncluster id_election)
	scalar T2_beta_eq = e(tau_bc)
	scalar T2_ci_l_eq = e(ci_l_rb)
	scalar T2_ci_h_eq = e(ci_r_rb)
	scalar T2_N_eq =  e(N_h_l) + e(N_h_r)	
	scalar T2_ymean_eq =  -e(beta_Y_p_l)[1,1]
			
	* Block: Hispanic
	rdrobust exp_total_per_stu_D m_hisp if year<=year_elected-2, p(1) kernel(uniform) bwselect(mserd) covs(yr_*) vce(nncluster id_election)
	scalar T2_beta_hi = e(tau_bc)
	scalar T2_ci_l_hi = e(ci_l_rb)
	scalar T2_ci_h_hi = e(ci_r_rb)
	scalar T2_N_hi =  e(N_h_l) + e(N_h_r)	
	scalar T2_ymean_hi =  -e(beta_Y_p_l)[1,1]
			
	* Block: Aggregate
	scalar T2_beta_ex = theta_unw_sub
	scalar T2_ci_l_ex = theta_unw_sub - invnormal(0.975)*theta_se_unw_sub
	scalar T2_ci_h_ex = theta_unw_sub + invnormal(0.975)*theta_se_unw_sub

	scalar T2_beta_iden = theta_unw_iden
	scalar T2_ci_l_iden = theta_unw_iden - invnormal(0.975)*theta_se_unw_iden
	scalar T2_ci_h_iden = theta_unw_iden + invnormal(0.975)*theta_se_unw_iden
	
	scalar T2_beta_ideo= theta_unw_ideo
	scalar T2_ci_l_ideo = theta_unw_ideo - invnormal(0.975)*theta_se_unw_ideo
	scalar T2_ci_h_ideo = theta_unw_ideo + invnormal(0.975)*theta_se_unw_ideo
	restore
	
	* Row 3: Balance of Outcome
	file write myfile "\multicolumn{7}{l}{\textit{\textbf{Panel C: Pre-Treatment Outcome Balance}}} \\" _n
	foreach x in eq fc hi ex iden ideo {
		local `x'_beta = string(T2_beta_`x', "%4.3f")
		local `x'_cl = string(T2_ci_l_`x', "%4.3f")
		local `x'_ch = string(T2_ci_h_`x', "%4.3f")
		local `x'_N = string(T2_N_`x', "%4.0f")
	}
	file write myfile "Estimate & `eq_beta' & `fc_beta' & `hi_beta' & `ex_beta' & `iden_beta' & `ideo_beta' \\" _n
	file write myfile "$ $ & [`eq_cl', `eq_ch'] & [`fc_cl', `fc_ch'] & [`hi_cl', `hi_ch'] & [`ex_cl', `ex_ch']  & [`iden_cl', `iden_ch']  & [`ideo_cl', `ideo_ch']  \\" _n
	local col_ex = "\multicolumn{3}{c}{`T2_N_ex'}"
	file write myfile "N & `eq_N' & `fc_N' & `hi_N' & \multicolumn{3}{c}{`ex_N'} \\" _n
		

********************************************************************************
* V. Selection in Meeting Record
********************************************************************************

	ereturn clear
	estimates clear
	eststo clear
		
	* Merge motion
	merge m:1 leaid year using "Temp/minutes_final_year_level.dta", keep(master matched) gen(_motion) // generated from Minutes/code/merge_minutes_elections.do (last section)
	
		forvalues t=1994/2026 {
			su _motion if year==`t'
			replace _motion =. if year==`t' & r(max)==1
			recode _motion (3=1) (1=0) if year==`t' & r(max)==3			
			cap label define _motion 1 "Yes Meeting Record" 0 "No"
			label values _motion _motion
		}
	

	foreach x in $list_demo $list_prio  {
		gen T_`x' = (m_`x' >= 0 & !missing(m_`x'))
		gen m2_`x' = cond(missing(m_`x'), 0, m_`x')
	
		rdrobust _motion m_`x' if year>=year_elected, p(1) kernel(uniform) bwselect(mserd) covs(yr_*) vce(cluster id_election)
		local bw = e(h_l)
		gen S_`x' = (abs(m_`x') <= `bw')
		
		// Create interactions	
		g T_`x'_S = T_`x' * S_`x'
		g m_`x'_S = m2_`x' * S_`x'	
		g int_`x' = T_`x'_S*m_`x'_S
		
		replace T_`x'_S = -T_`x'_S
		replace m_`x'_S = -m_`x'_S
		replace int_`x' = -int_`x'
		
		reg _motion T_`x'_S m_`x'_S int_`x' S_`x' ib(2020).year if year>=year_elected
		
			preserve	
			use "Temp/h0_list_demo_outcome.dta" if outcome_main==1 & strpos(demo, "`x'"), clear
			count if h_type_exact == 1
					local ht_suffix = cond(r(N) > 0, "_ht1", "")
			restore
		est store M_`x'`ht_suffix'
	}
	do 4_f_stacked_combine // stack and run suest
	  
	scalar T3_N_ex = e(N)	
	scalar T3_N_iden = e(N)
	scalar T3_N_ideo = e(N)
	
	*** 2/ Store Results
	* Block: Fiscal Conservatism
	rdrobust _motion m_budget_hawk if year<=year_elected-1 & year<=2017, p(1) kernel(uniform) bwselect(mserd) covs(yr_*) vce(cluster id_election)
	scalar T3_beta_fc = e(tau_bc)
	scalar T3_ci_l_fc = e(ci_l_rb)
	scalar T3_ci_h_fc = e(ci_r_rb)
	scalar T3_N_fc =  e(N_h_l) + e(N_h_r)	
	scalar T3_ymean_fc =  e(beta_Y_p_l)[1,1]
			
	* Block: Equity
	rdrobust _motion m_equity_prior if year<=year_elected-1, p(1) kernel(uniform) bwselect(mserd) covs(yr_*) vce(cluster id_election)
	scalar T3_beta_eq = e(tau_bc)
	scalar T3_ci_l_eq = e(ci_l_rb)
	scalar T3_ci_h_eq = e(ci_r_rb)
	scalar T3_N_eq =  e(N_h_l) + e(N_h_r)	
	scalar T3_ymean_eq =  e(beta_Y_p_l)[1,1]
			
	* Block: Hispanic
	rdrobust _motion m_hisp if year<=year_elected-1, p(1) kernel(uniform) bwselect(mserd) covs(yr_*) vce(cluster id_election)
	scalar T3_beta_hi = e(tau_bc)
	scalar T3_ci_l_hi = e(ci_l_rb)
	scalar T3_ci_h_hi = e(ci_r_rb)
	scalar T3_N_hi =  e(N_h_l) + e(N_h_r)	
	scalar T3_ymean_hi =  e(beta_Y_p_l)[1,1]
			
	* Block: Aggregate
	scalar T3_beta_ex = theta_unw_sub
	scalar T3_ci_l_ex = theta_unw_sub - invnormal(0.975)*theta_se_unw_sub
	scalar T3_ci_h_ex = theta_unw_sub + invnormal(0.975)*theta_se_unw_sub

	scalar T3_beta_iden = theta_unw_iden
	scalar T3_ci_l_iden = theta_unw_iden - invnormal(0.975)*theta_se_unw_iden
	scalar T3_ci_h_iden = theta_unw_iden + invnormal(0.975)*theta_se_unw_iden
	
	scalar T3_beta_ideo= theta_unw_ideo
	scalar T3_ci_l_ideo = theta_unw_ideo - invnormal(0.975)*theta_se_unw_ideo
	scalar T3_ci_h_ideo = theta_unw_ideo + invnormal(0.975)*theta_se_unw_ideo

	* Row 3: Balance of Outcome
	file write myfile "\multicolumn{7}{l}{\textit{\textbf{Panel D: Selection into Minutes Sample}}} \\" _n
	foreach x in eq fc hi ex iden ideo {
		local `x'_beta = string(T2_beta_`x', "%4.3f")
		local `x'_cl = string(T3_ci_l_`x', "%4.3f")
		local `x'_ch = string(T3_ci_h_`x', "%4.3f")
		local `x'_N = string(T3_N_`x', "%4.0f")
	}
	file write myfile "Estimate & `eq_beta' & `fc_beta' & `hi_beta' & `ex_beta' & `iden_beta' & `ideo_beta' \\" _n
	file write myfile "$ $ & [`eq_cl', `eq_ch'] & [`fc_cl', `fc_ch'] & [`hi_cl', `hi_ch'] & [`ex_cl', `ex_ch']  & [`iden_cl', `iden_ch']  & [`ideo_cl', `ideo_ch']  \\" _n
	local col_ex = "\multicolumn{3}{c}{`T3_N_ex'}"
	file write myfile "N & `eq_N' & `fc_N' & `hi_N' & \multicolumn{3}{c}{`ex_N'} \\" _n

	file write myfile "\bottomrule" _n
	file write myfile "\end{tabular}" _n
	file close myfile
