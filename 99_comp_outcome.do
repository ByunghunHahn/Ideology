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
global mindta "~/Dropbox/California Election Data/Minutes/data/dta"
  
global list_demo = "hisp female occ_teacher democrat_v2 "
global list_prio ="budget_hawk equity_prior safety_health_prior teacher_care"


global y_rev ="rev_cte_per_stu rev_f_s_drug_free_per_stu"
global y_fiscal= "log_exp_total_per_stu exp_capital_total_per_stu"
global y_teacher = "teacher_exit_dist teacher_enter_dist teacher_tenure log_wage_inst_per_tch entry_sal_Bcomp_log ba60_sal_Bcomp_log entry_sal_Bsal_log ba60_sal_Bsal_log"
global y_score = "ecd_test hsp_test"
global y_enroll = "log_enrollment"
global y_behv = "sus_share sus_share_hisp"	

global flag_balance = 0 // 0 if main rd 1 if balance
if $flag_balance==0 {
 global year_cond0 = "year>=year_elected & year<=2017"
 global year_cond1 = "year>=year_elected"
 global year_cond2 = "year>=year_elected"
}	
else if $flag_balance==1 {
 global year_cond0 = "year==year_elected-2 & year<=2017"	
 global year_cond1 = "year==year_elected-2"
 global year_cond2 = "year<year_elected"
} 



	use "outcomes.dta", clear
	ren *vocational* *cte*
	g log_enrollment=log(enrollment)
	foreach y in $y_rev $y_fiscal $y_score $y_enroll $y_behv teacher_exit_dist teacher_enter_dist teacher_tenure log_wage_inst_per_tch  {
	bysort year: egen z=std(`y')
	replace `y' = z
	drop z
	}
	save "Temp/outcomes_z.dta", replace

	foreach x in $list {
	use "Temp/with_missing/vote_share_dist_`x'.dta", clear
	destring leaid, replace	
	g year_elected = year

	expand 9
	bysort leaid year: replace year = year+_n-5

	merge m:1 leaid year using "Temp/outcomes_z.dta", gen(_platform) force keep(master matched)

	ren leaid id_district_nces
	sort id_district_nces year
	save "data_for_rd/with_missing/dist_`x'_z.dta", replace
	}
	
	
********************************************************************************
* I. Run RD 
********************************************************************************

use "data_for_rd/with_missing/dist_hisp.dta" if dist_cnty~=1, clear
rdrobust log_exp_pupils vote_margin if year==year_elected-1 , p(1) kernel(triangular) bwselect(mserd) 
regsave using "Results/outcome_uni_by_comp", replace

foreach xvar in $list_demo $list_prio {
	
	use "data_for_rd/with_missing/dist_`xvar'_z.dta" if dist_cnty~=1, clear
	g test = (cs_mn_all_mth+cs_mn_all_ela)/2
	g hsp_test_gap = (cs_mn_whg_mth+cs_mn_whg_ela)/2
	g ecd_test_gap = (cs_mn_neg_mth+cs_mn_neg_ela)/2
	
	g leaid = id_district_nces
	tostring leaid, replace  // Convert to string if not already

	*** 1) merge with composition
	// Ensure consistent width by adding leading zeros
	gen leaid_padded = string(real(leaid), "%07.0f")  // Adjust "8" to the required width
	replace leaid = leaid_padded
	drop leaid_padded
	
	merge m:1 leaid year using "board_composition/board_composition_elections.dta", keep(master matched) gen(_comp)
	
		
	*** 2) classification by baseline composition
	g size_estimate_b = size_estimate if year==year_elected-1 
	sort multi_raceid size_estimate_b
	bysort multi_raceid: replace size_estimate_b = size_estimate_b[1]
	
	foreach x in `xvar' {
	g `x'_total = 0 
	forvalue n=1/14 {
		replace `x'_total = `x'_total+1 if `x'`n'==1
	}
	g `x'_total2 = 0
	forvalue n=1/14 {
		replace `x'_total2 = `x'_total2+1 if `x'`n'==0 // exclude if invalid platform
	}
	g `x'_share =`x'_total/(`x'_total + `x'_total2)
	}	
	
	g `xvar'_total_b = `xvar'_total if year==year_elected-1 
	sort multi_raceid `xvar'_total_b
	bysort multi_raceid: replace `xvar'_total_b = `xvar'_total_b[1]
	
	g `xvar'_total2_b = `xvar'_total2 if year==year_elected-1 
	sort multi_raceid `xvar'_total2_b
	bysort multi_raceid: replace `xvar'_total2_b = `xvar'_total2_b[1]
	
	g comp_type = cond(`xvar'_total_b==0, 3, cond(`xvar'_total_b<`xvar'_total2_b, 1, 2)) if !missing(size_estimate_b)

	
	la de comp_type 1 "Minority among non-Missing" 2 "Majority among non-Missing" 3 "First among non-Missing"
	la val comp_type comp_type			
	
	
	*** 3) prep RD 
	global class entry ba30 ba60 expr
	do 4_f_merge_tsal 


	foreach y in entry_sal_Bcomp_log ba60_sal_Bcomp_log entry_sal_Bsal_log ba60_sal_Bsal_log {
		egen z=std(`y')
		replace `y'=z
		drop z
	}
		
	do 4_f_prep_rd
	
	
	*** 4) run RD using observed share	
	foreach y in $y_rev $y_fiscal {	
		rdrobust `y'_D vote_margin if $year_cond0, p(1) kernel(triangular) bwselect(mserd) covs(yr_*) vce(nncluster id_election)
		local cl_l= e(ci_l_rb)
		local cl_h= e(ci_r_rb)
		local pval = e(pv_rb)
		local bw = e(h_l)
		local N_eff = e(N_h_l) + e(N_h_r)		
		regsave using "Results/outcome_uni_by_comp", append addlabel(y,"`y'", x,"`xvar'", cl_l, `cl_l', cl_h, `cl_h', pval, `pval', bw, `bw', N_eff, `N_eff', comp, 0) 	
		forvalues t=1/3 {
		rdrobust `y'_D vote_margin if $year_cond0 & comp_type==`t', p(1) kernel(triangular) bwselect(mserd) vce(nncluster id_election)
		local cl_l= e(ci_l_rb)
		local cl_h= e(ci_r_rb)
		local pval = e(pv_rb)
		local bw = e(h_l)
		local N_eff = e(N_h_l) + e(N_h_r)		
		regsave using "Results/outcome_uni_by_comp", append addlabel(y,"`y'", x,"`xvar'", cl_l, `cl_l', cl_h, `cl_h', pval, `pval', bw, `bw', N_eff, `N_eff', comp,`t') 
		}
		}
		
	foreach y in $y_score $y_teacher $y_enroll {	
		rdrobust `y'_D vote_margin if $year_cond1, p(1) kernel(triangular) bwselect(mserd) covs(yr_*) vce(nncluster id_election)
		local cl_l= e(ci_l_rb)
		local cl_h= e(ci_r_rb)
		local pval = e(pv_rb)
		local bw = e(h_l)
		local N_eff = e(N_h_l) + e(N_h_r)		
		regsave using "Results/outcome_uni_by_comp", append addlabel(y,"`y'", x,"`xvar'", cl_l, `cl_l', cl_h, `cl_h', pval, `pval', bw, `bw', N_eff, `N_eff', comp, 0)
	forvalues t=1/3 {
		rdrobust `y'_D vote_margin if $year_cond1 & comp_type==`t', p(1) kernel(triangular) bwselect(mserd) vce(nncluster id_election)
		local cl_l= e(ci_l_rb)
		local cl_h= e(ci_r_rb)
		local pval = e(pv_rb)
		local bw = e(h_l)
		local N_eff = e(N_h_l) + e(N_h_r)		
		regsave using "Results/outcome_uni_by_comp", append addlabel(y,"`y'", x,"`xvar'", cl_l, `cl_l', cl_h, `cl_h', pval, `pval', bw, `bw', N_eff, `N_eff', comp,`t') 
		}
		}
	
	foreach y in $y_behv {	
		rdrobust `y'_D vote_margin if $year_cond2, p(1) kernel(triangular) bwselect(mserd) covs(yr_*) vce(nncluster id_election)
		local cl_l= e(ci_l_rb)
		local cl_h= e(ci_r_rb)
		local pval = e(pv_rb)
		local bw = e(h_l)
		local N_eff = e(N_h_l) + e(N_h_r)		
		regsave using "Results/outcome_uni_by_comp", append addlabel(y,"`y'", x,"`xvar'", cl_l, `cl_l', cl_h, `cl_h', pval, `pval', bw, `bw', N_eff, `N_eff', comp, 0) 	
	forvalues t=1/3 {
		rdrobust `y'_D vote_margin if $year_cond2 & comp_type==`t', p(1) kernel(triangular) bwselect(mserd) vce(nncluster id_election)
		local cl_l= e(ci_l_rb)
		local cl_h= e(ci_r_rb)
		local pval = e(pv_rb)
		local bw = e(h_l)
		local N_eff = e(N_h_l) + e(N_h_r)		
		regsave using "Results/outcome_uni_by_comp", append addlabel(y,"`y'", x,"`xvar'", cl_l, `cl_l', cl_h, `cl_h', pval, `pval', bw, `bw', N_eff, `N_eff', comp,`t') 
		}
		}	
}

*/


********************************************************************************
* II. 2D Graph 
********************************************************************************

	use Results/outcome_uni_by_comp, clear
    drop if y == ""
	
	g prio = "budget_hawk" if y=="log_exp_total_per_stu"
	replace prio="equity_prior" if strpos("ecd_test hsp_test sus_share_hisp",y)
	replace prio="safety_health_prior" if strpos("sus_share",y)
	replace prio="teacher_care" if strpos("log_wage_inst_per_tch entry_sal_Bcomp_log ba60_sal_Bcomp_log teacher_exit_dist teacher_tenure", y)
	replace prio="facility_prior" if y=="exp_capital_total_per_stu"
	replace prio="dropout_prior" if y=="log_enrollment"
	replace prio="cte_prior" if y=="rev_cte_per_stu"
	// didn't include dropout since outcome variables overlap with other priorities
	
	keep if strpos("female hisp occ_teacher democrat_v2", x) | x==prio
	sort x y
	
	*** 1) merge with first stage results 
	ren y y_dist
	ren prio y
	
	preserve 
	use "Results/first_stage_by_comp", clear
	drop if predicted=="yes"
	keep if strpos("female hisp occ_teacher democrat_v2", x) | y==x
	drop if y==""
	ren coef coef_p
	ren pval pval_p
	ren ymean ymean_p
	save "Temp/first_stage_by_comp_pred.dta", replace
	restore
	
	merge m:1 x y comp using "Temp/first_stage_by_comp_pred", keepus(*_p) keep(matched)

	** 2) some fine tuning before drawing graphs
	keep if strpos("log_exp_total_per_stu ecd_test hsp_test sus_share log_wage_inst_per_tch entry_sal_Bcomp_log ba60_sal_Bcomp_log teacher_exit_dist teacher_tenure exp_capital_total_per_stu log_enrollment", y_dist)

	gen ylabel = "Equity" if y=="equity_prior"
	replace ylabel = "Teacher Support" if y=="teacher_care"
	replace ylabel = "Fiscal Conservatism" if y=="budget_hawk"
	replace ylabel = "Safety" if y=="safety_health_prior"
	replace ylabel = "Dropout" if y=="dropout_prior"
	replace ylabel = "CTE" if y=="cte_prior"
	replace ylabel = "Facility Improvement" if y=="facility_prior"
	
	sort x y_dist N_eff
	bysort x y_dist: g N_eff_small = N_eff[1]
	drop if N_eff_small<30
	
	g prio = cond(x==y, 1,0)
	egen group = group(prio comp)

	replace cl_l= -cl_l if strpos("teacher_exit_dist log_exp_total_per_stu ", y_dist) 
	replace cl_h = -cl_h if strpos("teacher_exit_dist log_exp_total_per_stu ", y_dist) 
	replace coef = -coef if strpos("teacher_exit_dist log_exp_total_per_stu", y_dist) 
	
	/*foreach y in coef cl_l cl_h coef_p {
 	replace `y' = -`y' if coef_p<0
 }*/
 	g cl_h2 = cond(cl_h>=cl_l, cl_h, cl_l)
	g cl_l2 = cond(cl_h>=cl_l, cl_l, cl_h)
	sort coef
	
	
	*** 2D Graph
	 foreach y in equity_prior teacher_care budget_hawk safety_health_prior {
	 preserve
	 keep if y=="`y'"
	 
	 levelsof ylabel, local(subttl) 
	 
	 replace coef_p =ymean_p
 
	drop N
	bysort x y comp: g n=_n-1
	bysort x y comp: g N=_N-1
	replace coef_p = cond(N==0, coef_p, coef_p+n/N*0.05)

	local colors "ebblue navy cranberry"
	local line
	local i = 1

	foreach xcond in "x==y"    {
		foreach compval in 1 2 3 {
			su coef [aw=1/stderr] if `xcond' & comp==`compval', meanonly
			local mu = r(mean)
			local col : word `i' of `colors'
			if `mu' < . {
				local line `line' yline(`mu', lcolor(`col'))
			}
			local ++i
		}
	}

	display "`line'"
	
	** only prio
	twoway (scatter coef coef_p if x==y & comp==1, color(ebblue) symbol(T)) ///
		   (pcspike cl_h coef_p cl_l coef_p if x==y & comp==1, lc(ebblue) symbol(i)) /// 
		   (scatter coef coef_p if x==y & comp==2, color(navy) symbol(D)) ///
		   (pcspike cl_h coef_p cl_l coef_p if x==y & comp==2, lc(navy) symbol(i))  ///
		   (scatter coef coef_p if x==y & comp==3, color(cranberry) symbol(S)) ///
		   (pcspike cl_h coef_p cl_l coef_p if x==y & comp==3, lc(cranberry) symbol(i))  ///
	 , `line' ylab(,labsize(medlarge)) xlab(,labsize(medlarge)) ytitle("Effect on Standardized Outcomes",size(med)) ///
	 xtitle("Baseline Ideology Share",size(med)) subtitle(`subttl', size(med) ring(0) pos(12) box)  ///
	 legend(order (5 "Only Member" 1 "Minority" 3 "Majority") pos(6) col(3) size(medlarge)) ///
	 saving("Results/Graph/rd_uni_2d_by_comp_`y'", replace) 
	 
	 ** all 
	*twoway (scatter coef coef_p if x~=y & comp==1, color(dkorange) symbol(O)) (pcspike cl_h2 coef_p cl_l2 coef_p if x~=y & comp==1, lc(dkorange) symbol(i)) /// 
	 (scatter coef coef_p if x~=y & comp==2, color(red) symbol(D)) (pcspike cl_h2 coef_p cl_l2 coef_p if x~=y & comp==2, lc(red) symbol(i)) (scatter coef coef_p if x==y & comp==1, color(ebblue) symbol(T)) (pcspike cl_h2 coef_p cl_l2 coef_p if x==y & comp==1, lc(ebblue) symbol(i)) /// 
	 (scatter coef coef_p if x==y & comp==2, color(navy) symbol(T)) (pcspike cl_h2 coef_p cl_l2 coef_p if x==y & comp==2, lc(navy) symbol(i))  ///
	 , `line' ylab(,labsize(medlarge)) xlab(,labsize(medlarge)) ytitle("Effect on Outcomes",size(med)) xtitle("Baseline Ideology Share",size(med)) subtitle(`subttl', size(med) ring(0) pos(12) box)  ///
	 legend(order (1 "Identities-Minority" 2 "Identities-Majority" 3 "Ideologies-Minority" 4 "Ideologies-Minority") pos(6) col(2) size(small)) ///
	 saving("Results/Graph/rd_uni_2d_by_comp_`y'", replace) 
	 
	 ** only demo
	*twoway (scatter coef coef_p if x~=y & comp==1, color(dkorange) symbol(O)) (pcspike cl_h2 coef_p cl_l2 coef_p if x~=y & comp==1, lc(dkorange) symbol(i)) /// 
	 (scatter coef coef_p if x~=y & comp==2, color(red) symbol(D)) (pcspike cl_h2 coef_p cl_l2 coef_p if x~=y & comp==2, lc(red) symbol(i)) ///
	 , `line' ylab(,labsize(medlarge)) xlab(,labsize(medlarge)) ytitle("Effect on Outcomes",size(med)) xtitle("Baseline Ideology Share",size(med)) subtitle(`subttl', size(med) ring(0) pos(12) box)  ///
	 legend(order (1 "Identities-Minority" 3 "Identities-Majority" ) pos(6) col(2) size(small)) ///
	 saving("Results/Graph/rd_uni_2d_by_comp_`y'", replace)
	 
	 ** only demo + no CI
	*twoway (scatter coef coef_p if x~=y & comp==1, color(dkorange) symbol(O)) /// 
	 (scatter coef coef_p if x~=y & comp==2, color(red) symbol(D)) (fpfit coef coef_p[aweight = 1/(stderr^2)] if x~=y & comp~=0) ///
	 , `line' ylab(,labsize(medlarge)) xlab(,labsize(medlarge)) ytitle("Effect on Outcomes",size(med)) xtitle("Baseline Ideology Share",size(med)) subtitle(`subttl', size(med) ring(0) pos(12) box)  ///
	 legend(order (1 "Identities-Minority" 2 "Identities-Majority" ) pos(6) col(2) size(small)) ///
	 saving("Results/Graph/rd_uni_2d_by_comp_`y'", replace)

	 ** all + no CI
	 *twoway (scatter coef coef_p if x~=y & comp==1, color(dkorange) symbol(O)) /// 
	 (scatter coef coef_p if x~=y & comp==2, color(red) symbol(D)) ///
	 (scatter coef coef_p if x==y & comp==1, color(ebblue) symbol(T))  /// 
	 (scatter coef coef_p if x==y & comp==2, color(navy) symbol(T))  (fpfit coef coef_p[aweight = 1/(stderr^2)] if comp~=0)  ///
	 , ylab(,labsize(medlarge)) xlab(,labsize(medlarge)) ytitle("Effect on District Outcomes",size(med)) xtitle("Baseline Share",size(med)) subtitle(`subttl', size(med) ring(0) pos(12) box) legend(order (1 "Identities-Minority" 2 "Identities-Majority" 3 "Ideologies-Minority" 4 "Ideologies-Minority") pos(6) col(2) size(small)) ///
	 saving("Results/Graph/rd_uni_2d_by_comp_`y'", replace) 
	 *legend(order (2 "Identities" 4 "Ideologies") pos(5) col(2) size(medlarge) ring(0)) 
	 restore
	 }	
	 
	 grc1leg2 "Results/Graph/rd_uni_2d_by_comp_budget_hawk" "Results/Graph/rd_uni_2d_by_comp_equity_prior", ///
	 ytol imargin(zero) cols(2) xsize(5) ysize(2) labsize(med) ring(1) iscale(1.1)
	
	 
	 //"Results/Graph/rd_uni_2d_by_comp_safety_health_prior" "Results/Graph/rd_uni_2d_by_comp_teacher_care"
	 
	gr export "Results/Graph/rd_uni_2d_by_comp_base_prio.png", replace 
	
	
	
	
	
	
	
	
	
	
	
	
	
