********************************************************************************
* Univariate RD with All Outcome Variables 
* Written By Minseon Park 04/06/2025
********************************************************************************
*ssc install regsave 

set seed 1234
* set directory 
cd "~/Dropbox/California Election Data/Code"
global logpath "~/Dropbox/California Election Data/Logs"

global y_rev =""
global y_fiscal= "rev_cte_per_stu log_exp_total_per_stu exp_capital_total_per_stu"
global y_teacher = "teacher_exit_dist entry_sal_Bcomp_log ba60_sal_Bcomp_log"
global y_score = "ecd_test hsp_test"
global y_enroll = "log_enrollment"
global y_behv = "sus_share"	

global list = "hisp female occ_teacher democrat_v2 budget_hawk equity_prior safety_health_prior teacher_care agenda_bias parent_involvement_prior academic_prior facility_prior cte_prior dropout_prior enrollment_prior"
global class "entry ba60"

global missing = "with_missing" // "with_missing" or "no_missing"
global flag_balance=0

********************************************************************************
* I. Run RDs
********************************************************************************

	use "data_for_rd/$missing/dist_hisp.dta", clear
	rdrobust log_exp_total_per_stu vote_margin , p(1) kernel(triangular) bwselect(mserd) 
	regsave using "Results/outcome_uni_norm", replace

	foreach xvar in $list {
		
	use "data_for_rd/$missing/dist_`xvar'.dta", clear
	// Min: we can use standardized outcomes "data_for_rd/$missing/dist_`xvar'_z.dta", but I prefer not to use those because raw y variables make it easier to check if we can replicate previous studies
	g log_enrollment= log(enrollment)
	
	do 4_f_merge_tsal 

	foreach y in entry_sal_Bcomp_log ba60_sal_Bcomp_log {
		egen z=std(`y')
		replace `y'=z
		drop z
	}
		
	do 4_f_prep_rd

	foreach y in $y_rev $y_fiscal {
		rdrobust `y'_D vote_margin if year>=year_elected & year<=2017, p(1) kernel(triangular) bwselect(mserd) covs(yr_*) vce(nncluster id_election)
		local cl_l= e(ci_l_rb)
		local cl_h= e(ci_r_rb)
		local pval = e(pv_rb)
		local bw = e(h_l)
		local N_eff = e(N_h_l) + e(N_h_r)	
		regsave using "Results/outcome_uni_norm", append addlabel(outcome,"`y'", demo,"`xvar'", cl_l, `cl_l', cl_h, `cl_h', pval, `pval', bw, `bw', N_eff, `N_eff') 	
		}	
	foreach y in $y_score $y_teacher $y_enroll $y_behv {
		rdrobust `y'_D vote_margin if year>=year_elected, p(1) kernel(triangular) bwselect(mserd) covs(yr_*) vce(nncluster id_election)
		local cl_l= e(ci_l_rb)
		local cl_h= e(ci_r_rb)
		local pval = e(pv_rb)
		local bw = e(h_l)
		local N_eff = e(N_h_l) + e(N_h_r)	
		regsave using "Results/outcome_uni_norm", append addlabel(outcome,"`y'", demo,"`xvar'", cl_l, `cl_l', cl_h, `cl_h', pval, `pval', bw, `bw', N_eff, `N_eff') 	
		}
	}
	
	
********************************************************************************
* III. Plot 2D Graphs
********************************************************************************
	
	use Results/outcome_uni_norm if outcome~="", clear
	
	*** 1) merge with first stage results 
	preserve 
	use  "Temp/h0_list_demo_outcome.dta", clear // generated in 3_f_compile_h0	
	ren coef coef_p
	save "Temp/first_stage_pred.dta", replace
	restore
		
	merge 1:m outcome demo using "Temp/first_stage_pred", keepus(*_p h_type prio priolabel) keep(matched)

	*** 2) some fine tuning before viz
	foreach y in coef cl_l cl_h coef_p {
 	replace `y' = -`y' if coef_p<0 & h_type~=1 // keep the sign of h0 from previous lit
	}
	replace cl_l= -cl_l if strpos("teacher_exit_dist log_exp_total_per_stu", outcome) 
	replace cl_h = -cl_h if strpos("teacher_exit_dist log_exp_total_per_stu", outcome) 
	replace coef = -coef if strpos("teacher_exit_dist log_exp_total_per_stu", outcome) 
	g cl_h2 = cond(cl_h>=cl_l, cl_h, cl_l)
	g cl_l2 = cond(cl_h>=cl_l, cl_l, cl_h)
	sort coef

	drop N
	bysort demo prio : g n=_n-1
	bysort demo prio : g N=_N-1
	replace coef_p = cond(N==0, coef_p, coef_p+n/N*0.004)

	**** 3) 2D graph
	local symbols "O Th Dh Sh X A V t"
	
	 foreach y in equity_prior teacher_care safety_health_prior budget_hawk {
	 preserve
	 keep if prio=="`y'"
	 
	 levelsof priolabel, local(subttl)
	 
	 count if prio==demo
    if r(N) >= 2 {
	 reg coef [weight=1/stderr] if prio~=demo
	 local mu1 = _b[_cons]
	 reg coef [weight=1/stderr] if prio==demo
	 local mu2 = _b[_cons]
    }
    else {
	 su coef if prio~=demo
	 local mu1 = r(mean)		
	 su coef if prio==demo
	 local mu2 = r(mean)
    }
	 
	 twoway (scatter coef coef_p  if prio~=demo & h_type==0, color(dkorange) symbol(O)) ///  
			(pcspike cl_h2 coef_p cl_l2 coef_p if prio~=demo & h_type==0, lc(dkorange) symbol(i)) ///
			(scatter coef coef_p  if prio~=demo & h_type==1, color(dkorange*2) symbol(S)) ///
			(pcspike cl_h2 coef_p cl_l2 coef_p if prio~=demo & h_type==1, lc(dkorange*2) symbol(i)) ///
			(scatter coef coef_p  if prio~=demo & h_type==2, color(green*1.2) symbol(D)) ///  
			(pcspike cl_h2 coef_p cl_l2 coef_p if prio~=demo & h_type==2, lc(green*1.2) symbol(i)) ///
			(scatter coef coef_p if prio==demo, color(ebblue) symbol(T)) ///
			(pcspike cl_h2 coef_p cl_l2 coef_p if prio==demo, lc(ebblue) symbol(i)) ///
			, yline(`mu1',lcolor(cranberry)) yline(`mu2',lcolor(ebblue)) ylab(,labsize(medlarge)) xlab(,labsize(medlarge)) ///
			ytitle("{&Delta}District Outcomes",size(med)) xtitle("{&Delta}Share of Members with the Viewpoint",size(med)) ///
			subtitle(`subttl', size(med) ring(0) pos(12) box) ///
			legend(order (1 "Identities (other)" 3 "Identities (existing literature)" 5 "Identities (augmented)" 7 "Ideologies") pos(5) col(4) size(med) ring(0)) ///
			saving("Results/Graph/rd_uni_2d_`y'", replace) 
	 restore
	 }
		  
  grc1leg2 "Results/Graph/rd_uni_2d_budget_hawk" "Results/Graph/rd_uni_2d_equity_prior", ///
          cols(2) xsize(5) ysize(2) imargin(med) ///
          xtob ytol legendfrom("Results/Graph/rd_uni_2d_budget_hawk") labsize(med) ring(1) iscale(1.1) xcommon 
  gr export "Results/Graph/rd_uni_2d_panel_valid.png", replace
  
   grc1leg2 "Results/Graph/rd_uni_2d_budget_hawk" "Results/Graph/rd_uni_2d_equity_prior" "Results/Graph/rd_uni_2d_safety_health_prior" "Results/Graph/rd_uni_2d_teacher_care", ///
          cols(2) xsize(5) ysize(2) imargin(med) ///
          xtob ytol legendfrom("Results/Graph/rd_uni_2d_budget_hawk") labsize(med) ring(1) iscale(1.1) xcommon 
  
  
  /* CASESTUDY Labels - BUDGET
  preserve
	 keep if prio=="budget_hawk"
	 
	 levelsof priolabel, local(subttl)
	 
	 count if prio==demo
    if r(N) >= 2 {
	 reg coef [weight=1/stderr] if prio~=demo
	 local mu1 = _b[_cons]
	 reg coef [weight=1/stderr] if prio==demo
	 local mu2 = _b[_cons]
    }
    else {
	 su coef if prio~=demo
	 local mu1 = r(mean)		
	 su coef if prio==demo
	 local mu2 = r(mean)
    }
	
	tab outcome demo
	
	* case study labels 
	gen casestudy = ""
	replace casestudy = "Hispanic -> Spending" if demo=="hisp"
	replace casestudy = "Fiscal Conservative -> Spending" if demo=="budget_hawk"
	
	 twoway (scatter coef coef_p  if casestudy=="Hispanic -> Spending", color(dkorange) symbol(O) mlabel(casestudy) mlabposition(3) mlabgap(5) mlabcolor(black) mlabsize(medsmall)) ///  
			(pcspike cl_h2 coef_p cl_l2 coef_p if casestudy=="Hispanic -> Spending", lc(dkorange) symbol(i)) ///
			(scatter coef coef_p if casestudy=="Fiscal Conservative -> Spending", color(ebblue) symbol(T) mlabel(casestudy) mlabposition(9) mlabgap(5) mlabcolor(black) mlabsize(medsmall)) ///
			(pcspike cl_h2 coef_p cl_l2 coef_p if casestudy=="Fiscal Conservative -> Spending", lc(ebblue) symbol(i)) ///
			, ylab(-.05(0.05)0.2,labsize(medlarge)) xlab(0(0.05)0.25,labsize(medlarge)) ///
			ytitle("Change in District Outcomes",size(med)) xtitle("Change in Share with the Viewpoints",size(med)) ///
			subtitle(`subttl', size(med) ring(0) pos(12) box) ///
			legend(order (1 "Identities" 3 "Ideologies") pos(5) col(2) size(med) ring(0)) ///
			saving("Results/Graph/rd_uni_2d_casestudy_budget_hawk", replace) 
	 restore
  
  * CASESTUDY Labels - EQUITY
  preserve
	 keep if prio=="equity_prior"
	 
	 levelsof priolabel, local(subttl)
	 
	 count if prio==demo
    if r(N) >= 2 {
	 reg coef [weight=1/stderr] if prio~=demo
	 local mu1 = _b[_cons]
	 reg coef [weight=1/stderr] if prio==demo
	 local mu2 = _b[_cons]
    }
    else {
	 su coef if prio~=demo
	 local mu1 = r(mean)		
	 su coef if prio==demo
	 local mu2 = r(mean)
    }
	
	tab outcome demo
	
	* case study labels 
	gen casestudy = ""
	replace casestudy = "Hispanic -> Hispanic test scores" if demo=="hisp" & outcome=="hsp_test"
	replace casestudy = "Equity-focused -> ECD test scores" if demo=="equity_prior" & outcome=="ecd_test"
	
	 twoway (scatter coef coef_p  if casestudy=="Hispanic -> Hispanic test scores", color(dkorange) symbol(O) mlabel(casestudy) mlabposition(3) mlabgap(5) mlabcolor(black) mlabsize(medsmall)) ///  
			(pcspike cl_h2 coef_p cl_l2 coef_p if casestudy=="Hispanic -> Hispanic test scores", lc(dkorange) symbol(i)) ///
			(scatter coef coef_p if casestudy=="Equity-focused -> ECD test scores", color(ebblue) symbol(T) mlabel(casestudy) mlabposition(9) mlabgap(5) mlabcolor(black) mlabsize(medsmall)) ///
			(pcspike cl_h2 coef_p cl_l2 coef_p if casestudy=="Equity-focused -> ECD test scores", lc(ebblue) symbol(i)) ///
			, ylab(,labsize(medlarge)) xlab(0(0.05)0.25,labsize(medlarge)) ///
			ytitle("Change in District Outcomes",size(med)) xtitle("Change in Share with the Viewpoints",size(med)) ///
			subtitle(`subttl', size(med) ring(0) pos(12) box) ///
			legend(order (1 "Identities" 3 "Ideologies") pos(5) col(2) size(med) ring(0)) ///
			saving("Results/Graph/rd_uni_2d_casestudy_equity_prior", replace) 
	 restore
	 
	  grc1leg2 "Results/Graph/rd_uni_2d_casestudy_budget_hawk" "Results/Graph/rd_uni_2d_casestudy_equity_prior", ///
          cols(2) xsize(5) ysize(2) imargin(med) ///
          xtob ytol legendfrom("Results/Graph/rd_uni_2d_casestudy_budget_hawk") labsize(med) ring(1) iscale(1.1)
	gr export "Results/Graph/rd_uni_2d_casestudy_panel_valid.png", replace
  
  
  
