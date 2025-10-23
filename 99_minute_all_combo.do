********************************************************************************
* Written by Minseon Park 3/6/2025
********************************************************************************

clear all
set seed 1234

* set directory 
cd "~/Dropbox/California Election Data/Code"
global logpath "~/Dropbox/California Election Data/Logs"
global mindta "~/Dropbox/California Election Data/Minutes/data/dta"
  
global tab_opt = "b(a2) se(2) scalars(cl_l cl_h pval bw N_eff ymean) label star(+ 0.1 * 0.05 ** 0.01)"

global list = "female hisp occ_teacher democrat_v2 budget_hawk equity_prior safety_health_prior teacher_care agenda_bias parent_involvement_prior"	

global missing = "with_missing" // "with_missing" or "no_missing"
********************************************************************************
* I. Save all RD results
********************************************************************************

use "$mindta/minutes_final_year_level.dta", clear
foreach x in budget_hawk equity_prior safety_health_prior teacher_care agenda_bias engagement academic_prior {	
	egen z=std(n_pass_`x')
	replace n_pass_`x'=z
	drop z
}
save "Temp/minutes_z.dta", replace

use "data_for_rd/$missing/dist_hisp.dta" if dist_cnty~=1, clear
rdrobust log_exp_pupils vote_margin if $year_cond0, p(1) kernel(triangular) bwselect(mserd) 
regsave using "Results/minute_uni", replace
	
foreach xvar in $list {
	use "data_for_rd/$missing/dist_`xvar'.dta" if dist_cnty~=1, clear
	ren id_district_nces leaid
	tostring leaid, replace  // Convert to string if not already

	// Ensure consistent width by adding leading zeros
	gen leaid_padded = string(real(leaid), "%07.0f")  // Adjust "8" to the required width
	replace leaid = leaid_padded
	drop leaid_padded

	merge m:1 leaid year using "Temp/minutes_z.dta", keep(master matched) // generated from Minutes/code/merge_minutes_elections.do (last section)
	  
	tab year, g(yr_)
	egen id_election = group(leaid year_elected)

	foreach x in budget_hawk equity_prior safety_health_prior teacher_care agenda_bias engagement academic_prior {	
	rdrobust n_pass_`x' vote_margin if year>=year_elected+1, p(1) kernel(triangular) bwselect(mserd) covs(yr_*) vce(nncluster id_election)
	local cl_l= e(ci_l_rb)
	local cl_h= e(ci_r_rb)
	local pval = e(pv_rb)
	local bw = e(h_l)
	local N_eff = e(N_h_l) + e(N_h_r)	
	regsave using "Results/minute_uni", append addlabel(y,"`x'", x,"`xvar'", cl_l, `cl_l', cl_h, `cl_h', pval, `pval', bw, `bw', N_eff, `N_eff') 	
	}
	}

	
********************************************************************************
* II. 2D Graph - change in outcomes vs change in motions
********************************************************************************

	use "Results/minute_uni.dta", clear
	keep if strpos("female hisp occ_teacher democrat_v2", x) | x==y | strpos(y,"engagement") & strpos(x,"parent")

	expand 3 if y=="equity_prior"
	expand 4 if y=="teacher_care"
	bysort y x: g n=_n
	
	g y_dist ="log_exp_total_per_stu" if y=="budget_hawk"
	replace y_dist ="sus_share" if y=="safety_health_prior"
	replace y_dist = "ecd_test" if y=="equity_prior" & n==1
	replace y_dist = "hsp_test" if y=="equity_prior" & n==2
	replace y_dist = "entry_sal_Bcomp_log" if y=="teacher_care" & n==1
	replace y_dist = "ba60_sal_Bcomp_log" if y=="teacher_care" & n==2
	replace y_dist = "teacher_exit_dist" if y=="teacher_care" & n==3
	replace y_dist = "teacher_tenure" if y=="teacher_care" & n==4
 	
	merge m:1 x y using "Temp/first_stage_pred", keepus(*_p) keep(matched)
	
	g prio=1 if strpos("female hisp occ_teacher democrat_v2", x)==0
	
	gen ylabel = "Equity" if y=="equity_prior"
	replace ylabel = "Teacher Support" if y=="teacher_care"
	replace ylabel = "Fiscal Conservatism" if y=="budget_hawk"
	replace ylabel = "Safety" if y=="safety_health_prior"
	replace ylabel = "Dropout" if y=="dropout_prior"
	replace ylabel = "CTE" if y=="cte_prior"
	replace ylabel = "Facility Improvement" if y=="facility_prior"
	
	
	* create hypothesis groups
	g h_type = 0
	
	* existing lit 
	replace h_type = 1 if y=="teacher_care" & x=="occ_teacher"
	replace h_type = 1 if y=="equity_prior" & x=="hisp"
	replace h_type = 1 if y=="budget_hawk" & x=="hisp"
	
	replace h_type = 1 if y_dist=="ecd_test" & x=="hisp"
	replace h_type = 1 if y_dist=="hsp_test" & x=="hisp"
	replace h_type = 1 if y_dist=="log_exp_total_per_stu" & x=="hisp"
	
	replace h_type = 1 if y_dist=="ba60_sal_Bcomp_log" & x=="occ_teacher"
	replace h_type = 1 if y_dist=="entry_sal_Bcomp_log" & x=="occ_teacher"
	replace h_type = 1 if y_dist=="log_wage_inst_per_tch" & x=="occ_teacher"
	replace h_type = 1 if y_dist=="teacher_tenure" & x=="occ_teacher"
	replace h_type = 1 if y_dist=="teacher_exit_dist" & x=="occ_teacher"
	
	* augmented 
	replace h_type = 2 if y_dist=="ecd_test" & x=="occ_teacher" 
	replace h_type = 2 if y_dist=="hsp_test" & x=="occ_teacher" 
	replace h_type = 2 if y_dist=="exp_capital_total_per_stu" & x=="democrat_v2" 
	replace h_type = 2 if y_dist=="sus_share" & x=="democrat_v2" 
	replace h_type = 2 if y=="equity_prior" & x=="democrat_v2"

	
	 foreach y in budget_hawk safety_health_prior equity_prior teacher_care {	
	 	preserve
	keep if y=="`y'"
	
	levelsof ylabel, local(subttl)
	 reg coef [weight=1/(stderr^2)] if prio~=1
	 local mu1 = _b[_cons]
	 su coef if prio==1
	 local mu2 = r(mean)
	 
// 	*levelsof ylabel, local(subttl)	
	twoway  (scatter coef coef_p  if  prio~=1 & h_type==0, color(dkorange) symbol(O)) ///  
			(pcspike cl_l  coef_p cl_h  coef_p if  prio~=1 & h_type==0, lc(dkorange) symbol(i)) ///
			(scatter coef coef_p  if  prio~=1 & h_type==1, color(dkorange*2) symbol(S)) ///
			(pcspike cl_l  coef_p cl_h  coef_p if  prio~=1 & h_type==1, lc(dkorange*2) symbol(i)) ///
			(scatter coef coef_p if  prio==1 & h_type==0, color(ebblue) symbol(T)) ///
			(pcspike cl_l  coef_p cl_h  coef_p if prio==1 & h_type==0, lc(ebblue) symbol(i)) ///
			(scatter coef coef_p if prio==1 & h_type==1, color(ebblue*2) symbol(T)) ///
			(pcspike cl_l  coef_p cl_h  coef_p if prio==1 & h_type==1, lc(ebblue*2) symbol(i)) ///
			(scatter coef coef_p  if prio~=1 & h_type==2, color(green*1.2) symbol(D)) ///  
			(pcspike cl_l  coef_p cl_h  coef_p if prio~=1 & h_type==2, lc(green*1.2) symbol(i)) ///
			(scatter coef coef_p if prio~=1 & h_type==2, color(green) symbol(T)) ///
			(pcspike cl_l  coef_p cl_h  coef_p if prio~=1 & h_type==2, lc(green) symbol(i)) ///
			, yline(`mu1',lcolor(dkorange)) yline(`mu2',lcolor(ebblue)) ylab(,labsize(medlarge)) xlab(,labsize(medlarge)) ///
			ytitle("Change in N Passed Motions",size(med)) xtitle("Predicted Change in Share",size(med)) subtitle(`subttl', size(med) ring(0) pos(12) box) ///
		   legend(order (3 "Identities (existing literature)" 1 "Identities (other)" 9 "Identities (augmented)" 5 "Ideologies") pos(5) col(2) size(med) ring(0)) ///
		   saving("Results/Graph/rd_minute_2d_`y'", replace) 
	 restore
	 }
 
	  grc1leg2 "Results/Graph/rd_minute_2d_budget_hawk" "Results/Graph/rd_minute_2d_equity_prior" "Results/Graph/rd_minute_2d_safety_health_prior" "Results/Graph/rd_minute_2d_teacher_care" , ///
          cols(2) xsize(15) ysize(10) imargin(small) ///
          xtob ytol legendfrom("Results/Graph/rd_minute_2d_budget_hawk") ring(1)

		  
gr export "Results/Graph/rd_minute_2d_miss.png", replace
	
	