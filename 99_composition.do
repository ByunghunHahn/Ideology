********************************************************************************
* Heterogeneity Analysis by Board Composition
* Written by Minseon Park 3/6/2025
********************************************************************************

clear all
set seed 1234

* set directory 
cd "~/Dropbox/California Election Data/Code"
global logpath "~/Dropbox/California Election Data/Logs"
global mindta "~/Dropbox/California Election Data/Minutes/data/dta"
  
global tab_opt = "b(a2) se(2) scalars(cl_l cl_h pval bw N_eff ymean ysd) label star(+ 0.1 * 0.05 ** 0.01)"

********************************************************************************
* I. Generate Function
********************************************************************************

use "data_for_rd/with_missing/dist_hisp.dta" if dist_cnty~=1, clear
rdrobust log_exp_pupils vote_margin , p(1) kernel(triangular) bwselect(mserd) 
regsave using "Results/by_comp", replace
	
	
capture program drop rd_hetero
program define rd_hetero
  syntax, class_type(integer) data(string) y_type(integer) yearcond(string) vcetype(string) [ideo(string)] [mtn(string)] [yvar(string)]  
  
	use "data_for_rd/with_missing/dist_`data'.dta" if dist_cnty~=1, clear
	ren id_district_nces leaid
	tostring leaid, replace  // Convert to string if not already

	*** 1. merge with minute data
	// Ensure consistent widT by adding leading zeros
	gen leaid_padded = string(real(leaid), "%07.0f")  // Adjust "8" to Te required widT
	replace leaid = leaid_padded
	drop leaid_padded
	
	merge m:1 leaid year using "$mindta/minutes_final_year_level.dta", keep(master matched) gen(_minute)
	  
	tab year, g(yr_)
	egen id_election = group(leaid year_elected)
	
	*** 2. classification by the baseline keyword level
	if `class_type'==1 {
	bysort year: egen n_pass_`mtn'_p50 = median(n_pass_`mtn')
	g comp_type= cond(n_pass_`mtn'<n_pass_`mtn'_p50,1,2) if year==year_elected-1 & !missing(n_pass_`mtn')
	sort multi_raceid comp_type
	bysort multi_raceid: replace comp_type = comp_type[1]
	
	la de comp_type 1 "Less Tan Median in Te baseline" 2 "More Tan Median in Te baseline" 
	la val comp_type comp_type	
	}
	
	*** 3. merge with composition
	merge m:1 leaid year using "board_composition/board_composition_elections.dta", keep(master matched) gen(_comp)

	g `data'_total = 0 
	g `data'_total2 = 0
	forvalue n=1/14 {
		replace `data'_total = `data'_total+1 if `data'`n'==1
		replace `data'_total2 = `data'_total2+1 if `data'`n'==0
	}
		
	g `data'_share = `data'_total/(`data'_total+`data'_total2)
	
	*** 4. classification by Te baseline composition
	if `class_type'==2 {
	g `data'_total_b = `data'_total if year==year_elected-1 
	sort multi_raceid `data'_total_b
	bysort multi_raceid: replace `data'_total_b = `data'_total_b[1]
	
	g `data'_total2_b = `data'_total2 if year==year_elected-1 
	sort multi_raceid `data'_total2_b
	bysort multi_raceid: replace `data'_total2_b = `data'_total2_b[1]

	g size_estimate_b = size_estimate if year==year_elected-1 
	sort multi_raceid size_estimate_b
	bysort multi_raceid: replace size_estimate_b = size_estimate_b[1]
	
	g comp_type = cond(`data'_total_b==0 | `data'_total_b<`data'_total2_b,1,2) if !missing(size_estimate_b)
	
	la de comp_type 1 "Minority among non-Missing" 2 "Majority among non-Missing" 
	la val comp_type comp_type		
	}

	
	*** 5. generate outcome variables
	if `y_type'==1 { // y is educational outcome
		* Merge teacher salary
		replace state_leaid = subinstr(state_leaid, "CA-","",.)
		ren state_leaid cds
		destring cds, replace	

		global class "entry ba60"
		merge m:1 year cds using "../CDE/Certificated Salaries & Benefits/TSAL Outcome.dta", gen(_tsal) force keep(matched master) 
		merge m:1 year using "cpi.dta", keep(master matched) nogen

		ds *sal*
		local varlist = r(varlist)
		foreach x in `varlist' {
			replace `x'=. if `x'<0
			replace `x' = `x' * cpi
		}

		* outcomes: for each class, 1) avg salary fixing composition 2) avg salary fixing salary
		* note: ideally, we would do this across all classes and steps. but districts often change classes (~ 7% in year x dist obs), and this makes it hard. e.g. if BA + 75 or more is splitted into BA + 75 or more and BA + 90 or more, we don't know what step BA + 90 teachers fell in withint the prior system
		  
		* fill out the baseline value + generate FTE  
		foreach x in $class {
		gen `x'_totalBfte = 0
		gen `x'_totalfte =0 		
		
		forvalues i=1/40 {
		g `x'_Bfte`i' =`x'_fte`i' if year==year_elected-1
		sort multi_raceid `x'_Bfte`i'
		bysort multi_raceid: replace `x'_Bfte`i' = `x'_Bfte`i'[1] if missing(`x'_Bfte`i') 

		g `x'_Bsalary`i' =`x'_salary`i' if year==year_elected-1
		sort multi_raceid `x'_Bsalary`i'
		bysort multi_raceid: replace `x'_Bsalary`i' = `x'_Bsalary`i'[1] if missing(`x'_Bsalary`i') 
		
		replace `x'_totalBfte = `x'_totalBfte + `x'_Bfte`i' if !missing(`x'_Bfte`i')
		replace `x'_totalfte = `x'_totalfte + `x'_fte`i' if !missing(`x'_fte`i')
		}
		}

		* generate weighted mean of pctchg & salary : `x'_pctwght `x'_sal_Bcomp
		foreach x in $class {
		  gen `x'_sal_Bcomp = 0
		  gen `x'_sal_Bsal=0
		  
		  forvalues i = 1/40 {
			replace `x'_sal_Bcomp = `x'_sal_Bcomp + (`x'_salary`i' * `x'_Bfte`i')/`x'_totalBfte if !missing(`x'_Bfte`i') & !missing(`x'_salary`i')
			replace `x'_sal_Bsal = `x'_sal_Bsal + (`x'_Bsalary`i' * `x'_fte`i')/`x'_totalfte if !missing(`x'_fte`i') & !missing(`x'_Bsalary`i')
		  }
		  
		replace `x'_sal_Bcomp=. if `x'_sal_Bcomp==0
		replace `x'_sal_Bsal=. if `x'_sal_Bsal==0
		
		gen `x'_sal_Bcomp_log = log(`x'_sal_Bcomp)
		gen `x'_sal_Bsal_log = log(`x'_sal_Bsal)
		
		replace `x'_sal_Bcomp=`x'_sal_Bcomp/1000
		replace `x'_sal_Bsal=`x'_sal_Bsal/1000
		}
		
		foreach y in log_exp_total_per_stu ecd_test teacher_exit_dist entry_sal_Bcomp_log hsp_test {  
		cap g `y'_b = `y' if year==year_elected-1 
		sort multi_raceid `y'_b
		bysort multi_raceid: replace `y'_b = `y'_b[1]
		cap g `y'_D = `y'-`y'_b
	  }
		foreach y in sus_share {	// biannual toucome
			cap g `y'_b1 = `y' if year==year_elected-1 
			cap g `y'_b2 = `y' if year==year_elected-2 

			g `y'_b=.
			sort multi_raceid `y'_b1
			bysort multi_raceid: replace `y'_b = `y'_b1[1]

			sort multi_raceid `y'_b2
			bysort multi_raceid: replace `y'_b = `y'_b2[1] if missing(`y'_b)
			
			cap g `y'_D =`y'-`y'_b
		}	
		
		g y_var = `yvar'_D
		g y_var2=`yvar'
	}
	
	if `y_type'==2 { // outcome is action (motion)

		g y_var = n_pass_`mtn'
		g y_var2 = n_pass_`mtn'
	}
	
	if `y_type'==3 { // outcome is view point
		g `ideo'_total = 0 
		g `ideo'_total2 = 0
		forvalue n=1/14 {
			replace `ideo'_total = `ideo'_total+1 if `ideo'`n'==1 & valid_platform`n'==1
			replace `ideo'_total2 = `ideo'_total2+1 if `ideo'`n'==0  & valid_platform`n'==1
		}
		g `ideo'_share = `ideo'_total/(`ideo'_total+`ideo'_total2)
		
		g y_var = `ideo'_share
		g y_var2 = `ideo'_share
	}
	
  
	*** 6. run RD
	eststo clear
	eststo: qui rdrobust y_var vote_margin if `yearcond', p(1) kernel(triangular) bwselect(mserd) covs(yr_*) `vcetype'
	estadd scalar cl_l= e(ci_l_rb)
	estadd scalar cl_h= e(ci_r_rb)
	estadd scalar pval = e(pv_rb)
	estadd scalar bw = e(h_l)
	local bw= e(h_l)
	estadd scalar N_eff = e(N_h_l) + e(N_h_r)	
	su y_var2 if abs(vote_margin)<=`bw' & year==year_elected-1
	estadd scalar ymean = r(mean) 
	estadd scalar ysd = r(sd)
	local cl_l= e(ci_l_rb)
	local cl_h= e(ci_r_rb)
	local pval = e(pv_rb)
	local bw = e(h_l)
	local N_eff = e(N_h_l) + e(N_h_r)	
	regsave using "Results/by_comp", append addlabel(y,"`yvar'", x,"`data'", cl_l, `cl_l', cl_h, `cl_h', pval, `pval', bw, `bw', N_eff, `N_eff', hetero, 1) 	
	
	eststo: qui rdrobust y_var vote_margin if `yearcond' & comp_type==1, p(1) kernel(triangular) bwselect(mserd) covs(yr_*) `vcetype'
	estadd scalar cl_l= e(ci_l_rb)
	estadd scalar cl_h= e(ci_r_rb)
	estadd scalar pval = e(pv_rb)
	estadd scalar bw = e(h_l)
	local bw= e(h_l)
	estadd scalar N_eff = e(N_h_l) + e(N_h_r)
	su y_var2 if abs(vote_margin)<=`bw' & year==year_elected-1 & comp_type==1
	estadd scalar ymean = r(mean) 
	estadd scalar ysd = r(sd)	
	local cl_l= e(ci_l_rb)
	local cl_h= e(ci_r_rb)
	local pval = e(pv_rb)
	local bw = e(h_l)
	local N_eff = e(N_h_l) + e(N_h_r)	
	regsave using "Results/by_comp", append addlabel(y,"`yvar'", x,"`data'", cl_l, `cl_l', cl_h, `cl_h', pval, `pval', bw, `bw', N_eff, `N_eff', hetero, 2) 	 	
	
	eststo: qui rdrobust y_var vote_margin if `yearcond' & comp_type==2, p(1) kernel(triangular) bwselect(mserd) covs(yr_*) `vcetype'
	estadd scalar cl_l= e(ci_l_rb)
	estadd scalar cl_h= e(ci_r_rb)
	estadd scalar pval = e(pv_rb)
	estadd scalar bw = e(h_l)
	local bw= e(h_l)
	estadd scalar N_eff = e(N_h_l) + e(N_h_r)	
	su y_var2 if abs(vote_margin)<=`bw' & year==year_elected-1 & comp_type==2
	estadd scalar ymean = r(mean) 
	estadd scalar ysd = r(sd)	
	esttab using Results/results_temp.csv, $tab_opt append 
	local cl_l= e(ci_l_rb)
	local cl_h= e(ci_r_rb)
	local pval = e(pv_rb)
	local bw = e(h_l)
	local N_eff = e(N_h_l) + e(N_h_r)	
	regsave using "Results/by_comp", append addlabel(y,"`yvar'", x,"`data'", cl_l, `cl_l', cl_h, `cl_h', pval, `pval', bw, `bw', N_eff, `N_eff', hetero, 3) 	 	
		
end

	
********************************************************************************
* II. Run
********************************************************************************

*** class_type: 1;by keyword 2;by majority

* keyword - motion
rd_hetero, class_type(1) data("budget_hawk") y_type(2) yearcond("year>=year_elected+1") mtn("budget_hawk") vcetype("vce(nncluster id_election)")
rd_hetero, class_type(1) data("equity_prior") y_type(2) yearcond("year>=year_elected+1") mtn("equity_prior") vcetype("vce(nncluster id_election)")
rd_hetero, class_type(1) data("safety_health_prior") y_type(2) yearcond("year>=year_elected+1") mtn("safety_health_prior") vcetype("vce(nncluster id_election)")
rd_hetero, class_type(1) data("teacher_care") y_type(2) yearcond("year>=year_elected+1") mtn("teacher_care") vcetype("vce(nncluster id_election)")
rd_hetero, class_type(1) data("agenda_bias") y_type(2) yearcond("year>=year_elected+1") mtn("agenda_bias") vcetype("vce(nncluster id_election)")
rd_hetero, class_type(1) data("parent_involvement_prior") y_type(2) yearcond("year>=year_elected+1") mtn("engagement") vcetype("vce(nncluster id_election)")

* majority - outcome
rd_hetero, class_type(2) data("budget_hawk") y_type(1) yearcond("year>=year_elected & year<=2017") yvar("log_exp_total_per_stu") vcetype("vce(nncluster id_election)")
rd_hetero, class_type(2) data("equity_prior") y_type(1) yearcond("year>=year_elected") yvar("ecd_test") vcetype("vce(nncluster id_election)")
rd_hetero, class_type(2) data("safety_health_prior") y_type(1) yearcond("year>=year_elected") yvar("sus_share") vcetype("vce(nncluster id_election)")
rd_hetero, class_type(2) data("teacher_care") y_type(1) yearcond("year>=year_elected") yvar("teacher_exit_dist") vcetype("vce(nncluster id_election)")
foreach y in hsp_test {
rd_hetero, class_type(2) data("female") y_type(1) yearcond("year>=year_elected") yvar("`y'") vcetype("vce(nncluster id_election)")
rd_hetero, class_type(2) data("hisp") y_type(1) yearcond("year>=year_elected") yvar("`y'") vcetype("vce(nncluster id_election)")
}
rd_hetero, class_type(2) data("female") y_type(1) yearcond("year>=year_elected & year<=2017") yvar("log_exp_total_per_stu") vcetype("vce(nncluster id_election)")
rd_hetero, class_type(2) data("hisp") y_type(1) yearcond("year>=year_elected & year<=2017") yvar("log_exp_total_per_stu") vcetype("vce(nncluster id_election)")

* majority - view point
foreach y in budget_hawk equity_prior safety_health_prior teacher_care {
rd_hetero, class_type(2) data("female") y_type(3) yearcond("year>=year_elected") ideo("`y'") vcetype("vce(cluster id_election)")
rd_hetero, class_type(2) data("hisp") y_type(3) yearcond("year>=year_elected") ideo("`y'") vcetype("vce(cluster id_election)")
}
	
	
********************************************************************************
* III. Visualization
********************************************************************************
	
	use "Results/by_comp", clear
	
	gen xlabel = "Equity" if x=="equity_prior"
	replace xlabel = "Teacher Support" if x=="teacher_care"
	replace xlabel = "Fiscal Conservatism" if x=="budget_hawk"
	replace xlabel = "Safety" if x=="safety_health_prior"
	replace xlabel = "Dropout" if x=="dropout_prior"
	replace xlabel = "Hispanic" if x=="hisp"
	replace xlabel = "Female" if x=="female"
	
	g ylabel = "-log(Spending Per Pupil)" if y=="log_exp_total_per_stu"
	replace ylabel ="ECD Test" if y=="ecd_test"
	replace ylabel ="Share Exiting Teachers" if y=="teacher_exit_dist"
	replace ylabel ="-Share Suspended" if y=="sus_share"
	
	replace coef=-coef if strpos("log_exp_total_per_stu sus_share", y)
	replace cl_l=-cl_l if strpos("log_exp_total_per_stu sus_share", y)
	replace cl_h=-cl_h if strpos("log_exp_total_per_stu sus_share", y)
	
	drop N
	bysort x y : g n=_n-1
	bysort x y : g N=_N-1
	replace coef_p = cond(N==0, coef_p, coef_p+n/N*0.004)

	foreach x in budget_hawk equity_prior safety_health_prio teacher_care {
	preserve
	keep if x=="`x'"
	levelsof xlabel, local(subttl)
	levelsof ylabel, local(subttl2)
	
	twoway (rcap cl_l cl_h hetero if hetero==1, lc(ebblue%50)) (scatter coef hetero if  hetero==1, color(ebblue%50)) (rcap cl_l cl_h hetero if hetero==2, lc(ebblue)) (scatter coef hetero if hetero==2, color(ebblue) symbol(D)) (rcap cl_l cl_h hetero if hetero==3, lc(ebblue%80)) (scatter coef hetero if hetero==3, color(ebblue%80) symbol(T)), xtitle(`subttl', size(medlarge)) ytitle(`subttl2', size(medlarge)) xscale(range(0.8 3.2)) xlabel(1 "Overall" 2 "Minority" 3 "Majority", labsize(med) angle(45)) legend(off) saving(Results/Graph/by_comp_`x', replace)	
	restore
	}
	
	foreach x in female hisp {
	preserve
	keep if x=="`x'"
	levelsof xlabel, local(subttl)
	levelsof ylabel, local(subttl2)	
	
	twoway (rcap cl_l cl_h hetero if hetero==1, lc(dkorange%50)) (scatter coef hetero if hetero==1, color(dkorange%50)) (rcap cl_l cl_h hetero if hetero==2, lc(dkorange)) (scatter coef hetero if hetero==2, color(dkorange) symbol(D)) (rcap cl_l cl_h hetero if hetero==3, lc(dkorange%80)) (scatter coef hetero if hetero==3, color(dkorange%80) symbol(T)), xtitle(`subttl', size(medlarge)) ytitle(`subttl2', size(medlarge)) xscale(range(0.8 3.2)) xlabel(1 "Overall" 2 "Minority" 3 "Majority", labsize(med) angle(45)) legend(off) saving(Results/Graph/by_comp_`x', replace)	
	restore
	}	
	
foreach x in female hisp {
	preserve
	keep if x=="`x'"
	levelsof xlabel, local(subttl)
	levelsof ylabel, local(subttl2)	
	
	twoway (rcap cl_l cl_h hetero if hetero==1, lc(dkorange%50)) (scatter coef hetero if hetero==1, color(dkorange%50)) (rcap cl_l cl_h hetero if hetero==2, lc(dkorange)) (scatter coef hetero if hetero==2, color(dkorange) symbol(D)) (rcap cl_l cl_h hetero if hetero==3, lc(dkorange%80)) (scatter coef hetero if hetero==3, color(dkorange%80) symbol(T)), xtitle(`subttl', size(medlarge)) ytitle(`subttl2', size(medlarge)) xscale(range(0.8 3.2)) xlabel(1 "Overall" 2 "Minority" 3 "Majority", labsize(med) angle(45)) legend(off) saving(Results/Graph/by_comp_`x', replace)	
	restore
	}	

foreach x in female {
	preserve
	keep if x=="`x'"
	levelsof xlabel, local(subttl)
	levelsof ylabel, local(subttl2)	
	
	twoway (rcap cl_l cl_h hetero if hetero==1, lc(dkorange%50)) (scatter coef hetero if hetero==1, color(dkorange%50)) (rcap cl_l cl_h hetero if hetero==2, lc(dkorange)) (scatter coef hetero if hetero==2, color(dkorange) symbol(D)) (rcap cl_l cl_h hetero if hetero==3, lc(dkorange%80)) (scatter coef hetero if hetero==3, color(dkorange%80) symbol(T)), xtitle(`subttl', size(medlarge)) ytitle(`subttl2', size(medlarge)) xscale(range(0.8 3.2)) xlabel(1 "Overall" 2 "Minority" 3 "Majority", labsize(med) angle(45)) ylabel(0(0.05)0.2) legend(off) saving(Results/Graph/by_comp_`x', replace)	
	restore
	}	
	
	gr combine "Results/Graph/by_comp_budget_hawk" "Results/Graph/by_comp_equity_prior" "Results/Graph/by_comp_female" "Results/Graph/by_comp_hisp", col(4) imargin(zero)
gr export "Results/Graph/rd_hetero.png", replace
	