********************************************************************************
* Heterogeneity Analysis by Board Composition
* Written by Minseon Park 04/20/25
********************************************************************************

clear all
set seed 1234

* set directory 
cd "~/Dropbox/California Election Data/Code"
global logpath "~/Dropbox/California Election Data/Logs"
global mindta "~/Dropbox/California Election Data/Minutes/data/dta"
  
global list_demo = "hisp female occ_teacher democrat_v2 "
global list_prio ="budget_hawk equity_prior safety_health_prior teacher_care"


********************************************************************************
* I. Run RD 
********************************************************************************

use "data_for_rd/with_missing/dist_hisp.dta" if dist_cnty~=1, clear
rdrobust log_exp_pupils vote_margin if year==year_elected-1 , p(1) kernel(triangular) bwselect(mserd) 
regsave using "Results/first_stage_by_comp", replace

foreach xvar in $list_demo $list_prio {
	
	use "data_for_rd/with_missing/dist_`xvar'.dta" if dist_cnty~=1, clear

	ren id_district_nces leaid
	tostring leaid, replace  // Convert to string if not already

	*** 1) merge with composition
	// Ensure consistent width by adding leading zeros
	gen leaid_padded = string(real(leaid), "%07.0f")  // Adjust "8" to the required width
	replace leaid = leaid_padded
	drop leaid_padded
	  
	tab year, g(yr_)
	egen id_election = group(leaid year_elected)

	merge m:1 leaid year using "board_composition/board_composition_elections.dta", keep(master matched) gen(_comp)
	
	g size_estimate_b = size_estimate if year==year_elected-1 
	sort multi_raceid size_estimate_b
	bysort multi_raceid: replace size_estimate_b = size_estimate_b[1]
	
	foreach x in $list_prio $list_demo {
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
		
		
	*** 2) classification by baseline composition
	g `xvar'_total_b = `xvar'_total if year==year_elected-1 
	sort multi_raceid `xvar'_total_b
	bysort multi_raceid: replace `xvar'_total_b = `xvar'_total_b[1]
	
	g `xvar'_total2_b = `xvar'_total2 if year==year_elected-1 
	sort multi_raceid `xvar'_total2_b
	bysort multi_raceid: replace `xvar'_total2_b = `xvar'_total2_b[1]
	

	g comp_type = cond(`xvar'_total_b==0, 3, cond(`xvar'_total_b<`xvar'_total2_b, 1, 2)) if !missing(size_estimate_b)
	
	la de comp_type 1 "Minority among non-Missing" 2 "Majority among non-Missing" 3 "First among non-Missing"
	la val comp_type comp_type			
	
	
	*** 3) run RD using observed share
	foreach x in $list_prio {
	rdrobust `x'_share vote_margin if year>=year_elected+1, p(1) kernel(triangular) bwselect(mserd) covs(yr_*) vce(hc0)
	local cl_l= e(ci_l_rb)
	local cl_h= e(ci_r_rb)
	local pval = e(pv_rb)
	local bw = e(h_l)
	local N_eff = e(N_h_l) + e(N_h_r)	
	regsave using "Results/first_stage_by_comp", append addlabel(y,"`x'", x,"`xvar'", cl_l, `cl_l', cl_h, `cl_h', pval, `pval', bw, `bw', N_eff, `N_eff', comp, 0) 
	forvalues t=1/3 {
		rdrobust `x'_share vote_margin if year>=year_elected+1 & comp_type==`t', p(1) kernel(triangular) bwselect(mserd) covs(yr_*) vce(hc0)
		local cl_l= e(ci_l_rb)
		local cl_h= e(ci_r_rb)
		local pval = e(pv_rb)
		local bw = e(h_l)
		local N_eff = e(N_h_l) + e(N_h_r)	
		su `x'_share if year==year_elected-1 & comp_type==`t'
		local ymean = r(mean)
		regsave using "Results/first_stage_by_comp", append addlabel(y,"`x'", x,"`xvar'", cl_l, `cl_l', cl_h, `cl_h', pval, `pval', bw, `bw', N_eff, `N_eff', ymean, `ymean', comp, `t') 	
	}
	}
	

	*** 4) construct predicted share using raw difference and run RD
	
	if strpos("$list_prio", "`xvar'")==0 {
	g n=.
	forvalues n=1/14 {
		replace n=`n' if winner_multi_candid==multi_candid`n' & multi_candid`n'!=.
	}
	
	foreach x in $list_prio {	
		forvalues n=1/14 { 	// first detect "the" candidate and get rid of their characteristics	
		replace `x'`n'=. if n==`n'
	}		

	* then calculate N of members with the viewpoint excluding that member
	replace `x'_total = 0
	replace `x'_total2 = 0
		forvalue n=1/14 {
			replace `x'_total = `x'_total+1 if `x'`n'==1
			replace `x'_total2 = `x'_total2+1 if `x'`n'==0 // XX should we exclude members with invalid platform here?
		}	
	
	* construct predicted share using raw difference 
	preserve
	use Results/bar, clear
	su mean1 if x=="`xvar'" & y=="`x'"
	local mu1 = r(mean)
	su mean2 if x=="`xvar'" & y=="`x'"
	local mu2 = r(mean)
	restore
	
	g `x'_share_p = (`x'_total+ `mu2') /(`x'_total + `x'_total2 + 1) if vote_margin>=0
	replace `x'_share_p = (`x'_total+ `mu1') /(`x'_total + `x'_total2 + 1) if vote_margin<0
	
	
	*** 5) run RD
	rdrobust `x'_share_p vote_margin if year>=year_elected+1, p(1) kernel(triangular) bwselect(mserd) covs(yr_*) vce(hc0)
	local cl_l= e(ci_l_rb)
	local cl_h= e(ci_r_rb)
	local pval = e(pv_rb)
	local bw = e(h_l)
	local N_eff = e(N_h_l) + e(N_h_r)	
	regsave using "Results/first_stage_by_comp", append addlabel(y,"`x'", x,"`xvar'", cl_l, `cl_l', cl_h, `cl_h', pval, `pval', bw, `bw', N_eff, `N_eff', comp,0, predicted,"yes") 
	forvalues t=1/3 {
		rdrobust `x'_share_p vote_margin if year>=year_elected+1 & comp_type==`t', p(1) kernel(triangular) bwselect(mserd) covs(yr_*) vce(hc0)
		local cl_l= e(ci_l_rb)
		local cl_h= e(ci_r_rb)
		local pval = e(pv_rb)
		local bw = e(h_l)
		local N_eff = e(N_h_l) + e(N_h_r)	
		*local ymean =  e(beta_p_l)[1,1]
		su `x'_share_p if year==year_elected-1 & comp_type==`t'
		local ymean = r(mean)
		regsave using "Results/first_stage_by_comp", append addlabel(y,"`x'", x,"`xvar'", cl_l, `cl_l', cl_h, `cl_h', pval, `pval', bw, `bw', N_eff, `N_eff',ymean, `ymean', comp, `t', predicted,"yes") 	
		}
	}
	}
}

STOP 
********************************************************************************
* II. Data Viz on 2D 
********************************************************************************

 use "Results/first_stage_by_comp", clear

 drop if y==""
 keep if strpos("female hisp occ_teacher democrat_v2", x) | x==y
 
 egen group = group(y x comp)
 gsort group -predicted
 bysort group: g coef_p = coef[1]
 drop if predicted=="yes"
 
 foreach y in coef cl_l cl_h coef_p {
 	replace `y' = -`y' if coef_p<0
 }
 
	
	sort x y N_eff
	bysort x y: g N_eff_small = N_eff[1]
	drop if N_eff_small<30
	 
	 replace ymean =1 if ymean==.
	foreach y in coef coef_p cl_l cl_h {
  	replace `y'=`y'/ymean
  }
  
	g prio = cond(x==y, 1,0)
	egen group_c = group(prio comp)

	preserve	
	gen mean = .
	gen mean2 = .
	
	forvalues g=1/6 {
		reg coef [weight=1/(stderr^2)] if group_c==`g'
		replace mean = _b[_cons] if group_c==`g'
		reg coef if group_c==`g'
		replace mean2 = _b[_cons] if group_c==`g'
	}
	
	collapse mean mean2, by(group_c )
	ren mean mean1
	reshape long mean, i(group_c) j(weighted)
	reshape wide mean, i(weighted) j(group_c)
	
	label define weightedlbl 1 "Weighted" 2 "Unweighted"
label values weighted weightedlbl

	graph bar mean2 mean3 mean5 mean6,  over(weighted, label(labsize(medium))) ///
    bar(1, color(dkorange)) bar(2, color(red)) bar(3, color(ebblue)) bar(4, color(navy)) ///
    ytitle("Mean of Coef", size(medium)) ///
    blabel(bar, format(%4.2f)) ///
    ylabel(, labsize(medium)) ///
    legend(order (1 "Identities-Minority" 2 "Identities-Majority" 3 "Ideologies-Minority" 4 "Ideologies-Majority") pos(6) col(2) size(medium))
	gr export "Results/Graph/first_stage_by_comp_bar_rel.png", replace
	restore
 
 preserve
 twoway (function y = x, clpat(.-.) rang(-0.01 0.3)) (scatter coef coef_p if x~=y & comp==1, color(dkorange) symbol(O)) (pcspike cl_h coef_p cl_l coef_p if x~=y & comp==1, lc(dkorange) symbol(i)) ///
 (scatter coef coef_p if x~=y & comp==2, color(red) symbol(D)) (pcspike cl_h coef_p cl_l coef_p if x~=y & comp==2, lc(red) symbol(D)) ///
 (scatter coef coef_p if x==y & comp==1, color(ebblue) symbol(T)) (pcspike cl_h coef_p cl_l coef_p if x==y & comp==1, lc(ebblue) symbol(i)) ///
 (scatter coef coef_p if x==y & comp==2, color(navy) symbol(V)) (pcspike cl_h coef_p cl_l coef_p if x==y & comp==2, lc(navy) symbol(i)) ///
  , legend(order (2 "Identities: Minority" 4 "Identities: Majority" 6 "Ideologies: Minority" 8 "Ideologies: Majority") pos(5) col(2) size(medlarge) ring(0)) ylab(,labsize(medlarge)) xlab(,labsize(medlarge)) ytitle("Effect on Share of Members with the Viewpoints",size(med)) xtitle("Predicted Effect on Share",size(med))
  gr export "Results/graph/first_stage_by_comp.png", replace
  restore
  
  g omega = stderr^2
  su coef if comp==1 & x~=y [weight=1/omega]
  su coef if comp==2 & x~=y [weight=1/omega]
  su coef if comp==1 & x==y [weight=1/omega]
  su coef if comp==2 & x==y [weight=1/omega]
  
  egen group_xy = group(x y)
  reg coef ib(1).comp i.group_xy [weight=1/omega]
  
  foreach y in coef coef_p cl_l cl_h {
  	replace `y'=`y'/ymean
  }
  
 /*foreach y in coef coef_p cl_l cl_h {
  	replace `y'=`y'+ymean
  } */
 twoway (function y = x, clpat(.-.) rang(0 1.5)) (scatter coef coef_p if x~=y & comp==1, color(dkorange) symbol(O)) (pcspike cl_h coef_p cl_l coef_p if x~=y & comp==1, lc(dkorange) symbol(i)) ///
 (scatter coef coef_p if x~=y & comp==2, color(red) symbol(D)) (pcspike cl_h coef_p cl_l coef_p if x~=y & comp==2, lc(red) symbol(D)) ///
 (scatter coef coef_p if x==y & comp==1, color(ebblue) symbol(T)) (pcspike cl_h coef_p cl_l coef_p if x==y & comp==1, lc(ebblue) symbol(i)) ///
 (scatter coef coef_p if x==y & comp==2, color(navy) symbol(V)) (pcspike cl_h coef_p cl_l coef_p if x==y & comp==2, lc(navy) symbol(i)) ///
  , legend(order (2 "Identities: Minority" 4 "Identities: Majority" 6 "Ideologies: Minority" 8 "Ideologies: Majority") pos(5) col(2) size(medlarge) ring(0)) ylab(,labsize(medlarge)) xlab(,labsize(medlarge)) ytitle("Effect on Share Share/Baseline Share",size(med)) xtitle("Predicted Effect on Share/Baseline Share",size(med))
  gr export "Results/graph/first_stage_by_comp_rel.png", replace

  su coef if comp==1 & x~=y [weight=1/omega]
  su coef if comp==2 & x~=y [weight=1/omega]
  su coef if comp==1 & x==y [weight=1/omega]
  su coef if comp==2 & x==y [weight=1/omega]
  
  reg coef i.comp i.group_xy [weight=1/omega]
  reg ymean i.comp i.group_xy [weight=1/omega]
  
  
  replace coef_p = ymean
   twoway (scatter coef coef_p if x~=y & comp==1, color(dkorange) symbol(O)) (pcspike cl_h coef_p cl_l coef_p if x~=y & comp==1, lc(dkorange) symbol(i)) ///
 (scatter coef coef_p if x~=y & comp==2, color(red) symbol(D)) (pcspike cl_h coef_p cl_l coef_p if x~=y & comp==2, lc(red) symbol(D)) ///
 (scatter coef coef_p if x==y & comp==1, color(ebblue) symbol(T)) (pcspike cl_h coef_p cl_l coef_p if x==y & comp==1, lc(ebblue) symbol(i)) ///
 (scatter coef coef_p if x==y & comp==2, color(navy) symbol(V)) (pcspike cl_h coef_p cl_l coef_p if x==y & comp==2, lc(navy) symbol(i)) ///
  , legend(order (1 "Identities: Minority" 3 "Identities: Majority" 5 "Ideologies: Minority" 7 "Ideologies: Majority") pos(5) col(2) size(medlarge) ring(0)) ylab(,labsize(medlarge)) xlab(,labsize(medlarge)) ytitle("Effect on Share Share/Baseline Share",size(med)) xtitle("Baseline Share",size(med))
  gr export "Results/graph/first_stage_by_comp_base.png", replace
  
  

