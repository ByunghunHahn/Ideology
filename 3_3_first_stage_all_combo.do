********************************************************************************
* Univariate RD with All yvars Variables 
* Written By Minseon Park 04/06/2025
* Latest updated by Minseon Park 05/27/25
********************************************************************************

version 18.5
set seed 1234

* set directory 
cd "C:\Users\hahn0\Desktop\Hahn_Park\Code"
global logpath "New_Logs"


	global list_demo = "hisp female occ_teacher democrat_v2"
	global list_prio ="budget_hawk equity_prior safety_health_prior teacher_care agenda_bias parent_involvement score_concern facility_prior cte_prior dropout_prior enrollment_prior sup_concern"
	
	global missing = "with_missing" // "with_missing" or "no_missing"
	
	global flag_RD = 1 // 1 to generate RD results, 0 if already have one

if $flag_RD == 1 {
		
	* placeholder - dummy result to gerate .dta file
	use "data_for_rd/$missing/hisp.dta", clear
	rdrobust log_exp_pupils vote_margin if year==year_elected-1 , p(1) kernel(triangular) bwselect(mserd) 
	regsave using "Results/first_stage_$missing", replace
	
********************************************************************************
* I. RD using Observed Share 
********************************************************************************

	foreach xvar in $list_demo $list_prio {
		
		use "data_for_rd/$missing/`xvar'.dta", clear

			ren id_district_nces leaid
			tostring leaid, replace  // Convert to string if not already

			*** merge with minute data
			// Ensure consistent width by adding leading zeros
			gen leaid_padded = string(real(leaid), "%07.0f")  // Adjust "8" to the required width
			replace leaid = leaid_padded
			drop leaid_padded
			  
			tab year, g(yr_)
			egen id_election = group(leaid year_elected)
		
			*** merge with composition
			merge m:1 leaid year using "Temp/board_composition.dta", keep(master matched) gen(_comp)	
			
	foreach y in $list_prio {
			g `y'_share_b = `y'_share if year==year_elected-1
			sort multi_raceid `y'_share_b
			bysort multi_raceid: replace `y'_share_b = `y'_share_b[1]

			g `y'_share_D =`y'_share-`y'_share_b
			
			rdrobust `y'_share_D vote_margin if year>=year_elected & year<=year_elected+3, p(1) kernel(triangular) bwselect(mserd) covs(yr_*) vce(cluster id_election)
			local cl_l= e(ci_l_rb)
			local cl_h= e(ci_r_rb)
			local pval = e(pv_rb)
			local bw = e(h_l)
			local N_eff = e(N_h_l) + e(N_h_r)	
			regsave using "Results/first_stage_$missing", append addlabel(prio,"`y'", demo,"`xvar'", cl_l, `cl_l', cl_h, `cl_h', pval, `pval', bw, `bw', N_eff, `N_eff') 
	}
}

********************************************************************************
* II. RD using Predicted (based on raw difference) Share
********************************************************************************

foreach xvar in $list_demo {

		use "data_for_rd/$missing/`xvar'.dta" if dist_cnty~=1, clear

		*** merge with minute data
		// Ensure consistent width by adding leading zeros
		tostring id_district_nces, replace  // Convert to string if not already	
		gen leaid = string(real(id_district_nces), "%07.0f")  // Adjust "8" to the required width
		tab year, g(yr_)
		egen id_election = group(leaid year_elected)
		
		*** merge with composition
		merge m:1 leaid year using "board_composition/board_composition_elections.dta", keep(master matched) gen(_comp)
		
		* first detect "the" candidate and get rid of their characteristics
		g n=.
		forvalues n=1/14 {
			replace n=`n' if winner_multi_candid==multi_candid`n' & multi_candid`n'!=.
		}
		
		foreach y in $list_prio {
		forvalues n=1/14 {
			replace `y'`n'=. if n==`n'
		}
		
		g `y'_total = 0 
		g `y'_total2 = 0
		forvalue n=1/14 {
			replace `y'_total = `y'_total+1 if `y'`n'==1
			replace `y'_total2 = `y'_total2+1 if `y'`n'==0 // exclude if no platform
			}	
		
		* construct predicted share using raw difference 
		preserve
		use Results/bar_reg, clear
		su mean1 if demo=="`xvar'" & prio=="`y'"
		local mu1 = r(mean)
		su mean2 if demo=="`xvar'" & prio=="`y'"
		local mu2 = r(mean)
		restore
		
		g `y'_share = (`y'_total+ `mu2') /(`y'_total + `y'_total2 + 1) if vote_margin>=0
		replace `y'_share = (`y'_total+ `mu1') /(`y'_total + `y'_total2 + 1) if vote_margin<0
		
		g `y'_share_b = `y'_share if year==year_elected-1
		sort multi_raceid `y'_share_b
		bysort multi_raceid: replace `y'_share_b = `y'_share_b[1]

		g `y'_share_D =`y'_share-`y'_share_b
			
		eststo: rdrobust `y'_share_D vote_margin if year>=year_elected & year<=year_elected+3, p(1) kernel(triangular) bwselect(mserd) covs(yr_*) vce(cluster id_election)
		local cl_l= e(ci_l_rb)
		local cl_h= e(ci_r_rb)
		local pval = e(pv_rb)
		local bw = e(h_l)
		local N_eff = e(N_h_l) + e(N_h_r)	

		regsave using "Results/first_stage_$missing", append addlabel(prio,"`y'", demo,"`xvar'", cl_l, `cl_l', cl_h, `cl_h', pval, `pval', bw, `bw', N_eff, `N_eff', predicted,"yes") 
		}
}
}

********************************************************************************
* III. 2D Visualization and Bar Graph assuming Independent Error Terms
********************************************************************************

	do 3_f_compile_h0.do // some H0 is based on the first stage results, so 
	/// we should compile H0 here. then we use hypothesis type right away
	// to draw graph for first stage results as follows
	
	use "Results/first_stage_$missing", clear

	drop if prio==""
	keep if strpos("female hisp occ_teacher democrat_v2", demo) | prio==demo
	 
	egen group = group(demo prio)
	gsort group -predicted
	bysort group: g coef_p = coef[1]
	drop if predicted=="yes"
	 
	foreach y in coef cl_l cl_h coef_p {
	replace `y' = -`y' if coef_p<0
	 }
	
	merge 1:1 demo prio using "Temp/h0_list_first_stage.dta"

	* case study labels 
	gen casestudy = ""
	replace casestudy = "Hispanic -> Fiscal Conservative" if prio=="budget_hawk" & demo=="hisp"
	replace casestudy = "Hispanic -> Equity" if prio=="equity_prior" & demo=="hisp"
	replace casestudy = "Fiscal Conservative -> Fiscal Conservative" if prio=="budget_hawk" & demo=="budget_hawk"
	replace casestudy = "Equity -> Equity" if prio=="equity_prior" & demo=="equity_prior"
	replace casestudy = "Teacher -> Teacher Friendly" if prio=="teacher_care" & demo=="occ_teacher"

	drop if coef>0.25
	*** 2D graph
	twoway (function y=x, clpat(.-.) rang(-0.01 0.25) color(gray)) ///
			(scatter coef coef_p if prio~=demo, color(dkorange) symbol(O)) ///
			(pcspike cl_h coef_p cl_l coef_p if prio~=demo, lc(dkorange) symbol(i) ) ///
			(scatter coef coef_p if prio == demo, color(ebblue) symbol(T)) ///
			(pcspike cl_h coef_p cl_l coef_p if prio == demo, lc(ebblue) symbol(i)) ///
			(scatter coef coef_p if prio~=demo & casestudy~="" , color(dkorange*2) symbol(S)) ///
			(pcspike cl_h coef_p cl_l coef_p if prio~=demo & casestudy~="" , lc(dkorange*2) symbol(i)) ///
			(pcspike cl_h coef_p cl_l coef_p if prio==demo & casestudy~="" , lc(navy) symbol(i)) ///
	  , legend(order (6 "Identities (existing literature)" 4 "Ideologies" 2 "Identities (other)") pos(5) col(2) size(med) ring(0)) ///
	  ylab(,labsize(medlarge)) xlab(,labsize(medlarge)) ///
	  ytitle("Share of Ideology (vs. t=-1)",size(medlarge)) ///
	  xtitle("Implied Share of Ideology (vs. t=-1) from Group Difference",size(medlarge)) ///
	  text(0.03 0.045 "Hispanic→Equity", size(medlarge)) ///
	  text(-0.01 0.033 "Hispanic→Budget", size(medlarge)) ///
	  text(0.18 0.215 "Equity", size(medlarge)) ///
	  text(0.23 0.165 "Budget", size(medlarge)) ///	  
	  text(0.25 0.23 "45° line", place(e) size(medlarge))  
	 gr export "Results/New_Graph/first_stage_2d_all_$missing.png", replace
	 
	 *** the rest is for animation in presentation
	 * same as above but case studies labeled only 
	 twoway (scatter coef coef_p if prio==demo & casestudy!="", color(ebblue) symbol(T)) ///
			(pcspike cl_h coef_p cl_l coef_p if prio==demo & casestudy!="", lc(ebblue) symbol(i)) ///	
			(scatter coef coef_p if prio~=demo & casestudy!="" & demo~="occ_teacher", color(dkorange*2) symbol(S)) ///
			(pcspike cl_h coef_p cl_l coef_p if prio~=demo & casestudy!="" & demo~="occ_teacher", lc(dkorange*2) symbol(i)) ///
			(function y=x, clpat(.-.) rang(-0.01 0.25) color(gray)) ///
	  , legend(off) ytitle("Share of Ideology (vs. t=-1)",size(medlarge)) ///
	  ylab(,labsize(medlarge)) xlab(,labsize(medlarge)) ///
	  xtitle("Implied Share of Ideology (vs. t=-1) from Group Difference",size(medlarge)) ///
	  text(0.03 0.045 "Hispanic→Equity", size(medlarge)) ///
	  text(-0.01 0.033 "Hispanic→Budget", size(medlarge)) ///
	  text(0.18 0.215 "Equity", size(medlarge)) ///
	  text(0.23 0.165 "Budget", size(medlarge)) ///	  
	  text(0.25 0.23 "45° line", place(e) size(medlarge)) 
	  gr export "Results/New_Graph/first_stage_2d_casestudy.png", replace
	 
	  
	 twoway (scatter coef coef_p if demo=="equity_prior", color(ebblue) symbol(T)) ///
			(pcspike cl_h coef_p cl_l coef_p if demo=="equity_prior", lc(ebblue) symbol(i)) ///	
	  , legend(off) ytitle("Share of Ideology (vs. t=-1)",size(medlarge)) ///
	  ylab(,labsize(medlarge)) xlab(,labsize(medlarge)) ///
	  xtitle(".",size(medlarge)) ///
	  xsc(r(0 0.25)) ysc(r(-0.1 0.3)) xlabel(0(0.05)0.25) ylabel(-0.1(0.1)0.3) ///
	  text(0.18 0.215 "Equity", size(medlarge)) ///
	  yline(0.198)
	 gr export "Results/New_Graph/first_stage_2d_casestudy1.png", replace
	 
	 twoway (scatter coef coef_p if prio==demo & casestudy!="", color(ebblue) symbol(T)) ///
			(pcspike cl_h coef_p cl_l coef_p if prio==demo & casestudy!="", lc(ebblue) symbol(i)) ///	
	  , legend(off) ytitle("Share of Ideology (vs. t=-1)",size(medlarge)) ///
	  ylab(,labsize(medlarge)) xlab(,labsize(medlarge)) ///
	  xtitle(".",size(medlarge)) ///
	  xsc(r(0 0.25)) ysc(r(-0.1 0.3)) xlabel(0(0.05)0.25) ylabel(-0.1(0.1)0.3) ///
	  text(0.18 0.215 "Equity", size(medlarge)) ///
	   text(0.23 0.165 "Budget", size(medlarge)) ///
	  yline(0.198) yline(0.16) 
	 gr export "Results/New_Graph/first_stage_2d_casestudy2.png", replace
	 
	 twoway (scatter coef coef_p if prio==demo & casestudy!="", color(ebblue) symbol(T)) ///
			(pcspike cl_h coef_p cl_l coef_p if prio==demo & casestudy!="", lc(ebblue) symbol(i)) ///	
			(scatter coef coef_p if prio~=demo & casestudy!="" & demo~="occ_teacher", color(dkorange*2) symbol(S)) ///
			(pcspike cl_h coef_p cl_l coef_p if prio~=demo & casestudy!="" & demo~="occ_teacher", lc(dkorange*2) symbol(i)) ///
	  , legend(off) ytitle("Share of Ideology (vs. t=-1)",size(medlarge)) ///
	  ylab(,labsize(medlarge)) xlab(,labsize(medlarge)) ///
	  xtitle(".",size(medlarge)) ///
	  xsc(r(0 0.25)) ysc(r(-0.1 0.3)) xlabel(0(0.05)0.25) ylabel(-0.1(0.1)0.3) ///
	  text(0.18 0.215 "Equity", size(medlarge)) ///
	  text(0.23 0.165 "Budget", size(medlarge)) ///
	  text(0.03 0.045 "Hispanic→Equity", size(medlarge)) ///
	  text(-0.01 0.033 "Hispanic→Budget", size(medlarge)) /// 
	  yline(0.198) yline(0.16) yline(0.004) yline(0.019)
	 gr export "Results/New_Graph/first_stage_2d_casestudy3.png", replace
	 
	 twoway (scatter coef coef_p if prio~=demo, color(dkorange) symbol(O)) ///
			(pcspike cl_h coef_p cl_l coef_p if prio~=demo, lc(dkorange) symbol(i) ) ///
			(scatter coef coef_p if prio == demo, color(ebblue) symbol(T)) ///
			(pcspike cl_h coef_p cl_l coef_p if prio == demo, lc(ebblue) symbol(i)) ///
			(scatter coef coef_p if prio~=demo & casestudy~="", color(dkorange*2) symbol(S)) ///
			(pcspike cl_h coef_p cl_l coef_p if prio~=demo & casestudy~="", lc(dkorange*2) symbol(i)) ///
			(scatter coef coef_p if prio == demo & casestudy~="", color(navy) symbol(T)) ///
			(pcspike cl_h coef_p cl_l coef_p if prio==demo & casestudy~="" , lc(navy) symbol(i)) ///
	  , legend(order (6 "Identities (existing literature)" 4 "Ideologies" 2 "Identities (other)") pos(5) col(2) size(med) ring(0)) ///
	  ylab(,labsize(medlarge)) xlab(,labsize(medlarge)) ///
	  ytitle("Share of Ideology (vs. t=-1)",size(medlarge)) ///
	  xtitle(".",size(medlarge)) ///
	  xsc(r(0 0.25)) ysc(r(-0.1 0.3)) xlabel(0(0.05)0.25) ylabel(-0.1(0.1)0.3) ///
	  text(0.03 0.045 "Hispanic→Equity", size(medlarge)) ///
	  text(-0.01 0.033 "Hispanic→Budget", size(medlarge)) ///
	  text(0.18 0.215 "Equity", size(medlarge)) ///
	  text(0.23 0.165 "Budget", size(medlarge))
		gr export "Results/New_Graph/first_stage_2d_casestudy4.png", replace
	  
	 
	 twoway (scatter coef coef_p if prio==demo & casestudy!="", color(ebblue) symbol(T)) ///
			(pcspike cl_h coef_p cl_l coef_p if prio==demo & casestudy!="", lc(ebblue) symbol(i)) ///	
			(scatter coef coef_p if prio~=demo & casestudy!="" & demo~="occ_teacher", color(dkorange*2) symbol(S)) ///
			(pcspike cl_h coef_p cl_l coef_p if prio~=demo & casestudy!="" & demo~="occ_teacher", lc(dkorange*2) symbol(i)) ///
			(function y=x, clpat(.-.) rang(-0.01 0.25) color(gray)) ///
	  , legend(off) ytitle("Share of Ideology (vs. t=-1)",size(medlarge)) ///
	  ylab(,labsize(medlarge)) xlab(,labsize(medlarge)) ///
	  xtitle("Implied Share of Ideology (vs. t=-1) from Group Difference",size(medlarge)) ///
	  text(0.03 0.045 "Hispanic→Equity", size(medlarge)) ///
	  text(-0.01 0.033 "Hispanic→Budget", size(medlarge)) ///
	  text(0.18 0.215 "Equity", size(medlarge)) ///
	  text(0.23 0.165 "Budget", size(medlarge)) ///	  
	  text(0.25 0.23 "45° line", place(e) size(medlarge)) ///
	  xline(0.0075) xline(0.0175)
	  gr export "Results/New_Graph/first_stage_2d_casestudy5.png", replace
	 