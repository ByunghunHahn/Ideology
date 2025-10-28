********************************************************************************
* Mechanism: Specifically how do they acheive these changes in outcomes?
* Written by Minseon Park 05/04/25
* Latest updated by Minseon Park 06/05/25
********************************************************************************

version 18.5
clear all
set seed 1234

* set directory 
// cd "~/Library/CloudStorage/Dropbox/California Election Data/Code"
// global logpath "~/Library/CloudStorage/Dropbox/California Election Data/Logs"
// global grpath ="Results/Graph"

cd "C:\Users\hahn0\Desktop\Hahn_Park\Code"
global logpath "New_Logs"
global grpath ="Results/New_Graph"

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

global y_rev =""
global y_teacher = ""
global y_enroll = ""
global y_behv = ""
global y_fiscal= "exp_total_per_stu"
global y_score = "ecd_test"

do 0_f_rd_plots	


********************************************************************************
* I. Run RDs
********************************************************************************

foreach x in budget_hawk equity_prior hisp {
	use "data_for_rd/with_missing/`x'.dta", clear

	g necd_test = (cs_mn_nec_mth+cs_mn_nec_ela)/2

	do 4_f_prep_rd
	
	rdrobust bond_total vote_margin if year==year_elected-1, p(1) kernel(triangular) bwselect(mserd) covs(yr_*) vce(nn)
	regsave using "Results/outcome_dynamic_`x'", replace addlabel(y,"") 
	
	foreach y in $y_fiscal {
	foreach t in -4 -3 -2 0 1 2 3 4 {
		rdrobust `y'_D vote_margin if year==year_elected+`t' & year<=2017, p(1) kernel(triangular) bwselect(mserd) covs(yr_*) vce(nn)
		cap local bw = e(h_l) 
		
		g wgt = 1-abs(vote_margin)/`bw'
		reghdfe `y'_D i.T##c.vote_margin if year==year_elected+`t' & year<=2017 & abs(vote_margin)<=`bw' [aweight=wgt], absorb(year) vce(r)
		drop wgt
			
		lincom _cons
		local ymean = r(estimate)
		lincom _cons + 1.T
		local coef_sum = r(estimate)
		local cl_l = r(lb)
		local cl_h = r(ub)
	
	regsave using "Results/outcome_dynamic_`x'", append addlabel(coef_sum, `coef_sum', y,"`y'", x, "budget_hawk", t, "`t'", cl_l, `cl_l', cl_h, `cl_h', y0, `ymean') 
	}
	}
	foreach y in $y_score $y_enroll {
	foreach t in -4 -3 -2 0 1 2 3 4 {
		rdrobust `y'_D vote_margin if year==year_elected+`t', p(1) kernel(triangular) bwselect(mserd) covs(yr_*) vce(nn)
		cap local bw = e(h_l) 

		g wgt = 1-abs(vote_margin)/`bw'
		reghdfe `y'_D i.T##c.vote_margin if year==year_elected+`t' & abs(vote_margin)<=`bw' [aweight=wgt], absorb(year) vce(r)
		drop wgt
		
		lincom _cons
		local ymean = r(estimate)
		lincom _cons + 1.T
 local coef_sum = r(estimate)
		local cl_l = r(lb)
		local cl_h = r(ub)
	
	regsave using "Results/outcome_dynamic_`x'", append addlabel(coef_sum, `coef_sum', y,"`y'", x, "budget_hawk", t, "`t'", cl_l, `cl_l', cl_h, `cl_h', y0, `ymean') 
	}	
	}
}

	
********************************************************************************
* II. Draw Graphs
********************************************************************************

*** 1) Budget Hawk
	use "Results/outcome_dynamic_budget_hawk", clear

	keep if var=="1.T"
	expand 2 if t==-2
	bysort y x t: replace t=t+_n-1
	tab t
	
	foreach y in cl_h cl_l y0 coef_sum {
	replace `y' =0 if t==-1
	}

	plot_rd_dynamic, yvar("exp_total_per_stu") ///
 ytitle("Spending Per Pupil (vs. t=-1, $1k)")legend(legend(order(1 `"Fiscally Conservative Win"' 3 `"Lose"') pos(6) col(2) size(med))) ylabel(-2(1)3) ///
 flag_animation(1) ypos(3) 
	gr export "$grpath/rd_dynamic_fiscal_exp_1.png", replace
	
	plot_rd_dynamic, yvar("exp_total_per_stu") ///
 ytitle("Spending Per Pupil (vs. t=-1, $1k)")legend(legend(order(1 `"Fiscally Conservative Win"' 3 `"Lose"') pos(6) col(2) size(med))) ylabel(-2(1)3) ///
 ypos(3)  name("rd_dynamic_fiscal_exp")
	gr export "$grpath/rd_dynamic_fiscal_exp.png", replace
		
		
	*** 2) Equity	
	use "Results/outcome_dynamic_equity_prior", clear

	keep if var=="1.T"|var=="m2_mean:1.T"
	expand 2 if t==-2
	bysort y x t: replace t=t+_n-1
	tab t
	
	foreach y in cl_h cl_l y0 coef_sum {
	replace `y' =0 if t==-1
	}

	*** raw test scores
	plot_rd_dynamic, yvar("ecd_test") ytitle("Low-income Test Score (vs. t=-1)") ///
	ylabel(-0.05(0.05)0.1) flag_animation(1) ypos(0.1) ///
 legend(legend(order(1 `"Equity-focused Win"' 2 `"Lose"') pos(6) col(2) size(med))) 
	gr export "Results/New_Graph/rd_dynamic_equity_test_1.png", replace

	plot_rd_dynamic, yvar("ecd_test") ytitle("Low-income Test Score (vs. t=-1)") ///
	ylabel(-0.04(0.02)0.08) ypos(0.08) ///
 legend(legend(order(1 `"Equity-focused Win"' 3 `"Lose"') pos(6) col(2) size(med))) name("rd_dynamic_equity_test")
	gr export "Results/New_Graph/rd_dynamic_equity_test.png", replace
	
	*** composition itself
	plot_rd_dynamic, yvar("enrollment_black_share") ytitle("{&Delta}Share Black") ///
 legend(legend(order(1 `"Equity-focused Win"' 3 `"Lose"') pos(6) col(2) size(med) ring(0))) ypos(0.1) name("rd_dynamic_enrollment_black_share")

	plot_rd_dynamic, yvar("enrollment_hisp_share") ytitle("{&Delta}Share Hispanic") ///
 legend(legend(order(1 `"Equity-focused Win"' 3 `"Lose"') pos(6) col(2) size(med) ring(0))) ypos(0.1) name("rd_dynamic_enrollment_hisp_share")
	
	plot_rd_dynamic, yvar("perfrl") ytitle("{&Delta}Share FRL") ///
 legend(legend(order(1 `"Equity-focused Win"' 3 `"Lose"') pos(6) col(2) size(med) ring(0))) ypos(0.1) name("rd_dynamic_perfrl")

	plot_rd_dynamic, yvar("perecd") ytitle("{&Delta}Share ECD") ///
 legend(legend(order(1 `"Equity-focused Win"' 3 `"Lose"') pos(6) col(2) size(med) ring(0))) ypos(0.1) name("rd_dynamic_perecd")
	graph combine "Results/New_Graph/rd_dynamic_enrollment_black_share.gph" "Results/New_Graph/rd_dynamic_enrollment_hisp_share.gph" "Results/New_Graph/rd_dynamic_perfrl.gph" "Results/New_Graph/rd_dynamic_perecd", col(2) ysize(3) xsize(6) ycommon imargin(0 0 0 0)
	gr export "Results/New_Graph/rd_dynamic_equity_comp.png", replace
	
	*** composition effect?
	twoway (scatter coef_sum t if y=="ecd_test", color(ebblue) symbol(O)) /// 
			(pcspike cl_h t cl_l t if y=="ecd_test", lc(ebblue) symbol(i)) ///
			(scatter y0 t if y == "ecd_test", color(gs10%50) symbol(D)) ///
			(scatter coef_sum t if y=="test_pred", color(dkorange) symbol(T)) /// 
			, yline(0) xlab(-4(1)4,labsize(medlarge)) ///
			ytitle("{&Delta}ECD Test Score",size(medlarge)) xtitle("Relative Time From Election",size(med)) ///
			ylabel(-0.05(0.05)0.1) /// 
			legend(order(1 "Equity-focused Win (Total Effect)" 3 "Lose" 4 "Equity-focused Win (Composition Effect)" ) pos(6) col(2) size(med))
	gr export "Results/New_Graph/rd_dynamic_equity_test_ecd_comp.png", replace
	
	twoway (scatter coef_sum t if y=="test", color(ebblue) symbol(O)) /// 
			(pcspike cl_h t cl_l t if y=="test", lc(ebblue) symbol(i)) ///
			(scatter y0 t if y == "test", color(gs10%50) symbol(D)) ///
			(scatter coef_sum t if y=="test_pred", color(dkorange) symbol(T)) /// 
			, yline(0) xlab(-4(1)4,labsize(medlarge)) ///
			ytitle("{&Delta}Test Score",size(medlarge)) xtitle("Relative Time From Election",size(med)) ///
			subtitle(`subttl', size(med) ring(0) pos(12) box) ///
			ylabel(-0.05(0.05)0.1) /// 
			legend(order(1 "Equity-focused Win (Total Effect)" 3 "Lose" 4 "Equity-focused Win (Composition Effect)" ) pos(6) col(2) size(med))
	gr export "Results/New_Graph/rd_dynamic_equity_test_comp.png", replace
		
	
	*** 3) Hispanic	
	use "Results/outcome_dynamic_hisp", clear

	keep if var=="1.T"|var=="m2_mean:1.T"
	expand 2 if t==-2
	bysort y x t: replace t=t+_n-1
	tab t
	
	foreach y in cl_h cl_l y0 coef_sum {
	replace `y' =0 if t==-1
	}

	*** raw test scores
	plot_rd_dynamic, yvar("ecd_test") ytitle("ECD Test Score (vs. t=-1)") ///
	ylabel(-0.05(0.05)0.1) flag_animation(1) ypos(0.1) ///
 legend(legend(order(1 `"Equity-focused Win"' 2 `"Lose"') pos(6) col(2) size(med))) 
	gr export "Results/New_Graph/rd_dynamic_equity_test_1.png", replace

	plot_rd_dynamic, yvar("ecd_test") ytitle("ECD Test Score (vs. t=-1)") ///
	ylabel(-0.05(0.05)0.1) ypos(0.1) ///
 legend(legend(order(1 `"Hispanic Win"' 3 `"Lose"') pos(6) col(2) size(med))) name("rd_dynamic_hisp_test")
	gr export "Results/New_Graph/rd_dynamic_hisp_test.png", replace
	
	graph combine "$grpath/rd_dynamic_fiscal_exp.gph" "$grpath/rd_dynamic_equity_test.gph" "$grpath/rd_dynamic_hisp_test.gph" , col(1) ysize(2) xsize(1) imargin(0 0 0 0)
gr export "$grpath/rd_outcome_dynamic_case_study.png", replace // width(600) height(1200)

	plot_rd_dynamic, yvar("exp_total_per_stu") ytitle("Spending Per Pupil (vs. t=-1, $1k)") ///
	ylabel(-2(1)3) ypos(3) ///
 legend(legend(order(1 `"Hispanic Win"' 3 `"Lose"') pos(6) col(2) size(med))) name("rd_dynamic_hisp_exp")
	gr export "Results/New_Graph/rd_dynamic_hisp_exp.png", replace
	