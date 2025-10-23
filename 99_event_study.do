********************************************************************************
* Main Univariate RDs in RD Plots
* Last Updated By Minseon Park 1/26/25
********************************************************************************

clear all
set seed 1234

* set directory 
cd "~/Dropbox/California Election Data/Code"
global logpath "~/Dropbox/California Election Data/Logs"

global year_cond0 = "year<=2017"
global year_cond1 = "year<."

*set scheme white_ptol

********************************************************************************
* I. Define Program
********************************************************************************

capture program drop did_plot
program define did_plot
  syntax, data(string) yvar(string) [yearcond(string) ytitle(string) subtitle(string) ylabel(string) saving(string)]
    
**********************
* 1/ Get coefficients
**********************
  
  use "data_for_rd/with_missing/dist_`data'.dta" if dist_cnty~=1, clear
  ren log_exp_total_per_stu exp_st
  ren teacher_exit_dist tch_exit	
  ren sus_share sus_pct
		
  foreach y in exp_st hsp_test ecd_test wht_test nec_test tch_exit {  
    cap g `y'_b = `y' if year==year_elected-1 
    sort multi_raceid `y'_b
    bysort multi_raceid: replace `y'_b = `y'_b[1]
    cap g `y'_D = `y'-`y'_b

  }
	foreach y in sus_pct {	// biannual toucome
		cap g `y'_b1 = `y' if year==year_elected-1 
		cap g `y'_b2 = `y' if year==year_elected-2 

		g `y'_b=.
		sort multi_raceid `y'_b1
		bysort multi_raceid: replace `y'_b = `y'_b1[1]

		sort multi_raceid `y'_b2
		bysort multi_raceid: replace `y'_b = `y'_b2[1] if missing(`y'_b)
		
		cap g `y'_D =`y'-`y'_b
		
	}	
  
  tab year, g(yr_)
  egen id_election = group(id_district_nces year_elected)
  
  g year_lag = year-year_elected
  replace year_lag = year_lag+4
  g T = cond(vote_margin>=0,1,0) if !missing(vote_margin)

	g year_treated = T*year_elected
	g year_treated2 = year_treated 
	recode year_treated2 (0=.)	

  rdrobust `yvar'_D vote_margin if year>=year_elected & `yearcond', p(1) kernel(triangular) bwselect(mserd) covs(yr_*) vce(nncluster id_election)
  cap local bandwidth = e(h_l) 
  cap drop kernel
  cap gen kernel = `bandwidth' - abs(vote_margin) if abs(vote_margin) <= `bandwidth'
  mat coef_l=e(beta_p_l)
  mat coef_r=e(beta_p_r)
	
	cap eststo: reghdfe `yvar' T##ib(3).year_lag##c.vote_margin if abs(vote_margin)<`bandwidth' & `yearcond' [aweight=kernel], absorb(id_election year) vce(robust)
	regsave using "Results/twfe_coefs.dta", ci replace addlabel(x, `data', y, `yvar', method, twfe)
	
	g yhat = .
	replace yhat = coef_l[1,1] + coef_l[2,1]*vote_margin if vote_margin < 0
	replace yhat = coef_l[1,1] + coef_r[2,1]*vote_margin if vote_margin >= 0
	
	g resid = `yvar'_D - yhat

	eststo: csdid resid if abs(vote_margin)<`bandwidth' & `yearcond' [iweight=kernel], ivar(id_election) time(year) gvar(year_treated) agg(event)
	regsave using "Results/twfe_coefs.dta", ci append addlabel(x, `data', y, `yvar', method, csdid)
	eststo: did_imputation resid id_election year year_treated2 if abs(vote_margin)<`bandwidth' & `yearcond' [aw=kernel], autosample maxit(1000) horizon(0/4) pre(3) minn(0)
	regsave using "Results/twfe_coefs.dta", ci append addlabel(x, `data', y, `yvar', method, did)

	
**********************
* 2/ Draw graph
**********************

use "Results/twfe_coefs.dta", clear

keep if strpos(var, "tau")|strpos(var, "pre")|strpos(var, "1.T#")|strpos(var, "Tm")|strpos(var, "Tp")
drop if strpos(var, "vote_margin")

gen evtime  = real(regexr(var, "tau", "")) if strpos(var, "tau") > 0
replace evtime  = -real(regexr(var, "pre", ""))-1 if strpos(var, "pre") > 0
replace evtime  = real(regexr(var, "Tp", "")) if strpos(var, "Tp") > 0
replace evtime  = -real(regexr(var, "Tm", ""))-1 if strpos(var, "Tm") > 0
replace evtime  = real(regexs(1))-4 if regexm(var, "1.T#([0-9]+)\.year_lag")

expand 2 if evtime== -2, gen(rep)
replace evtime=-1 if rep==1
replace ci_lower=0 if rep==1
replace ci_upper=0 if rep==1
replace coef=0 if rep==1
sort x y evtime

replace evtime = evtime-0.1 if method=="twfe"
replace evtime = evtime+0.1 if method=="did"


    // Handle optional ytitle
    local ytitle_option = ""
    if ("`ytitle'" != "") {
        local ytitle_option ytitle("`ytitle'")
    }
    
    // Handle optional subtitle with its options
    local subtitle_option = ""
    if ("`subtitle'" != "") {
        local subtitle_option subtitle("`subtitle'", ring(0) pos(11) size(*1.5))
    }
    
    // Handle optional ylabel
    local ylabel_option "ylabel(, labsize(medlarge))"
    if ("`ylabel'" != "") {
        local ylabel_option ylabel(`ylabel', labsize(medlarge))
    }
    
    twoway  (scatter coef evtime if method=="twfe" & strpos(y,"`yvar'") & x=="`data'", mcolor(dkorange*1.2)) ///
            (rcap ci_lower ci_upper evtime if method=="twfe" & strpos(y,"`yvar'") & x=="`data'", lcolor(dkorange*1.2)) /// 
            (scatter coef evtime if method=="csdid" & strpos(y,"`yvar'") & x=="`data'", mcolor(ebblue*2)) ///
            (rcap ci_lower ci_upper evtime if method=="csdid" & strpos(y,"`yvar'") & x=="`data'", lcolor(ebblue*2)) /// 
            (scatter coef evtime if method=="did" & strpos(y,"`yvar'") & x=="`data'", mcolor(green)) ///
            (rcap ci_lower ci_upper evtime if method=="did" & strpos(y,"`yvar'") & x=="`data'", lcolor(green)) /// 
            , xlabel(-4 "-4" -3 "-3" -2 "-2" -1 "-1" 0 "0" 1 "1" 2 "2" 3 "3" 4 "4", labsize(medlarge)) ///
            xtitle("Years Relative to Treatment") ///
            legend(col(3) pos(6) region(lcolor(white)) order(1 "TWFE" 3 "CSDID" 5 "DID Imputation")) yline(0) ///
            `ylabel_option' ///
            `ytitle_option' ///
            `subtitle_option' ///
            saving("Results/Graph/rd_`yvar'_`data'", replace)
            
    if ("`saving'" != "") {
        graph save "`saving'", replace
    }

	
end
   

********************************************************************************
* II. Draw and Combine Graphs
********************************************************************************
   
** Female   
did_plot, data("female") yvar("exp_st") yearcond("$year_cond0") ytitle("Log Spending Per Pupil") ylabel("-0.2(0.1)0.2") saving("Results/Graph/twfe_exp_st_female")
did_plot, data("female") yvar("ecd_test") yearcond("$year_cond1") ytitle("ECD Test Score") ylabel("-0.1(0.1)0.3") saving("Results/Graph/twfe_ecd_test_female")
did_plot, data("female") yvar("sus_pct") yearcond("$year_cond1") ytitle("Share Suspended") ylabel("-0.1(0.1)0.1") saving("Results/Graph/twfe_sus_pct_female")

*** Hisp
did_plot, data("hisp") yvar("exp_st") yearcond("$year_cond0") ylabel("-0.2(0.1)0.2") saving("Results/Graph/twfe_exp_st_hisp")
did_plot, data("hisp") yvar("ecd_test") yearcond("$year_cond1") ylabel("-0.1(0.1)0.3") saving("Results/Graph/twfe_ecd_test_hisp")
did_plot, data("hisp") yvar("sus_pct") yearcond("$year_cond1") ylabel("-0.1(0.1)0.1") saving("Results/Graph/twfe_sus_pct_hisp")

*** Teacher
did_plot, data("occ_teacher") yvar("exp_st") yearcond("$year_cond0") ytitle("Log Spending Per Pupil") ylabel("-0.2(0.1)0.2") saving("Results/Graph/twfe_exp_st_occ_teacher")
did_plot, data("occ_teacher") yvar("ecd_test") yearcond("$year_cond1") ytitle("ECD Test Score") ylabel("-0.1(0.1)0.3") saving("Results/Graph/twfe_ecd_test_occ_teacher")
did_plot, data("occ_teacher") yvar("tch_exit") yearcond("$year_cond1") ytitle("Share Exiting Teacher") ylabel("-0.3(0.2)0.3") saving("Results/Graph/twfe_tch_exit_occ_teacher")

*** Democrat
did_plot, data("democrat_v2") yvar("exp_st") yearcond("$year_cond0") ylabel("-0.2(0.1)0.2") saving("Results/Graph/twfe_exp_st_democrat")
did_plot, data("democrat_v2") yvar("ecd_test") yearcond("$year_cond1") ylabel("-0.1(0.1)0.3") saving("Results/Graph/twfe_ecd_test_democrat")
did_plot, data("democrat_v2") yvar("tch_exit") yearcond("$year_cond1") ylabel("-0.3(0.2)0.3") saving("Results/Graph/twfe_tch_exit_democrat")

*** Priorities
did_plot, data("budget_hawk") yvar("exp_st") yearcond("$year_cond0") ylabel("-0.2(0.1)0.2") subtitle("Budget Hawk")  saving("Results/Graph/twfe_exp_st_budget_hawk")
did_plot, data("equity_prior") yvar("ecd_test") yearcond("$year_cond1") ylabel("-0.1(0.1)0.3") subtitle("Equity")  saving("Results/Graph/twfe_ecd_test_equity_prior")
did_plot, data("safety_health_prior") yvar("sus_pct") yearcond("$year_cond1") ylabel("-0.2(0.1)0.2") subtitle("Safety")  saving("Results/Graph/twfe_sus_pct_safety_health_prior")
did_plot, data("teacher_care") yvar("tch_exit") yearcond("$year_cond1") ylabel("-0.3(0.2)0.3") subtitle("Teacher Support")  saving("Results/Graph/twfe_tch_exit_teacher_care")


graph combine "Results/Graph/twfe_exp_st_hisp.gph" "Results/Graph/twfe_ecd_test_hisp.gph" "Results/Graph/twfe_sus_pct_hisp.gph", ///
	col(1) ysize(3) xsize(9) subtitle("Hispanic Member", pos(12) box) saving("Results/Graph/twfe_hisp", replace)

graph combine "Results/Graph/twfe_exp_st_female.gph" "Results/Graph/twfe_ecd_test_female.gph" "Results/Graph/twfe_sus_pct_female.gph", ///
	col(1) ysize(3) xsize(9) subtitle("Female Member", pos(12) box) saving("Results/Graph/twfe_female", replace)

	graph combine "Results/Graph/twfe_exp_st_occ_teacher.gph" "Results/Graph/twfe_ecd_test_occ_teacher.gph" "Results/Graph/twfe_tch_exit_occ_teacher.gph", ///
	col(1) ysize(3) xsize(9) subtitle("Teacher Member", pos(12) box) saving("Results/Graph/twfe_occ_teacher", replace)
	
	graph combine "Results/Graph/twfe_exp_st_democrat.gph" "Results/Graph/twfe_ecd_test_democrat.gph" "Results/Graph/twfe_tch_exit_democrat.gph", ///
	col(1) ysize(3) xsize(9) subtitle("Democrat Member", pos(12) box) saving("Results/Graph/twfe_democrat", replace)
	
	graph combine "Results/Graph/twfe_exp_st_budget_hawk.gph" "Results/Graph/twfe_ecd_test_equity_prior.gph" "Results/Graph/twfe_sus_pct_safety_health_prior.gph", ///
	col(1) ysize(3) xsize(9) subtitle("By Priorities", pos(12) box) saving("Results/Graph/twfe_priority", replace)
	 
	graph combine "Results/Graph/twfe_exp_st_budget_hawk.gph" "Results/Graph/twfe_ecd_test_equity_prior.gph" "Results/Graph/twfe_tch_exit_teacher_care.gph", ///
	col(1) ysize(3) xsize(9) subtitle("By Priorities", pos(12) box) saving("Results/Graph/twfe_priority_v2", replace)

	graph combine "Results/Graph/twfe_female" "Results/Graph/twfe_hisp" "Results/Graph/twfe_priority", col(3) imargin(0 0 0 0)
	gr export "Results/Graph/twfe.png", replace
	graph combine "Results/Graph/twfe_occ_teacher" "Results/Graph/twfe_democrat" "Results/Graph/twfe_priority_v2", col(3) imargin(0 0 0 0)
	gr export "Results/Graph/twfe_v2.png", replace	
	


	
