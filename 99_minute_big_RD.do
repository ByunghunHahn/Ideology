********************************************************************************
* RD with Multiple Chracteristics
* Written by Minseon Park 12/2/24
* Last updated by Minseon Park 2/19/25
********************************************************************************

set seed 123

set more off
set matsize 5000
set emptycells drop

* set directory 
cd "~/Dropbox/California Election Data/Code"
global logpath "~/Dropbox/California Election Data/Logs"
global datapath = "data_for_rd/with_missing"

global tab_opt = "b(a2) se(2) stats(N_eff N) label star(+ 0.1 * 0.05 ** 0.001) noomit compress nobaselevels"

capture program drop big_rd   
program define big_rd
		syntax, yvar(string) yearcond(string) kerneltype(string) treatment(string) [flag_tsal(integer 0)]
	
	
********************************************************************************
* I. Merge all RD data sets
********************************************************************************

	use "$datapath/dist_female.dta", clear
	ren vote_margin m_demo1
	merge 1:1 multi_raceid year using "$datapath/dist_hisp.dta", gen(_hisp)
	ren vote_margin m_demo2
	merge 1:1 multi_raceid year using "$datapath/dist_occ_teacher.dta", gen(_tch)
	ren vote_margin m_demo3
	merge 1:1 multi_raceid year using "$datapath/dist_democrat_v2.dta", gen(_demo)
	ren vote_margin m_demo4
	
	merge 1:1 multi_raceid year using "$datapath/dist_budget_hawk.dta", gen(_budget)
	ren vote_margin m_prio1
	merge 1:1 multi_raceid year using "$datapath/dist_equity_prior.dta", gen(_equity)
	ren vote_margin m_prio2
	merge 1:1 multi_raceid year using "$datapath/dist_safety_health_prior.dta", gen(_safety)
	ren vote_margin m_prio3
	merge 1:1 multi_raceid year using "$datapath/dist_teacher_care.dta", gen(_tch_care)
	ren vote_margin m_prio4
	merge 1:1 multi_raceid year using "$datapath/dist_agenda_bias.dta", gen(_agenda)
	ren vote_margin m_prio5
	merge 1:1 multi_raceid year using "$datapath/dist_parent_involvement_prior.dta", gen(_engage)
	ren vote_margin m_prio6		
	
	// Ensure consistent widT by adding leading zeros	
	gen leaid = string(id_district_nces, "%07.0f")  // Adjust "8" to Te required widT

	merge m:1 leaid year using "$mindta/minutes_final_year_level.dta", keep(master matched) gen(_minute)
	  
	
********************************************************************************
* III. Then run the rest of regressions
********************************************************************************

	tab year, g(yr_)
	egen id_election= group(id_district_nces year_elected) // should be unique up to multi_raceid; multi_raceid is a string variable thus to be used for std err clustering, we need another numeric variable

	*** first run RD with each characteristics
	forvalues t=1/4 {
	g T_demo`t'= cond(m_demo`t'>=0,1,0) if !missing(m_demo`t')
	
	qui rdrobust `yvar' m_demo`t' if `yearcond', p(1) kernel(`kerneltype') bwselect(mserd) covs(yr_*) vce(nncluster id_election)
	cap local bw = e(h_l) 
	estadd scalar N_eff = e(N_h_l) + e(N_h_r)
	
	*** calculate nuisance parameters
	g S_demo`t' = cond(abs(m_demo`t')<`bw',1,0)
	gen w_demo`t' = `bw' - abs(m_demo`t') if abs(m_demo`t') <= `bw'
	}
	
	*** repeat this for all other characteristics
	forvalues t=1/6 {
	g T_prio`t' = cond(m_prio`t'>=0,1,0) if !missing(m_prio`t')
		
	qui rdrobust `yvar' m_prio`t' if `yearcond', p(1) kernel(`kerneltype') bwselect(mserd) covs(yr_*) vce(nncluster id_election)
	cap local bw = e(h_l) 
	estadd scalar N_eff = e(N_h_l) + e(N_h_r)
	
	g S_prio`t' = cond(abs(m_prio`t')<`bw',1,0)
	gen w_prio`t' = `bw' - abs(m_prio`t') if abs(m_prio`t') <= `bw'
	}
	
	foreach x in demo1 demo2 demo3 demo4 prio1 prio2 prio3 prio4 prio5 prio6 {
		* margin/treatment variable is missing if the obs not from the data set of the focal char, so we code the missing to be any random number (coded as 0 here)	
		g T2_`x' = cond(missing(T_`x'),0,T_`x')
		g m2_`x' = cond(missing(m_`x'),0,m_`x')
		* then we want RD coefficient + linear control to be estimated only using when abs(m_l)<bw <=> S_l==1
		g T2_`x'_S = T2_`x' * S_`x'
		g m2_`x'_S = m2_`x' * S_`x'
	}

	// Generate spec automatically from treatment
    local spec ""
    local gr_spec ""
    foreach var of local treatment {
        local spec "`spec' T2_`var'_S##c.m2_`var'_S"
        local gr_spec "`gr_spec' S_`var'"
    }
    
	egen group = group(`gr_spec')    
		
    // Rest of your code using `final_spec' instead of hardcoded spec
    eststo: qui reghdfe `yvar' `spec' if `yearcond',absorb(year group) vce(robust)
    qui estadd ysumm
    eststo: qui reghdfe `yvar' `spec' if `yearcond',absorb(year group) vce(cluster id_election)
    qui estadd ysumm    
end


* demo1/2/3/4 - female/hisp/teacher/democrat
* prio1/2/3/4 - budget/equity/safety/teacher

eststo clear
big_rd, yvar("n_pass_budget_hawk") yearcond("year>=year_elected+1") kerneltype("triangular") treatment("demo1 demo2 demo3 demo4 prio1")
big_rd, yvar("n_pass_equity_prior") yearcond("year>=year_elected+1") kerneltype("triangular") treatment("demo1 demo2 demo3 demo4 prio2")
big_rd, yvar("n_pass_safety_health") yearcond("year>=year_elected+1") kerneltype("triangular") treatment("demo1 demo2 demo3 demo4 prio3")
big_rd, yvar("n_pass_teacher_care") yearcond("year>=year_elected+1") kerneltype("triangular") treatment("demo1 demo2 demo3 demo4 prio4")
big_rd, yvar("n_pass_agenda_bias") yearcond("year>=year_elected+1") kerneltype("triangular") treatment("demo1 demo2 demo3 demo4 prio5")
big_rd, yvar("n_pass_engagement") yearcond("year>=year_elected+1") kerneltype("triangular") treatment("demo1 demo2 demo3 demo4 prio6")
esttab using Results/results_temp.csv,  $tab_opt keep(*T2*) replace
