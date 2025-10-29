********************************************************************************
* Main Univariate RDs in RD Plots
* Last Updated By Minseon Park 07/07/25
* I. Define Program
* II. Run and generate RD graphs
********************************************************************************
	
	version 18.5
clear all
set seed 1234

* set directory 
cd "C:\Users\hahn0\Desktop\Hahn_Park\Code"
global logpath "New_Logs"
global grpath ="Results/New_Graph"

global flag_balance = 0 // 0 if main rd 1 if balance
if $flag_balance==0 {
 global year_cond0 = "year>=year_elected & year<=year_elected+3" 
 global year_cond1 = "year>=year_elected"
}	
else if $flag_balance==1 {
 global year_cond0 = "year==year_elected-2" 
 global year_cond1 = "year<year_elected"
}


* member-level votes variables 
foreach yn in yes no {
	global y_votes_`yn' = "total_`yn'_votes academic_`yn'_votes budget_`yn'_votes curric_`yn'_votes equity_`yn'_votes parental_`yn'_votes procedural_`yn'_votes safety_`yn'_votes facilities_`yn'_votes supp_t_`yn'_votes other_NA_`yn'_votes"
	
	global y_votes_`yn'_share = "total_`yn'_votes_share academic_`yn'_votes_share budget_`yn'_votes_share curric_`yn'_votes_share equity_`yn'_votes_share parental_`yn'_votes_share procedural_`yn'_votes_share safety_`yn'_votes_share facilities_`yn'_votes_share supp_t_`yn'_votes_share other_NA_`yn'_votes_share"
}

foreach x in all oth {
	foreach yn in yes no {
		global y_votes_`yn'_`x' = "total_`yn'_votes_`x' academic_`yn'_votes_`x' budget_`yn'_votes_`x' curric_`yn'_votes_`x' equity_`yn'_votes_`x' parental_`yn'_votes_`x' procedural_`yn'_votes_`x' safety_`yn'_votes_`x' facilities_`yn'_votes_`x' supp_t_`yn'_votes_`x' other_NA_`yn'_votes_`x'"
		
		global y_votes_`yn'_`x'_share = "total_`yn'_votes_`x'_share academic_`yn'_votes_`x'_share budget_`yn'_votes_`x'_share curric_`yn'_votes_`x'_share equity_`yn'_votes_`x'_share parental_`yn'_votes_`x'_share procedural_`yn'_votes_`x'_share safety_`yn'_votes_`x'_share facilities_`yn'_votes_`x'_share supp_t_`yn'_votes_`x'_share other_NA_`yn'_votes_`x'_share"
	}
}

********************************************************************************
* I. Define Program
********************************************************************************

capture program drop rd_plot_wrapper
program define rd_plot_wrapper
 syntax, data(string) yvar(string) ///
		yearcond(string) ylab(string) ///
		[xlab(string) ytitle(string)] [xtitle(string)] ///
		[subtitle(string)] [cluster(string)] ///
		[xrange(string asis)] [name(string)] ///
		[flag_balance(integer 0)] [level(integer 0)] ///
		[weights(string)]
 
  use "data_for_rd/with_missing/votes_`data'.dta" if dist_cnty~=1, clear
 * year dummies and lags 
tab year, g(yr_)
egen id_election= group(leaid year_elected) // should be unique up to multi_raceid; multi_raceid is a string variable thus to be used for std err clustering, we need another numeric variable

g year_lag = year-year_elected
replace year_lag = year_lag+4
cap g T = cond(vote_margin>=0,1,0) if !missing(vote_margin)


 * rd plot 
 rd_plot, yvar(`yvar') yearcond(`yearcond') ///
				ylab(`ylab') ytitle(`ytitle') xtitle(`xtitle') ///
				cluster(`cluster') xrange(`xrange') name(`name') ///
				weights(`weights') level(`level') subtitle(`subtitle')
end

* rd plot functions 
do 0_f_rd_plots

* Equity 
rd_plot_wrapper, data("equity_prior") yvar("n_mtn_equity") yearcond("$year_cond0") ///
    ylab("0(10)30, labsize(*1.5)") xrange(10) ///
    ytitle("ytitle(N Passed, size(*1.5))") xtitle("") ///
    subtitle("subtitle(Equity Motions, ring(1) pos(12) size(*1.5))") ///
    flag_balance(0) cluster("nncluster") level(1) 
	gr export "Results/New_Graph/rd_mtn_equity.png", replace

rd_plot_wrapper, data("equity_prior") yvar("n_mtn_equity") yearcond("$year_cond0") ///
    ylab("0(10)30, labsize(*2)") xrange(10) ///
    ytitle("ytitle(N Passed Equity Motions, size(*2))") xtitle("xtitle(., size(*0.05))") ///
    flag_balance(0) cluster("nncluster") level(1) 
	gr export "Results/New_Graph/rd_mtn_equity_b.png", replace
	
rd_plot_wrapper, data("equity_prior") yvar("equity_yes_votes_mg1") yearcond("$year_cond0") ///
    ylab("-0.2(.2).6, labsize(*1.5)") xrange(10) ///
    ytitle("ytitle(Marginal Yes Votes, size(*1.5))") ///
    subtitle("subtitle(Equity Motions, ring(1) pos(12) size(*1.5))") ///
    flag_balance(0) cluster("nncluster") level(1) 
	gr export "Results/New_Graph/rd_votes_equity_mg1.png", replace

rd_plot_wrapper, data("equity_prior") yvar("equity_yes_votes_mg1") yearcond("$year_cond0") ///
    ylab("-0.2(.2).6, labsize(*2)") xrange(10) ///
    ytitle("ytitle(Pivotal Yes on Passed Equity Votes, size(*2))") xtitle("xtitle(., size(*0.05))") ///
    flag_balance(0) cluster("nncluster") level(1) 
	gr export "Results/New_Graph/rd_votes_equity_mg1_b.png", replace

	
* Budget Hawk 
rd_plot_wrapper, data("budget_hawk") yvar("n_mtn_budget") yearcond("$year_cond0") ///
    ylab("20(20)60, labsize(*1.5)") xrange(10) ///
    ytitle("ytitle(N Passed, size(*1.5))") ///
    subtitle("subtitle(Budget Motions, ring(1) pos(12) size(*1.5))") ///
    flag_balance(0) cluster("nncluster") level(1) 
	gr export "Results/New_Graph/rd_mtn_budget.png", replace

rd_plot_wrapper, data("budget_hawk") yvar("n_mtn_budget") yearcond("$year_cond0") ///
    ylab("20(20)60, labsize(*2)") xrange(10) ///
    ytitle("ytitle(N Passed Budget Motions, size(*2))") xtitle("xtitle(., size(*0.05))") ///
    flag_balance(0) cluster("nncluster") level(1) 
	gr export "Results/New_Graph/rd_mtn_budget_b.png", replace
	
rd_plot_wrapper, data("budget_hawk") yvar("budget_yes_votes_mg1") yearcond("$year_cond0") ///
    ylab("-0.2(.2).6, labsize(*1.5)") xrange(10) ///
    ytitle("ytitle(Marginal Yes Votes, size(*1.5))") ///
    subtitle("subtitle(Budget Motions, ring(1) pos(12) size(*1.5))") ///
    flag_balance(0) cluster("nncluster") level(1) 
	gr export "Results/New_Graph/rd_votes_budget_mg1.png", replace
	
rd_plot_wrapper, data("budget_hawk") yvar("budget_yes_votes_mg1") yearcond("$year_cond0") ///
    ylab("-0.2(.2).6, labsize(*2)") xrange(10) ///
    ytitle("ytitle(Pivotal Yes on Passed Budget Motions, size(*1.8))") xtitle("xtitle(., size(*0.05))") ///
    flag_balance(0) cluster("nncluster") level(1) 
	gr export "Results/New_Graph/rd_votes_budget_mg1_b.png", replace






