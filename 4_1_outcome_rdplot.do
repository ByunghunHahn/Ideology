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

global y_rev ="rev_cte_per_stu rev_f_s_drug_free_per_stu"
global y_fiscal= "log_exp_total_per_stu exp_total_per_stu exp_capital_total_per_stu"
global y_teacher = ""
global y_score = "ecd_test hsp_test test"
global y_enroll = ""
global y_behv = "sus_share sus_share_hisp"	
global y_sup = "sup_change"


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
 
 // use "data_for_rd/with_missing/dist_`data'.dta", clear
 use "data_for_rd/with_missing/`data'.dta", clear
 
 * prep y vars 
 do 4_f_prep_rd
 
 * rd plot 
 rd_plot, yvar(`yvar') yearcond(`yearcond') ///
				ylab(`ylab') ytitle(`ytitle') xtitle(`xtitle') ///
				cluster(`cluster') xrange(`xrange') name(`name') ///
				weights(`weights') level(`level')		
end

* rd plot functions 
do 0_f_rd_plots


********************************************************************************
* II. Run
********************************************************************************

*** Generate individual plots
* Y var = Expenditure
rd_plot_wrapper, data("hisp") yvar("exp_total_per_stu") yearcond("$year_cond0") ///
    ylab("0.5(0.5)2.5, labsize(*1.5)") xrange(6) ///
	xtitle("xtitle(""Vote Margin of Hispanic"", size(*1.5))") ///
    ytitle("ytitle(Spending Per Pupil (vs. t=-1, $1k), size(*1.5))") ///
    flag_balance(0) cluster("nncluster") name("rd_exp_st_hisp")
gr export "$grpath/rd_exp_st_hisp.png", replace 

rd_plot_wrapper, data("budget_hawk") yvar("exp_total_per_stu") yearcond("$year_cond0") ///
    ylab("0.5(0.5)2.5, labsize(*1.5)") xrange(3) ///
	xtitle("xtitle(""Vote Margin of Fiscally Conservative"", size(*1.5))") ///
    ytitle("ytitle(Spending Per Pupil (vs. t=-1, $1k), size(*1.5))") ///
    flag_balance(0) cluster("nncluster") name("rd_exp_st_budget_hawk")
gr export "$grpath/rd_exp_st_budget_hawk.png", replace 

* Y var = Test Scores
rd_plot_wrapper, data("hisp") yvar("ecd_test") yearcond("$year_cond1") ///
    ylab("-0.04(0.02)0.06, labsize(*1.5)") xrange(8) ///
    ytitle("ytitle(ECD Test Score (vs. t=-1), size(*1.5))") ///
		xtitle("xtitle(""Vote Margin of Hispanic"", size(*1.5))") ///
    subtitle("subtitle(Hispanic, ring(1) pos(12) size(*1.5))") ///
    flag_balance(0) cluster("nncluster") name("rd_ecd_test_hisp")
gr export "$grpath/rd_ecd_test_hisp.png", replace	 

rd_plot_wrapper, data("equity_prior") yvar("ecd_test") yearcond("$year_cond1") ///
    ylab("-0.04(0.02)0.06, labsize(*1.5)") xrange(3) ///
	 xtitle("xtitle(""Vote Margin of Equity-focused (pp)"", size(*1.5))") ///
    ytitle("ytitle(Low-income Test Score (vs. t=-1), size(*1.5))") ///
    flag_balance(0) cluster("nncluster") name("rd_ecd_test_equity_prior")
gr export "$grpath/rd_ecd_test_equity_prior.png", replace	 

rd_plot_wrapper, data("hisp") yvar("hsp_test") yearcond("$year_cond1") ///
    ylab("-0.02(0.02)0.06, labsize(*1.5)") xrange(10) ///
    ytitle("ytitle({&Delta}Hispanic Test Score, size(*1.5))") ///
    flag_balance(0) cluster("nncluster")
gr export "$grpath/rd_hsp_test_hisp.png", replace
