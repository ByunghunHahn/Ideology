********************************************************************************
* First Stage Simple RD Table & Figure
* Written by Minseon Park Mar 11, 2025
* Latest updated by Minseon Park Mar 27, 2025
* I. Process Composition Data
* II. RD Figure
* III. Simple Table
********************************************************************************

	version 18.5
	set seed 1234

	* set directory 
	cd "C:\Users\hahn0\Desktop\Hahn_Park\Code"
    global logpath "New_Logs"
	
	* setting
	global flag_balance =0 // 0 if main rd 1 if balance
	if $flag_balance==0 {
	 global year_cond = "year>=year_elected & year<=year_elected+3"
	}	
	else if $flag_balance==1 {
	 global year_cond = "year<year_elected-1"
	} 

	global list_prio =" budget_hawk equity_prior agenda_bias facility_prior parent_involvement safety_health_prior teacher_care dropout_prior enrollment_prior cte_prior score_concern sup_concern"
	global list_demo = "hisp female occ_teacher democrat_v2"

	global missing = "with_missing" // "with_missing" or "no_missing"
	
	global y_rev ="rev_cte_per_stu rev_f_s_drug_free_per_stu"
	global y_fiscal= "log_exp_total_per_stu exp_capital_total_per_stu"
	global y_teacher = ""
	global y_score = "ecd_test hsp_test"
	global y_enroll = ""
	global y_behv = "sus_share sus_share_hisp"	
	global y_sup= ""


********************************************************************************
* I. Process Composition Data
********************************************************************************
	
	use "board_composition/board_composition_elections.dta", clear
	*ren parent_involvement_prior* parent_involvement*
	
	foreach y in $list_prio $list_demo {
		g `y'_total = 0 
		forvalue n=1/14 {
			replace `y'_total = `y'_total+1 if `y'`n'==1
		}
		g `y'_total2 = 0
		forvalue n=1/14 {
			replace `y'_total2 = `y'_total2+1 if `y'`n'==0 // XX should we exclude members with invalid platform here?
		}	
		g `y'_share =`y'_total/(`y'_total + `y'_total2)
	}
	
	replace year=year-1
	save "Temp/board_composition.dta", replace
	

********************************************************************************
* II. RD Figure	
********************************************************************************

	*** rd plot wrapper function
	capture program drop rd_plot_wrapper
	program define rd_plot_wrapper
	  syntax, y(string) x(string) ylab(string) [ytitle(string)] [xtitle(string)] [subtitle(string)] [cluster(string)] [xrange(string asis)] [name(string)] [level(integer 0)]
	   
		use "data_for_rd/$missing/`x'.dta" if dist_cnty~=1, clear
		
		* prep y vars 
		do 4_f_prep_rd_new
		
		* merge with board composition data 
		merge m:1 leaid year using "Temp/board_composition.dta", keep(master matched) gen(_comp)
		
		* prep share vars (unique to first-stage analysis)
		g `y'_share_b = `y'_share if year==year_elected-1
		sort multi_raceid `y'_share_b
		bysort multi_raceid: replace `y'_share_b = `y'_share_b[1]
		cap g `y'_share_D = `y'_share - `y'_share_b
	  
		rd_plot, yvar(`y'_share) yearcond($year_cond) ///
				 ylab(`ylab') ytitle(`ytitle') xtitle(`xtitle') ///
				 cluster(`cluster') xrange(`xrange') name(`name') level(`level')	  
	end

	* rd plot functions 
	do 0_f_rd_plots
	

	*** Draw graphs and export PNG

	* equity 
	rd_plot_wrapper, y("equity_prior") x("equity_prior") ylab("-0.2(0.1)0.3, labsize(*1.5)") ytitle("ytitle(""Share of Equity-focused (vs. t=-1)"", size(*1.5))") xtitle("xtitle(""Vote Margin of Equity-focused (pp)"", size(*1.5))") cluster("cluster") xrange(15) name("first_stage_equity_prior") level(0)
	gr export "Results/New_Graph/first_stage_equity_prior.png", replace 

	rd_plot_wrapper, y("equity_prior") x("hisp") ylab("-0.2(0.1)0.3, labsize(*1.5)") ytitle("ytitle(""Share of Equity-focused (vs. t=-1)"", size(*1.5))") xtitle("xtitle(""Vote Margin of Hispanic"", size(*1.5))") cluster("cluster") xrange(15) name("first_stage_equity_prior_hisp")
	gr export "Results/New_Graph/first_stage_equity_prior_hisp.png", replace
	
	* budget - budget 
	rd_plot_wrapper, y("budget_hawk") x("budget_hawk") ylab("-0.2(0.1)0.3, labsize(*1.5)") ytitle("ytitle(""Share of Fiscally Conservative (vs. t=-1)"", size(*1.5))") xtitle("xtitle(""Vote Margin of Fiscally Conservative"", size(*1.5))") cluster("cluster") xrange(15) name("first_stage_budget_hawk")
	gr export "Results/New_Graph/first_stage_budget_hawk.png", replace 

	rd_plot_wrapper, y("budget_hawk") x("hisp") ylab("-0.2(0.1)0.3, labsize(*1.5)") ytitle("ytitle(""Share of Fiscally Conservative (vs. t=-1)"", size(*1.5))") xtitle("xtitle(""Vote Margin of Hispanic"", size(*1.5))") cluster("cluster") xrange(15) name("first_stage_budget_hawk_hisp")
	gr export "Results/New_Graph/first_stage_budget_hawk_hisp.png", replace 

********************************************************************************
* III. Simple Table	
********************************************************************************

	capture program drop rd_first_tab
	program define rd_first_tab
	  syntax, y(string) x(string)
		
		use "data_for_rd/$missing/`x'.dta", clear

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
		
		foreach yvar in `y'_share {	
			cap g `yvar'_b = `yvar' if year==year_elected-1
			sort multi_raceid `yvar'_b
			bysort multi_raceid: replace `yvar'_b = `yvar'_b[1]

			cap g `yvar'_D =`yvar'-`yvar'_b
		}

		eststo: rdrobust `y'_share_D vote_margin if $year_cond, p(1) kernel(triangular) bwselect(mserd) covs(yr_*) vce(cluster id_election)
		estadd scalar beta = round(tau_cl,0.001)
		estadd local CI = "[" + string(round(e(ci_l_rb),0.001),"%4.3f") + ", " + string(round(e(ci_r_rb),0.01),"%4.3f") + "]"
		estadd scalar cl_l= round(e(ci_l_rb),0.001)
		estadd scalar cl_h= round(e(ci_r_rb),0.001)
		estadd scalar Bandwidth = round(e(h_l),0.001)
		estadd scalar N_eff = round(e(N_h_l) + e(N_h_r)	,1)
	end
	
	
	* own identities
	eststo clear
	foreach y in $list_demo {
		rd_first_tab, y("`y'") x("`y'")
	}

	esttab using "Results/New_Tex/first_stage.tex", replace ///
    cells("b(fmt(3))") ///
    stats(CI Bandwidth N_eff, labels( " " "Bandwidth" "N") fmt("%s" 3 0)) ///
    coeflabels(RD_Estimate "Estimate") ///
    plain noobs nostar nogap nonumbers nomtitles nolines ///
    tex fragment collabels(none)
	
	* own priorities 
	eststo clear
	foreach y in $list_prio {
		rd_first_tab, y("`y'") x("`y'")
	}

	esttab est1 est2 est3 est4 est5 est6 using "Results/Tex/first_stage_prio_b1.tex", replace ///
    cells("b(fmt(3))") ///
    stats(CI Bandwidth N_eff, labels( " " "Bandwidth" "N") fmt("%s" 3 0)) ///
    coeflabels(RD_Estimate "Estimate") ///
    plain noobs nostar nogap nonumbers nomtitles nolines ///
    tex fragment collabels(none)

	esttab est7 est8 est9 est10 est11 est12 using "Results/Tex/first_stage_prio_b2.tex", replace ///
    cells("b(fmt(3))") ///
    stats(CI Bandwidth N_eff, labels( " " "Bandwidth" "N") fmt("%s" 3 0)) ///
    coeflabels(RD_Estimate "Estimate") ///
    plain noobs nostar nogap nonumbers nomtitles nolines ///
    tex fragment collabels(none)
	
