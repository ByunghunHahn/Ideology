********************************************************************************
* First Stage Dynamic Plots
* Written by Minseon Park Mar 27, 2025
********************************************************************************

	version 18.5
	set seed 1234

	* set directory 
	cd "C:\Users\hahn0\Desktop\Hahn_Park\Code"
    global logpath "New_Logs"

	* setting
	global flag_balance =0 // 0 if main rd 1 if balance
	if $flag_balance==0 {
	 global year_cond = "year>=year_elected"          
	}	
	else if $flag_balance==1 {
	 global year_cond = "year<year_elected-1"
	} 

	global list_prio ="agenda_bias budget_hawk equity_prior facility_prior parent_involvement_prior safety_health_prior teacher_care dropout_prior enrollment_prior cte_prior score_concern sup_concern"
	global list_demo = "hisp female occ_teacher democrat_v2"

	global missing = "with_missing" // "with_missing" or "no_missing"


	*** 1/ Store results
	capture program drop rd_dynamic
	program define rd_dynamic
	syntax, y(string) x(string)
    display "Starting rd_dynamic with y=`y' x=`x'"
    
    use "data_for_rd/$missing/`x'.dta", clear
    
    ren id_district_nces leaid
    tostring leaid, replace
    
    // Ensure consistent width by adding leading zeros
    gen leaid_padded = string(real(leaid), "%07.0f")
    replace leaid = leaid_padded
    drop leaid_padded
		  
		tab year, g(yr_)
		egen id_election = group(leaid year_elected)
		
		*** merge with composition
		merge m:1 leaid year using "Temp/board_composition.dta", keep(master matched) gen(_comp) // pre-processed in 3_1_first_stage.do
			
		cap g T = cond(vote_margin>=0,1,0) if !missing(vote_margin)

			foreach yvar in `y'_share {	
				cap g `yvar'_b = `yvar' if year==year_elected-1
				sort multi_raceid `yvar'_b
				bysort multi_raceid: replace `yvar'_b = `yvar'_b[1]

				cap g `yvar'_D =`yvar'-`yvar'_b
			}
			
		foreach t in  -3 -2 0 1 2 3 4  {
			eststo: rdrobust `y'_share_D vote_margin if year==year_elected+`t', p(1) kernel(triangular) bwselect(mserd) covs(yr_*) vce(hc0)
 		    local bw = e(h_l) 
 			local N_eff= e(N_h_l) + e(N_h_r)
			
			g wgt = 1-abs(vote_margin)/`bw'
			reghdfe `y'_share_D i.T##c.vote_margin if year==year_elected+`t' & abs(vote_margin)<=`bw' [aweight=wgt], absorb(year) vce(r)
			drop wgt
			
			lincom _cons
			local ymean = r(estimate)
			lincom _cons + 1.T
			local coef_sum = r(estimate)
			local se_sum = r(se)
			local cl_l = r(lb)
			local cl_h = r(ub)
			local pval = r(p)
		
		regsave using "Results/first_dynamic", append addlabel(coef_sum, `coef_sum', y,"`y'", x, "`x'", t, "`t'", cl_l, `cl_l', cl_h, `cl_h', pval, `pval',  N_eff, `N_eff', y0, `ymean') 	
		}
	end

	use "data_for_rd/$missing/budget_hawk.dta", clear
	rdrobust ecd_test vote_margin if year>=year_elected, p(1) kernel(triangular) bwselect(mserd) vce(hc0)
	regsave using "Results/first_dynamic", replace

	rd_dynamic, y("budget_hawk") x("budget_hawk")
	rd_dynamic, y("budget_hawk") x("hisp")
	rd_dynamic, y("budget_hawk") x("female")
	rd_dynamic, y("budget_hawk") x("democrat_v2")
	rd_dynamic, y("equity_prior") x("equity_prior")
	rd_dynamic, y("equity_prior") x("hisp")
	rd_dynamic, y("equity_prior") x("female")
				 
*** 2/ Draw plots
	do 0_f_rd_plots
	
	use "Results/first_dynamic", clear

	keep if var=="1.T"|var=="m2_mean:1.T"
	
	expand 2 if t==-2
	bysort y x t: replace t=t+_n-1
	tab t
	
	foreach y in coef cl_h cl_l y0 coef_sum {
	replace `y' =0 if t==-1
	}
	
	preserve
	keep if x=="budget_hawk"
	plot_rd_dynamic, yvar("budget_hawk") ///
    ytitle("Share of Fiscally Conservative (vs. t=-1)") ///
    legend(legend(order(1 `"Fiscally Conservative Win"' 3 `"Lose"') pos(6) col(2) size(med))) ///
    xlabel(-3(1)4) ylabel(-0.2(0.1)0.3) ypos(0.2) flag_animation(0) name("first_stage_dynamic_budget_hawk")
	gr export "Results/New_Graph/first_stage_dynamic_budget_hawk.png", replace
	restore
	
	preserve
	keep if x=="equity_prior"
	plot_rd_dynamic, yvar("equity_prior") ///
    ytitle("Share of Equity-focused (vs. t=-1)") ///
    legend(legend(order(1 `"Equity-focused Win"' 3 `"Lose"') pos(6) col(2) size(med))) ///
    xlabel(-3(1)4) ylabel(-0.2(0.1)0.3) ypos(0.2) flag_animation(0) name("first_stage_dynamic_equity_prior")
	gr export "Results/New_Graph/first_stage_dynamic_equity_prior.png", replace
	restore	

	preserve
	keep if x=="hisp" & y=="budget_hawk"
	plot_rd_dynamic, yvar("budget_hawk") ///
    ytitle("Share of Fiscally Conservative (vs. t=-1)") ///
    legend(legend(order(1 `"Hispanic Win"' 3 `"Lose"') pos(6) col(2) size(med))) ///
     xlabel(-3(1)4) ylabel(-0.2(0.1)0.3) ypos(0.2) flag_animation(0) name("first_stage_dynamic_budget_hawk_hisp")
	gr export "Results/New_Graph/first_stage_dynamic_budget_hawk_hisp.png", replace
	restore	
	
	preserve
	keep if x=="hisp" & y=="equity_prior"
	plot_rd_dynamic, yvar("equity_prior") ///
    ytitle("Share of Equity-focused (vs. t=-1)") ///
    legend(legend(order(1 `"Hispanic Win"' 3 `"Lose"') pos(6) col(2) size(med))) ///
     xlabel(-3(1)4) ypos(0.2) flag_animation(0) name("first_stage_dynamic_equity_prior_hisp")
	gr export "Results/New_Graph/first_stage_dynamic_equity_prior_hisp.png", replace
	restore	
	
	preserve
	keep if x=="female" & y=="budget_hawk"
	plot_rd_dynamic, yvar("budget_hawk") ///
    ytitle("Share of Fiscal Conservatism (vs. t=-1)") ///
    legend(legend(order(1 `"Female Win"' 3 `"Lose"') pos(6) col(2) size(med))) ///
     xlabel(-3(1)4) ypos(0.15) flag_animation(0) name("first_stage_dynamic_budget_hawk_female")
	gr export "Results/New_Graph/first_stage_dynamic_budget_hawk_female.png", replace
	restore	
	
	preserve
	keep if x=="female" & y=="equity_prior"
	plot_rd_dynamic, yvar("equity_prior") ///
    ytitle("Share of Equity-focused (vs. t=-1)") ///
    legend(legend(order(1 `"Female Win"' 3 `"Lose"') pos(6) col(2) size(med))) ///
     xlabel(-3(1)4) ypos(0.3) flag_animation(0) name("first_stage_dynamic_equity_prior_female")
	gr export "Results/New_Graph/first_stage_dynamic_equity_prior_female.png", replace
	restore	
	
	graph combine "Results/New_Graph/first_stage_dynamic_budget_hawk.gph" "Results/New_Graph/first_stage_dynamic_equity_prior.gph" "Results/New_Graph/first_stage_dynamic_equity_prior_hisp.gph" , col(1) ysize(2) xsize(1) imargin(0 0 0 0) ycommon
	gr export "Results/New_Graph/first_stage_dynamic_case_study.png", replace
	
	graph combine "Results/New_Graph/first_stage_dynamic_budget_hawk_female.gph" "Results/New_Graph/first_stage_dynamic_budget_hawk_hisp.gph" "Results/New_Graph/first_stage_dynamic_budget_hawk.gph", col(1) ysize(2) xsize(1) imargin(0 0 0 0) ycommon
	gr export "Results/New_Graph/first_stage_dynamic_budget_hawk_3panel.png", replace

	graph combine "Results/New_Graph/first_stage_dynamic_equity_prior_female.gph" "Results/New_Graph/first_stage_dynamic_equity_prior_hisp.gph" "Results/New_Graph/first_stage_dynamic_equity_prior.gph", col(1) ysize(2) xsize(1) imargin(0 0 0 0) ycommon
	gr export "Results/New_Graph/first_stage_dynamic_equity_prior_3panel.png", replace
	