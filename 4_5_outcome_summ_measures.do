********************************************************************************
* Mechanism: Specifically how do they acheive these changes in outcomes?
* 4_3_outcome_mechanism_transposed.do
* Mechanism: Specifically how do they achieve these changes in outcomes?
* Based on original 4_3_outcome_mechanism.do but exports transposed table format
* Written by Minseon Park 05/04/25
* Latest updated by Minseon Park 06/05/25
********************************************************************************

version 18.5
clear all
set seed 1234

* set directory 
cd "C:\Users\hahn0\Desktop\Hahn_Park\Code"
global logpath "New_Logs"
global grpath ="Results/New_Graph"

global y_rev =""
global y_teacher = ""
global y_enroll = ""
global y_behv = ""
global y_fiscal= "exp_total_per_stu"
global y_score = "test ecd_test hsp_test"
global y_sup = ""
do 0_f_rd_plots	
	
global flag_balance =0


********************************************************************************
* I. Run RDs
********************************************************************************

foreach x in budget_hawk equity_prior hisp {
	use "data_for_rd/with_missing/`x'.dta", clear

	do 4_f_prep_rd_new
	
	*** 2/ Run RDs
	rdrobust test_D vote_margin if year>=year_elected
	regsave using "Results/outcome_uni_`x'", replace // placeholder
		
	eststo clear
	foreach y in $y_fiscal {
		eststo: rdrobust `y'_D vote_margin if year>=year_elected & year<=2017, p(1) kernel(triangular) bwselect(mserd) covs(yr_*) vce(nncluster id_election)
		local cl_l= e(ci_l_rb)
		local cl_h= e(ci_r_rb)
		local pval = e(pv_rb)
		local bw = e(h_l)
		local N_eff = e(N_h_l) + e(N_h_r)	
		regsave using "Results/outcome_uni_`x'", append addlabel(y,"`y'", x,"`x'", cl_l, `cl_l', cl_h, `cl_h', pval, `pval', bw, `bw', N_eff, `N_eff') 	
	}
	
	foreach y in  $y_score {
		eststo: rdrobust `y'_D vote_margin if year>=year_elected, p(1) kernel(triangular) bwselect(mserd) covs(yr_*) vce(nncluster id_election)
		local cl_l= e(ci_l_rb)
		local cl_h= e(ci_r_rb)
		local pval = e(pv_rb)
		local bw = e(h_l)
		local N_eff = e(N_h_l) + e(N_h_r)	
		regsave using "Results/outcome_uni_`x'", append addlabel(y,"`y'", x,"`x'", cl_l, `cl_l', cl_h, `cl_h', pval, `pval', bw, `bw', N_eff, `N_eff') 	
	}	
	
	preserve
	xtset id_election year
	keep if year>=year_elected
	tsegen double sup_change_cum = rowmax(L(0/4).sup_change) // or rowtotal

	eststo: rdrobust sup_change_cum vote_margin if year>=year_elected, p(1) kernel(triangular) bwselect(mserd) covs(yr_*) vce(nncluster id_election)
	local cl_l= e(ci_l_rb)
	local cl_h= e(ci_r_rb)
	local pval = e(pv_rb)
	local bw = e(h_l)
	local N_eff = e(N_h_l) + e(N_h_r)	
	regsave using "Results/outcome_uni_`x'", append addlabel(y,"sup_change_cum", x,"`x'", cl_l, `cl_l', cl_h, `cl_h', pval, `pval', bw, `bw', N_eff, `N_eff')
	restore
}

clear
	local files "budget_hawk equity_prior hisp"
	foreach x in `files' {
		append using Results/outcome_uni_`x'.dta
	}


********************************************************************************
* Export Original Table Format 
********************************************************************************

	* Step 3: Manually label outcomes for display
	gen y_label = ""
	replace y_label = "Total Spending"        if y == "exp_total_per_stu"
	replace y_label = "Test"                  if y == "test"
	replace y_label = "Low-income Test"              if y == "ecd_test"
	replace y_label = "Hispanic Test"         if y == "hsp_test"

	* Step 4: Create display-ready strings
	gen coef_str = string(coef, "%4.3f")
	gen ci_str   = "[" + string(cl_l, "%4.3f") + ", " + string(cl_h, "%4.3f") + "]"
	gen N_str    = string(N_eff, "%9.0fc")
	* Step 4: Create display-ready strings for original format
	gen coef_str_orig = string(coef, "%4.3f")
	gen ci_str_orig   = "[" + string(cl_l, "%4.3f") + ", " + string(cl_h, "%4.3f") + "]"
	gen N_str_orig    = string(N_eff, "%9.0fc")

	* Step 5: Drop empty labels
	keep if y_label != ""

	* Step 6: Open LaTeX file
	* Step 6: Open LaTeX file for original format
	file open myfile using "Results/New_Tex/outcome_summary.tex", write replace

	file write myfile "\begin{tabular}{rccc} \toprule \hline" _n
	file write myfile " & Equity & Fiscally Conservatism &  Hispanic \\" _n
	file write myfile " & (1) & (2) & (3) \\ \midrule" _n

	* Step 7: Write Summary Measures
	foreach label in "Total Spending" "Test" "Low-income Test" "Hispanic Test" {
		foreach t in fc eq hi {
			preserve
			keep if x == cond("`t'"=="fc", "budget_hawk", cond("`t'"=="eq", "equity_prior", "hisp")) & y_label == "`label'"
			local `t'_est = coef_str[1]
			local `t'_ci  = ci_str[1]
			local `t'_N   = N_str[1]
			local `t'_est = coef_str_orig[1]
			local `t'_ci  = ci_str_orig[1]
			local `t'_N   = N_str_orig[1]
			restore
		}
		file write myfile "Estimate & `eq_est' & `fc_est'  & `hi_est' \\" _n
		file write myfile "\$[CI]\$ & `eq_ci' & `fc_ci'  & `hi_ci' \\" _n
		file write myfile " N & `eq_N'  & `fc_N' & `hi_N' \\" _n
	}
	
	file write myfile "\hline \bottomrule" _n
	file write myfile "\end{tabular}" _n
	file close myfile
