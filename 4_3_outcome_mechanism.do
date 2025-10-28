********************************************************************************
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
global y_behv = "enrollment_black_share enrollment_hisp_share enrollment_asian_share"
global y_fiscal= "exp_total_per_stu sal_inst_per_stu sal_reg_per_stu sal_sped_per_stu sal_oth_per_stu exp_capital_total_per_stu exp_cap_const_per_stu exp_cap_land_per_stu exp_capital_equip_per_stu exp_admin_per_stu bond_total"
global y_score = "test ecd_test necd_test hsp_test wht_test test_pred"
global y_sup = ""
do 0_f_rd_plots	
	
global flag_balance =0

	* do 4_f_prep_rd
	* Error with xtset id_election year (Since there are more than two elections in a single year)
	* Instead, we generate id_election = group(leaid year_elected multi_raceid)

********************************************************************************
* I. Run RDs
********************************************************************************

foreach x in budget_hawk equity_prior hisp {
	use "data_for_rd/with_missing/`x'.dta", clear

	g necd_test = (cs_mn_nec_mth+cs_mn_nec_ela)/2
			
	*** 1/ Generate more detailed spending variables
	g sal_oth_per_stu=(sal_total - sal_inst)/enrollment
	g sal_reg_per_stu=sal_tch_regular_prog/enrollment
	g sal_sped_per_stu=sal_tch_sped/enrollment
	g log_sal_oth_per_stu = log(sal_oth_per_stu)
	g exp_admin_per_stu = (exp_sch_admin+exp_general_admin)/enrollment
	g exp_enterprise_per_stu = exp_enterprise /enrollment
	g exp_operation_plant_per_stu =exp_operation_plant/enrollment
 
	ren exp_capital_construction exp_cap_const
	ren exp_capital_land_structures exp_cap_land
	g exp_capital_equip = exp_capital_instruc_equip + exp_capital_other_equip

	foreach y in exp_cap_const exp_cap_land exp_capital_equip {
		g `y'_per_stu=`y'/enrollment
		g log_`y'_per_stu = log(`y'_per_stu)
	}	

	*do 4_f_prep_rd
	* clean leaid var 
	cap ren id_district_nces leaid
	tostring leaid, replace  // Convert to string if not already

	* rename vocational vars 
	cap ren *vocational* *cte*

	// Ensure consistent width by adding leading zeros
	gen leaid_padded = string(real(leaid), "%07.0f")  // Adjust "8" to 	the required width
	replace leaid = leaid_padded
	drop leaid_padded
 
	* gen baseline and difference vars 
	foreach yv in $y_rev $y_fiscal $y_teacher $y_enroll $y_charter 	$y_score $y_sup {	
	cap g `yv'_b = `yv' if year==year_elected-1
	sort multi_raceid `yv'_b
	bysort multi_raceid: replace `yv'_b = `yv'_b[1]

	cap g `yv'_D =`yv'-`yv'_b
	
	if $flag_balance ==1 {
		replace `yv'_D=-`yv'_D
	}
}

	* gen baseline and difference vars for vars with biannual outcomes 
	foreach yv in $y_behv {	// biannual outcome
	cap g `yv'_b1 = `yv' if year==year_elected-1 
	cap g `yv'_b2 = `yv' if year==year_elected-2 

	g `yv'_b=.
	sort multi_raceid `yv'_b1
	bysort multi_raceid: replace `yv'_b = `yv'_b1[1]

	sort multi_raceid `yv'_b2
	bysort multi_raceid: replace `yv'_b = `yv'_b2[1] if missing(`yv'_b)
	
	cap g `yv'_D =`yv'-`yv'_b
	if $flag_balance ==1 {
		replace `yv'_D=-`yv'_D
	}
}

	
	
	* year dummies and lags 
	tab year, g(yr_)
	egen id_election= group(leaid year_elected multi_raceid) // should be unique up to multi_raceid; multi_raceid is a string variable thus to be used for std err clustering, we need another numeric variable

	g year_lag = year-year_elected
	replace year_lag = year_lag+4
	cap g T = cond(vote_margin>=0,1,0) if !missing(vote_margin)


	*** 2/ Run RDs
	rdrobust bond_total_D vote_margin if year>=year_elected
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
		
	*** 3/ School Level Spending
	keep leaid year vote_margin year year_elected multi_raceid id_election yr_*
	ren leaid id_dist_nces
	destring id_dist_nces, replace
	
	*** 1) merge with school level finance data
	joinby id_dist_nces year using "outcomes_sch_finance.dta" //CRDC & ESSA & SFP (Fischer)
	// generated from 99_outcome_sch

	g pos_sfp = cond(fund_total_pp>0,1,0) if !missing(fund_total_pp)
	foreach y in hisp_high frl_high fund_total_pp_w pos_sfp {	 // annual variable
		g `y'_b = `y' if year==year_elected-1 
		sort multi_raceid id_sch_nces `y'_b
		bysort multi_raceid id_sch_nces: replace `y'_b = `y'_b[1]

		g `y'_D =`y'-`y'_b
	}

	foreach y in log_exp_per_stu_w exp_per_stu_w {	// biannual variable
		g `y'_b1 = `y' if year==year_elected-1 
		g `y'_b2 = `y' if year==year_elected-2 

		g `y'_b=.
		sort multi_raceid id_sch_nces `y'_b1
		bysort multi_raceid id_sch_nces: replace `y'_b = `y'_b1[1]

		sort multi_raceid id_sch_nces `y'_b2
		bysort multi_raceid id_sch_nces: replace `y'_b = `y'_b2[1] if missing(`y'_b)
		
		g `y'_D =`y'-`y'_b
	}

	*** 2) run regressions
	eststo clear
	foreach char in frl_high hisp_high {
		foreach y in fund_total_pp_w pos_sfp {	
		eststo: rdrobust `y'_D vote_margin if year>year_elected & `char'==1, p(1) kernel(triangular) bwselect(mserd) covs(yr_*) vce(nncluster id_election) weights(enrollment)
		local cl_l= e(ci_l_rb)
		local cl_h= e(ci_r_rb)
		local pval = e(pv_rb)
		local bw = e(h_l)
		local N_eff = e(N_h_l) + e(N_h_r)	
		local ymean =  e(beta_Y_p_l)[1,1]
		regsave using "Results/outcome_uni_`x'", append addlabel(y,"`y'", x,"`x'", cl_l, `cl_l', cl_h, `cl_h', pval, `pval', bw, `bw', N_eff, `N_eff', ymean, `ymean', subgroup, "`char'1") 
		eststo: rdrobust `y'_D vote_margin if year>year_elected & `char'==0, p(1) kernel(triangular) bwselect(mserd) covs(yr_*) vce(nncluster id_election) weights(enrollment)
		local cl_l= e(ci_l_rb)
		local cl_h= e(ci_r_rb)
		local pval = e(pv_rb)
		local bw = e(h_l)
		local N_eff = e(N_h_l) + e(N_h_r)	
		local ymean =  e(beta_Y_p_l)[1,1]
		regsave using "Results/outcome_uni_`x'", append addlabel(y,"`y'", x,"`x'", cl_l, `cl_l', cl_h, `cl_h', pval, `pval', bw, `bw', N_eff, `N_eff', ymean, `ymean' , subgroup, "`char'0") 
		}
	}
}

	* Step 1: Append all saved RD results 
	clear
	local files "budget_hawk equity_prior hisp"
	foreach x in `files' {
		append using Results/outcome_uni_`x'.dta
	}

	* Step 2: Keep and label variables
	keep x y coef cl_l cl_h N_eff subgroup

	* Step 3: Create display-ready strings
	gen coef_str = string(coef, "%6.3f")
	gen ci_str   = "[" + string(cl_l, "%6.3f") + ", " + string(cl_h, "%6.3f") + "]"
	gen N_str    = string(N_eff, "%9.0fc")

* Panel A: Equity Candidates
local eq_panel "equity_prior"
* Each entry: variable subgroup (use . for subgroup if not needed)
local eq_vars "pos_sfp frl_high1 pos_sfp frl_high0 pos_sfp hisp_high1 pos_sfp hisp_high0 test_pred . sup_change_cum ."

* Panel B: Fiscally Conservative Candidates
local fc_panel "budget_hawk"
local fc_vars "sal_inst_per_stu . sal_oth_per_stu . exp_capital_total_per_stu . exp_cap_const_per_stu . test_pred . sup_change_cum ."

* Panel C: Hispanic Candidates
local hi_panel "hisp"
local hi_vars "sal_inst_per_stu . sal_oth_per_stu . exp_capital_total_per_stu . exp_cap_const_per_stu . test_pred . sup_change_cum ."

file open myfile using "Results/Tex/outcome_mechanism_transposed.tex", write replace

file write myfile "\\begin{tabular}{@{}l*{6}{c}@{}}" _n
file write myfile "\\toprule \\hline" _n

* Panel A
file write myfile "\\multicolumn{7}{@{}l}{\\textit{\\textbf{Panel A: Equity Candidates}}} \\" _n
file write myfile "& \\multicolumn{4}{c}{SFP Funds to Schools with} & & \\cmidrule(l){2-5}" _n
file write myfile "& High ECD Share & Low ECD Share & High Hispanic Share & Low Hispanic Share & Predicted Test Scores & Super Turnover \\ & (1) & (2) & (3) & (4) & (5) & (6) \\" _n
file write myfile "\\cmidrule(l){2-2} \\cmidrule(l){3-3} \\cmidrule(l){4-4} \\cmidrule(l){5-5} \\cmidrule(l){6-6}  \\cmidrule(l){7-7}" _n

local eq_estimates ""
local eq_cis ""
local eq_ns ""
forvalues i = 1/6 {
    local var : word `=2*`i'-1' of `eq_vars'
    local sub : word `=2*`i'' of `eq_vars'
    preserve
    if "`sub'" == "." {
        keep if x == "`eq_panel'" & y == "`var'"
    }
    else {
        keep if x == "`eq_panel'" & y == "`var'" & subgroup == "`sub'"
    }
    local est = coef_str[1]
    local ci = ci_str[1]
    local n = N_str[1]
    restore
    local eq_estimates = "`eq_estimates' `est'"
    local eq_cis = "`eq_cis' `ci'"
    local eq_ns = "`eq_ns' `n'"
}
file write myfile "Estimate &`eq_estimates' \\" _n
file write myfile " &`eq_cis' \\" _n
file write myfile "N &`eq_ns' \\midrule" _n

* Panel B
file write myfile "\\multicolumn{7}{@{}l}{\\textit{\\textbf{Panel B: Fiscally Conservative Candidates}}} \\" _n
file write myfile "& \\multicolumn{4}{c}{Spending By Sub-Category} & & \\cmidrule(l){2-5}" _n
file write myfile "& Teacher Salaries & Staff Salaries & Total Capital & Construction & Predicted Test Scores & Super Turnover \\ & (1) & (2) & (3) & (4) & (5) & (6) \\" _n
file write myfile "\\cmidrule(l){2-2} \\cmidrule(l){3-3} \\cmidrule(l){4-4} \\cmidrule(l){5-5} \\cmidrule(l){6-6}  \\cmidrule(l){7-7}" _n

local fc_estimates ""
local fc_cis ""
local fc_ns ""
forvalues i = 1/6 {
    local var : word `=2*`i'-1' of `fc_vars'
    local sub : word `=2*`i'' of `fc_vars'
    preserve
    if "`sub'" == "." {
        keep if x == "`fc_panel'" & y == "`var'"
    }
    else {
        keep if x == "`fc_panel'" & y == "`var'" & subgroup == "`sub'"
    }
    local est = coef_str[1]
    local ci = ci_str[1]
    local n = N_str[1]
    restore
    local fc_estimates = "`fc_estimates' `est'"
    local fc_cis = "`fc_cis' `ci'"
    local fc_ns = "`fc_ns' `n'"
}
file write myfile "Estimate &`fc_estimates' \\" _n
file write myfile " &`fc_cis' \\" _n
file write myfile "N &`fc_ns' \\midrule" _n

* Panel C
file write myfile "\\multicolumn{7}{@{}l}{\\textit{\\textbf{Panel C: Hispanic Candidates}}} \\" _n
file write myfile "& \\multicolumn{4}{c}{Spending By Sub-Category} & & \\cmidrule(l){2-5}" _n
file write myfile "& Teacher Salaries & Staff Salaries & Total Capital & Construction & Predicted Test Scores & Super Turnover\\  & (1) & (2) & (3) & (4) & (5) & (6) \\" _n
file write myfile "\\cmidrule(l){2-2} \\cmidrule(l){3-3} \\cmidrule(l){4-4} \\cmidrule(l){5-5} \\cmidrule(l){6-6}  \\cmidrule(l){7-7}" _n

local hi_estimates ""
local hi_cis ""
local hi_ns ""
forvalues i = 1/6 {
    local var : word `=2*`i'-1' of `hi_vars'
    local sub : word `=2*`i'' of `hi_vars'
    preserve
    if "`sub'" == "." {
        keep if x == "`hi_panel'" & y == "`var'"
    }
    else {
        keep if x == "`hi_panel'" & y == "`var'" & subgroup == "`sub'"
    }
    local est = coef_str[1]
    local ci = ci_str[1]
    local n = N_str[1]
    restore
    local hi_estimates = "`hi_estimates' `est'"
    local hi_cis = "`hi_cis' `ci'"
    local hi_ns = "`hi_ns' `n'"
}
file write myfile "Estimate &`hi_estimates' \\" _n
file write myfile " &`hi_cis' \\" _n
file write myfile "N &`hi_ns' \\" _n

file write myfile "\\bottomrule" _n
file write myfile "\\end{tabular}" _n
file close myfile

********************************************************************************
* Export Original Table Format 
********************************************************************************

	* Step 3: Manually label outcomes for display
	gen y_label = ""
	replace y_label = "Teacher Salary"        if y == "sal_inst_per_stu"
	replace y_label = "Staff Salary"          if y == "sal_oth_per_stu"
	replace y_label = "Capital Spending"      if y == "exp_capital_total_per_stu"
	replace y_label = "Construction Spending" if y == "exp_cap_const_per_stu"

	replace y_label = "SFP: More-ECD"          if y == "pos_sfp" & subgroup == "frl_high1"
	replace y_label = "SFP: Less-ECD"          if y == "pos_sfp" & subgroup == "frl_high0"
	replace y_label = "SFP: More-Hispanic"     if y == "pos_sfp" & subgroup == "hisp_high1"
	replace y_label = "SFP: Less-Hispanic"     if y == "pos_sfp" & subgroup == "hisp_high0"
	replace y_label = "Test (Composition)"    if y == "test_pred"

	replace y_label = "New Superintendent"    if y == "sup_change_cum"
	replace y_label = "Total Spending"        if y == "exp_total_per_stu"
	replace y_label = "Test"                  if y == "test"
	replace y_label = "ECD Test"              if y == "ecd_test"
	replace y_label = "Hispanic Test"         if y == "hsp_test"

	* Step 4: Create display-ready strings for original format
	gen coef_str_orig = string(coef, "%4.3f")
	gen ci_str_orig   = "[" + string(cl_l, "%4.3f") + ", " + string(cl_h, "%4.3f") + "]"
	gen N_str_orig    = string(N_eff, "%9.0fc")

	* Step 5: Drop empty labels
	keep if y_label != ""

	* Step 6: Open LaTeX file for original format
	file open myfile using "Results/Tex/outcome_mechanism.tex", write replace

	file write myfile "\begin{tabular}{rrccc} \toprule \hline" _n
	file write myfile "Outcome & Estimate & Fiscal Conservatism & Equity & Hispanic \\\\ \midrule" _n
	

	file write myfile "\multicolumn{5}{c}{\textit{Panel A: Summary Measures}} \\\\" _n

	* Step 7: Write Summary Measures
	foreach label in "Total Spending" "Test" "ECD Test" "Hispanic Test" {
		foreach t in fc eq hi {
			preserve
			keep if x == cond("`t'"=="fc", "budget_hawk", cond("`t'"=="eq", "equity_prior", "hisp")) & y_label == "`label'"
			local `t'_est = coef_str_orig[1]
			local `t'_ci  = ci_str_orig[1]
			local `t'_N   = N_str_orig[1]
			restore
		}
		file write myfile "`label' & Estimate & `fc_est' & `eq_est' & `hi_est' \\\\" _n
		file write myfile " & \$[CI]\$ & `fc_ci' & `eq_ci' & `hi_ci' \\\\" _n
		file write myfile " & N & `fc_N' & `eq_N' & `hi_N' \\\\" _n
	}

		* Step 8: Mechanism
	file write myfile "\multicolumn{5}{c}{\textit{Panel B: Mechanism}} \\\\" _n

	foreach label in ///
		"Teacher Salary" "Staff Salary" "Capital Spending" "Construction Spending" ///
		"SFP: More-ECD" "SFP: Less-ECD" "SFP: More-Hispanic" "SFP: Less-Hispanic" ///
		"New Superintendent" "Test (Composition)" {
			
		foreach t in fc eq hi {
			preserve
			keep if x == cond("`t'"=="fc", "budget_hawk", cond("`t'"=="eq", "equity_prior", "hisp")) & y_label == "`label'"
			local `t'_est = coef_str_orig[1]
			local `t'_ci  = ci_str_orig[1]
			local `t'_N   = N_str_orig[1]
			restore
		}
		file write myfile "`label' & Estimate & `fc_est' & `eq_est' & `hi_est' \\\\" _n
		file write myfile " & \$[CI]\$ & `fc_ci' & `eq_ci' & `hi_ci' \\\\" _n
		file write myfile " & N & `fc_N' & `eq_N' & `hi_N' \\\\" _n
	}

	file write myfile "\hline \bottomrule" _n
	file write myfile "\end{tabular}" _n
	file close myfile
