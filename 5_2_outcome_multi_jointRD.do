********************************************************************************
* Joint Testing of Identity-related H0s in Prior Papers
* Written By Minseon Park 05/09/25
* I. Define RD Function
* II. A table with a few examples
********************************************************************************

version 18.5
set seed 1234
clear all
eststo clear

* set directory 
cd "~/Library/CloudStorage/Dropbox/California Election Data/Code"
global logpath "~/Library/CloudStorage/Dropbox/California Election Data/Logs"

global y_rev ="rev_cte_per_stu"
global y_fiscal= "exp_total_per_stu exp_capital_total_per_stu bond_amount_per_stu"
global y_teacher = "teacher_exit_dist teacher_tenure entry_sal_Bcomp_log ba60_sal_Bcomp_log"
global y_score = "hsp_test ecd_test test"
global y_enroll = "log_enrollment"
global y_behv = "sus_share"
global y_charter = "enrollment_charter_share n_charter_sch"
global y_sup = "sup_change_cum"

global class "entry ba60"

global missing ="with_missing"
global flag_balance=0
global flag_bw=0 // do we want to force bw to be the same across specifications?

global list_demo = "hisp female occ_teacher democrat_v2"
global tab_opt = "b(a2) se(2) scalars(N r2) label nostar"


********************************************************************************
* I. Define RD Function
********************************************************************************
	
capture program drop big_rd   
program define big_rd
		syntax, yvar(string) yearcond(string) treatment(string) kernel(string) [sch_level(integer 0)]

	foreach x in `treatment' {
	* generate treatment indicator
	gen T_`x' = cond(m_`x'>=0, 1, 0) if !missing(m_`x')

	* run RD and get bandwidth
	if `sch_level'~=1 {
		qui rdrobust `yvar'_D m_`x' if `yearcond', p(1) kernel(`kernel') bwselect(mserd) covs(yr_*)  vce(nncluster id_election)
	   local bw = e(h_l)
	   estadd scalar N_eff = e(N_h_l) + e(N_h_r)
	}
	if `sch_level'==1 {
		qui rdrobust `yvar'_D m_`x' if `yearcond', p(1) kernel(`kernel') bwselect(mserd) covs(yr_*) vce(hc0) weights(enrollment)
		local bw = e(h_l)
		estadd scalar N_eff = e(N_h_l) + e(N_h_r)
	}

	* generate sample and weight vars
	gen S_`x' = cond(abs(m_`x')<=`bw', 1, 0)
	gen w_`x' = `bw' - abs(m_`x') if abs(m_`x') <= `bw'
	}
	
	if $flag_bw ==1 {
		local bw=$bw
		foreach x in `treatment' {
			replace S_`x' = cond(abs(m_`x')<`bw',1,0)
			replace w_`x' = `bw' - abs(m_`x') if abs(m_`x') <= `bw'
		}
	}

	
	g s=0
	foreach x in `treatment' {
		replace s=1 if  S_`x'==1
	}
	
	foreach x in `treatment' {
		gen T2_`x' = cond(missing(T_`x'),0,T_`x')
		gen m2_`x' = cond(missing(m_`x'),0,m_`x')
		gen T2_`x'_S = T2_`x' * S_`x' if s==1
		gen m2_`x'_S = m2_`x' * S_`x' if s==1
	}
	drop s

	// Generate spec automatically from treatment
    
    local gr_spec ""
    foreach var of local treatment {
		g int_`var' = T2_`var'_S*m2_`var'_S
		local gr_spec "`gr_spec' S_`var'"
    }
    
	egen group = group(`gr_spec') 
	display "`gr_spec'"
	if `sch_level'==1 {
		drop group
		egen group = group(`gr_spec' hisp_high)

	}	
end


********************************************************************************
* II. A table with a few examples
********************************************************************************

	use "data_for_rd/$missing/dist_stacked.dta", clear
	g log_enrollment = log(enrollment)
	ren *vocational* *cte*
	
	do 4_f_merge_tsal
	
	foreach y in $y_rev $y_fiscal $y_score $y_teacher $y_enroll $y_charter {	
		cap g `y'_b = `y' if year==year_elected-1 
		sort multi_raceid `y'_b
		bysort multi_raceid: replace `y'_b = `y'_b[1]

		cap g `y'_D =`y'-`y'_b
		cap g `y'_Dn = -`y'_D
	}

	foreach y in $y_behv {	// biannual outcome from CRDC
		cap g `y'_b1 = `y' if year==year_elected-1 
		cap g `y'_b2 = `y' if year==year_elected-2 

		g `y'_b=.
		sort multi_raceid `y'_b1
		bysort multi_raceid: replace `y'_b = `y'_b1[1]

		sort multi_raceid `y'_b2
		bysort multi_raceid: replace `y'_b = `y'_b2[1] if missing(`y'_b)
		
		cap g `y'_D =`y'-`y'_b
		cap g `y'_Dn = -`y'_D
	}

	tab year, g(yr_)
	egen id_election= group(id_district_nces year_elected) // should be unique up to multi_raceid; multi_raceid is a string variable thus to be used for std err clustering, we need another numeric variable
	
	egen multi_raceid_num = group(multi_raceid)
	xtset multi_raceid_num year
	keep if year>=year_elected
	tsegen double sup_change_cum_D = rowmax(L(0/4).sup_change) // or rowtotal
	
	preserve

	* -------------------------------------------
	* 0. Setup and RD preparation using big_rd
	* -------------------------------------------
	eststo clear
	local ylist "exp_total_per_stu ecd_test"
	local kernel "uni"
	local yearcond1 "year>=year_elected & year<=2017"
	local yearcond2 "year>=year_elected"

	* Run big_rd for each outcome and rename generated variables for later stacking
	local i = 1
	foreach y in `ylist' {
		local ycond = cond("`y'"=="exp_total_per_stu", "`yearcond1'", "`yearcond2'")
		local treat = cond("`y'"=="exp_total_per_stu", "budget_hawk $list_demo", "equity_prior $list_demo")

		big_rd, yvar("`y'") yearcond("`ycond'") treatment("`treat'") kernel("`kernel'")

		* Standardize variable names for stacking
		foreach prefix in T2 m2 int group {
			rename `prefix'* eq`i'_`prefix'*
		}
		drop T* S* w*
		local ++i
	}

	* -------------------------------------------
	* 1. Stack data for joint regression
	* -------------------------------------------
	keep exp_total_per_stu_D ecd_test_D eq* year year_elected id_election 
	gen id = _n
	ren eq1_* *1
	ren eq2_* *2

	* Long reshape for stacking across equations
	reshape long T2_budget_hawk_S T2_equity_prior_S T2_hisp_S T2_female_S T2_occ_teacher_S T2_democrat_v2_S ///
				 m2_budget_hawk_S m2_equity_prior_S m2_hisp_S m2_female_S m2_occ_teacher_S m2_democrat_v2_S ///
				 int_budget_hawk int_equity_prior int_hisp int_female int_occ_teacher int_democrat_v2 ///
				 group, i(id) j(eq)

	* -------------------------------------------
	* 2. Label predictors for table output
	* -------------------------------------------
	label var T2_budget_hawk_S "Fiscal Conservatism"
	label var T2_equity_prior_S "Equity"
	label var T2_hisp_S "Hispanic"
	label var T2_female_S "Female"
	label var T2_occ_teacher_S "Teacher"
	label var T2_democrat_v2_S "Democrat"

	* -------------------------------------------
	* 3. Estimate stacked regressions for each outcome
	* -------------------------------------------
	global iden = "T2_hisp_S T2_female_S T2_occ_teacher_S T2_democrat_v2_S m2_hisp_S m2_female_S m2_occ_teacher_S m2_democrat_v2_S int_hisp int_female int_occ_teacher int_democrat_v2"

	eststo clear
	eststo: reg exp_total_per_stu_D *budget_hawk_S $iden ib(2016).year i.group if year>=year_elected & year<=2017 & eq==1, r cl(id_election)
	eststo: reg ecd_test_D *equity_prior_S $iden ib(2016).year i.group if year>=year_elected & eq==2, r cl(id_election)

	* -------------------------------------------
	* 4. Export coefficient table
	* -------------------------------------------
	global tab_opt = "b(a3) se(3) scalars(N r2) label nostar"

	esttab using "Results/Tex/outcome_joint_body.tex", ///
		keep(*T2*_S) $tab_opt ///
		replace ///
		fragment ///
		mtitles("Spending per pupil (\$1,000)" "ECD Test Scores") ///
		title("") ///
		numbers ///
		compress ///
		nogaps ///
		nonotes ///
		booktabs ///
		order(*budget_hawk* *equity*) ///
		stats(N r2, label("N" "R2"))

	* -------------------------------------------
	* 5. Run suest and joint tests
	* -------------------------------------------
	eststo clear
	eststo: reg exp_total_per_stu_D *budget_hawk_S $iden ib(2016).year i.group if year>=year_elected & year<=2017 & eq==1
	eststo: reg ecd_test_D *equity_prior_S $iden ib(2016).year i.group if year>=year_elected & eq==2

	estimates dir
	local all_models `r(names)'
	suest `all_models', vce(cl id_election)

	* Joint test across models: Identity variables
	test [est1_mean]T2_hisp_S = [est2_mean]T2_hisp_S = ///
		[est1_mean]T2_female_S = [est2_mean]T2_female_S  = ///
		[est1_mean]T2_occ_teacher_S = [est2_mean]T2_occ_teacher_S  = ///
		[est1_mean]T2_democrat_v2_S = [est2_mean]T2_democrat_v2_S 

	scalar chi_iden = r(chi2)
	scalar p_iden = r(p)
	scalar df_iden = r(df)
	
	* Joint test across models: Ideology variables
	test [est1_mean]T2_budget_hawk_S = [est2_mean]T2_equity_prior_S 

	scalar chi_ideo = r(chi2)
	scalar p_ideo = r(p)
	scalar df_ideo = r(df)

	* -------------------------------------------
	* 6. Final LaTeX table with joint test results
	* -------------------------------------------

	file open myfile using "Results/Tex/outcome_joint.tex", write replace

	file write myfile "\input{../Results/Tex/outcome_joint_body.tex} \\\\" _n
	file write myfile "\multicolumn{3}{l}{\textit{Joint Tests Results}} \\\\" _n

	local chi_iden_str = string(chi_iden, "%4.3f")
	local p_iden_str   = string(p_iden, "%4.3f")
	local df_iden_str  = string(df_iden, "%1.0f")

	local chi_ideo_str = string(chi_ideo, "%4.3f")
	local p_ideo_str   = string(p_ideo, "%4.3f")
	local df_ideo_str  = string(df_ideo, "%1.0f")

	file write myfile "\multicolumn{3}{c}{\textbf{H\textsubscript{0}}: Identities jointly zero \hfill $\chi^2(`df_iden_str') = `chi_iden_str',\ p = `p_iden_str'$} \\\\" _n
	file write myfile "\multicolumn{3}{c}{\textbf{H\textsubscript{0}}: Ideologies jointly zero \hfill $\chi^2(`df_ideo_str') = `chi_ideo_str',\ p = `p_ideo_str'$} \\\\" _n

	file close myfile


********************************************************************************
* IV. Joint testing across hypothesis types
********************************************************************************

	file open myfile using "Results/Tex/joint_test_summary.tex", write replace

	file write myfile "\begin{tabular}{lcccc}" _n
	file write myfile "\toprule\midrule" _n
	file write myfile " & \multicolumn{2}{c}{\textbf{H\textsubscript{0}}: Identities jointly zero} & \multicolumn{2}{c}{\textbf{H\textsubscript{0}}: Ideologies jointly zero} \\" _n
	file write myfile " & (p-value)  & (p-value) \\" _n
	file write myfile " & (1)  & (2) \\" _n
	file write myfile "\midrule" _n

	* Panel A: Identity and Ideology
	file write myfile "\multicolumn{5}{c}{Panel A: Identity and Ideology Variables Tested Simultaneously} \\\\" _n

	foreach htype in 1 2 3 {
		if `htype' == 1 global h0_type "h_type_exact==1"
		if `htype' == 2 global h0_type "h_type_exact==2"
		if `htype' == 3 global h0_type "h_type_exact<."

		local label = cond(`htype'==1, "Examined in Prior Studies", ///
						   cond(`htype'==2, "Augmented Using Ideological Differences", "All Identities and Ideologies"))

		global flag_iden_only = 0
		ereturn clear
		estimates clear
		eststo clear

		do 5_f_joint_RD_testing.do

		local chi_iden_str = "\$\chi^2$(`=string(df_iden)') = `=string(chi_iden,"%4.2f")'"
		local chi_ideo_str = "\$\chi^2$(`=string(df_ideo)') = `=string(chi_ideo,"%4.2f")'"
		local p_iden_str = string(p_iden,"%4.3f")
		local p_ideo_str = string(p_ideo,"%4.3f")

		file write myfile "`label'" ///
			" & `p_iden_str'" ///
			" & `p_ideo_str' \\\\" _n
	}

	file write myfile "\multicolumn{5}{c}{Panel B: Identity and Ideology Variables Tested Separately} \\" _n

	* Panel B: Identity or Ideology
	foreach htype in 1 2 3 {
		if `htype' == 1 global h0_type "h_type_exact==1"
		if `htype' == 2 global h0_type "h_type_exact==2"
		if `htype' == 3 global h0_type "h_type_exact<."

		local label = cond(`htype'==1, "Examined in Prior Studies", ///
						   cond(`htype'==2, "Augmented Using Ideological Differences", "All Identities and Ideologies"))

		foreach andflag in 1 2 {
			global flag_iden_only = `andflag'
			ereturn clear
			estimates clear
			eststo clear

			do 5_f_joint_RD_testing.do
		}
			local chi_iden_str = "\$\chi^2$(`=string(df_iden)') = `=string(chi_iden,"%4.2f")'"
			local chi_ideo_str = "\$\chi^2$(`=string(df_ideo)') = `=string(chi_ideo,"%4.2f")'"
			local p_iden_str = string(p_iden,"%4.3f")
			local p_ideo_str = string(p_ideo,"%4.3f")

			file write myfile "`label'" ///
				"  & `p_iden_str'" ///
				"  & `p_ideo_str' \\\\" _n
	}

	file write myfile "\bottomrule" _n
	file write myfile "\end{tabular}" _n
	file close myfile
