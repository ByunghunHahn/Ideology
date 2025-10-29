********************************************************************************
* Generate Summary Table for Priorities 
* Written by Minseon Park 11/17/24
* Latest updated by Minseon Park 05/26/25
********************************************************************************

	set more off
	set matsize 5000
	set emptycells drop
	
	* install commands 
	* ssc install rforest

	* set directory 
	cd "C:\Users\hahn0\Desktop\Hahn_Park\Code"
	global logpath "New_Logs"
	global grpath ="Results/New_Graph"
	global tab_opt = "dec(2) pdec(2) label"

	global flag_close = 0 // 0: all 1: close elctions

	global list_prio ="equity_prior budget_hawk agenda_bias parent_involvement cte_prior dropout_prior enrollment_prior facility_prior safety_health_prior sup_concern teacher_care score_concern"

	global list_demo = "hisp female occ_teacher democrat_v2"

	global missing = "with_missing" // "with_missing" or "no_missing"


********************************************************************************
* I. Selection into Platform 
********************************************************************************

	* Load data
	if $flag_close==0 {
		use "candidates_all_$missing.dta", clear 
	}
	if $flag_close==1 {
		use "candidates_close_election_$missing.dta", clear 
	}

	* Election-level vars
	bysort multi_raceid: egen max_lose_share = max(vote_share*(win_status==2))
	bysort multi_raceid: egen min_win_share = min(exp(log(vote_share*(win_status==1))))
	gen win_lose_diff = min_win_share - max_lose_share
	replace win_lose_diff = . if uncontested == 1

	gen win = win_status == 1
	bysort multi_raceid: egen sum_win = total(win)
	gen full_term = strpos(term1, "full") > 0

	replace year = year - 1
	destring leaid, replace
	merge m:1 leaid year using "outcomes.dta", gen(_platform) force keep(master matched)

	* Label dictionary
	label variable valid_platform "Platform = 1"
	label variable female "Female"
	label variable hisp "Hispanic"
	label variable occ_teacher "Teacher"
	label variable democrat "Democrat"
	label variable n_candidates "N Candidates/election"
	label variable sum_win "N Positions/election"
	label variable win_lose_diff "Vote Margin ($\Delta$ Vote Share, Least Popular Winner vs. Most Popular Loser)"
	label variable enrollment "Enrollment"
	label variable enrollment_white_share "Share White"
	label variable enrollment_hisp_share "Share Hispanic"
	label variable enrollment_black_share "Share Black"
	label variable enrollment_asian_share "Share Asian"
	label variable exp_total_per_stu "Spending Per Pupil (\$1k)"

	global list_cand = "female hisp occ_teacher democrat"
	global list_election = "n_candidates sum_win win_lose_diff"
	global list_dist = "enrollment enrollment_white_share enrollment_hisp_share enrollment_black_share enrollment_asian_share exp_total_per_stu"

	* Open file
	file open myfile using "Results/Tex/summary_with_platform.tex", write replace

	* Header
	file write myfile "\begin{tabular}{lcc}" _n
	file write myfile "\toprule \hline" _n
	file write myfile "& No Platform & Yes Platform \\\\ \midrule" _n
	file write myfile "& (1) & (2)  \\\\" _n
	* Panel A
	file write myfile "\multicolumn{3}{l}{\textit{\textbf{Panel A: Candidate Characteristics}}} \\\\" _n
	foreach y in $list_cand {
		quietly ttest `y', by(valid_platform)
		local h1 = string(r(mu_1), "%4.2f")
		local h2 = string(r(mu_2), "%4.2f")
		if r(p)<0.05 {
			local h1 = "\textbf{`h1'}"
			local h2 = "\textbf{`h2'}"
		}
		local rowlabel : variable label `y'
		file write myfile "`rowlabel' & `h1' & `h2' \\\\" _n
	}
	file write myfile "& & \\" _n
	* Panel B
	file write myfile "\multicolumn{3}{l}{\textit{\textbf{Panel B: Election Characteristics}}} \\\\" _n
	foreach y in $list_election {
		quietly ttest `y', by(valid_platform)
		local h1 = string(r(mu_1), "%4.2f")
		local h2 = string(r(mu_2), "%4.2f")
		if r(p)<0.05 {
			local h1 = "\textbf{`h1'}"
			local h2 = "\textbf{`h2'}"
		}
		local rowlabel : variable label `y'
		file write myfile "`rowlabel' & `h1' & `h2' \\\\" _n
	}
	file write myfile "& & \\" _n
	* Panel C
	file write myfile "\multicolumn{3}{l}{\textit{\textbf{Panel C: District Characteristics}}} \\\\" _n
	foreach y in $list_dist {
		quietly ttest `y', by(valid_platform)
		local h1 = string(r(mu_1), "%4.2f")
		local h2 = string(r(mu_2), "%4.2f")
		if r(p)<0.05 {
			local h1 = "\textbf{`h1'}"
			local h2 = "\textbf{`h2'}"
		}
		local rowlabel : variable label `y'
		file write myfile "`rowlabel' & `h1' & `h2' \\\\" _n
	}

	* Ns
	count if valid_platform==0
	local N1 = string(r(N))
	count if valid_platform==1
	local N2 = string(r(N))
	file write myfile "\\\\ Total N Candidates & `N1' & `N2' \\\\" _n

	* Footer
	file write myfile "\hline \bottomrule" _n
	file write myfile "\end{tabular}" _n
	file close myfile


********************************************************************************
* II. T-test by Group 
********************************************************************************

	* Load data
	if $flag_close==0 {
		use "candidates_all_$missing.dta", clear 
	}
	if $flag_close==1 {
		use "candidates_close_election_$missing.dta", clear 
	}	

	* Clean up priorities
	order $list_prio
	foreach y in $list_prio top3_leng {
		replace `y' = . if valid_platform != 1
	}

	lab var cte_prior "Career and Technical Education" 
	lab var sup_concern "Superintendent Accountability" 
	lab var score_concern "Test Scores" 

	* Open file
	file open myfile using "Results/Tex/summ_prio.tex", write replace

	* Header
	file write myfile "\begin{tabular}{lcccccccccc}" _n
	file write myfile "\toprule \midrule" _n
	file write myfile "& \multicolumn{8}{c}{Share w/Priority by Demographic Group} & \multicolumn{2}{c}{Prediction Accuracy} \\\\ \cmidrule(lr{0.5cm}){2-9}  \cmidrule(l{0.5cm}){10-11}" _n
	file write myfile "& \multicolumn{2}{c}{Hispanicity} & \multicolumn{2}{c}{Gender} & \multicolumn{2}{c}{Occupation} & \multicolumn{2}{c}{Political Affiliation} & R-forest & Mean Only \\\\ \cmidrule(lr{0.5cm}){2-3} \cmidrule(lr{0.5cm}){4-5} \cmidrule(lr{0.5cm}){6-7} \cmidrule(l{0.5cm}){8-9}" _n
	file write myfile "& Hispanic & Non- & Female & Male & Teacher & Non- & Democrat & Republican & & \\\\" _n
	file write myfile "& (1) & (2) & (3) & (4) & (5) & (6) & (7) & (8) & (9) & (10) \\\\ \midrule" _n

	* N row
	quietly ttest equity_prior, by(hisp)
	local N_h1 = r(N_1)
	local N_h2 = r(N_2)

	quietly ttest equity_prior, by(female)
	local N_g1 = r(N_1)
	local N_g2 = r(N_2)

	quietly ttest equity_prior, by(occ_teacher)
	local N_t1 = r(N_1)
	local N_t2 = r(N_2)

	quietly ttest equity_prior, by(democrat_v2)
	local N_d1 = r(N_1)
	local N_d2 = r(N_2)

	file write myfile "N Candidates & `N_h1' & `N_h2' & `N_g1' & `N_g2' & `N_t1' & `N_t2' & `N_d1' & `N_d2' & & \\\\" _n

	* Row: Share with Platform (outcome = valid_platform)
	foreach demo in hisp female occ_teacher democrat_v2 {
		quietly ttest valid_platform, by(`demo')
		local `demo'_1 = string(r(mu_1), "%4.2f")
		local `demo'_2 = string(r(mu_2), "%4.2f")
		if r(p) < 0.05 {
			local `demo'_1 = "\textbf{``demo'_1'}"
			local `demo'_2 = "\textbf{``demo'_2'}"
		}
	}

	rforest valid_platform hisp female occ_teacher democrat_v2 , type(class) seed(123) iterations(10)
	local rf_oob = string(round(1 - e(OOB_Error), 0.01), "%4.2f")
	su valid_platform
	local mean_oob = string(r(mean)^2 + (1 - r(mean))^2, "%4.2f")

	local label : variable label valid_platform
	file write myfile "`label' & `hisp_2' & `hisp_1' & `female_2' & `female_1' & `occ_teacher_2' & `occ_teacher_1' & `democrat_v2_2' & `democrat_v2_1' & `rf_oob' & `mean_oob' \\\\" _n

	* Section header for priorities
	file write myfile "\multicolumn{9}{l}{\textit{Share with Priority Among Candidates with Valid Platform}} \\\\" _n

	* Restrict to valid platform
	keep if valid_platform == 1

	* Loop over priorities
	foreach y in $list_prio {
		foreach demo in hisp female occ_teacher democrat_v2 {
			quietly ttest `y', by(`demo')
			local `demo'_1 = string(r(mu_1), "%4.2f")
			local `demo'_2 = string(r(mu_2), "%4.2f")
			if r(p) < 0.05 {
				local `demo'_1 = "\textbf{``demo'_1'}"
				local `demo'_2 = "\textbf{``demo'_2'}"
			}
		}

		rforest `y' hisp female occ_teacher democrat_v2 , type(class) seed(123) iterations(10)
		local rf_oob = string(round(1 - e(OOB_Error), 0.01), "%4.2f")
		su `y'
		local mean_oob = string(r(mean)^2 + (1 - r(mean))^2, "%4.2f")

			

		local label : variable label `y'

		file write myfile "\hspace{1em}`label' & `hisp_2' & `hisp_1' & `female_2' & `female_1' & `occ_teacher_2' & `occ_teacher_1' & `democrat_v2_2' & `democrat_v2_1' & `rf_oob' & `mean_oob' \\\\" _n
	}

	* End table
	file write myfile "\bottomrule" _n
	file write myfile "\end{tabular}" _n
	file close myfile
	
	
********************************************************************************
* III. Generate Rforest Graph
********************************************************************************

	use "candidates_all_$missing.dta", clear 

	foreach y in $list_prio {
		local `y'_label : variable label `y'
		rforest `y' hisp female occ_teacher democrat_v2 if valid_platform==1, type(class) seed(123) iterations(10)
		matrix `y' = e(importance)
		scalar `y'_oob = round(1 - e(OOB_Error), 0.01)
		
		su `y' if valid_platform==1
		scalar `y'_coin = r(mean)^2 + (1-r(mean))^2
	}
	
	recode valid_platform (.=0)
	rforest valid_platform hisp female occ_teacher democrat_v2, type(class) seed(123) iterations(10)
	matrix valid_platform = e(importance)
	scalar valid_platform_oob = round(1 - e(OOB_Error), 0.01)
	
	su valid_platform
	scalar valid_platform_coin = r(mean)^2 + (1-r(mean))^2
	local valid_platform_label : variable label valid_platform
	
	*-- Step 1: Collect results into a dataset
	clear
	tempfile accuracies
	save `accuracies', emptyok

	* Start with an empty dataset
	clear
	set obs 0

	* Define variables only once, before the loop
	gen str20 varname = ""
	gen oob = .
	gen coin = .

	* Now, in the loop, just add a new observation and fill in values
	foreach y in $list_prio valid_platform {
    set obs `=_N+1'
    replace varname = "``y'_label'" in L
    replace oob     = `y'_oob in L
    replace coin    = `y'_coin in L
}

	*-- Keep only the relevant variables and observations
	keep varname oob coin
	drop if missing(oob)

	*-- Save the dataset for plotting
	save "Results/accuracy_comparison.dta", replace

	use "Results/accuracy_comparison.dta", clear
	gen bar_order = _n   // Creates a variable with values 1,2,3,... for each row
	gen oob_gain = oob - coin

	* Find out the bar_order for "Have a Platform"
	list varname bar_order if varname=="Have a Platform"

	* Suppose "Have a Platform" is at bar_order==7, so you want to insert at bar_order==7

	* Add a blank row at bar_order==7
	set obs `=_N+1'
	replace varname = " " in L   // A string of one space
	replace bar_order = 13 in L
	replace coin = . in L
	replace oob_gain = . in L

	* Now increment bar_order by 1 for "Have a Platform" and any bars after it
	replace bar_order = bar_order + 1 if bar_order >= 13 & varname != " "

	*-- Step 3: Draw the stacked bar graph	
	graph bar coin oob_gain, over(varname, sort(bar_order) label(angle(45))) stack horizontal ///
		legend(label(1 "Guess only with Mean") label(2 "Gain with Identities") pos(6) col(2)) ///
		ytitle("Classification Accuracy") ///
		bar(1, color(ebblue)) bar(2, color(ebblue)) ///
		ylabel(0(0.2)1)
	graph export "Results/Graph/accuracy_bar.png", replace
	
	graph bar coin oob_gain, over(varname, sort(bar_order) label(angle(45))) stack horizontal ///
		legend(label(1 "Guess only with Mean") label(2 "Gain with Identities") pos(6) col(2)) ///
		ytitle("Classification Accuracy") ///
		bar(1, color(gs10)) bar(2, color(ebblue)) ///
		ylabel(0(0.2)1)
	graph export "Results/Graph/accuracy_bar_1.png", replace
		
	
********************************************************************************
* IV. Generate Mean Difference Graph 
********************************************************************************

*** 1) Save all results first
	use "candidates_all_$missing.dta" if offedu==0, clear 
	keep if valid_platform==1 

	global tab_opt = "b(a2) se(2) scalars(N r2) label star(+ 0.1 * 0.05 ** 0.01)"

	preserve 
	clear
	set obs 1
	gen x=""
	save Results/bar_reg, replace
	restore

	eststo clear
	foreach y in $list_prio {
	foreach x in $list_demo {
		ttest `y', by(`x')
		local mu1 = r(mu_1)
		local mu2 = r(mu_2)
		local low1 = r(mu_1) - 1.96*r(sd_1)/sqrt(r(N_1))
		local high1 = r(mu_1) + 1.96*r(sd_1)/sqrt(r(N_1))
		local low2 = r(mu_2) - 1.96*r(sd_2)/sqrt(r(N_2))
		local high2 = r(mu_2) + 1.96*r(sd_2)/sqrt(r(N_2))
		local pvalue1 = r(p)
		local pvalue2 = r(p)
		
		eststo: reg `y' `x', vce(robust)
		local mu3 = _b[`x']
		local low3 = `mu3' - 1.96*_se[`x']
		local high3 = `mu3' + 1.96*_se[`x']
		local pvalue3 =  2*ttail(e(df_r), abs(_b[`x']/_se[`x']))
		
		eststo: reghdfe `y' `x', absorb(multi_raceid) vce(robust)
		local mu4 =  _b[`x']
		local low4 = `mu4' - 1.96*_se[`x']
		local high4 = `mu4' + 1.96*_se[`x']
		local pvalue4 =  2*ttail(e(df_r), abs(_b[`x']/_se[`x']))
		
		preserve
		clear
		set obs 1
		gen demo = "`x'"
		g prio = "`y'"
		forvalues t=1/4 {
			gen mean`t' = `mu`t''
			gen low`t' = `low`t''
			gen high`t' = `high`t''
			gen pvalue`t' = `pvalue`t''		
		}
		append using Results/bar_reg.dta, force
		save Results/bar_reg, replace
		restore
	}
	}


*** 2) Graphs
	use Results/bar_reg, clear
	egen x_num = group(demo prio) 
	reshape long mean low high pvalue, i(x_num) j(n)

	set scheme white_ptol

	gen stars = ""
	replace stars = "+" if pvalue < 0.1
	replace stars = "*" if pvalue < 0.05
	replace stars = "**" if pvalue < 0.01

	drop if pvalue==.
	g n2=n
	replace n2= n2+2.5 if demo~="hisp"
	replace n2= n2+2.5 if demo~="hisp" & demo~="female"
	replace n2= n2+2.5 if demo~="hisp" & demo~="female" & demo~="occ_teacher"

	g ylabel = "Fiscal Conservatism" if prio=="budget_hawk" 
	replace ylabel="Equity" if prio=="equity_prior"
	replace ylabel="Teacher Support" if prio=="teacher_care"
	replace ylabel="Safety" if prio=="safety_health_prior"
	replace ylabel="Facility Improvement" if prio=="facility_prior"
	replace ylabel="Enrollment" if prio=="enrollment_prior"
	replace ylabel="Dropout" if prio=="dropout_prior"
	replace ylabel="Career and Technical Education" if prio=="cte_prior"
	replace ylabel="Academic Achievement" if prio=="academic_prior"
	replace ylabel="Community Engagement" if prio=="parent_involvement_prior"
	replace ylabel="Agenda Bias" if prio=="agenda_bias"
	replace ylabel="Superintendent Accountability" if prio=="sup_concern"
	replace ylabel="Scorecard Concern" if prio=="score_concern"

	*** Raw means by identity groups
	foreach y in $list_prio {
	preserve
	keep if prio=="`y'"
	summarize ylabel, meanonly
	local subtitle = ylabel[1]

	twoway (bar mean n2 if n<=2, color(ebblue)) (rcap high low n2 if n<=2, lw(thick) lc(dkorange)) ///
			(scatter mean n2 if n<=2, mlabel(mean) mlabposition(6) mlabgap(2)  mlabsize(medium) mlabformat(%9.2f)) ///
			(scatter mean n2 if n==2, mlabel(stars) mlabposition(11) mlabsize(medlarge)), yscale(range(0 0.5)) ylabel(0(0.2)1) ///
		   legend(off) xlabel(2 "Hispanic" 1 "NonHispanic" 4.5 "Female" 3.5 "Male" 7 "Teacher" 6 "NonTeacher" 9.5 "Democrat" 8.5 "Republican", labsize(med) angle(45)) xtitle("") ///
		   ytitle("") ylabel(,labsize(medlarge))  plotregion(margin(zero)) subtitle(`subtitle', pos(12) ring(.8) bcolor(none))  ///
		   saving("$graph/bar_`y'", replace)   
	restore	
	}

	graph combine "$graph/bar_equity_prior.gph" 
	gr export "$graph/bar_equity_prior.png", replace
	graph combine "$graph/bar_budget_hawk.gph"
	gr export "$graph/bar_budget_hawk.png", replace
	
	foreach y in $list_prio { 
	preserve

	keep if prio=="`y'"
	summarize ylabel, meanonly
	local subtitle = ylabel[1]

	twoway (bar mean n2 if n==3, color(ebblue)) (scatter mean n2 if n==3, color(none) mlabel(stars) mlabposition(11) mlabsize(medlarge)), ylabel(-.25(0.25).5) xlabel(1 " " 2 "Hispanic" 4.5 "Female" 7 "Teacher" 9.5 "Democrat" 11 " ", labsize(med) angle(45)) legend(off) ytitle("") xtitle("") subtitle("`subtitle'", pos(12) ring(0) bcolor(none)) saving("$graph/bar_`y'", replace)
	restore
	}

	gr combine "$graph/bar_budget_hawk" "$graph/bar_equity_prior" "$graph/bar_safety_health_prior" "$graph/bar_teacher_care" "$graph/bar_dropout_prior" "$graph/bar_facility_prior"  "$graph/bar_cte_prior" "$graph/bar_enrollment_prior"  "$graph/bar_academic_prior" "$graph/bar_agenda_bias" "$graph/bar_parent_involvement_prior", imargin(zero) xsize(10) ysize(5) iscale(0.7)
	gr export "$graph/bar_priority_reg.png", replace	

	