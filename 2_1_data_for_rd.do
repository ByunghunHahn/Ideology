********************************************************************************
* Merge outcome - election file for RD analysis 
* Written by Minseon Park 05/23/2024
* Last updated 04/29/25
* I. Priority into Candidate Characteristics
* II. Close Election
* III. First Stage
* IV. District x Year with Multiple Elections
* V. Merge with Outcome
* VI. Merge aross Data Sets for Joint RDs
********************************************************************************

*** Setup
set more off
set matsize 5000
set emptycells drop

cd "C:\Users\hahn0\Desktop\Hahn_Park\Code"
global logpath "New_Logs"

global list_demo = "hisp female occ_teacher democrat_v2"
global list_prio ="equity_prior budget_hawk agenda_bias parent_involvement cte_prior dropout_prior enrollment_prior facility_prior safety_health_prior teacher_care sup_concern score_concern"

global tab_opt = "b(a3) se(3) scalars(cl_l cl_h pval bw N_eff ymean ysd) label star(+ 0.1 * 0.05 ** 0.01)"

local today : di %td date("$S_DATE", "DMY")
log using "$logpath/2_rd_1stStage_`today'.log", replace

global missing = "no_missing" // "with_missing" or "no_missing"


********************************************************************************
*						I. Priority into Candidate Characteristics
********************************************************************************

	use "../Voters Edge/scrape/output/all_candidates_matched.dta", clear
	merge 1:1 multi_candid using "../Voters Edge/priority_rating/dofiles/results/all_candidates_final_demo.dta", gen(_demo_fin)
	merge 1:1 multi_candid using "../L2/data/matches_final_all_with_var.dta", gen(_l2) keepus(n_matches democrat* republican* unique_party)
	
	* drop irrelvant cases 
	drop if college==1 // drop college board election; 2,006 cand	
	drop if leaid=="" // drop if no leaid; 106 cand	
	unique multi_race // 6,312
	drop if offedu==1 // drop if county/state level board; 1,020 cand, 393 elections
	drop if uncontested // drop if no context; 116 cand, 88 elections
	bysort multi_raceid: egen win_any= min(win_status)
	drop if win_any~=1 // drop if no win; 183 cand, 49 elections
	unique multi_race // 5,782
	
	* define candidates characteristics based on priorities
	g valid_platform= cond(~missing(academic_concern_rate),1,0)

	g teacher_care = cond(union_friendly_q5_rate==3|union_friendly_q5_rate==2,1,0)
	g budget_hawk = cond(fiscal_conservatism_q3_rate==3|fiscal_conservatism_q3_rate==2,1,0) 
	g agenda_bias = cond(agenda_bias_q1_rate==3|agenda_bias_q1_rate==2,1,0)
	g against_left = cond(agenda_bias_q2_rate==3|agenda_bias_q2_rate==2,1,0) 
	g against_right = cond(agenda_bias_q3_rate==3|agenda_bias_q3_rate==2,1,0)
	g academic_prior = cond(academic_concern_rate==3|academic_concern_rate==2,1,0)
	g score_concern = cond(score_concern_rate==3|score_concern_rate==2,1,0)
	g sup_concern = cond(sup_concern_rate==2,1,0)

	foreach x in equity facility it overcrowding enrollment dropout safety_health mental_health cte {
		g `x'_prior = cond(`x'_concern_rate==3|`x'_concern_rate==2,1,0)
	}

	drop parent_involvement police_in_school
	foreach x in parent_involvement police_in_school {
		g `x' = cond(`x'_rate==3|`x'_rate==2,1,0)
	}

	g agenda_left = cond(agenda_bias_q4_rate==1|agenda_bias_q4_rate==2,1,0)
	g agenda_right = cond(agenda_bias_q4_rate==4|agenda_bias_q4_rate==5,1,0)

	g hisp = cond(pcthisp>0.5,1,0) if !missing(pcthisp) // histogram pcthisp 
	g white = cond(pctwhite>0.5,1,0) if !missing(pctwhite) 
	g api = cond(pctapi>0.5,1,0) if !missing(pctapi) 
		
	g nonmale = cond(gender=="male"|gender=="mostly_male",0,1)
	g female = cond(gender=="female"|gender=="mostly_female",1,0) if strpos(gender,"male")
	g male = cond(gender=="male"|gender=="mostly_male",1,0) if strpos(gender,"male")
	
	if "$missing" == "no_missing" {
	   drop if missing(academic_concern_rate) // how do we want to deal with candidates with missing platform at all? 
	}
	
	foreach y in $list_prio {
		rforest `y' hisp female occ_teacher democrat_v2 if valid_platform==1, type(class) seed(123) iterations(50)
		predict `y'_p0 `y'_p1, pr
	}
	
	*** Labeling
	la var valid_platform "Have a Platform"
	la var hisp "Hispanic"
	la var white "White"
	la var api "Asian"
	la var male "Male"
	la var female "Female"
	la var occ_teacher "Educator"
	la var democrat_v2 "Registered Democrat"
	la var republican_v2 "Registered Republican"
	la var democrat "Registered Democrat"
	la var republican "Registered Republican"
	la var budget_hawk             "Fiscal Conservatism"
	la var equity_prior            "Equity"
	la var teacher_care            "Teacher Support"
	la var safety_health_prior     "Safety"
	la var facility_prior          "Facility Improvement"
	la var enrollment_prior        "Enrollment"
	la var dropout_prior           "Dropout"
	la var cte_prior               "CTE"
	la var academic_prior          "Academic Achievement"
	la var parent_involvement "Community Engagement"
	la var agenda_bias             "Agenda Bias"
	la var sup_concern "Sup Accountability/Hiring"
	la var score_concern "Test Score"

	save "candidates_all_$missing.dta", replace
	
	
foreach x in $list_demo $list_prio {
global candChar = "`x'" // characteristics of candidate of interest?


********************************************************************************
*				II. Define Close Election (original code from Ariel)
********************************************************************************
	use "candidates_all_$missing.dta", replace
	
	egen tag_race = tag(multi_raceid)

*** 1/ Close election in general
	count if n_candidates != cand // 40 races with a missing candidate - probably ppl that didn't match to admin data? 
	keep if n_candidates==cand
	drop cand

	* for now, work with win_status & vote_share
	bysort multi_raceid: egen numwinners = total(win_status==1)
	bysort multi_raceid: egen sumvoteshares = total(vote_share)

	* keep races where shares add up to 100
	drop if sumvoteshares < 97 | sumvoteshares > 100

	sort multi_raceid vote_share

	bysort multi_raceid: egen max_lose_share = max(vote_share*(win_status==2))
	bysort multi_raceid: egen min_win_share = min(exp(log(vote_share*(win_status==1))))

	unique multi_raceid if min_win_share==. // 66 races where there's a runoff - no specified winner in admin data 

	bysort multi_raceid: gen close_race = (min_win_share - max_lose_share) < 5
	unique multi_raceid if close_race==1

	gen win_lose_diff = min_win_share - max_lose_share
	replace win_lose_diff = . if uncontested==1

	bysort multi_raceid: gen n_cand_profile = sum(has_profile)

	* identify least winner and max loser 
	gen least_winner = vote_share==min_win_share
	gen highest_loser = vote_share==max_lose_share

	* check number of min winner and max loser per race and mark "clean" elections (one min win, one max lose)
	* min / max winners problems arise with runoff elections 
	bysort multi_raceid: egen ct_least_winner = sum(least_winner)
	bysort multi_raceid: egen ct_highest_loser = sum(highest_loser)

	gen clean_max_min_race = ct_least_winner==1 & ct_highest_loser==1
	drop if clean_max_min_race==0 // keep only elections with one winner and one loser ; necessary to define distance in other priorities
	
	preserve
	keep if (least_winner==1 | highest_loser==1)  & abs(win_lose_diff)<=5
	save "candidates_close_election_$missing.dta", replace
	restore
	
*** 2/ Close election, by candidate's charatereristics
	* is the election winner char_yes or char_no? (missing if neither)
	bysort multi_raceid: egen winner_char_yes = max(least_winner * $candChar )
	bysort multi_raceid: egen winner_char_no = max(least_winner* ~$candChar)
	replace winner_char_yes=. if winner_char_yes==0 & winner_char_no==0

	* identify most popular opposite-identity loser in each contest
	*gen opposite_loser_perc = abs(winner_char_yes - $candChar ) * vote_share * (win_status!=1)	
	gen opposite_loser_perc = abs(winner_char_yes - $candChar ) * vote_share if highest_loser==1
	bysort multi_raceid: egen max_opp_loser_perc = max(opposite_loser_perc)

	gen max_opp_loser = vote_share==max_opp_loser_perc

	* gen vote share diff between least winner and highest opposite identity loser 
	gen vote_margin = min_win_share - max_opp_loser_perc
	replace vote_margin=. if max_opp_loser_perc==0

	* make the difference negative if char_yeseral winner
	replace vote_margin = -vote_margin if winner_char_yes==0
	drop if win_lose_diff<0 // (TODO) how should we deal with these cases?

	la var winner_char_yes "Least popular winner is $candChar"
	la var winner_char_no "Least popular winner is not $candChar"
	
	
*** 3/ differences in other priorities	
	foreach y in $list_demo $list_prio {
		g winner_y_temp=`y' if least_winner
		bysort multi_raceid: egen winner_y_all= max(winner_y_temp)
		
		g loser_y_temp=`y' if highest_loser
		bysort multi_raceid: egen loser_y_all= min(loser_y_temp)
		*g winner_D`y' = winner_y_all - loser_y_all
		g winner_`y' = winner_y_all - loser_y_all
		cap drop winner_y* loser_y*
	}
	foreach y in $list_prio {
		g winner_y_temp=equity_prior_p1 if least_winner
		bysort multi_raceid: egen winner_y_all= max(winner_y_temp)
		
		g loser_y_temp=equity_prior_p1 if highest_loser
		bysort multi_raceid: egen loser_y_all= min(loser_y_temp)
		*g winner_D`y' = winner_y_all - loser_y_all
		g winner_`y'_p = winner_y_all - loser_y_all		
		cap drop winner_y* loser_y*
	}	
	
	g long winner_multi_candid = multi_candid if least_winner==1 & $candChar ==1
	format %12.0g winner_multi_candid
	sort multi_raceid winner_multi_candid
	bysort multi_raceid: replace winner_multi_candid=winner_multi_candid[1]

	
********************************************************************************
*							III. Specification Checks
********************************************************************************
	
*** 1) density of runnig variable
	rddensity vote_margin if tag_race==1, pl plot_range(-50 50) plot_n(100 100) ///
	graph_opt(xtitle("Vote Margin") title("") ytitle("Density") legend(off)) 
    local pv = round(e(pv_q), .01)
    local h_l = -e(h_l)
    local h_r = e(h_r)
    local N = e(N_h_l) + e(N_h_r)
  
	local title = cond("$candChar"=="hisp", "Hispanic", ///
				  cond("$candChar"=="female", "Female", ///
				  cond("$candChar"=="equity_prior", "Equity", ///
				  cond("$candChar"=="budget_hawk", "Fiscal Conservative", " "))))
	global title = "`title'"
	
	rddensity vote_margin if tag_race==1, pl plot_range(-30 30) plot_n(100 100) ///
	graph_opt(xtitle("Vote Margin") title("$title") ytitle("Density") legend(off) note("p-val = `pv',  N = `N'", ring(0) pos(1) size(*1.5)) ylabel(0(0.02)0.1) xline(`h_l' `h_r')) ///
	esll_opt(lcolor(ebblue*2) lwidth(med)) ///
	eslr_opt(lcolor(dkorange*1.2) lwidth(med)) ///
	cirl_opt(color(ebblue*2%30)) ///
	cirr_opt(color(dkorange*1.2%30)) ///
	histl_opt(color(ebblue%30)) ///
	histr_opt(color(dkorange%30))

	gr save "Results/Graph/density_test_$candChar", replace
	gr export "Results/Graph/density_test_$candChar.png", replace

	* 6,980 unique elections
	su vote_margin winner_char_yes if tag_race==1

*** 2) should we consider full dynamics?
// some districts have multiple elections in a year, because of the ward system
// what Macartney and Singleton did was to treat each election as distinct board
	bysort leaid year: egen winner_char_yes_any_dist = max((win_status==1) * $candChar )
	la var winner_char_yes_any_dist "At least one winner in elections of the district is $candChar"

	sort leaid district year
	bysort leaid: g winner_char_yes_any_next = winner_char_yes_any_dist[_n+1] if year~=year[_n+1]
	sort leaid year winner_char_yes_any_next
	bysort leaid year: replace winner_char_yes_any_next=winner_char_yes_any_next[1]
	la var winner_char_yes_any_next "At least one winner in next elections of the district is $candChar"

	eststo clear
	eststo: rdrobust winner_char_yes_any_next vote_margin if tag_race==1, p(1) kernel(triangular) bwselect(mserd)
	estadd scalar cl_l= e(ci_l_rb)
	estadd scalar cl_h= e(ci_r_rb)
	estadd scalar pval = e(pv_rb)
	estadd scalar bw = e(h_l)
	estadd scalar N_eff = e(N_h_l) + e(N_h_r)
	qui estadd ysumm
	esttab using "Results/Graph/dynamic_need_check.csv", append $tab_opt

	
********************************************************************************
*				IV. District x Year with Multiple Elections
********************************************************************************

	duplicates drop multi_raceid, force // keep one obs from each election

	duplicates r multi_raceid
	duplicates r multi_raceid year
	duplicates r leaid year
	duplicates r leaid year area1 
	duplicates r leaid year area1 term1 
	duplicates r leaid year term1 area1 date

	keep leaid state_leaid id_ccd multi_raceid year off_cycle vote_margin term1 area1 date elementary_dummy high_dummy offedu winner_* 
	ren (elementary_dummy high_dummy offedu) (dist_elem dist_high dist_cnty)
	save "Temp/$missing/vote_share_$candChar.dta", replace

	* when there were multiple elections within a district X year + when multiple elections have non-missing vote share
	use "Temp/$missing/vote_share_$candChar.dta", clear	
	egen id_temp = group(leaid year)
	keep if !missing(vote_margin)
	bysort id_temp: g j_temp = _n

	reshape wide off_cycle vote_margin term1 area1 date multi_raceid winner_* dist_elem dist_high dist_cnty, i(id_temp) j(j_temp)
	order leaid state_leaid id_ccd year
	drop id_temp
	ren area1* area*
	ren term1* term*

	* 1) drop if multiple elections are treated in an opposite way within a district x year
	g multi_treat = cond(!missing(vote_margin2),1,0)

	gen diff_sign = 0 if missing(vote_margin2)
	ds vote_margin*
	local vote_margin_vars `r(varlist)'
	foreach x in `vote_margin_vars' {
	replace diff_sign = (sign(vote_margin1) ~= sign(`x')) if !missing(`x') 
	}

	outreg2 using "summary_election.xls", append sum(detail) eqkeep(N mean sd p50 min max) label keep(multi_treat diff_sign)

	ds vote_margin*
	local vote_margin_vars `r(varlist)'
	g vote_margin = vote_margin1
	g case =1 
	local c=1
	foreach x in `vote_margin_vars' {
		replace vote_margin = `x' if !missing(`x') & diff_sign==0 & abs(vote_margin1)>abs(`x')
		replace case = `c' if !missing(`x') & diff_sign==0 & abs(vote_margin1)>abs(`x')
		local c=`c'+1
	}
	drop if diff_sign==1 
	drop diff_sign


	* 2) if multiple treatment in the same direction, then use the case with the closest margin
	su case
	local C = r(max)
	foreach x in area term date dist_cnty dist_elem dist_high {
		g `x' = `x'1
	}
	ds multi_raceid*
	local vars = r(varlist)
	foreach x in `vars' {
		tostring `x', replace
	}
	g multi_raceid = multi_raceid1
	g off_cycle = off_cycle1
	foreach x in $list_demo multi_candid {
		g long winner_`x'=winner_`x'1
	}
	foreach x in $list_prio {
		g long winner_`x'=winner_`x'1
		g winner_`x'_p=winner_`x'_p1 // long is a integer type
	}
	
	forvalues c=2/`C' {
		foreach x in area term date multi_raceid off_cycle dist_cnty dist_elem dist_high {
			replace `x'=`x'`c' if case==`c'
		}
	}
	forvalues c=2/`C' {
		foreach x in $list_demo multi_candid {
			replace winner_`x'=winner_`x'`c' if case==`c'
		}
		foreach x in $list_prio {
			replace winner_`x'=winner_`x'`c' if case==`c'
			replace winner_`x'_p=winner_`x'_p`c' if case==`c'
		}
	}

	keep leaid state_leaid id_ccd multi_raceid year off_cycle vote_margin term area winner_* dist_cnty dist_elem dist_high
	foreach n in 1 2 3 4 5 6 {
		cap drop winner_*`n'
	}
	save "Temp/$missing/vote_share_dist_$candChar.dta", replace


********************************************************************************
*							V. Merge with Outcome
********************************************************************************

	use "Temp/$missing/vote_share_$candChar.dta", clear
	destring leaid, replace	
	g year_elected = year

	expand 9
	bysort multi_raceid year: replace year = year+_n-5

	merge m:1 leaid year using "outcomes.dta", gen(_platform) force keep(master matched)

	ren leaid id_district_nces
	sort id_district_nces year
	drop if id_district_nces==.
	save "data_for_rd/$missing/$candChar.dta", replace

	use "Temp/$missing/vote_share_dist_$candChar.dta", clear
	destring leaid, replace	
	g year_elected = year

	expand 9
	bysort leaid year: replace year = year+_n-5

	merge m:1 leaid year using "outcomes.dta", gen(_platform) force keep(master matched)

	ren leaid id_district_nces
	sort id_district_nces year
	drop if id_district_nces==.
	save "data_for_rd/$missing/dist_$candChar.dta", replace
}


********************************************************************************
*						VI. Merged Data Set for Joint RDs
********************************************************************************

	use "data_for_rd/$missing/dist_female.dta", clear
	ren vote_margin m_female 
	foreach x in hisp occ_teacher democrat_v2 $list_prio {
		merge 1:1 multi_raceid year using "data_for_rd/$missing/dist_`x'.dta", gen(_`x')
		ren vote_margin m_`x'
	}
	drop if dist_cnty==1
	save "data_for_rd/$missing/dist_stacked.dta", replace
	
	
	use "data_for_rd/$missing/female.dta", clear
    ren vote_margin m_female 

    foreach x in hisp occ_teacher democrat_v2 $list_prio {
        merge 1:1 multi_raceid year using "data_for_rd/$missing/`x'.dta", gen(_`x')
        ren vote_margin m_`x'
    }

    drop if dist_cnty==1

    save "data_for_rd/$missing/stacked.dta", replace
	

********************************************************************************
*							VII. Density Check Results
********************************************************************************

	if "$missing" == "with_missing" {
	   gr combine "Results/Graph/density_test_equity_prior" "Results/Graph/density_test_budget_hawk" "Results/Graph/density_test_hisp" , col(3) imargin(zero) ysize(2) iscale(1.2)
	   gr export "Results/Graph/density_test_$missing.png", replace
	}

	
********************************************************************************
*						VIII. RD data With Normalized Outcomes
* // Min: not sure what's the right way to normalize J90 variables... the right way should be to normalize it within each year across all districts as I'm doing for other variables, but J90 variables are constructed using the election year, so it's tricky. I'm normalizing those variables among all close elections for now
********************************************************************************

	use "outcomes.dta", clear
	
	global list_outcome ="rev_cte_per_stu exp_total_per_stu  exp_capital_total_per_stu bond_amount_per_stu teacher_exit_dist ecd_test hsp_test test log_enrollment sus_share enrollment_charter_share n_charter_sch" // list used in 3_f_compile_h0 + charter - TSAL - superintendent
	 
	ren *vocational* *cte*
	g log_enrollment=log(enrollment)
	foreach y in $list_outcome  {
	bysort year: egen z=std(`y')
	replace `y' = z
	drop z
	}
	save "Temp/outcomes_z.dta", replace

	foreach x in $list_demo $list_prio {
	use "Temp/$missing/vote_share_dist_`x'.dta", clear
	destring leaid, replace	
	g year_elected = year

	expand 9
	bysort leaid year: replace year = year+_n-5

	merge m:1 leaid year using "Temp/outcomes_z.dta", gen(_platform) force keep(master matched)

	ren leaid id_district_nces
	sort id_district_nces year
	save "data_for_rd/$missing/dist_`x'_z.dta", replace
	}

	
	
log c
