********************************************************************************
* votes_by_member.do

* Written by Ariel Gelrud 6/6/25						
********************************************************************************
clear all
set more off

* install and update packages as needed
/*ssc install libjson
ssc install educationdata, replace
ssc install sxpose
ssc install egenmore
ssc install missings
ssc install tabout
ssc install matmap
ssc install carryforward
*/

cd "C:\Users\hahn0\Desktop\Hahn_Park"

* set directory 
global mindta "Minutes/data/dta"

* save multi_candid and name for marginal winner and marginal loser for all elections 
********************************************************************************
use "Code/candidates_all_with_missing.dta", replace 

keep multi_raceid multi_candid leaid win_status vote_share name name_sc 

sort multi_raceid win_status vote_share 
by multi_raceid: gen marg_win = _n==1
replace marg_win=0 if win_status!=1

gsort multi_raceid -win_status -vote_share 
by multi_raceid: gen marg_los = _n==1
replace marg_los=0 if win_status!=2

tostring multi_candid, replace 

gen marg_win_multi_candid = multi_candid 
replace marg_win_multi_candid="" if marg_win!=1

gen marg_los_multi_candid = multi_candid 
replace marg_los_multi_candid="" if marg_los!=1

gen marg_win_name = name 
replace marg_win_name="" if marg_win!=1

gen marg_los_name = name 
replace marg_los_name="" if marg_los!=1

gen marg_win_name_sc = name_sc 
replace marg_win_name_sc="" if marg_win!=1

gen marg_los_name_sc = name_sc 
replace marg_los_name_sc="" if marg_los!=1

gsort multi_raceid -marg_win_multi_candid
by multi_raceid: carryforward marg_win_multi_candid, replace

gsort multi_raceid -marg_los_multi_candid
by multi_raceid: carryforward marg_los_multi_candid, replace

gsort multi_raceid -marg_win_name
by multi_raceid: carryforward marg_win_name, replace

gsort multi_raceid -marg_los_name
by multi_raceid: carryforward marg_los_name, replace

gsort multi_raceid -marg_win_name_sc
by multi_raceid: carryforward marg_win_name_sc, replace

gsort multi_raceid -marg_los_name_sc
by multi_raceid: carryforward marg_los_name_sc, replace


keep multi_race marg_win_* marg_los_* leaid 

duplicates drop 
duplicates drop multi_race, force 

destring marg_win_multi, replace
format marg_win_multi %15.0g
destring marg_los_multi, replace
format marg_los_multi %15.0g
destring multi_raceid, replace
format multi_raceid %15.0g

* save 
save "$mindta/marg_win_los_multicandid.dta", replace 


* * *  
unique multi_race // 5782

unique leaid // 761

tostring multi_raceid, gen(mrid)
gen year = substr(mrid,1,4)

unique leaid year // 4634

tab year
 

* * * 
* construct year x district x member vote counts by category using data created in 
* construct_member_data.py 

* FIRST FIX unique member ID matches for last name only matches 
********************************************************************************
use "$mindta/merged_member_level_votes_marg.dta", replace 

unique leaid year if match_confidence=="LOW" 

* drop low confidence matches - all seem wrong but represent only 0.15% of sample 
drop if match_confidence=="LOW"
drop if leaid==""

duplicates drop unique_member, force

sort unique_member
br name unique

count // 8060



* Sort by id for efficiency
sort leaid

* Create variable to store the ID of the obs that contains the name
generate name_contained = .
gen name_id = _n

* Create a loop that checks each observation against others in same id group
forvalues i = 1/`=_N' {
    if mod(`i', 20) == 0 {
        noisily display "Processing observation `i'"
    }
    if missing(name_contained[`i']) {  // Skip if already found
        local current_id = leaid[`i']
        local current_name = name[`i']
        
        * Find the first observation with same id that contains current name
        forvalues j = 1/`=_N' {
            if `j' != `i' & leaid[`j'] == "`current_id'" {
                local other_name = name[`j']
                if strpos("`other_name'", "`current_name'") > 0 {
                    replace name_contained = `j' in `i'
                    continue, break
                }
            }
        }
    }
}


* save 
save "$mindta/temp_unique_member_fix.dta", replace 



* * * 

use "$mindta/temp_unique_member_fix.dta", replace 


gen name_test = name_id
replace name_test = name_contained if name_contained!=.

br unique_member* name name_contained name_test name_id

forvalues i = 1/`=_N' {
	local t = name_contained[`i']
	replace name_test = name_contained[`t'] if name_contained[`t']!=. & _n==`i'
}

rename name_test name_id_f

duplicates tag name_id_f, gen(dup)

gen len = length(name)

sort name_id name_id_f

gen unique_member_map = ""

forvalues i = 1/`=_N' {
	local t = name_id_f[`i']
	replace unique_member_map = unique_memberid[`t'] if _n==`i'
}


* keep unique_member mapping and save 
keep unique_member* 

sort unique_memberid
save "$mindta/temp_unique_member_mapping.dta", replace 



* construct year x district x member vote counts by category using data created in 
* construct_member_data.py 
********************************************************************************

* read in member-level counts from python file 
use "$mindta/merged_member_level_votes_marg.dta", replace 

unique leaid year if match_confidence=="LOW" 

* drop low confidence matches - all seem wrong but represent only 0.15% of sample 
drop if match_confidence=="LOW"
drop if leaid==""


* merge in member ID mapping from previous section to correct for individuals identified as separate people 
merge m:1 unique_memberid using "$mindta/temp_unique_member_mapping.dta"

gen name_len = length(name)
sort unique_member_map name_len

* * * 
* rename marginal var names that were cutoff 
rename academic_achievement_yes_votes_m academic_yes_votes_mg1  
rename _0academic_achievement_yes_votes academic_yes_votes_mg2  

rename budget_spending_yes_votes_margin budget_yes_votes_mg1  
rename _1budget_spending_yes_votes_marg budget_yes_votes_mg2

rename curriculum_bias_yes_votes_margin curric_yes_votes_mg1  
rename _2curriculum_bias_yes_votes_marg curric_yes_votes_mg2

rename parental_engagement_yes_votes_ma parental_yes_votes_mg1  
rename _3parental_engagement_yes_votes_ parental_yes_votes_mg2

rename safety_and_health_yes_votes_marg safety_yes_votes_mg1  
rename _4safety_and_health_yes_votes_ma safety_yes_votes_mg2

rename school_facilities_yes_votes_marg facilities_yes_votes_mg1  
rename _5school_facilities_yes_votes_ma facilities_yes_votes_mg2

rename support_for_teachers_yes_votes_m supp_t_yes_votes_mg1  
rename _6support_for_teachers_yes_votes supp_t_yes_votes_mg2

rename *marginal* *mg*

/*
academic_achievement_yes_votes_marginal1   ->   academic_achievement_yes_votes_m
academic_achievement_yes_votes_marginal2   ->   _0academic_achievement_yes_votes
budget_spending_yes_votes_marginal1   ->   budget_spending_yes_votes_margin
budget_spending_yes_votes_marginal2   ->   _1budget_spending_yes_votes_marg
curriculum_bias_yes_votes_marginal1   ->   curriculum_bias_yes_votes_margin
curriculum_bias_yes_votes_marginal2   ->   _2curriculum_bias_yes_votes_marg
parental_engagement_yes_votes_marginal1   ->   parental_engagement_yes_votes_ma
parental_engagement_yes_votes_marginal2   ->   _3parental_engagement_yes_votes_
safety_and_health_yes_votes_marginal1   ->   safety_and_health_yes_votes_marg
safety_and_health_yes_votes_marginal2   ->   _4safety_and_health_yes_votes_ma
school_facilities_yes_votes_marginal1   ->   school_facilities_yes_votes_marg
school_facilities_yes_votes_marginal2   ->   _5school_facilities_yes_votes_ma
support_for_teachers_yes_votes_marginal1   ->   support_for_teachers_yes_votes_m
support_for_teachers_yes_votes_marginal2   ->   _6support_for_teachers_yes_votes
other/NA_yes_votes_marginal1   ->   other_NA_yes_votes_marginal1
other/NA_yes_votes_marginal2   ->   other_NA_yes_votes_marginal2
* * */


* collapse vote counts 
collapse (sum) *votes* (last) name, by(year leaid unique_member_map)

* rename vars 
rename academic_achievement* academic*
rename parental_engagement* parental*
rename budget_spending* budget*
rename school_facilities* facilities*
rename safety_and_health* safety*
rename curriculum_bias* curric*
rename support_for_teachers* supp_t*


* save totals by district and year 
preserve
collapse (sum) *votes, by(year leaid)

rename *votes *votes_all

save "$mindta/temp_vote_sums_all.dta", replace 
restore 

* merge leaid year totals to main data 
merge m:1 leaid year using "$mindta/temp_vote_sums_all.dta"

* keep fullest name 
gen name_len = length(name)
gsort unique_member_map -name_len

gen name_full = name 
by unique_member: replace name_full = "" if _n!=1
by unique_member: carryforward name_full, replace 

replace name = name_full
drop name_full

* save 
save "$mindta/collapsed_member_votes.dta", replace 

unique unique_member_map

* construct dataset from elections for merge (expand 4 years after election to match with minutes)
********************************************************************************
cd "C:\Users\hahn0\Desktop\Hahn_Park\Code"
use "candidates_all_with_missing.dta", replace

keep name name_sc leaid year multi_candid

* expand 5 years (year of election + 4 years after) 
expand 5 

rename year elecyear
bys multi_candid: gen year = _n - 1
replace year = elecyear + year

* keep relevant years in minutes data (2016-2023)
keep if year > 2015 & year < 2024

* save 
save "$mindta/temp_election_formerge.dta", replace 



********************************************************************************
/* Import Merge
	- strict year + leaid match 
	- fuzzy name match 
	- mapping from multi_candid to unique_member_map by leaid + year 
********************************************************************************/

import delim "$mindta/vote_match_results.csv", clear 

sort unique_member multi_candid

by unique_member: gen match = multi_candid[1]==multi_candid[_N]



* * * TEST - add two observations for edge years where candidate should count towards incumbent term and next term (ex. 2020 for candidate in 2016 and 2020 election)

* 1. drop cases where a member is counted for two separate terms but a new election already happened for them. (Ex. in candidate is elected in 2018 and again in 2020, 2021 record should only count for the 2020 term)
tostring multi_candid, gen(mstr)
gen elecyr = substr(mstr,1,4)
destring elecyr, replace 

duplicates tag unique_member year, gen(mem_yr_dup)

bys unique_member year: egen max_elecyr = max(elecyr)

gen past_new_election = mem_yr_dup==1 & max_elecyr < year & elecyr != max_elecyr

drop if past_new_election==1

* 2. add two observations for edge years where candidate should count towards incumbent term and next term (ex. 2020 for candidate in 2016 and 2020 election)
sort unique_member multi_candid year

gen edge = elecyr == year[_n-1] & unique_member==unique_member[_n-1] & multi_candid!=multi_candid[_n-1]

* drop generated vars 
drop mstr elecyr mem_yr_dup max_elecyr past_new_election edge 
* * * 

unique leaid 
unique unique_member

duplicates tag leaid year unique_member, gen(dupu)
duplicates tag leaid year multi_candid, gen(dupm)

drop if name_1=="xavier nguyen" & name_2=="frances nguyen" & dupm==1
drop if name_1=="liz gamez samuels" & name_2=="kyle samuels" & dupm==1
drop if name_1=="xavier nguyen" & name_2=="khanh nguyen" & dupm==1

tostring leaid, replace 
gen leaid_padded = string(real(leaid), "%07.0f")  
replace leaid = leaid_padded
drop leaid_padded

* ^ unique at year - leaid - multi_candid - unique_member level. Duplicates exist at 
* multi_candid - unique_member level because some motions happen within 4 years of two elections for same person (2-year terms)

* save 
save "$mindta/cw_vote_match_results.dta", replace  

unique multi_candid // 4622
unique leaid year // 3046
unique leaid // 510

* merge multi_candid to votes data and add motions data 
********************************************************************************
use "$mindta/collapsed_member_votes.dta", replace 

merge 1:m year leaid unique_member_map using "$mindta/cw_vote_match_results.dta", gen(_mult)
	
/*
    Result                      Number of obs
    -----------------------------------------
    Not matched                        12,090
        from master                    12,090  (_mult==1)
        from using                          0  (_mult==2)

    Matched                            12,876  (_mult==3)
    -----------------------------------------
*/


* prep for merge to dist_* datasets 
drop if multi_candid==.
keep year leaid multi_candid *votes* _*


save "$mindta/collapsed_member_votes_multi_candid.dta", replace 

unique multi_candid // 4622
unique leaid year // 3046
unique leaid // 510

* * *
tostring multi_candid, gen(mcid)
gen elec_year = substr(mcid,1,4)

duplicates drop multi_candid, force 
keep multi_candid 
save "$mindta/temp_unique_multi_candid_min.dta", replace 


/********************************************************************************
* merge for counting purposes 
********************************************************************************
use "$mindta/marg_win_los_multicandid.dta", replace 

rename marg_win_multi multi_candid 
drop if multi_candid==.

merge 1:1 multi_candid using "$mindta/temp_unique_multi_candid_min.dta" 

tostring multi_raceid, gen(mrid)
gen years = substr(mrid,1,4)
destring years, replace 
// replace year = years if year==.
// drop years 

tab years _m

* get match by leaid x year 
use "$mindta/minutes_for_merge_allobs.dta", replace 

duplicates drop leaid year, force

keep leaid year 
drop if year==.
drop if leaid==""

expand 5

bys leaid year: gen elec_year = _n - 1

replace elec_year = year - elec_year

gen edge_year = elec_year==year | elec_year==year-4


duplicates drop leaid elec_year, force 

save "$mindta/temp_min_leaid_years.dta", replace 


* merge on leaid year 
use "$mindta/marg_win_los_multicandid.dta", replace 

tostring multi_raceid, gen(mrid)
gen year = substr(mrid,1,4)
destring year, replace 

*duplicates drop leaid year, force 

rename year elec_year 

merge m:1 leaid elec_year using "$mindta/temp_min_leaid_years.dta", gen(_mdist)


rename marg_win_multi multi_candid 
drop if multi_candid==.

merge 1:1 multi_candid using "$mindta/temp_unique_multi_candid_min.dta", gen(_mcand)


tostring multi_raceid, gen(mrids)
gen years = substr(mrids,1,4)
destring years, replace 

tab years _mc if _md==3

********************************************************************************
********************************************************************************/





* merge votes data to dist_* datasets 
********************************************************************************

cd "C:\Users\hahn0\Desktop\Hahn_Park\Code"

global list_demo = "hisp female occ_teacher democrat_v2"
global list_prio ="budget_hawk equity_prior safety_health_prior teacher_care agenda_bias parent_involvement_prior academic_prior facility_prior cte_prior dropout_prior enrollment_prior sup_concern"

foreach y in $list_demo $list_prio { //

	use "data_for_rd/with_missing/`y'.dta" if dist_cnty~=1, clear
	ren id_district_nces leaid
	tostring leaid, replace  // Convert to string if not already

	// Ensure consistent width by adding leading zeros
	gen leaid_padded = string(real(leaid), "%07.0f")  // Adjust "8" to the required width
	replace leaid = leaid_padded
	drop leaid_padded
	
	* * * 
	* merge in motions data 
	merge m:1 leaid year using "$mindta/minutes_final_year_level.dta", gen(_mot) keep(1 3)

	* rename motions 
	rename n_*_academic_prior n_*_academic
	rename n_*_engagement n_*_parental
	rename n_*_budget_hawk n_*_budget
	rename n_*_facility n_*_facilities
	rename n_*_safety_health_prior n_*_safety
	rename n_*_agenda_bias n_*_curric
	rename n_*_teacher_care n_*_supp_t
	rename n_*_equity_prior n_*_equity

	rename n_mtn n_mtn_total
	rename n_pass n_pass_total
	rename n_unan n_unan_total
	* * * 
	  	
	* * *  merge in marginal winner and loser multi_candid
	destring multi_raceid, replace 
	format multi_raceid %15.0g
	
	merge m:1 multi_raceid using "$mindta/marg_win_los_multicandid.dta", gen(_multi) keep(1 3) 
	
	/* create year var for _multi==2 observations 
	tostring multi_raceid, gen(yrstr)
	replace yrstr = substr(yrstr,1,4)
	destring yrstr, replace 
	replace year = yrstr if year==.
	drop yrstr */

	tab year _multi,m
	
	* set multi_candid to marginal_winner always
	gen double multi_candid = marg_win_multi 
	format %15.0g multi_candid
	
	merge m:1 multi_candid year using "$mindta/collapsed_member_votes_multi_candid.dta", gen(_votes) //keep(1 3)
	
	* generate others vote counts 
	foreach v in total academic budget curric equity parental procedural safety facilities supp_t other_NA {
		foreach yn in yes no {
			gen `v'_`yn'_votes_oth = `v'_`yn'_votes_all - `v'_`yn'_votes
		}
	}
	
	* generate share vote counts 
// 	foreach v in total academic budget curric equity parental procedural safety facilities supp_t other_NA {
// 		foreach yn in yes no {
// 			gen `v'_`yn'_votes_share = `v'_`yn'_votes / total_`yn'_votes
// 			gen `v'_`yn'_votes_oth_share = `v'_`yn'_votes_oth / total_`yn'_votes_oth
// 		}
// 	}
	
	* generate yes(no) / motion counts 
	foreach v in total academic budget curric equity parental safety facilities supp_t {
		foreach yn in yes no {
			gen `v'_`yn'_votes_permot = `v'_`yn'_votes / (n_mtn_`v')
			gen `v'_`yn'_votes_oth_permot = `v'_`yn'_votes_oth / (n_mtn_`v')
			gen `v'_`yn'_votes_all_permot = `v'_`yn'_votes_all / (n_mtn_`v')
		}
	}

	* motions per minute + marginal motions 
	foreach v in total academic budget curric equity parental safety facilities supp_t {
		gen mot_per_min_`v' = n_mtn_`v' / runtime_minutes
		gen n_marg_`v' = n_mtn_`v' - n_unan_`v'
	}
	
	* keep only obs with successfully merged vote data 
	keep if _vot==3

	* save dist_y datasets with new motion vars 
	save "data_for_rd/with_missing/votes_`y'.dta", replace 
	
}
 
STOP_END
********************************************************************************
* Checks 
********************************************************************************

cd "C:\Users\hahn0\Desktop\Hahn_Park\Code"


use "data_for_rd/with_missing/dist_votes_budget_hawk.dta", replace 
tab year
tab year_elec
missings report vote_margin n_mtn_budget n_pass_budget budget_*_votes_* total_*_votes *mg*

summ budget_yes_votes_permot,d
summ budget_yes_votes_oth_permot,d
summ budget_yes_votes_mg*,d

use "data_for_rd/with_missing/dist_votes_equity_prior.dta", replace 
missings report vote_margin n_mtn_equity n_pass_equity equity_*_votes_* total_*_votes *mg*

summ equity_yes_votes_permot,d
summ equity_yes_votes_oth_permot,d
summ equity_yes_votes_mg*,d


********************************************************************************
* Scrap work
********************************************************************************

	use "data_for_rd/with_missing/dist_budget_hawk.dta" if dist_cnty~=1, clear
	ren id_district_nces leaid
	tostring leaid, replace  // Convert to string if not already

	// Ensure consistent width by adding leading zeros
	gen leaid_padded = string(real(leaid), "%07.0f")  // Adjust "8" to the required width
	replace leaid = leaid_padded
	drop leaid_padded
	
	* * * 
	* merge in motions data 
	merge m:1 leaid year using "$mindta/minutes_final_year_level.dta", gen(_mot) //keep(1 3)
	
	tab _mot,m
	tab year _mot,m
	
	keep if _mot==1 | _mot==3

	* rename motions 
	rename n_*_academic_prior n_*_academic
	rename n_*_engagement n_*_parental
	rename n_*_budget_hawk n_*_budget
	rename n_*_facility n_*_facilities
	rename n_*_safety_health_prior n_*_safety
	rename n_*_agenda_bias n_*_curric
	rename n_*_teacher_care n_*_supp_t
	rename n_*_equity_prior n_*_equity

	rename n_mtn n_mtn_total
	rename n_pass n_pass_total
	rename n_unan n_unan_total
	* * * 
	  
	* * *  merge in marginal winner and loser multi_candid
	destring multi_raceid, replace 
	format multi_raceid %15.0g
	
	merge m:1 multi_raceid using "$mindta/marg_win_los_multicandid.dta", gen(_multi) // keep(1 3) nogen
	
	* create year var for _multi==2 observations 
	tostring multi_raceid, gen(yrstr)
	replace yrstr = substr(yrstr,1,4)
	destring yrstr, replace 
	replace year = yrstr if year==.
	
	tab year _multi,m
	
	* set multi_candid to marginal_winner or marginal_loser depending on whether vote_margin is +/-
	gen double multi_candid = marg_win_multi 
	format %15.0g multi_candid
	
	// (Min: 1. Loser will never show up in the meeting record. So when a budget hawk loses, we should use the opponent's id
	// 		2. Second, without having "double" part in the "gen" line, Stata was approximating multi_candid by itself, messing things up
	//		3. Even when using "double" we should still format the id properly in line 669
	// 		These three things together, increases the N of merged cases in line 673 from 590 to 2,471.)
	*replace multi_candid = marg_los_multi if vote_margin<0
	
	
	* merge member-level votes 
	merge m:1 multi_candid year using "$mindta/collapsed_member_votes_multi_candid.dta", gen(_votes) //keep(1 3)
	
	gen n_marg = n_mtn_total - n_unan_total
	tab year _votes if _multi==3 & _mot==3 
	
	// (Min: Odd cases are motions and candidates are merged plus, the year is grater than the election year
	// 		So 10%, 156 cases fall in this category. I'd check the multi_candid somewhat manually to understand why
	//		we're missing those in the member level vote count record.)
	tab _votes if _multi==3 & _mot==3 & year>=year_elected & year<=year_elected+3
	/*                   merge |      Freq.     Percent        Cum.
------------------------+-----------------------------------
        Master only (1) |        156       10.16       10.16
            Matched (3) |      1,379       89.84      100.00
------------------------+-----------------------------------
                  Total |      1,535      100.00*/
	tab year _votes if _multi==3 & _mot==3 & year>=year_elected & year<=year_elected+3
	 
	 // check
	 eststo clear
	 eststo: rdrobust n_pass_budget vote_margin if year>=year_elected & year<=year_elected+3
	 estadd scalar N_eff = e(N_h_l) + e(N_h_r)
	 eststo: rdrobust n_pass_budget vote_margin if year>=year_elected & year<=year_elected+3 & _mot==3 	 
	 estadd scalar N_eff = e(N_h_l) + e(N_h_r)
	 eststo: rdrobust n_pass_budget vote_margin if year>=year_elected & year<=year_elected+3 & _mot==3 & _votes==3
	 estadd scalar N_eff = e(N_h_l) + e(N_h_r)
	 esttab, scalars(N_eff)
	 /*------------------------------------------------------------
						  (1)             (2)             (3)   
				 n_pass_bud~t    n_pass_bud~t    n_pass_bud~t   
	------------------------------------------------------------
	RD_Estimate        -13.85***       -13.85***       -13.57***
					  (-5.77)         (-5.77)         (-5.21)   
	------------------------------------------------------------
	N                    1535            1535            1379   
	N_eff                 729             729             645   
	------------------------------------------------------------
*/

	 
	 
use "data_for_rd/with_missing/dist_votes_budget_hawk.dta" if dist_cnty~=1, clear
tab year 

