********************************************************************************
* motions_by_member.do

* Written by Ariel Gelrud 5/10/25						
********************************************************************************
clear all
set more off

* install and update packages as needed
// ssc install libjson
// ssc install educationdata, replace
// ssc install sxpose
// ssc install egenmore
// ssc install missings
// ssc install tabout
// ssc install matmap

* set directory 
global mindta "~/Dropbox/California Election Data/Minutes/data/dta"
global votersedge "~/Dropbox/California Election Data/Voters Edge"
global bc "~/Dropbox/California Election Data/Code/board_composition"
global minfile "California_Combined"


* save multi_candid for marginal winner and marginal loser for all elections 
********************************************************************************
use "/Users/aeg88/Dropbox/California Election Data/Code/candidates_all_with_missing.dta", replace 

keep multi_raceid multi_candid leaid win_status vote_share 

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

gsort multi_raceid -marg_win_multi_candid
by multi_raceid: carryforward marg_win_multi_candid, replace

gsort multi_raceid -marg_los_multi_candid
by multi_raceid: carryforward marg_los_multi_candid, replace

keep multi_race marg_win_ marg_los_ leaid

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


* Save motions with proposed_by member 
********************************************************************************

*** start with member level data, combined with general info
use "$mindta/minutes_for_merge_leaid.dta", clear 
keep meeting_id-pdf_pg_length year school_district leaid
duplicates drop
duplicates r meeting_id //meeting level
bysort leaid year: g n_meetings = _N


*** merge with motion level and extract keyword from each motion
merge 1:m meeting_id using "$mindta/${minfile}_motions.dta", gen(_motion)

forvalues n=1/5 {
replace keyword`n' = subinstr(keyword`n', "professional", "teachers", .) 
replace keyword`n' = subinstr(keyword`n', "student achievement", "academic", .) 
replace keyword`n' = subinstr(keyword`n', "facilities", "facility", .) 
}

foreach x in budget equity safety teachers facility curriculum engagement academic {
	g mtn_`x'_t = 0
	forvalues n=1/5 {
		replace mtn_`x'_t=1 if strpos(keyword`n',"`x'")	
	}
g pass_`x'_t = mtn_`x'_t * (passed=="true")
g unan_`x'_t = mtn_`x'_t * (unanimous=="true")
}

bysort meeting_id: g n_mtn = _N
*bysort meeting_id: g n_prcd = sum(strpos(keyword1,"procedural") & keyword2=="")
*replace n_mtn = n_mtn-n_prcd
bysort meeting_id: egen n_pass = sum(passed=="true")
bysort meeting_id: egen n_unan = sum(unanimous=="true")

foreach x in budget equity safety teachers facility curriculum engagement academic {
bysort meeting_id: egen n_mtn_`x' = sum(mtn_`x'_t) 
g pct_mtn_`x' = n_mtn_`x'/n_mtn
bysort meeting_id: egen n_pass_`x' = sum(pass_`x'_t)
g pct_pass_`x' = n_pass_`x'/n_mtn
bysort meeting_id: egen n_unan_`x' = sum(unan_`x'_t)
g pct_unan_`x' = n_unan_`x'/n_mtn
}

ren *budget* *budget_hawk*
ren *equity* *equity_prior*
ren *safety* *safety_health_prior*
ren *teachers* *teacher_care*
ren *curriculum* *agenda_bias*
ren *academic* *academic_prior*
ren comments_teacher_care comments_teachers

* keep relevant vars 
keep meeting_id pass* mtn* motion_id proposed_by seconded_by keyword* num_yes num_no yes_vote no_vote year leaid 

* drop missing motions
drop if motion_id==.

* save 
save "$mindta/motion_level.dta", replace 



* read in output data from python script that matches proposed_by names to multi_candid
********************************************************************************
use "$mindta/motion_by_member_match.dta", replace 

tostring multi_candid, replace
replace multi_candid="" if multi_candid=="."


* fill in multi_candid 

gsort leaid proposed_by -multi_candid
br leaid proposed_by multi_candid
by leaid proposed_by: carryforward multi_candid, replace

* * collapse to year-level sums and save 
collapse (sum) mtn_* , by(multi_candid leaid year) // leaid or not

drop if year==. | multi_candid==""

sort multi_candid

br multi_candid mtn*

destring multi_candid, replace 
format multi_candid %15.0g

rename mtn_*_t ownmtn_*

save "$mindta/member_level_motion_sums.dta", replace 


* merge with main dist_y datasets 
********************************************************************************

cd "~/Dropbox/California Election Data/Code/"
foreach y in budget_hawk equity_prior safety_health_prior teacher_care agenda_bias parent_involvement_prior { //
foreach y in budget_hawk equity_prior safety_health_prior teacher_care agenda_bias { //

	use "data_for_rd/with_missing/dist_`y'.dta" if dist_cnty~=1, clear
	ren id_district_nces leaid
	tostring leaid, replace  // Convert to string if not already

	// Ensure consistent width by adding leading zeros
	gen leaid_padded = string(real(leaid), "%07.0f")  // Adjust "8" to the required width
	replace leaid = leaid_padded
	drop leaid_padded

	merge m:1 leaid year using "$mindta/minutes_final_year_level.dta", keep(master matched) // generated from Minutes/code/merge_minutes_elections.do (last section)
	  
	tab year, g(yr_)
	
	* * *  merge in marginal winner and loser multi_candid
	destring multi_raceid, replace 
	format multi_raceid %15.0g
	
	merge m:1 multi_raceid using "$mindta/marg_win_los_multicandid.dta", keep(1 3) nogen
	
	* set multi_candid to marginal_winner or marginal_loser depending on whether vote_margin is +/-
	gen multi_candid = marg_win
	replace multi_candid = marg_los if vote_margin<0
	gen multi_candid = winner_multi_candid
	*replace multi_candid = marg_los if vote_margin<0
	
	
	merge m:1 multi_candid year using "$mindta/member_level_motion_sums.dta", keep(1 3) gen(_mot)
@ -200,96 +200,104 @@
	foreach v in budget_hawk equity_prior safety_health_prior teacher_care agenda_bias academic_prior {
		
		* own count
		gen n_ownmtn_`v' = ownmtn_`v' / n_meetings
		gen n_ownmtn_`v' = ownmtn_`v' 
		
		* others counts
		gen n_othrmtn_`v' = n_mtn_`v' - n_ownmtn_`v'
		replace n_othrmtn_`v' = n_mtn_`v' if missing(n_ownmtn_`v')
		
		* reference varaible for counts that represents (N others - N winner) if election won or N total if vote_margin<0 
		* (Seth thinks we should avoid this analysis since we are comparing partial counts when candidate wins to full counts when candidate loses)
		gen refmtn_`v' = n_othrmtn_`v'
		replace refmtn_`v' = n_mtn_`v' if vote_margin<0
	}
	
	egen id_election = group(leaid year_elected)
	
	
	* save dist_y datasets with new motion vars 
	save "data_for_rd/with_missing/dist_mtn_`y'.dta", replace 
	
}


* RD plot program
********************************************************************************	
capture program drop rd_plot
program define rd_plot
  syntax, data(string) xvar(string) ylab(string) ytitle(string) [subtitle(string)] [flag_balance(integer 0)]
  
	* switch on and off for balance checks 
	********************************************************************************
	global flag_balance = 0 // 0 if main rd 1 if balance
	if `flag_balance'==0 {
	 global year_cond = "year>=year_elected+1"
	}	
	else if `flag_balance'==1 {
	 global year_cond = "year<=year_elected-1"
	} 
	
	use "data_for_rd/with_missing/dist_mtn_`data'.dta" if dist_cnty~=1, clear
	tostring leaid, replace  // Convert to string if not already

	
  
	rdrobust `xvar' vote_margin if $year_cond , p(1) kernel(triangular) bwselect(mserd) covs(yr_*) vce(nncluster id_election)
	local bw = e(h_l) 
	local coef = round(e(tau_bc), .01)
	local ci_l = round(e(ci_l_rb), .01)
	local ci_h = round(e(ci_r_rb), .01)
  
	rdplot `xvar' vote_margin if abs(vote_margin) <= 10 & $year_cond , nbins(15 15) graph_options(legend(off)) genvars 
	rdplot `xvar' vote_margin if abs(vote_margin) <= 10 & $year_cond , nbins(5 5) graph_options(legend(off)) genvars 
	gen margin_d = 1 - (vote_margin < 0)
	
	if("`ytitle'"=="-") {
	tw (scatter rdplot_mean_y rdplot_mean_bin if $year_cond & abs(vote_margin) > `bw' & abs(vote_margin)<10, color(gray)) ///
    (lfitci `xvar' vote_margin if margin_d==1 & abs(vote_margin) <= `bw' & $year_cond , clcolor(dkorange*1.2) color(dkorange*.2)) ///
    (lfitci `xvar' vote_margin if margin_d==0 & abs(vote_margin) <= `bw' & $year_cond , clcolor(ebblue*2) color(ebblue*.2)) ///
    (scatter rdplot_mean_y rdplot_mean_bin if margin_d==1 & abs(vote_margin) <= `bw' & $year_cond , color(dkorange*1.2)) ///
    (scatter rdplot_mean_y rdplot_mean_bin if margin_d==0 & abs(vote_margin) <= `bw' & $year_cond , color(ebblue*2)) ///  
    , xtitle("Vote Margin") legend(off) graphregion(color(white)) ///
		xlabel(-10(5)10, labsize(*1.5)) ///
    note("{&beta} = `coef' [`ci_l',`ci_h']", ring(0) pos(1) size(*1.5)) ///
    ylabel(`ylab') ytitle("") `subtitle' plotregion(margin(zero)) ///
    saving("Results/Graph/rd_`xvar'_`data'", replace)
	gr export "Results/Graph/rd_`xvar'_`data'.png", replace
	}
	
	else {
		tw (scatter rdplot_mean_y rdplot_mean_bin if $year_cond & abs(vote_margin) > `bw' & abs(vote_margin)<10, color(gray)) ///
    (lfitci `xvar' vote_margin if margin_d==1 & abs(vote_margin) <= `bw' & $year_cond , clcolor(dkorange*1.2) color(dkorange*.2)) ///
    (lfitci `xvar' vote_margin if margin_d==0 & abs(vote_margin) <= `bw' & $year_cond , clcolor(ebblue*2) color(ebblue*.2)) ///
    (scatter rdplot_mean_y rdplot_mean_bin if margin_d==1 & abs(vote_margin) <= `bw' & $year_cond , color(dkorange*1.2)) ///
    (scatter rdplot_mean_y rdplot_mean_bin if margin_d==0 & abs(vote_margin) <= `bw' & $year_cond , color(ebblue*2)) ///  
    , xtitle("Vote Margin") legend(off) graphregion(color(white)) ///
		xlabel(-10(5)10, labsize(*1.5)) ///
    note("{&beta} = `coef' [`ci_l',`ci_h']", ring(0) pos(1) size(*1.5)) ///
    ylabel(`ylab') `ytitle' `subtitle' plotregion(margin(zero)) ///
    saving("Results/Graph/rd_`xvar'_`data'", replace)
	gr export "Results/Graph/rd_`xvar'_`data'.png", replace
	}
	
end 
  

* plots
********************************************************************************
rd_plot, data("budget_hawk") xvar("n_othrmtn_budget_hawk") ylab(", labsize(*1.5)") ytitle("ytitle(""N of Other Budget Motions Proposed"", size(*1.5))") subtitle("subtitle(""Fiscal Conservative"", ring(1) pos(12) size(*1.5))") flag_balance(0)
 
rd_plot, data("budget_hawk") xvar("n_othrmtn_budget_hawk") ylab(", labsize(*1.5)") ytitle("ytitle(""N of Budget Motions Others Proposed"", size(*1.5))") subtitle("subtitle(""Fiscal Conservative"", ring(1) pos(12) size(*1.5))") flag_balance(0)
rd_plot, data("budget_hawk") xvar("n_mtn_budget_hawk") ylab(", labsize(*1.5)") ytitle("ytitle(""N of Budget Motions Proposed"", size(*1.5))") subtitle("subtitle(""Fiscal Conservative"", ring(1) pos(12) size(*1.5))") flag_balance(0)

rd_plot, data("equity_prior") xvar("n_othrmtn_equity_prior") ylab(", labsize(*1.5)") ytitle("ytitle(""N of Other Budget Motions Proposed"", size(*1.5))") subtitle("subtitle(""Equity Priority"", ring(1) pos(12) size(*1.5))") flag_balance(0)
rd_plot, data("equity_prior") xvar("n_othrmtn_equity_prior") ylab(", labsize(*1.5)") ytitle("ytitle(""N of Equity Motions Others Proposed"", size(*1.5))") subtitle("subtitle(""Equity Priority"", ring(1) pos(12) size(*1.5))") flag_balance(0)
rd_plot, data("equity_prior") xvar("n_mtn_equity_prior") ylab(", labsize(*1.5)") ytitle("ytitle(""N of Equity Motions Proposed"", size(*1.5))") subtitle("subtitle(""Equity Priority"", ring(1) pos(12) size(*1.5))") flag_balance(0)

graph combine "Results/Graph/rd_n_othrmtn_equity_prior_equity_prior.gph" "Results/Graph/rd_n_othrmtn_budget_hawk_budget_hawk.gph", ///
	col(2) ysize(2) xsize(5) 
	
rd_plot, data("agenda_bias") xvar("n_othrmtn_agenda_bias") ylab(", labsize(*1.5)") ytitle("ytitle(""N of Curriculumn Motions Others Proposed"", size(*1.5))") subtitle("subtitle(""Agenda Bias"", ring(1) pos(12) size(*1.5))") flag_balance(0)
rd_plot, data("agenda_bias") xvar("n_mtn_agenda_bias") ylab(", labsize(*1.5)") ytitle("ytitle(""N of Curriculumn Motions Proposed"", size(*1.5))") subtitle("subtitle(""Agenda Bias"", ring(1) pos(12) size(*1.5))") flag_balance(0)


graph combine "Results/Graph/rd_n_othrmtn_budget_hawk_budget_hawk.gph" "Results/Graph/rd_n_othrmtn_equity_prior_equity_prior.gph" "Results/Graph/rd_n_othrmtn_agenda_bias_agenda_bias.gph", ///
	col(1) ysize(2) xsize(1) imargin(0 0 0 0)
gr export "Results/Graph/motion_others_proposed.png", replace	

graph combine "Results/Graph/rd_n_mtn_budget_hawk_budget_hawk.gph" "Results/Graph/rd_n_mtn_equity_prior_equity_prior.gph" "Results/Graph/rd_n_mtn_agenda_bias_agenda_bias.gph", ///
	col(1) ysize(2) xsize(1) imargin(0 0 0 0)
gr export "Results/Graph/motion_proposed.png", replace	

