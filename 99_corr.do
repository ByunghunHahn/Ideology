********************************************************************************
* Check the Correlation Across Priorities
* Written by Minseon Park 08/26/24
* Updated 09/24/24
********************************************************************************

set more off
set matsize 5000
set emptycells drop

* set directory 
cd "~/Dropbox/California Election Data/Code"
global logpath "~/Dropbox/California Election Data/Logs"

global list = "academic_prior agenda_bias agenda_left agenda_right budget_hawk cte_prior dropout_prior enrollment_prior equity_prior facility_prior it_prior mental_health_prior overcrowding_prior parent_involvement_prior police_in_school_prior safety_health_prior teacher_care"	

global list_full = "academic_prior agenda_bias agenda_left agenda_right against_left against_right budget_hawk cte_prior dropout_prior enrollment_prior equity_prior facility_prior it_prior mental_health_prior overcrowding_prior parent_involvement_prior police_in_school_prior safety_health_prior teacher_care"	


********************************************************************************
*	I. Correlation Matrix
********************************************************************************

*** priorities into candidate characteristics
use "../Voters Edge/scrape/output/all_candidates_matched.dta", clear
merge 1:1 multi_candid using "../Voters Edge/priority_rating/dofiles/results/all_candidates_final_demo.dta", nogen
drop if college==1 // drop college board election
drop if leaid==""

* define candidates characteristics you're interested in
g hisp = cond( (pcthisp>0.9 & !missing(pcthisp)),1,0) // (TODO) what's the right cutoff?
g white = cond( (pctwhite>0.8 & !missing(pctwhite)),1,0)
g teacher_care = cond(union_friendly_q5_rate==3|union_friendly_q5_rate==2,1,0)
g budget_hawk = cond( fiscal_conservatism_q3_rate==3|fiscal_conservatism_q3_rate==2,1,0) 
g agenda_bias = cond(agenda_bias_q1_rate==3|agenda_bias_q1_rate==2,1,0)
g against_left = cond(agenda_bias_q2_rate==3|agenda_bias_q2_rate==2,1,0) 
g against_right = cond(agenda_bias_q3_rate==3|agenda_bias_q3_rate==2,1,0)
g academic_prior = cond(academic_concern_rate==3,1,0) 

foreach x in equity facility it overcrowding enrollment dropout safety_health mental_health cte {
	g `x'_prior = cond(`x'_concern_rate==3|`x'_concern_rate==2,1,0)
}

foreach x in parent_involvement police_in_school {
	g `x'_prior = cond(`x'_rate==3|`x'_rate==2,1,0)
}

g agenda_left = cond(agenda_bias_q4_rate==1|agenda_bias_q4_rate==2,1,0)
g agenda_right = cond(agenda_bias_q4_rate==4|agenda_bias_q4_rate==5,1,0)


*** correlation matrix in a heatmap
*keep if academic_concern_rate~=.

keep $list_full
order $list_full

outreg2 using "summary_priority.xls", replace sum(detail) eqkeep(N mean sd p50 min max) label


ren *_prior *
pwcorr *, sig
matrix C = r(C)	

// Assuming matrix C is already created
matrix corr = C

// Get the number of rows (and columns, since it's square)
local n = rowsof(corr)

// Zero out the upper triangle
forvalues i = 1/`n' {
	local i1 = `i'+1
	forvalues j=`i'/`n'{
        matrix corr[`j', `i'] = .
}
}

heatplot corr, values(format(%9.1f) size(2)) color(RdBu, intensity(.6)) aspectratio(1) ///
cuts(-1(.2)1) xlabel(, angle(45)) legend(on  position(7) ring(0)) nodiagonal upper ///
xsize(7) ysize(7) plotregion(margin(zero)) graphregion(margin(2 2 2 2))
gr export "Results/priority_corr.png", replace



********************************************************************************
*	II. RD among priorities
********************************************************************************

global missing_flag =0 // 0 if including missing, 1 if not
if $missing_flag==0 {
	global foldername = "vote_share_with_missing"
	global filename = "corr_rd.dta"
	global figname = "priority_rd.png"
}
if $missing_flag==1 {
	global foldername = "vote_share_no_missing"
	global filename = "corr_rd_no_missing.dta"
	global figname = "priority_rd_no_missing.png"
}

*** 1) run RD for all combination
//dummy
use "Temp/$foldername/vote_share_dist_budget_hawk.dta", clear
tab year, g(yr_)
reg diff_budget_hawk vote_margin 
regsave using "Temp/$filename", ci replace detail(all) addlabel(y,"", x,"")	

g list = "$list"

set scheme white_ptol
foreach treat in $list {
use "Temp/$foldername/vote_share_dist_`treat'.dta", clear

reg diff_`treat' vote_margin 
regsave using "Temp/$filename", ci append detail(all) addlabel(y,"`treat'", x,"`treat'")	// dummy to make later stage easier
 
g list_temp = subinstr("$list", "`treat'", "",.)
local list_temp=list_temp[1]
drop list_temp
	 
foreach var in `list_temp' {
rdrobust diff_`var' vote_margin, p(1) kernel(triangular)
regsave using "Temp/$filename", ci append detail(all) addlabel(y,"`var'", x, "`treat'")	
}
}

*** 2) Plot
use "Temp/$filename", clear
drop if y==""
drop if var=="_cons"
replace coef = . if x==y & y~=""

sort x y 
bysort x: g n=_n

duplicates r x
local R= r(unique_value) 
matrix C = J(`R', `R', .) 

forvalues i = 1/`R' {
	local i1=`i'+1
	forvalues j=1/`R'{
		local n=(`i'-1)*`R'+`j'
        matrix C[`i', `j'] = coef[`n']
}
}

heatplot C, values(format(%9.1f) size(1)) color(RdBu, intensity(.6)) aspectratio(1) ///
cuts(-1(.2)1) xlabel(, angle(45)) legend(on  position(7) ring(0) ) generate 

gen str sig = cond(ci_l_rb > 0 | ci_r_rb< 0, "*","")

heatplot C, color(RdBu, intensity(.6)) legend(off) aspectratio(1) cuts(-1(.2)1)  ///
	xlabel(1 "academic" 2 "agenda_bias" 3 "agenda_left" 4 "agenda_right" 5 "budget_hawk" ///
           6 "cte" 7 "dropout" 8 "enrollment" 9 "equity" 10 "facility" ///
           11 "it" 12 "mental_health" 13 "overcrowding" 14 "parent_involvement" ///
           15 "police_in_school" 16 "safety_health" 17 "teacher_care", angle(45)) ///
    ylabel(1 "academic" 2 "agenda_bias" 3 "agenda_left" 4 "agenda_right" 5 "budget_hawk" ///
           6 "cte" 7 "dropout" 8 "enrollment" 9 "equity" 10 "facility" ///
           11 "it" 12 "mental_health" 13 "overcrowding" 14 "parent_involvement" ///
           15 "police_in_school" 16 "safety_health" 17 "teacher_care") ///
		   ytitle("Vote Margin") xtitle("As Outcome") ///
		   xsize(7) ysize(7) plotregion(margin(zero)) graphregion(margin(2 2 2 2)) ///
    addplot(scatter _Y _X, msym(i) mlab(_Z) mlabf(%9.2f) mlabpos(0) mlabangle(25) mlabc(black) ///
        || scatter _Y _X if _Y!=_X, msym(i) mlab(sig) mlabpos(12) mlabgap(0) mlabc(black)) 
gr export "Results/$figname", replace

		