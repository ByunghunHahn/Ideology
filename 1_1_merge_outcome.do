********************************************************************************
* Merge All Outcome Files
* Written by Minseon Park 04/30/2024
* Last updated 10/24/24
********************************************************************************

*ssc install winsor 

set more off
set matsize 5000
set emptycells drop

* set directory 
cd "~/Library/CloudStorage/Dropbox/California Election Data/Code"
global logpath "~/Library/CloudStorage/Dropbox/California Election Data/Logs"
global cde "~/Library/CloudStorage/Dropbox/California Election Data/CDE/Data/Cleaned"



local today : di %td date("$S_DATE", "DMY")

cap log close
log using "$logpath/1_merge_all_`today'.log", replace


********************************************************************************
* 0. Other Data Sets To Be Merged
********************************************************************************

*** CPI 
import excel "SeriesReport-20240508085814_4cf1fc.xlsx", sheet("BLS Data Series") cellrange(A12:O10000) firstrow clear
keep Year Dec
ren (Year Dec) (year dec)

drop if year==.
g cpi_base = dec if year==2019
sort cpi_base
replace cpi_base = cpi_base[1]
g cpi = dec/cpi_base 
keep year cpi
save "cpi.dta", replace

*** Charter enrollment (CCD)
use "../CCD/ccd_school_cleaned.dta", clear
keep if charter==1
recode enrollment (-1=.) (-2=.) (-3=.)
collapse (sum) enrollment (count) n_charter_sch=enrollment, by(id_district_state year)
ren (id_district_state enrollment) (id_dist_state enrollment_charter)
la var enrollment_charter "CCD: enrollment in charter"
save "Temp/enroll_charter.dta", replace

*** CRDC behavioral outcomes
use "../CRDC/dta/crdc_crosswalk_schools.dta", clear 
foreach x in combokey nces_sch1112 nces_sch1415 nces_sch1516 nces_sch1617 nces_sch1718 nces_sch1819 {
	replace `x' = substr(`x',1,7)
}

drop sch_name sch_name_crdc
duplicates drop
ren combokey leaid 

ds nces_sch*
local varlist = r(varlist)
foreach x in `varlist' {
	gsort leaid -`x'
	bysort leaid: replace `x' = `x'[1] if `x'==""
}
duplicates drop
duplicates r leaid
bysort leaid: g N=_N
br if N>=2 // 31; so wierd and no hope to merge cleanly with CCD
duplicates drop leaid, force
drop N
save "Temp/crdc_crosswalk_dist.dta",replace 

use "../CRDC/dta/CRDC_behavioral_districts_clean.dta", clear
merge m:1 leaid using "Temp/crdc_crosswalk_dist.dta", keep(matched) nogen

g id_dist_nces = ""
foreach t in 1112 1415 1516 1617 1718 1819 {
replace id_dist_nces=nces_sch`t' if (year==2009|year==2011|year==2013) & id_dist_nces==""
}
foreach t in 1516 1415 1617 1718 1112 1819 {
replace id_dist_nces=nces_sch`t' if year==2015 & id_dist_nces==""
}
foreach t in 1718 1516 1617 1415 1819 1112 {
replace id_dist_nces=nces_sch`t' if year==2017 & id_dist_nces==""
}
foreach t in 1819 1718 1617 1516 1415 1112 {
replace id_dist_nces=nces_sch`t' if year==2020 & id_dist_nces==""
}
replace id_dist_nces=leaid if id_dist_nces==""
duplicates r id_dist_nces year // multiple obs - charter schools

drop nces_sch*

ren dist_* * 
su arrest_f_bla-sus_outschool_mult_tot_whi
ds arrest_f_bla-sus_outschool_mult_tot_whi
local varlist = r(varlist)
foreach x in `varlist' {
	bysort id_dist_nces year: egen `x'_agg = sum(`x')
	replace `x'=`x'_agg if !missing(`x'_agg)
}
drop *_agg
su arrest_f_bla-sus_outschool_mult_tot_whi

keep id_dist_nces year arrest_f_bla-sus_outschool_mult_tot_whi
duplicates drop 
save "Temp/CRDC_behavioral_districts_clean_clean.dta", replace

*** Bond
use  "../Bond & HPI/ca_bonds.dta", clear
replace year=year-1
save "Temp/ca_bonds.dta",replace



********************************************************************************
* I. CCD aggregate charter schools
// geodist before 2017, admindist after 2018 (charter school separate after 2018)
********************************************************************************

use "../CCD/ccd_cleaned.dta" if year>1990, clear //leaid year
merge m:1 id_dist_state year using "../CDE/pubschls.dta", gen(_charter) keep(matched master)
merge m:1 id_dist_state year using "Temp/enroll_charter.dta", gen(_charter_enroll) keep(matched master)
replace enrollment_charter = 0 if enrollment_charter==.

merge m:1 year using  "cpi.dta", keep(matched) nogen

*** check merge accuracy - looks good
tab year _charter 
bysort id_dist_nces: egen year_min = min(year)
tab _charter if year_min==2018 & year==2018 // master only- state governed

*** aggregate charter 1 case into LEA
// what about charter 2 case? ignore for now
count if ncesdist1~=ncesdist2 & ncesdist1~="" & ncesdist2~="" // 100 out of about 5000 charter x year obs

g id_dist_char_nces = id_dist_nces if _charter==3
replace id_dist_nces = ncesdist1 if _charter==3 & ncesdist1~="" & ncesdist1~="no data"

duplicates tag id_dist_nces year, gen(n_charter)
g yes_charter = cond(n_charter>=1,1,0)
bysort id_dist_nces: egen ever_yes_charter = max(yes_charter)
count if yes_charter==1 & _charter==1 & year==2018 // 247 LEA from which charter schools became separate

	*** 1) FTE variables
	ds enrollment-sprt_staff_other_fte enrollment_charter
	local varlist = r(varlist)
	foreach x in `varlist' {
		eststo clear
		eststo: qui reg `x' ib(2017).year##i.ever_yes_charter if _charter==1 & year>=2000 & year<=2022
		bysort id_dist_nces year: egen `x'_agg = sum(`x')
		replace `x' = `x'_agg if yes_charter==1 & _charter==1 
		eststo: qui reg `x' ib(2017).year##i.ever_yes_charter if _charter==1 & year>=2000 & year<=2022
		esttab, keep(*year#1.ever_yes_charter)
	}
	drop *_agg

	g inst_aid_fte = lea_inst_coord_fte+inst_aides_fte
	la var inst_aid_fte "CCD: school instructional aid + LEA instructional coordinators"
	
	g staff_total_fte_v2 =tch_total_fte+sch_staff_total_fte_v2+sprt_staff_other_fte+lea_staff_total_fte 
	// note: 1) original staff_total_fte is tch_total_fte+sch_staff_total_fte+sprt_staff_other_fte+lea_staff_total_fte; but sch_staff_total_fte starts only from 2020 
	// 2) staff_total_fte_v2 should be missing some staffs but covers longer period 
	// 3) plus, staff_total_fte is wrong after the aggregation since for school staff members, it treats charter schools separately
	drop staff_total_fte
	order lea_*fte inst_aid_fte staff_total_fte_v2, before(censusid)
		
	eststo clear 
	foreach x in inst_aid_fte staff_total_fte_v2 {
	eststo: qui reg `x' ib(2017).year##i.ever_yes_charter if _charter==1 & year>=2000 & year<=2022
		}
	esttab, keep(*year#1.ever_yes_charter)
	
	la var staff_total_fte_v2 "CCD: Total full-time equivalent staff (made by us)"
	drop tch_kindergarten_fte-tch_ungraded_fte
	drop inst_aides_fte-sch_staff_total_fte
	drop sch_psychologists_fte-lea_admin_sprt_staff_fte
	
	g tch_stu_ratio =  tch_total_fte/enrollment
	g staff_stu_ratio = staff_total_fte /enrollment
	la var tch_stu_ratio "CCD: FTE-weighted Teacher per pupil"
	la var staff_stu_ratio "CCD: FTE-weighted Staff per pupil"

	
	*** 2) budget variables 
	// unlike FTE variables, aggregation is not that simple
	// ever charter districts do show 10% decrease in funding after 2018, but aggregated funding increases by 15% after 2018, implying before 2017, some money for charter schools were not included in the main district since most of them are directly funded   
	// seems to safer to use only up to year 2017 as budget variables
	g log_exp_total= log(exp_total)
	eststo clear
	cap eststo: qui reghdfe log_exp_total ib(2017).year##i.ever_yes_charter if _charter==1 & year>=2000 & year<=2022, absorb(id_dist_nces)
	bysort id_dist_nces year: egen exp_total_agg = sum(exp_total)
	*drop log_exp_total
	*g log_exp_total = log(exp_total_agg)
	cap eststo: qui reghdfe log_exp_total_agg ib(2017).year##i.ever_yes_charter if _charter==1 & year>=2000 & year<=2022, absorb(id_dist_nces)
	esttab, keep(*year#1.ever_yes_charter) 
	drop *_agg log_*
		

	// so just make log variable and adjust with CPI
	ds rev_* exp_* payments_* sal_* bnfit_* debt_* assets_* 
	local varlist = r(varlist)
	foreach x in `varlist' {
		replace `x' = `x' * cpi
		g log_`x' = log(`x')		
		local label : variable label `x'
		la var log_`x' "`label'"		
	}
	
	drop exp_sped_inst-exp_sped_trans_sprt_services
	drop exp_cares_act_inst-exp_cares_act_food
	drop exp_cares_act_current exp_arra
	drop exp_other_elsec exp_supp_serv_nonspec exp_nonelsec_other
	
	g rev_vocational = rev_f_state_vocational+rev_s_vocational_ed 
	g rev_lunch = rev_f_child_nutrition_act +rev_s_sch_lunch+rev_l_sch_lunch
	g rev_special_ed = rev_s_special_ed +rev_f_state_idea
	ren rev_f_state_bilingual_ed rev_bilingual_ed
	ren rev_s_compens_basic_ed rev_s_basic_ed
	ren rev_s_gifted_talented rev_s_gte
	ren rev_f_state_drug_free rev_f_s_drug_free
	
	foreach x in exp_total exp_inst_total exp_serve_total exp_capital_total debt_lterm_end_fy sal_inst bnfit_inst {
		g `x'_per_stu = `x'/enrollment
		g log_`x'_per_stu=log(`x'/enrollment)
		
		g `x'_per_tch = `x'/tch_total_fte
		g log_`x'_per_tch=log(`x'/tch_total_fte)
		
		local label : variable label `x'
		la var `x'_per_stu "`label' per pupil"	
		la var `x'_per_tch "`label' per teacher"
		la var log_`x'_per_stu "`label' per pupil"	
		la var log_`x'_per_tch "`label' per teacher"
	}

		foreach x in rev_vocational rev_lunch rev_special_ed rev_bilingual_ed rev_s_basic_ed rev_s_gte rev_f_s_drug_free {
		local oldlabel : variable label `x'
		local newlabel = subinstr("`oldlabel'", " (1k)", "", .)
		label variable `x' "`newlabel'"
	
		g `x'_per_stu = 1000*`x'/enrollment
		g log_`x'_per_stu=log(`x'/enrollment)
		
		g `x'_per_tch = 1000*`x'/tch_total_fte
		g log_`x'_per_tch=log(`x'/tch_total_fte)
		
		local label : variable label `x'
		la var `x'_per_stu "`label' per pupil"	
		la var `x'_per_tch "`label' per teacher"
		la var log_`x'_per_stu "`label' per pupil"	
		la var log_`x'_per_tch "`label' per teacher"
	}
	
	g exp_inst_share = exp_inst_total/exp_total
	g exp_service_share = exp_serve_total/exp_total
	g exp_capital_share = exp_capital_total/exp_total
	
	la var rev_vocational "CCD: Revenue for Vocational Program"
	la var rev_lunch "CCD: Revenue for Lunch/Nutrition Program"
	la var rev_special_ed "CCD: Revenue for Special Ed"

	g wage_inst_per_tch = sal_inst_per_tch + bnfit_inst_per_tch
	g log_wage_inst_per_tch = log(sal_inst_per_tch + bnfit_inst_per_tch)

	*** 3) enrolllment 
	// should be aggregated including charter schools after 2018
	ds enrollment_*
	local varlist = r(varlist)
	foreach x in `varlist' {
		eststo clear
		eststo: qui reg `x' ib(2017).year##i.ever_yes_charter if _charter==1 & year>=2000 & year<=2022
		bysort id_dist_nces year: egen `x'_agg = sum(`x')
		replace `x' = `x'_agg if yes_charter==1 & _charter==1 
		eststo: qui reg `x' ib(2017).year##i.ever_yes_charter if _charter==1 & year>=2000 & year<=2022
		esttab, keep(*year#1.ever_yes_charter)
	}
	drop *_agg
	
	ds enrollment_white-enrollment_2race
	local varlist = r(varlist)
	foreach x in `varlist' {
		g `x'_share=`x'/enrollment
		local label : variable label `x'
		la var `x'_share "`label' share"	
	}

	*** 4) grad rate
	// charter schools don't report graduation rate post 2018...
	// but the number of counted students do decrease at 2018, implying before 2017, charter schools were included 
	// seems to safer to use only up to year 2017 as budget variables
	su grad_rate_all if _charter==3

	eststo clear
	ds grad*N
	local varlist = r(varlist)
	foreach x in `varlist' {
		eststo: qui reghdfe `x' ib(2017).year##i.ever_yes_charter if _charter==1 & year>=2000 & year<=2022, absorb(id_dist_nces)
	}
	esttab, keep(*year#1.ever_yes_charter)
	
	
	*** 5) test proficiency 
	// seems that even before 2017, charter school were not included in the LEA record	
	// seems to safer to use only up to year 2017 as budget variables
	ren (read_test_pct_prof_midpt math_test_pct_prof_midpt) (read_prof_midpt math_prof_midpt)
	
	su math_test_num_valid if _charter==3
	eststo clear
	foreach x in math read {		
		g log_`x'_test_num = log(`x'_test_num_valid)
		eststo: qui reghdfe log_`x'_test_num ib(2017).year##i.ever_yes_charter if _charter==1 & year>=2000 & year<=2022, absorb(id_dist_nces)
		
		bysort id_dist_nces year: egen `x'_test_num_valid_agg = sum(`x'_test_num_valid)
		drop log_`x'_test_num
		g log_`x'_test_num = log(`x'_test_num_valid_agg)
		eststo: qui reg log_`x'_test_num ib(2017).year##i.ever_yes_charter if _charter==1 & year>=2000 & year<=2022
		drop `x'_test_num_valid_agg log_`x'_test_num
		
	}
	esttab, keep(*year#1.ever_yes_charter)
	

drop district_id -est_population_5_17_pct
drop if _charter==3 // drop all charter schools
	
	
********************************************************************************
* II. Merge with Other Data Sets
********************************************************************************

*** 1/ SEDA
g sedalea = id_dist_nces
destring sedalea, replace
merge 1:1 sedalea year using "../SEDA/seda_cleaned_geodist.dta", gen(_seda)
tab year _seda 

ds cs_mn*
local varlist = r(varlist)
foreach x in `varlist' {
	replace `x' = `x'/2.86
}

g hsp_test= (cs_mn_hsp_mth+cs_mn_hsp_ela)/2
g ecd_test= (cs_mn_ecd_mth+cs_mn_ecd_ela)/2
g wht_test= (cs_mn_wht_mth+cs_mn_wht_ela)/2
g nec_test= (cs_mn_nec_mth+cs_mn_nec_ela)/2

la var hsp_test "SEDA: Hispanic Test Scores"
la var ecd_test "SEDA: ECD Test Scores"
la var wht_test "SEDA: White Test Scores"
la var nec_test "SEDA: NECD Test Scores"
  
/* 
  CCD: Academic year |    Matching result from merge
     (fall semester) | Master on  Using onl  Matched ( |     Total
---------------------+---------------------------------+----------
                2008 |       378         13        758 |     1,149 
                2009 |       441         12        755 |     1,208 
                2010 |       436         10        761 |     1,207 
                2011 |       436          8        760 |     1,204 
                2012 |       450          5        743 |     1,198 
                2013 |     1,184          0          0 |     1,184 
                2014 |       434          1        748 |     1,183 
                2015 |       432          1        739 |     1,172 
                2016 |       430          0        741 |     1,171 
                2017 |       429          0        736 |     1,165 
                2018 |       419          0        740 |     1,159 */
drop if _seda==2
				
*** 2/ CDE - Teachers
destring id_dist_state, replace
merge 1:1 id_dist_state year using "../CDE/Data/Cleaned/teacher_by_district.dta", gen(_teacher) keepus(teacher* staff* districtname) 
// checked: only 10 using only, some charter school name starting with sbe
// checked: only master cases surge starting from 2018
// CCD started to count charter schools as separate districts starting from 2018
tab year _teacher
ren districtname district_name_cde
drop if _teacher==2

//sanity check; 0.999. first from CCD second Min made using CDE
corr tch_total_fte teacher_N_fte 


**** 3/ HPI & Bond
rename sedalea leaid
merge 1:1 leaid year using "../Bond & HPI/hpi.dta", gen(_hpi) 
drop if leaid<600001|leaid>700000 // other states
tab year _hpi
drop if _hpi==2


merge m:1 leaid year using "Temp/ca_bonds.dta",  gen(_bond) keep(master match)
replace amount = 0 if _bond==3 & missing(amount)

g bond_total=0 if _bond==3
ds busvehicle-infra
local list = r(varlist)
	foreach y in `list' {
		replace bond_total=bond_total+ `y' if !missing(`y')
		replace `y' = 0 if _bond==3 & missing(`y')
	}
su bond_total, de
replace amount = amount*cpi
g bond_amount_per_stu = cpi*amount/enrollment
la var bond_amount_per_stu "Bond: amoung per pupil"


*** 4/ CRCD behavioral outcomes 
merge 1:1 id_dist_nces year using "Temp/CRDC_behavioral_districts_clean_clean.dta", gen(_crdc_behv) 
drop if _crdc_behv==2
g sus_tot = sus_inschool_tot+ sus_outschool_mult_tot
g sus_tot_hisp = sus_inschool_tot_his+ sus_outschool_mult_tot_his
g legal_tot = reflaw_tot+arrest_tot
g legal_tot_hisp =arrest_tot_his+reflaw_tot_his

g sus_share = sus_tot/enrollment
g sus_share_hisp = sus_tot_hisp/enrollment_hisp
g legal_share = legal_tot/enrollment
g legal_share_hisp = legal_tot_hisp/enrollment_hisp

g sus_tot_fem = sus_inschool_f_tot+sus_outschool_mult_f_tot 
g sus_tot_mal = sus_inschool_m_tot+sus_outschool_mult_m_tot 
g legal_tot_fem = reflaw_f_tot+arrest_f_tot
g legal_tot_mal = reflaw_m_tot+arrest_m_tot

g sus_share_fem = 2*sus_tot_fem/enrollment
g sus_share_mal = 2*sus_tot_mal/enrollment
g legal_share_fem = 2*legal_tot_fem/enrollment
g legal_share_mal = 2*legal_tot_mal/enrollment

ds legal_share* sus_share*
local varlist = r(varlist)
foreach x in `varlist' {
		ren `x' `x'_t
		winsor `x'_t, gen(`x') p(0.025) high
		drop `x'_t
}


*** 5/ superintendent variables
merge m:1 leaid year using "$cde/superintendents.dta", gen(_super)
la var sup_change "CDE: Superintendent change indicator"
 

*** 6/ using all possible info, generate predicted score variable
g enrollment_charter_share = enrollment_charter/enrollment
g test = (cs_mn_all_ela+cs_mn_all_mth)/2
eststo clear
foreach y in test {
eststo: reghdfe `y' enrollment_black_share enrollment_hisp_share enrollment_asian_share perspeced perell perecd perfrl povertyall tch_stu_ratio, absorb(id_dist_state year) resid
predict `y'_pred, xbd
gen `y'_resid = `y' - `y'_pred
}
esttab, drop(_cons)

la var exp_inst_share  "CCD: Share of Instruction spending"
la var exp_service_share "CCD: Share of Service spending"
la var exp_capital_share "CCD: Share of Capital spending"
la var enrollment_charter_share "CCD: Share of Charter Enrollment"

*county office of education
g cnty_office_temp = cond(strpos(lea_name,"county office of educ"),1,0)
bysort id_dist_state: egen cnty_office = max(cnty_office_temp)
// this is based on the name; CEDA vote results has independent variable 
// there are small N of districts that are county office of ed without that wording in the distrct name. Thus we use CEDA variable as the main variable 
drop *temp*

eststo clear
drop leaid_num
destring id_dist_nces, replace
count if id_dist_nces~=leaid
drop id_dist_nces
save "outcomes.dta", replace

log c
