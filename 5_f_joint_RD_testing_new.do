
	use "data_for_rd/$missing/stacked.dta", clear
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
	
	
********************************************************************************
* 1. Generate variables for district level analayis 
********************************************************************************

	preserve
	use "Temp/h0_list_demo_outcome.dta" if $h0_type & outcome_main==1, clear
	if "$h0_type" == "h_type_exact==1" {
		duplicates drop demo outcome, force
	}
	if "$h0_type" ~= "h_type_exact==1" {
		duplicates drop outcome, force
	}	
	
	global n_obs = _N
	restore
	
		forvalues n=1/$n_obs {
			
		preserve
		use "Temp/h0_list_demo_outcome.dta" if $h0_type & outcome_main==1, clear
		drop if demo==prio
		drop priolabel

		bysort outcome: g n = _n
		keep demo prio h_type outcome n subset
		reshape wide demo* prio* h_type* , i(outcome) j(n)
		
		local y = outcome[`n']
		local cond = subset[`n']
		if $flag_iden_only == 1 {
			ds demo* 
		}
		if $flag_iden_only == 2 {
			ds prio* 
		}
		if $flag_iden_only == 0 {
			ds demo* prio*
		}		
		
		local varlist = r(varlist)
		local x ""
		foreach xvar in `varlist' {
			local xx = `xvar'[`n']
			local x "`x' `xx'"
		}
		
		local uniq_x ""
		foreach word in `x' {
			if strpos(" `uniq_x' ", " `word' ") == 0 {
				local uniq_x "`uniq_x' `word'"
			}
		}
		display "`cond'"
		display "`x'"
		display "`uniq_x'"
		restore
	
		big_rd, yvar("`y'") yearcond("year>=year_elected `cond'") treatment("`uniq_x'") kernel("uni")
		ren T2* eq`n'_T2*
		ren m2* eq`n'_m2*
		ren int* eq`n'_int*
		ren group eq`n'_group
		drop T* S* w*
	}

	
********************************************************************************
* 2. Merge with school level funding record
********************************************************************************
 
	ren id_district_nces id_dist_nces
	
	preserve
	use "outcomes_sch_finance.dta", clear
	keep id_dist_nces id_sch_nces year hisp_high frl_high log_exp_per_stu_w enrollment fund_total_pp_w exp_per_stu_w 
	ren enrollment enrollment_sch
	save "Temp/outcomes_sch_finance.dta", replace
	restore
	
	joinby id_dist_nces year using "Temp/outcomes_sch_finance.dta", unm(m) _merge(_sch_fin)

	g sfp_pos = cond(fund_total_pp_w>0,1,0) if !missing(fund_total_pp_w)
	
	foreach y in fund_total_pp_w sfp_pos {	 // annual variable
		g `y'_b = `y' if year==year_elected
		sort multi_raceid id_sch_nces `y'_b
		bysort multi_raceid id_sch_nces: replace `y'_b = `y'_b[1]

		g `y'_D =`y'-`y'_b
	}
	
	foreach y in exp_per_stu_w {	// biannual outcome from CRDC
		cap g `y'_b1 = `y' if year==year_elected-1 
		cap g `y'_b2 = `y' if year==year_elected 

		g `y'_b=.
		sort multi_raceid id_sch_nces `y'_b1
		bysort multi_raceid id_sch_nces: replace `y'_b = `y'_b1[1]

		sort multi_raceid id_sch_nces `y'_b2
		bysort multi_raceid id_sch_nces : replace `y'_b = `y'_b2[1] if missing(`y'_b)
		
		cap g `y'_D =`y'-`y'_b
	}

	egen tag_dist = tag(multi_raceid year)

	
********************************************************************************
* 3. Run district level anals
********************************************************************************

		forvalues n=1/$n_obs {
			
		preserve
		use "Temp/h0_list_demo_outcome.dta" if $h0_type & outcome_main==1, clear
		drop if demo==prio
		drop priolabel

		bysort outcome: g n = _n
		keep demo prio h_type outcome n subset
		reshape wide demo* prio* h_type* , i(outcome) j(n)
		
		local y = outcome[`n']
		local cond = subset[`n']
		display "`cond'"
		restore
		
		eststo: reg `y'_D eq`n'_T2*_S eq`n'_m2*_S eq`n'_int* ib(2016).year i.eq`n'_group if year>=year_elected & tag_dist==1 `cond'
		est store eq`n'
	}	
	
	
********************************************************************************
* 4. Somewhat manuallay run school level anals
********************************************************************************

	if $flag_iden_only ==1 {
		local treat="hisp"
		local spec ="hisp_high##eq_sch1_T2_hisp_S##c.eq_sch1_m2_hisp_S"
		local spec2 ="hisp_high##eq_sch2_T2_hisp_S##c.eq_sch2_m2_hisp_S"
	}
	if $flag_iden_only ==0 {
		local treat="hisp equity_prior"
		local spec ="hisp_high##eq_sch1_T2_hisp_S##c.eq_sch1_m2_hisp_S hisp_high##eq_sch1_T2_equity_prior_S##c.eq_sch1_m2_equity_prior_S"
		local spec2 ="hisp_high##eq_sch2_T2_hisp_S##c.eq_sch2_m2_hisp_S hisp_high##eq_sch2_T2_equity_prior_S##c.eq_sch2_m2_equity_prior_S"
	}
	if $flag_iden_only ==2 {
		local treat="equity_prior"
		local spec ="hisp_high##eq_sch1_T2_equity_prior_S##c.eq_sch1_m2_equity_prior_S"
		local spec2 ="hisp_high##eq_sch2_T2_equity_prior_S##c.eq_sch2_m2_equity_prior_S"
	}
	
	big_rd, yvar("sfp_pos") yearcond("year>year_elected") treatment("`treat'") kernel("uni") sch_level(1)
	ren T2* eq_sch1_T2*
	ren m2* eq_sch1_m2*
	ren int* eq_sch1_int*
	ren group eq_sch1_group
	drop T* S* w* 
	 
	eststo: reg sfp_pos_D `spec' ib(2016).year##i.eq_sch1_group if year>year_elected
	est store eq_sch1

********************************************************************************
* 5. Joint testing
********************************************************************************

	local eq ""
	
	forvalues n=1/$n_obs {
		local eq `eq' eq`n'
	}
	
	suest `eq', r cl(id_election)
	
	matrix list e(b)  // optional: inspect structure
	local colnames : colnames e(b)
	local all_demo ""
	local all_prio ""
	foreach name of local colnames {
		if regexm("`name'", "(T2_hisp|T2_female|T2_democrat_v2|T2_occ_teacher)") {
			if strpos("`name'", "T2_") & strpos("`name'", "sch")<1 & strpos("`name'", "m2_")<1 & strpos("`name'", "0b")<1  {
				local all_demo "`all_demo' `name'"
			}
			if strpos("`name'", "T2_") & strpos("`name'", "sch")>0 & strpos("`name'", "1.hisp_high")>0  & strpos("`name'", "m2_")<1{
				local all_demo "`all_demo' `name'"
			}
		}
		if regexm("`name'", "(T2_hisp|T2_female|T2_democrat_v2|T2_occ_teacher)")==0 {
			if strpos("`name'", "T2_") & strpos("`name'", "sch")<1 & strpos("`name'", "m2_")<1 & strpos("`name'", "0b")<1  {
				local all_prio "`all_prio' `name'"
			}
			if strpos("`name'", "T2_") & strpos("`name'", "sch")>0 & strpos("`name'", "1.hisp_high")>0  & strpos("`name'", "m2_")<1{
				local all_prio "`all_prio' `name'"
			}
		}	
	}
	eststo suest_model
	
	if $flag_iden_only ~= 2 {
	test "`all_demo'"
	scalar p_iden   = r(p)
	scalar chi_iden = r(chi2)
	scalar df_iden  = r(df)
	}
	if $flag_iden_only ~= 1 {	
	test "`all_prio'"
	scalar p_ideo=r(p)
	scalar chi_ideo=r(chi2)
	scalar df_ideo = r(df) 
	}
