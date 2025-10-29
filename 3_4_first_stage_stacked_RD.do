version 18.5
clear all
set seed 1234

* set directory 
cd "C:\Users\hahn0\Desktop\Hahn_Park\Code"
global logpath "New_Logs"

global list_demo = "hisp female occ_teacher democrat_v2"
global list_prio ="equity_prior budget_hawk agenda_bias parent_involvement cte_prior dropout_prior enrollment_prior facility_prior safety_health_prior teacher_care sup_concern score_concern"

global missing="with_missing"
global datapath = "data_for_rd/$missing"
	
	
********************************************************************************
* I. Run Super-stacked RD
********************************************************************************

	use "$datapath/stacked.dta", clear
	
	tostring id_district_nces, replace  // Convert to string if not already	
	gen leaid = string(real(id_district_nces), "%07.0f")  // Adjust "8" to the required width
	tab year, g(yr_)
	egen id_election = group(leaid year_elected)
	
	*** merge with composition
	merge m:1 leaid year using "Temp/board_composition.dta", keep(master matched) gen(_comp)	
	

	foreach y in $list_prio {
		g `y'_share_b = `y'_share if year==year_elected-1
		sort multi_raceid `y'_share_b
		bysort multi_raceid: replace `y'_share_b = `y'_share_b[1]

		g `y'_share_D =`y'_share-`y'_share_b
	}
			
	keep if year>=year_elected & year<=year_elected+3
	save "Temp/data_for_rd_stacked_first_stage.dta", replace			
			
	
********************************************************************************
* II. Run Stacked Data
********************************************************************************	
	
	* Define categories
	local filters h_type_exact==1 prio~=demo&h_type_exact==2 prio==demo
	
	* Create empty matrix to store results
	mat results = J(3,4,.)
	local i = 1
	
	foreach f of local filters {
		* Step 1: Filter h0_list and set up pairs
		use Temp/h0_list_demo_outcome.dta, clear
		
		// can't run the full code with (demo=="democrat_v2" & strpos(outcome,"sup")) somehow.
		// this combo is 1 pair out of 46 identities (other) pairs
		keep if `f'
		duplicates drop demo prio, force 
		
		gen pair_id = _n
		local npairs = _N

		tempfile stacked
		save `stacked', emptyok replace
		
		forvalues j = 1/`npairs' {
			local xvar = demo[`j']
			local yvar = prio[`j']

			preserve
				use "Temp/data_for_rd_stacked_first_stage.dta", clear
				gen outcome_D = `yvar'_share_D
				gen vote_margin  = m_`xvar'
				rdrobust outcome_D vote_margin, p(1) kernel(triangular) bwselect(mserd) covs(yr_*) vce(cluster id_election)
				replace outcome_D= -outcome_D if e(tau_cl)<0
				
				gen pair_id = `j'
				keep outcome_D vote_margin year* yr_* id_election pair_id
				*keep if !missing(outcome_D, vote_margin)
				append using `stacked'
				save `stacked', replace
			restore
		}

		use `stacked', clear
		tab pair_id, gen(pair_id_)
		
		rdrobust outcome_D vote_margin, p(1) kernel(triangular) bwselect(mserd) covs(yr_* pair_id_*) vce(cluster id_election)
		
		* Extract point estimate and CIs
		mat results[`i',1] = e(tau_cl)
		mat results[`i',2] = e(ci_l_rb)
		mat results[`i',3] = e(ci_r_rb)
		mat results[`i',4] = `npairs'
		local ++i
	}
	
	
	* Save matrix as dataset for graphing
	clear
	svmat results, names(col)
	gen group = _n

	* Add group labels
	label define grouplab 1 "Identities (existing literature)" 2 "Identities (other)" 3 "Ideologies"
	label values group grouplab
	
	twoway (bar c1 group if group==2, barwidth(0.7) color(dkorange) ///
     mlabel(c1) mlabposition(1) mlabgap(0.5) mlabcolor(black) mlabformat(%9.2f) mlabsize(med)) ///
    (rcap c2 c3 group if group==2, lc(black)) ///
    (bar c1 group if group==3, barwidth(0.7) color(ebblue) ///
     mlabel(c1) mlabposition(1) mlabgap(0.5) mlabcolor(black) mlabformat(%9.2f) mlabsize(med)) ///
    (rcap c2 c3 group if group==3, lc(black)) ///
	(bar c1 group if group==1, barwidth(0.7) color(dkorange*2) ///
     mlabel(c1) mlabposition(1) mlabgap(0.5) mlabcolor(black) mlabformat(%9.2f) mlabsize(med)) ///
    (rcap c2 c3 group if group==1, lc(black)) ///
, ytitle("Mean Effect on Share with Priority", size(medlarge)) ///
  legend(order( 5 "Identities (existing lit.)" 1 "Identities (augmented)" 3 "Ideologies") col(3) pos(6) size(medlarge)) ///
  xtitle("") xla(, labcolor(bg) tlength(0))

	gr export "Results/New_Graph/first_stage_bar.png",replace			
