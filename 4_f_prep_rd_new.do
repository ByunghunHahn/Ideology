
* clean leaid var 
cap ren id_district_nces leaid
tostring leaid, replace  // Convert to string if not already

* rename vocational vars 
cap ren *vocational* *cte*

// Ensure consistent width by adding leading zeros
gen leaid_padded = string(real(leaid), "%07.0f")  // Adjust "8" to the required width
replace leaid = leaid_padded
drop leaid_padded
 
* gen baseline and difference vars 
foreach yv in $y_rev $y_fiscal $y_teacher $y_enroll $y_charter $y_score $y_sup {	
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

