	replace state_leaid = subinstr(state_leaid, "CA-","",.)
	ren state_leaid cds
	destring cds, replace	

	* Merge teacher salary
	merge m:1 year cds using "../CDE/Certificated Salaries & Benefits/TSAL Outcome.dta", gen(_tsal) force keep(matched master) 
	merge m:1 year using "cpi.dta", keep(master matched) nogen

	ds *sal*
	local varlist = r(varlist)
	foreach x in `varlist' {
		replace `x'=. if `x'<0
		replace `x' = `x' * cpi
	}

	*** outcomes: for each class, 1) avg salary fixing composition 2) avg salary fixing salary
	* note: ideally, we would do this across all classes and steps. but districts often change classes (~ 7% in year x dist obs), and this makes it hard. e.g. if BA + 75 or more is splitted into BA + 75 or more and BA + 90 or more, we don't know what step BA + 90 teachers fell in withint the prior system
	  
	*** fill out the baseline value + generate FTE  
	foreach x in $class {
	gen `x'_totalBfte = 0
	gen `x'_totalfte =0 		
	
	forvalues i=1/40 {
	g `x'_Bfte`i' =`x'_fte`i' if year==year_elected-1
	sort multi_raceid `x'_Bfte`i'
	bysort multi_raceid: replace `x'_Bfte`i' = `x'_Bfte`i'[1] if missing(`x'_Bfte`i') 

	g `x'_Bsalary`i' =`x'_salary`i' if year==year_elected-1
	sort multi_raceid `x'_Bsalary`i'
	bysort multi_raceid: replace `x'_Bsalary`i' = `x'_Bsalary`i'[1] if missing(`x'_Bsalary`i') 
	
	replace `x'_totalBfte = `x'_totalBfte + `x'_Bfte`i' if !missing(`x'_Bfte`i')
	replace `x'_totalfte = `x'_totalfte + `x'_fte`i' if !missing(`x'_fte`i')
	
	recode `x'_totalBfte (0=.)
	recode `x'_totalfte (0=.)
	
	}
	}

	*** generate weighted mean of pctchg & salary : `x'_pctwght `x'_sal_Bcomp
	foreach x in $class {
	  gen `x'_sal_Bcomp = 0
	  gen `x'_sal_Bsal=0
	  gen `x'_sal_wght =0
	  gen `x'_sal_rep = -99
	  gen `x'_sal_rep_fte=-99
	
	  forvalues i = 1/40 {
		replace `x'_sal_Bcomp = `x'_sal_Bcomp + (`x'_salary`i' * `x'_Bfte`i')/`x'_totalBfte if !missing(`x'_Bfte`i') & !missing(`x'_salary`i')
		replace `x'_sal_Bsal = `x'_sal_Bsal + (`x'_Bsalary`i' * `x'_fte`i')/`x'_totalfte if !missing(`x'_fte`i') & !missing(`x'_Bsalary`i')
		replace `x'_sal_wght = `x'_sal_wght + (`x'_salary`i' * `x'_fte`i')/`x'_totalfte if !missing(`x'_fte`i') & !missing(`x'_salary`i')
		
		replace `x'_sal_rep=`x'_salary`i' if `x'_fte`i'>`x'_sal_rep_fte & !missing(`x'_fte`i')
		replace `x'_sal_rep_fte=`x'_fte`i' if `x'_fte`i'>`x'_sal_rep_fte & !missing(`x'_fte`i')
	  }
	  
	replace `x'_sal_Bcomp=. if `x'_sal_Bcomp==0
	replace `x'_sal_Bsal=. if `x'_sal_Bsal==0
	replace `x'_sal_wght=. if `x'_sal_wght==0
	
	gen `x'_sal_Bcomp_log = log(`x'_sal_Bcomp)
	gen `x'_sal_Bsal_log = log(`x'_sal_Bsal)
	gen `x'_sal_wght_log = log(`x'_sal_wght)
	
	replace `x'_sal_Bcomp=`x'_sal_Bcomp/1000
	replace `x'_sal_Bsal=`x'_sal_Bsal/1000
	replace `x'_sal_wght=`x'_sal_wght/1000
	
	replace `x'_sal_rep=. if `x'_sal_rep==-99
	gen `x'_sal_rep_log = log(`x'_sal_rep)
    replace `x'_sal_rep = `x'_sal_rep / 1000
	}

