********************************************************************************
* Different programs for plots accessed from multiple other dofiles
********************************************************************************


*** 1/ Dynamic plot
	capture program drop plot_rd_dynamic
	program define plot_rd_dynamic, rclass
		syntax , yvar(string) ytitle(string) legend(string asis) ///
			[ylabel(string asis)] [xlabel(string asis)] ///
			[ypos(string asis)] [flag_animation(integer 0)] ///
			[name(string)] 
			
		if "`xlabel'" == "" {
			local xlab "-4(1)4"
		}
		else {
			local xlab `xlabel'
		}
		if `flag_animation' == 1 {
			twoway (scatter coef_sum t if y == "`yvar'", color(white%100) symbol(O)) ///
				   (scatter y0 t if y == "`yvar'", color(gs10%50) symbol(D)) ///
				   , xline(0) text( `ypos' 0 "Election →", place(w) size(med)) xlab(`xlab', labsize(medlarge)) ///
					 ytitle("`ytitle'", size(medlarge)) ///
					 xtitle("Relative Time From Election", size(med)) ///
					 ylabel(`ylabel', labsize(medlarge)) `legend' ///
					 saving("Results/Graph/`name'", replace)
		}
		else {
			twoway (scatter coef_sum t if y == "`yvar'", color(ebblue) symbol(O)) ///
				   (pcspike cl_h t cl_l t if y == "`yvar'", lc(ebblue) symbol(i)) ///
				   (scatter y0 t if y == "`yvar'", color(gs10%50) symbol(D)) ///
				   , xline(0) text( `ypos' 0 "Election →", place(w) size(med)) xlab(`xlab', labsize(medlarge)) ///
					 ytitle("`ytitle'", size(medlarge)) ///
					 xtitle("Relative Time From Election", size(med)) ///
					 ylabel(`ylabel', labsize(medlarge)) `legend' ///
					 saving("Results/Graph/`name'", replace)
		}
	end
	

*** 2/ RD plot
	capture program drop rd_plot
	program define rd_plot
		syntax, yvar(string) yearcond(string) ///
				[ylab(string)] [xlab(string asis)] ///
				[ytitle(string)] [xtitle(string)] ///
				[flag_balance(integer 0)] ///
				[cluster(string)] /// cluster or nncluster 
				[xrange(string asis)] ///
				[name(string)] ///
				[subtitle(string)] ///
				[subset(string)] ///
				[weights(string)] ///
				[level(integer 0)] // =1 if you want y var to be levels instead of difference
		
		if `level'==0 {

		* Run rdrobust
		eststo: rdrobust `yvar'_D vote_margin if `yearcond' `subset', ///
			p(1) kernel(triangular) bwselect(mserd) covs(yr_*) ///
			vce(`cluster' id_election) 

		local bw = e(h_l)
		local coef = round(e(tau_cl), 0.001)
		local ci_l = round(e(ci_l_rb), 0.001)
		local ci_h = round(e(ci_r_rb), 0.001)
		g wgt = 1-abs(vote_margin)/`bw'
		
		eststo: rdrobust `yvar' vote_margin if `yearcond' `subset', ///
			p(1) kernel(triangular) bwselect(mserd) covs(yr_*) ///
			vce(`cluster' id_election) 
		local ymean =  round(e(beta_Y_p_l)[1,1], 0.01)
		
		* Generate mean bins for plotting
		rdplot `yvar'_D vote_margin if abs(vote_margin) <= `bw' & `yearcond' `subset', genvars `weights' binselect(es)
		gen margin_d = 1 - (vote_margin < 0)
		ren (rdplot_mean_y rdplot_mean_bin) (rdmain_mean_y rdmain_mean_bin)
		drop rdplot_*

		* Add second rdplot for visual fill-out (optional)
		rdplot `yvar'_D vote_margin if abs(vote_margin)<=`xrange' & `yearcond' `subset', genvars `weights' binselect(es)

		* Plot with fitted lines and CIs
		tw (scatter rdplot_mean_y rdplot_mean_bin if `yearcond' `subset' & abs(vote_margin) > `bw' & abs(vote_margin)<=`xrange', color(gray)) ///
		   (lfitci `yvar'_D vote_margin if margin_d==1 & abs(vote_margin) <= `bw' & `yearcond' `subset' [weight=wgt], clcolor(dkorange*1.2) color(dkorange*.2)) ///
		   (lfitci `yvar'_D vote_margin if margin_d==0 & abs(vote_margin) <= `bw' & `yearcond' `subset' [weight=wgt], clcolor(ebblue*2) color(ebblue*.2)) ///
		   (scatter rdmain_mean_y rdmain_mean_bin if margin_d==1 & abs(vote_margin) <= `bw' & `yearcond' `subset', color(dkorange*1.2)) ///
		   (scatter rdmain_mean_y rdmain_mean_bin if margin_d==0 & abs(vote_margin) <= `bw' & `yearcond' `subset', color(ebblue*2)) ///
		   , xtitle("Vote Margin") legend(off) graphregion(color(white)) xlabel(-`xrange' 0 `xrange', labsize(*1.5)) xscale(range(-`xrange' `xrange')) ///
		  note("{&beta} = `coef' [`ci_l',`ci_h']", ring(0) pos(1) size(*1.5)) ///
		   ylabel(`ylab') xlabel(`xlab') `ytitle' `xtitle' `subtitle' plotregion(margin(zero)) ///
		   saving("Results/Graph/`name'", replace)
		   *note("{&beta} = `coef' [`ci_l',`ci_h']" "Baseline mean = `ymean'", ring(0) pos(1) size(*1.5)) ///
		
		}   
		
		if `level'==1 {
			
		* Run rdrobust
		eststo: rdrobust `yvar' vote_margin if `yearcond' `subset', ///
			p(1) kernel(triangular) bwselect(mserd) covs(yr_*) ///
			vce(`cluster' id_election) 

		local bw = e(h_l)
// 		local coef = round(e(tau_cl), 0.01)
// 		local ci_l = round(e(ci_l_rb), 0.01)
// 		local ci_h = round(e(ci_r_rb), 0.01)
		
		local coef : display %4.2f e(tau_cl)
		local ci_l : display %4.2f e(ci_l_rb)
		local ci_h : display %4.2f e(ci_r_rb)
	 
		local ymean =  round(e(beta_Y_p_l)[1,1], 0.01)
		
		g wgt = 1-abs(vote_margin)/`bw'
		
		* Generate mean bins for plotting
		rdplot `yvar' vote_margin if abs(vote_margin) <= `bw' & `yearcond' `subset', genvars `weights' binselect(es)
		gen margin_d = 1 - (vote_margin < 0)
		ren (rdplot_mean_y rdplot_mean_bin) (rdmain_mean_y rdmain_mean_bin)
		drop rdplot_*

		* Add second rdplot for visual fill-out (optional)
		rdplot `yvar' vote_margin if abs(vote_margin)<=`xrange' & `yearcond' `subset', genvars `weights' binselect(es)

		* Plot with fitted lines and CIs
		tw (scatter rdplot_mean_y rdplot_mean_bin if `yearcond' `subset' & abs(vote_margin) > `bw' & abs(vote_margin)<=`xrange', color(gray)) ///
		   (lfitci `yvar' vote_margin if margin_d==1 & abs(vote_margin) <= `bw' & `yearcond' `subset' [weight=wgt], clcolor(dkorange*1.2) color(dkorange*.2)) ///
		   (lfitci `yvar' vote_margin if margin_d==0 & abs(vote_margin) <= `bw' & `yearcond' `subset' [weight=wgt], clcolor(ebblue*2) color(ebblue*.2)) ///
		   (scatter rdmain_mean_y rdmain_mean_bin if margin_d==1 & abs(vote_margin) <= `bw' & `yearcond' `subset', color(dkorange*1.2)) ///
		   (scatter rdmain_mean_y rdmain_mean_bin if margin_d==0 & abs(vote_margin) <= `bw' & `yearcond' `subset', color(ebblue*2)) ///
		   , xtitle("Vote Margin") legend(off) graphregion(color(white)) xlabel(-`xrange' 0 `xrange', labsize(*1.5)) xscale(range(-`xrange' `xrange')) ///
		   note("{&beta} = `coef' [`ci_l',`ci_h']" "Baseline mean = `ymean'", ring(0) pos(1) size(*2)) ///
		   ylabel(`ylab') xlabel(`xlab') `ytitle' `xtitle' `subtitle' plotregion(margin(zero)) ///
		   saving("Results/Graph/`name'", replace)  	
		}
		   
	end
	

*** 3/ Coefficient plot
capture program drop plot_rd_coef
program define plot_rd_coef, rclass
	syntax, gr(string) ///
		[subtitle(string)] ///
		name(string) ///
		ylabel(string asis) ///
		[xlabel(string asis)] ///
		[flag_animation(integer 0)]

	* Set color (can extend this later if needed)
	if `flag_animation' == 1 {
		local color "ebblue%0"
	}
	else if `flag_animation' == 0 {
		local color "ebblue"
	}

	// Set dynamic title based on ygroup
	if `gr' == 1 {
		local title_text "Focal Outcome:"
	}
	else if `gr' == 2 {
		local title_text "Mechanism:"
	}
	else if `gr' == 3 {
		local title_text "District Performance:"
	}

	// Plot
	twoway ///
		(scatter n coef if group == `gr', color(`color') symbol(O) msize(medlarge)) ///
		(pcspike n cl_l n cl_h if group == `gr', color(`color')), ///
		legend(off) xline(0) ytitle("") ///
		title("`title_text'") subtitle("`subtitle'", box) ///
		xsize(3) ysize(4) ylabel(`ylabel', labsize(medlarge)) xlabel(`xlabel') ///
		saving("Results/Graph/`name'", replace)
end
	