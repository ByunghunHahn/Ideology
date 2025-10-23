********************************************************************************
* RD with Indidividual Characteristics
* Written By Minseon Park 11/18/24
********************************************************************************

version 18.5

set seed 1234

* set directory 
cd "~/Library/CloudStorage/Dropbox/California Election Data/Code"
global logpath "~/Library/CloudStorage/Dropbox/California Election Data/Logs"

global y_rev =""
global y_fiscal= "exp_total_per_stu "
global y_teacher = ""
global y_score = "ecd_test "
global y_enroll = ""
global y_behv = ""
global y_sup =""

********************************************************************************
* SET PARAMETERS
********************************************************************************

global flag_balance = 0 // 0 if main rd 1 if balance

if $flag_balance==0 {
 global year_cond1 = "year>=year_elected"
 global year_cond2 = "year>=year_elected & year<=year_elected+3"
}	
else if $flag_balance==1 {
 global year_cond1 = "year==year_elected-1"
 global year_cond2 = "year<year_elected"
} 

********************************************************************************
********************************************************************************

********************************************************************************
* Generate Voting Analysis Table using texdoc
* Based on RD analysis with individual characteristics
********************************************************************************

* Create directories
capture mkdir "Results"
capture mkdir "Results/Tex"

* Initialize texdoc
capture file close _all
texdoc init "Results/Tex/voting_analysis_table.tex", replace force

* Write table header
// texdoc write \begin{table}[htbp]
// texdoc write \centering
// texdoc write \caption{Voting Analysis by Category}
// texdoc write \resizebox{\textwidth}{!}{%
texdoc write \label{tab:motions_analysis}
texdoc write \begin{tabular}{llccccccc}
texdoc write \toprule \hline
texdoc write & & \multicolumn{2}{c}{{Equity}} & \multicolumn{2}{c}{{Fiscal}} & \multicolumn{2}{c}{{Hispanic}} \\
texdoc write \cmidrule(lr){2-3} \cmidrule(lr){4-5} \cmidrule(lr){6-7}
texdoc write & {Focal} & {All} & {Focal} & {All} & {Equity} & {Fiscal} \\
texdoc write & (1) & (2) & (3) & (4) & (5) & (6) \\
texdoc write \midrule

********************************************************************************
* Function to run RD analysis and store results
********************************************************************************
cap program drop run_rd_analysis
program define run_rd_analysis
    args dataset outcome
    
    use "data_for_rd/with_missing/dist_votes_`dataset'.dta" if dist_cnty~=1, clear
    g charter_any = cond(agency_charter_indicator==2,1,0) if agency_charter_indicator>=2 & agency_charter_indicator<=3
    ren *vocational* *cte*
    g exp_admin_per_stu = (exp_sch_admin+exp_general_admin)/enrollment
    g exp_enterprise_per_stu = exp_enterprise /enrollment
    g exp_operation_plant_per_stu = exp_operation_plant/enrollment
    
    do 4_f_prep_rd
    
    capture rdrobust `outcome' vote_margin if $year_cond2, p(1) kernel(triangular) bwselect(mserd) covs(yr_*) vce(nncluster id_election)
    
    if _rc == 0 {
        local coef = string(e(tau_bc), "%4.3f")
        local ci_l = string(e(ci_l_rb), "%4.3f")
        local ci_h = string(e(ci_r_rb), "%4.3f")
        local N_eff = string(e(N_h_l) + e(N_h_r), "%9.0fc")
		*local ymean = string(e(beta_Y_p_l), "%4.3fc") 
		* Calculate control group mean manually
		quietly sum `outcome' if abs(vote_margin) <= e(h_l) & vote_margin < 0 & $year_cond2
		if r(N) > 0 {
			local ymean = string(r(mean), "%4.3f")
		}
		else {
			local ymean = ""
		}
				
    }
    else {
        local coef = ""
        local ci_l = ""
        local ci_h = ""
        local N_eff = ""
        local ymean = ""
    }
    
    c_local coef `coef'
    c_local ci_l `ci_l'
    c_local ci_h `ci_h'
    c_local N_eff `N_eff'
    c_local ymean `ymean'
end

********************************************************************************
* MOTIONS Section
********************************************************************************
texdoc write  \multicolumn{7}{l}{\textit{\textbf{Panel A: Proposed Motions}}} \\

* Proposed motions
local equity_focal_outcomes "n_mtn_equity n_mtn_total"
local equity_all_outcomes "n_mtn_equity n_mtn_total"
local fiscal_focal_outcomes "n_mtn_budget n_mtn_total"
local fiscal_all_outcomes "n_mtn_budget n_mtn_total"
local hisp_equity_outcomes "n_mtn_equity n_mtn_total"
local hisp_fiscal_outcomes "n_mtn_budget n_mtn_total"

//texdoc write Proposed & & & & & & \\

* Get results for proposed motions (using first outcome from each list)
run_rd_analysis equity_prior n_mtn_equity
local eq_foc_coef `coef'
local eq_foc_ci "[`ci_l', `ci_h']"
local eq_foc_N `N_eff'
local eq_foc_ymean `ymean'

run_rd_analysis equity_prior n_mtn_total
local eq_all_coef `coef'
local eq_all_ci "[`ci_l', `ci_h']"
local eq_all_N `N_eff'
local eq_all_ymean `ymean'

run_rd_analysis budget_hawk n_mtn_budget
local fc_foc_coef `coef'
local fc_foc_ci "[`ci_l', `ci_h']"
local fc_foc_N `N_eff'
local fc_foc_ymean `ymean'

run_rd_analysis budget_hawk n_mtn_total
local fc_all_coef `coef'
local fc_all_ci "[`ci_l', `ci_h']"
local fc_all_N `N_eff'
local fc_all_ymean `ymean'

run_rd_analysis hisp n_mtn_equity
local hi_eq_coef `coef'
local hi_eq_ci "[`ci_l', `ci_h']"
local hi_eq_N `N_eff'
local hi_eq_ymean `ymean'

run_rd_analysis hisp n_mtn_budget
local hi_fc_coef `coef'
local hi_fc_ci "[`ci_l', `ci_h']"
local hi_fc_N `N_eff'
local hi_fc_ymean `ymean'

* Write coefficient row
texdoc write Proposed & Estimate & `eq_foc_coef' & `eq_all_coef' & `fc_foc_coef' & `fc_all_coef' & `hi_eq_coef' & `hi_fc_coef' \\
* Write CI row
texdoc write &   & `eq_foc_ci' & `eq_all_ci' & `fc_foc_ci' & `fc_all_ci' & `hi_eq_ci' & `hi_fc_ci' \\
* Write Y mean row
texdoc write & Mean Dep. Var.& `eq_foc_ymean' & `eq_all_ymean' & `fc_foc_ymean' & `fc_all_ymean' & `hi_eq_ymean' & `hi_fc_ymean' \\
* Write N row
texdoc write & N & `eq_foc_N' & `eq_all_N' & `fc_foc_N' & `fc_all_N' & `hi_eq_N' & `hi_fc_N' \\

* Passed motions
texdoc write  \multicolumn{7}{l}{\textit{\textbf{Panel B: Passed Motions}}} \\

run_rd_analysis equity_prior n_pass_equity
local eq_foc_coef `coef'
local eq_foc_ci "[`ci_l', `ci_h']"
local eq_foc_N `N_eff'
local eq_foc_ymean `ymean'

run_rd_analysis equity_prior n_pass_total
local eq_all_coef `coef'
local eq_all_ci "[`ci_l', `ci_h']"
local eq_all_N `N_eff'
local eq_all_ymean `ymean'

run_rd_analysis budget_hawk n_pass_budget
local fc_foc_coef `coef'
local fc_foc_ci "[`ci_l', `ci_h']"
local fc_foc_N `N_eff'
local fc_foc_ymean `ymean'

run_rd_analysis budget_hawk n_pass_total
local fc_all_coef `coef'
local fc_all_ci "[`ci_l', `ci_h']"
local fc_all_N `N_eff'
local fc_all_ymean `ymean'

run_rd_analysis hisp n_pass_equity
local hi_eq_coef `coef'
local hi_eq_ci "[`ci_l', `ci_h']"
local hi_eq_N `N_eff'
local hi_eq_ymean `ymean'

run_rd_analysis hisp n_pass_budget
local hi_fc_coef `coef'
local hi_fc_ci "[`ci_l', `ci_h']"
local hi_fc_N `N_eff'
local hi_fc_ymean `ymean'

texdoc write \vspace{.2} \\
texdoc write Passed & Estimate & `eq_foc_coef' & `eq_all_coef' & `fc_foc_coef' & `fc_all_coef' & `hi_eq_coef' & `hi_fc_coef' \\
texdoc write &   & `eq_foc_ci' & `eq_all_ci' & `fc_foc_ci' & `fc_all_ci' & `hi_eq_ci' & `hi_fc_ci' \\
texdoc write & Mean Dep. Var.& `eq_foc_ymean' & `eq_all_ymean' & `fc_foc_ymean' & `fc_all_ymean' & `hi_eq_ymean' & `hi_fc_ymean' \\
texdoc write & N & `eq_foc_N' & `eq_all_N' & `fc_foc_N' & `fc_all_N' & `hi_eq_N' & `hi_fc_N' \\

* Non-unanimous motions (moved from marginal votes section)
//texdoc write Non-Unanimous & & & & & & \\
texdoc write  \multicolumn{7}{l}{\textit{\textbf{Panel C: Non-Unanimous Motions}}} \\

run_rd_analysis equity_prior n_marg_equity
local eq_foc_coef `coef'
local eq_foc_ci "[`ci_l', `ci_h']"
local eq_foc_N `N_eff'
local eq_foc_ymean `ymean'

run_rd_analysis equity_prior n_marg_total
local eq_all_coef `coef'
local eq_all_ci "[`ci_l', `ci_h']"
local eq_all_N `N_eff'
local eq_all_ymean `ymean'

run_rd_analysis budget_hawk n_marg_budget
local fc_foc_coef `coef'
local fc_foc_ci "[`ci_l', `ci_h']"
local fc_foc_N `N_eff'
local fc_foc_ymean `ymean'

run_rd_analysis budget_hawk n_marg_total
local fc_all_coef `coef'
local fc_all_ci "[`ci_l', `ci_h']"
local fc_all_N `N_eff'
local fc_all_ymean `ymean'

run_rd_analysis hisp n_marg_equity
local hi_eq_coef `coef'
local hi_eq_ci "[`ci_l', `ci_h']"
local hi_eq_N `N_eff'
local hi_eq_ymean `ymean'

run_rd_analysis hisp n_marg_budget
local hi_fc_coef `coef'
local hi_fc_ci "[`ci_l', `ci_h']"
local hi_fc_N `N_eff'
local hi_fc_ymean `ymean'

texdoc write \vspace{.2} \\
texdoc write Non-Unanimous & Estimate & `eq_foc_coef' & `eq_all_coef' & `fc_foc_coef' & `fc_all_coef' & `hi_eq_coef' & `hi_fc_coef' \\
texdoc write &   & `eq_foc_ci' & `eq_all_ci' & `fc_foc_ci' & `fc_all_ci' & `hi_eq_ci' & `hi_fc_ci' \\
texdoc write & Mean Dep. Var.& `eq_foc_ymean' & `eq_all_ymean' & `fc_foc_ymean' & `fc_all_ymean' & `hi_eq_ymean' & `hi_fc_ymean' \\
texdoc write & N & `eq_foc_N' & `eq_all_N' & `fc_foc_N' & `fc_all_N' & `hi_eq_N' & `hi_fc_N' \\


********************************************************************************
* MARGINAL VOTES Section
********************************************************************************
texdoc write \vspace{.2} \\
texdoc write  \multicolumn{7}{l}{\textit{\textbf{Panel D: Marginal Votes in One-Margin Votes}}} \\


* One-Vote Margin
//texdoc write One-Vote Margin & & & & & & \\

run_rd_analysis equity_prior equity_yes_votes_mg1
local eq_foc_coef `coef'
local eq_foc_ci "[`ci_l', `ci_h']"
local eq_foc_N `N_eff'
local eq_foc_ymean `ymean'

run_rd_analysis equity_prior total_yes_votes_mg1
local eq_all_coef `coef'
local eq_all_ci "[`ci_l', `ci_h']"
local eq_all_N `N_eff'
local eq_all_ymean `ymean'

run_rd_analysis budget_hawk budget_yes_votes_mg1
local fc_foc_coef `coef'
local fc_foc_ci "[`ci_l', `ci_h']"
local fc_foc_N `N_eff'
local fc_foc_ymean `ymean'

run_rd_analysis budget_hawk total_yes_votes_mg1
local fc_all_coef `coef'
local fc_all_ci "[`ci_l', `ci_h']"
local fc_all_N `N_eff'
local fc_all_ymean `ymean'

run_rd_analysis hisp equity_yes_votes_mg1
local hi_eq_coef `coef'
local hi_eq_ci "[`ci_l', `ci_h']"
local hi_eq_N `N_eff'
local hi_eq_ymean `ymean'

run_rd_analysis hisp budget_yes_votes_mg1
local hi_fc_coef `coef'
local hi_fc_ci "[`ci_l', `ci_h']"
local hi_fc_N `N_eff'
local hi_fc_ymean `ymean'

texdoc write One-Vote Margin & Estimate & `eq_foc_coef' & `eq_all_coef' & `fc_foc_coef' & `fc_all_coef' & `hi_eq_coef' & `hi_fc_coef' \\
texdoc write &   & `eq_foc_ci' & `eq_all_ci' & `fc_foc_ci' & `fc_all_ci' & `hi_eq_ci' & `hi_fc_ci' \\
texdoc write & Mean Dep. Var.& `eq_foc_ymean' & `eq_all_ymean' & `fc_foc_ymean' & `fc_all_ymean' & `hi_eq_ymean' & `hi_fc_ymean' \\
texdoc write & N & `eq_foc_N' & `eq_all_N' & `fc_foc_N' & `fc_all_N' & `hi_eq_N' & `hi_fc_N' \\

* Non-Unanimous
//texdoc write Non-Unanimous & & & & & & \\
texdoc write \multicolumn{7}{l}{\textit{\textbf{Panel E: Marginal Votes in Non-Unanimous Votes}}} \\

run_rd_analysis equity_prior equity_yes_votes_mg2
local eq_foc_coef `coef'
local eq_foc_ci "[`ci_l', `ci_h']"
local eq_foc_N `N_eff'
local eq_foc_ymean `ymean'

run_rd_analysis equity_prior total_yes_votes_mg2
local eq_all_coef `coef'
local eq_all_ci "[`ci_l', `ci_h']"
local eq_all_N `N_eff'
local eq_all_ymean `ymean'

run_rd_analysis budget_hawk budget_yes_votes_mg2
local fc_foc_coef `coef'
local fc_foc_ci "[`ci_l', `ci_h']"
local fc_foc_N `N_eff'
local fc_foc_ymean `ymean'

run_rd_analysis budget_hawk total_yes_votes_mg2
local fc_all_coef `coef'
local fc_all_ci "[`ci_l', `ci_h']"
local fc_all_N `N_eff'
local fc_all_ymean `ymean'

run_rd_analysis hisp equity_yes_votes_mg2
local hi_eq_coef `coef'
local hi_eq_ci "[`ci_l', `ci_h']"
local hi_eq_N `N_eff'
local hi_eq_ymean `ymean'

run_rd_analysis hisp budget_yes_votes_mg2
local hi_fc_coef `coef'
local hi_fc_ci "[`ci_l', `ci_h']"
local hi_fc_N `N_eff'
local hi_fc_ymean `ymean'

texdoc write \vspace{.2} \\
texdoc write Non-Unanimous & Estimate & `eq_foc_coef' & `eq_all_coef' & `fc_foc_coef' & `fc_all_coef' & `hi_eq_coef' & `hi_fc_coef' \\
texdoc write &   & `eq_foc_ci' & `eq_all_ci' & `fc_foc_ci' & `fc_all_ci' & `hi_eq_ci' & `hi_fc_ci' \\
texdoc write & Mean Dep. Var.& `eq_foc_ymean' & `eq_all_ymean' & `fc_foc_ymean' & `fc_all_ymean' & `hi_eq_ymean' & `hi_fc_ymean' \\
texdoc write & N & `eq_foc_N' & `eq_all_N' & `fc_foc_N' & `fc_all_N' & `hi_eq_N' & `hi_fc_N' \\

* Close table
texdoc write \bottomrule \hline
texdoc write \end{tabular}
// texdoc write }
// texdoc write \end{table}

* Close texdoc
texdoc close

display "Voting analysis table written to Results/Tex/voting_analysis_table.tex"



********************************************************************************
* VOTES Section
********************************************************************************

capture file close _all
texdoc init "Results/Tex/voting_own_table.tex", replace force

* Write table header
// texdoc write \begin{table}[htbp]
// texdoc write \centering
// texdoc write \caption{Voting Analysis by Category}
// texdoc write \resizebox{\textwidth}{!}{%
texdoc write \label{tab:voting_analysis}
texdoc write \begin{tabular}{lccccccc}
texdoc write \toprule \midrule
texdoc write & & \multicolumn{2}{c}{Equity} & \multicolumn{2}{c}{Fiscal} & \multicolumn{2}{c}{Hispanic} \\
texdoc write \cmidrule(lr){2-3} \cmidrule(lr){4-5} \cmidrule(lr){6-7}
texdoc write &  Focal & All & Focal & All & Equity & Fiscal \\
texdoc write \midrule
* Own Yes votes
//texdoc write Own Yes & & & & & & \\

run_rd_analysis equity_prior equity_yes_votes
local eq_foc_coef `coef'
local eq_foc_ci "[`ci_l', `ci_h']"
local eq_foc_N `N_eff'
local eq_foc_ymean `ymean'

run_rd_analysis equity_prior total_yes_votes
local eq_all_coef `coef'
local eq_all_ci "[`ci_l', `ci_h']"
local eq_all_N `N_eff'
local eq_all_ymean `ymean'

run_rd_analysis budget_hawk budget_yes_votes
local fc_foc_coef `coef'
local fc_foc_ci "[`ci_l', `ci_h']"
local fc_foc_N `N_eff'
local fc_foc_ymean `ymean'

run_rd_analysis budget_hawk total_yes_votes
local fc_all_coef `coef'
local fc_all_ci "[`ci_l', `ci_h']"
local fc_all_N `N_eff'
local fc_all_ymean `ymean'

run_rd_analysis hisp equity_yes_votes
local hi_eq_coef `coef'
local hi_eq_ci "[`ci_l', `ci_h']"
local hi_eq_N `N_eff'
local hi_eq_ymean `ymean'

run_rd_analysis hisp budget_yes_votes
local hi_fc_coef `coef'
local hi_fc_ci "[`ci_l', `ci_h']"
local hi_fc_N `N_eff'
local hi_fc_ymean `ymean'

texdoc write \multicolumn{7}{l}{\textbf{\emph{Panel A: Yes votes, own}}} \\
texdoc write Estimate & `eq_foc_coef' & `eq_all_coef' & `fc_foc_coef' & `fc_all_coef' & `hi_eq_coef' & `hi_fc_coef' \\
texdoc write &   & `eq_foc_ci' & `eq_all_ci' & `fc_foc_ci' & `fc_all_ci' & `hi_eq_ci' & `hi_fc_ci' \\
texdoc write & Mean Dep. Var.& `eq_foc_ymean' & `eq_all_ymean' & `fc_foc_ymean' & `fc_all_ymean' & `hi_eq_ymean' & `hi_fc_ymean' \\
texdoc write & N & `eq_foc_N' & `eq_all_N' & `fc_foc_N' & `fc_all_N' & `hi_eq_N' & `hi_fc_N' \\

* Own No votes
//texdoc write Own No & & & & & & \\

run_rd_analysis equity_prior equity_no_votes
local eq_foc_coef `coef'
local eq_foc_ci "[`ci_l', `ci_h']"
local eq_foc_N `N_eff'
local eq_foc_ymean `ymean'

run_rd_analysis equity_prior total_no_votes
local eq_all_coef `coef'
local eq_all_ci "[`ci_l', `ci_h']"
local eq_all_N `N_eff'
local eq_all_ymean `ymean'

run_rd_analysis budget_hawk budget_no_votes
local fc_foc_coef `coef'
local fc_foc_ci "[`ci_l', `ci_h']"
local fc_foc_N `N_eff'
local fc_foc_ymean `ymean'

run_rd_analysis budget_hawk total_no_votes
local fc_all_coef `coef'
local fc_all_ci "[`ci_l', `ci_h']"
local fc_all_N `N_eff'
local fc_all_ymean `ymean'

run_rd_analysis hisp equity_no_votes
local hi_eq_coef `coef'
local hi_eq_ci "[`ci_l', `ci_h']"
local hi_eq_N `N_eff'
local hi_eq_ymean `ymean'

run_rd_analysis hisp budget_no_votes
local hi_fc_coef `coef'
local hi_fc_ci "[`ci_l', `ci_h']"
local hi_fc_N `N_eff'
local hi_fc_ymean `ymean'

texdoc write \vspace{.2} \\
texdoc write \multicolumn{7}{l}{\textbf{\emph{Panel B: No votes, own}}} \\
texdoc write Estimate & `eq_foc_coef' & `eq_all_coef' & `fc_foc_coef' & `fc_all_coef' & `hi_eq_coef' & `hi_fc_coef' \\
texdoc write &   & `eq_foc_ci' & `eq_all_ci' & `fc_foc_ci' & `fc_all_ci' & `hi_eq_ci' & `hi_fc_ci' \\
texdoc write & Mean Dep. Var.& `eq_foc_ymean' & `eq_all_ymean' & `fc_foc_ymean' & `fc_all_ymean' & `hi_eq_ymean' & `hi_fc_ymean' \\
texdoc write & N & `eq_foc_N' & `eq_all_N' & `fc_foc_N' & `fc_all_N' & `hi_eq_N' & `hi_fc_N' \\

* Others Yes votes
//texdoc write Others Yes & & & & & & \\

run_rd_analysis equity_prior equity_yes_votes_oth
local eq_foc_coef `coef'
local eq_foc_ci "[`ci_l', `ci_h']"
local eq_foc_N `N_eff'
local eq_foc_ymean `ymean'

run_rd_analysis equity_prior total_yes_votes_oth
local eq_all_coef `coef'
local eq_all_ci "[`ci_l', `ci_h']"
local eq_all_N `N_eff'
local eq_all_ymean `ymean'

run_rd_analysis budget_hawk budget_yes_votes_oth
local fc_foc_coef `coef'
local fc_foc_ci "[`ci_l', `ci_h']"
local fc_foc_N `N_eff'
local fc_foc_ymean `ymean'

run_rd_analysis budget_hawk total_yes_votes_oth
local fc_all_coef `coef'
local fc_all_ci "[`ci_l', `ci_h']"
local fc_all_N `N_eff'
local fc_all_ymean `ymean'

run_rd_analysis hisp equity_yes_votes_oth
local hi_eq_coef `coef'
local hi_eq_ci "[`ci_l', `ci_h']"
local hi_eq_N `N_eff'
local hi_eq_ymean `ymean'

run_rd_analysis hisp budget_yes_votes_oth
local hi_fc_coef `coef'
local hi_fc_ci "[`ci_l', `ci_h']"
local hi_fc_N `N_eff'
local hi_fc_ymean `ymean'

texdoc write \vspace{.2} \\
texdoc write \multicolumn{7}{l}{\textbf{\emph{Panel C: Yes votes, others}}} \\
texdoc write Estimate & `eq_foc_coef' & `eq_all_coef' & `fc_foc_coef' & `fc_all_coef' & `hi_eq_coef' & `hi_fc_coef' \\
texdoc write &   & `eq_foc_ci' & `eq_all_ci' & `fc_foc_ci' & `fc_all_ci' & `hi_eq_ci' & `hi_fc_ci' \\
texdoc write & Mean Dep. Var.& `eq_foc_ymean' & `eq_all_ymean' & `fc_foc_ymean' & `fc_all_ymean' & `hi_eq_ymean' & `hi_fc_ymean' \\
texdoc write & N & `eq_foc_N' & `eq_all_N' & `fc_foc_N' & `fc_all_N' & `hi_eq_N' & `hi_fc_N' \\

* Others No votes
//texdoc write Others No & & & & & & \\

run_rd_analysis equity_prior equity_no_votes_oth
local eq_foc_coef `coef'
local eq_foc_ci "[`ci_l', `ci_h']"
local eq_foc_N `N_eff'
local eq_foc_ymean `ymean'

run_rd_analysis equity_prior total_no_votes_oth
local eq_all_coef `coef'
local eq_all_ci "[`ci_l', `ci_h']"
local eq_all_N `N_eff'
local eq_all_ymean `ymean'

run_rd_analysis budget_hawk budget_no_votes_oth
local fc_foc_coef `coef'
local fc_foc_ci "[`ci_l', `ci_h']"
local fc_foc_N `N_eff'
local fc_foc_ymean `ymean'

run_rd_analysis budget_hawk total_no_votes_oth
local fc_all_coef `coef'
local fc_all_ci "[`ci_l', `ci_h']"
local fc_all_N `N_eff'
local fc_all_ymean `ymean'

run_rd_analysis hisp equity_no_votes_oth
local hi_eq_coef `coef'
local hi_eq_ci "[`ci_l', `ci_h']"
local hi_eq_N `N_eff'
local hi_eq_ymean `ymean'

run_rd_analysis hisp budget_no_votes_oth
local hi_fc_coef `coef'
local hi_fc_ci "[`ci_l', `ci_h']"
local hi_fc_N `N_eff'
local hi_fc_ymean `ymean'

texdoc write \vspace{.2} \\
texdoc write \multicolumn{7}{l}{\textbf{\emph{Panel D: No votes, others}}} \\
texdoc write Estimate & `eq_foc_coef' & `eq_all_coef' & `fc_foc_coef' & `fc_all_coef' & `hi_eq_coef' & `hi_fc_coef' \\
texdoc write &   & `eq_foc_ci' & `eq_all_ci' & `fc_foc_ci' & `fc_all_ci' & `hi_eq_ci' & `hi_fc_ci' \\
texdoc write & Mean Dep. Var.& `eq_foc_ymean' & `eq_all_ymean' & `fc_foc_ymean' & `fc_all_ymean' & `hi_eq_ymean' & `hi_fc_ymean' \\
texdoc write & N & `eq_foc_N' & `eq_all_N' & `fc_foc_N' & `fc_all_N' & `hi_eq_N' & `hi_fc_N' \\



* Close table
texdoc write \bottomrule
texdoc write \end{tabular}
// texdoc write }
// texdoc write \end{table}

* Close texdoc
texdoc close

* Drop the program
program drop run_rd_analysis

display "Voting analysis table written to Results/Tex/voting_own_table.tex"



