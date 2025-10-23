********************************************************************************
* II. Run Seemingly Unrelated Regression + Extract List of Variables
********************************************************************************

	estimates dir
	local all_models `r(names)'
	suest `all_models', vce(cl id_election)


	* Initialize identity and ideology model lists
	local iden_models ""
	local ideo_models ""
	local treat_iden ""
	local treat_ideo ""
	
	foreach m of local all_models {
		gettoken prefix rest : m, parse("_")
		local y = substr("`m'", 3, .) // get rid of M_
		local y : subinstr local y "_ht1" "", all // get rid of _ht1
		local treat_this = "T_`y'_S"

		if regexm("`m'", "(hisp|female|democrat_v2|occ_teacher)") {
			local iden_models `iden_models' `m'
			local treat_iden "`treat_iden' `m'_mean:`treat_this'"
		}
		else {
			local ideo_models `ideo_models' `m'
			local treat_ideo "`treat_ideo' `m'_mean:`treat_this'"
		}
	}
	display "`iden_models'"
	display "`ideo_models'"
	
	display "`treat_iden'"
	display "`treat_ideo'"
	

********************************************************************************
* III. Calculate Precision-weighted Means
********************************************************************************
		
mata:
    // --- Import data ---
    b = st_matrix("e(b)")
    V = st_matrix("e(V)")
    names = st_matrixcolstripe("e(b)")

    iden_treats = tokens("`treat_iden'")
    ideo_treats = tokens("`treat_ideo'")

    // --- Index matching ---
	iden_indices = J(0,1,.)
	ideo_indices = J(0,1,.)

	for (i=1; i<=cols(b); i++) {
		for (j=1; j<=cols(iden_treats); j++) {
			if (names[i,1] + ":" + names[i,2] == iden_treats[j]) {
				iden_indices = iden_indices \ i
			}
		}
		for (j=1; j<=cols(ideo_treats); j++) {
			if (names[i,1] + ":" + names[i,2] == ideo_treats[j]) {
				ideo_indices = ideo_indices \ i
			}
		}
	}
	
    // --- Subgroup (_ht1) indices ---
    sub_idx = J(0,1,.)
    for (i = 1; i <= cols(b); i++) {
        coefname = names[i,1] + ":" + names[i,2]
        if (strpos(names[i,1], "_ht1") & (anyof(iden_treats, coefname) | anyof(ideo_treats, coefname))) {
            sub_idx = sub_idx \ i
        }
    }

    // --- Exclude subgroup terms from main identity list ---
	keep = J(rows(iden_indices), 1, 1)
	for (i = 1; i <= rows(iden_indices); i++) {
		if (anyof(sub_idx, iden_indices[i])) {
			keep[i] = 0
		}
	}
	iden_indices = select(iden_indices, keep)

    // --- Subset b and V matrices ---
    b_iden = b[1, iden_indices]
    V_iden = V[iden_indices, iden_indices]

    b_ideo = b[1, ideo_indices]
    V_ideo = V[ideo_indices, ideo_indices]

    b_sub = b[1, sub_idx]
    V_sub = V[sub_idx, sub_idx]

    // --- Unweighted subgroup ---
    theta_unw_sub = mean(b_sub')
    theta_se_unw_sub = sqrt(variance(b_sub') / cols(b_sub))
    theta_unw_ideo = mean(b_ideo')
    theta_se_unw_ideo = sqrt(variance(b_ideo') / cols(b_ideo))
    theta_unw_iden = mean(b_iden')
    theta_se_unw_iden = sqrt(variance(b_iden') / cols(b_iden))

    // --- Export to Stata ---
    st_numscalar("theta_unw_sub", theta_unw_sub)
    st_numscalar("theta_se_unw_sub", theta_se_unw_sub)
    st_numscalar("theta_unw_ideo", theta_unw_ideo)
    st_numscalar("theta_se_unw_ideo", theta_se_unw_ideo)
    st_numscalar("theta_unw_iden", theta_unw_iden)
    st_numscalar("theta_se_unw_iden", theta_se_unw_iden)
end
