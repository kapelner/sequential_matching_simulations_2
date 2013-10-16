
#sometimes the reservoir just isn't large enough
if (nRT <= 1 || nRC <= 1){
	beta_Ts[nsim] = d_bar
	b_T_est_sd = sqrt(ssqD_bar)
	pct_only_matchess[nsim] = 1
	pct_only_reservoirs[nsim] = 0
	
} else if (m == 0){ #sometimes there's no matches
	
	beta_Ts[nsim] = r_bar
	b_T_est_sd = sqrt(ssqR)
	pct_only_matchess[nsim] = 0
	pct_only_reservoirs[nsim] = 1
	
} else {
	ssqR = (var(YleftT) * (nRT - 1) + var(YleftC) * (nRC - 1)) / (nR - 2) * (1 / nRT + 1 / nRC) #pooled variance in reservoir	
	w_star = ssqR / (ssqR + ssqD_bar) #just a convenience for faster runtime	
	
	
	beta_Ts[nsim] = w_star * d_bar + (1 - w_star) * r_bar #proper weighting	
	b_T_est_sd = sqrt(ssqR * ssqD_bar / (ssqR + ssqD_bar)) #see eq's
	
	ssq_from_ssqr = ssqR / (1 / nRT + 1 / nRC)
	ssq_from_ssq_Dbar = ssqD_bar * m
	ssqr_over_sums[nsim] = ssq_from_ssqr / (ssq_from_ssqr + ssq_from_ssq_Dbar)
	ssqr_eq_ssqdbar_pvals[nsim] = pf(ssq_from_ssqr * 2 / ssq_from_ssq_Dbar, nRT + nRC - 2, m - 1, lower.tail = F)
	
	yMT = Xymatched[Xymatched$indic_T == 1, ]$y
	yCT = Xymatched[Xymatched$indic_T == 0, ]$y
	matched_correlations[nsim] = cor(yMT, yCT)
#	corr_eq_zero_pvals[nsim] = cor.test(yMT, yCT, alternative = "greater", method = "pearson")$p.value
	pct_only_matchess[nsim] = 0
	pct_only_reservoirs[nsim] = 0
	
	#now what's the difference in % of the variance and the true variance?
	sample_variance_eq_4 = b_T_est_sd^2
	true_variance_Dbar = 1 / m^2 * sum(zdiffs^2) + 2 / m * sigsq_e
	true_variance_Rbar = 4 / nR^2 * sum(Xymatched$z^2) + 4 / nR * sigsq_e
	true_variance_eq_4 = true_variance_Rbar * true_variance_Dbar / (true_variance_Rbar + true_variance_Dbar)
	
	true_var_prop_diffs[nsim] = (sample_variance_eq_4 - true_variance_eq_4) / true_variance_eq_4
}

T_stats[nsim] = beta_Ts[nsim] / b_T_est_sd #we calculate the T-stat against the null of zero effect	

if (Z_TEST){
	pval = 2 * (1 - pnorm(abs(T_stats[nsim]))) #approximate by using real Z
} else {
	pval = 2 * (1 - pt(abs(T_stats[nsim]), max(m - 1, min(nRT, nRC)))) #approximate by using a T
}


