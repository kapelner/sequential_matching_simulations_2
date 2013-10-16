max_std_diff_balances = array(0, Nsim_per_block)
max_ks_stats = array(0, Nsim_per_block)
beta_Ts = array(NA, Nsim_per_block)
T_stats = array(NA, Nsim_per_block)
final_reservoir_size = array(NA, Nsim_per_block)
Rsqs = array(NA, Nsim_per_block)
Ha_acceptances = array(NA, Nsim_per_block)
pct_Ts = array(NA, Nsim_per_block)
ssqr_over_sums = array(NA, Nsim_per_block)
ssqr_eq_ssqdbar_pvals = array(NA, Nsim_per_block)
matched_correlations = array(NA, Nsim_per_block)
corr_eq_zero_pvals = array(NA, Nsim_per_block)
pct_only_matchess = array(NA, Nsim_per_block)
pct_only_reservoirs = array(NA, Nsim_per_block)
true_var_prop_diffs = array(NA, Nsim_per_block)

for (nsim in 1 : Nsim_per_block){
	if (nsim %% 50 == 0){
		cat(".")
	}
	
	#generate data
	source(paste("data_and_err_gen_", data_and_err_gen, ".R", sep = ""))
	
	#run one run of whatever simulation type
	source(paste("rand_type_", randomization_type, ".R", sep = ""))
	Ha_acceptances[nsim] = ifelse(pval < 0.05, 1, 0)

	#calculate balance metrics
	xTs = Xy[Xy$indic_T == 1, 1 : p]
	xCs = Xy[Xy$indic_T == 0, 1 : p]
	
	#compute balance	
	max_std_diff_balance = -999999
	for (j in 1 : p){
		std_diff_balance = abs(mean(xTs[, j]) - mean(xCs[, j])) / sqrt(var(xTs[, j]) / length(xTs[, j]) + var(xCs[, j]) / length(xCs[, j]))
		if (std_diff_balance > max_std_diff_balance){
			max_std_diff_balances[nsim] = std_diff_balance
		}
	}
	max_ks_stat = -99999
	for (j in 1 : p){
		ks_stat = ks.test(xTs[, j], xCs[, j])$statistic
		if (ks_stat > max_ks_stat){
			max_ks_stats[nsim] = ks_stat
		}
	}
	
	pct_Ts[nsim] = nrow(xTs) / n
}

cat("\n")

#add to results
results["avg_max_std_diff_bal", 1] = mean(max_std_diff_balances, na.rm = TRUE)
results["avg_beta_T", 1] = mean(beta_Ts, na.rm = TRUE)	
results["avg_abs_bias", 1] = mean(abs(beta_Ts - beta_T), na.rm = TRUE)
results["avg_max_ks_stat", 1] = mean(max_ks_stats, na.rm = TRUE)
results["std_err_beta_T", 1] = sd(beta_Ts, na.rm = TRUE)
results["power", 1] = mean(Ha_acceptances, na.rm = TRUE)
results["pct_trt_diff", 1] = mean(abs(pct_Ts - prob_trt), na.rm = TRUE)

#only save reservoir data strategies that involve a matching algorithm
if (length(grep("match", 1)) > 0){
	results["res_end_prop_avg", 1] = mean(final_reservoir_size)
	results["ssqr_over_sum", 1] = mean(ssqr_over_sums, na.rm = TRUE)
	results["ssqr_eq_ssqd_pval", 1] = mean(ssqr_eq_ssqdbar_pvals, na.rm = TRUE)
	results["pct_only_matches", 1] = mean(pct_only_matchess, na.rm = TRUE)
	results["pct_only_reservoir", 1] = mean(pct_only_reservoirs, na.rm = TRUE)
	results["match_corr", 1] = mean(matched_correlations, na.rm = TRUE)
	results["corr_eq_zero_pval", 1] = mean(corr_eq_zero_pvals, na.rm = TRUE)
	results["true_var_prop_diff", 1] = mean(true_var_prop_diffs, na.rm = TRUE)
}
