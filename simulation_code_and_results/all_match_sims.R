
###set simulation parameters here

#how precise do we want our simulated results?
Nsim_per_block = 2000
Nsim_exact_test = 1000

#the treatment effects: 0 tests the size of test and something \neq 0 tests power
treatment_effects =c(1, 0)

#do we use the Z test and/or the T test (only when applicable)
Z_TESTS = c(TRUE, FALSE)

#balanced assignment of treatment and control
prob_trt = 0.5



#how many subjects enter the sequential experiment?
ns_to_test = c( 
	50,
	100,
	200
)

#cutoff parameter for matching
prob_match_cutoff_lambdas = c(
	0.01,
	0.025,
	0.05,
	0.075,
	0.10,
	0.20,
	0.35,
	0.5
)

#How do we randomize subjects into treatment or control? 
#Then, how do we analyze the resulting data to obtain an effect size and significance level?
#Everything uncommented becomes part of the simulation
randomization_types = c(
	"crd_ttest",					#completely randomized design (CRD)							#analyzed via t-test
	"efron_ttest",					#Efron's biased coin alpha=2/3 design						#analyzed via t-test
	"strat_ttest",					#stratification by tertiles => 9 blocks design  			#analyzed via t-test
	"ps_min_ttest",					#Pocock & Simon's minimization design						#analyzed via t-test
	"seq_match_kk",					#Kapelner & Krieger Seq Matching  							#analyzed via classic test of Section 2.3.1, Equation 4
	"post_match_crd_kk",			#Post Matching Design with CRD assignment					#analyzed via classic test of Section 2.3.1, Equation 4
	"post_match_strat_kk",			#Post Matching Design with stratified assignment			#analyzed via classic test of Section 2.3.1, Equation 4
	"crd_lin",						#completely randomized design (CRD)							#analyzed via OLS
	"efron_lin",					#Efron's biased coin alpha=2/3 design						#analyzed via OLS
	"strat_lin",					#stratification by tertiles => 9 blocks design  			#analyzed via OLS
	"ps_min_lin",					#Pocock & Simon's minimization design						#analyzed via OLS
	"seq_match_kk_lin",				#Kapelner & Krieger Seq Matching design  					#analyzed via modified OLS of Section 2.3.2, Equation 6
	"post_match_crd_kk_lin",		#Post Matching Design with CRD assignment					#analyzed via modified OLS of Section 2.3.2, Equation 6
	"post_match_strat_kk_lin",		#Post Matching Design with stratified assignment			#analyzed via modified OLS of Section 2.3.2, Equation 6
	"crd_exact",					#completely randomized design (CRD)							#analyzed via classic permutation test
	"efron_exact",					#Efron's biased coin alpha=2/3 design						#analyzed via classic permutation test	
	"strat_exact",					#stratification by tertiles => 9 blocks design  			#analyzed via classic permutation test	
	"ps_min_exact",					#Pocock & Simon's minimization design						#analyzed via classic permutation test
	"seq_match_kk_exact",			#Kapelner & Krieger Seq Matching design  					#analyzed via permutation test of Section 2.3.3
	"post_match_crd_kk_exact",		#Post Matching Design with CRD assignment					#analyzed via permutation test of Section 2.3.3
	"post_match_strat_kk_exact"		#Post Matching Design with stratified assignment			#analyzed via permutation test of Section 2.3.3
)


#what do we measure for each set of simulations?
metrics_for_each_run = c(
	"avg_max_std_diff_bal", 
	"avg_max_ks_stat",
	"avg_beta_T", 
	"avg_abs_bias",
	"std_err_beta_T",
	"power",
	"pct_trt_diff",
	"res_end_prop_avg",
	"ssqr_over_sum",
	"ssqr_eq_ssqd_pval",
	"match_corr",
	"corr_eq_zero_pval",
	"pct_only_matches",
	"pct_only_reservoir",
	"true_var_prop_diff"
)


#how should the X's and the errors be generated?
data_and_err_gen = "just_multnorm"

#what kind of models are we running?
sim_type = "series_of_quadratics_and_interactions"

source(paste("sim_type_", sim_type, ".R", sep = ""))
