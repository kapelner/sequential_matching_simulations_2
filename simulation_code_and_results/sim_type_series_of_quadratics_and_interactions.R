response_model = "linear_quad_and_interact_medley"


#take care of the parameters of the grid iteration
sim_param_mat = matrix(NA, nrow = 0, ncol = 5)
sim_param_mat = rbind(sim_param_mat, c(1,1,1,1,1))		#NON-LINEAR model
sim_param_mat = rbind(sim_param_mat, c(2,2,0,0,0))		#LINEAR model
sim_param_mat = rbind(sim_param_mat, c(0,0,0,0,0))		#ZERO-EFFECTS model

#now we augment beta_mat by n's
sim_param_mat_with_n = matrix(NA, nrow = 0, ncol = 6)
for (n in ns_to_test){
	sim_param_mat_with_n = rbind(sim_param_mat_with_n, cbind(sim_param_mat, n))
}
#now we augment beta_mat by lambda's
sim_param_mat_with_n_and_lambda = matrix(NA, nrow = 0, ncol = 7)
for (prob_match_cutoff_lambda in prob_match_cutoff_lambdas){
	sim_param_mat_with_n_and_lambda = rbind(sim_param_mat_with_n_and_lambda, cbind(sim_param_mat_with_n, prob_match_cutoff_lambda))
}
#now we augment beta_mat by beta_T's
sim_param_mat_with_n_and_lambda_and_beta_T = matrix(NA, nrow = 0, ncol = 8)
for (beta_T in treatment_effects){
	sim_param_mat_with_n_and_lambda_and_beta_T = rbind(sim_param_mat_with_n_and_lambda_and_beta_T, cbind(sim_param_mat_with_n_and_lambda, beta_T))
}
#now we augment beta_mat by Z_TEST
sim_param_mat_with_n_and_lambda_and_beta_T_and_Ztest = matrix(NA, nrow = 0, ncol = 9)
for (z_test in Z_TESTS){
	sim_param_mat_with_n_and_lambda_and_beta_T_and_Ztest = rbind(sim_param_mat_with_n_and_lambda_and_beta_T_and_Ztest, cbind(sim_param_mat_with_n_and_lambda_and_beta_T, z_test))
}
#now augment beta_mat by randomization test
MASTER = matrix(NA, nrow = 0, ncol = 10)
for (randomization_type in randomization_types){
	MASTER = rbind(MASTER, cbind(sim_param_mat_with_n_and_lambda_and_beta_T_and_Ztest, randomization_type))
}
head(MASTER)
dim(MASTER)




###kill all those that do not need to run
MASTERnon_match_run = array(NA, nrow(MASTER))
for (imaster in 1 : nrow(MASTER)){
	MASTERnon_match_run[imaster] = length(grep("match", MASTER[imaster, 10])) == 0
}

MASTERSEQT = MASTER[, 10] == "seq_match_kk_lin" | MASTER[, 10] == "seq_match_kk"
MASTERlamdasfirst =  as.numeric(MASTER[, 7]) == prob_match_cutoff_lambdas[1]
MASTERZs = ifelse(as.numeric(MASTER[, 9]) == 1, TRUE, FALSE)

MASTER = MASTER[(MASTERnon_match_run == FALSE | (MASTERnon_match_run & MASTERlamdasfirst & MASTERZs)) & (MASTERZs | MASTERSEQT), ]

#now pull the parameters for this simulation set from the matrix of parameter values
beta_1 = as.numeric(MASTER[iter_num, 1])
beta_2 = as.numeric(MASTER[iter_num, 2])
beta_3 = as.numeric(MASTER[iter_num, 3])
beta_4 = as.numeric(MASTER[iter_num, 4])
beta_5 = as.numeric(MASTER[iter_num, 5])
n = as.numeric(MASTER[iter_num, 6])
prob_match_cutoff_lambda = as.numeric(MASTER[iter_num, 7])
beta_T = as.numeric(MASTER[iter_num, 8])
Z_TEST = ifelse(as.numeric(MASTER[iter_num, 9] == 1), TRUE, FALSE)
randomization_type = MASTER[iter_num, 10]


cat(randomization_type, "n =", n, "alpha =", prob_match_cutoff_lambda, " betas ", beta_1, " ", beta_2, " ", beta_3, " ", beta_4, " ", beta_5, "betaT", beta_T, "Z_TEST", Z_TEST, "\n")


#each prob_match and response gets their own results matrix and it gets saved to a different CSV file
results = matrix(NA, nrow = length(metrics_for_each_run), ncol = 1)
rownames(results) = array(NA, length(metrics_for_each_run))
#add row names to the results matrix
for (m in 1 : length(metrics_for_each_run)){
	rownames(results) = metrics_for_each_run	
}

#run the simulation series!
source("inner_simulation.R")

#save the results
write.csv(results, paste("results_quads_and_ints_betas_", prob_match_cutoff_lambda, "_", n, "_", beta_1, "_", beta_2, "_", beta_3, "_", beta_4, "_", beta_5, "_betaT_", beta_T, "_Z_TEST_", ifelse(Z_TEST, 1, 0), "_rand_type_", randomization_type, ".csv", sep = ""))


						
