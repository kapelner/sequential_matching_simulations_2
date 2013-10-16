tau_beta = 1 #std dev of the beta_j's

mu_x = 0
sigma_x = 1
sigsq_e = 3

p = 2 #the number of dimensions is fixed by simulation

#build mvnp covariates
x_s = matrix(rnorm(n * p, mu_x, sigma_x), ncol = p)

#build errors
errors = rnorm(n, 0, sqrt(sigsq_e))