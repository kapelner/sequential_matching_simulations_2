#get beta_T est from a regression
linear_mod = lm(y ~ ., data = Xy)

coefs = coef(summary(linear_mod))
beta_Ts[nsim] = coefs[p + 2, 1]
pval = coefs[p + 2, 4]
Rsqs[nsim] = summary(linear_mod)$r.squared