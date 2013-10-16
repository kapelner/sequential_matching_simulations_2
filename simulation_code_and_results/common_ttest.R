#now get beta_T est via a regular classic pooled 2-sample t-test
ttest = t.test(yTs, yCs)
beta_Ts[nsim] = mean(yTs) - mean(yCs)
pval = ttest$p.value
Rsqs[nsim] = 1 - sum((Xy$y - beta_Ts[nsim] * Xy$indic_T)^2) / sum((Xy$y - mean(Xy$y))^2)