
#initialize the indicator treatment vector
indic_T = array(NA, n) 

EFRON_ALPHA = 2 / 3 #his personal favorite

#now we're going to go through and do the matching
for (i_match in 1 : n){
	if (sum(indic_T == 1, na.rm = TRUE) == sum(indic_T == 0, na.rm = TRUE)){
		indic_T[i_match] = rbinom(1, 1, prob_trt)
	} else if (sum(indic_T == 1, na.rm = TRUE) < sum(indic_T == 0, na.rm = TRUE)){
		indic_T[i_match] = rbinom(1, 1, EFRON_ALPHA)
	} else if (sum(indic_T == 1, na.rm = TRUE) > sum(indic_T == 0, na.rm = TRUE)){
		indic_T[i_match] = rbinom(1, 1, 1 - EFRON_ALPHA)
	}
}

#create response vector
source(paste("create_response_", response_model, ".R", sep = ""))

#create design matrix
Xy = as.data.frame(cbind(x_s, indic_T, y))
#pull out yT, yC
yTs = Xy[Xy$indic_T == 1, "y"]
yCs = Xy[Xy$indic_T == 0, "y"]