indic_T = rbinom(n, 1, prob_trt)

source(paste("create_response_", response_model, ".R", sep = ""))

#create design matrix
Xy = as.data.frame(cbind(x_s, indic_T, y))
#pull out yT, yC
yTs = Xy[Xy$indic_T == 1, "y"]
yCs = Xy[Xy$indic_T == 0, "y"]