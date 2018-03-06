if(!require("glmnet")) install.packages("glmnet"); library("glmnet")
library(ggfortify)
library(caret)
#the bigger t, our cutoff value, the smaller λ, the shrinkage, gets



india = readRDS(".\\india.rds")

#1. initial variable selection and normalization
colselection = c("pr_q1","pr_q2","pr_q3","pr_q4","tas_q1","tas_q2","tas_q3","tas_q4",
                 "prod_amount_y","daily_caloric_supply","exp_sug","exp_veg","exp_cer","imp_sug","imp_veg","imp_cer", 
                 "agri_gdp","gni_pc","cp_inflation","avg_p_barrel","population") 
target = c("avg_price_prod_year")


feats = india[, (colnames(india) %in% colselection)]
d_target = india[, (colnames(india) %in% target)]

model = glmnet(as.matrix(feats),d_target,standardize = TRUE, alpha =1)
model
plot.glmnet(model)
fit = cv.glmnet(as.matrix(feats),d_target,standardize = TRUE, type.measure ="mse",alpha=1, nfolds=3)
autoplot(fit)


which(fit$lambda == fit$lambda.1se)
coef(fit, s = "lambda.min")
fit$glmnet.fit$beta[, which(fit$lambda == fit$lambda.min)]
