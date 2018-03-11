if(!require("glmnet")) install.packages("glmnet"); library("glmnet")
library(ggfortify)
library(caret)
library("plotmo")

#the bigger t, our cutoff value, the smaller λ, the shrinkage, gets


india = readRDS(".\\india.rds")


#1. initial variable selection 
colselection = c("pr_q1","pr_q2","pr_q3","pr_q4","tas_q1","tas_q2","tas_q3","tas_q4",
                 "prod_amount_y","daily_caloric_supply","exp_sug","exp_veg","exp_cer","imp_sug","imp_veg","imp_cer", 
                 "agri_gdp","gni_pc","cp_inflation","avg_p_barrel","population","cm_name") 
target = c("avg_price_prod_year")
feats = india[, (colnames(india) %in% colselection)]

feats  = as.data.frame(scale(feats))


d_target = india[, (colnames(india) %in% target)]
d_target = as.data.frame(scale(d_target))
#2. Applying the Lasso technique 
model = glmnet(as.matrix(feats),as.matrix(d_target),standardize = FALSE, alpha =1)
model
plot_glmnet(model,label = 5)
fit = cv.glmnet(as.matrix(feats),as.matrix(d_target),standardize = FALSE, type.measure ="mse",alpha=1, nfolds=3)

#3. Results
autoplot(fit)
#with lambda.min
lambda_min = which(fit$lambda == fit$lambda.min)

#selecting coefficients of variables at lambda where mse is minimal
tempmincoefs = as.data.frame(fit$glmnet.fit$beta[, which(fit$lambda == fit$lambda.min)])
mincoefs =  data.frame(matrix(ncol = 2, nrow = (NROW(tempmincoefs))))
mincoefs$variables = as.vector(labels(tempmincoefs)[[1]])
mincoefs$coefs_minlambda = as.vector(tempmincoefs[[1]])
mincoefs$X1 = NULL
mincoefs$X2 = NULL

#get names in the decreasing order they appear in when lambda is minimal

names = names(coef(model)[,ncol(coef(model))][order(coef(model)[,ncol(coef(model))],decreasing=TRUE)])

names = names[!names %in% c("(Intercept)")]




#set colors for displayed variables
mincoefs[mincoefs$coefs_minlambda > 0,]$variables
mincoefs[mincoefs$coefs_minlambda < 0,]$variables
disp_cols = data.frame(matrix(ncol = 2, nrow = NROW(tempmincoefs)))
disp_cols$vars = names
disp_cols$colors = c("grey")
disp_cols[disp_cols$vars %in% mincoefs[mincoefs$coefs_minlambda > 0,]$variables,]$colors = c("green")
disp_cols[disp_cols$vars %in% mincoefs[mincoefs$coefs_minlambda < 0,]$variables,]$colors = c("red")
disp_cols$X1 = NULL
disp_cols$X2 = NULL



dev.new(width=5, height=7)

plot_glmnet(model,s=fit$lambda.min,col =disp_cols$colors, label = TRUE )
coef(fit, s = "lambda.min")



#with lambda.1se
lambda.1se = which(fit$lambda == fit$lambda.1se)
coef(fit, s = "lambda.1se")
fit$glmnet.fit$beta[, which(fit$lambda == fit$lambda.1se)]
