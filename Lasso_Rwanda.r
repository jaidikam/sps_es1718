setwd("C:/Users/Raiber/Desktop/SPL 2")
if(!require("glmnet")) install.packages("glmnet"); library("glmnet")
if(!require("ggfortify")) install.packages("ggfortify"); library("ggfortify")
if(!require("caret")) install.packages("caret"); library("caret")
if(!require("plotmo")) install.packages("plotmo"); library("plotmo")
#the bigger t, our cutoff value, the smaller ??, the shrinkage, gets


Rwanda = readRDS(".\\Rwanda.rds")


#1. initial variable selection and normalization

Rwanda <- Rwanda[,c("Item", "Year", "pr_q1", "pr_q2", "pr_q3", "pr_q4", "tas_q1", "tas_q2", "tas_q3", "tas_q4",
                    "Production_Amount", "daily_caloric_supply", "exp_veg", "exp_cer", "imp_veg", "imp_cer", 
                    "Agriculture_GDP", "GNI","Inflation", "oil_avarage_price_per_barrel", "Population_Value", "Value")]
Rwanda$Item <- as.factor(Rwanda$Item)
x <- model.matrix(Value ~. -1, Rwanda)
y <- Rwanda$Value

#2. Applying the Lasso technique
lasso <- glmnet(x = x, y = y,  standardize = TRUE, alpha = 1, nlambda = 100)
lasso
autoplot(lasso)

fit = cv.glmnet(x = x,y =y, type.measure ="mse", alpha=1, nfolds=3)

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

names = names(coef(lasso)[,ncol(coef(lasso))][order(coef(lasso)[,ncol(coef(lasso))],decreasing=TRUE)])

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

plot_glmnet(lasso,s=fit$lambda.min,col =disp_cols$colors, label = TRUE )
coef(fit, s = "lambda.min")



#with lambda.1se
lambda.1se = which(fit$lambda == fit$lambda.1se)
coef(fit, s = "lambda.1se")
fit$glmnet.fit$beta[, which(fit$lambda == fit$lambda.1se)]