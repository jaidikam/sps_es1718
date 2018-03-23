if(!require("glmnet")) install.packages("glmnet"); library("glmnet")

#load the data
india = readRDS(".\\Qfolder2\\Q2_india_fin.rds")

#select explanatory variables
exp_var_in = c("prod_price","pr_q1","pr_q2","pr_q3","pr_q4","tas_q1","tas_q2","tas_q3","tas_q4",
               "prod_amount","daily_caloric_supply","exp_sug","exp_veg","exp_cer","imp_sug","imp_veg","imp_cer", 
               "agri_gdp","gni_pc","cp_inflation","avg_p_barrel","population") 
india      = india[,colnames(india) %in%exp_var_in ]

#function for LASSO method  
impVarsLasso = function(ds,targ){
  
  #1. initial variable selection and normalization
  
  val = ds[[targ]]
  x = model.matrix(ds[[targ]]~.-1 , ds[!colnames(ds) %in% targ])
  
  #2. Applying the Lasso technique
  lasso <- glmnet(x = x, y = val,  standardize = TRUE, alpha = 1, nlambda = 100)
  
  fit = cv.glmnet(x = x, y = val, type.measure ="mse", alpha=1, nfolds=3)
  
  #3. Results
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
  
  #set colors for variables  when displayed in a graph
  mincoefs[mincoefs$coefs_minlambda > 0,]$variables
  mincoefs[mincoefs$coefs_minlambda < 0,]$variables
  disp_cols = data.frame(matrix(ncol = 2, nrow = NROW(tempmincoefs)))
  disp_cols$vars = names
  disp_cols$colors = c("grey")
  if(NROW(disp_cols[disp_cols$vars %in% mincoefs[mincoefs$coefs_minlambda > 0,]$variables,])>0){
    disp_cols[disp_cols$vars %in% mincoefs[mincoefs$coefs_minlambda > 0,]$variables,]$colors = c("green")
  }
  if(NROW( disp_cols[disp_cols$vars %in% mincoefs[mincoefs$coefs_minlambda < 0,]$variables,]$colors)>0){
    disp_cols[disp_cols$vars %in% mincoefs[mincoefs$coefs_minlambda < 0,]$variables,]$colors = c("red")
  }
  disp_cols$X1 = NULL
  disp_cols$X2 = NULL
  
  #create a list to store the result
  resultset =  vector("list",3)
  resultset[[1]] = lasso
  resultset[[2]] = fit
  resultset[[3]] = disp_cols
  
  return(resultset)
}


#Get most important variables with Lasso function
india_lasso_result = impVarsLasso(india,"prod_price")

#save results
savestring = paste0(deparse(substitute(india)),"_lasso.rds")
saveRDS(india_lasso_result, (paste0(".\\Qfolder4\\","Q4_",savestring)))

#cleanup
rm(list = setdiff(ls(), lsf.str()))
