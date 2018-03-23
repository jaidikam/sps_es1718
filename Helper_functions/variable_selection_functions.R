if(!require("fmsb")) install.packages("fmsb"); library("fmsb")
if(!require("glmnet")) install.packages("glmnet"); library("glmnet")
if(!require("VSURF")) install.packages("VSURF"); library("VSURF")

#function for VIF based stepwise removal of multicorrelated variables
removeVif<-function(explan_vars,cutoffval=10){
 
  tempresults = as.data.frame(matrix(ncol = 2, nrow = 0))
  colnames(tempresults) = c("variable","vif")
  #initially calculate VIF for each explanatory variable
  for (i in 1:NROW(colnames(explan_vars)) ){
    temptarget = colnames(explan_vars)[i]
    tempexpvars = paste(colnames(explan_vars[,!(colnames(explan_vars) %in% temptarget)]),collapse = "+")
    tempformula = paste(temptarget,"~", tempexpvars, collapse = " ")
    
    tempresults[i,1] = temptarget 
    tempresults[i,2] = VIF(lm( tempformula,data = explan_vars))
  }
  print(tempresults[order(tempresults$vif),])
  #remove variable with highest VIF, calculate new VIF for remaining variables until all VIF are below cutoff value
  while(max(tempresults$vif) >= cutoffval){
    tempresults = tempresults[!tempresults$vif == max(tempresults$vif),]
    tempremvars = tempresults$variable
    for(j in 1: NROW(tempremvars)){
      temptarget = tempremvars[j]
      tempexpvars = paste(tempremvars[!tempremvars %in% temptarget],collapse = "+")
      tempformula = paste(temptarget,"~", tempexpvars, collapse = " ")
      
      tempresults[j,1] = temptarget 
      tempresults[j,2] = VIF(lm( tempformula,data = explan_vars))
    }
    
    print("Remaining variables:")
    print(tempresults[order(tempresults$vif),])
    cat("\n")
    
  }
  return(tempresults$variable)
}




#calculate most important variables with LASSO for Lambda where MSE is minimal
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


#function for finding most important variables based on random forest  
impVarsRf = function(ds,targ){

  result_rf = VSURF(ds[[targ]] ~ ., data = ds[!colnames(ds) %in% targ], ntree = 2000,
                nfor.thres = 50, nmin = 1, nfor.interp = 25, nsd = 1,
                nfor.pred = 25, nmj = 1, parallel = FALSE, ncores = detectCores() - 1,
                clusterType = "PSOCK")
  #create a list to store the result
  resultset =  vector("list",2)  
  resultset[[1]] = result_rf
  resultset[[2]] = colnames(ds[!colnames(ds) %in% targ])
  return(resultset)
}

