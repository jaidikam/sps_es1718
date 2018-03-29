if(!require("fmsb")) install.packages("fmsb"); library("fmsb")
if(!require("glmnet")) install.packages("glmnet"); library("glmnet")
if(!require("VSURF")) install.packages("VSURF"); library("VSURF")
if(!require("plyr")) install.packages("plyr"); library("plyr")

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
  return(tempresults)
}




#calculate most important variables with LASSO for Lambda where MSE is minimal
if(!require("glmnet")) install.packages("glmnet"); library("glmnet")
if(!require("plyr")) install.packages("plyr"); library("plyr")

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
  lasso = glmnet(x = x, y = val,  standardize = TRUE, alpha = 1)
  
  fit   = cv.glmnet(x = x, y = val, standardize = TRUE, type.measure ="mse", alpha=1, nfolds=3)
  
  #3. Results
  #with lambda.min
  lambda_min = which(fit$lambda == fit$lambda.min)
  
  #selecting coefficients of variables at lambda where mse is minimal
  tempmincoefs             = as.data.frame(fit$glmnet.fit$beta[, which(fit$lambda == fit$lambda.min)])
  mincoefs                 =  data.frame(matrix(ncol = 2, nrow = (NROW(tempmincoefs))))
  mincoefs$variables       = as.vector(as.character(labels(tempmincoefs)[[1]]))
  mincoefs$coefs_minlambda = as.vector(tempmincoefs[[1]])
  mincoefs$X1              = NULL
  mincoefs$X2              = NULL
  
  #get names in the decreasing order they appear in when lambda is minimal
  names           = names(coef(lasso)[,ncol(coef(lasso))][order(coef(lasso)[,ncol(coef(lasso))],decreasing=TRUE)])
  names           = names[!names %in% c("(Intercept)")]
  names           = as.data.frame(names)
  colnames(names) = "variables"
  
  #add coefficient to names
  disp_colors = join(names,mincoefs, by = "variables" )
  disp_colors = disp_colors[!disp_colors$variables %in% c("(Intercept)"),]
  
  #set colors for variables  when displayed in a graph
  disp_colors$colors = 0
  if(NROW(disp_colors[disp_colors$coefs_minlambda >0,])>0){
    disp_colors[disp_colors$coefs_minlambda >0,]$colors = c("green")
  }
  if(NROW(disp_colors[disp_colors$coefs_minlambda <0,])>0){
    disp_colors[disp_colors$coefs_minlambda <0,]$colors = c("red")
  }
  
  #create a list to store the result
  resultset =  vector("list",3)
  resultset[[1]] = lasso
  resultset[[2]] = fit
  resultset[[3]] = disp_colors
  
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

