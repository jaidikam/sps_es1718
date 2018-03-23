if(!require("VSURF")) install.packages("VSURF"); library("VSURF")
#load the data
india = readRDS(".\\Processed_ds\\india_fin.rds")

#initial variable selection and normalization
colselection_in = c("prod_price","pr_q1","pr_q2","pr_q3","pr_q4","tas_q1","tas_q2","tas_q3","tas_q4",
                    "prod_amount","daily_caloric_supply","exp_sug","exp_veg","exp_cer","imp_sug","imp_veg","imp_cer", 
                    "agri_gdp","gni_pc","cp_inflation","avg_p_barrel","population") 
india = as.data.frame(scale(india[colselection_in]))

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

#apply the function
india_v_imp_rf = impVarsRf(india,"prod_price")

#save results
savestring = paste0(deparse(substitute(india)),"_rf.rds")
saveRDS(india_v_imp_rf, (paste0(".\\Qfolder5\\","Q5_",savestring)))


#cleanup
rm(list = setdiff(ls(), lsf.str()))