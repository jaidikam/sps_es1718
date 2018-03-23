if(!require("fmsb")) install.packages("fmsb"); library("fmsb")
if(!require("Hmisc")) install.packages("Hmisc"); library("Hmisc")
if(!require("caret")) install.packages("caret"); library("caret")


#loading the data
india = readRDS(".\\Qfolder2\\Q2_india_fin.rds")

#initial variable selection and normalization
colselection_in = c("prod_price","pr_q1","pr_q2","pr_q3","pr_q4","tas_q1","tas_q2","tas_q3","tas_q4",
                    "prod_amount","daily_caloric_supply","exp_sug","exp_veg","exp_cer","imp_sug","imp_veg","imp_cer", 
                    "agri_gdp","gni_pc","cp_inflation","avg_p_barrel","population") 
target_in = c("prod_price")
normalized_in = as.data.frame(scale(india[colselection_in]))
feats_in = normalized_in[, !(colnames(normalized_in) %in% target_in)]


#Variable selection and modeling

#Model with all explanatory variables 
insign_in = cor( feats_in, method = "pearson", use = "complete.obs")




# Discovering highly correlated explanatory variables
hicorvars_in = findCorrelation(cor(feats_in), cutoff = 0.70)
expvarsnohc_in = paste(colnames(feats_in[,-hicorvars_in]), collapse = "+")
formulanohc_in = paste(target_in,"~",expvarsnohc_in,collapse = "+")
mod_varnohc_in = lm(formulanohc_in,data = normalized_in)



#Multicolinearity removal 
#function for VIF based stepwise removal of multicorrelated variables
removeVif = function(explan_vars,cutoffval=10){
  
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


# for highly correlated variables
varslovifhc_in = removeVif(feats_in[,hicorvars_in],8) 
# for lower correlated variables
varslovifnohc_in = removeVif(feats_in[,-hicorvars_in],8) 
#Model without multicolinearity
expvars_lovif_in = paste(paste(varslovifhc_in,collapse = "+"),"+",paste(varslovifnohc_in,collapse = "+"),collapse = "+")
formula_lovif_in = paste(target_in,"~",expvars_lovif_in,collapse = "+")
mod_lovif_in = lm(formula_lovif_in,data = normalized_in)


#save results
savesuffix = deparse(substitute(insign_in))
savestring = paste0(".\\Qfolder3\\","Q3_",savesuffix,".rds")
saveRDS(insign_in, savestring)

savesuffix = deparse(substitute(mod_varnohc_in))
savestring = paste0(".\\Qfolder3\\","Q3_",savesuffix,".rds")
saveRDS(mod_varnohc_in, savestring)

savesuffix = deparse(substitute(mod_lovif_in))
savestring = paste0(".\\Qfolder3\\","Q3_",savesuffix,".rds")
saveRDS(mod_lovif_in, savestring)


#cleanup
rm(list = setdiff(ls(), lsf.str()))