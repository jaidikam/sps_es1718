source(".\\Helper_functions\\variable_selection_functions.R")

india = readRDS(".\\Processed_ds\\india_fin.rds")


#initial variable selection and normalization
colselection_in = c("prod_price","pr_q1","pr_q2","pr_q3","pr_q4","tas_q1","tas_q2","tas_q3","tas_q4",
                    "prod_amount","daily_caloric_supply","exp_sug","exp_veg","exp_cer","imp_sug","imp_veg","imp_cer", 
                    "agri_gdp","gni_pc","cp_inflation","avg_p_barrel","population") 
india = as.data.frame(scale(india[colselection_in]))

#run the model
india_v_imp_rf = impVarsRf(india,"prod_price")


#save results
savestring = paste0(deparse(substitute(india)),"_rf.rds")
saveRDS(india_v_imp_rf, (paste0(".\\Results\\",savestring)))


#results comparison
#rf:
colnames(normalized_in[india_v_imp_rf$varselect.thres])
"tas_q4"               
"imp_cer"              
"agri_gdp"             
"avg_p_barrel"         
"imp_sug"             
"exp_sug"              
"prod_amount"          
"exp_cer"              
"gni_pc"               
"imp_veg"             
"daily_caloric_supply"


#lasso
india_lasso[[3]][india_lasso[[3]]$colors %in% c("red","green"),]
prod_amount_y  
exp_veg  
agri_gdp  
imp_sug  
imp_veg  
imp_cer    
pr_q2

#vif 
summary(india_vif_result)
population  
prod_amount 


removeVif(normalized_in[colnames(normalized_in) %in% colnames(normalized_in[india_v_imp_rf$varselect.thres]) ],10)


#cleanup
rm(list = setdiff(ls(), lsf.str()))