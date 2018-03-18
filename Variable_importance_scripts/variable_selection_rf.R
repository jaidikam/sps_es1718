source(".\\Helper_functions\\variable_selection_functions.R")

india = readRDS(".\\Processed_ds\\india_fin.rds")
rwanda = readRDS(".\\Processed_ds\\rwanda_fin.rds")
philippines = readRDS(".\\Processed_ds\\philippines_fin.rds")



#initial variable selection and normalization
colselection_in = c("prod_price","pr_q1","pr_q2","pr_q3","pr_q4","tas_q1","tas_q2","tas_q3","tas_q4",
                    "prod_amount","daily_caloric_supply","exp_sug","exp_veg","exp_cer","imp_sug","imp_veg","imp_cer", 
                    "agri_gdp","gni_pc","cp_inflation","avg_p_barrel","population") 
india = as.data.frame(scale(india[colselection_in]))

colselection_rw =  c("pr_q1", "pr_q2", "pr_q3", "pr_q4", "tas_q1", "tas_q2", "tas_q3", "tas_q4",
                     "prod_amount", "daily_caloric_supply", "exp_veg", "exp_cer", "imp_veg", "imp_cer", 
                     "agri_gdp", "gni_pc","cp_inflation", "avg_p_barrel", "population","prod_price")
rwanda = as.data.frame(scale(rwanda[colselection_rw]))

colselection_ph =  c("prod_price","tas_q1","tas_q2","tas_q3","tas_q4","pr_q1","pr_q2","pr_q3",
                     "pr_q4","avg_p_barrel", "population", "prod_amount","gni_pc", "exchange_rate",
                     "gdp","cp_inflation","agri_gdp", "imp_cer","exp_agri","daily_caloric_supply")
philippines = as.data.frame(scale(philippines[colselection_ph]))

#run the model
india_v_imp_rf = impVarsRf(india,"prod_price")
rwanda_v_imp_rf = impVarsRf(rwanda,"prod_price")
philippines_v_imp_rf = impVarsRf(philippines,"prod_price")


#save results
savestring = paste0(deparse(substitute(india)),"_rf.rds")
saveRDS(india_v_imp_rf, (paste0(".\\Results\\",savestring)))

savestring = paste0(deparse(substitute(rwanda)),"_rf.rds")
saveRDS(rwanda_v_imp_rf, (paste0(".\\Results\\",savestring)))

savestring = paste0(deparse(substitute(philippines)),"_rf.rds")
saveRDS(philippines_v_imp_rf, (paste0(".\\Results\\",savestring)))

#cleanup
rm(list = setdiff(ls(), lsf.str()))