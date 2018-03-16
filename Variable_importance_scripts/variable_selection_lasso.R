source(".\\Helper_functions\\variable_selection_functions.R")

rwanda = readRDS(".\\Processed_ds\\rwanda_fin.rds")
india = readRDS(".\\Processed_ds\\india_fin.rds")
philippines = readRDS(".\\Processed_ds\\philippines_fin.rds")
#select explanatory variables
exp_var_rw =  c("pr_q1", "pr_q2", "pr_q3", "pr_q4", "tas_q1", "tas_q2", "tas_q3", "tas_q4",
                            "prod_amount", "daily_caloric_supply", "exp_veg", "exp_cer", "imp_veg", "imp_cer", 
                    "Agriculture_GDP", "GNI","Inflation", "oil_avarage_price_per_barrel", "Population_Value","prod_price")
rwanda = rwanda[,colnames(rwanda) %in%exp_var_rw ]

exp_var_in = c("prod_price","pr_q1","pr_q2","pr_q3","pr_q4","tas_q1","tas_q2","tas_q3","tas_q4",
               "prod_amount","daily_caloric_supply","exp_sug","exp_veg","exp_cer","imp_sug","imp_veg","imp_cer", 
               "agri_gdp","gni_pc","cp_inflation","avg_p_barrel","population") 
india = india[,colnames(india) %in%exp_var_in ]

exp_var_ph =  c("prod_price","tas_q1","tas_q2","tas_q3","tas_q4","pr_q1","pr_q2","pr_q3",
                     "pr_q4","oil_avarage_price_per_barrel", "PopulationValue", "prod_amount","GNI", "ExchangeRate",
                     "GDP","Inflation","Agriculture_GDP","Import","Export","daily_caloric_supply")
philippines = philippines[,colnames(philippines) %in%exp_var_ph ]

#Get most important variables with Lasso method
rwanda_lasso_result = impVarsLasso(rwanda,"prod_price")
india_lasso_result = impVarsLasso(india,"prod_price")
philippines_lasso_result = impVarsLasso(philippines,"prod_price")

#save results
savestring = paste0(deparse(substitute(rwanda)),"_lasso.rds")
saveRDS(rwanda_lasso_result, (paste0(".\\Results\\",savestring)))

savestring = paste0(deparse(substitute(india)),"_lasso.rds")
saveRDS(india_lasso_result, (paste0(".\\Results\\",savestring)))

savestring = paste0(deparse(substitute(philippines)),"_lasso.rds")
saveRDS(philippines_lasso_result, (paste0(".\\Results\\",savestring)))


rm(list = setdiff(ls(), lsf.str()))
