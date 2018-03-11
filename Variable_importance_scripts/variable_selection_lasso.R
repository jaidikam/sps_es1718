if(!require("plotmo")) install.packages("plotmo"); library("plotmo")
source(".\\Helper_functions\\variable_selection_functions.R")

rwanda = readRDS(".\\Processed_ds\\rwanda_fin.rds")
india = readRDS(".\\Processed_ds\\india_fin.rds")
#select explanatory variables
exp_var_rw =  c("pr_q1", "pr_q2", "pr_q3", "pr_q4", "tas_q1", "tas_q2", "tas_q3", "tas_q4",
                            "Production_Amount", "daily_caloric_supply", "exp_veg", "exp_cer", "imp_veg", "imp_cer", 
                    "Agriculture_GDP", "GNI","Inflation", "oil_avarage_price_per_barrel", "Population_Value","Value")
rwanda = rwanda[,colnames(rwanda) %in%exp_var_rw ]

exp_var_in = c("avg_price_prod_year","pr_q1","pr_q2","pr_q3","pr_q4","tas_q1","tas_q2","tas_q3","tas_q4",
               "prod_amount_y","daily_caloric_supply","exp_sug","exp_veg","exp_cer","imp_sug","imp_veg","imp_cer", 
               "agri_gdp","gni_pc","cp_inflation","avg_p_barrel","population") 
india = india[,colnames(india) %in%exp_var_in ]

#Get most important variables with Lasso method
rwanda_lasso_result = impVarsLasso(rwanda,"Value")
india_lasso_result = impVarsLasso(india,"avg_price_prod_year")

#save results
savestring = paste0(deparse(substitute(rwanda)),"_lasso.rds")
saveRDS(rwanda_lasso_result, (paste0(".\\Results\\",savestring)))

savestring = paste0(deparse(substitute(india)),"_lasso.rds")
saveRDS(india_lasso_result, (paste0(".\\Results\\",savestring)))

#plot graphs
dev.new(width=5, height=7)
#Optimal number of variables -> where MSE is minimal
autoplot(rwanda_lasso_result[[2]])
#At what Lambda do variables enter the model
dev.new(width=5, height=7)
plot_glmnet(rwanda_lasso_result[[1]],s=rwanda_lasso_result[[2]]$lambda.min,col =rwanda_lasso_result[[3]]$colors, label = TRUE )



dev.new(width=5, height=7)
#Optimal number of variables -> where MSE is minimal
autoplot(india_lasso_result[[2]])
#At what Lambda do variables enter the model
dev.new(width=5, height=7)
plot_glmnet(india_lasso_result[[1]],s=india_lasso_result[[2]]$lambda.min,col =india_lasso_result[[3]]$colors, label = TRUE )



rm(list = setdiff(ls(), lsf.str()))
