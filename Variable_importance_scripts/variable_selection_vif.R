if(!require("Hmisc")) install.packages("Hmisc"); library("Hmisc")
if(!require("corrplot")) install.packages("corrplot"); library("corrplot")
if(!require("caret")) install.packages("caret"); library("caret")

source(".\\Helper_functions\\variable_selection_functions.R")

india = readRDS(".\\Processed_ds\\india_fin.rds")
rwanda = readRDS(".\\Processed_ds\\rwanda_fin.rds")

#initial variable selection and normalization
colselection_in = c("avg_price_prod_year","pr_q1","pr_q2","pr_q3","pr_q4","tas_q1","tas_q2","tas_q3","tas_q4",
                 "prod_amount_y","daily_caloric_supply","exp_sug","exp_veg","exp_cer","imp_sug","imp_veg","imp_cer", 
                 "agri_gdp","gni_pc","cp_inflation","avg_p_barrel","population") 
target_in = c("avg_price_prod_year")
normalized_in = as.data.frame(scale(india[colselection_in]))
feats_in = normalized_in[, !(colnames(normalized_in) %in% target_in)]

colselection_rw =  c("pr_q1", "pr_q2", "pr_q3", "pr_q4", "tas_q1", "tas_q2", "tas_q3", "tas_q4",
                "Production_Amount", "daily_caloric_supply", "exp_veg", "exp_cer", "imp_veg", "imp_cer", 
                "Agriculture_GDP", "GNI","Inflation", "oil_avarage_price_per_barrel", "Population_Value","Value")
target_rw = c("Value")
normalized_rw = as.data.frame(scale(rwanda[colselection_rw]))
feats_rw = normalized_rw[, !(colnames(normalized_rw) %in% target_rw)]

#Variable selection and modeling

#Model with all explanatory variables 
insign_in = cor( feats_in, method = "pearson", use = "complete.obs")
insign_rw = cor( feats_rw, method = "pearson", use = "complete.obs")
#print corrplot for all explanatory variables
corrplot(insign_in, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)





#save results
savestring = paste0(deparse(substitute(india)),"_correlations_raw.rds")
saveRDS(insign_in, (paste0(".\\Results\\",savestring)))

savestring = paste0(deparse(substitute(rwanda)),"_correlations_raw.rds")
saveRDS(insign_rw, (paste0(".\\Results\\",savestring)))