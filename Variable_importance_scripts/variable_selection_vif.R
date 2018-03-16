if(!require("Hmisc")) install.packages("Hmisc"); library("Hmisc")
if(!require("corrplot")) install.packages("corrplot"); library("corrplot")
if(!require("caret")) install.packages("caret"); library("caret")

source(".\\Helper_functions\\variable_selection_functions.R")

india = readRDS(".\\Processed_ds\\india_fin.rds")
rwanda = readRDS(".\\Processed_ds\\rwanda_fin.rds")
philippines = readRDS(".\\Processed_ds\\philippines_fin.rds")

#initial variable selection and normalization
colselection_in = c("prod_price","pr_q1","pr_q2","pr_q3","pr_q4","tas_q1","tas_q2","tas_q3","tas_q4",
                 "prod_amount","daily_caloric_supply","exp_sug","exp_veg","exp_cer","imp_sug","imp_veg","imp_cer", 
                 "agri_gdp","gni_pc","cp_inflation","avg_p_barrel","population") 
target_in = c("prod_price")
normalized_in = as.data.frame(scale(india[colselection_in]))
feats_in = normalized_in[, !(colnames(normalized_in) %in% target_in)]

colselection_rw =  c("pr_q1", "pr_q2", "pr_q3", "pr_q4", "tas_q1", "tas_q2", "tas_q3", "tas_q4",
                "Production_Amount", "daily_caloric_supply", "exp_veg", "exp_cer", "imp_veg", "imp_cer", 
                "Agriculture_GDP", "GNI","Inflation", "oil_avarage_price_per_barrel", "Population_Value","prod_price")
target_rw = c("prod_price")
normalized_rw = as.data.frame(scale(rwanda[colselection_rw]))
feats_rw = normalized_rw[, !(colnames(normalized_rw) %in% target_rw)]

colselection_ph =  c("prod_price","tas_q1","tas_q2","tas_q3","tas_q4","pr_q1","pr_q2","pr_q3",
                     "pr_q4","oil_avarage_price_per_barrel", "PopulationValue", "ProductionAmount","GNI", "ExchangeRate",
                     "GDP","Inflation","Agriculture_GDP","Import","Export","daily_caloric_supply")
target_ph = c("prod_price")
normalized_ph = as.data.frame(scale(philippines[colselection_ph]))
feats_ph = normalized_ph[, !(colnames(normalized_ph) %in% target_ph)]

#Variable selection and modeling

#Model with all explanatory variables 
insign_in = cor( feats_in, method = "pearson", use = "complete.obs")
insign_rw = cor( feats_rw, method = "pearson", use = "complete.obs")
insign_ph = cor( feats_ph, method = "pearson", use = "complete.obs")



# Discovering highly correlated explanatory variables
hicorvars_in = findCorrelation(cor(feats_in), cutoff = 0.70)
expvarsnohc_in = paste(colnames(feats_in[,-hicorvars_in]), collapse = "+")
formulanohc_in = paste(target_in,"~",expvarsnohc_in,collapse = "+")
mod_varnohc_in = lm(formulanohc_in,data = normalized_in)

hicorvars_rw = findCorrelation(cor(feats_rw), cutoff = 0.70)
expvarsnohc_rw = paste(colnames(feats_rw[,-hicorvars_rw]), collapse = "+")
formulanohc_rw = paste(target_rw,"~",expvarsnohc_rw,collapse = "+")
mod_varnohc_rw = lm(formulanohc_rw,data = normalized_rw)

hicorvars_ph = findCorrelation(cor(feats_ph), cutoff = 0.70)
expvarsnohc_ph = paste(colnames(feats_ph[,-hicorvars_ph]), collapse = "+")
formulanohc_ph = paste(target_ph,"~",expvarsnohc_ph,collapse = "+")
mod_varnohc_ph = lm(formulanohc_ph,data = normalized_ph)


#Multicolinearity removal 
# for highly correlated variables
varslovifhc_in = removeVif(feats_in[,hicorvars_in],8) 
varslovifhc_rw = removeVif(feats_rw[,hicorvars_rw],8) 
varslovifhc_ph = removeVif(feats_ph[,hicorvars_ph],8) 
# the rest
varslovifnohc_in = removeVif(feats_in[,-hicorvars_in],8) 
varslovifnohc_rw = removeVif(feats_rw[,-hicorvars_rw],8) 
varslovifnohc_ph = removeVif(feats_ph[,-hicorvars_ph],8) 
#Model without multicolinearity
expvars_lovif_in = paste(paste(varslovifhc_in,collapse = "+"),"+",paste(varslovifnohc_in,collapse = "+"),collapse = "+")
formula_lovif_in = paste(target_in,"~",expvars_lovif_in,collapse = "+")
mod_lovif_in = lm(formula_lovif_in,data = normalized_in)

expvars_lovif_rw = paste(paste(varslovifhc_rw,collapse = "+"),"+",paste(varslovifnohc_rw,collapse = "+"),collapse = "+")
formula_lovif_rw = paste(target_rw,"~",expvars_lovif_rw,collapse = "+")
mod_lovif_rw = lm(formula_lovif_rw,data = normalized_rw)

expvars_lovif_ph = paste(paste(varslovifhc_ph,collapse = "+"),"+",paste(varslovifnohc_ph,collapse = "+"),collapse = "+")
formula_lovif_ph = paste(target_ph,"~",expvars_lovif_ph,collapse = "+")
mod_lovif_ph = lm(formula_lovif_ph,data = normalized_ph)
#save results
savesuffix = deparse(substitute(insign_in))
savestring = paste0(savesuffix,".rds")
saveRDS(insign_in, (paste0(".\\Results\\",savestring)))

savesuffix = deparse(substitute(mod_varnohc_in))
savestring = paste0(savesuffix,".rds")
saveRDS(mod_varnohc_in, (paste0(".\\Results\\",savestring)))

savesuffix = deparse(substitute(mod_lovif_in))
savestring = paste0(savesuffix,".rds")
saveRDS(mod_lovif_in, (paste0(".\\Results\\",savestring)))

savesuffix = deparse(substitute(insign_rw))
savestring = paste0(savesuffix,".rds")
saveRDS(insign_rw, (paste0(".\\Results\\",savestring)))

savesuffix = deparse(substitute(mod_varnohc_rw))
savestring = paste0(savesuffix,".rds")
saveRDS(mod_varnohc_rw, (paste0(".\\Results\\",savestring)))

savesuffix = deparse(substitute(mod_lovif_rw))
savestring = paste0(savesuffix,".rds")
saveRDS(mod_lovif_rw, (paste0(".\\Results\\",savestring)))

savesuffix = deparse(substitute(insign_ph))
savestring = paste0(savesuffix,".rds")
saveRDS(insign_ph, (paste0(".\\Results\\",savestring)))

savesuffix = deparse(substitute(mod_varnohc_ph))
savestring = paste0(savesuffix,".rds")
saveRDS(mod_varnohc_ph, (paste0(".\\Results\\",savestring)))

savesuffix = deparse(substitute(mod_lovif_ph))
savestring = paste0(savesuffix,".rds")
saveRDS(mod_lovif_ph, (paste0(".\\Results\\",savestring)))


#cleanup
rm(list = setdiff(ls(), lsf.str()))