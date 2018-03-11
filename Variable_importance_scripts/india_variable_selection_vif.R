if(!require("Hmisc")) install.packages("Hmisc"); library("Hmisc")
if(!require("corrplot")) install.packages("corrplot"); library("corrplot")
if(!require("caret")) install.packages("caret"); library("caret")

source(".\\Helper_functions\\variable_selection_functions.R")

india = readRDS(".\\Processed_ds\\india_fin.rds")

#1. initial variable selection and normalization
colselection = c("avg_price_prod_year","pr_q1","pr_q2","pr_q3","pr_q4","tas_q1","tas_q2","tas_q3","tas_q4",
                 "prod_amount_y","daily_caloric_supply","exp_sug","exp_veg","exp_cer","imp_sug","imp_veg","imp_cer", 
                 "agri_gdp","gni_pc","cp_inflation","avg_p_barrel","population") 
target = c("avg_price_prod_year")
normalized = as.data.frame(scale(india[colselection]))

feats = normalized[, !(colnames(normalized) %in% target)]

#2 Variable selection and modeling
#2.1 Model with all explanatory variables 
#2.1.1 print corrplot for all explanatory variables

insign_in = cor( feats, method = "pearson", use = "complete.obs")
corrplot(insign_in, type = "upper", order = "hclust", 
          tl.col = "black", tl.srt = 45)

#2.1.2 build model with all explanatory variables
expvarsall = paste(colnames(feats), collapse = '+')
formulaall = paste(target,"~", expvarsall, collapse = " ")
mod_varall = summary(lm(formulaall ,data = normalized))

#  Regression coefficients have NAs, high R^2 but few signifikant predictors
# => indicator for multicolinearity
# we should remove some explanatory variables!

#2.2.Model with significant explanatory variables 
#2.2.1 check pvalue for correlation target <=> explanatory variables (alpha = 0,05)

sign_in = rcorr(as.matrix(normalized))
cors <- as.data.frame(sign_in$r)
pvals = as.data.frame(sign_in$P) 
pvalsr = pvals[pvals$avg_price_prod_year < 5*10^-2,]
vars = rownames(pvalsr)
vars = vars[vars != "NA"]
#vars = append(vars,"avg_price_prod_year")
nona_sign_in = cor(normalized[colnames(feats) %in% vars, ], method = "pearson", use = "complete.obs")
corrplot(nona_sign_in, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

#2.2.2 build model with significant explanatory variables only
expvarssig = paste(vars, collapse = "+")
formulasig = paste(target,"~",expvarssig,collapse ="+")
mod_varsig = summary(lm(formulasig,data = normalized))

#no more NAs in coefficients, still only one significant variable in lm model




#2.3. model with produced amount and population only
mod_varsmall = summary(lm(avg_price_prod_year ~ 
              + prod_amount_y + population ,data = normalized))

#according to oecd food prices (just like any other price...) should be mostly influenced by 
#supply and demand. So if we just pick one supply related factor (produced amount) and one for demand
#(population size) we find out, they are both significant. 
#But R^2 is getting lower. There could be more to find out.

#2.4.Stepwise approach to tackle multicolinearity
#2.4.1 Discovering highly correlated explanatory variables

hicorvars = findCorrelation(cor(feats), cutoff = 0.70)
expvarsnohc = paste(colnames(feats[,-hicorvars]), collapse = "+")
formulanohc = paste(target,"~",expvarsnohc,collapse = "+")
mod_varnohc = summary(lm(formulanohc,data = normalized))

#vegetable imports are sign of demand, produced amount is supply.
#seems like we're getting somewhere but we threw away features known to be significant.
#the model still has high adjusted R^2

#2.4.2 Multicolinearity removal 
# for highly correlated variables
varslovifhc = removeVif(feats[,hicorvars],8) 
# the rest
varslovifnohc = removeVif(feats[,-hicorvars],8) 
#2.3.4 Model without multicolinearity
expvars_lovif = paste(paste(varslovifhc,collapse = "+"),"+",paste(varslovifnohc,collapse = "+"),collapse = "+")
formula_lovif = paste(target,"~",expvars_lovif,collapse = "+")
mod_lovif = summary(lm(formula_lovif,data = normalized))

##just as stated by oecd, supply (produced amount) and demand (population size) are the significant
#factors for food prices.
#R^2 is similar to the model built without all highly correlated variables
rm(list = setdiff(ls(), lsf.str()))
