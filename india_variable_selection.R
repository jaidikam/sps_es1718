if(!require("Hmisc")) install.packages("Hmisc"); library("Hmisc")
if(!require("corrplot")) install.packages("corrplot"); library("corrplot")
if(!require("caret")) install.packages("caret"); library("caret")

#Write a little function
removeVif<-function(explan_vars,cutoffval=10){
  
  if(!require("fmsb")) install.packages("fmsb"); library("fmsb")
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



india = readRDS(".\\india.rds")
indiafoods = unique(india$cm_name)

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

foo_insign = cor( feats, method = "pearson", use = "complete.obs")
corrplot(foo_insign, type = "upper", order = "hclust", 
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

boo = rcorr(as.matrix(normalized))

cors <- as.data.frame(boo$r)

pvals = as.data.frame(boo$P) 
pvalsr = pvals[pvals$avg_price_prod_year < 5*10^-2,]
vars = rownames(pvalsr)
vars = vars[vars != "NA"]
#vars = append(vars,"avg_price_prod_year")
foo = cor(normalized[colnames(feats) %in% vars, ], method = "pearson", use = "complete.obs")
corrplot(foo, type = "upper", order = "hclust", 
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

#2.4.Stepwise approach to fight multicolinearity
#2.4.1 Discovering highly correlated explanatory variables

hicorvars = findCorrelation(cor(feats), cutoff = 0.7)
expvarsnohc = paste(colnames(feats[,-hicorvars]), collapse = "+")
formulanohc = paste(target,"~",expvarsnohc,collapse = "+")
mod_varnohc = summary(lm(paste(target, '~', expvarsnohc) ,data = normalized))

#vegetable imports are sign of demand, produced amount is supply.
#seems like we're getting somewhere but we threw away features known to be significant.

#2.4.2 Multicolinearity removal 
# for highly correlated variables
varslovifhc = removeVif(feats[,hicorvars],17) 
# the rest
varslovifnohc = removeVif(feats[,-hicorvars],17) 
#2.3.4 Model without multicolinearity
expvars_lovif = paste(paste(varslovifhc,collapse = "+"),"+",paste(varslovifnohc,collapse = "+"),collapse = "+")
formula_lovif = paste(target,"~",expvars_lovif,collapse = "+")
mod_varnohc = summary(lm(formula_lovif,data = normalized))

#

