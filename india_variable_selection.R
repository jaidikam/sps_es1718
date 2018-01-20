if(!require("Hmisc")) install.packages("Hmisc"); library("Hmisc")
if(!require("corrplot")) install.packages("corrplot"); library("corrplot")

india = readRDS(".\\india.rds")
indiafoods = unique(india$cm_name)

#1. initial variable selection and normalization
colselection = c("avg_price_prod_year","pr_q1","pr_q2","pr_q3","pr_q4","tas_q1","tas_q2","tas_q3","tas_q4",
                 "prod_amount_y","daily_caloric_supply","exp_sug","exp_veg","exp_cer","imp_sug","imp_veg","imp_cer", 
                 "agri_gdp","gni_pc","cp_inflation","avg_p_barrel","population") 
normalized = as.data.frame(scale(india[colselection]))

#2.Model with all variables 
#2.1 print corrplot for all variables


foo_insign = cor( normalized, method = "pearson", use = "complete.obs")
corrplot(foo_insign, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

#2.2 build model with all variables

summary(lm(avg_price_prod_year ~ pr_q1 + pr_q2 + pr_q3 + pr_q4 + tas_q1 + tas_q2 + tas_q3 + tas_q4
           + prod_amount_y + daily_caloric_supply + exp_sug + exp_veg + exp_cer + imp_sug + imp_veg + imp_cer
           + agri_gdp + gni_pc + cp_inflation + avg_p_barrel + population ,data = normalized))

#3.Model with all variables 
#3.1 check correlation for statistically significant variables only (pvalue = 0,05)

boo = rcorr(as.matrix(normalized))

cors <- as.data.frame(boo$r)

pvals = as.data.frame(boo$P) 
pvalsr = pvals[pvals$avg_price_prod_year < 5*10^-2,]
vars = rownames(pvalsr)
vars = vars[vars != "NA"]
foo = cor(normalized[colnames(normalized) %in% vars | colnames(normalized) == "avg_price_prod_year" ], method = "pearson", use = "complete.obs")
corrplot(foo, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

#3.2 build model with significant variables only

summary(lm(avg_price_prod_year ~ pr_q1+ prod_amount_y +daily_caloric_supply + exp_sug
           +exp_veg   + exp_cer + imp_sug + imp_veg +
             +agri_gdp  + gni_pc +  cp_inflation + avg_p_barrel + population
           ,data = normalized))






#4. model with produced amount and population only
summary(lm(avg_price_prod_year ~ 
           + prod_amount_y + population ,data = normalized))

#5.multicolinearity removal
# Every model so far has high R^2 but few signifikant predictors
# => indicator for multicolinearity

#maybe VIF or manually, see function in bottom section

#6. model with variables left after multicolinearity removal
summary(lm(avg_price_prod_year ~ 
             tas_q3+ tas_q4 +  prod_amount_y 
           + exp_sug + exp_veg + exp_cer + imp_cer + cp_inflation,data = normalized))


#not mine, don't use officially!! unless adapted recreation.

vif_func<-function(in_frame,thresh=10,trace=T,...){
  
  library(fmsb)
  
  if(any(!'data.frame' %in% class(in_frame))) in_frame<-data.frame(in_frame)
  
  #get initial vif value for all comparisons of variables
  vif_init<-NULL
  var_names <- names(in_frame)
  for(val in var_names){
    regressors <- var_names[-which(var_names == val)]
    form <- paste(regressors, collapse = '+')
    form_in <- formula(paste(val, '~', form))
    vif_init<-rbind(vif_init, c(val, VIF(lm(form_in, data = in_frame, ...))))
  }
  vif_max<-max(as.numeric(vif_init[,2]), na.rm = TRUE)
  
  if(vif_max < thresh){
    if(trace==T){ #print output of each iteration
      prmatrix(vif_init,collab=c('var','vif'),rowlab=rep('',nrow(vif_init)),quote=F)
      cat('\n')
      cat(paste('All variables have VIF < ', thresh,', max VIF ',round(vif_max,2), sep=''),'\n\n')
    }
    return(var_names)
  }
  else{
    
    in_dat<-in_frame
    
    #backwards selection of explanatory variables, stops when all VIF values are below 'thresh'
    while(vif_max >= thresh){
      
      vif_vals<-NULL
      var_names <- names(in_dat)
      
      for(val in var_names){
        regressors <- var_names[-which(var_names == val)]
        form <- paste(regressors, collapse = '+')
        form_in <- formula(paste(val, '~', form))
        vif_add<-VIF(lm(form_in, data = in_dat, ...))
        vif_vals<-rbind(vif_vals,c(val,vif_add))
      }
      max_row<-which(vif_vals[,2] == max(as.numeric(vif_vals[,2]), na.rm = TRUE))[1]
      
      vif_max<-as.numeric(vif_vals[max_row,2])
      
      if(vif_max<thresh) break
      
      if(trace==T){ #print output of each iteration
        prmatrix(vif_vals,collab=c('var','vif'),rowlab=rep('',nrow(vif_vals)),quote=F)
        cat('\n')
        cat('removed: ',vif_vals[max_row,1],vif_max,'\n\n')
        flush.console()
      }
      
      in_dat<-in_dat[,!names(in_dat) %in% vif_vals[max_row,1]]
      
    }
    
    return(names(in_dat))
    
  }
  
}
vif_func(normalized[,!colnames(normalized) %in%c("avg_price_prod_year")])