if(!require("plotmo")) install.packages("plotmo"); library("plotmo")
if(!require("Hmisc")) install.packages("Hmisc"); library("Hmisc")
if(!require("corrplot")) install.packages("corrplot"); library("corrplot")
if(!require("caret")) install.packages("caret"); library("caret")
if(!require("ggfortify")) install.packages("ggfortify"); library("ggfortify")



#function for plotting VSURF Objects
plotVsurf = function(iVsurfOb,iStep,iCountry){
  header_prefix = "not specified"
  if(iStep == "thres"){
    header_prefix = "Thresholding step"
  }
  if(!iStep == "thres"){
    header_prefix = "Interpretation step"
  }
  
  plot(iVsurfOb,step = iStep, var.names = FALSE,
       nvar.interp = length(iVsurfOb$varselect.thres), main = paste(header_prefix,iCountry))
}


#loading results for correlation / VIF based variable selection
india_insig_vif = readRDS(".\\Qfolder3\\Q3_insign_in.rds")
india_nohc_vif  = readRDS(".\\Qfolder3\\Q3_mod_varnohc_in.rds")
india_lo_vif    = readRDS(".\\Qfolder3\\Q3_mod_lovif_in.rds")

#create corrplots for all variables and save plot and variable names to file
jpeg(".//Qfolder6//Q6_india_corr.jpg", width = 1120, height = 1000, units = "px", pointsize = 20,
     quality = 100)
corrplot(india_insig_vif, type = "lower", order = "hclust", 
         tl.col = "black", tl.srt = 45)
title("Pairwise Correlations India")
dev.off()
highcorrcolnames_in = colnames(india_insig_vif)[findCorrelation(india_insig_vif, cutoff = 0.70)]
sink(".\\Qfolder6\\Q6_hcvars_in.txt")
print(highcorrcolnames_in)
sink() 


#significant variables left after removal of all variables part of a pair with correlation >0.7
#save results to file

sink(".\\Qfolder6\\Q6_nohc_mod_in.txt")
print(summary(india_nohc_vif))
sink() 

#significant variables left after removal of multicorrelated variables by removeVif()
#save results to file
sink(".\\Qfolder6\\Q6_vif_mod_in.txt")
print(summary(india_lo_vif))
sink() 



#load results for lasso
india_lasso_result = readRDS(".\\Qfolder4\\Q4_india_lasso.rds")

#Optimal number of variables -> where MSE is minimal
#save plot and variable names to file
jpeg(".//Qfolder6//Q6_india_lasso_mse.jpg", width = 600, height = 600, units = "px", pointsize = 20,
     quality = 100)
autoplot(india_lasso_result[[2]], main = "Optimal number of variables by MSE for India")
dev.off()
#At what Lambda do variables enter the model
#jpeg(".//Qfolder6//Q6_india_lasso_lambda.jpg", width = 1200, height = 1400, units = "px", pointsize = 20,
#     quality = 100)
#plot_glmnet(india_lasso_result[[1]],s=india_lasso_result[[2]]$lambda.min,col =india_lasso_result[[3]]$colors, label = TRUE )
#dev.off()
sink(".\\Qfolder6\\Q6_lasso_vars_in.txt")
print(india_lasso_result[[3]])
sink() 




#loading results for random forest based variable selection 
india_rf_result = readRDS(".\\Qfolder5\\Q5_india_rf.rds")

#threshold step 
#save variables and plot to file
sink(".\\Qfolder6\\Q6_rf_thres_in.txt")
print(india_rf_result[[2]][india_rf_result[[1]]$varselect.thres])
sink() 

jpeg(".//Qfolder6//Q6_india_rf_thres.jpg", width = 1000, height = 700, units = "px", pointsize = 20,
     quality = 100)
plotVsurf(india_rf_result[[1]],"thres","India")
dev.off()

#interpretation step 
sink(".\\Qfolder6\\Q6_rf_interp_in.txt")
print(india_rf_result[[2]][india_rf_result[[1]]$varselect.interp])
sink()

jpeg(".//Qfolder6//Q6_india_rf_interp.jpg", width = 1000, height = 700, units = "px", pointsize = 20,
     quality = 100)
plotVsurf(india_rf_result[[1]],"interp","India")
dev.off()




#cleanup
rm(list = setdiff(ls(), lsf.str()))