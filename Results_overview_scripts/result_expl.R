if(!require("plotmo")) install.packages("plotmo"); library("plotmo")
if(!require("Hmisc")) install.packages("Hmisc"); library("Hmisc")
if(!require("corrplot")) install.packages("corrplot"); library("corrplot")
if(!require("caret")) install.packages("caret"); library("caret")
if(!require("ggfortify")) install.packages("ggfortify"); library("ggfortify")

source(".\\Helper_functions\\exploration_functions.r")


#results for VIF
india_insig_vif = readRDS(".\\Results\\insign_in.rds")
india_nohc_vif  = readRDS(".\\Results\\mod_varnohc_in.rds")
india_lo_vif    = readRDS(".\\Results\\mod_lovif_in.rds")

rwanda_insig_vif = readRDS(".\\Results\\insign_rw.rds")
rwanda_nohc_vif  = readRDS(".\\Results\\mod_varnohc_rw.rds")
rwanda_lo_vif    = readRDS(".\\Results\\mod_lovif_rw.rds")

philippines_insig_vif = readRDS(".\\Results\\insign_ph.rds")
philippines_nohc_vif  = readRDS(".\\Results\\mod_varnohc_ph.rds")
philippines_lo_vif    = readRDS(".\\Results\\mod_lovif_ph.rds")


##corrplot for all variables

jpeg(".//Results//Rs_plots//india_corr.jpg", width = 1120, height = 1000, units = "px", pointsize = 20,
     quality = 100)
corrplot(india_insig_vif, type = "lower", order = "hclust", 
         tl.col = "black", tl.srt = 45)
title("Pairwise Correlations India")
dev.off()
highcorrcolnames_in = colnames(india_insig_vif)[findCorrelation(india_insig_vif, cutoff = 0.70)]
sink(".\\Results\\Rs_data\\hcvars_in.txt")
print(highcorrcolnames_in)
sink() 


jpeg(".//Results//Rs_plots//rwanda_corr.jpg", width = 1120, height = 1000, units = "px", pointsize = 20,
     quality = 100)
corrplot(rwanda_insig_vif, type = "lower", order = "hclust", 
         tl.col = "black", tl.srt = 45)
title("Pairwise Correlations Rwanda")
dev.off()
highcorrcolnames_rw = colnames(rwanda_insig_vif)[findCorrelation(rwanda_insig_vif, cutoff = 0.70)]
sink(".\\Results\\Rs_data\\hcvars_rw.txt")
print(highcorrcolnames_rw)
sink() 


jpeg(".//Results//Rs_plots//philippines_corr.jpg", width = 1120, height = 1000, units = "px", pointsize = 20,
     quality = 100)
corrplot(philippines_insig_vif, type = "lower", order = "hclust", 
         tl.col = "black", tl.srt = 45)
title("Pairwise Correlations Philippines")
dev.off()
highcorrcolnames_ph = colnames(philippines_insig_vif)[findCorrelation(philippines_insig_vif, cutoff = 0.70)]
sink(".\\Results\\Rs_data\\hcvars_ph.txt")
print(highcorrcolnames_ph)
sink() 


##significant variables left after removal of all variables part of a pair with correlation >0.7

sink(".\\Results\\Rs_data\\nohc_mod_in.txt")
print(summary(india_nohc_vif))
sink() 


sink(".\\Results\\Rs_data\\nohc_mod_rw.txt")
print(summary(rwanda_nohc_vif))
sink() 

sink(".\\Results\\Rs_data\\nohc_mod_ph.txt")
print(summary(philippines_nohc_vif))
sink() 


##significant variables left after removal of multicorrelated variables by removeVif()
sink(".\\Results\\Rs_data\\vif_mod_in.txt")
print(summary(india_lo_vif))
sink() 


sink(".\\Results\\Rs_data\\vif_mod_rw.txt")
print(summary(rwanda_lo_vif))
sink() 

sink(".\\Results\\Rs_data\\vif_mod_ph.txt")
print(summary(philippines_lo_vif))
sink() 



#results for lasso
india_lasso_result = readRDS(".\\Results\\india_lasso.rds")
rwanda_lasso_result = readRDS(".\\Results\\rwanda_lasso.rds")
philippines_lasso_result = readRDS(".\\Results\\philippines_lasso.rds")



#India
#Optimal number of variables -> where MSE is minimal
jpeg(".//Results//Rs_plots//india_lasso_mse.jpg", width = 600, height = 600, units = "px", pointsize = 20,
     quality = 100)
autoplot(india_lasso_result[[2]], main = "Optimal number of variables by MSE for India")
dev.off()
#At what Lambda do variables enter the model
#jpeg(".//Results//Rs_plots//india_lasso_lambda.jpg", width = 1200, height = 1400, units = "px", pointsize = 20,
#     quality = 100)
#plot_glmnet(india_lasso_result[[1]],s=india_lasso_result[[2]]$lambda.min,col =india_lasso_result[[3]]$colors, label = TRUE )
#dev.off()
sink(".\\Results\\Rs_data\\lasso_vars_in.txt")
print(india_lasso_result[[3]])
sink() 


#Rwanda
#Optimal number of variables -> where MSE is minimal
jpeg(".//Results//Rs_plots//rwanda_lasso_mse.jpg", width = 600, height = 600, units = "px", pointsize = 20,
     quality = 100)
autoplot(rwanda_lasso_result[[2]], main = "Optimal number of variables by MSE for Rwanda")
dev.off()
#At what Lambda do variables enter the model
#jpeg(".//Results//Rs_plots//rwanda_lasso_lambda.jpg", width = 1200, height = 1200, units = "px", pointsize = 20,
#     quality = 100)
#plot_glmnet(rwanda_lasso_result[[1]],s=rwanda_lasso_result[[2]]$lambda.min,col =rwanda_lasso_result[[3]]$colors, label = TRUE )
#dev.off()
sink(".\\Results\\Rs_data\\lasso_vars_rw.txt")
print(rwanda_lasso_result[[3]])
sink() 


#Philippines
#Optimal number of variables -> where MSE is minimal
jpeg(".//Results//Rs_plots//philippines_lasso_mse.jpg", width = 600, height = 600, units = "px", pointsize = 20,
     quality = 100)
autoplot(philippines_lasso_result[[2]], main = "Optimal number of variables by MSE for Philippines")
dev.off()
#At what Lambda do variables enter the model
#jpeg(".//Results//Rs_plots//philippines_lasso_lambda.jpg", width = 1200, height = 1200, units = "px", pointsize = 20,
#     quality = 100)
#plot_glmnet(philippines_lasso_result[[1]],s=philippines_lasso_result[[2]]$lambda.min,col =philippines_lasso_result[[3]]$colors, label = TRUE )
#dev.off()
sink(".\\Results\\Rs_data\\lasso_vars_ph.txt")
print(philippines_lasso_result[[3]])
sink() 


#results for random forest based variable selection 
india_rf_result = readRDS(".\\Results\\india_rf.rds")
rwanda_rf_result = readRDS(".\\Results\\rwanda_rf.rds")
philippines_rf_result = readRDS(".\\Results\\philippines_rf.rds")

#threshold step india
sink(".\\Results\\Rs_data\\rf_thres_in.txt")
print(india_rf_result[[2]][india_rf_result[[1]]$varselect.thres])
sink() 

jpeg(".//Results//Rs_plots//india_rf_thres.jpg", width = 1000, height = 700, units = "px", pointsize = 20,
     quality = 100)
plotVsurf(india_rf_result[[1]],"thres","India")
dev.off()

#interpretation step india
sink(".\\Results\\Rs_data\\rf_interp_in.txt")
print(india_rf_result[[2]][india_rf_result[[1]]$varselect.interp])
sink()

jpeg(".//Results//Rs_plots//india_rf_interp.jpg", width = 1000, height = 700, units = "px", pointsize = 20,
     quality = 100)
plotVsurf(india_rf_result[[1]],"interp","India")
dev.off()


#threshold step rwanda
sink(".\\Results\\Rs_data\\rf_thres_rw.txt")
print(rwanda_rf_result[[2]][rwanda_rf_result[[1]]$varselect.thres])
sink() 

jpeg(".//Results//Rs_plots//rwanda_rf_thres.jpg", width = 1000, height = 700, units = "px", pointsize = 20,
     quality = 100)
plotVsurf(rwanda_rf_result[[1]],"thres","Rwanda")
dev.off()

#interpretation step rwanda
sink(".\\Results\\Rs_data\\rf_interp_rw.txt")
print(rwanda_rf_result[[2]][rwanda_rf_result[[1]]$varselect.interp])
sink()

jpeg(".//Results//Rs_plots//rwanda_rf_interp.jpg", width = 1000, height = 700, units = "px", pointsize = 20,
     quality = 100)
plotVsurf(rwanda_rf_result[[1]],"interp","Rwanda")
dev.off()


#threshold step philippines
sink(".\\Results\\Rs_data\\rf_thres_ph.txt")
print(philippines_rf_result[[2]][philippines_rf_result[[1]]$varselect.thres])
sink() 

jpeg(".//Results//Rs_plots//philippines_rf_thres.jpg", width = 1000, height = 700, units = "px", pointsize = 20,
     quality = 100)
plotVsurf(philippines_rf_result[[1]],"thres","Philippines")
dev.off()

#interpretation step philippines
sink(".\\Results\\Rs_data\\rf_interp_ph.txt")
print(philippines_rf_result[[2]][philippines_rf_result[[1]]$varselect.interp])
sink()

jpeg(".//Results//Rs_plots//philippines_rf_interp.jpg", width = 1000, height = 700, units = "px", pointsize = 20,
     quality = 100)
plotVsurf(philippines_rf_result[[1]],"interp","Philippines")
dev.off()


#cleanup
rm(list = setdiff(ls(), lsf.str()))