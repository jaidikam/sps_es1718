#this script shall load all result files created in the variable selection stage and include the code for displaying the results
#graphs and tables 


if(!require("plotmo")) install.packages("plotmo"); library("plotmo")
if(!require("Hmisc")) install.packages("Hmisc"); library("Hmisc")
if(!require("corrplot")) install.packages("corrplot"); library("corrplot")
if(!require("caret")) install.packages("caret"); library("caret")
if(!require("ggfortify")) install.packages("ggfortify"); library("ggfortify")



#results for VIF
india_insig_vif = readRDS(".\\Results\\insign_in.rds")
india_nohc_vif = readRDS(".\\Results\\mod_varnohc_in.rds")
india_lo_vif = readRDS(".\\Results\\mod_lovif_in.rds")

rwanda_insig_vif = readRDS(".\\Results\\insign_rw.rds")
rwanda_nohc_vif = readRDS(".\\Results\\mod_varnohc_rw.rds")
rwanda_lo_vif = readRDS(".\\Results\\mod_lovif_rw.rds")

philippines_insig_vif = readRDS(".\\Results\\insign_ph.rds")
philippines_nohc_vif = readRDS(".\\Results\\mod_varnohc_ph.rds")
philippines_lo_vif = readRDS(".\\Results\\mod_lovif_ph.rds")


##corrplot for all variables
corrplot(india_insig_vif, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

corrplot(rwanda_insig_vif = readRDS(".\\Results\\insign_rw.rds"), type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

corrplot(philippines_insig_vif, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

##significant variables left after removal of all variables part of a pair with correlation >0.7
summary(india_nohc_vif)
summary(rwanda_nohc_vif)
summary(philippines_nohc_vif)

##significant variables left after removal of multicorrelated variables by removeVif()
summary(india_lo_vif)
summary(rwanda_lo_vif)
summary(philippines_lo_vif)


#results for lasso
india_lasso_result = readRDS(".\\Results\\india_lasso.rds")
rwanda_lasso_result = readRDS(".\\Results\\rwanda_lasso.rds")
philippines_lasso_result = readRDS(".\\Results\\philippines_lasso.rds")

#feel free to play around with window sizes to achieve optimal size and readability of graphs
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

dev.new(width=5, height=7)
#Optimal number of variables -> where MSE is minimal
autoplot(philippines_lasso_result[[2]])
#At what Lambda do variables enter the model
dev.new(width=5, height=7)
plot_glmnet(philippines_lasso_result[[1]],s=philippines_lasso_result[[2]]$lambda.min,col =philippines_lasso_result[[3]]$colors, label = TRUE )


#results for random forest based variable selection 
india_rf_result = readRDS(".\\Results\\india_rf.rds")
rwanda_rf_result = readRDS(".\\Results\\rwanda_rf.rds")
philippines_rf_result = readRDS(".\\Results\\philippines_rf.rds")


india_rf_result[[2]][india_rf_result[[1]]$varselect.thres]

rwanda_rf_result[[2]][rwanda_rf_result[[1]]$varselect.thres]

philippines_rf_result[[2]][philippines_rf_result[[1]]$varselect.thres]
