#this script shall load all result files created in the variable selection stage and include the code for displaying the results
#graphs and tables 


if(!require("plotmo")) install.packages("plotmo"); library("plotmo")



#display graphs for lasso variable selection results
india_lasso_result = readRDS(".\\Results\\india_lasso.rds")
rwanda_lasso_result = readRDS(".\\Results\\rwanda_lasso.rds")


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

