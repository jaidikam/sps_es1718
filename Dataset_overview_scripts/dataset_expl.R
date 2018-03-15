#this script shall load all final dataset files created in the preparation stage and include the code for displaying 
# eplorative graphs and tables.


# plotting the population for the all countries and the world
source(".\\Helper_functions\\preparation_functions.R")

if(!require("reshape2")) install.packages("reshape2");library("reshape2")
if(!require("ggplot2")) install.packages("ggplot2");library("ggplot2")

#reading the data  
world_population = read.csv(".\\Comman datasets\\world_population.csv", stringsAsFactors = FALSE, sep = ",", header = TRUE)

# stacks a set of columns into a single column of data to be able to process it 
world_population = melt(world_population, id=c("Year"))

# calculating the percentage of the change in the population
for(i in unique(world_population$variable)){
  # i is the name of the land 
  #print(i)
  world_population = calcPercFixBaseyear(world_population,"variable",i,"Year",1991,"value", "percentage")
}

# creating and saving the plot
jpeg(".//Plots//population_plot.jpg")
ggplot(world_population) + geom_line(aes(x=Year, y=percentage, colour=variable), size=1.5) +
  scale_colour_manual(values=c("red","green","blue", "gray")) +
  ylab(label="Growth Precentage") +
  xlab("Year") 
dev.off()

############################################################################################################


# plotting the production amount of the selected products for the specified countries compared to the world

