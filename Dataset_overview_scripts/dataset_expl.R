#this script shall load all final dataset files created in the preparation stage and include the code for displaying 
# eplorative graphs and tables.


# plotting the population for the all countries and the world
source(".\\Helper_functions\\exploration_functions.r")

if(!require("reshape2")) install.packages("reshape2");library("reshape2")
if(!require("ggplot2")) install.packages("ggplot2");library("ggplot2")
if(!require("data.table")) install.packages("data.table");library("data.table")

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

world_production = read.csv(".\\Comman datasets\\coutries_and_world_production_data.csv", stringsAsFactors = FALSE, sep = ",", header = TRUE)


ggplot(world_production, aes(x = Year)) +
  geom_line(aes(y = India_Potatoes), colour="blue") +
  geom_line(aes(y = World_Potatoes), colour = "grey") +
  ylab(label="precentage") +
  xlab("Year")

require(ggplot2)
require(scales)

ggplot(world_production, aes(x = Year)) +  
  geom_bar(aes(y = (..India_Potatoes..)/sum(..India_Potatoes..))) +
  ## version 3.0.9
  # scale_y_continuous(labels = percent_format())
  ## version 3.1.0
  scale_y_continuous(labels = scales::percent)

###########################3

d = data.frame()
final_d = world_production
for(j in colnames(world_production[,-1])){
  d = calcPercPreBaseyear(world_production, j, "Year")
  final_d = merge(x = final_d, y = d, by= colnames(world_production), all.x =TRUE)
}
ggplot(final_d, aes(x = Year)) + 
   geom_line(aes(y = India_Potatoes_perc), colour="blue") + 
   geom_line(aes(y = World_Potatoes_perc), colour = "grey") + 
   ylab(label="precentage") + 
   xlab("Year")



####################################

wp = as.matrix(world_production)
wp2 = as.data.frame(scale(wp[,-1]))
wp2$Year = world_production$Year

ggplot(wp2, aes(x = Year)) +
  geom_line(aes(y = India_Potatoes), colour="blue") +
  geom_line(aes(y = World_Potatoes), colour = "grey") +
  ylab(label="precentage") +
  xlab("Year")
















