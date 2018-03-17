#this script shall load all final dataset files created in the preparation stage and include the code for displaying 
# eplorative graphs and tables.


# plotting the population for the all countries and the world
source(".\\Helper_functions\\preparation_functions.r")
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

world_production = read.csv(".\\Comman datasets\\world production.csv", stringsAsFactors = FALSE, sep = ",", header = TRUE)
world_production$X = NULL
colnames(world_production) = c("Area", "Item", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015")

world_production = melt(world_production, id=c("Area","Item"), value.name = "Production_Amount")
# select the intersting items
world_production = world_production[world_production$Item %in% c("Sugar cane", "Rice, paddy", "Wheat", "Potatoes",
                                                     "Bananas", "Coconuts",
                                                     "Cassava", "Beans, dry", "Maize", "Sweet potatoes"),]
# remove some uninterstting itmes specified by a land
world_production = world_production[!(world_production$Area == "India" & world_production$Item %in% c("Cassava", "Bananas", "Beans, dry", "Maize", "Sweet potatoes", "Coconuts")),]

world_production = world_production[!(world_production$Area == "Philippines" & world_production$Item %in% c("Cassava", "Wheat", "Beans, dry", "Maize", "Sweet potatoes", "Sugar cane", "Potatoes")),]

world_production = world_production[!(world_production$Area == "Rwanda" & world_production$Item %in% c("Wheat", "Sugar cane", "Coconuts")),]
colnames(world_production)[3] = "Year"
world_production$Year = as.numeric(levels(world_production$Year))[world_production$Year] 
# ggplot(world_production, aes(x = Year)) +
#   geom_line(aes(y = India_Potatoes), colour="blue") +
#   geom_line(aes(y = World_Potatoes), colour = "grey") +
#   ylab(label="precentage") +
#   xlab("Year")
# 
# require(ggplot2)
# require(scales)
# 
# ggplot(world_production, aes(x = Year)) +  
#   geom_bar(aes(y = (..India_Potatoes..)/sum(..India_Potatoes..))) +
#   ## version 3.0.9
#   # scale_y_continuous(labels = percent_format())
#   ## version 3.1.0
#   scale_y_continuous(labels = scales::percent)
# 
# ###########################3
# 
# d = data.frame()
# final_d = world_production
# for(j in colnames(world_production[,-1])){
#   d = calcPercPreBaseyear(world_production, j, "Year")
#   final_d = merge(x = final_d, y = d, by= colnames(world_production), all.x =TRUE)
# }
# ggplot(final_d, aes(x = Year)) + 
#    geom_line(aes(y = India_Potatoes_perc), colour="blue") + 
#    geom_line(aes(y = World_Potatoes_perc), colour = "grey") + 
#    ylab(label="precentage") + 
#    xlab("Year")
# 


####################################

wp = as.matrix(world_production)
world_production$Production_Amount = scale(world_production$Production_Amount)

ggplot(world_production, aes(x = Year)) +
  geom_line(aes(y = world_production[world_production$Area == "India" & world_production$Item == "Potatoes","value"]), colour="blue") +
  geom_line(aes(y = world_production[world_production$Area == "World" & world_production$Item == "Potatoes","value"]), colour = "grey") +
  ylab(label="precentage") +
  xlab("Year")

ggplot(world_production, aes(x = unique(Year))) +
  geom_line(aes(y = world_production[world_production$Area == "India", "value"]), colour="blue") +
  #geom_line(aes(y = world_production[world_production$Area == "World" & world_production$Item == "Potatoes","value"]), colour = "grey") +
  ylab(label="precentage") +
  xlab("Year")



p1 <- ggplot(world_production, aes(x=Year[],
                     y=price[product=='p3'],
                     colour=factor(skew[product == 'p1']))) +
  geom_point(size=2, shape=19)

#####################################################################################################

# source FAO
price_index = read.csv(".\\Comman datasets\\Food_price_indices_data.csv", stringsAsFactors = FALSE, sep = ",")
price_index[,8:16] = NULL
price_index$Date = as.yearmon(price_index$Date, format = "%m/%Y")
price_index = price_index[!price_index$Date %in% c(1990,2016,2017,2018),]

# creating and saving the plot
jpeg(".//Plots//price_index.jpg")
ggplot(price_index, aes(x = Date)) +
  geom_line(aes(y = Food.Price.Index, colour="Food")) +
  geom_line(aes(y = Cereals.Price.Index, colour="Cereals")) +
  geom_line(aes(y = Oils.Price.Index, colour="Oils")) +
  geom_line(aes(y = Sugar.Price.Index, colour="Sugar")) +
  scale_x_yearmon(format="%m/%Y", n=5)+
  ylab(label="Price Index") +
  xlab("Year")
dev.off()


