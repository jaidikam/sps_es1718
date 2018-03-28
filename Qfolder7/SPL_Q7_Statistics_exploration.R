

#reading the data  
world_population = read.csv(".\\Qfolder7\\world_population.csv", stringsAsFactors = FALSE, sep = ",", header = TRUE)
world_production = read.csv(".\\Qfolder7\\world_production.csv", stringsAsFactors = FALSE, sep = ",", header = TRUE)
price_index = read.csv(".\\Qfolder7\\Food_price_indices_data.csv", stringsAsFactors = FALSE, sep = ",")

rdata = readRDS(".\\Qfolder7\\rwanda_fin.rds")
pdata = readRDS(".\\Qfolder7\\philippines_fin.rds")
idata = readRDS(".\\Qfolder2\\Q2_india_fin.rds")

# libraries 
if(!require("reshape2")) install.packages("reshape2");library("reshape2")
if(!require("ggplot2")) install.packages("ggplot2");library("ggplot2")
if(!require("data.table")) install.packages("data.table");library("data.table")
if(!require("zoo")) install.packages("zoo");library("zoo")




#calculating the percentage of the change in a column's value on a fixed base year 
calcPercFixBaseyear =  function(ds, areacol, areaname, yearcol, baseyear,valuecol, perccol){
  base = ds[ds[[yearcol]] == baseyear & ds[[areacol]] %in% areaname, ][[valuecol]]
  if(is.null(ds[[perccol]])){
    ds[[perccol]] = 0 
  }
  #
  for(i in baseyear: max(ds[[yearcol]])){
    later = ds[ds[[yearcol]]==i & ds[[areacol]] %in% areaname,][[valuecol]]
    sub =  later - base
    ds[ds[[yearcol]] == i & ds[[areacol]] %in% areaname,][[perccol]] = (sub / later) * 100
  }
  return(ds)
}


# calculate the prcentage of changes in a colname value in a predefined year, where the base is for every change is the value from the previous year.
calcPercPreBaseyear = function(ds, areacol, areaname, yearcol, valuecol){
  for(i in unique(ds[[yearcol]])){
    base = ds[ds[[yearcol]] == i & ds[[areacol]] %in% areaname, ][[valuecol]]
    later = ds[ds[[yearcol]] == i+1 & ds[[areacol]] %in% areaname, ][[valuecol]]
    #if(length(later) == 0L) break
    if(i == 2015L) break
    sub =  later - base
    if(length(sub) == 0L)next 
    ds[ds[[yearcol]] == i+1 & ds[[areacol]] %in% areaname , paste(valuecol, "Percent", sep = "_")]= (sub / later) * 100
  }
  ds[ds[[yearcol]] == min(ds[[yearcol]]) & ds[[areacol]] %in% areaname, paste(valuecol, "Percent", sep = "_")]= 0
  return(ds)
}

# To plot production data with specific area and itmes
prodPlot = function(ds, area, items){
  p = ggplot(data=ds[ds$Area == area & ds$Item %in% items,], aes(x=Year, y=Percentage, colour=Item)) +
    geom_line() +
    geom_point()+
    ylim(-30, 75)+
    ggtitle(label=area)+
    ylab(label="Normalized Production Amount") +
    xlab("Year")
  return(p)
}

# bar plot for product price change 

prodBarPlot = function(d, dname){
  ggplot(d[d$year %in% c(2010:2015) ,c("year", "prod_name", "prod_price_Percent")], aes(x = year, y = prod_price_Percent)) +
    geom_bar(aes(fill = prod_name), position = "dodge", stat="identity") +
    ggtitle(label=dname)+
    ylab(label="price change based on previous year") +
    xlab("Year")
}

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


# 1. plotting the population for the all countries and the world

# stacks a set of columns into a single column of data to be able to process it 
world_population = melt(world_population, id=c("Year"), value.name = "population")

# calculating the percentage of the change in the population
for(i in unique(world_population$variable)){
  # i is the name of the land 
  #print(i)
  world_population = calcPercFixBaseyear(world_population,"variable",i,"Year",1991,"population", "percentage")
}

# creating and saving the plot
jpeg(".//Qfolder7//population_plot.jpg", width = 800, height = 480, units = "px", pointsize = 12,
     quality = 75)
ggplot(world_population) + geom_line(aes(x=Year, y=percentage, colour=variable), size=1.2) +
  scale_colour_manual(values=c("red","green","blue", "grey")) +
  ylab(label="Growth Precentage") +
  xlab("Year") 
dev.off()

############################################################################################################


# 2. plotting the production amount of the selected products for the specified countries compared to the world

# preparing the dataset 
world_production$X = NULL
colnames(world_production) = c("Area", "Item", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015")

world_production = melt(world_production, id=c("Area","Item"), value.name = "Production_Amount")
# select the intersting items
world_production = world_production[world_production$Item %in% c("Sugar cane", "Rice, paddy", "Wheat", "Potatoes",
                                                                 "Bananas", "Coconuts",
                                                                 "Cassava", "Beans, dry", "Maize", "Sweet potatoes"),]
# remove some uninterstting itmes specified by a land
world_production = world_production[!(world_production$Area == "India" & world_production$Item %in% c("Cassava", "Bananas", "Beans, dry", "Maize", "Sweet potatoes", "Coconuts")),]

world_production = world_production[!(world_production$Area == "Philippines" & world_production$Item %in% c("Cassava", "Wheat", "Beans, dry", "Maize", "Sweet potatoes", "Potatoes")),]

world_production = world_production[!(world_production$Area == "Rwanda" & world_production$Item %in% c("Wheat", "Sugar cane", "Coconuts")),]
colnames(world_production)[3] = "Year"
world_production$Year = as.numeric(levels(world_production$Year))[world_production$Year] 
# nornalize the production amount
#world_production$Production_Amount = scale(world_production$Production_Amount)
h = data.frame()
for(i in unique(world_production$Item)){
  d = world_production[world_production$Item == i,]
  for(j in unique(d$Area)){
    # i is the name of the land 
    #print(i)
    d = calcPercFixBaseyear(d,"Area",j,"Year",1991,"Production_Amount", "Percentage")
  }
  h = rbind(h,d)
}
world_production = h


# calling the plot function and get the plot
p1 = prodPlot(world_production, "India", c("Sugar cane", "Rice, paddy", "Wheat", "Potatoes"))
p2 = prodPlot(world_production, "World", c("Sugar cane", "Rice, paddy", "Wheat", "Potatoes"))
p3 = prodPlot(world_production, "Philippines", c("Sugar cane", "Bananas", "Coconuts", "Rice, paddy"))
p4 = prodPlot(world_production, "World", c("Sugar cane", "Bananas", "Coconuts", "Rice, paddy"))
# p5 = prodPlot(world_production, "Rwanda", c("Cassava", "Bananas", "Beans, dry", "Maize", "Sweet potatoes", "Potatoes", "Rice, paddy"))
# p6 = prodPlot(world_production, "World", c("Cassava", "Bananas", "Beans, dry", "Maize", "Sweet potatoes", "Potatoes", "Rice, paddy"))
p5 = prodPlot(world_production, "Rwanda", c("Cassava", "Bananas", "Maize", "Sweet potatoes"))
p6 = prodPlot(world_production, "World", c("Cassava", "Bananas", "Maize", "Sweet potatoes"))

# plot in one screnn and save the image 
jpeg(".//Qfolder7//production.jpg", width = 1200, height = 800, units = "px", pointsize = 12,
     quality = 75)
multiplot(p1, p3, p5,p2, p4,p6, cols=2)
dev.off()

#####################################################################################################

# 3.Ploting the price index 
price_index[,8:16] = NULL
price_index$Date = as.yearmon(price_index$Date, format = "%m/%Y")
price_index = price_index[!price_index$Date %in% c(1990,2016,2017,2018),]

# creating and saving the plot
jpeg(".//Qfolder7//price_index.jpg", width = 800, height = 480, units = "px", pointsize = 12,
     quality = 75)
ggplot(price_index, aes(x = Date)) +
  geom_line(aes(y = Food.Price.Index, colour="Food")) +
  geom_line(aes(y = Cereals.Price.Index, colour="Cereals")) +
  geom_line(aes(y = Oils.Price.Index, colour="Oils")) +
  geom_line(aes(y = Sugar.Price.Index, colour="Sugar")) +
  scale_x_yearmon(format="%m/%Y", n=5)+
  ylab(label="Price Index") +
  xlab("Year")
dev.off()

#######################################################################################################
# 4. Ploting a bar chart for each item form 2010 - 2015

for(i in unique(rdata$prod_name)){
  rdata = calcPercPreBaseyear(rdata, "prod_name", i, "year", "prod_price")
}
for(i in unique(pdata$prod_name)){
  pdata = calcPercPreBaseyear(pdata, "prod_name", i, "year", "prod_price")
}
for(i in unique(idata$prod_name)){
  idata = calcPercPreBaseyear(idata, "prod_name", i, "year", "prod_price")
}
idata[idata$year == 2012 & idata$prod_name == "Potatoes", "prod_price_Percent"] = 0
b1 = prodBarPlot(rdata, "Rwanda")
b2 = prodBarPlot(pdata, "Philippines")
b3 = prodBarPlot(idata, "India")

jpeg(".//Qfolder7//barplot_price_change.jpg", width = 1200, height = 800, units = "px", pointsize = 12,
     quality = 75)
multiplot(b1,b2,b3, cols=1)
dev.off()


#cleanup
rm(list = setdiff(ls(), lsf.str()))