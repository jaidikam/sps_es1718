setwd("C:/Users/Raiber/Desktop/HU Lectures/Statistical Programming/sps_ws1718/Rwanda datasets")
#setwd(".\\Rwanda datasets\")
library(data.table)
if(!require("plyr")) install.packages("plyr"); library("plyr")
if(!require("Hmisc")) install.packages("Hmisc"); library("Hmisc")
if(!require("corrplot")) install.packages("corrplot"); library("corrplot")
if(!require("ggplot2")) install.packages("ggplot2");library("ggplot2")
if(!require("grid")) install.packages("grid");library("grid")
if(!require("gridExtra")) install.packages("gridExtra");library("gridExtra")
if(!require("data.table")) install.packages("data.table");library("data.table")

# http://www.fao.org/faostat/en/#data/PP
data = read.csv("Food prices.csv", sep = ","  ,stringsAsFactors = FALSE)

# I take only the important varibles 

data = data[, c("Item", "Year", "Unit", "Value", "Flag", "Flag.Description")] 

# choose a number of the most important products based of the production quantity 
# Sweet potatoes Rice, paddy Potatoes Maize Cassava Bananas Beans, dry

data = data[data$Item %in% c("Cassava", "Bananas", "Beans, dry", "Maize", "Sweet potatoes", "Potatoes", "Rice, paddy"),]

### rain and temp data 

# temprature and rainfall data from 1991 - 2015
#source: http://sdwebx.worldbank.org/climateportal/index.cfm?page=downscaled_data_download&menu=historical
temp = read.csv("Rwanda_temp.csv", head = TRUE, sep = ";", stringsAsFactors = FALSE)
rain = read.csv("Rwanda_rainfall.csv", head = TRUE, sep = ";", stringsAsFactors = FALSE)

#replace commas with dots so we can convert to numeric
colnames(rain)[1] = "pr" 
colnames(temp)[1] = "tas" 
rain$pr = as.numeric(gsub(",", ".", gsub("\\.", "", rain$pr)))
temp$tas = as.numeric(gsub(",", ".", gsub("\\.", "", temp$tas)))

#prepare rain and temp data: mean amount of rain / mean temp for each quarter year
colnames(rain)[2] = "Year"
colnames(temp)[2] = "Year"
rain[, c("ISO3", "ISO2", "Country")] = NULL
temp[, c("ISO3", "ISO2", "Country")] = NULL
rain$pr_q1 = 0
rain$pr_q2 = 0
rain$pr_q3 = 0
rain$pr_q4 = 0
rain$q =0
rain[rain$Month == c(1,2,3),]$q = 1
rain[rain$Month == c(4,5,6),]$q = 2
rain[rain$Month == c(7,8,9),]$q = 3
rain[rain$Month == c(10,11,12),]$q = 4
temp$tas_q1 = 0
temp$tas_q2 = 0
temp$tas_q3 = 0
temp$tas_q4 = 0
temp$q =0
temp[temp$Month == c(1,2,3),]$q = 1
temp[temp$Month == c(4,5,6),]$q = 2
temp[temp$Month == c(7,8,9),]$q = 3
temp[temp$Month == c(10,11,12),]$q = 4

for(z in 1991:2015){
  rain[rain$Year == z ,]$pr_q1 = sum(rain[rain$Year == z & rain$Month == c(1,2,3),]$pr)/3
  rain[rain$Year == z ,]$pr_q2 = sum(rain[rain$Year == z & rain$Month == c(4,5,6),]$pr)/3
  rain[rain$Year == z ,]$pr_q3 = sum(rain[rain$Year == z & rain$Month == c(7,8,9),]$pr)/3
  rain[rain$Year == z ,]$pr_q4 = sum(rain[rain$Year == z & rain$Month == c(10,11,12),]$pr)/3
  
  temp[temp$Year == z ,]$tas_q1 = sum(temp[temp$Year == z & temp$Month == c(1,2,3),]$tas)/3
  temp[temp$Year == z ,]$tas_q2 = sum(temp[temp$Year == z & temp$Month == c(4,5,6),]$tas)/3
  temp[temp$Year == z ,]$tas_q3 = sum(temp[temp$Year == z & temp$Month == c(7,8,9),]$tas)/3
  temp[temp$Year == z ,]$tas_q4 = sum(temp[temp$Year == z & temp$Month == c(10,11,12),]$tas)/3
}


data = merge(data,unique(rain[c("pr_q1","pr_q2","pr_q3","pr_q4","Year")]),by=c("Year")) 
data = merge(data,unique(temp[c("tas_q1","tas_q2","tas_q3","tas_q4","Year")]),by=c("Year")) 

##################################################################################################

### read the oil prices data

# Source: https://www.statista.com/statistics/262858/change-in-opec-crude-oil-prices-since-1960/
oil_prices = read.csv("Oil prices .csv", head = TRUE, sep = ";", stringsAsFactors = FALSE)
colnames(oil_prices) = c("Year", "oil_avarage_price_per_barrel")
oil_prices$oil_avarage_price_per_barrel = as.numeric(gsub(",", ".", gsub("\\.", "", oil_prices$oil_avarage_price_per_barrel)))
data = merge(x = data, y = oil_prices, by= "Year", all.x = TRUE)

###################################################################################################

### read the Population data 

# Source: http://www.fao.org/faostat/en/#data/OA
population = read.csv("Population.csv", head = TRUE, sep = ",", stringsAsFactors = FALSE)
population = population[, c("Year", "Unit", "Value")]
colnames(population) = c("Year","Population_Unit","Population_Value")
data = merge(x = data, y = population, by= "Year", all.x = TRUE)

##################################################################################################

### Production Amount

# Source: http://www.fao.org/faostat/en/#data/OA
Production_amount = read.csv("Production Amount.csv", head = TRUE, sep = ",", stringsAsFactors = FALSE)
Production_amount = Production_amount[, c("Year", "Item", "Value")]
colnames(Production_amount)[3] = "Production_Amount"
data = merge(x = data, y = Production_amount, by= c("Year", "Item"), all.x = TRUE)


#################################################################################################
# same as Ben 
# GNI per capita, Atlas method (current US$)
# https://data.worldbank.org/indicator/NY.GNP.PCAP.KD?locations=RW
GNI = read.csv("GNI.csv", head = TRUE, sep = ",", stringsAsFactors = FALSE)
GNI = GNI[GNI$Country.Name == "Rwanda",]
GNI[1:35] = NULL
GNI[c("X2017", "X")] = NULL
colnames(GNI) = c("1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015")
GNI = as.data.frame(t(GNI))
setDT(GNI, keep.rownames = TRUE)[]

colnames(GNI) = c("Year", "GNI")
data = merge(x = data, y = GNI, by= "Year", all.x = TRUE)
######################################################################################################

# Exchange rate
# Source: http://www.fao.org/faostat/en/#data/OA

exchange_rate = read.csv("Exchange rate.csv", head = TRUE, sep = ",", stringsAsFactors = FALSE)
exchange_rate = exchange_rate[, c("Year", "Value")]
colnames(exchange_rate)[2] = "Exchange_Rate"
data = merge(x = data, y = exchange_rate, by= "Year", all.x = TRUE)

####################################################################################################
# GDP (current US$)
# https://data.worldbank.org/indicator/NY.GDP.MKTP.CD

GDP = read.csv("GDP.csv", head = TRUE, sep = ",", stringsAsFactors = FALSE)
GDP = GDP[GDP$Country.Name == "Rwanda",]
GDP[1:35] = NULL
GDP[c("X2016","X2017", "X")] = NULL
colnames(GDP) = c("1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015")
GDP = as.data.frame(t(GDP))
setDT(GDP, keep.rownames = TRUE)[]

colnames(GDP) = c("Year", "GDP")
data = merge(x = data, y = GDP, by= "Year", all.x = TRUE)

##################################################################################################
# same as Ben
# Inflation, GDP deflator (annual %)
# https://data.worldbank.org/indicator/FP.CPI.TOTL.ZG?locations=RW
Inflation = read.csv("Inflation.csv", head = TRUE, sep = ",", stringsAsFactors = FALSE)
Inflation = Inflation[Inflation$Country.Name == "Rwanda",]
Inflation[1:35] = NULL
Inflation[c("X2016","X2017", "X")] = NULL
colnames(Inflation) = c("1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015")
Inflation = as.data.frame(t(Inflation))
setDT(Inflation, keep.rownames = TRUE)[]

colnames(Inflation) = c("Year", "Inflation")
# the inflation data for the year 1994, 1995 where missing so we got them from another source: http://rwanda.opendataforafrica.org/rjirstd/cpi-by-country-statistics?country=Rwanda 
Inflation$Inflation[Inflation$Year == "1994"] = 21.0
Inflation$Inflation[Inflation$Year == "1995"] = 56.0
data = merge(x = data, y = Inflation, by= "Year", all.x = TRUE)

####################################################################################################
# same as Ben
# Agriculture GDP
# Agriculture, value added (constant 2010 US$)
# https://data.worldbank.org/indicator/NV.AGR.TOTL.KD?locations=RW

Agriculture_GDP = read.csv("Agriculture GDP.csv", head = TRUE, sep = ",", stringsAsFactors = FALSE)
Agriculture_GDP = Agriculture_GDP[Agriculture_GDP$Country.Name == "Rwanda",]
Agriculture_GDP[1:35] = NULL
Agriculture_GDP[c("X2016","X2017", "X")] = NULL
colnames(Agriculture_GDP) = c("1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015")
Agriculture_GDP = as.data.frame(t(Agriculture_GDP))
setDT(Agriculture_GDP, keep.rownames = TRUE)[]

colnames(Agriculture_GDP) = c("Year", "Agriculture_GDP")
data = merge(x = data, y = Agriculture_GDP, by= "Year", all.x = TRUE)

##################################################################################################

# reading the import and export data for vegetables and Cereals
# Source: http://rwanda.opendataforafrica.org/UNCTADMTMEIWCG2017/merchandise-trade-matrix-product-groups-exports-and-imports-in-thousands-of-dollars-annual-1995-2016
# thousand USD
Vegetables = read.csv("Vegetables.csv", head = TRUE, sep = ",", stringsAsFactors = FALSE)
Cereals = read.csv("Cereal.csv", head = TRUE, sep = ",", stringsAsFactors = FALSE)
colnames(Vegetables)[6] = "Year"
colnames(Cereals)[6] = "Year"
veg_import = Vegetables[Vegetables$flow == "Imports",c("Year", "Value")]
colnames(veg_import)[2] = "imp_veg"
veg_export = Vegetables[Vegetables$flow == "Exports",c("Year", "Value")]
colnames(veg_export)[2] = "exp_veg"
cer_import = Cereals[Cereals$flow == "Imports",c("Year", "Value")]
colnames(cer_import)[2] = "imp_cer"
cer_export = Cereals[Cereals$flow == "Exports",c("Year", "Value")]
colnames(cer_export)[2] = "exp_cer"
india = readRDS("india.rds")

data = merge(x = data, y = veg_export, by= "Year", all.x = TRUE)
data = merge(x = data, y = cer_export, by= "Year", all.x = TRUE)
data = merge(x = data, y = veg_import, by= "Year", all.x = TRUE)
data = merge(x = data, y = cer_import, by= "Year", all.x = TRUE)

# now we have the import and export data is empty between 1991 - 1994, we will take the mean of each of the fllowing five years 

data$exp_veg[data$Year == "1991"] = mean(unique(data$exp_veg[data$Year %in% c("1995", "1996", "1997", "1998", "1999")]))
data$imp_veg[data$Year == "1991"] = mean(unique(data$imp_veg[data$Year %in% c("1995", "1996", "1997", "1998", "1999")]))
data$imp_cer[data$Year == "1991"] = mean(unique(data$imp_cer[data$Year %in% c("1995", "1996", "1997", "1998", "1999")]))
data$exp_cer[data$Year == "1991"] = mean(unique(data$exp_cer[data$Year %in% c("1995", "1996", "1997", "1998", "1999")]))

data$exp_veg[data$Year == "1992"] = mean(unique(data$exp_veg[data$Year %in% c("1996", "1997", "1998", "1999", "2000")]))
data$imp_veg[data$Year == "1992"] = mean(unique(data$imp_veg[data$Year %in% c("1996", "1997", "1998", "1999", "2000")]))
data$imp_cer[data$Year == "1992"] = mean(unique(data$imp_cer[data$Year %in% c("1996", "1997", "1998", "1999", "2000")]))
data$exp_cer[data$Year == "1992"] = mean(unique(data$exp_cer[data$Year %in% c("1996", "1997", "1998", "1999", "2000")]))

data$exp_veg[data$Year == "1993"] = mean(unique(data$exp_veg[data$Year %in% c("1997", "1998", "1999", "2000", "2001")]))
data$imp_veg[data$Year == "1993"] = mean(unique(data$imp_veg[data$Year %in% c("1997", "1998", "1999", "2000", "2001")]))
data$imp_cer[data$Year == "1993"] = mean(unique(data$imp_cer[data$Year %in% c("1997", "1998", "1999", "2000", "2001")]))
data$exp_cer[data$Year == "1993"] = mean(unique(data$exp_cer[data$Year %in% c("1997", "1998", "1999", "2000", "2001")]))

data$exp_veg[data$Year == "1994"] = mean(unique(data$exp_veg[data$Year %in% c("1998", "1999", "2000", "2001", "2002")]))
data$imp_veg[data$Year == "1994"] = mean(unique(data$imp_veg[data$Year %in% c("1998", "1999", "2000", "2001", "2002")]))
data$imp_cer[data$Year == "1994"] = mean(unique(data$imp_cer[data$Year %in% c("1998", "1999", "2000", "2001", "2002")]))
data$exp_cer[data$Year == "1994"] = mean(unique(data$exp_cer[data$Year %in% c("1998", "1999", "2000", "2001", "2002")]))

###################################################################################################################################

# Average daily per capita caloric supply, measured in kilocalories per person per day. 
# Source: https://ourworldindata.org/food-per-person
dpccs = read.csv("daily-per-capita-caloric-supply.csv", head = TRUE, sep = ",", stringsAsFactors = FALSE)
dpccs = dpccs[dpccs$Entity == "Rwanda", c("Year","X.kcal.person.day.")]
colnames(dpccs)[2] = "daily_caloric_supply"
data = merge(x = data, y = dpccs, by= "Year", all.x = TRUE)

# 2014 and 2015 there is no data, replace with median 
data$daily_caloric_supply[data$Year == "2014"] = mean(unique(data$daily_caloric_supply[data$Year %in% c("2012", "2011", "2010", "2009", "2008")]))
data$daily_caloric_supply[data$Year == "2015"] = mean(unique(data$daily_caloric_supply[data$Year %in% c("2013" ,"2012", "2011", "2010", "2009")]))
                                                              

##################################################################################################################################

saveRDS(data, ".\\Processed_ds\\rwanda_fin.rds")

####################################################################################################################

# ploting products prices 
ggplot(data=data, aes(x=Year, y=Value, group=Item, colour=Item)) +
  geom_line() +
  geom_point()


################################################################################################################

# ploting a bar chart for each item form 2010 - 2015 

# value
if(!require("lattice")) install.packages("lattice");library("lattice")
barchart(data = data[data$Year %in% c(2010:2015), c("Year", "Item", "Value")], Value ~ Item, group = Year)

# Production Amount 
barchart(data = data[data$Year %in% c(2010:2015), c("Year", "Item", "Production_Amount")], Production_Amount ~ Item, group = Year)

#####################################################################################################

# calculating the percentage of chance in the prices
f = function(d, value){
  for(i in unique(d$Item)){
    for(j in unique(d$Year)){
      base = value[d$Item == i & d$Year == j]
      later = value[d$Item == i & d$Year == j+1]
      sub =  later - base
      d$perc[d$Item == i & d$Year == j+1]= (sub / base) * 100
    }
  }
  d$perc[d$Year == 1991] = 0
  return(d)
}
data$Year = as.factor(data$Year)
data = f(d = data, value = data$Value)

if(!require("plotly")) install.packages("plotly");library("plotly")

plotd = data[data$Year %in% c(2010:2015) ,c("Year", "Item", "perc")]


ggplot(plotd, aes(Item, perc)) +   
  geom_bar(aes(fill = as.factor(Year)), position = "dodge", stat="identity")

######################################################################################################

#food_price_index = read.csv("Food_price_indices_data.csv", stringsAsFactors = FALSE, sep = ",", header = TRUE)
######################################################################################################

world_population = read.csv("World Population.csv", stringsAsFactors = FALSE, sep = ",", header = TRUE)
world_population = world_population[world_population$?..Country.Name == "World",]

world_population[1:35] = NULL
world_population[c("X2016","X2017", "X")] = NULL
colnames(world_population) = c("1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015")
world_population = as.data.frame(t(world_population))
setDT(world_population, keep.rownames = TRUE)[]

colnames(world_population) = c("Year", "world_population")
world_population$Year <- as.numeric(world_population$Year)
k <- function(d){
  base = d$world_population[d$Year == 1991]
  for(i in d$Year){
    later = d$world_population[d$Year == i]
    sub =  later - base
    d$perc[d$Year == i]= (sub / base) * 100
  }
  return(d)
}
world_population = k(world_population)
colnames(world_population)[3] = "population_growth_precent"
data = merge(x = data, y = world_population, by= "Year", all.x = TRUE)



datapop <- unique(data[, c("Year", "Population_Value", "world_population")])


ggplot(datapop, aes(x = Year)) + 
  geom_line(aes(y = Population_Value), colour="blue") + 
  geom_line(aes(y = world_population), colour = "grey") + 
  ylab(label="Number of new members") + 
  xlab("Week")


f1 = function(d, r){
  for(i in d$Year){
      base = d$r[d$Year == i]
      later = d$r[d$Year == j+1]
      s =  later - base
      d$perc[d$Item == i & d$Year == j+1]= (s / base) * 100
  }
  d$r[d$Year == 1991] = 0
  return(d)
}

p_all <- read.csv("Production_Crops_E_All_Data.csv", stringsAsFactors = FALSE, sep = ",", header = TRUE)
p_all <- p_all[p_all$Area == "World",]  



#cleanup
rm(list = setdiff(ls(), lsf.str()))




