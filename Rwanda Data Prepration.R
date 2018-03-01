setwd("~/Desktop/SPL 2")
library(data.table)
if(!require("plyr")) install.packages("plyr"); library("plyr")
if(!require("Hmisc")) install.packages("Hmisc"); library("Hmisc")
if(!require("corrplot")) install.packages("corrplot"); library("corrplot")
if(!require("ggplot2")) install.packages("ggplot2");library("ggplot2")
if(!require("grid")) install.packages("grid");library("grid")
if(!require("gridExtra")) install.packages("gridExtra");library("gridExtra")
if(!require("data.table")) install.packages("data.table");library("data.table")

# http://www.fao.org/faostat/en/#data/PP
data <- read.csv("Food prices.csv", sep = ","  ,stringsAsFactors = FALSE)

# I take only the important varibles 

data <- data[, c("Item", "Year", "Unit", "Value", "Flag", "Flag.Description")] 

# choose a number of the most important products based of the production quantity 
# Sweet potatoes Rice, paddy Potatoes Maize Cassava Bananas Beans, dry

data <- data[data$Item %in% c("Cassava", "Bananas", "Beans, dry", "Maize", "Sweet potatoes", "Potatoes", "Rice, paddy"),]

### rain and temp data 

# temprature and rainfall data from 1991 - 2015
#source: http://sdwebx.worldbank.org/climateportal/index.cfm?page=downscaled_data_download&menu=historical
temp <- read.csv("Rwanda_temp.csv", head = TRUE, sep = ";", stringsAsFactors = FALSE)
rain <- read.csv("Rwanda_rainfall.csv", head = TRUE, sep = ";", stringsAsFactors = FALSE)

#replace commas with dots so we can convert to numeric
rain$pr = as.numeric(gsub(",", ".", gsub("\\.", "", rain$pr)))
temp$tas = as.numeric(gsub(",", ".", gsub("\\.", "", temp$tas)))

#prepare rain and temp data: mean amount of rain / mean temp for each quarter year
colnames(rain)[2] <- "Year"
colnames(temp)[2] <- "Year"
rain[, c("ISO3", "ISO2", "Country")] <- NULL
temp[, c("ISO3", "ISO2", "Country")] <- NULL
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
oil_prices <- read.csv("Oil prices .csv", head = TRUE, sep = ";", stringsAsFactors = FALSE)
colnames(oil_prices) <- c("Year", "oil_avarage_price_per_barrel")

data <- merge(x = data, y = oil_prices, by= "Year", all.x = TRUE)

###################################################################################################

### read the Population data 

# Source: http://www.fao.org/faostat/en/#data/OA
population <- read.csv("Population.csv", head = TRUE, sep = ",", stringsAsFactors = FALSE)
population <- population[, c("Year", "Unit", "Value")]
colnames(population) <- c("Year","Population Unit","Population Value")
data <- merge(x = data, y = population, by= "Year", all.x = TRUE)

##################################################################################################

### Production Amount

# Source: http://www.fao.org/faostat/en/#data/OA
Production_amount <- read.csv("Production Amount.csv", head = TRUE, sep = ",", stringsAsFactors = FALSE)
Production_amount <- Production_amount[, c("Year", "Item", "Value")]
colnames(Production_amount)[3] <- "Production Amount"
data <- merge(x = data, y = Production_amount, by= c("Year", "Item"), all.x = TRUE)

###################################################################################################

### Food supply kcal/capita/day 

# Source: http://www.fao.org/faostat/en/#data/OA
food_supply <- read.csv("Food supply.csv", head = TRUE, sep = ",", stringsAsFactors = FALSE)
food_supply$Item[food_supply$Item == "Beans"] <- "Beans, dry"
food_supply$Item[food_supply$Item == "Cassava and products"] <- "Cassava"
food_supply$Item[food_supply$Item == "Maize and products"] <- "Maize"
food_supply$Item[food_supply$Item == "Potatoes and products"] <- "Potatoes"
food_supply$Item[food_supply$Item == "Rice (Milled Equivalent)"] <- "Rice, paddy"

food_supply <- food_supply[, c("Year", "Item", "Value")]
colnames(food_supply)[3] <- "Food Supply"
data <- merge(x = data, y = food_supply, by= c("Year", "Item"), all.x = TRUE)

# we have data only until 2013. for 2014 and 2015 we have NA values, we replace them with the mean for each respected product 
mean_food_supply <- vector(mode = "double")
for(i in unique(food_supply$Item)){
  mean_food_supply[i] <- mean(food_supply$`Food Supply`[food_supply$Item == i])
}

data$`Food Supply`[is.na(data$`Food Supply`) == TRUE & data$Item == "Rice, paddy"] <- mean_food_supply["Rice, paddy"]
data$`Food Supply`[is.na(data$`Food Supply`) == TRUE & data$Item == "Maize"] <- mean_food_supply["Maize"]
data$`Food Supply`[is.na(data$`Food Supply`) == TRUE & data$Item == "Cassava"] <- mean_food_supply["Cassava"]
data$`Food Supply`[is.na(data$`Food Supply`) == TRUE & data$Item == "Potatoes"] <- mean_food_supply["Potatoes"]
data$`Food Supply`[is.na(data$`Food Supply`) == TRUE & data$Item == "Sweet potatoes"] <- mean_food_supply["Sweet potatoes"]
data$`Food Supply`[is.na(data$`Food Supply`) == TRUE & data$Item == "Beans, dry"] <- mean_food_supply["Beans, dry"]
data$`Food Supply`[is.na(data$`Food Supply`) == TRUE & data$Item == "Bananas"] <- mean_food_supply["Bananas"]

#################################################################################################

# GNI per capita, Atlas method (current US$)
# https://data.worldbank.org/indicator/NY.GNP.PCAP.CD
GNI <- read.csv("GNI.csv", head = TRUE, sep = ",", stringsAsFactors = FALSE)
GNI <- GNI[GNI$Country.Name == "Rwanda",]
GNI[1:35] <- NULL
GNI[c("X2017", "X")] <- NULL
colnames(GNI) <- c("1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015")
GNI <- as.data.frame(t(GNI))
setDT(GNI, keep.rownames = TRUE)[]

colnames(GNI) <- c("Year", "GNI")
data <- merge(x = data, y = GNI, by= "Year", all.x = TRUE)
######################################################################################################

# Exchange rate
# Source: http://www.fao.org/faostat/en/#data/OA

exchange_rate <- read.csv("Exchange rate.csv", head = TRUE, sep = ",", stringsAsFactors = FALSE)
exchange_rate <- exchange_rate[, c("Year", "Value")]
colnames(exchange_rate)[2] <- "Exchange Rate"
data <- merge(x = data, y = exchange_rate, by= "Year", all.x = TRUE)

####################################################################################################
# GDP (current US$)
# https://data.worldbank.org/indicator/NY.GDP.MKTP.CD

GDP <- read.csv("GDP.csv", head = TRUE, sep = ",", stringsAsFactors = FALSE)
GDP <- GDP[GDP$Country.Name == "Rwanda",]
GDP[1:35] <- NULL
GDP[c("X2016","X2017", "X")] <- NULL
colnames(GDP) <- c("1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015")
GDP <- as.data.frame(t(GDP))
setDT(GDP, keep.rownames = TRUE)[]

colnames(GDP) <- c("Year", "GDP")
data <- merge(x = data, y = GDP, by= "Year", all.x = TRUE)

##################################################################################################
# Inflation, GDP deflator (annual %)
# https://data.worldbank.org/indicator/NY.GDP.DEFL.KD.ZG
Inflation <- read.csv("Inflation.csv", head = TRUE, sep = ",", stringsAsFactors = FALSE)
Inflation <- Inflation[Inflation$Country.Name == "Rwanda",]
Inflation[1:35] <- NULL
Inflation[c("X2016","X2017", "X")] <- NULL
colnames(Inflation) <- c("1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015")
Inflation <- as.data.frame(t(Inflation))
setDT(Inflation, keep.rownames = TRUE)[]

colnames(Inflation) <- c("Year", "Inflation")
data <- merge(x = data, y = Inflation, by= "Year", all.x = TRUE)

####################################################################################################
# Agriculture GDP
# Agriculture, value added (% of GDP)
# https://data.worldbank.org/indicator/NV.AGR.TOTL.ZS

# Agriculture_GDP <- read.csv("Agriculture GDP.csv", head = TRUE, sep = ",", stringsAsFactors = FALSE)
# Agriculture_GDP <- Agriculture_GDP[Agriculture_GDP$Country.Name == "Rwanda",]
# Agriculture_GDP[1:35] <- NULL
# Agriculture_GDP[c("X2016","X2017", "X")] <- NULL
# colnames(Agriculture_GDP) <- c("1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015")
# Agriculture_GDP <- as.data.frame(t(Agriculture_GDP))
# setDT(Agriculture_GDP, keep.rownames = TRUE)[]
# 
# colnames(Agriculture_GDP) <- c("Year", "Agriculture_GDP")
# data <- merge(x = data, y = Agriculture_GDP, by= "Year", all.x = TRUE)

saveRDS(data, ("Rwanda.rds"))

