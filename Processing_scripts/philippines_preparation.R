

library(data.table)
if(!require("plyr")) install.packages("plyr"); library("plyr")
if(!require("Hmisc")) install.packages("Hmisc"); library("Hmisc")
if(!require("corrplot")) install.packages("corrplot"); library("corrplot")
if(!require("ggplot2")) install.packages("ggplot2");library("ggplot2")
if(!require("grid")) install.packages("grid");library("grid")
if(!require("gridExtra")) install.packages("gridExtra");library("gridExtra")
if(!require("data.table")) install.packages("data.table");library("data.table")

source("~/sps_ws1718/Helper_functions/preparation_functions.R")



# http://www.fao.org/faostat/en/#data/PP
data <- read.csv("FoodPrices.csv", sep = ","  ,stringsAsFactors = FALSE)
# temprature and rainfall data from 1991 - 2015
#source: http://sdwebx.worldbank.org/climateportal/index.cfm?page=downscaled_data_download&menu=historical
rain <- read.csv(file="Philipines_rain.csv",head=TRUE,sep=",")
temp <- read.csv(file="Philipines_temp.csv",head=TRUE,sep=",")
# Source: https://www.statista.com/statistics/262858/change-in-opec-crude-oil-prices-since-1960/
oil_prices <- read.csv("OilPrices.csv", head = TRUE, sep = ";", stringsAsFactors = FALSE)
# Source: http://www.fao.org/faostat/en/#data/OA
population <- read.csv("Population.csv", head = TRUE, sep = ",", stringsAsFactors = FALSE)
# Source: http://www.fao.org/faostat/en/#data/OA
Production_amount <- read.csv("ProductionAmount.csv", head = TRUE, sep = ",", stringsAsFactors = FALSE)
# https://data.worldbank.org/indicator/NY.GNP.PCAP.KD?locations=RW
GNI <- read.csv("GNI.csv", head = TRUE, sep = ",", stringsAsFactors = FALSE)
# Source: http://www.fao.org/faostat/en/#data/OA
exchange_rate <- read.csv("ExchangeRate.csv", head = TRUE, sep = ",", stringsAsFactors = FALSE)
# https://data.worldbank.org/indicator/NY.GDP.MKTP.CD
GDP <- read.csv("GDP.csv", head = TRUE, sep = ",", stringsAsFactors = FALSE)
# https://data.worldbank.org/indicator/FP.CPI.TOTL.ZG?locations=RW
Inflation <- read.csv("Inflation.csv", head = TRUE, sep = ",", stringsAsFactors = FALSE)
# https://data.worldbank.org/indicator/NV.AGR.TOTL.KD?locations=RW
Agriculture_GDP <- read.csv("AgricultureGDP.csv", head = TRUE, sep = ",", stringsAsFactors = FALSE)
#https://psa.gov.ph/nap-press-release/data-charts
importData <- read.csv("imports.csv", head = TRUE, sep = ",", stringsAsFactors = FALSE)
#https://psa.gov.ph/nap-press-release/data-charts
exportData <- read.csv("exports.csv", head = TRUE, sep = ",", stringsAsFactors = FALSE)
# Source: https://ourworldindata.org/food-per-person
dpccs <- read.csv("daily-per-capita-supply-of-calories.csv", head = TRUE, sep = ",", stringsAsFactors = FALSE)


# I take only the important variables 
data <- data[, c("Item", "Year", "Unit", "Value", "Flag", "Flag.Description")]

# choose a number of the most important products based of the production quantity 
# Sweet potatoes Rice, paddy Potatoes Maize Cassava Bananas Beans, dry
data <- data[data$Item %in% c("Sugar Cane", "Bananas", "Coconuts", "Rice_paddy"),]

### rain and temp data 
#replace commas with dots where necessary so we can convert to numeric
rain$pr = as.numeric(gsub(",", ".", gsub("\\.", "", rain$pr)))
temp$tas = as.numeric(gsub(",", ".", gsub("\\.", "", temp$tas)))

rain$ISO3 = NULL
rain$ISO2 = NULL
rain$year = rain$X.Year
rain$X.Year = NULL
rain$month = rain$Month
rain$Month = NULL

temp$year = temp$X.Year
temp$X.Year = NULL
temp$month = temp$Month
temp$Month = NULL

raintemp = merge(rain,temp[c("tas","Year","month")],by=c("month","Year"))

#calling the helper function 
raintemp = avgRainTempQuarter(raintemp,"month","Year","pr","tas")


data = merge(data,unique(raintemp[c("tas_q1","tas_q2","tas_q3","tas_q4","pr_q1","pr_q2","pr_q3","pr_q4","Year")]),by=c("Year")) 

##################################################################################################

### read the oil prices data

colnames(oil_prices) <- c("Year", "oil_avarage_price_per_barrel")
#replace commas with dots where necessary so we can convert to numeric
oil_prices$oil_avarage_price_per_barrel = as.numeric(gsub(",", ".", gsub("\\.", "", oil_prices$oil_avarage_price_per_barrel)))

data <- merge(x = data, y = oil_prices, by= "Year", all.x = TRUE)

###################################################################################################
### read the Population data 
population <- population[, c("Year", "Unit", "Value")]
population$Unit <- 1000
colnames(population) <- c("Year","PopulationUnit","PopulationValue")
data <- merge(x = data, y = population, by= "Year", all.x = TRUE)

##################################################################################################
### Production Amount
Production_amount <- Production_amount[, c("Year", "Item", "Value")]
colnames(Production_amount)[3] <- "ProductionAmount"

#Production_amount$ProductionAmount = as.numeric(levels(data$Production_amount))[data$Production_amount]

data <- merge(x = data, y = Production_amount, by= c("Year", "Item"), all.x = TRUE)

#################################################################################################
# GNI per capita, Atlas method (current US$)
GNI <- GNI[GNI$Country.Name == "Philippines",]
GNI[1:35] <- NULL
GNI[c("X2017", "X")] <- NULL
colnames(GNI) <- c("1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015")
GNI <- as.data.frame(t(GNI))
setDT(GNI, keep.rownames = TRUE)[]

colnames(GNI) <- c("Year", "GNI")

#GNI$GNI = as.numeric(gsub(",", ".", gsub("\\.", "", GNI$GNI)))
#GNI$Year = as.numeric(gsub(",", ".", gsub("\\.", "", GNI$Year)))


data <- merge(x = data, y = GNI, by= "Year", all.x = TRUE)

######################################################################################################
# Exchange rate
exchange_rate <- exchange_rate[, c("Year", "Value")]
colnames(exchange_rate)[2] <- "ExchangeRate"
data <- merge(x = data, y = exchange_rate, by= "Year", all.x = TRUE)

####################################################################################################
# GDP (current US$)
GDP <- GDP[GDP$Country.Name == "Philippines",]
GDP[1:35] <- NULL
GDP[c("X2016","X2017", "X")] <- NULL
colnames(GDP) <- c("1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015")
GDP <- as.data.frame(t(GDP))
setDT(GDP, keep.rownames = TRUE)[]

colnames(GDP) <- c("Year", "GDP")
data <- merge(x = data, y = GDP, by= "Year", all.x = TRUE)

##################################################################################################
# Inflation, GDP deflator (annual %)
Inflation <- Inflation[Inflation$Country.Name == "Philippines",]
Inflation[1:35] <- NULL
Inflation[c("X2016","X2017", "X")] <- NULL
colnames(Inflation) <- c("1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015")
Inflation <- as.data.frame(t(Inflation))
setDT(Inflation, keep.rownames = TRUE)[]

colnames(Inflation) <- c("Year", "Inflation")

#replace commas with dots where necessary so we can convert to numeric
#Inflation$Inflation = as.numeric(gsub(",", ".", gsub("\\.", "", Inflation$Inflation)))

data <- merge(x = data, y = Inflation, by= "Year", all.x = TRUE)

####################################################################################################
# Agriculture GDP
# Agriculture, value added (constant 2010 US$)
Agriculture_GDP <- Agriculture_GDP[Agriculture_GDP$Country.Name == "Philippines",]
Agriculture_GDP[1:35] <- NULL
Agriculture_GDP[c("X2016","X2017", "X")] <- NULL
colnames(Agriculture_GDP) <- c("1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015")
Agriculture_GDP <- as.data.frame(t(Agriculture_GDP))
setDT(Agriculture_GDP, keep.rownames = TRUE)[]

colnames(Agriculture_GDP) <- c("Year", "Agriculture_GDP")

#replace commas with dots where necessary so we can convert to numeric
#Agriculture_GDP$Agriculture_GDP = as.numeric(gsub(",", ".", gsub("\\.", "", Agriculture_GDP$Agriculture_GDP)))

data <- merge(x = data, y = Agriculture_GDP, by= "Year", all.x = TRUE)

####################################################################################################
#Import of Agricultrual Products 
#-- cereal 
importData <- importData[importData$ITEM == "Cereals",]
importData[,c("X2016","ITEM")] <- NULL
colnames(importData) <- c("1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015")
importData <- as.data.frame(t(importData))
setDT(importData, keep.rownames = TRUE)[]

colnames(importData) <- c("Year", "Import")

#replace commas with dots where necessary so we can convert to numeric
importData$Import = as.numeric(gsub(",", ".", gsub("\\.", "", importData$Import)))

data <- merge(x = data, y = importData, by= "Year", all.x = TRUE)

####################################################################################################
#Export of Agricultural Products 
#Bananas - Coconut Oil - Copra Oil, Coconut - Mango - Pineapple - Sugar
exportData <- exportData[exportData$ITEM == "AgriculturalProducts",]
exportData[,c("X2016","ITEMS")] <- NULL
colnames(exportData) <- c("1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015")
exportData <- as.data.frame(t(exportData))
setDT(exportData, keep.rownames = TRUE)[]

colnames(exportData) <- c("Year", "Export")

#replace commas with dots where necessary so we can convert to numeric
exportData$Export = as.numeric(gsub(",", ".", gsub("\\.", "", exportData$Export)))

data <- merge(x = data, y = exportData, by= "Year", all.x = TRUE)

####################################################################################################
# Average daily per capita caloric supply, measured in kilocalories per person per day. 

dpccs <- dpccs[dpccs$Entity == "Philippines", c("Year","Daily.caloric.supply..FAO..2017....kcal.person.day.")]
colnames(dpccs)[2] <- "daily_caloric_supply"
data <- merge(x = data, y = dpccs, by= "Year", all.x = TRUE)

# 2014 and 2015 there is no data, replace with median 
data$daily_caloric_supply[data$Year == "2014"] <- mean(unique(data$daily_caloric_supply[data$Year %in% c("2012", "2011", "2010", "2009", "2008")]))
data$daily_caloric_supply[data$Year == "2015"] <- mean(unique(data$daily_caloric_supply[data$Year %in% c("2013" ,"2012", "2011", "2010", "2009")]))

#removing rows with NAs in any column
data1 <- data[complete.cases(data),]

#renameing colums for final use
colnames(data1)[colnames(data1) %in% "Value"] = "prod_price"
colnames(data1)[colnames(data1) %in% "ProductionAmount" ] = "prod_amount"
colnames(data1)[colnames(data1) %in% "Year" ] = "year"
colnames(data1)[colnames(data1) %in% "Item" ] = "prod_name"
colnames(data1)[colnames(data1) %in% "PopulationValue" ] = "population"
colnames(data1)[colnames(data1) %in% "oil_avarage_price_per_barrel" ] = "avg_p_barrel"
colnames(data1)[colnames(data1) %in% "GNI" ] = "gni_pc"
colnames(data1)[colnames(data1) %in% "GDP" ] = "gdp"
colnames(data1)[colnames(data1) %in% "Inflation" ] = "cp_inflation"
colnames(data1)[colnames(data1) %in% "Agriculture_GDP" ] = "agri_gdp"
colnames(data1)[colnames(data1) %in% "Import" ] = "imp_cer"
colnames(data1)[colnames(data1) %in% "Export" ] = "exp_agri"
colnames(data1)[colnames(data1) %in% "ExchangeRate" ] = "exchange_rate"
colnames(data1)[colnames(data1) %in% "Flag" ] = "flag"
colnames(data1)[colnames(data1) %in% "Flag.Description" ] = "flag.description"
colnames(data1)[colnames(data1) %in% "PopulationUnit" ] = "population_unit"
colnames(data1)[colnames(data1) %in% "Unit" ] = "unit"


colnames(data1)

#save our dataset for later
saveRDS(data1, ("~/sps_ws1718/Processed_ds/philippines_fin.rds"))




#cleanup
rm(list = setdiff(ls(), lsf.str()))

