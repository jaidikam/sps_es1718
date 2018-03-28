source(".\\Helper_functions\\preparation_functions.R")

if(!require("plyr")) install.packages("plyr"); library("plyr")
if(!require("Hmisc")) install.packages("Hmisc"); library("Hmisc")
if(!require("corrplot")) install.packages("corrplot"); library("corrplot")
if(!require("ggplot2")) install.packages("ggplot2");library("ggplot2")
if(!require("grid")) install.packages("grid");library("grid")
if(!require("gridExtra")) install.packages("gridExtra");library("gridExtra")
if(!require("data.table")) install.packages("data.table");library("data.table")


# http://www.fao.org/faostat/en/#data/PP
data = read.csv(".\\Rwanda datasets\\Food_prices.csv", sep = ","  ,stringsAsFactors = FALSE)
# temprature and rainfall data from 1991 - 2015
#source: http://sdwebx.worldbank.org/climateportal/index.cfm?page=downscaled_data_download&menu=historical
temp = read.csv(".\\Rwanda datasets\\Rwanda_temp.csv", head = TRUE, sep = ";", stringsAsFactors = FALSE)
rain = read.csv(".\\Rwanda datasets\\Rwanda_rainfall.csv", head = TRUE, sep = ";", stringsAsFactors = FALSE)
# Source: https://www.statista.com/statistics/262858/change-in-opec-crude-oil-prices-since-1960/
oil_prices = read.csv(".\\Rwanda datasets\\Oil_prices.csv", head = TRUE, sep = ";", stringsAsFactors = FALSE)
# Source: http://www.fao.org/faostat/en/#data/OA
population = read.csv(".\\Rwanda datasets\\Population.csv", head = TRUE, sep = ",", stringsAsFactors = FALSE)
# Source: http://www.fao.org/faostat/en/#data/OA
Production_amount = read.csv(".\\Rwanda datasets\\Production_Amount.csv", head = TRUE, sep = ",", stringsAsFactors = FALSE)
# https://data.worldbank.org/indicator/NY.GNP.PCAP.KD?locations=RW
GNI = read.csv(".\\Rwanda datasets\\GNI.csv", head = TRUE, sep = ",", stringsAsFactors = FALSE)
# Source: http://www.fao.org/faostat/en/#data/OA
exchange_rate = read.csv(".\\Rwanda datasets\\Exchange_rate.csv", head = TRUE, sep = ",", stringsAsFactors = FALSE)
# https://data.worldbank.org/indicator/NY.GDP.MKTP.CD
GDP = read.csv(".\\Rwanda datasets\\GDP.csv", head = TRUE, sep = ",", stringsAsFactors = FALSE)
# https://data.worldbank.org/indicator/FP.CPI.TOTL.ZG?locations=RW
Inflation = read.csv(".\\Rwanda datasets\\Inflation.csv", head = TRUE, sep = ",", stringsAsFactors = FALSE)
# https://data.worldbank.org/indicator/NV.AGR.TOTL.KD?locations=RW
Agriculture_GDP = read.csv(".\\Rwanda datasets\\Agriculture_GDP.csv", head = TRUE, sep = ",", stringsAsFactors = FALSE)
# Source: http://rwanda.opendataforafrica.org/UNCTADMTMEIWCG2017/merchandise-trade-matrix-product-groups-exports-and-imports-in-thousands-of-dollars-annual-1995-2016
Vegetables = read.csv(".\\Rwanda datasets\\Vegetables.csv", head = TRUE, sep = ",", stringsAsFactors = FALSE)
Cereals = read.csv(".\\Rwanda datasets\\Cereal.csv", head = TRUE, sep = ",", stringsAsFactors = FALSE)
# Source: https://ourworldindata.org/food-per-person
dpccs = read.csv(".\\Rwanda datasets\\daily-per-capita-caloric-supply.csv", head = TRUE, sep = ",", stringsAsFactors = FALSE)



# I take only the important varibles 

data = data[, c("Item", "Year", "Unit", "Value", "Flag", "Flag.Description")] 

# choose a number of the most important products based of the production quantity 
# Sweet potatoes Rice, paddy Potatoes Maize Cassava Bananas Beans, dry

data = data[data$Item %in% c("Cassava", "Bananas", "Beans, dry", "Maize", "Sweet potatoes", "Potatoes", "Rice, paddy"),]

### rain and temp data 

rain$pr = rain$ï..pr
rain$Year = rain$X.Year
rain$month = rain$Month
rain[, c("ISO3", "ISO2", "X.Year", "Month", "ï..pr")] = NULL

temp$Year = temp$X.Year
temp$tas = temp$ï..tas
temp$month = temp$Month
temp[, c("X.Year", "ï..tas", "Month")] = NULL

#replace commas with dots where necessary so we can convert to numeric
rain$pr = as.numeric(gsub(",", ".", gsub("\\.", "", rain$pr)))
temp$tas = as.numeric(gsub(",", ".", gsub("\\.", "", temp$tas)))
raintemp = merge(rain,temp[c("tas","Year","month")],by=c("month","Year"))

# calling the function 
raintemp = avgRainTempQuarter(raintemp,"month","Year","pr","tas")

data = merge(data,unique(raintemp[c("tas_q1","tas_q2","tas_q3","tas_q4","pr_q1","pr_q2","pr_q3","pr_q4","Year")]),by=c("Year"))


##################################################################################################

### read the oil prices data


colnames(oil_prices) = c("Year", "oil_avarage_price_per_barrel")
oil_prices$oil_avarage_price_per_barrel = as.numeric(gsub(",", ".", gsub("\\.", "", oil_prices$oil_avarage_price_per_barrel)))
data = merge(x = data, y = oil_prices, by= "Year", all.x = TRUE)

###################################################################################################

### Population data 

population = population[, c("Year", "Unit", "Value")]
colnames(population) = c("Year","Population_Unit","Population_Value")
data = merge(x = data, y = population, by= "Year", all.x = TRUE)

##################################################################################################

### Production Amount

Production_amount = Production_amount[, c("Year", "Item", "Value")]
colnames(Production_amount)[3] = "Production_Amount"
data = merge(x = data, y = Production_amount, by= c("Year", "Item"), all.x = TRUE)


#################################################################################################
# GNI per capita, Atlas method (current US$)

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
exchange_rate = exchange_rate[, c("Year", "Value")]
colnames(exchange_rate)[2] = "Exchange_Rate"
data = merge(x = data, y = exchange_rate, by= "Year", all.x = TRUE)

####################################################################################################

# GDP (current US$)
GDP = GDP[GDP$Country.Name == "Rwanda",]
GDP[1:35] = NULL
GDP[c("X2016","X2017", "X")] = NULL
colnames(GDP) = c("1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015")
GDP = as.data.frame(t(GDP))
setDT(GDP, keep.rownames = TRUE)[]

colnames(GDP) = c("Year", "GDP")
data = merge(x = data, y = GDP, by= "Year", all.x = TRUE)

##################################################################################################

# Inflation, GDP deflator (annual %)
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

# Agriculture GDP
# Agriculture, value added (constant 2010 US$)
Agriculture_GDP = Agriculture_GDP[Agriculture_GDP$Country.Name == "Rwanda",]
Agriculture_GDP[1:35] = NULL
Agriculture_GDP[c("X2016","X2017", "X")] = NULL
colnames(Agriculture_GDP) = c("1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015")
Agriculture_GDP = as.data.frame(t(Agriculture_GDP))
setDT(Agriculture_GDP, keep.rownames = TRUE)[]

colnames(Agriculture_GDP) = c("Year", "Agriculture_GDP")
data = merge(x = data, y = Agriculture_GDP, by= "Year", all.x = TRUE)

##################################################################################################

# import and export data for vegetables and Cereals
# thousand USD
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
dpccs = dpccs[dpccs$Entity == "Rwanda", c("Year","X.kcal.person.day.")]
colnames(dpccs)[2] = "daily_caloric_supply"
data = merge(x = data, y = dpccs, by= "Year", all.x = TRUE)

# 2014 and 2015 there is no data, replace with median 
data$daily_caloric_supply[data$Year == "2014"] = mean(unique(data$daily_caloric_supply[data$Year %in% c("2012", "2011", "2010", "2009", "2008")]))
data$daily_caloric_supply[data$Year == "2015"] = mean(unique(data$daily_caloric_supply[data$Year %in% c("2013" ,"2012", "2011", "2010", "2009")]))
                                                              

##################################################################################################################################

# matching the data with the other datas 
colnames(data) = c("year", "prod_name", "unit","prod_price", "flag", "flag.description", "tas_q1", "tas_q2", "tas_q3", "tas_q4", "pr_q1", "pr_q2", "pr_q3", "pr_q4", "avg_p_barrel","population_unit", "population", "prod_amount", "gni_pc", "exchange_rate", "gdp", "cp_inflation", "agri_gdp", "exp_veg", "exp_cer", "imp_veg", "imp_cer", "daily_caloric_supply")
data$prod_price = as.numeric(data$prod_price)
# saving the data
saveRDS(data, ".\\Processed_ds\\rwanda_fin.rds")



