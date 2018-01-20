setwd("C:/Users/Raiber/Desktop/SPL")
if(!require("plyr")) install.packages("plyr"); library("plyr")
if(!require("Hmisc")) install.packages("Hmisc"); library("Hmisc")
if(!require("corrplot")) install.packages("corrplot"); library("corrplot")

data <- read.csv("wfp_market_food_prices.csv", head = TRUE, sep = ",", stringsAsFactors = FALSE)
# Rwanda data is 115037 in the time period 2008 - 2015
RwandaData <- data[data$adm0_name == "Rwanda",]

### Rwanda Data

# we are not interested in adm0_id and adm0_name (they represent Rwanda id and Rwanda name)
# we have five adm1_id and adm1_name (Provinces)
unique(RwandaData[,c("adm1_id", "adm1_name")])

RwandaData$adm1_name[RwandaData$adm1_name == "$West/Iburengerazuba"] <- "Western"
RwandaData$adm1_name[RwandaData$adm1_name == "$North/Amajyaruguru"] <- "Northern"
RwandaData$adm1_name[RwandaData$adm1_name == "$South/Amajyepfo"] <- "Southern"
RwandaData$adm1_name[RwandaData$adm1_name == "$East/Iburasirazuba"] <- "Eastern"
RwandaData$adm1_name[RwandaData$adm1_name == "Kigali City/Umujyi wa Kigali"] <- "Kigali"
# adm1_id  adm1_name
# 21973    Western
# 21971    Northern
# 21972    Southern
# 2587     Kigali
# 21969    Eastern

# we have 63 mkt_id and mkt_name (Cities)
count(RwandaData[,c("mkt_id", "mkt_name")])

# we have 55 cm_id and cm_name (products/ commodities)
count(RwandaData[,c("cm_id", "cm_name")])

# we have only one (pt_id and pt_name) market type (Retail) [market types (Retail/Wholesale/Producer/Farm Gate)]
unique(RwandaData[,c("pt_id", "pt_name")])

# we have one currency cur_id and cur_name which is RWF(Rwandan Franc)
unique(RwandaData[,c("cur_id", "cur_name")])
datasetRwanda <- merge(RwandaData[, c()])

# we have 4 unit of goods measurement um_id and um_name  (KG, L, Unit, Sack)
unique(RwandaData[,c("um_id", "um_name")])

# as mentioned before the time fame is 2008 - 2015 monthly basis 
unique(RwandaData[,c("mp_month", "mp_year")])

# mp_commoditysource Source supplying price information is only MINAGRI (Ministry of Agriculture of Peru)
unique(RwandaData["mp_commoditysource"]) 

finaldata <- RwandaData[,c("adm1_name", "mkt_name", "cm_name", "um_name", "mp_month", "mp_year", "mp_price")]


### rain and temp data 

# temprature and rainfall data from 1991 - 2015
Rwanda_temp <- read.csv("Rwanda_temp.csv", head = TRUE, sep = ";", stringsAsFactors = FALSE)
Rwanda_rainfall <- read.csv("Rwanda_rainfall.csv", head = TRUE, sep = ";", stringsAsFactors = FALSE)

df<-merge(x=Rwanda_temp,y=Rwanda_rainfall,by=c("Month", "X.Year"),all.x=TRUE)
df[, c("ISO3.x", "ISO2.x", "ISO3.y", "ISO2.y", "Country.x", "Country.y")] <- NULL

colnames(df) <- c("mp_month", "mp_year", "temp", "rainfall") 

# assign the Season column to the data
for (i in 1:nrow(df)){
  if (df[i, "mp_month"] %in% c(3,4,5)) {
    df[i, "season"] <- "spring"
  } else if (df[i, "mp_month"] %in% c(6,7,8)){
    df[i, "season"] <- "Summer"
  } else if (df[i, "mp_month"] %in% c(9,10,11)){
    df[i, "season"] <- "Fall"
  } else if (df[i, "mp_month"] %in% c(12,1,2)){
    df[i, "season"] <- "Winter"
  } 
}

finaldata <- merge(x=finaldata,y=df,by=c("mp_month", "mp_year"),all.x=TRUE)












