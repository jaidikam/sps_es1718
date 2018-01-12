if(!require("Hmisc")) install.packages("Hmisc"); library("Hmisc")
if(!require("corrplot")) install.packages("corrplot"); library("corrplot")

data <- read.csv(file=".\\wfp_market_food_prices.csv",head=TRUE,sep=",")
rain <- read.csv(file=".\\rain_india.csv",head=TRUE,sep=";")
temp <- read.csv(file=".\\temp_india.csv",head=TRUE,sep=";")

unique(data[data$adm0_name == 'Afghanistan',]$cm_name)
unique(data[data$adm0_name == 'India',]$cm_name)
unique(data[data$adm0_name == 'Nigeria',]$cm_name)
unique(data$adm0_name)



#https://www.mapsofindia.com/top-ten/india-crops/Wheat
#choose most important food
indiafoods = c("Rice","Wheat","Milk (pasteurized)","Lentils")
india = data[data$adm0_name == 'India' & data$mp_year >= 2000,]
india = india[india$cm_name %in% indiafoods, ]
india$avg_price_prod_month = 0

#calculate average price per food per month for the whole country

for (k in min(india$mp_year):max(india$mp_year)){
  print(paste('year is ',k))
for (j in 1:NROW(indiafoods)){
a <- india[india$mp_year == k & india$cm_name == indiafoods[j],]
print(paste('food is ',indiafoods[j]))
for (i in 1:12){
  b <- a[a$mp_month == i,]
  if (NROW(india[india$mp_year == k & india$cm_name == indiafoods[j] & india$mp_month == i,]$avg_price_prod_month) >0) {
    india[india$mp_year == k & india$cm_name == indiafoods[j] & india$mp_month == i,]$avg_price_prod_month =  sum(b$mp_price)/ NROW(b)
    print(paste('month is ',i))  
    }
  
   }
  }
}

india$adm0_id = NULL
india$adm1_id = NULL
india$adm1_name = NULL
india$mkt_id = NULL
india$mkt_name = NULL
india$cm_id = NULL
india$cur_id = NULL
india$cur_name = NULL
india$pt_id = NULL
india$pt_name = NULL
india$mp_commoditysource = NULL
india$mp_price = NULL
india = unique(india)
india$month = india$mp_month
india$mp_month = NULL
india$year = india$mp_year
india$mp_year = NULL

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

india <- merge(india,rain,by=c("month","year")) 
india <- merge(india,temp,by=c("month","year")) 
india$Country.x = NULL
india$Country.y =NULL

#replace commas with dots so we can convert to numeric
india$pr = as.numeric(gsub(",", ".", gsub("\\.", "", india$pr)))
india$tas = as.numeric(gsub(",", ".", gsub("\\.", "", india$tas)))

#save our dataset for later
saveRDS(india, (".\\india.rds"))

foo = cor(india[,7:9], method = "pearson", use = "complete.obs")
boo = rcorr(as.matrix(india[,7:9]))
boo$P
corrplot(foo, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

#weak correlation between price and rain, but not significant -> pvalue > 0.05 (~0.18)

cor()
summary(india)

