if(!require("Hmisc")) install.packages("Hmisc"); library("Hmisc")
if(!require("corrplot")) install.packages("corrplot"); library("corrplot")

data <- read.csv(file=".\\wfp_market_food_prices.csv",head=TRUE,sep=",")
rain <- read.csv(file=".\\rain_india.csv",head=TRUE,sep=";")
temp <- read.csv(file=".\\temp_india.csv",head=TRUE,sep=";")
prodcrops <- read.csv(file=".\\prodcrops_india.csv",head=TRUE,sep=";")
daycal <- read.csv(file=".\\per_capita_calories_india.csv",head=TRUE,sep=";")
exports <- read.csv(file=".\\india_exp.csv",head=TRUE,sep=";")
imports <- read.csv(file=".\\india_imp.csv",head=TRUE,sep=";")
agrigdp <- read.csv(file=".\\agri_gdp.csv",head=TRUE,sep=";")
gni_pc <- read.csv(file=".\\gni_pc_india.csv",head=TRUE,sep=";")
inflation <- read.csv(file=".\\inflation_india.csv",head=TRUE,sep=";")
oilprice <- read.csv(file=".\\opec-oil-price-annually.csv",head=TRUE,sep=";")
population <- read.csv(file=".\\population-india.csv",head=TRUE,sep=";")

#replace commas with dots so we can convert to numeric
rain$pr = as.numeric(gsub(",", ".", gsub("\\.", "", rain$pr)))
temp$tas = as.numeric(gsub(",", ".", gsub("\\.", "", temp$tas)))
exports$exp_sug = as.numeric(gsub(",", ".", gsub("\\.", "", exports$exp_sug)))
exports$exp_veg = as.numeric(gsub(",", ".", gsub("\\.", "", exports$exp_veg)))
exports$exp_cer = as.numeric(gsub(",", ".", gsub("\\.", "", exports$exp_sug)))
imports$imp_sug = as.numeric(gsub(",", ".", gsub("\\.", "", imports$imp_sug)))
imports$imp_veg = as.numeric(gsub(",", ".", gsub("\\.", "", imports$imp_veg)))
imports$imp_cer = as.numeric(gsub(",", ".", gsub("\\.", "", imports$imp_cer)))
agrigdp$agri_gdp = as.numeric(gsub(",", ".", gsub("\\.", "", agrigdp$agri_gdp)))
gni_pc$gni_pc = as.numeric(gsub(",", ".", gsub("\\.", "", gni_pc$gni_pc)))
inflation$cp_inflation = as.numeric(gsub(",", ".", gsub("\\.", "", inflation$cp_inflation)))
oilprice$avg_p_barrel = as.numeric(gsub(",", ".", gsub("\\.", "", oilprice$avg_p_barrel)))

prodcrops$prod_amount_y = as.numeric(levels(prodcrops$prod_amount_y))[prodcrops$prod_amount_y]




#we only look at crops falling under HS Code 2017 06-15
#https://www.foreign-trade.com/reference/hscode.htm?cat=2
#top 4 crops in terms of produced amount: sugarcane, rice, wheat, potatoes
#according to statistical yearbook of india 2017
#http://www.mospi.gov.in/statistical-year-book-india/2017/177
indiafoods = c("Sugar","Rice","Wheat","Potatoes")
india = data[data$adm0_name == 'India' & data$mp_year >= 2001,]
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
#remove unnecessary variables
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



#average price per year
india$avg_price_prod_year = 0
for (x in min(india$year):max(india$year)){
  print(paste('year is ',x))
  for(y in 1:NROW(indiafoods)){
    print(paste('food is ',indiafoods[y]))
    if(NROW(india[india$year == x & india$cm_name == indiafoods[y],]$avg_price_prod_year) > 0){
      india[india$year == x & india$cm_name == indiafoods[y],]$avg_price_prod_year = sum(india[india$year == x & india$cm_name == indiafoods[y],]$avg_price_prod_month) / NROW(india[india$year == x & india$cm_name == indiafoods[y],]$avg_price_prod_month)
    }
   
  }
}

#prepare rain and temp data: mean amount of rain / mean temp for each quarter year
rain$ISO3 = NULL
rain$ISO2 = NULL
rain$year = rain$X.Year
rain$X.Year = NULL
rain$month = rain$Month
rain$Month = NULL
rain$pr_q1 = 0
rain$pr_q2 = 0
rain$pr_q3 = 0
rain$pr_q4 = 0
rain$q =0
rain[rain$month == c(1,2,3),]$q = 1
rain[rain$month == c(4,5,6),]$q = 2
rain[rain$month == c(7,8,9),]$q = 3
rain[rain$month == c(10,11,12),]$q = 4
temp$tas_q1 = 0
temp$tas_q2 = 0
temp$tas_q3 = 0
temp$tas_q4 = 0
temp$year = temp$X.Year
temp$X.Year = NULL
temp$month = temp$Month
temp$Month = NULL
temp$q =0
temp[temp$month == c(1,2,3),]$q = 1
temp[temp$month == c(4,5,6),]$q = 2
temp[temp$month == c(7,8,9),]$q = 3
temp[temp$month == c(10,11,12),]$q = 4


# average rain and temp per quarter
for(z in 2001:2015){
  rain[rain$year == z ,]$pr_q1 = sum(rain[rain$year == z & rain$month == c(1,2,3),]$pr)/3
  rain[rain$year == z ,]$pr_q2 = sum(rain[rain$year == z & rain$month == c(4,5,6),]$pr)/3
  rain[rain$year == z ,]$pr_q3 = sum(rain[rain$year == z & rain$month == c(7,8,9),]$pr)/3
  rain[rain$year == z ,]$pr_q4 = sum(rain[rain$year == z & rain$month == c(10,11,12),]$pr)/3
  
  temp[temp$year == z ,]$tas_q1 = sum(temp[temp$year == z & temp$month == c(1,2,3),]$tas)/3
  temp[temp$year == z ,]$tas_q2 = sum(temp[temp$year == z & temp$month == c(4,5,6),]$tas)/3
  temp[temp$year == z ,]$tas_q3 = sum(temp[temp$year == z & temp$month == c(7,8,9),]$tas)/3
  temp[temp$year == z ,]$tas_q4 = sum(temp[temp$year == z & temp$month == c(10,11,12),]$tas)/3
}


india$month = NULL
india$avg_price_prod_month = NULL
india = unique(india)


india = merge(india,unique(rain[c("pr_q1","pr_q2","pr_q3","pr_q4","year")]),by=c("year")) 
india = merge(india,unique(temp[c("tas_q1","tas_q2","tas_q3","tas_q4","year")]),by=c("year")) 
india = merge(india,prodcrops[c("year","cm_name","prod_amount_y")],by=c("year","cm_name")) 
india = merge(india,daycal[c("year","daily_caloric_supply")],by=c("year")) 
india = merge(india,exports,by=c("year")) 
india = merge(india,imports,by=c("year")) 
india = merge(india,agrigdp,by=c("year"))
india = merge(india,gni_pc,by=c("year"))
india = merge(india,inflation ,by=c("year"))
india = merge(india,oilprice  ,by=c("year"))
india = merge(india,population  ,by=c("year"))







#save our dataset for later
saveRDS(india, (".\\india.rds"))



