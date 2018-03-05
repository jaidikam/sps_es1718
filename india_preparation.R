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

#replace commas with dots where necessary so we can convert to numeric
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


#calculate average price per food per month for the whole country
india =avgPriceFoodMonth(india,"cm_name","mp_price","mp_year","mp_month")
#calculate average price per food per year 
india = avgPriceFoodYear(india,"cm_name","mp_year","avg_price_prod_month")

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





#prepare rain and temp data: mean amount of rain / mean temp for each quarter year

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

raintemp = merge(rain,temp[c("tas","year","month")],by=c("month","year"))



# average rain and temp per quarter
raintemp = avgRainTempQuarter(raintemp,"month","year","pr","tas")

# we've decided to base our analysis on years, so we delete month related columns
india$month = NULL
india$avg_price_prod_month = NULL
india = unique(india)



india = merge(india,unique(raintemp[c("tas_q1","tas_q2","tas_q3","tas_q4","pr_q1","pr_q2","pr_q3","pr_q4","year")]),by=c("year")) 
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



