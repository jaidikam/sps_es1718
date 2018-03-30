
#loading the food prices for India
india  = readRDS(".\\Qfolder1\\Q1_india_prices.rds")

#calculate average price per food per month for the whole country
#Define the function for food price per month across all markets
avgPriceFoodMonth = function(ds,cm_name,mp_price,mp_year,mp_month){
  dsfoods = unique(ds[[cm_name]])
  ds$avg_price_prod_month = 0
  for (k in min(ds[[mp_year]]):max(ds[[mp_year]])){                      
    print(paste('year is ',k))
    for (j in 1:NROW(dsfoods)){                                           
      a <- ds[ds[[mp_year]] == k & ds[[cm_name]] == dsfoods[j],]
      print(paste('food is ',dsfoods[j]))
      for (i in 1:12){                                                
        b <- a[a[[mp_month]] == i,]
        if (NROW(ds[ds[[mp_year]] == k 
                    & ds[[cm_name]] == dsfoods[j] 
                    & ds[[mp_month]] == i,]$avg_price_prod_month) >0) {
                          ds[ds[[mp_year]] == k 
                             & ds[[cm_name]] == dsfoods[j] 
                             & ds[[mp_month]] == i,]$avg_price_prod_month 
                             =  sum(b[[mp_price]])/ NROW(b)
                             print(paste('month is ',i))  
        }
        
      }
    }
  }
  return(ds)
}


india = avgPriceFoodMonth(india,"cm_name","price","year","month")

#calculate average price per food per year 
#Define function for average price per year
avgPriceFoodYear = function(ds,cm_name,mp_year,avg_price_prod_month){
  dsfoods = unique(ds[[cm_name]]) 
  ds$avg_price_prod_year = 0
  for (x in min(ds[[mp_year]]):max(ds[[mp_year]])){
    print(paste('year is ',x))
    for(y in 1:NROW(dsfoods)){
      print(paste('food is ',dsfoods[y]))
      if(NROW(ds[ds[[mp_year]] == x & ds[[cm_name]] == dsfoods[y],]$avg_price_prod_year) > 0){
        ds[ds[[mp_year]] == x & ds[[cm_name]] == dsfoods[y],]$avg_price_prod_year = sum(ds[ds[[mp_year]] == x & ds[[cm_name]] == dsfoods[y],][[avg_price_prod_month]]) / NROW(ds[ds[[mp_year]] == x & ds[[cm_name]] == dsfoods[y],][[avg_price_prod_month]])
      }
      
    }
  }
  return(ds)
}


india  = avgPriceFoodYear(india,"cm_name","year","avg_price_prod_month")

# we've decided to base our analysis on years, so we delete month related columns and other columns we don'd need
india$month                 = NULL
india$avg_price_prod_month  = NULL
india$adm1_name             = NULL
india$mkt_name              = NULL
india$price                 = NULL
india                       = unique(india)

#renaming columns due to convention
colnames(india)[colnames(india) %in% "avg_price_prod_year"] = "prod_price"
colnames(india)[colnames(india) %in% "adm0_name" ] = "country"
colnames(india)[colnames(india) %in% "cm_name"] = "prod_name"
colnames(india)[colnames(india) %in% "um_id" ] = "prod_uid"
colnames(india)[colnames(india) %in% "um_name" ] = "prod_unit"


#save our dataset for later
saveRDS(india, (".\\Qfolder1\\Q1_india_wip.rds"))

#cleanup
rm(list = setdiff(ls(), lsf.str()))


