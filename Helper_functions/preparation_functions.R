#calculate average price per food per month for the whole country
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
        if (NROW(ds[ds[[mp_year]] == k & ds[[cm_name]] == dsfoods[j] & ds[[mp_month]] == i,]$avg_price_prod_month) >0) {
          ds[ds[[mp_year]] == k & ds[[cm_name]] == dsfoods[j] & ds[[mp_month]] == i,]$avg_price_prod_month =  sum(b[[mp_price]])/ NROW(b)
          print(paste('month is ',i))  
        }
        
      }
    }
  }
  return(ds)
}




#average price per year
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



# average rain and temp per quarter
avgRainTempQuarter = function(ds,month,mp_year,pr,tas){

  if (is.na(ds[[pr]]) ||is.na(ds[[tas]])) {
    message(paste("No missing values allowed!"))
  } else {

    ds$tas_q1 = 0
    ds$tas_q2 = 0
    ds$tas_q3 = 0
    ds$tas_q4 = 0
    ds$pr_q1 = 0
    ds$pr_q2 = 0
    ds$pr_q3 = 0
    ds$pr_q4 = 0
    
    
    for(z in min(ds[[mp_year]]):max(ds[[mp_year]])){
   
      ds[ds[[mp_year]] == z ,]$pr_q1 = sum(ds[ds[[mp_year]] == z & ds[[month]] %in% c("1","2","3"),][[pr]])/3
      ds[ds[[mp_year]] == z ,]$pr_q2 = sum(ds[ds[[mp_year]] == z & ds[[month]] %in% c("4","5","6"),][[pr]])/3
      ds[ds[[mp_year]] == z ,]$pr_q3 = sum(ds[ds[[mp_year]] == z & ds[[month]] %in% c("7","8","9"),][[pr]])/3
      ds[ds[[mp_year]] == z ,]$pr_q4 = sum(ds[ds[[mp_year]] == z & ds[[month]] %in% c("10","11","12"),][[pr]])/3
      ds[ds[[mp_year]] == z ,]$tas_q1 = sum(ds[ds[[mp_year]] == z & ds[[month]] %in% c("1","2","3"),][[tas]])/3
      ds[ds[[mp_year]] == z ,]$tas_q2 = sum(ds[ds[[mp_year]] == z & ds[[month]] %in% c("4","5","6"),][[tas]])/3
      ds[ds[[mp_year]] == z ,]$tas_q3 = sum(ds[ds[[mp_year]] == z & ds[[month]] %in% c("7","8","9"),][[tas]])/3
      ds[ds[[mp_year]] == z ,]$tas_q4 = sum(ds[ds[[mp_year]] == z & ds[[month]] %in% c("10","11","12"),][[tas]])/3
    }
    return(ds)
  }  
}


