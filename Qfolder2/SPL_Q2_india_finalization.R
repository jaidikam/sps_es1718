#prepare rain and temp data: mean amount of rain and mean temperature for each quarter year
rain        = readRDS(".\\Qfolder2\\Q2_india_rain.rds")
temp        = readRDS(".\\Qfolder2\\Q2_india_temp.rds")

rain$ISO3   = NULL
rain$ISO2   = NULL
rain$year   = rain$X.Year
rain$X.Year = NULL
rain$month  = rain$Month
rain$Month  = NULL

temp$year   = temp$X.Year
temp$X.Year = NULL
temp$month  = temp$Month
temp$Month  = NULL

raintemp = merge(rain,temp[c("tas","year","month")],by=c("month","year"))

# Calculate average rain and temp per quarter
# Define function for average rain and temp per quarter
avgRainTempQuarter = function(ds,month,mp_year,pr,tas){
  if (is.na(ds[[pr]]) ||is.na(ds[[tas]])) {
    message(paste("No missing values allowed!"))
  } else {
    ds$tas_q1 = 0
    ds$tas_q2 = 0
    ds$tas_q3 = 0
    ds$tas_q4 = 0
    ds$pr_q1  = 0
    ds$pr_q2  = 0
    ds$pr_q3  = 0
    ds$pr_q4  = 0
    
    for(z in min(ds[[mp_year]]):max(ds[[mp_year]])){
      
      ds[ds[[mp_year]] == z ,]$pr_q1  =   sum(ds[ds[[mp_year]] == z & ds[[month]] %in% c("1","2","3"),][[pr]])/3
      ds[ds[[mp_year]] == z ,]$pr_q2  =   sum(ds[ds[[mp_year]] == z & ds[[month]] %in% c("4","5","6"),][[pr]])/3
      ds[ds[[mp_year]] == z ,]$pr_q3  =   sum(ds[ds[[mp_year]] == z & ds[[month]] %in% c("7","8","9"),][[pr]])/3
      ds[ds[[mp_year]] == z ,]$pr_q4  =   sum(ds[ds[[mp_year]] == z & ds[[month]] %in% c("10","11","12"),][[pr]])/3
      ds[ds[[mp_year]] == z ,]$tas_q1 =   sum(ds[ds[[mp_year]] == z & ds[[month]] %in% c("1","2","3"),][[tas]])/3
      ds[ds[[mp_year]] == z ,]$tas_q2 =   sum(ds[ds[[mp_year]] == z & ds[[month]] %in% c("4","5","6"),][[tas]])/3
      ds[ds[[mp_year]] == z ,]$tas_q3 =   sum(ds[ds[[mp_year]] == z & ds[[month]] %in% c("7","8","9"),][[tas]])/3
      ds[ds[[mp_year]] == z ,]$tas_q4 =   sum(ds[ds[[mp_year]] == z & ds[[month]] %in% c("10","11","12"),][[tas]])/3
    }
    return(ds)
  }  
}
raintemp  = avgRainTempQuarter(raintemp,"month","year","pr","tas")

#load the india dataset and the remaining variables
india_wip = readRDS(".\\Qfolder1\\Q1_india_wip.rds")
rest      = readRDS(".\\Qfolder2\\Q2_india_rest.rds")


#merge india with wheater data
india_wip = merge(india_wip,unique(raintemp[c("tas_q1","tas_q2","tas_q3","tas_q4","pr_q1","pr_q2","pr_q3","pr_q4","year")]),by=c("year")) 

#join the datasets
india_fin = merge(india_wip, rest, by=c("prod_price"))  #prod_price is unique, 
                                                        #therefore it can be used as a key for the merge


#save dataset 
saveRDS(india_fin, (".\\Qfolder2\\Q2_india_fin.rds"))

#cleanup
rm(list = setdiff(ls(), lsf.str()))


