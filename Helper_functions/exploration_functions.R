
#calculating the percentage of the change in a column's value on a fixed base year 
calcPercFixBaseyear =  function(ds, areacol, areaname, yearcol, baseyear,valuecol, perccol){
  base = ds[ds[[yearcol]] == baseyear & ds[[areacol]] %in% areaname, ][[valuecol]]
  if(is.null(ds[[perccol]])){
    ds[[perccol]] = 0 
  }
  #
  for(i in baseyear: max(ds[[yearcol]])){
    later = ds[ds[[yearcol]]==i & ds[[areacol]] %in% areaname,][[valuecol]]
    sub =  later - base
    ds[ds[[yearcol]] == i & ds[[areacol]] %in% areaname,][[perccol]] = (sub / later) * 100
  }
  return(ds)
}


# calculate the prcentage of changes in a colname value in a predefined year, where the base is for every change is the value from the previous year.
calcPercPreBaseyear = function(ds, areacol, areaname, yearcol, valuecol){
  for(i in unique(ds[[yearcol]])){
    base = ds[ds[[yearcol]] == i & ds[[areacol]] %in% areaname, ][[valuecol]]
    later = ds[ds[[yearcol]] == i+1 & ds[[areacol]] %in% areaname, ][[valuecol]]
    #if(length(later) == 0L) break
    if(i == 2015L) break
    sub =  later - base
    if(length(sub) == 0L)next 
    ds[ds[[yearcol]] == i+1 & ds[[areacol]] %in% areaname , paste(valuecol, "Percent", sep = "_")]= (sub / later) * 100
  }
  ds[ds[[yearcol]] == min(ds[[yearcol]]) & ds[[areacol]] %in% areaname, paste(valuecol, "Percent", sep = "_")]= 0
  return(ds)
}

# To plot production data with specific area and itmes
prodPlot = function(ds, area, items){
  p = ggplot(data=ds[ds$Area == area & ds$Item %in% items,], aes(x=Year, y=Percentage, colour=Item)) +
    geom_line() +
    geom_point()+
    ylim(-30, 75)+
    ggtitle(label=area)+
    ylab(label="Normalized Production Amount") +
    xlab("Year")
  return(p)
}

# bar plot for product price change 

prodBarPlot = function(d, dname){
  ggplot(d[d$year %in% c(2010:2015) ,c("year", "prod_name", "prod_price_Percent")], aes(x = year, y = prod_price_Percent)) +
    geom_bar(aes(fill = prod_name), position = "dodge", stat="identity") +
    ggtitle(label=dname)+
    ylab(label="price change based on previous year") +
    xlab("Year")
}

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}