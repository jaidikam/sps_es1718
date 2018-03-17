
#calculating the percentage of the change in a column's value on a fixed base year 
calcPercFixBaseyear =  function(ds,namecol,cname,yearcol, baseyear,valuecol, perccol){
  base = ds[ds[[yearcol]] == baseyear & ds[[namecol]] %in% cname, ][[valuecol]]
  if(is.null(ds[[perccol]])){
    ds[[perccol]] = 0 
  }
  #
  for(i in baseyear: max(ds[[yearcol]])){
    later = ds[ds[[yearcol]]==i & ds[[namecol]] %in% cname,][[valuecol]]
    sub =  later - base
    ds[ds[[yearcol]] == i & ds[[namecol]] %in% cname,][[perccol]] = (sub / base) * 100
  }
  return(ds)
}


# calculate the prcentage of changes in a colname value in a predefined year, where the base is for every change is the value from the previous year.
calcPercPreBaseyear = function(ds, colname, yearcol){
  for(i in ds[, yearcol]){
    base = ds[ds[,yearcol] == i, colname]
    later = ds[ds[,yearcol] == i+1, colname]
    if(length(later) == 0L) break
    sub =  later - base
    ds[ds[,yearcol] == i+1, paste(colname, "Percent", sep = "_")]= (sub / base) * 100
  }
  ds[ds[,yearcol] == min(ds[,yearcol]), paste(colname, "Percent", sep = "_")]= 0
  return(ds)
}

# To plot production data with specific area and itmes
prodPlot = function(ds, area, items){
  p = ggplot(data=ds[ds$Area == area & world_production$Item %in% items,], aes(x=Year, y=Production_Amount, colour=Item)) +
    geom_line() +
    geom_point()+
    ggtitle(label=area)+
    ylab(label="Normalized Production Amount") +
    xlab("Year")
  return(p)
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