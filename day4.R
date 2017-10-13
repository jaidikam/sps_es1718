#1)
dfnorm <- rnorm(100, 0, 2)
dfnorm <- as.data.frame(dfnorm)
options(digits=8)
str(dfnorm)


dfnorm[dfnorm[,1]>=0,] # case xi >=0
dfnorm[dfnorm[,1]>-1 & dfnorm[,1]<0,] # case -1 < xi <0
dfnorm[dfnorm[,1]<=-1,] # case xi <-1
y1<-0


dftemp <- dfnorm

case1 <- dftemp[dftemp[,1]>=0,]+y1
case2 <- y1- 2*(dftemp[dftemp[,1]>-1 & dftemp[,1]<0,])
case3 <- y1-dftemp[dftemp[,1]<=-1,]



#2)


ddf <- as.data.frame(unique(iris$Species))
i<-0
for (i in 1:NROW(ddf)){
 mmin<- paste("min",i,"") 
 assign(mmin, min(iris[iris$Species==ddf[i,],]$Sepal.Length))
 
 mmax<- paste("max",i,"") 
 assign(mmax,max(iris[iris$Species==ddf[i,],]$Sepal.Length))
 
 i+1
}

#3)
dax.prices <- read.csv("./dax_prices.csv")
dax.prices <- na.omit(dax.prices)
# alternative
dax.prices[complete.cases(dax.prices),]


#4)




fun1 <-  function(x){
  dftemp <- x
  y1<-0
  if (test_expression) {
    statement
  }
  
  return(y)
}