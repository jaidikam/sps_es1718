#1)

df = c("Belgium","Denmark","France","GB","Ireland","Italy","Luxembourg","Holland","Portugal","Spain"
       ,"USA","Japan","Deutschland")

df =cbind(df,c(2.8,1.2, 2.1,1.6,1.5, 4.6, 3.6,2.1, 6.5, 4.6,3.0, 1.3, 4.2))
df = cbind(df, c(9.4,
                 10.4,
                 10.8,
                 10.5,
                 18.4,
                 11.1,
                 2.6,
                 8.8,
                 5.0,
                 21.5,
                 6.7,
                 2.5,
                 5.6))

df=as.data.frame(df)

colnames(df)[1] <- "Land"
colnames(df)[2] <- "increase of the index (x)"
colnames(df)[3] <- "unemployment (y)"

df[,2] <- as.numeric(as.character(df[,2]))
df[,3] <- as.numeric(as.character(df[,3]))    
range1 <-max(df[,2])-min(df[,2])
range2 <-max(df[,3])-min(df[,3])

#2-5)
mtcars[order(mtcars$mpg, -mtcars$cyl),]
mpg <- mtcars$mpg
hp <- mtcars$hp
mtcars$mpg <- hp
mtcars$hp <- mpg
mtcars$carb <- NULL
r.cars <- mtcars

#6)
mtcars[grep("Merc", rownames(mtcars)),]

#7
dax.prices <- read.csv("./dax_prices.csv")
str(dax.prices)
dax.prices$DAXPrices <- dax.prices$DAX
dax.prices$DAX <- NULL
dax.prices$DAXPrices

#8-11)

write.table(dax.prices, "./daxprices.txt", append = FALSE, sep = ";", dec = ",",
            row.names = TRUE, col.names = TRUE)
