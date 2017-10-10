a = "pommes frites"
b = chartr("oemsp", "eol H", a)
c = unlist (strsplit (b, " "))[c(1 ,3)]


#exercise day 1 
#1)
ba <- "slumdog"  
bi <- "millionaire"
bo <- paste(ba,bi,sep = " ")

#2)
bu <- seq(from = 1, to = 19, by = 2)
bu[-8]
bu <- matrix(bu,nrow=1)


#3)
n <- seq(from = 1, to = 8, by = 1)
boo <- c(2^n)
boo <- matrix(boo,ncol=1)

#4)
bee <- n^2
bee <- matrix(bee,ncol=1)

#5)
baa <- which(bee==boo)


#10)
funct <-  function(x){
 y=exp(1)^-x
  
  return(y)
}


boo <- funct(seq(from = -3, to = 3, by = 0.5))


#11)

d <- seq(from = 1, to = 100, by = 1)
e <- matrix(7 ,ncol = 100)


#15)
funct1 <-  function(x){
  y= x*5 +x*4 +x*3 +x*2+x+1
  return(y)
}

funct2 <-  function(x){
  
  y= 1+x *(1+x *(1+x *(1+x *(1+x))))
  return(y)
}

funcdiff <- funct1(n)-funct2(n)









sum(1:100)
cumsum(1:100)
cummin(100:1)
