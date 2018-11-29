# Wharton data in R
setwd("E:/LSE")

# STOCKS : 2005 -- 2012
dataraw2 <- read.csv("STOCKS.csv")
names(dataraw2) 

ids1<-unique(dataraw2[,1])
nAssets1<-length(ids1)
nObs1<-nrow(dataraw2)/nAssets1

data2<-matrix(NA,nObs1,nAssets1+1)
data2[,1]<-dataraw2[dataraw2[,1]==ids1[1],2]

for (i in 1:nAssets1){
  data2[,i+1]<-dataraw2[dataraw2[,1]==ids1[i],3]
}

time2<-as.Date(as.character(data2[,1]),"%Y%m%d")
ticker2 <- as.vector(unique(dataraw2$TICKER))
colnames(data2)[2:(nAssets1+1)]<-ticker2


##### Plot the stock price
price <- data2[,2:(nAssets1+1)]
library(timeSeries)
Y1 = timeSeries(price)
date = as.character(time2)
row.names(Y1) <- date
plot(Y1,main="",col=c("red","green","blue","cadetblue","deeppink","grey","black",
                      "aquamarine","coral","darkorchid3"))


# Calculate and plot the log return.
logprice <- log(price)
logreturn <- diff(logprice)
T1 <- nrow(logreturn) 

Y.logreturn = timeSeries(logreturn)
date.logreturn = as.character(time2[2:(T1+1)])
row.names(Y.logreturn) = date.logreturn
plot(Y.logreturn,main="",col=c("red","green","blue","cadetblue","deeppink","grey","black",
                               "aquamarine","coral","darkorchid3"))

#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
## 2008-3-1 ~ 2008-10-31 stock price
which(time2=="2008-03-03") #796
which(time2=="2008-10-31") #966
which(time2=="2008-3-14") #805
which(time2=="2008-09-15") #932
which(time2=="2008-10-01") #944
which(time2=="2008-10-10") #951

for(i in 1:nAssets1){
plot(time2[796:966],data2[796:966,(i+1)],ylab=ticker2[i],xlab="2008",type="l",xaxt="n")
axis(1,at=time2[c(805,932,944,951)],label=c("14 Mar","15 Sep","1 Oct","10 Oct"))
abline(v=as.Date(as.character("20080314"),"%Y%m%d"),lty=2,col=2)
abline(v=as.Date(as.character("20080915"),"%Y%m%d"),lty=2,col=2)
abline(v=as.Date(as.character("20081001"),"%Y%m%d"),lty=2,col=2)
abline(v=as.Date(as.character("20081010"),"%Y%m%d"),lty=2,col=2)
}

## 2008-3-1 ~ 2008-10-31 log return
plot(time2[544:714],logreturn[544:714,1],type="l",ylim=c(min(logreturn),max(logreturn)),ylab="",xlab="2008",xaxt="n")
axis(1,at=time2[c(553,680,692,699)],label=c("14 Mar","15 Sep","1 Oct","10 Oct"))
title("Log returns of selected stocks")
for(i in 2:nAssets1){
  lines(time2[544:714],logreturn[544:714,i],type="l",col=i)
}
abline(v=as.Date(as.character("20080314"),"%Y%m%d"),lty=3,col=4)
abline(v=as.Date(as.character("20080915"),"%Y%m%d"),lty=3,col=4)
abline(v=as.Date(as.character("20081001"),"%Y%m%d"),lty=3,col=4)
abline(v=as.Date(as.character("20081010"),"%Y%m%d"),lty=3,col=4)
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


###  Plot scatter plots of return pairs
pairs(logreturn[,c("BMO","JPM","BAC","GS")])
pairs(logreturn[,c("BMO","MSFT","IBM","GE")])

library(corrgram)
Y = as.data.frame(logreturn)
corrgram(Y)


#------------------------------------------------------------------------
###~ Generate the portfolio:
lambda <- rep(1000,10) # the number of shares in each stock

#### loss
#~ Construct a loss operator function:
lo.func <- function(Lambda,Price,Logreturn,n, linear=FALSE){
  # parameter Logreturn should be a matrix or vector of n returns (risk-factors)
  if ((!(is.vector(Logreturn))) & (!(is.matrix(Logreturn)))) 
    stop("Logreturn must be vector or matrix with rows corresponding to risk 
         factor return observations")
  N <- n
  T <- 1
  if (is.matrix(Logreturn)) T <- dim(Logreturn)[1]
  
  # Lambda in a matrix
  lambda.mat <- matrix(Lambda,nrow=T,ncol=N,byrow=TRUE)
  
  # On the parameter 'linear': The default is nonlinear which means the 
  # function which calculated the return factor (e.g. mk.returns() used
  # a default log() (or some other nonlinear) transform.  Hence using 
  # exp(x) will reconvert the log() so the summand will reflect actual 
  # changes in value  
  if (linear) 
  { summand <- lambda.mat * Price * Logreturn } 
  else { summand <- lambda.mat * Price * (exp(Logreturn)-1) }
  
  # We return a vector where each row is the change in value summed across 
  # each of the risk factors on a single day.
  # By taking the negative we convert to losses
  loss <- -rowSums(summand)
  return(loss)
}


### daily loss
loss1 <-lo.func(lambda,price[1:T1,],logreturn,nAssets1)

meanloss1 <- mean(loss1)
varloss1<- var(loss1)*(T1-1)/T1 

# ES and VaR
p1 <- 0.95
VaR.normal1 <- meanloss1 + sqrt(varloss1) * qnorm(p1) 
ES.normal1 <- meanloss1 + sqrt(varloss1) * dnorm(qnorm(p1))/(1-p1) 

VaR.hs1 <- quantile(loss1,p1) 
ES.hs1 <- mean(loss1[loss1 > VaR.hs1]) 

#~ Plot results
hist(loss1,nclass=100, prob=TRUE, xlab="Loss Distribution",main="")
abline(v=c(VaR.hs1,VaR.normal1),col=c(1,2),lty=2)
legendnames <- c(paste(p1,"HS VaR"),paste(p1,"normal VaR"))
legend("topleft", legend = legendnames, col=1:2, pch="¡ª¡ª") 


#### test the VaR
#2013
which(time1=="2013-01-02") # 2014
which(time1=="2013-12-31") # 2265
timenew <- time1[2014:2265] 

datanew <- data1[2014:2265,-c(6,8,9,11,16)]
pricenew <- datanew[,2:11]
logpricenew <- log(pricenew)
logreturnnew <- diff(logpricenew)

# VaR.normal1  
count = 0
for(i in 1:nrow(logreturnnew)){
  a = lo.func(lambda,pricenew[i,],logreturnnew[i,],nAssets1)
  if(a>VaR.normal1)
    count = count + 1
}
count/nrow(datanew) 

# VaR.hs1
count = 0
for(i in 1:nrow(logreturnnew)){
  a = lo.func(lambda,pricenew[i,],logreturnnew[i,],nAssets1)
  if(a>VaR.hs1)
    count = count + 1
}
count/nrow(datanew)   

#--------------------------------------------------------------------------

# 2005-2007 VaR
which(time2=="2007-12-31") #754

price.var = price[1:754,]
logreturn.var = logreturn[1:753,]

loss2<- lo.func(lambda,price.var[1:753,],logreturn.var,nAssets1)

meanloss2 <-mean(loss2)
varloss2 <- var(loss2)*(753-1)/753 


# ES and VaR
p1 <- 0.95
VaR.normal2 <- meanloss2 + sqrt(varloss2) * qnorm(p1) 
ES.normal2 <- meanloss2 + sqrt(varloss2) * dnorm(qnorm(p1))/(1-p1) 

VaR.hs2 <- quantile(loss2,p1)
ES.hs2 <- mean(loss2[loss2 > VaR.hs2]) 



#~ Plot results
hist(loss2,nclass=100, prob=TRUE, xlab="Loss Distribution",main="")
abline(v=c(VaR.hs2,VaR.normal2),col=c(1,2),lty=2)
legendnames <- c(paste(p1,"HS VaR"),paste(p1,"normal VaR"))
legend("topleft", legend = legendnames, col=1:2, pch="¡ª¡ª") 



#2008
which(time1=="2008-01-02") # 755
which(time1=="2008-12-31") # 1007
timenew <- time1[755:1007] 

datanew <- data1[755:1007,-c(6,8,9,11,16)]
pricenew <- datanew[,2:11]
logpricenew <- log(pricenew)
logreturnnew <- diff(logpricenew)

# VaR.normal2
count = 0
for(i in 1:nrow(logreturnnew)){
  a = lo.func(lambda,pricenew[i,],logreturnnew[i,],nAssets1)
  if(a>VaR.normal2)
    count = count + 1
}
count/nrow(datanew)

# VaR.hs2
count = 0
for(i in 1:nrow(logreturnnew)){
  a = lo.func(lambda,pricenew[i,],logreturnnew[i,],nAssets1)
  if(a>VaR.hs2)
    count = count + 1
}
count/nrow(datanew) 


#------------------------------------------------------------------

# copula
# focusing on the financial stocks
logreturn.fin <- logreturn[,c(6,7,9,10)]
# Construct pseudo copula data
library(QRM)
copula1 <- apply(logreturn.fin,2,edf,adjust=TRUE)
plot(copula1)

#~ Compare various bivariate models
# Gaussian copula fit
copulaGauss1 <- fit.gausscopula(copula1)
copulaGauss1
# t copula
copulat1 <- fit.tcopula(copula1)
copulat1
# 2-dimensional Archimedian copulas (Gumbel and Clayton)
# copulaGumb1 <- fit.AC(copula1,"gumbel")
copulaClay1 <- fit.AC(copula1,"clayton")
# Evaluate: calculate Spearman rank correlation and Kendall's tau (cf Example 5.54)
# c(copulaGauss1$ll.max, copulat1$ll.max, copulaGumb1$ll.max, copulaClay1$ll.max)
c(copulaGauss1$ll.max, copulat1$ll.max, copulaClay1$ll.max)
#  2314.208 3113.098 1821.328
