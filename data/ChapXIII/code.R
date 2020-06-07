#Chapter XIII
findPtrn <- function(ISgthDF) {
  N <- dim(ISgthDF)[1]
  ISptrnDF <- ISgthDF[c(1:(3*N)),c(1:9)] #sets schema
  #2 years back
  ISptrnDF[1:N,c(1,2)] <- ISgthDF[1:N,c(1,2)]
  ISptrnDF[1:N,c(3:7)] <- round(ISgthDF[1:N,c(3:7)],2)
  ISptrnDF[1:N,c(8:9)] <- rep(NA,2*N)
  ISptrnDF[1:N,7] <- rep(2,N)
  #1 year back
  ISptrnDF[(N+1):(2*N),c(1,2)] <- ISgthDF[1:N,c(1,2)]
  ISptrnDF[(N+1):(2*N),c(3:7)] <- round(ISgthDF[1:N,c(7:10)],2)
  ISptrnDF[(N+1):(2*N),c(8:9)] <- rep(NA,2*N)
  ISptrnDF[(N+1):(2*N),7] <- rep(1,N)
  #0 year back
  ISptrnDF[(2*N+1):(3*N),c(1,2)] <- ISgthDF[1:N,c(1,2)]
  ISptrnDF[(2*N+1):(3*N),c(3:7)] <- round(ISgthDF[1:N,c(11:14)],2)
  ISptrnDF[(2*N+1):(3*N),c(8:9)] <- rep(NA,2*N)
  ISptrnDF[(2*N+1):(3*N),7] <- rep(0,N)
  colnames(ISptrnDF) <- c("symbol","basedate","netincgth",
                          "totrevgth","gsprofgth","dnepsgth","yrsback",
                          "meanabvsp","sdev")
  rownames(ISptrnDF) <- NULL
  ISptrnDF
}
ISptrnDF <- findPtrn(ISgthDF)
D <- dim(ISptrnDF)[1]/3
lab <- ISptrnDF[1:D,1]

idx <- match('UNP',lab)
ISptrnDF[c(idx,idx+D,idx+2*D),]

dir <- 'MVO4'
setwd(paste(homeuser,"/FinAnalytics/",dir,"/",sep=""))
len = 1006 #start with all four years
findCachedPrices <- function(dir,lab,prices,
                             start=NA,end=NA) {
  if(!is.na(start) && !is.na(end))
    prices <- prices[start:end,] #cut down size of prices
  d = 1
  for(l in lab) {
    attempts <- 0
    fileName = paste('cached',l,'.csv',sep='')
    for(subdir in c('NYSE','NASDAQ')) {
      setwd(paste(homeuser,"/FinAnalytics/",dir,'/',
                  subdir,sep=''))
      attempts <- attempts + 1
      if(file.exists(fileName)) {
        break
      } else if(attempts == 2) {
        attempts <- -1 #unsuccessful
      }
    }
    if(attempts == -1) { #unsuccessful
      print(paste(fileName,"not in NYSE nor NASDAQ"))
      prices[,d] = rep(NA,len)[start:end]
    } else { #successful
      print(paste(fileName,"in",subdir))
      prices[,d] = read.csv(fileName,header=TRUE,
                            sep='')[start:end,1]
    }
    d = d + 1
  }
  #return vector may have NAs
  return(prices)
}
#unit test:
labtest <- c('AAN','MCD','PCLN') #2 NYSEs, 1 NASDAQ
dir <- 'MVO4'
len <- 1006
D   <- length(labtest)
px  <- matrix(rep(NA,len*D),nrow=len,ncol=D)
px  <- findCachedPrices(dir,labtest,px,start=253,end=504)

findCached3YrsBackPrices <- function(dir,lab,len) {
  #Go back 3 years in cached files for prices
  D <- length(lab)
  isSplitAdjusted <<- TRUE
  prices2 <- matrix(rep(NA,len*D),nrow=len,ncol=D)
  prices2 <- findCachedPrices(dir,lab,prices2,
                              start=253,end=504)
  prices1 <- matrix(rep(NA,len*D),nrow=len,ncol=D)
  prices1 <- findCachedPrices(dir,lab,prices1,
                              start=504,end=755)
  prices0 <- matrix(rep(NA,len*D),nrow=len,ncol=D)
  prices0 <- findCachedPrices(dir,lab,prices0,
                              start=755,end=1006)
  return(rbind(prices2,prices1,prices0))
}
D <- length(lab)
allPrices <- findCached3YrsBackPrices(dir,lab,len)
dim(allPrices)
prices2 <- allPrices[1:252,]
dim(prices2)
prices1 <- allPrices[253:504,]
dim(prices1)
prices0 <- allPrices[505:756,]
dim(prices0)

library(tseries)
setwd(paste(homeuser,"/FinAnalytics/MVO4",sep=""))

findSPprices <- function(fn="cachedGSPC.csv") {
  if(!file.exists(fn)) {
    pricesSP <- getHistPrices(c('^GSPC'),c(1),len,
                              start="2011-02-09",end="2015-02-09",
                              startBck1="2011-02-08",
                              startFwd1="2011-02-10")[,1]
    write.csv(pricesSP,file="cachedGSPC.csv",row.names=FALSE)
  } else {
    pricesSP <- read.csv("cachedGSPC.csv")[1]
    #error handling
    if(is.na(pricesSP[1,1])) {
      unlink('cachedGSPC.csv')
      findSPprices()
    }
  }
  pricesSP[,1]
}
pricesSP  <- findSPprices()
pricesSP2 <- pricesSP[253:504]
pricesSP1 <- pricesSP[504:755]
pricesSP0 <- pricesSP[755:1006]

R2 <- findR(prices2)
R1 <- findR(prices1)
R0 <- findR(prices0)
r2 <- findR(as.matrix(pricesSP2,252,1))
r1 <- findR(as.matrix(pricesSP1,252,1))
r0 <- findR(as.matrix(pricesSP0,252,1))

findOneYrPriceStats <- function(R,r) {
  #Go back 3 years mean log ret and sdev
  meanSP     <- apply(r,2,mean)
  meanvAbvSP <- apply(R,2,mean)-meanSP
  meanv      <- apply(R,2,mean)
  cov_mat    <- cov(R/100) #rescale back to logret wo 100 factor
  diag_cov_mat <- diag(cov_mat)
  sdevv      <- sqrt(diag_cov_mat)
  SR         <- meanvAbvSP/sdevv
  return(list(meanvAbvSP,sdevv))
}
res <- findOneYrPriceStats(R2,r2)
meanvAbvSP2 <- res[[1]]
sdevv2      <- res[[2]]
res <- findOneYrPriceStats(R1,r1)
meanvAbvSP1 <- res[[1]]
sdevv1      <- res[[2]]
res <- findOneYrPriceStats(R0,r0)
meanvAbvSP0 <- res[[1]]
sdevv0      <- res[[2]]
meanvAbvSP = c(meanvAbvSP0,meanvAbvSP1,meanvAbvSP2)
summary(meanvAbvSP)

augPtrn <- function(ISptrnDF) {
  #augment DF with price stats
  N <- dim(ISptrnDF)[1]/3
  ISptrnDF[1:N,c(8,9)] <-
    cbind(round(meanvAbvSP2,4),round(sdevv2,4))
  ISptrnDF[(N+1):(2*N),c(8,9)] <-
    cbind(round(meanvAbvSP1,4),round(sdevv1,4))
  ISptrnDF[(2*N+1):(3*N),c(8,9)] <-
    cbind(round(meanvAbvSP0,4),round(sdevv0,4))
  ISptrnDF
}
ISptrnDFcln <- na.omit(augPtrn(ISptrnDF))
D <- dim(ISptrnDFcln)[1]/3
lab <- ISptrnDFcln[1:D,1]

thisD <- dim(ISptrnDFcln)[1]/3
idx = match('NSC',lab)
ISptrnDFcln[c(idx,idx+thisD,idx+2*thisD),c(3:9)]

library(moments)
yb2logrets <- ISptrnDFcln[1:D,8]
yb1logrets <- ISptrnDFcln[(D+1):(2*D),8]
yb0logrets <- ISptrnDFcln[(2*D+1):(3*D),8]
alllogrets <- c(yb2logrets,yb1logrets,yb0logrets)
skewness(alllogrets)
kurtosis(alllogrets)
plot(density(yb2logrets),main="")
lines(density(yb1logrets),col=4)
lines(density(yb0logrets),col=9)
abline(v=0.0)
summary(alllogrets)
skewness(alllogrets)
kurtosis(alllogrets)

#Calc classification tree impurity
p = seq(0,1,.01)
En <- function(p) {1-max(p,1-p)}
Gn <- function(p) {p*(1-p)+(1-p)*p}
Dn <- function(p) {-p*log(p)-(1-p)*log(1-p)}
EnVec <- sapply(p,En) #error
GnVec <- sapply(p,Gn) #Gini
DnVec <- sapply(p,Dn) #cross-entropy

plot(p,EnVec,ylim=c(0,1),col=4,type="l",
     ylab="En,Gn,Dn")
text(c(.7),En(.7),"Error",col=4,cex=.95)
lines(p,GnVec,ylim=c(0,1),col=3)
text(c(.7),Gn(.7),"Gini index",col=3,cex=.95)
lines(p,DnVec,ylim=c(0,1),col=2)
text(c(.7),Dn(.7),"Cross-entropy",col=2,cex=.95)

library(party)
attach(ISptrnDFcln)
train <- c(1:D,(D+1):(2*D))
length(train)==2*D

HL=ifelse(ISptrnDFcln$meanabv > 0,"Up>0","Down<=0")
IStreeDF = data.frame(ISptrnDFcln,HL)

thisD <- dim(IStreeDF)[1]/3
idx = match('NSC',lab)
IStreeDF[c(idx,idx+thisD,idx+2*thisD),c(3:8,10)]

attach(IStreeDF)
istree=ctree(HL ~ netincgth + totrevgth + gsprofgth + dnepsgth,
             data=IStreeDF, subset=train)
predRes <- predict(type="response",
                   istree, IStreeDF[-train,])
tbl <- round(table(predRes,IStreeDF[c(-train), "HL"])/D,3)
tbl
(tbl[1,1]+tbl[2,2])/sum(tbl)

par(mar=c(4,4,1,1))
par(mfrow=c(1,1))
plot(istree,cex=.25)

sum(predRes == "Up>0")
sum(predRes != "Up>0")
istree

library(party)
library(randomForest)
library(e1071)
attach(IStreeDF)
runClassifier <- function(IStreeDF,train,name="ctree") {
  if(name == "ctree") {
    classifier=ctree(HL ~ netincgth + totrevgth +
               gsprofgth + dnepsgth,
               data=IStreeDF, subset=train)
    predRes <- predict(type="response",
               classifier, IStreeDF[-train,])
  } else if(name == "randomForest"){
    set.seed(100)
    classifier=randomForest(HL ~ netincgth + totrevgth +
               gsprofgth + dnepsgth,
               data=IStreeDF, subset=train, mtry=4, importance=TRUE)
    predRes <- predict(type="response",
               classifier, IStreeDF[-train,])
  } else if(name == "svm") {
    classifier <- svm(formula=HL ~ netincgth + totrevgth +
               gsprofgth + dnepsgth,data=IStreeDF, subset=train,
               kernel="sigmoid",na.action=na.omit, scale = TRUE)
    predRes <- predict(type="response",
               classifier, IStreeDF[-train,])
  }
  par(mar=c(4,4,1,1))
  par(mfrow=c(1,1))
  if(name == "ctree" || name == "randomForest")
    plot(classifier,cex=.25) 
  tbl <- round(table(predRes,IStreeDF[c(-train), "HL"])/D,3)
  print(tbl)
  print(tbl[1,1]+tbl[2,2]/sum(tbl))
}

runClassifier(IStreeDF,train,"ctree")
runClassifier(IStreeDF,train,"randomForest")
runClassifier(IStreeDF,train,"svm")
