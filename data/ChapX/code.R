#Chapter X
setwd(paste(homeuser,"/FinAnalytics/ChapX",sep=""))

spy=read.csv("spy.csv",header=TRUE)
ewj=read.csv("ewj.csv",header=TRUE)
ewl=read.csv("ewl.csv",header=TRUE)
spy[1:3,]
smoothProbspy=read.csv("smoothProbspy.csv",header=TRUE)
smoothProbspy[1:3,]

#Plot series:
par(mfrow=c(5,1))
par(mar=c(1,2,1,1))
plot(spy[,5],type="l",col=4)
plot(ewj[,5],type="l",col=4)
plot(ewl[,5],type="l",col=4)
plot(smoothProbspy[,1],type="l",col=4) 
#lines(smoothProbspy[,2],type="l",col=5)

stateProb=rep(0,length(ewl$Date))
for (i in 1:length(ewl$Date)){
  if (smoothProbspy$V1[i]>0.5){
    stateProb[i]=1.0
  }
}
plot(stateProb,type="l",col=4) 

#sum(stateProb)
Rspy=rep(0,length(ewl$Date))
Rewl=rep(0,length(ewl$Date))
Rewj=rep(0,length(ewl$Date))
for (i in 2:length(ewl$Date)) {
  Rspy[i]=spy$Adj.Close[i]/spy$Adj.Close[i-1]-1
  Rewl[i]=ewl$Adj.Close[i]/ewl$Adj.Close[i-1]-1
  Rewj[i]=ewj$Adj.Close[i]/ewj$Adj.Close[i-1]-1
}

a=1
b=1
#sum(stateProb)
bullRspy=rep(0,sum(stateProb))
bullRewl=rep(0,sum(stateProb))
bullRewj=rep(0,sum(stateProb))
bearRspy=rep(0,length(ewl$Date)-sum(stateProb))
bearRewl=rep(0,length(ewl$Date)-sum(stateProb))
bearRewj=rep(0,length(ewl$Date)-sum(stateProb))
for (i in 1:length(ewl$Date)) {
  if (smoothProbspy$V1[i]>0.5) {
    bullRspy[a]=Rspy[i]
    bullRewl[a]=Rewl[i]
    bullRewj[a]=Rewj[i]
    a=a+1
  } else {
    bearRspy[b]=Rspy[i]
    bearRewl[b]=Rewl[i]
    bearRewj[b]=Rewj[i]
    b=b+1
  }
}
#Plot series:
par(mfrow=c(3,1)) 
par(mar=c(2,2,1,1)) 
plot(bearRspy,type="l",col=4,main="S&P 500 Index") 
plot(bearRewl,type="l",col=4,main="Swiss Index") 
plot(bearRewj,type="l",col=4,main="Japanese Index") 

hist(bearRspy,breaks=40,col=4,xlim=c(-.2,.2),main="S&P 500 Index")
hist(bearRewl,breaks=40,col=4,xlim=c(-.2,.2),main="Swiss Index") 
hist(bearRewj,breaks=40,col=4,xlim=c(-.2,.2),main="Japanese Index") 
mean(bearRspy)
mean(bearRewl)
mean(bearRewj)

setwd(paste(homeuser,"/FinAnalytics/ChapX",sep=""))
ec = read.csv("ECprices201305.csv")[,1]
(diff(log(ec))>0)[1:20]

N=10000
par(mar=c(2,2,2,2))
computePostDist <- function(n=5) {
  theta = vector(length=N)
  betaDensTheta = vector(length=N)
  priorDensTheta = vector(length=N)
  postYisnDensTheta = vector(length=N)
  postYis0DensTheta = vector(length=N)
  postYisyDensTheta = matrix(rep(0,(n+1)*N),nrow=(n+1),ncol=N)
  for(i in 1:N) {
    theta[i] = i/N
    betaDensTheta[i] = dbeta(theta[i],2,2)
    #validate our expression for priorDensTheta
    priorDensTheta[i] = 6*theta[i]*(1-theta[i])
    postYisnDensTheta[i] = 6*theta[i]*(1-theta[i])*(theta[i])^n
    postYis0DensTheta[i] = 6*theta[i]*(1-theta[i])*(1-theta[i])^n
    for(y in 0:n)
      postYisyDensTheta[(y+1),i] = dbeta(theta[i],2,2)*dbinom(y,n,theta[i])
  }
  print(paste("Cn is",sum(postYisnDensTheta/N)))
  print(paste("C0 is",sum(postYis0DensTheta/N)))
  #
  postYisnDensTheta = N*postYisnDensTheta/sum(postYisnDensTheta)
  for(y in 0:n)
    postYisyDensTheta[(y+1),] = N*postYisyDensTheta[(y+1),]/
    sum(postYisyDensTheta[(y+1),])
  par(mfrow=c(ceiling((4+n)/3),3))
  plot(theta,betaDensTheta,type='l',col=4)
  plot(theta,priorDensTheta,type='l',col=4)
  plot(theta,postYisnDensTheta,type='l',col=4)
  #par(mfrow=c(2,3))
  for(y in 0:n)
    plot(theta,postYisyDensTheta[(y+1),],col=4,
         type='l',main=paste("Y =",y),ylab="prob")
}
computePostDist()

setwd(paste(homeuser,"/FinAnalytics/ChapX",sep=""))
ec = read.csv("ECprices201305.csv")[,1]
maxlag=30
n=59
acfval = vector(length=(maxlag+1))
R = 100*diff(log(ec[1:(n+1)]))
Rbar = mean(R)
for(lag in 0:maxlag) {
  R1=R[1:(n-lag)] #ec[1:(60-lag)]))
  R2=R[(1+lag):n] #ec[(lag+1):60]))
  if(lag == 0)
    c0 = 1/n*sum((R-Rbar)*(R-Rbar))
  acfval[lag+1] = 1/n*sum((R1-Rbar)*(R2-Rbar))/c0
}
par(mfrow=c(1,2))
plot(R1,type='l',ylim=c(-.04,.04),col=5,
     main=paste("Lag =",maxlag))
lines(R2,type='l',col=3)
round(acfval,3)
acf <- acf(R, lag.max=maxlag)
acf
lines(0:maxlag,acfval,col=5)

round(acfval,3)

setwd(paste(homeuser,"/FinAnalytics/ChapXI",sep=""))
ec = read.csv("ECprices201308.csv")[,1]
ind = diff(log(ec))>0 #[1:60])) > 0
len = length(ind)
sum = matrix(rep(0,4),nrow=2,ncol=2)
N = 0
for(t in 1:(len-1)) {
  Y1 = ind[t]
  Y2 = ind[t+1]
  if(!Y1 && !Y2) sum[1,1] = sum[1,1] + 1
  if(!Y1 && Y2)  sum[1,2] = sum[1,2] + 1
  if(Y1 && !Y2)  sum[2,1] = sum[2,1] + 1
  if(Y1 && Y2)   sum[2,2] = sum[2,2] + 1
  N = N + 1
}
prob = sum/N
ind
prob
prob/.25
sum(prob)

#Should we expect consec. logrets to be
#up then down with prob .2500?
set.seed(1001)
N <- 30000;vec<-rnorm(N); sum<-0
for(i in 1:(N-1)){
  if(vec[i]>0 && vec[i+1]<=0) sum<-sum+1
}
sum/N
100*sum/N/.2500

countInd <- function(R) {
  ind = R > 0
  len = length(ind)
  sumUp = 0; sumDn = 0
  N = 0
  for(t in 1:(len-4)) {
    if(is.na(ind[t])) {
      ind[t] = ind[t+1]
      print(ind[t])
    }
    Y1 = ind[t]
    Y2 = ind[t+1]
    Y3 = ind[t+2]
    Y4 = ind[t+3]
    Y5 = ind[t+4]
    if(Y1 && Y2 && Y3 && Y4 && Y5)
      sumUp = sumUp + 1
    if(!Y1 && !Y2 && !Y3 && !Y4 && !Y5)
      sumDn = sumDn + 1
    N = N + 1
    #print(paste(Y1,"->",Y2))
  }
  probUp = sumUp/N
  print(paste("Prob of seeing long ind",probUp))
  print(paste(round(probUp/(1/32)*100,2),"of 100 %"))
  probDn = sumDn/N
  print(paste("Prob of seeing shrt ind",probDn))
  print(paste(round(probDn/(1/32)*100,2),"of 100 %"))
  N
}
#unit test:
pvec <- c(1.3,1.2,1.4,1.25,1.2,1.4,1.2,1.25,1.35,1.4,1.35,
          1.3,1.2,1.24,1.25,1.26,1.27,1.28,1.25,1.35,1.4,1.35,
          1.3,1.2,1.4,1.25,1.2,1.4,1.2,1.25,1.35,1.4,1.35,
          1.3,1.2,1.4,1.25,1.2,1.4,1.2,1.25,1.35,1.4,1.35)
countInd(diff(log(pvec)))

#Collecting 5 consecutive log ret directions
setwd(paste(homeuser,"/FinAnalytics/ChapX",sep=""))
par(mfrow=c(1,2))
ec = read.csv("ECprices201305.csv")[,1]
plot(diff(log(ec)),type='l',ylim=c(-.006,.006))
countInd(diff(log(ec)))
ec2 = rnorm(length(ec),0,sd(diff(log(ec))))
plot(ec2,type='l',ylim=c(-.006,.006))
countInd(ec2)
