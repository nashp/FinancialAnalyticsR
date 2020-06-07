#Chapter VI
library(quantmod)
library(PerformanceAnalytics)
sym.vec <-c("^GSPC","^VIX")
getSymbols(sym.vec, from = "2005-01-03", to = "2015-09-16")

GSPC <- GSPC[, "GSPC.Adjusted", drop=F]
GSPC.logret = CalculateReturns(GSPC, method="log")
GSPC.logret[1]
GSPC.logret[1] = 0.0
par(mfrow=c(3,1))
plot(GSPC)
plot(GSPC.logret)
plot(VIX)
library(TSA)
library(ggplot2)
data(google)
hist(google, breaks=100)
curve(dnorm(x, mean=mean(google), sd=sd(google)), add=TRUE, col="blue")
ggplot(NULL,aes(x=as.vector(GSPC.logret),y=..density..)) +
  geom_histogram(fill="cornsilk", colour="grey60", size = 0.2) +
  geom_density(colour="blue")
dt4<-function(x) dt(x,df=4)
ggplot(data.frame(x=c(-5,5)),aes(x=x)) +
  stat_function(fun=dnorm, colour="blue") +
  stat_function(fun=dcauchy, colour="green") +
  stat_function(fun=dt4, colour="red")

par(mfrow=c(2,2))
hist(rcauchy(n=10000), main="Cauchy",breaks=100)
hist(rt(n=10000,df=4), main="t(4)",breaks=100)
hist(rnorm(n=10000), main="Standard Normal",breaks=100)
hist(runif(n=10000), main="Uniform",breaks=100)
set.seed(255270)
kurtosis(rcauchy(n=10000))
kurtosis(rt(n=10000,df=4))
kurtosis(rnorm(n=10000))
kurtosis(GSPC.logret[c(-1)]) #remove 1st elem
kurtosis(runif(n=10000))

library(TSA)
data(tempdub)
plot(tempdub,col='blue')
adf.test(tempdub)

month <- season(tempdub)
model1 <- lm(tempdub ~ month - 1)
summary(model1)

data(hare)
plot(hare,col='blue')
adf.test(hare)
par(mfrow=c(2,2))
BoxCox.ar(hare)
plot(sqrt(hare),col='blue')
acf(sqrt(hare))
pacf(sqrt(hare))
adf.test(sqrt(hare))

layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
plot(armasubsets(y=hare,nar=7,nma=7))
acf(sqrt(hare))
pacf(sqrt(hare))

m1.hare <- arima(x=sqrt(hare),order=c(3,0,0))
runs(rstandard(m1.hare))

tsdiag(m1.hare)
par(mfrow=c(1,2))
hist(rstandard(m1.hare))
qqnorm(rstandard(m1.hare),col='blue')
qqline(rstandard(m1.hare))
shapiro.test(residuals(m1.hare))
square<-function(x) {y=x^2}
plot(m1.hare,n.ahead=25,xlab='Year',ylab='Hare Abundance',
         pch=19,transform=square,
         col='blue')

#Earnings of Johnson and Johnson:
data(JJ)
plot(JJ,col='blue')
par(mfrow=c(2,1))
plot(JJ,col='blue')
plot(log(JJ),ylab='log(Earnings)',type='l',col='blue')

par(mfrow=c(3,1))
plot(diff(log(JJ)),ylab='log differenced',type='l',col='blue')
plot(diff(log(JJ),lag=4),ylab='seasonal diff',type='l',col='blue')
plot(diff(diff(log(JJ),lag=4)),ylab='diff differenced',type='l',
     col='blue')

series<-diff(diff(log(JJ),lag=4))
adf.test(series)
par(mfrow=c(1,2))
acf(as.vector(series),ci.type='ma')
pacf(as.vector(series),ci.type='ma')
model<-arima(x=log(JJ),order=c(0,1,1),seasonal=
                 list(order=c(0,1,1),period=4))
model
shapiro.test(residuals(model))
tsdiag(model)
plot(model,n1=c(1975,1), n.ahead=8, pch=19, ylab='Earnings',
       transform=exp,col='blue')

#Monthly Airline Passenger Loadings:
data(airpass)
par(mfrow=c(3,1))
plot(airpass,ylab="Air Passengers",col="blue")
plot(log(airpass),ylab=" Log of Air Passengers",col="blue")
plot(diff(log(airpass)), ylab="Diff of Log Air Passengers",col="blue")
points(diff(log(airpass)),
       x=time(diff(log(airpass))),
       pch=as.vector(season(diff(log(airpass)))))
layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
acf(as.vector(diff(log(airpass))),main="differenced")
acf(as.vector(diff(diff(log(airpass)),lag=12)),
    main="seasonal differenced")
plot(diff(diff(log(airpass)),lag=12),col="blue",
     ylab="seasonal differenced")
hist(diff(diff(log(airpass)),lag=12),main="histogram",
     xlab="difference")

mod <- arima(log(airpass), order = c(0,1,1),seasonal=
               list(order=c(0,1,1),period=12))
mod
tsdiag(mod)
shapiro.test(residuals(mod))
plot(mod,n1=c(1970,1),n.ahead=36,pch=19,
  ylab="Predicted Air Passengers",transform=exp,col="blue")

#Electricity Production:
data(electricity)
plot(electricity,col='blue')
layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
BoxCox.ar(electricity)
acf(diff(log(as.vector(electricity))),main="differenced")
acf(diff(diff(log(as.vector(electricity))),lag=12),
        main="seasonal differenced")
hist(diff(diff(log(as.vector(electricity))),lag=12),
         main="histogram",xlab="difference")
mod2 <- arima(log(electricity), order = c(0,1,1),
          seasonal=list(order=c(0,1,1),period=12))
mod2
tsdiag(mod2)
shapiro.test(residuals(mod2))
plot(mod2,n1=c(2004,1),n.ahead=24,pch=19,
  ylab="Predicted Electricity Production",transform=exp,col="blue")

#Volatility of Google Stock
data(google)
plot(google,col='blue')
price <- exp(cumsum(google)) * 50.12
plot(price,type='l',col='blue')

hist(google,breaks=100)
sum(abs(google)>0.06)
shapiro.test(google)

par(mfrow=c(2,2))
acf(google)
pacf(google)
acf(google^2)
pacf(google^2)
mean(google)
t.test(google, alternative='greater')

par(mfrow=c(1,2))
McLeod.Li.test(y=google)
McLeod.Li.test(y=rnorm(500))
eacf(google^2)

m1 <- garch(x=google-mean(google),order=c(1,1),reltol=1e-6)
summary(m1)

plot(residuals(m1),type='h',ylab='standard residuals',col='blue')
par(mfrow=c(3,1))
plot(price,type='l',col='blue',ylab='price')
plot(google,type='l',col='blue',ylab='log returns')
plot((fitted(m1)[,1])^2,type='l',
     ylab='conditional variance',xlab='time',col='blue')

par(mfrow=c(2,2))
plot(residuals(m1),col="blue",main="Residuals")
hist(residuals(m1))
McLeod.Li.test(y=residuals(m1),main="McLeod-Li")
qqnorm(residuals(m1),col='blue')
qqline(residuals(m1))
shapiro.test(residuals(m1))
var(google)
