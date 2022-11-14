library(quantmod)
getSymbols('TCS.NS')
tail(TCS.NS)
plot(TCS.NS$TCS.NS.Adjusted)
getSymbols('^NSEI')
tail(NSEI)
plot(NSEI$NSEI.Adjusted)
TCS_rt = diff(log(TCS.NS$TCS.NS.Adjusted))
Nifty_rt = diff(log(NSEI$NSEI.Adjusted))
retrn = cbind.xts(TCS_rt,Nifty_rt)
retrn = na.omit(data.frame(retrn))
plot(retrn$NSEI.Adjusted,retrn$TCS.NS.Adjusted
     ,pch=20
     ,xlab='Market Return'
     ,ylab='TCS Return'
     ,xlim=c(-0.18,0.18)
     ,ylim=c(-0.18,0.18))
grid(col='grey',lty=1)
View(retrn)

#MoM
x <- retrn$NSEI.Adjusted
y <- retrn$TCS.NS.Adjusted
n <- length(x)
betahat <- sum((x-mean(x))*(y-mean(y)))/sum((x-mean(x))**2)
alphahat <- mean(y)-betahat*mean(x)
yhat <- alphahat + betahat*x
resi <- y-yhat
sigma <- (sum(resi**2)/(n-2))**0.5
sigma

#linear model using OLS
model<-lm(retrn$TCS.NS.Adjusted~retrn$NSEI.Adjusted,data=retrn)
summary(model)

#Filling table
var(model$residuals)**0.5
model$coefficients[2]
MM_OLS <- data.frame(Parameters=c('alpha','beta','sigma'),
                     MoM=c(alphahat,betahat,sigma),
                     OLS=c(model$coefficients[1],model$coefficients[2],var(model$residuals)**0.5))
MM_OLS
#plot(model)

# Predicting TCS price
a <- log(18200)-log(18000)
tcshat<-alphahat+betahat*a
exp(tcshat+log(3200))
