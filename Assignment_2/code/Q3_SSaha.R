attach(faithful)
head(faithful)
hist(eruptions)
hist(waiting,probability = T)
#model1
a=waiting
p<-length(a[a<65])/length(a)
p
d = p*dgamma(waiting,shape=26,scale = 2)+(1-p)*dnorm(waiting,mean=80,sd=6)
lines(waiting,d,lwd=2,col='blue')

NegLogLikeMix <- function(theta,data){
  mu1 = theta[1]
  sigma1 = (theta[2])
  mu2 = theta[3]
  sigma2 = exp(theta[4])
  p = exp(theta[5])/(1+exp(theta[5]))
  n = length(data)
  l=0
  for(i in 1:n){
    l = l + log(p*dgamma(data[i],shape= mu1,scale=sigma1)
                +(1-p)*dnorm(data[i],mean=mu2,sd=sigma2))
  }
  return(-l)
}
theta_initial=c(26,2,80,6,0.35)
NegLogLikeMix(theta_initial,waiting)

fit = optim(theta_initial
            ,NegLogLikeMix
            ,data=waiting
            ,control = list(maxit=1500))

theta_hat = fit$par
mu1_hat = theta_hat[1]
sigma1_hat = theta_hat[2]
mu2_hat = theta_hat[3]
sigma2_hat = exp(theta_hat[4])
p_hat = exp(theta_hat[5])/(1+exp(theta_hat[5]))


d_mle1 = p_hat*dgamma(waiting,shape= mu1_hat,scale=sigma1_hat)+(1-p_hat)*dnorm(waiting,mean=mu2_hat,sd=sigma2_hat)
lines(waiting,d_mle1,lwd=3,col='purple')



#model 2
hist(eruptions)
hist(waiting,probability = T)
a=waiting
p<-length(a[a<65])/length(a)
p
t=mean(a[a<65])
s=var((a[a<65]))
scale1=s/t
shape1=t/scale1

t1=mean(a[a>=65])
s1=var((a[a>=65]))
scale2=s1/t1
shape2=t1/scale2


d = p*dgamma(waiting,shape=shape1,scale =scale1)+(1-p)*dgamma(waiting,shape=shape2,scale = scale2)
lines(waiting,d,lwd=2,col='blue')




NegLogLikeMix2 <- function(theta,data){
  mu1 = theta[1]
  sigma1 = (theta[2])
  mu2 = theta[3]
  sigma2 = (theta[4])
  p = theta[5]
  n = length(data)
  l=0
  for(i in 1:n){
    l = l + log(p*dgamma(data[i],shape= mu1,scale=sigma1)
                +(1-p)*dgamma(data[i],shape= mu2,scale=sigma2))
  }
  return(-l)
}
theta_initial1=c(shape1,scale1,shape2,scale2,p)
NegLogLikeMix2(theta_initial1,waiting)

fit1 = optim(theta_initial1
             ,NegLogLikeMix2
             ,data=waiting
             ,control = list(maxit=1500)
             ,lower=c(0,0,0,0,0),
             upper=c(Inf,Inf,Inf,Inf,1),
             method="L-BF65-B")

theta_hat1 = fit1$par

mu1_hat1 = theta_hat1[1]
sigma1_hat1 = theta_hat1[2]
mu2_hat1 = theta_hat1[3]
sigma2_hat1 =(theta_hat1[4])
p_hat1 = theta_hat1[5]

d_mle2 = p_hat1*dgamma(waiting,shape= mu1_hat1,scale=sigma1_hat1)+(1-p_hat1)*dgamma(waiting,shape=mu2_hat1,scale=sigma2_hat1)
lines(waiting,d_mle2,lwd=3,col='purple')

#d_mle2 = p_hat1*dgamma(waiting,shape= mu1_hat1,scale=sigma1_hat1)+(1-p_hat1)*dgamma(waiting,shape=mu2_hat1,scale=sigma2_hat1)
#lines(waiting,d_mle2,lwd=3,col='purple')

theta_hat1

#model3

hist(eruptions)
hist(waiting,probability = T)

m1=mean(a[a<65])
v1=var((a[a<65]))
sig1=log((v1/m1^2)+1)
mu1=log(m1)-sig1/2

m2=mean(a[a>=65])
v2=var((a[a>=65]))
sig2=log((v2/m2^2)+1)
mu2=log(m2)-sig2/2
d = p*(dlnorm(waiting,meanlog=mu1,sdlog=sig1))+(1-p)*(dlnorm(waiting,meanlog=mu2,sdlog=sig2))
lines(waiting,d,lwd=2,col='blue')




NegLogLikeMix3 <- function(theta,data){
  muu1 = theta[1]
  sigma1 = (theta[2])
  muu2 = theta[3]
  sigma2 = (theta[4])
  p = exp(theta[5])/(1+exp(theta[5]))
  n = length(data)
  l=0
  for(i in 1:n){
    l = l + log(p*(dlnorm(data[i],meanlog= muu1,sdlog=sigma1))
                +(1-p)*(dlnorm(data[i],meanlog=muu2,sdlog=sigma2)))
  }
  return(-l)
}
theta_initial3=c(mu1,sig1,mu2,sig2,p)
NegLogLikeMix3(theta_initial3,waiting)

fit3 = optim(theta_initial3
             ,NegLogLikeMix3
             ,data=waiting
             ,control = list(maxit=1500))

theta_hat3 = fit3$par
mu1_hat3 = theta_hat3[1]
sigma1_hat3 = theta_hat3[2]
mu2_hat3= theta_hat3[3]
sigma2_hat3 = (theta_hat3[4])
p_hat3 = exp(theta_hat3[5])/(1+exp(theta_hat3[5]))


d_mle3= p_hat3*(dlnorm(waiting,meanlog= mu1_hat3,sdlog=sigma1_hat3))+(1-p_hat3)*(dlnorm(waiting,meanlog=mu2_hat3,sdlog=sigma2_hat3))
lines(waiting,d_mle3,lwd=3,col='purple')

aic1=2*NegLogLikeMix(theta_initial,waiting)+2*4
aic1

aic2=2*NegLogLikeMix2(theta_initial1,waiting)+2*4
aic2

aic3=2*NegLogLikeMix3(theta_initial3,waiting)+2*4
aic3

print('Bases on AIC of the three models,model 2 has the best fit since it has the lowest AIC among the three')
#P(60<waiting<70)according to model 2


