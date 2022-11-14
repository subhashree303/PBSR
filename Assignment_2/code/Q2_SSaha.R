MLE=function(n1){
  #1)function to compute mle
  MyMLE=function(data1){
    fit = optimize(data1,c(0,50),maximum = T)
    
    return(log(fit$maximum))}
  

  
  loglikegamma=function(shape){
    #2i)simulating from gamma distribution
    data2=rgamma(n=n1, shape=1.5, scale = 2.2) 
    
    l=sum(dgamma(data2,shape,scale=2.2,log=T))
    return(l)
  }
  
  v<-c((MyMLE(loglikegamma)))
  #2ii and iii)
  for(x in 1:1000){
    v<-append(v,(MyMLE(loglikegamma)))
    
  }
return(v) 
}
  #2iv)
  hist(MLE(20), col="lightblue",xlab='estimated MLE of θ',main=' Histogram of the estimated MLEs of θ when n=20'))
  #2v)
  abline(v=log(1.5), col="red", lwd=3, lty=2)
  #2vi)
  quantile(MLE(20),probs=c(0.025,0.975))
  
  #3
  hist(MLE(40), col="lightblue",xlab='estimated MLE of θ',main=' Histogram of the estimated MLEs of θ when n=40'))
  abline(v=log(1.5), col="red", lwd=3, lty=2)
  
  quantile(MLE(40),probs=c(0.025,0.975))

  #4
  hist(MLE(100), col="lightblue",xlab='estimated MLE of θ',main=' Histogram of the estimated MLEs of θ when n=100'))
  abline(v=log(1.5), col="red", lwd=3, lty=2)
  
  quantile(MLE(100),probs=c(0.025,0.975))
  
  print(' Therefore we can see that the gap between 2.5 and 97.5-percentile points are shrinking as sample size n is increasing ')
  
  
  
  
