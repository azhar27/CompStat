funcrange <- function(N, mean, sd, lwr, upr, nnorm) {
  samp <- rnorm(nnorm, mean, sd)
  samp <- samp[samp >= lwr & samp <= upr]
  if (length(samp) >= N) {
    return(sample(samp, N))
  }  
  stop(simpleError("Try increasing nnorm."))
}

############################## Currently smokes ###################################################
dgp.csmoke2<- function(n){  ###### round 2
    vec<-c(rep(0,n*0.630035),rep(1, n*0.37))
    ffs<-sample(vec)
    lfi<- rnorm(n,mean=8.15, sd=0.3)
    perf<-abs(rnorm(n, mean=1.36, sd=1.32))
    age<-rnorm(n, mean=14.3, sd=0.79)
    retedu<-abs(rnorm(n, mean=272, sd=616))
    
    vec1<-c(rep(0,n*0.42019),rep(1,n*0.58))
    returntosch<-sample(vec1)
    vec2<-c(rep(0,n*0.70015),rep(1,n*0.3))
    cwork2<-sample(vec2)
    hpw<-rnorm(n, mean=4.1, sd=8.9)
    hpw2<-hpw*cwork2
    epw<-abs(rnorm(n, mean=67.7, sd=172.6))
    epw2<-epw*cwork2
    dipw2<-abs(rnorm(n, mean=56.4, sd=71.1))
    vectreat<-c(rep(0, n*0.49), rep(1, n*0.5102))
    treat<-sample(vectreat)
    frpsmoke2<-funcrange(N=n, mean=2.68, sd=1.13, lwr=1, upr=5, nnorm=10000)
    frpdrink2<-funcrange(N=n, mean=3.32, sd=1.27, lwr=1, upr=5, nnorm=10000) 
    
    X.sm<-cbind(ffs, lfi, perf, age, returntosch, cwork2, hpw2, epw2, dipw2, treat,frpsmoke2, frpdrink2)
    
    beta<-c(-12, -15, -13.6, 0.25, -25.5, 0.3, 0.4, 0.5, 0.5, -24.5, 1.2, 0.02)
    eps<-rnorm(n, mean=0, sd=1)
    csmoke.2<-(exp(X.sm%*%beta+eps))/(1+exp(X.sm%*%beta+eps))
    csmoke2<-ifelse(csmoke.2<0.5, 'does not smoke', 'smokes')
    
    return (data.frame(csmoke2,X.sm))
}

n=2011



csmoke2.test<-dgp.csmoke2(n)

csmoke2.test$frpsmoke2<-round(csmoke2.test$frpsmoke2)
csmoke2.test$frpdrink2<-round(csmoke2.test$frpdrink2)
csmoke2.test$frpsmoke2<-as.factor(csmoke2.test$frpsmoke2)
csmoke2.test$frpdrink2<-as.factor(csmoke2.test$frpdrink2)

csmoke2.test$ffs<-as.factor(csmoke2.test$ffs)
csmoke2.test$returntosch<-as.factor(csmoke2.test$returntosch)
csmoke2.test$cwork2<-as.factor(csmoke2.test$cwork2)
csmoke2.test$treat<-as.factor(csmoke2.test$treat)
