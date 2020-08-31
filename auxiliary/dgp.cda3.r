funcrange <- function(N, mean, sd, lwr, upr, nnorm) {
  samp <- rnorm(nnorm, mean, sd)
  samp <- samp[samp >= lwr & samp <= upr]
  if (length(samp) >= N) {
    return(sample(samp, N))
  }  
  stop(simpleError("Try increasing nnorm."))
}

dgp.cda3<-function(n){


################# outcomes round 3 #############################

    vec<-c(rep(0,n*0.630035),rep(1, n*0.37))
    ffs<-sample(vec)
    lfi<- rnorm(n,mean=8.15, sd=0.3)
    perf<-abs(rnorm(n, mean=1.36, sd=1.32))
    age<-rnorm(n, mean=17.3, sd=0.79)
    
    vectreat<-c(rep(0, n*0.49), rep(1, n*0.5102))
    treat<-sample(vectreat)
    vec7<-c(rep(0,n*0.68),rep(1,n*0.32024))
    finhigh<-sample(vec7)
    yearssch<-rnorm(n, mean=9.86, sd=1.77)
    vec8<-c(rep(0,n*0.64),rep(1,n*0.36002))
    cwork3<-sample(vec8)
    hpww<-abs(rnorm(n, mean=6, sd=11.6))
    hpw3<-hpww*cwork3
    epww<-abs(rnorm(n, mean=250.6, sd=833.4))
    epw3<-epww*cwork3
    dipw3<-abs(rnorm(n, mean=121.2, sd=391.6))
    patient3<-funcrange(N=n, mean=2.06, sd=1.14, lwr=1, upr=5, nnorm=10000)
    frpsmoke3<-funcrange(N=n, mean=3.31, sd=1.24, lwr=1, upr=5, nnorm=10000)
    frpdrink3<-funcrange(N=n, mean=4.48, sd=1, lwr=1, upr=5, nnorm=10000)
    risk<-funcrange(N=n, mean=1.98, sd=1.2, lwr=1, upr=5, nnorm=10000)
    smkbad<-funcrange(N=n, mean=3.67, sd=0.68, lwr=1, upr=5, nnorm=10000)
    drinkbad<-funcrange(N=n, mean=2.49, sd=0.63, lwr=1, upr=5, nnorm=10000) 
    
    X3<-cbind(ffs, lfi, perf, age, treat, finhigh, yearssch, cwork3, hpw3, epw3, dipw3, patient3, frpsmoke3, 
              frpdrink3, risk, smkbad, drinkbad )
    
    
    b<-c(-0.8, -0.9, -0.4, 0.37, -1.5, -1.1, -2.1, 0.11, 0.1, 0.12, 0.1, -0.05, 0.7, 1.7, 0.4, -0.15, -2.5)
    eps<-rnorm(n, mean=0, sd=1)
    cda.3<-(exp(X3%*%b+eps))/(1+exp(X3%*%b+eps))
    cda3<-ifelse(cda.3<0.5, 'does not drink', 'drinks')
    
    
 return (data.frame(cda3,X3))
}

n=2011
cda3.data<-dgp.cda3(n)

cda3.data$frpsmoke3<-round(cda3.data$frpsmoke3)
cda3.data$frpdrink3<-round(cda3.data$frpdrink3)
cda3.data$frpsmoke3<-as.factor(cda3.data$frpsmoke3)
cda3.data$frpdrink3<-as.factor(cda3.data$frpdrink3)
cda3.data$patient3<-round(cda3.data$patient3)
cda3.data$risk<-round(cda3.data$risk)
cda3.data$patient3<-as.factor(cda3.data$patient3)
cda3.data$risk<-as.factor(cda3.data$risk)
cda3.data$smkbad<-round(cda3.data$smkbad)
cda3.data$drinkbad<-round(cda3.data$drinkbad)
cda3.data$smkbad<-as.factor(cda3.data$smkbad)
cda3.data$drinkbad<-as.factor(cda3.data$drinkbad)

cda3.data$ffs<-as.factor(cda3.data$ffs)
cda3.data$finhigh<-as.factor(cda3.data$finhigh)
cda3.data$cwork3<-as.factor(cda3.data$cwork3)
cda3.data$treat<-as.factor(cda3.data$treat)






