#### round 3
    
funcrange <- function(N, mean, sd, lwr, upr, nnorm) {
  samp <- rnorm(nnorm, mean, sd)
  samp <- samp[samp >= lwr & samp <= upr]
  if (length(samp) >= N) {
    return(sample(samp, N))
  }  
  stop(simpleError("Try increasing nnorm."))
}

dgp.csmoke3<-function(n){    
   
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
    
    Xsm3<-cbind(ffs, lfi, perf, age, treat, finhigh, yearssch, cwork3, hpw3, epw3, dipw3, patient3, frpsmoke3, 
              frpdrink3, risk, smkbad, drinkbad )
    
    
    
    b1<-c(-0.5, -0.4, -0.5, 0.18, -0.47, -0.21, -0.51, 0.011, 0.01, 0.02, 0.01, -0.35, 0.47, 0.4, 0.33, -0.92, -0.5)
    eps<-rnorm(n, mean=0, sd=1)
    csmoke.3<-(exp(Xsm3%*%b1+eps))/(1+exp(Xsm3%*%b1+eps))
    csmoke3<-ifelse(csmoke.3<0.5, 'does not smoke', 'smokes')
    
    
 return (data.frame(csmoke3,Xsm3))
}

n=2011
csmoke3.data<-dgp.csmoke3(n)

csmoke3.data$frpsmoke3<-round(csmoke3.data$frpsmoke3)
csmoke3.data$frpdrink3<-round(csmoke3.data$frpdrink3)
csmoke3.data$frpsmoke3<-as.factor(csmoke3.data$frpsmoke3)
csmoke3.data$frpdrink3<-as.factor(csmoke3.data$frpdrink3)
csmoke3.data$patient3<-round(csmoke3.data$patient3)
csmoke3.data$risk<-round(csmoke3.data$risk)
csmoke3.data$patient3<-as.factor(csmoke3.data$patient3)
csmoke3.data$risk<-as.factor(csmoke3.data$risk)
csmoke3.data$smkbad<-round(csmoke3.data$smkbad)
csmoke3.data$drinkbad<-round(csmoke3.data$drinkbad)
csmoke3.data$smkbad<-as.factor(csmoke3.data$smkbad)
csmoke3.data$drinkbad<-as.factor(csmoke3.data$drinkbad)

csmoke3.data$ffs<-as.factor(csmoke3.data$ffs)
csmoke3.data$treat<-as.factor(csmoke3.data$treat)
csmoke3.data$finhigh<-as.factor(csmoke3.data$finhigh)
csmoke3.data$cwork3<-as.factor(csmoke3.data$cwork3)





