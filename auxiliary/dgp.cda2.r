################################ Currently drinks #############################################
funcrange <- function(N, mean, sd, lwr, upr, nnorm) {
  samp <- rnorm(nnorm, mean, sd)
  samp <- samp[samp >= lwr & samp <= upr]
  if (length(samp) >= N) {
    return(sample(samp, N))
  }  
  stop(simpleError("Try increasing nnorm."))
}

dgp.cda2 <- function(n){
    
########### Socioeconomic characteristics #######################

    vec<-c(rep(0,n*0.630035),rep(1, n*0.37))
    ffs<-sample(vec)
    lfi<- rnorm(n,mean=8.15, sd=0.3)
    perf<-abs(rnorm(n, mean=1.36, sd=1.32))
    age<-rnorm(n, mean=14.3, sd=0.79)
    retedu<-abs(rnorm(n, mean=272, sd=616))

################### outcomes round 2 ###########################

    vec1<-c(rep(0,n*0.42019),rep(1,n*0.58))
    returntosch<-sample(vec1)
    vec2<-c(rep(0,n*0.70015),rep(1,n*0.3))
    cwork2<-sample(vec2)
    hpw<-abs(rnorm(n, mean=4.1, sd=8.9))
    hpw2<-hpw*cwork2
    epw<-abs(rnorm(n, mean=67.7, sd=172.6))
    epw2<-epw*cwork2
    dipw2<-abs(rnorm(n, mean=56.4, sd=71.1))
    vectreat<-c(rep(0, n*0.49), rep(1, n*0.5102))
    treat<-sample(vectreat)
    frpsmoke2<-funcrange(N=n, mean=2.68, sd=1.13, lwr=1, upr=5, nnorm=10000)
    frpdrink2<-funcrange(N=n, mean=3.32, sd=1.27, lwr=1, upr=5, nnorm=10000) 
    
    X1<-cbind(ffs, lfi, perf, age, returntosch, cwork2, hpw2, epw2, dipw2, treat,frpsmoke2, frpdrink2)
    
    beta<-c(-0.8, -1.5, 0.02, 0.37, -2.5, 0.07, 0.1, 0.1, 0.1, -1.5, 0.02, 0.7)
    eps<-rnorm(n, mean=0, sd=1)
    cda.2<-(exp(X1%*%beta+eps))/(1+exp(X1%*%beta+eps))
    cda2<-ifelse(cda.2<0.5, 'does not drink', 'drinks')
    
 return (data.frame(cda2,X1))
}

n=2011
cda2.data<-dgp.cda2(n)

cda2.data$frpsmoke2<-round(cda2.data$frpsmoke2)
cda2.data$frpdrink2<-round(cda2.data$frpdrink2)
cda2.data$frpsmoke2<-as.factor(cda2.data$frpsmoke2)
cda2.data$frpdrink2<-as.factor(cda2.data$frpdrink2)

cda2.data$ffs<-as.factor(cda2.data$ffs)
cda2.data$returntosch<-as.factor(cda2.data$returntosch)
cda2.data$cwork2<-as.factor(cda2.data$cwork2)
cda2.data$treat<-as.factor(cda2.data$treat)




