#dgp2 <- function(n){
    
    funcrange <- function(N, mean, sd, lwr, upr, nnorm) {
  samp <- rnorm(nnorm, mean, sd)
  samp <- samp[samp >= lwr & samp <= upr]
  if (length(samp) >= N) {
    return(sample(samp, N))
  }  
  stop(simpleError("Try increasing nnorm."))
}
n=2011
########### Socioeconomic characteristics #######################

    vec<-c(rep(0,n*0.630035),rep(1, n*0.37))
    ffs<-sample(vec)
    lfi<- rnorm(n,mean=8.15, sd=0.3)
    perf<-rnorm(n, mean=1.36, sd=1.32)
    age<-rnorm(n, mean=14.3, sd=0.79)
    retedu<-abs(rnorm(n, mean=272, sd=616))

################### outcomes round 2 ###########################

    vec1<-c(rep(0,n*0.42019),rep(1,n*0.58))
    returntosch<-sample(vec1)
    vec2<-c(rep(0,n*0.70015),rep(1,n*0.3))
    cwork2<-sample(vec2)
    hpw<-rnorm(n, mean=4.1, sd=8.9)
    hpw2<-hpw*cwork2
    epw<-abs(rnorm(n, mean=67.7, sd=172.6))
    epw2<-epw*cwork2
    dipw2<-abs(rnorm(n, mean=56.4, sd=71.1))
    vectreat<-c(rep(1, n*0.49), rep(0, n*0.5102))
    treat<-sample(vectreat)
    frpsmoke2<-funcrange(N=n, mean=2.68, sd=1.13, lwr=1, upr=5, nnorm=10000)
    frpdrink2<-funcrange(N=n, mean=3.32, sd=1.27, lwr=1, upr=5, nnorm=10000) 
    
    X2<-data.frame(ffs, lfi, perf, age, returntosch, cwork2, hpw2, epw2, retedu, dipw2, treat,frpsmoke2, frpdrink2)
    
 #return (data.frame(X2))}

#dgp3 <- function(n){


################# outcomes round 3 #############################

    vec<-c(rep(0,n*0.630035),rep(1, n*0.37))
    ffs<-sample(vec)
    lfi<- rnorm(n,mean=8.15, sd=0.3)
    perf<-rnorm(n, mean=1.36, sd=1.32)
    age<-rnorm(n, mean=14.3, sd=0.79)
    
    vectreat<-c(rep(1, n*0.49), rep(0, n*0.5102))
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
    
    X3<-data.frame(ffs, lfi, perf, age, treat, finhigh, yearssch, cwork3, hpw3, epw3, dipw3, patient3, frpsmoke3, 
              frpdrink3, risk, smkbad, drinkbad )
 #  return (data.frame(X3))}

#### Round 2

#dgp4 <- function(n){
    vec3<-c(rep(0,n*0.9502735),rep(1,n*0.05))
    csmoke2<-sample(vec3)

    vec4<-c(rep(0,n*0.39),rep(1,n*0.61015))
    cda2<-sample(vec4)

    vec5<-c(rep(0,n*0.810045),rep(1,n*0.19))
    droncew2<-sample(vec5)

    vec6<-c(rep(0, n*0.9801094),rep(1,n*0.02))
    drevd2<-sample(vec6)

    drfr2<-funcrange(N=n, mean=1.96, sd=0.7, lwr=1, upr=4, nnorm=10000)
    Y2<-data.frame(csmoke2, cda2, droncew2, drevd2, drfr2)
#    return(data.frame(Y2))}

#### Round 3

#dgp5 <- function(n){
    vec9<-c(rep(0,n*0.8703),rep(1,n*0.13))
    csmoke3<-sample(vec9)

    vec10<-c(rep(0,n*0.27002),rep(1,n*0.73))
    cda3<-sample(vec10)

    vec11<-c(rep(0,n*0.54003),rep(1,n*0.46))
    droncew3<-sample(vec11)

    vec12<-c(rep(0,n*0.87022),rep(1,n*0.13))
    drevd3<-sample(vec12)

    drfr3<-funcrange(N=n, mean=2.37, sd=0.95, lwr=1, upr=4, nnorm=10000)

    Y3<-data.frame(csmoke3, cda3, droncew3, drevd3, drfr3)
#    return(data.frame(Y3))}
