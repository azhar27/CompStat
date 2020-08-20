  
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

    vec<-c(rep('did not finish',n*0.630035),rep('finished', n*0.37))
    ffs<-sample(vec)
    lfi<- rnorm(n,mean=8.15, sd=0.3)
    perf<-rnorm(n, mean=1.36, sd=1.32)
    age<-rnorm(n, mean=14.3, sd=0.79)
    retedu<-abs(rnorm(n, mean=272, sd=616))

################### outcomes round 2 ###########################

    vec1<-c(rep('did not return',n*0.42019),rep('returned',n*0.58))
    returntosch<-sample(vec1)
    vec2<-c(rep(0,n*0.70015),rep(1,n*0.3))
    cwork2<-sample(vec2)
    hpw<-rnorm(n, mean=4.1, sd=8.9)
    hpw2<-hpw*cwork2
    epw<-abs(rnorm(n, mean=67.7, sd=172.6))
    epw2<-epw*cwork2
    dipw2<-abs(rnorm(n, mean=56.4, sd=71.1))
    vectreat<-c(rep('treated', n*0.49), rep('control', n*0.5102))
    treat<-sample(vectreat)
    frpsmoke2<-funcrange(N=n, mean=2.68, sd=1.13, lwr=1, upr=5, nnorm=10000)
    frpdrink2<-funcrange(N=n, mean=3.32, sd=1.27, lwr=1, upr=5, nnorm=10000) 
    
    X2<-data.frame(ffs, lfi, perf, age, returntosch, cwork2, hpw2, epw2, retedu, dipw2, treat,frpsmoke2, frpdrink2)
    
 #return (data.frame(X2))}

#dgp3 <- function(n){


################# outcomes round 3 #############################

    vec<-c(rep('did not finish',n*0.630035),rep('finished', n*0.37))
    ffs<-sample(vec)
    lfi<- rnorm(n,mean=8.15, sd=0.3)
    perf<-rnorm(n, mean=1.36, sd=1.32)
    age<-rnorm(n, mean=14.3, sd=0.79)
    
    vectreat<-c(rep('treated', n*0.49), rep('control', n*0.5102))
    treat<-sample(vectreat)
    vec7<-c(rep('did not finish',n*0.68),rep('finished',n*0.32024))
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
    vec3<-c(rep('does not smoke',n*0.9502735),rep('smokes',n*0.05))
    csmoke2<-sample(vec3)

    vec4<-c(rep('does not drink',n*0.39),rep('drinks',n*0.61015))
    cda2<-sample(vec4)

    vec5<-c(rep('does not drink once a week',n*0.810045),rep('drinks once a week',n*0.19))
    droncew2<-sample(vec5)

    vec6<-c(rep('does not drink every day', n*0.9801094),rep('drinks every day',n*0.02))
    drevd2<-sample(vec6)

    drfr2<-funcrange(N=n, mean=1.96, sd=0.7, lwr=1, upr=4, nnorm=10000)
    Y2<-data.frame(csmoke2, cda2, droncew2, drevd2, drfr2)
#    return(data.frame(Y2))}

#### Round 3

#dgp5 <- function(n){
    vec9<-c(rep('does not smoke',n*0.8703),rep('smokes',n*0.13))
    csmoke3<-sample(vec9)

    vec10<-c(rep('does not drink',n*0.27002),rep('drinks',n*0.73))
    cda3<-sample(vec10)

    vec11<-c(rep('does not drink once a week',n*0.54003),rep('drinks once a week',n*0.46))
    droncew3<-sample(vec11)

    vec12<-c(rep('does not drink every day',n*0.87022),rep('drinks every day',n*0.13))
    drevd3<-sample(vec12)

    drfr3<-funcrange(N=n, mean=2.37, sd=0.95, lwr=1, upr=4, nnorm=10000)

    Y3<-data.frame(csmoke3, cda3, droncew3, drevd3, drfr3)
#    return(data.frame(Y3))}


#converting categorical to numeric
X2$cwork2<-as.factor(X2$cwork2)
X2$lfi<-as.numeric(X2$lfi)
X2$perf<-as.numeric(X2$perf)
X2$age<-as.numeric(X2$age)
X2$retedu<-as.numeric(X2$retedu)
X2$hpw2<-as.numeric(X2$hpw2)
X2$epw2<-as.numeric(X2$epw2)
X2$dipw2<-as.numeric(X2$dipw2)

#X2$frpsmoke2<-as.numeric(X2$frpsmoke2)
#X2$frpdrink2<-as.numeric(X2$frpdrink2)
X2$frpsmoke2<-round(X2$frpsmoke2)
X2$frpdrink2<-round(X2$frpdrink2)
X2$frpsmoke2<-as.factor(X2$frpsmoke2)
X2$frpdrink2<-as.factor(X2$frpdrink2)

#split into test and train data
X2.train<-X2[1:1408,]
X2.test<-X2[1409:2011,]

#converting categorical to numeric
X3$cwork3<-as.factor(X3$cwork3)
X3$lfi<-as.numeric(X3$lfi)
X3$perf<-as.numeric(X3$perf)
X3$age<-as.numeric(X3$age)
X3$hpw3<-as.numeric(X3$hpw3)
X3$epw3<-as.numeric(X3$epw3)
X3$dipw3<-as.numeric(X3$dipw3)
#X3$frpsmoke3<-as.numeric(X3$frpsmoke3)
#X3$frpdrink3<-as.numeric(X3$frpdrink3)
#X3$patient3<-as.numeric(X3$patient3)
#X3$risk<-as.numeric(X3$risk)
X3$frpsmoke3<-round(X3$frpsmoke3)
X3$frpdrink3<-round(X3$frpdrink3)
X3$patient3<-round(X3$patient3)
X3$risk<-round(X3$risk)
X3$frpsmoke3<-as.factor(X3$frpsmoke3)
X3$frpdrink3<-as.factor(X3$frpdrink3)
X3$patient3<-as.factor(X3$patient3)
X3$risk<-as.factor(X3$risk)
X3$yearssch<-as.numeric(X3$yearssch)

#split into test and train data
X3.train<-X3[1:1408,]
X3.test<-X3[1409:2011,]

#Y2$drfr2<-as.numeric(Y2$drfr2)
Y2$drfr2<-round(Y2$drfr2)
Y2$drfr2<-as.factor(Y2$drfr2)

Y2.train<-Y2[1:1408,]
Y2.test<-Y2[1409:2011,]

#Y3$drfr3<-as.numeric(Y3$drfr3)
#Y3$smkbad<-as.numeric(Y3$smkbad)
#Y3$drinkbad<-as.numeric(Y3$drinkbad)
Y3$drfr3<-round(Y3$drfr3)
X3$smkbad<-round(X3$smkbad)
X3$drinkbad<-round(X3$drinkbad)
Y3$drfr3<-as.factor(Y3$drfr3)
X3$smkbad<-as.factor(X3$smkbad)
X3$drinkbad<-as.factor(X3$drinkbad)

Y3.train<-Y3[1:1408,]
Y3.test<-Y3[1409:2011,]
