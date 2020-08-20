rf.csmoke2<-randomForest(formula=Y2.train$csmoke2~., data=X2.train,proximity=T, importance=T)
#rf.cda2.pred<-predict(rf.cda2,round2.x.test )
#rf.cda2$err.rate

#To see if 500 trees is enough for optimal classification, we can plot the error rates
oob.error.data<-data.frame( 
    Trees=rep(1:nrow(rf.csmoke2$err.rate), times=3),
    Type=rep(c("OOB", "does not smoke", "smokes"), each=nrow(rf.csmoke2$err.rate)),
    Error=c(rf.csmoke2$err.rate[,"OOB"],
           rf.csmoke2$err.rate[,"does not smoke"],
           rf.csmoke2$err.rate[,'smokes']))