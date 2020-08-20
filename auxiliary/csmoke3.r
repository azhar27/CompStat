rf.csmoke3<-randomForest(formula=Y3.train$csmoke3~., data=X3.train,proximity=T, importance=T, mtry=3)
#ntree=500,mtry=3,

#rf.cda2.pred<-predict(rf.cda2,round2.x.test )
#rf.cda2$err.rate

#To see if 500 trees is enough for optimal classification, we can plot the error rates
oob.error.data32<-data.frame( 
    Trees=rep(1:nrow(rf.csmoke3$err.rate), times=3),
    Type=rep(c("OOB", "does not smoke", "smokes"), each=nrow(rf.csmoke3$err.rate)),
    Error=c(rf.csmoke3$err.rate[,"OOB"],
           rf.csmoke3$err.rate[,"does not smoke"],
           rf.csmoke3$err.rate[,'smokes']))