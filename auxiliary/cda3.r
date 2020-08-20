rf.cda3<-randomForest(formula=Y3.train$cda3~., data=X3.train,proximity=T, importance=T, mtry=3)
#ntree=500,mtry=3,

#rf.cda2.pred<-predict(rf.cda2,round2.x.test )
#rf.cda2$err.rate

#To see if 500 trees is enough for optimal classification, we can plot the error rates
oob.error.data31<-data.frame( 
    Trees=rep(1:nrow(rf.cda3$err.rate), times=3),
    Type=rep(c("OOB", "does not drink", "drinks"), each=nrow(rf.cda3$err.rate)),
    Error=c(rf.cda3$err.rate[,"OOB"],
           rf.cda3$err.rate[,"does not drink"],
           rf.cda3$err.rate[,'drinks']))