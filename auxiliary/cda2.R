rf.cda2<-randomForest(Y2.train$cda2~., data=X2.train,proximity=T, importance=T, metric='Accuracy', nodes=T)

#rf.cda2.pred<-predict(rf.cda2,round2.x.test )
#rf.cda2$err.rate

#To see if 500 trees is enough for optimal classification, we can plot the error rates
oob.error.data<-data.frame( 
    Trees=rep(1:nrow(rf.cda2$err.rate), times=3),
    Type=rep(c("OOB", "does not drink", "drinks"), each=nrow(rf.cda2$err.rate)),
    Error=c(rf.cda2$err.rate[,"OOB"],
           rf.cda2$err.rate[,"does not drink"],
           rf.cda2$err.rate[,'drinks']))
