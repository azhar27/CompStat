rf.drevd2<-randomForest(formula=Y2.train$drevd2~., data=X2.train,proximity=T, importance=T)
#ntree=500,mtry=3,

#rf.cda2.pred<-predict(rf.cda2,round2.x.test )
#rf.cda2$err.rate

#To see if 500 trees is enough for optimal classification, we can plot the error rates
oob.error.data<-data.frame( 
    Trees=rep(1:nrow(rf.drevd2$err.rate), times=3),
    Type=rep(c("OOB", "does not drink every day", "drinks every day"), each=nrow(rf.drevd2$err.rate)),
    Error=c(rf.drevd2$err.rate[,"OOB"],
           rf.drevd2$err.rate[,"does not drink every day"],
           rf.drevd2$err.rate[,'drinks every day']))