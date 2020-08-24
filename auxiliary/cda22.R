rff.cda2<-randomForest(formula=cda2.data$cda2~., data=cda2.data[-1],proximity=T, ntree=1000, importance=T, mtry=4)
oob.error.data<-data.frame( 
    Trees=rep(1:nrow(rff.cda2$err.rate), times=3),
    Type=rep(c("OOB", "does not drink", "drinks"), each=nrow(rff.cda2$err.rate)),
    Error=c(rff.cda2$err.rate[,"OOB"],
           rff.cda2$err.rate[,"does not drink"],
           rff.cda2$err.rate[,'drinks']))
