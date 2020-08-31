rff.cda3<-randomForest(formula=cda3.data$cda3~., data=cda3.data[-1],proximity=T, ntree=1000, importance=T, mtry=4)
oob.error.data<-data.frame( 
    Trees=rep(1:nrow(rff.cda3$err.rate), times=3),
    Type=rep(c("OOB", "does not drink", "drinks"), each=nrow(rff.cda3$err.rate)),
    Error=c(rff.cda3$err.rate[,"OOB"],
           rff.cda3$err.rate[,"does not drink"],
           rff.cda3$err.rate[,'drinks']))
