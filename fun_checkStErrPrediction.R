checkStErrPrediction<- function(inputDataset){
  
  curData = inputDataset[,c("BestAtt5b","mseReal","mseAll", "mseb0")]
  curData[curData ==-3]= NA
  curData = na.exclude(curData)
  
  #replace attendance with percentage
    curData$attend2 = NA
    lookup = data.frame(id = c(1,2,3,4,5), percent = c(33, 60, 75, 85, 100))
    for (i in 1:nrow(curData)){
    curData$attend2[i] = lookup[(1:dim(lookup)[1])[lookup[,1]==curData$BestAtt5b[i]],2]
    }
        allModel = lm(mseAll~attend2, data = curData)
        realModel = lm(mseReal~attend2, data = curData)
        b0Model = lm(mseb0 ~ attend2, data = curData)
  
  #output
    outMatrix = data.frame(matrix(nrow = 3, ncol = 6))
    colnames(outMatrix)= c("error metric", "intercept coeff", "error coeff", "intercept p", "coeff p", "Adj R2")
    #outMatrix$error_metric = c("All", "Real", "b0")
    outMatrix[1,]= c("All", summary(allModel)$coefficients[1,1], summary(allModel)$coefficients[2,1], summary(allModel)$coefficients[1,4], summary(allModel)$coefficients[2,4], summary(allModel)$adj.r.squared)
    outMatrix[2,]= c("real", summary(realModel)$coefficients[1,1], summary(realModel)$coefficients[2,1], summary(realModel)$coefficients[1,4], summary(realModel)$coefficients[2,4], summary(realModel)$adj.r.squared)
    outMatrix[3,]= c("b0", summary(b0Model)$coefficients[1,1], summary(b0Model)$coefficients[2,1], summary(b0Model)$coefficients[1,4], summary(b0Model)$coefficients[2,4], summary(b0Model)$adj.r.squared)
  
  return(outMatrix)
}