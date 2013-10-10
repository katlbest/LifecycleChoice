adjustIncomes <- function(ageVectList, incomeVectList, enrollVectList){
  outMatrix = data.frame(matrix(ncol = length(ENROLL_DATA), nrow =8))
  for (i in 1:nrow(ENROLL_DATA)){
    curData = data.frame(age = ageVectList[[i]], income = incomeVectList[[i]], enroll = enrollVectList[[i]])
    curData[curData == -3] <- NA
    curData[curData == -4] <- NA 
    curData<- na.exclude(curData)
    if (dim(curData)[1]>2){
      numObs = length(na.exclude(curData$age))
      firstDataYear = ageVectList[[i]][1]
      lastDataYear = ageVectList[[i]][length(ageVectList[[i]])]
      outMatrix[,i] <- c(rep(-3, firstDataYear-19), incomeVectList[[i]])
    }
    else{
      outMatrix[,i] <- rep(-3,8)
    }
  }  
  colnames(outMatrix)=ENROLL_DATA$PUBID_1997
  return(outMatrix)
}