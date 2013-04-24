getStError<- function(inData, inb0){

  #set up dataset
    catList = levels(as.factor(ENROLL_DATA$cat))
    transData = data.frame(t(inData))
    colnames(transData)= c(19:100)
    transData$cat = ENROLL_DATA$cat
    inb0[is.na(inb0)]=-3
    inb0 = unlist(inb0)
    transData$b0 = inb0
    #remove those with out of range values
      b0Min = -500000 #upper limit of about 140K top salary
      b0Max = -100 #lower limit of about 12K top salary
      transData = transData[transData$b0 >= -500000 & transData$b0 <= -100,]
    transData[transData == -3] = NA
    transData[transData == -4] = NA
    
  #output vector
    stdVect = rep(NA, length(catList)) #stores standard deviations
    nVect = rep(NA, length(catList)) #stores sample sizes
    
  #calc standard errors
  for (i in 1:length(catList)){
    curData = transData[transData$cat == catList[i],]
    if (nrow(curData) > 1){ #if there are at least two people in this category
      avgVect = rep(NA, 82)
      outData = data.frame(matrix(ncol = 82, nrow = nrow(curData)))
      colnames(outData) = colnames(curData)[1:82]
      for (j in 1:length(avgVect)){
        avgVect[j]= mean(na.exclude(curData[,j]))
        for (k in 1:nrow(curData)){
          if (is.na(curData[k,j])){
            outData[k,j] = NA
          } else {
            outData[k,j] = curData[k,j]-avgVect[j]
          }
        }
      }
      outData = outData[,1:82]
      allObs = na.exclude(unlist(outData))
      if (length(allObs) > 5){ #must have at least 5 data points to determine st. error
        stdVect[i]=sd(allObs)
        nVect[i]= length(allObs)
      } else{
        stdVect[i]=NA
        nVect[i]=NA
      }  
    } else{
      stdVect[i]=NA
      nVect[i]=NA
    }
  }
  outList = list(stdVect, nVect)
  return(outList)
}