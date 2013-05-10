getStErrorbyAdmit<- function(inData, inb0){
  
  #set up dataset
  catList = levels(as.factor(ENROLL_DATA$BestAd5b))
  transData = data.frame(t(inData))
  colnames(transData)= c(19:100)
  transData$admit = ENROLL_DATA$BestAd5b
  transData$cat = ENROLL_DATA$cat
  transData$attend = ENROLL_DATA$BestAtt5b
  transData$PUBID_1997= ENROLL_DATA$PUBID_1997
  inb0[is.na(inb0)]=-3
  inb0 = unlist(inb0)
  transData$b0 = inb0
  #remove those with out of range values
  b0Min = -500000 #upper limit of about 140K top salary
  b0Max = -100 #lower limit of about 12K top salary
  transData = transData[transData$b0 >= -500000 & transData$b0 <= -100,]
  transData = transData[transData$admit != 7,]
  transData = transData[transData$attend != 7,]
  transData[transData == -3] = NA
  transData[transData == -4] = NA
  
  #calc square error
  for (i in 1:length(catList)){
    curData = transData[transData$admit == catList[i],]
    curData$sse = NA
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
            outData[k,j] = (curData[k,j]-avgVect[j])^2 #we want SSE
          }
        }
        
      }
    }
    #pick only values from outData
      for (k in 1:nrow(curData)){
        myDat =as.numeric(outData[k,])
        curData$sse[k]=sum(na.exclude(myDat))/(length(na.exclude(myDat))-1)
      }
    curData = curData[,c("PUBID_1997","sse")]
    outData<- merge(x = ENROLL_DATA, y = curData, by = "PUBID_1997", all.x = TRUE)
  }
    return(outData)
  }