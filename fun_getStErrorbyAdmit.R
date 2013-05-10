getStErrorbyAdmit<- function(inData, inb0){
  
  #set up dataset
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
      catList = levels(as.factor(transData$admit))
    
  #setup output
    lookupData = data.frame(matrix(ncol = 84, nrow = length(catList)))
    colnames(lookupData) = c("cat", 19:100, "b0")
    lookupData[,1]= catList
    
  #calculate average of all data, real data, and b0 for each admissions group
    for (i in 1:length(catList)){
      curData = transData[transData$admit == catList[i],]
      if (nrow(curData) > 1){ #if there are at least two people in this category
        #avgVect = rep(NA, 82)
        #outData = data.frame(matrix(ncol = 82, nrow = nrow(curData)))
        for (j in 1:82){
          lookupData[i,j+1]= mean(na.exclude(curData[,j]))
          } 
        lookupData[i,84]=mean(na.exclude(curData[,c("b0")]))
        }
      }
      return(lookupData)
}