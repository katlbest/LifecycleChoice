getRelevantData<- function(inData, inb0){
  
  #set up dataset
  catList = levels(as.factor(ENROLL_DATA$cat))
  transData = data.frame(t(inData))
  colnames(transData)= c(19:100)
  transData$cat = ENROLL_DATA$cat
  transData$admit = ENROLL_DATA$BestAd5b
  transData$attend = ENROLL_DATA$BestAtt5b
  #transData$graduated = NA
  #transData[ENROLL_DATA$CVC_BA_DEGREE_XRND<0,]$graduated = 0
  #transData[ENROLL_DATA$CVC_BA_DEGREE_XRND>=0,]$graduated = 1
  inb0[is.na(inb0)]=-3
  inb0 = unlist(inb0)
  transData$b0 = inb0
  
  #remove those with out of range values
    b0Min = -500000 #upper limit of about 140K top salary
    b0Max = -100 #lower limit of about 12K top salary
    transData = transData[transData$b0 >= -500000 & transData$b0 <= -100,]
  
  #remove those with category 7
    transData[transData$attend == 7,]$cat <- -3
    transData[transData$admit == 7,]$cat <- -3
    transData[transData$admit == 7,]$admit <- -3
    transData[transData$attend == -3,]$attend <- -10
    transData[transData$attend == 7,]$attend <- -3
  
  #select only relevant people
    transData[transData$cat == -3,]$cat <- NA
    transData[transData$admit == -3,]$admit <- NA
    transData[transData$attend == -3,]$attend <- NA
    
  #return
    outData = na.exclude(transData)
    return(outData)
}