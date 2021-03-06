checkPredictionAbility<- function(b0Name, nameString){
  
  #create prediction datasets
    inputDataset = data.frame(b0 = b0Name, cat = ENROLL_DATA$cat, admit = ENROLL_DATA$BestAd5b, attend = ENROLL_DATA$BestAtt5b)

  #remove those entries where b0 out of range
    b0Min = -500000 #upper limit of about 140K top salary
    b0Max = -100 #lower limit of about 12K top salary
    inputDataset[inputDataset$b0 < -500000 | inputDataset$b0 > -100,]$b0 <- -3
    
  #7 values for admission/attendance should not be included, and associated cateogires should be discarded
    levels(inputDataset$cat) <- c(levels(inputDataset$cat),-3)
    inputDataset[inputDataset$attend == 7,]$cat <- -3 #fix this for gradonly
    inputDataset[inputDataset$admit == 7,]$cat <- -3
    inputDataset[inputDataset$admit == 7,]$admit <- -3
    inputDataset[inputDataset$attend == -3,]$attend <- -10
    inputDataset[inputDataset$attend == 7,]$attend <- -3
    inputDataset[inputDataset == -3] <- NA
    inputDataset[inputDataset == -4] <- NA
    inputDataset[inputDataset == -5] <- NA
    
  #plot
    myDir = "C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/Curve fitting/Plots/"
    fileName = paste(myDir, nameString, "Cat.pdf",sep ="")
    qplot(factor(cat), b0, data = na.exclude(inputDataset), notch= TRUE, geom = "boxplot", position = "dodge")+theme_bw() + labs(title = paste("By Category, ", nameString, sep =""))
    ggsave(file = fileName)
    fileName = paste(myDir, nameString, "Admit.pdf",sep ="")
    qplot(factor(admit), b0, data = na.exclude(inputDataset), notch= TRUE, geom = "boxplot", position = "dodge")+theme_bw()+ labs(title = paste("By best admitted, ", nameString, sep= ""))
    ggsave(file = fileName)
    fileName = paste(myDir, nameString, "Attend.pdf",sep ="")
    qplot(factor(attend), b0, data = na.exclude(inputDataset), notch= TRUE, geom = "boxplot", position = "dodge")+theme_bw()+ labs(title =paste("By best attended, ", nameString, sep=""))
    ggsave(file = fileName)
    print("ANOVA by attend")
    fit = aov(b0~factor(attend), data = na.exclude(inputDataset))
    print(summary(fit))
    inputDataset2 = na.exclude(inputDataset)
    myTest = pairwise.t.test(inputDataset2$b0, factor(inputDataset2$attend), p.adj= "bonf")
    print(myTest)
    
    print("ANOVA by admit")
    fit = aov(b0~factor(admit), data = na.exclude(inputDataset))
    print(summary(fit))
    myTest = pairwise.t.test(inputDataset2$b0, factor(inputDataset2$admit), p.adj= "bonf")
    print(myTest)
    
 
  #regress
    CatMod <- lm(b0~ factor(cat), data=na.exclude(inputDataset))
    print(summary(CatMod))
    AdmitMod <- lm(b0~ factor(admit), data=na.exclude(inputDataset))
    print(summary(AdmitMod))
    AttendMod <- lm(b0~ factor(attend), data=na.exclude(inputDataset))
    print(summary(AttendMod))

  #regress and anova by admit
    admit_cats <- c(1, 2, 3, 4, 5)
    byAdmitCoeffVect = data.frame(matrix(ncol = 15, nrow = length(admit_cats)))
    colnames(byAdmitCoeffVect)=c("intercept","1",  "2", "3", "4", "5/6", "intsig", "1sig", "2sig", "3sig", "4sig", "5/6sig","R2", "NumObservations", "levels")
    inputDataset[is.na(inputDataset)] <- -3
    coeffList = list()
    plotList= list()
    for (i in 1:length(admit_cats)){
      curData = inputDataset[inputDataset$admit==admit_cats[i],]
      curCount = nrow(curData)
      if (curCount >0){
        #curData[curData$attend == -3,]$attend <- -4 #change attends to -4 so they dont get deleted
        curData[curData == -3] <- NA
        curCount = nrow(na.exclude(curData))
        numCoeffs <- length(levels(factor(curData$attend)))
        if (numCoeffs >1 & curCount > numCoeffs){
          curData<-curData[c("b0", "attend")]
          curModel = lm(b0~factor(attend), data = na.exclude(curData))
          numCoeffs <- length(curModel$coefficients)
          for (j in 1:numCoeffs){
            byAdmitCoeffVect[i,j]= curModel$coefficients[j]
            byAdmitCoeffVect[i,j+6]= summary(curModel)$coefficients[j,4]
          }
          byAdmitCoeffVect[i,13]= summary(curModel)$r.squared
          byAdmitCoeffVect[i,15]= toString(levels(factor(curData$attend)))
          curCoeffs = summary(curModel)$coefficients
          curCoeffs= data.frame(curCoeffs)
          colnames(curCoeffs) = c("mean", "se", "t", "p")
          curCoeffs$admit = admit_cats[i]
          curCoeffs$attend = levels(factor(curData$attend))
          curCoeffs$lb = curCoeffs$mean - curCoeffs$se 
          curCoeffs$ub= curCoeffs$mean + curCoeffs$se
          coeffList[[i]]=curCoeffs
          fit = aov(b0~factor(attend), data = na.exclude(curData))
          print(admit_cats[i])
          print(summary(fit))
          curPlot = qplot(factor(attend), b0, data = na.exclude(curData), notch= TRUE, geom = "boxplot", position = "dodge")+theme_bw()+ labs(title =paste("Best admitted = ", admit_cats[i], sep=""))
          plotList[[length(plotList)+1]]= curPlot
        } 
      } else{
        coeffList[[i]]= NA 
      }
      byAdmitCoeffVect[i,14]= curCount
    }
    outFile <- paste("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/",nameString,"-byAdmit.csv", sep = "")
    write.csv(byAdmitCoeffVect, outFile)
    multiplot(plotlist = plotList, cols=2, file = "C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/outPlot.pdf")
    dev.copy(pdf,paste("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/outplot", nameString, ".pdf", sep = ""))
    dev.off()
    
  #get by attendance
    attend_cats <- c(-10, 1, 2, 3, 4, 5)
    byAttendCoeffVect = data.frame(matrix(ncol = 13, nrow = length(attend_cats)))
    colnames(byAttendCoeffVect)=c("intercept",  "2", "3", "4", "5/6", "intsig", "2sig", "3sig", "4sig", "5/6sig","R2", "NumObservations", "levels")
    for (i in 1:length(attend_cats)){
      curData = inputDataset[inputDataset$attend==attend_cats[i],]
      curCount = nrow(curData)
      if (curCount >0){
        if (i == 1){
          #curData[curData$attend == -3,]$attend <- -4 #change attends to -4 so they dont get deleted
        }
        curData[curData == -3] <- NA
        curCount = nrow(na.exclude(curData))
        numCoeffs <- length(levels(factor(curData$admit)))
        if (numCoeffs >1 & curCount > numCoeffs){
          curData<-curData[c("b0", "admit")]
          curModel = lm(b0~factor(admit), data = na.exclude(curData))
          numCoeffs <- length(curModel$coefficients)
          for (j in 1:numCoeffs){
            byAttendCoeffVect[i,j]= curModel$coefficients[j]
            byAttendCoeffVect[i,j+5]= summary(curModel)$coefficients[j,4]
          }
          byAttendCoeffVect[i,11]= summary(curModel)$r.squared
          byAttendCoeffVect[i,13]= toString(levels(factor(curData$admit)))
          fit = aov(b0~factor(admit), data = na.exclude(curData))
          print(attend_cats[i])
          print(summary(fit))
        } 
      }
      byAttendCoeffVect[i,12]= curCount
    }
    outFile <- paste("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/",nameString,"-byAttend.csv", sep = "")
    write.csv(byAttendCoeffVect, outFile)
  
    #return from by admit
      return(coeffList)
}