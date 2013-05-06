checkPredictionAbilityInterval<- function(inputDataset, nameString){
        
  inputDataset = inputDataset[,c("attend","graduated","b0" ,"attend2","admit2" ,"attendError","admitError", "admit")]
  #regress by admit
    admit_cats <- c(1, 2, 3, 4, 5)
    byAdmitCoeffVect = data.frame(matrix(ncol = 7, nrow = length(admit_cats)))
    colnames(byAdmitCoeffVect)=c("intercept",  "attend2", "intsig", "attend2sig", "R2", "NumObservations", "levels")
    coeffList = list()
    plotList= list()
    for (i in 1:length(admit_cats)){
      curData = inputDataset[inputDataset$admit==admit_cats[i],]
      curCount = nrow(curData)
      if (curCount >0){
        numCoeffs = length(levels(factor(curData$attend)))
        if (numCoeffs >1 & curCount > numCoeffs){
          curData<-curData[c("b0", "attend2", "attend")]
          curModel = lm(b0~attend2, data = na.exclude(curData))
          numCoeffs <- 2
          for (j in 1:numCoeffs){
            byAdmitCoeffVect[i,j]= curModel$coefficients[j]
            byAdmitCoeffVect[i,j+2]= summary(curModel)$coefficients[j,4]
          }
          byAdmitCoeffVect[i,5]= summary(curModel)$r.squared
          byAdmitCoeffVect[i,7]= toString(levels(factor(curData$attend)))
          curPlot = qplot(attend2, b0, data = na.exclude(curData), notch= TRUE, geom = "boxplot", position = "dodge")+theme_bw()+ labs(title =paste("Best admitted = ", admit_cats[i], sep=""))
          plotList[[length(plotList)+1]]= curPlot
        } 
      } else{
        coeffList[[i]]= NA 
      }
      byAdmitCoeffVect[i,6]= curCount
    }
    outFile <- paste("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/",nameString,"-byAdmitInt.csv", sep = "")
    write.csv(byAdmitCoeffVect, outFile)
    multiplot(plotlist = plotList, cols=2, file = "C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/outPlotInt.pdf")
    dev.copy(pdf,paste("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/outplotInt", nameString, ".pdf", sep = ""))
    dev.off()
  
  #regress by attend
    coeffList = list()
    plotList= list()
    attend_cats <- c(1, 2, 3, 4, 5)
    byAttendCoeffVect = data.frame(matrix(ncol = 7, nrow = length(attend_cats)))
    colnames(byAttendCoeffVect)=c("intercept",  "admit2", "intsig", "admit2sig", "R2", "NumObservations", "levels")
    coeffList = list()
    plotList= list()
    for (i in 1:length(attend_cats)){
      curData = inputDataset[inputDataset$attend==attend_cats[i],]
      curCount = nrow(curData)
      if (curCount >0){
        numCoeffs = length(levels(factor(curData$admit)))
        if (numCoeffs >1 & curCount > numCoeffs){
          curData<-curData[c("b0", "admit2", "admit")]
          curModel = lm(b0~admit2, data = na.exclude(curData))
          numCoeffs <- 2
          for (j in 1:numCoeffs){
            byAttendCoeffVect[i,j]= curModel$coefficients[j]
            byAttendCoeffVect[i,j+2]= summary(curModel)$coefficients[j,4]
          }
          byAttendCoeffVect[i,5]= summary(curModel)$r.squared
          byAttendCoeffVect[i,7]= toString(levels(factor(curData$admit)))
          curPlot = qplot(admit, b0, data = na.exclude(curData), notch= TRUE, geom = "boxplot", position = "dodge")+theme_bw()+ labs(title =paste("Best attended = ", attend_cats[i], sep=""))
          plotList[[length(plotList)+1]]= curPlot
        } 
      } else{
        coeffList[[i]]= NA 
      }
      byAttendCoeffVect[i,6]= curCount
    }
    outFile <- paste("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/",nameString,"-byAttendInt.csv", sep = "")
    write.csv(byAttendCoeffVect, outFile)
    multiplot(plotlist = plotList, cols=2, file = "C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/outPlotIntbyAttend.pdf")
    dev.copy(pdf,paste("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/outplotIntbyAttend", nameString, ".pdf", sep = ""))
    dev.off()
    

}