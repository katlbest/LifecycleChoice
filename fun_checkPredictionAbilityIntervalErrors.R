checkPredictionAbilityIntervalErrors<- function(inputDataset, nameString){
        
  inputDataset = inputDataset[,c("attend","graduated","b0" ,"attend2","admit2" ,"attendError","admitError", "admit")]
  #regress by admit
    admit_cats <- c(1, 2, 3, 4, 5)
    byAdmitCoeffVect = data.frame(matrix(ncol = 7, nrow = length(admit_cats)))
    colnames(byAdmitCoeffVect)=c("intercept",  "attendError", "intsig", "attendErrorsig", "R2", "NumObservations", "levels")
    coeffList = list()
    plotList= list()
    for (i in 1:length(admit_cats)){
      curData = inputDataset[inputDataset$admit==admit_cats[i],]
      curCount = nrow(curData)
      if (curCount >0){
        numCoeffs = length(levels(factor(curData$attend)))
        if (numCoeffs >1 & curCount > numCoeffs){
          curData<-curData[c("b0", "attendError", "attend", "attend2")]
          curModel = lm(b0~attendError, data = na.exclude(curData))
          numCoeffs <- 2
          for (j in 1:numCoeffs){
            byAdmitCoeffVect[i,j]= curModel$coefficients[j]
            byAdmitCoeffVect[i,j+2]= summary(curModel)$coefficients[j,4]
          }
          byAdmitCoeffVect[i,5]= summary(curModel)$r.squared
          byAdmitCoeffVect[i,7]= toString(levels(factor(curData$attend)))
          curPlot = qplot(attendError, b0, data = na.exclude(curData), notch= TRUE, geom = "boxplot", position = "dodge")+theme_bw()+ labs(title =paste("Best admitted = ", admit_cats[i], sep=""))
          plotList[[length(plotList)+1]]= curPlot
        } 
      } else{
        coeffList[[i]]= NA 
      }
      byAdmitCoeffVect[i,6]= curCount
    }
    outFile <- paste("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/",nameString,"-byAdmitIntErr.csv", sep = "")
    write.csv(byAdmitCoeffVect, outFile)
    multiplot(plotlist = plotList, cols=2, file = "C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/outPlotIntERr.pdf")
    dev.copy(pdf,paste("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/outplotIntErr", nameString, ".pdf", sep = ""))
    dev.off()
  
  #regress by attend
    coeffList = list()
    plotList= list()
    attend_cats <- c(1, 2, 3, 4, 5)
    byAttendCoeffVect = data.frame(matrix(ncol = 7, nrow = length(attend_cats)))
    colnames(byAttendCoeffVect)=c("intercept",  "admitError", "intsig", "admitErrorsig", "R2", "NumObservations", "levels")
    coeffList = list()
    plotList= list()
    for (i in 1:length(attend_cats)){
      curData = inputDataset[inputDataset$attend==attend_cats[i],]
      curCount = nrow(curData)
      if (curCount >0){
        numCoeffs = length(levels(factor(curData$admit)))
        if (numCoeffs >1 & curCount > numCoeffs){
          curData<-curData[c("b0", "admitError", "admit")]
          curModel = lm(b0~admitError, data = na.exclude(curData))
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
    outFile <- paste("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/",nameString,"-byAttendIntErr.csv", sep = "")
    write.csv(byAttendCoeffVect, outFile)
    multiplot(plotlist = plotList, cols=2, file = "C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/outPlotIntbyAttendErr.pdf")
    dev.copy(pdf,paste("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/outplotIntbyAttendErr", nameString, ".pdf", sep = ""))
    dev.off()
    

}