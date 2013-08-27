checkPredictionAbilityAttendOnly<- function(inputDataset){
  
  #regress and anova by admit
  admit_cats <- c(1, 2, 3, 4, 5)
  byAdmitCoeffVect = data.frame(matrix(ncol = 7, nrow = length(admit_cats)))
  colnames(byAdmitCoeffVect)=c("intercept","attended_school", "intsig", "attendedsig", "R2", "NumObservations")
  coeffList = list()
  plotList= list()
  for (i in 1:length(admit_cats)){
    curData = inputDataset[inputDataset$admit==admit_cats[i],c("admit", "attend", "b0", "attendInd")]
    curCount = nrow(curData)
    if (curCount >0){
      numCoeffs <- length(levels(factor(curData$attendInd)))
      if (numCoeffs >1 & curCount > numCoeffs){
        curData<-curData[c("b0", "attendInd")]
        curModel = lm(b0~factor(attendInd), data = curData)
        numCoeffs <- length(curModel$coefficients)
        for (j in 1:numCoeffs){
          byAdmitCoeffVect[i,j]= curModel$coefficients[j]
          byAdmitCoeffVect[i,j+2]= summary(curModel)$coefficients[j,4]
        }
        byAdmitCoeffVect[i,5]= summary(curModel)$r.squared
        curCoeffs = summary(curModel)$coefficients
        curCoeffs= data.frame(curCoeffs)
        colnames(curCoeffs) = c("mean", "se", "t", "p")
        curCoeffs$admit = admit_cats[i]
        curCoeffs$attend = levels(factor(curData$attendInd))
        curCoeffs$lb = curCoeffs$mean - curCoeffs$se 
        curCoeffs$ub= curCoeffs$mean + curCoeffs$se
        coeffList[[i]]=curCoeffs
        curPlot = qplot(factor(attendInd), b0, data = na.exclude(curData), notch= TRUE, geom = "boxplot", position = "dodge")+theme_bw()+ labs(title =paste("Best admitted = ", admit_cats[i], sep=""))
        plotList[[length(plotList)+1]]= curPlot
      } 
    } else{
      coeffList[[i]]= NA 
    }
    byAdmitCoeffVect[i,6]= curCount
  }
  outFile <- paste("byAdmitcatAttendOnly.csv", sep = "")
  write.csv(byAdmitCoeffVect, outFile)
  multiplot(plotlist = plotList, cols=2, file = "outPlotAttendOnly.pdf")
  nameString = "attendercheck"
  dev.copy(pdf,paste("outplotAttendOnly", nameString, ".pdf", sep = ""))
  dev.off()
  
  return(coeffList)
}