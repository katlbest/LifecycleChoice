checkPredictionAbilityAttendCats<- function(inputDataset){
  
  #regress and anova by admit
  admit_cats <- c(1, 2, 3, 4, 5)
  byAdmitCoeffVect = data.frame(matrix(ncol = 12, nrow = length(admit_cats)))
  colnames(byAdmitCoeffVect)=c("attended_best", "second", "third","fourth","fifth", "sig1", "sig2", "sig3", "sig4", "sig5", "R2", "NumObservations")
  coeffList = list()
  plotList= list()
  for (i in 1:length(admit_cats)){
    curData = inputDataset[inputDataset$admit==admit_cats[i],c("admit", "attend", "b0")]
    curCount = nrow(curData)
    if (curCount >0){
      numCoeffs <- length(levels(factor(curData$attend)))
      #curData$attend2 <- as.factor(curData$attend)
      #curData$attend2 <- factor(curData$attend2, levels=rev(levels(curData$attend2)) )
      if (numCoeffs >1 & curCount > numCoeffs){
        curData<-curData[c("b0", "attend")]
        curModel = lm(b0~factor(attend), data = curData)
        numCoeffs <- length(curModel$coefficients)
        for (j in 1:numCoeffs){
          byAdmitCoeffVect[i,j]= curModel$coefficients[j]
          byAdmitCoeffVect[i,j+5]= summary(curModel)$coefficients[j,4]
        }
        byAdmitCoeffVect[i,11]= summary(curModel)$r.squared
        curCoeffs = summary(curModel)$coefficients
        curCoeffs= data.frame(curCoeffs)
        colnames(curCoeffs) = c("mean", "se", "t", "p")
        curCoeffs$admit = admit_cats[i]
        curCoeffs$attend = levels(factor(curData$attend))
        curCoeffs$lb = curCoeffs$mean - curCoeffs$se 
        curCoeffs$ub= curCoeffs$mean + curCoeffs$se
        coeffList[[i]]=curCoeffs
        curPlot = qplot(factor(attend), b0, data = na.exclude(curData), notch= TRUE, geom = "boxplot", position = "dodge")+theme_bw()+ labs(title =paste("Best admitted = ", admit_cats[i], sep=""))
        plotList[[length(plotList)+1]]= curPlot
      } 
    } else{
      coeffList[[i]]= NA 
    }
    byAdmitCoeffVect[i,12]= curCount
  }
  outFile <- paste("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/byAdmitcatAttendCats.csv", sep = "")
  write.csv(byAdmitCoeffVect, outFile)
  multiplot(plotlist = plotList, cols=2, file = "C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/outPlotAttendCats.pdf")
  dev.copy(pdf,paste("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/outplotAttendCats", nameString, ".pdf", sep = ""))
  dev.off()
  
  return(coeffList)
}