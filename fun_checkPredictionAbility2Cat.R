checkPredictionAbility2Cat<- function(inputDataset){
  
  #regress and anova by admit
  admit_cats <- c(1, 2, 3, 4, 5)
  byAdmitCoeffVect = data.frame(matrix(ncol = 8, nrow = length(admit_cats)))
  colnames(byAdmitCoeffVect)=c("intercept","other",  "best_admitted", "intsig", "othersig",  "best_admittedsig", "R2", "NumObservations")
  coeffList = list()
  plotList= list()
  for (i in 1:length(admit_cats)){
    curData = inputDataset[inputDataset$admit==admit_cats[i],c("admit", "attend", "b0", "bestSchool")]
    curCount = nrow(curData)
    if (curCount >0){
      numCoeffs <- length(levels(factor(curData$bestSchool)))
      if (numCoeffs >1 & curCount > numCoeffs){
        curData<-curData[c("b0", "bestSchool")]
        curModel = lm(b0~factor(bestSchool), data = curData)
        numCoeffs <- length(curModel$coefficients)
        for (j in 1:numCoeffs){
          byAdmitCoeffVect[i,j]= curModel$coefficients[j]
          byAdmitCoeffVect[i,j+3]= summary(curModel)$coefficients[j,4]
        }
        byAdmitCoeffVect[i,7]= summary(curModel)$r.squared
        curCoeffs = summary(curModel)$coefficients
        curCoeffs= data.frame(curCoeffs)
        colnames(curCoeffs) = c("mean", "se", "t", "p")
        curCoeffs$admit = admit_cats[i]
        curCoeffs$attend = levels(factor(curData$bestSchool))
        curCoeffs$lb = curCoeffs$mean - curCoeffs$se 
        curCoeffs$ub= curCoeffs$mean + curCoeffs$se
        coeffList[[i]]=curCoeffs
        fit = aov(b0~factor(bestSchool), data = na.exclude(curData))
        print(admit_cats[i])
        print(summary(fit))
        curPlot = qplot(factor(bestSchool), b0, data = na.exclude(curData), notch= TRUE, geom = "boxplot", position = "dodge")+theme_bw()+ labs(title =paste("Best admitted = ", admit_cats[i], sep=""))
        plotList[[length(plotList)+1]]= curPlot
      } 
    } else{
      coeffList[[i]]= NA 
    }
    byAdmitCoeffVect[i,8]= curCount
  }
  outFile <- paste("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/byAdmit2cat.csv", sep = "")
  write.csv(byAdmitCoeffVect, outFile)
  multiplot(plotlist = plotList, cols=2, file = "C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/outPlot2cat.pdf")
  dev.copy(pdf,paste("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/outplot2cat", nameString, ".pdf", sep = ""))
  dev.off()
  
  return(coeffList)
}