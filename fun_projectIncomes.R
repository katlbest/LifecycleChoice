projectIncomes <- function(ageVectList, incomeVectList, enrollVectList, nameString){
  coeffVect = data.frame(matrix(ncol = 5, nrow = dim(ENROLL_DATA)[1]))
  colnames(coeffVect)=c("b0","b1" ,"b2", "R2", "NumObservations")
  outMatrix = data.frame(matrix(ncol = length(ENROLL_DATA), nrow =82))
  for (i in 1:nrow(ENROLL_DATA)){
    curData = data.frame(age = ageVectList[[i]], income = incomeVectList[[i]], enroll = enrollVectList[[i]])
    curData[curData == -3] <- NA
    curData[curData == -4] <- NA 
    curData<- na.exclude(curData)
    if (dim(curData)[1]>2){
      numObs = length(na.exclude(curData$age))
      input1 = (1-exp(-curData$age/tau))/(curData$age/tau)
      B = input1 - exp(-curData$age/tau)
      input2 = (1+m*B+n*input1)
      output = curData$income -(b * B)- (a * input1)
      quadMod <- lm(output~0 + input2)
      
      coeffVect[i,1] = quadMod$coefficients[1]
      coeffVect[i,2] = quadMod$coefficients[1] * n + a
      coeffVect[i,3]= quadMod$coefficients[1] * m + b
      coeffVect[i,4]= summary(quadMod)$r.squared
      coeffVect[i,5]= numObs
      firstDataYear = ageVectList[[i]][1]
      lastDataYear = ageVectList[[i]][length(ageVectList[[i]])]
      new <-  c((lastDataYear+1):100)
      new1 <-(1-exp(-new/tau))/(new/tau)
      newB <- new1 - exp(-new/tau)
      new2 <- (1+m*newB+n*new1)
      new <- data.frame(input2 = new2)
      newIncs <-predict(quadMod,new)+b*newB + a * new1
      outMatrix[,i] <- c(rep(-3, firstDataYear-19), incomeVectList[[i]], newIncs)
    }
    else{
      outMatrix[,i] <- rep(-3,82)
    }
  }  
  colnames(outMatrix)=ENROLL_DATA$PUBID_1997
  outMatrixString <- paste("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/",nameString ,"-individualoutput.csv", sep = "")
  write.csv(outMatrix, outMatrixString)
  outCoeffString <- paste("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/",nameString ,"-individualcoefficients.csv" ,sep = "")
  write.csv(coeffVect, outCoeffString)
  outList <- list(coeffVect, outMatrix)
  return(outList)
}