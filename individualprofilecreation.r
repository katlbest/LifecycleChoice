library(plyr)

ENROLL_DATA<-read.csv("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/enroll_data_withvectors.csv")

#set up vector lists of input data==============================================================================
ageVectList <- list()
incomeVectList <- list()
enrollVectList <- list()
for (i in 1:nrow(ENROLL_DATA)){
  incVectOut = NULL
  ageVectOut= NULL
  incVectFull = NULL
  enrollVectOut = NULL
  incVect = c(ENROLL_DATA$y1[i], ENROLL_DATA$y2[i], ENROLL_DATA$y3[i], ENROLL_DATA$y4[i], ENROLL_DATA$y5[i], ENROLL_DATA$y6[i],ENROLL_DATA$y7[i],ENROLL_DATA$y8[i])
  enrollVect = c(ENROLL_DATA$enroll2[i], ENROLL_DATA$enroll3[i], ENROLL_DATA$enroll4[i], ENROLL_DATA$enroll5[i], ENROLL_DATA$enroll6[i], ENROLL_DATA$enroll7[i], ENROLL_DATA$enroll8[i], ENROLL_DATA$enroll9[i])
  startIndex = 0
  endIndex = 0
  for (j in 1:length(incVect)){
    if (j == length(incVect)){
      if (incVect[j]>=0 & startIndex ==0){ #without enrollment modification
        endIndex = j
      }
      if (endIndex - startIndex >= 3 & startIndex > 0){
        incVectOut = incVect[startIndex:endIndex]
        ageVectOut = c(startIndex:endIndex)
        enrollVectOut = enrollVect[startIndex:endIndex]
        incomeVectList[[i]]<-incVectOut
        ageVectList[[i]]<-ageVectOut
        enrollVectList[[i]]<-enrollVectOut
      }
    }
    if (incVect[j]>=0){#without enrollment modification: 
      if (startIndex ==0){
        startIndex = j
      }
      endIndex = j
    }
    else {
      if (endIndex - startIndex >= 3 & startIndex > 0){
        incVectOut = incVect[startIndex:endIndex]
        ageVectOut = c(startIndex:endIndex)
        enrollVectOut = enrollVect[startIndex:endIndex]
        incomeVectList[[i]]<-incVectOut
        ageVectList[[i]]<-ageVectOut
        enrollVectList[[i]]<-enrollVectOut
      }
      startIndex = 0
      endIndex = 0
    }
  }
  if (is.null(incVectOut)){
    incVectOut = c(-3)
    ageVectOut = c(-3)
    enrollVectOut = c(-3)
  }
}

#project using all numbers==================================================================================
tau = 27.8818
b1 = 10118.85
#stringVect = rep(NA, nrow(ENROLL_DATA))
coeffVect = data.frame(matrix(ncol = 4, nrow = dim(ENROLL_DATA)[1]))
colnames(coeffVect)=c("b0","b2", "R2", "NumObservations")
output = data.frame(matrix(ncol = length(ENROLL_DATA), nrow = 100))
colnames(output)=ENROLL_DATA$PUBID_1997
for (i in 1:nrow(ENROLL_DATA)){
  if (length(incomeVectList[[i]])>2){
    curData = data.frame(age = ageVectList[[i]], income = incomeVectList[[i]], enroll = enrollVectList[[i]])
    #startIndex = ageVectOut[1]
    #endIndex = ageVectOut[length(ageVectOut)]
    numObs = length(ageVectList[[i]])
    input1 = (1-exp(-curData$age/tau))/(curData$age/tau)
    input2 = input1 - exp(-curData$age/tau)
    quadMod <- lm(curData$income~input2)
    coeffVect[i,1] = quadMod$coefficients[1]
    #b1Vect[i] = quadMod$coefficients[2]
    coeffVect[i,2]= quadMod$coefficients[2]
    coeffVect[i,3]= summary(quadMod)$r.squared
    coeffVect[i,4]= numObs
    firstDataYear = ageVectList[[i]][1]
    lastDataYear = ageVectList[[i]][length(ageVectList[[i]])]
    new <-  c((lastDataYear+1):100)
    new1 <-(1-exp(-new/tau))/(new/tau)
    new2 <- new1 - exp(-new/tau)
    new <- data.frame(input2 = new2)
    newIncs <-predict(quadMod,new)
    output[,i] <- c(rep(-3, firstDataYear-1), curData$income, newIncs)
  }
  else{
    output[,i] <- rep(-3, 100)
  }
}  
  
write.csv(output, "C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/individualoutput.csv")
write.csv(coeffVect, "C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/individualcoefficients.csv")