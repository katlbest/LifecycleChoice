ENROLL_DATA<-read.csv("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/allindivdata.csv")

#functions===============================================================================================
fillMissing <- function(IncomeVector, EnrollmentVector, personindex){
  startIndex = 0
  endIndex = 0
  #fill in intermediate zeros/missings
  changing = 0
  last = 0
  for (j in 2:length(IncomeVector)){
    if (changing ==0)  {
      if (IncomeVector[j-1]>0 & IncomeVector[j]<=0){
        last = IncomeVector[j-1]
        change_start_ind = j
        change_start_value = IncomeVector[j-1]
        changing = 1
      }
    }
    else{
      if (IncomeVector[j]> 0){
        for (k in (change_start_ind:j-1)){
          IncomeVector[k] = max(mean(last, change_start_value), last)
        }
        changing = 0
      }
      else if (IncomeVector[j] > 0){
        changing = 0
      }
    }
  }
  #find longest run of usable data
  for (j in 1:length(IncomeVector)){
    if (j == length(IncomeVector)){
      #if (incVect[j]>=0 & startIndex ==0 & enrollVect[j]!= 1){ #with enrollment modification
      if (IncomeVector[j]>0 & startIndex ==0){ #without enrollment modification
        endIndex = j
      }
      if (endIndex - startIndex >= 2 & startIndex > 0){
        incVectOut = IncomeVector[startIndex:endIndex]
        ageVectOut = c(startIndex:endIndex)
        enrollVectOut = EnrollmentVector[startIndex:endIndex]
        returnList = list(incVectOut, ageVectOut, enrollVectOut)
      }
    }
    #if (incVect[j]>=0 & enrollVect[j]!= 1){ #with enrollment modification: 
    if (IncomeVector[j]>0){#without enrollment modification: 
      if (startIndex ==0){
        startIndex = j
      }
      endIndex = j
    }
    else {
      if (endIndex - startIndex >= 2 & startIndex > 0){
        incVectOut = IncomeVector[startIndex:endIndex]
        ageVectOut = c(startIndex:endIndex)
        enrollVectOut = EnrollmentVector[startIndex:endIndex]
        returnList = list(incVectOut, ageVectOut, enrollVectOut)
      }
      startIndex = 0
      endIndex = 0
    }
  }
  if (is.null(incVectOut)){
    incVectOut = c(-3)
    ageVectOut = c(-3)
    enrollVectOut = c(-3)
    returnList = list(incVectOut, ageVectOut, enrollVectOut)
  }
  return(returnList)
  #print(returnList)
}

removeZeros <- function(IncomeVectorList, AgeVectorList, EnrollmentVectorList){
  outInc = list()
  outAge = list()
  outEnroll = list()
  for (i in 1:length(IncomeVectorList)){
    outInc[[i]] = IncomeVectorList[[i]]
    outAge[[i]] = AgeVectorList[[i]]
    outEnroll[[i]] = EnrollmentVectorList[[i]]
    if (IncomeVectorList[[i]][1]>=0){
      firstBigIndex = 0
      for (j in 2:length(IncomeVectorList[[i]])){
        if (IncomeVectorList[[i]][j]<IncomeVectorList[[i]][j-1]){
          IncomeVectorList[[i]][j]=IncomeVectorList[[i]][j-1]
        }
        if (firstBigIndex == 0){
          if (IncomeVectorList[[i]][j-1] >= 10000){
            firstBigIndex = j-1
          }
        }
      }
      outInc[[i]]= IncomeVectorList[[i]][firstBigIndex:length(IncomeVectorList[[i]])]
      outAge[[i]]= AgeVectorList[[i]][firstBigIndex:length(AgeVectorList[[i]])]
      outEnroll[[i]]= EnrollmentVectorList[[i]][firstBigIndex:length(EnrollmentVectorList[[i]])]
    }
  }
  outList = list(outInc, outAge, outEnroll)
  return(outList)
}
#set up vector lists of input data==============================================================================
ageVectListNm <- list()
incomeVectListNm <- list()
enrollVectListNm <- list()
ageVectListM<- list()
incomeVectListM <- list()
enrollVectListM <- list()
ageVectListLabNm <- list()
incomeVectListLabNm <- list()
enrollVectListLabNm <- list()
ageVectListLabM <- list()
incomeVectListLabM <- list()
enrollVectListLabM <- list()

#populate list===============
  for (i in 1:nrow(ENROLL_DATA)){
    incVectOut = NULL
    ageVectOut= NULL
    enrollVectOut = NULL
  
    incVectNm = c(ENROLL_DATA$ynm1[i], ENROLL_DATA$ynm2[i], ENROLL_DATA$ynm3[i], ENROLL_DATA$ynm4[i], ENROLL_DATA$ynm5[i], ENROLL_DATA$ynm6[i],ENROLL_DATA$ynm7[i],ENROLL_DATA$ynm8[i])
    incVectM = c(ENROLL_DATA$y1[i], ENROLL_DATA$y2[i], ENROLL_DATA$y3[i], ENROLL_DATA$y4[i], ENROLL_DATA$y5[i], ENROLL_DATA$y6[i],ENROLL_DATA$y7[i],ENROLL_DATA$y8[i])
    incVectLabNm = c(ENROLL_DATA$znm1[i], ENROLL_DATA$znm2[i], ENROLL_DATA$znm3[i], ENROLL_DATA$znm4[i], ENROLL_DATA$znm5[i], ENROLL_DATA$znm6[i],ENROLL_DATA$znm7[i],ENROLL_DATA$znm8[i])
    incVectLabM = c(ENROLL_DATA$z1[i], ENROLL_DATA$z2[i], ENROLL_DATA$z3[i], ENROLL_DATA$z4[i], ENROLL_DATA$z5[i], ENROLL_DATA$z6[i],ENROLL_DATA$z7[i],ENROLL_DATA$z8[i])
    enrollVect = c(ENROLL_DATA$enroll2[i], ENROLL_DATA$enroll3[i], ENROLL_DATA$enroll4[i], ENROLL_DATA$enroll5[i], ENROLL_DATA$enroll6[i], ENROLL_DATA$enroll7[i], ENROLL_DATA$enroll8[i], ENROLL_DATA$enroll9[i])
    
    NmReturn <- fillMissing(incVectNm, enrollVect, i)
    incomeVectListNm[[i]]<-NmReturn[[1]]
    ageVectListNm[[i]]<-NmReturn[[2]]
    enrollVectListNm[[i]]<-NmReturn[[3]]
    
    MReturn <-fillMissing(incVectM, enrollVect, i)
    incomeVectListM[[i]]<-MReturn[[1]]
    ageVectListM[[i]]<-MReturn[[2]]
    enrollVectListM[[i]]<-MReturn[[3]]
    
    LabNmReturn <-fillMissing(incVectLabNm, enrollVect, i)
    incomeVectListLabNm[[i]]<-LabNmReturn[[1]]
    ageVectListLabNm[[i]]<-LabNmReturn[[2]]
    enrollVectListLabNm[[i]]<-LabNmReturn[[3]]
    
    LabMReturn <-fillMissing(incVectLabM, enrollVect, i)
    incomeVectListLabM[[i]]<-LabMReturn[[1]]
    ageVectListLabM[[i]]<-LabMReturn[[2]]
    enrollVectListLabM[[i]]<-LabMReturn[[3]]
  }

#must add 18 to each age number so that tau is correct==========================
  for (i in 1:length(ageVectListNm)){
    if (ageVectListNm[[i]][1]>0){
      ageVectListNm[[i]] = ageVectListNm[[i]] + 18
    }
  }
  for (i in 1:length(ageVectListM)){
    if (ageVectListM[[i]][1]>0){
      ageVectListM[[i]] = ageVectListM[[i]] + 18
    }
  }
  for (i in 1:length(ageVectListLabNm)){
    if (ageVectListLabNm[[i]][1]>0){
      ageVectListLabNm[[i]] = ageVectListLabNm[[i]] + 18
    }
  }
  for (i in 1:length(ageVectListLabM)){
    if (ageVectListLabM[[i]][1]>0){
      ageVectListLabM[[i]] = ageVectListLabM[[i]] + 18
    }
  }

#set to missing if only zeros available======================
  for (i in 1:length(incomeVectListNm)){
    #set to missing if only zeros available
    if (sum(incomeVectListNm[[i]])<= 0){
      incomeVectListNm[[i]]<-c(-3)
      ageVectListNm[[i]]<-c(-3)
      enrollVectListNm[[i]]<-c(-3)
    }
  }
  for (i in 1:length(incomeVectListM)){
    #set to missing if only zeros available
    if (sum(incomeVectListM[[i]])<= 0){
      incomeVectListM[[i]]<-c(-3)
      ageVectListM[[i]]<-c(-3)
      enrollVectListM[[i]]<-c(-3)
    }
  }
  for (i in 1:length(incomeVectListLabNm)){
    #set to missing if only zeros available
    if (sum(incomeVectListLabNm[[i]])<= 0){
      incomeVectListLabNm[[i]]<-c(-3)
      ageVectListLabNm[[i]]<-c(-3)
      enrollVectListLabNm[[i]]<-c(-3)
    }
  }
  for (i in 1:length(incomeVectListLabM)){
    #set to missing if only zeros available
    if (sum(incomeVectListLabM[[i]])<= 0){
      incomeVectListLabM[[i]]<-c(-3)
      ageVectListLabM[[i]]<-c(-3)
      enrollVectListLabM[[i]]<-c(-3)
    }
  }

#OPTIONAL: if lower than previous, set to previous, and remove leading zeros======================
#NOTE: this leaves you with vectors without zeros

NmReturn <- removeZeros(incomeVectListNm, ageVectListNm, enrollVectListNm)
incomeVectListNm<-NmReturn[[1]]
ageVectListNm<-NmReturn[[2]]
enrollVectListNm<-NmReturn[[3]]

MReturn <-removeZeros(incomeVectListM, ageVectListM, enrollVectListM)
incomeVectListM[[i]]<-MReturn[[1]]
ageVectListM[[i]]<-MReturn[[2]]
enrollVectListM[[i]]<-MReturn[[3]]

LabNmReturn <-removeZeros(incomeVectListLabNm, ageVectListLabNm, enrollVectListLabNm)
incomeVectListLabNm[[i]]<-LabNmReturn[[1]]
ageVectListLabNm[[i]]<-LabNmReturn[[2]]
enrollVectListLabNm[[i]]<-LabNmReturn[[3]]

LabMReturn <-removeZeros(incomeVectListLabM, ageVectListLabM, enrollVectListLabM)
incomeVectListLabM[[i]]<-LabMReturn[[1]]
ageVectListLabM[[i]]<-LabMReturn[[2]]
enrollVectListLabM[[i]]<-LabMReturn[[3]]

#Project======================================================================================
#project with fixed relationship between b2 and b0
#set up
  #Nm
    ageVectList <- ageVectListNm
    incomeVectList <- incomeVectListNm
    enrollVectList <- enrollVectListNm
  #M
    #ageVectList <- ageVectListM
    #incomeVectList <- incomeVectListM
    #enrollVectList <- enrollVectListM
  #LabNm
    #ageVectList <- ageVectListLabNm
    #incomeVectList <- incomeVectListLabNm
    #enrollVectList <- enrollVectListLabNm
  #LabM
    #ageVectList <- ageVectListLabM
    #incomeVectList <- incomeVectLisLabM
    #enrollVectList <- enrollVectListLabM

#inputs
tau =27.8818
m = -3.8149
b = 36241
n =  -0.2445
a = 2234.3

coeffVect = data.frame(matrix(ncol = 5, nrow = dim(ENROLL_DATA)[1]))
colnames(coeffVect)=c("b0","b1" ,"b2", "R2", "NumObservations")
outMatrix = data.frame(matrix(ncol = length(ENROLL_DATA), nrow =82))
for (i in 1:nrow(ENROLL_DATA)){
  if (length(incomeVectList[[i]])>2){
    curData = data.frame(age = ageVectList[[i]], income = incomeVectList[[i]], enroll = enrollVectList[[i]])
    numObs = length(ageVectList[[i]])
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
    outMatrix[,i] <- c(rep(-3, firstDataYear-19), curData$income, newIncs)
  }
  else{
    outMatrix[,i] <- rep(-3,82)
  }
}  
colnames(outMatrix)=ENROLL_DATA$PUBID_1997
write.csv(outMatrix, "C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/individualoutput.csv")
write.csv(coeffVect, "C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/individualcoefficients.csv")

#OLD=======================================================================================
#project without fixing any variables
tau = 27.8818
coeffVect = data.frame(matrix(ncol = 5, nrow = dim(ENROLL_DATA)[1]))
colnames(coeffVect)=c("b0","b1" ,"b2", "R2", "NumObservations")
output = data.frame(matrix(ncol = length(ENROLL_DATA), nrow = 82))
for (i in 1:nrow(ENROLL_DATA)){
  if (length(incomeVectList[[i]])>2){
    curData = data.frame(age = ageVectList[[i]], income = incomeVectList[[i]], enroll = enrollVectList[[i]])
    numObs = length(ageVectList[[i]])
    input1 = (1-exp(-curData$age/tau))/(curData$age/tau)
    input2 = input1 - exp(-curData$age/tau)
    quadMod <- lm(curData$income~input1 + input2)
    coeffVect[i,1] = quadMod$coefficients[1]
    coeffVect[i,2] = quadMod$coefficients[2]
    coeffVect[i,3]= quadMod$coefficients[3]
    coeffVect[i,4]= summary(quadMod)$r.squared
    coeffVect[i,5]= numObs
    firstDataYear = ageVectList[[i]][1]
    lastDataYear = ageVectList[[i]][length(ageVectList[[i]])]
    new <-  c((lastDataYear+1):100)
    new1 <-(1-exp(-new/tau))/(new/tau)
    new2 <- new1 - exp(-new/tau)
    new <- data.frame(input1 = new1, input2 = new2)
    newIncs <-predict(quadMod,new)
    output[,i] <- c(rep(-3, firstDataYear-19), curData$income, newIncs)
  }
  else{
    output[,i] <- rep(-3, 82)
  }
}  
colnames(output)=ENROLL_DATA$PUBID_1997
write.csv(output, "C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/individualoutput.csv")
write.csv(coeffVect, "C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/individualcoefficients.csv")

#project with fixed b1
tau = 27.8818
b1 = 29332 #same result if you do them all together
coeffVect = data.frame(matrix(ncol = 5, nrow = dim(ENROLL_DATA)[1]))
colnames(coeffVect)=c("b0","b1","b2", "R2", "NumObservations")
output = data.frame(matrix(ncol = length(ENROLL_DATA), nrow = 82))
for (i in 1:nrow(ENROLL_DATA)){
  if (length(incomeVectList[[i]])>2){
    curData = data.frame(age = ageVectList[[i]], income = incomeVectList[[i]], enroll = enrollVectList[[i]])
    numObs = length(ageVectList[[i]])
    input1 = (1-exp(-curData$age/tau))/(curData$age/tau)
    input2 = input1 - exp(-curData$age/tau)
    adjoutput = curData$income - b1 * input1
    quadMod <- lm(adjoutput~input2)
    coeffVect[i,1] = quadMod$coefficients[1]
    coeffVect[i,2] = b1
    coeffVect[i,3]= quadMod$coefficients[2]
    coeffVect[i,4]= summary(quadMod)$r.squared
    coeffVect[i,5]= numObs
    firstDataYear = ageVectList[[i]][1]
    lastDataYear = ageVectList[[i]][length(ageVectList[[i]])]
    new <-  c((lastDataYear+1):100)
    new1 <-(1-exp(-new/tau))/(new/tau)
    new2 <- new1 - exp(-new/tau)
    new <- data.frame(input2 = new2)
    newIncs <-predict(quadMod,new)
    newIncs <- newIncs + b1 * new1
    output[,i] <- c(rep(-3, firstDataYear-19), curData$income, newIncs)
  }
  else{
    output[,i] <- rep(-3, 82)
  }
}  
colnames(output)=ENROLL_DATA$PUBID_1997
write.csv(output, "C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/individualoutput.csv")
write.csv(coeffVect, "C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/individualcoefficients.csv")

#project with fixed relationship between b2 and b0
tau = 27.8818
m = -2.8149
b = 36241
coeffVect = data.frame(matrix(ncol = 5, nrow = dim(ENROLL_DATA)[1]))
colnames(coeffVect)=c("b0","b1" ,"b2", "R2", "NumObservations")
output = data.frame(matrix(ncol = length(ENROLL_DATA), nrow =82))
for (i in 1:nrow(ENROLL_DATA)){
  if (length(incomeVectList[[i]])>2){
    curData = data.frame(age = ageVectList[[i]], income = incomeVectList[[i]], enroll = enrollVectList[[i]])
    numObs = length(ageVectList[[i]])
    input1 = (1-exp(-curData$age/tau))/(curData$age/tau)
    input2 = input1 - exp(-curData$age/tau)
    quadMod <- lm(curData$income~0+ input1 + input2)
    gamma = quadMod$coefficients[2]
    b0= (gamma-b)/m
    coeffVect[i,1] = b0
    coeffVect[i,2] = quadMod$coefficients[2]
    coeffVect[i,3]= (m-1)*b0+b
    coeffVect[i,4]= summary(quadMod)$r.squared
    coeffVect[i,5]= numObs
    firstDataYear = ageVectList[[i]][1]
    lastDataYear = ageVectList[[i]][length(ageVectList[[i]])]
    new <-  c((lastDataYear+1):100)
    new1 <-(1-exp(-new/tau))/(new/tau)
    new2 <- new1 - exp(-new/tau)
    new <- data.frame(input1 = new1, input2 = new2)
    newIncs <-predict(quadMod,new)
    output[,i] <- c(rep(-3, firstDataYear-19), curData$income, newIncs)
  }
  else{
    output[,i] <- rep(-3,82)
  }
}  
colnames(output)=ENROLL_DATA$PUBID_1997
write.csv(output, "C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/individualoutput.csv")
write.csv(coeffVect, "C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/individualcoefficients.csv")

#project with fixed b1 and fixed relationship between b2 and b0
tau = 27.8818
b1 = 29332 #same result if you do them all together
m = -2.8149
b = 36241
coeffVect = data.frame(matrix(ncol = 6, nrow = dim(ENROLL_DATA)[1]))
colnames(coeffVect)=c("Gamma","b0","b1" ,"b2", "R2", "NumObservations")
output = data.frame(matrix(ncol = length(ENROLL_DATA), nrow = 82))
for (i in 1:nrow(ENROLL_DATA)){
  if (length(incomeVectList[[i]])>2){
    curData = data.frame(age = ageVectList[[i]], income = incomeVectList[[i]], enroll = enrollVectList[[i]])
    numObs = length(ageVectList[[i]])
    input1 = (1-exp(-curData$age/tau))/(curData$age/tau)
    input2 = input1 - exp(-curData$age/tau)
    adjoutput = curData$income - b1 * input1
    quadMod <- lm(adjoutput~0+input2)
    gamma = quadMod$coefficients[1]
    b0= (gamma-b)/m
    coeffVect[i,1] = gamma
    coeffVect[i,2] = b0
    coeffVect[i,3] = b1
    coeffVect[i,4]= (m-1)*b0+b
    coeffVect[i,5]= summary(quadMod)$r.squared
    coeffVect[i,6]= numObs
    firstDataYear = ageVectList[[i]][1]
    lastDataYear = ageVectList[[i]][length(ageVectList[[i]])]
    new <-  c((lastDataYear+1):100)
    new1 <-(1-exp(-new/tau))/(new/tau)
    new2 <- new1 - exp(-new/tau)
    new <- data.frame(input1 = new1, input2 = new2)
    newIncs <-predict(quadMod,new)
    newIncs <- newIncs + b1 * new1
    output[,i] <- c(rep(-3, firstDataYear-19), curData$income, newIncs)
  }
  else{
    output[,i] <- rep(-3, 82)
  }
}  
colnames(output)=ENROLL_DATA$PUBID_1997
write.csv(output, "C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/individualoutput.csv")
write.csv(coeffVect, "C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/individualcoefficients.csv")