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
incomeVectListM<-MReturn[[1]]
ageVectListM<-MReturn[[2]]
enrollVectListM<-MReturn[[3]]

LabNmReturn <-removeZeros(incomeVectListLabNm, ageVectListLabNm, enrollVectListLabNm)
incomeVectListLabNm<-LabNmReturn[[1]]
ageVectListLabNm<-LabNmReturn[[2]]
enrollVectListLabNm<-LabNmReturn[[3]]

LabMReturn <-removeZeros(incomeVectListLabM, ageVectListLabM, enrollVectListLabM)
incomeVectListLabM<-LabMReturn[[1]]
ageVectListLabM<-LabMReturn[[2]]
enrollVectListLabM<-LabMReturn[[3]]

#Project======================================================================================
#project with fixed relationship between b2 and b0
#set up
  #Nm
    #ageVectList <- ageVectListNm
    #incomeVectList <- incomeVectListNm
    #enrollVectList <- enrollVectListNm
  #M
    #ageVectList <- ageVectListM
    #incomeVectList <- incomeVectListM
    #enrollVectList <- enrollVectListM
  #LabNm
    #ageVectList <- ageVectListLabNm
    #incomeVectList <- incomeVectListLabNm
    #enrollVectList <- enrollVectListLabNm
  #LabM
    ageVectList <- ageVectListLabM
    incomeVectList <- incomeVectListLabM
    enrollVectList <- enrollVectListLabM

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

#store all four coefficient lists
#coeffVectNmRaw<- coeffVect
write.csv(coeffVectNmRaw, "C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/coeffVectNmRaw.csv")
#coeffVectMRaw<- coeffVect
write.csv(coeffVectMRaw, "C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/coeffVectMRaw.csv")
#coeffVectLabNmRaw<- coeffVect
write.csv(coeffVectLabNmRaw, "C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/coeffVectLabNmRaw.csv")
#coeffVectLabMRaw<- coeffVect
write.csv(coeffVectLabMRaw, "C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/coeffVectLabMRaw.csv")

#coeffVectNmFilled<- coeffVect
write.csv(coeffVectNmFilled, "C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/coeffVectNmFilled.csv")
#coeffVectMFilled<- coeffVect
write.csv(coeffVectMFilled, "C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/coeffVectMFilled.csv")
#coeffVectLabNmFilled<- coeffVect
write.csv(coeffVectLabNmFilled, "C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/coeffVectLabNmFilled.csv")
#coeffVectLabMFilled<- coeffVect
write.csv(coeffVectLabMFilled, "C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/coeffVectLabMFilled.csv")


#store all eight outmatrixes and place in list
#outMatrixNmRaw <- outMatrix
write.csv(outMatrixNmRaw, "C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/outNmRaw.csv")
#outMatrixMRaw <- outMatrix
write.csv(outMatrixMRaw, "C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/outMRaw.csv")
#outMatrixLabNmRaw <- outMatrix
write.csv(outMatrixLabNmRaw, "C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/outLabNmRaw.csv")
#outMatrixLabMRaw <- outMatrix
write.csv(outMatrixLabMRaw, "C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/outLabMRaw.csv")

#outMatrixNmFilled <- outMatrix
write.csv(outMatrixNmFilled, "C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/outNmFilled.csv")
#outMatrixMFilled <- outMatrix
write.csv(outMatrixMFilled, "C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/outMFilled.csv")
#outMatrixLabNmFilled <- outMatrix
write.csv(outMatrixLabNmFilled, "C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/outLabNmFilled.csv")
#outMatrixLabMFilled <- outMatrix
write.csv(outMatrixLabMFilled, "C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/outLabMFilled.csv")

outMatrixList = list(outMatrixMRaw, outMatrixNmRaw,outMatrixLabMRaw,outMatrixLabNmRaw,outMatrixMFilled,outMatrixNmFilled,outMatrixLabMFilled,outMatrixLabNmFilled)
#outMatrixList = list(outMatrixMFilled,outMatrixNmFilled,outMatrixLabMFilled,outMatrixLabNmFilled,outMatrixMRaw, outMatrixNmRaw,outMatrixLabMRaw,outMatrixLabNmRaw)

#save this workspace for later loading
#save.image(file="inddata.RData")
#load("inddata.RData")

#create best estimate for each person====================================================
incomeEstimate<- list()
incomeb0 <- list()
b0Min = -500000 #upper limit of about 140K top salary
b0Max = -100 #lower limit of about 12K top salary
  
for (i in 1:length(incomeVectList)){
  b0Vect <- c(coeffVectMRaw[i,1], coeffVectNmRaw[i,1],coeffVectLabMRaw[i,1],coeffVectLabNmRaw[i,1],coeffVectMFilled[i,1],coeffVectNmFilled[i,1],coeffVectLabMFilled[i,1],coeffVectLabNmFilled[i,1])
  #b0Vect <- c(coeffVectMFilled[i,1],coeffVectNmFilled[i,1],coeffVectLabMFilled[i,1],coeffVectLabNmFilled[i,1],coeffVectMRaw[i,1], coeffVectNmRaw[i,1],coeffVectLabMRaw[i,1],coeffVectLabNmRaw[i,1])
  useIndex = 0
  for (j in 1:length(b0Vect)){
    if (useIndex == 0 & !(is.na(b0Vect[j]))){
      if ((b0Vect[j]>=b0Min) & (b0Vect[j] <= b0Max)){
        useIndex = j
      }
    }
  }
  if (useIndex >0){
    incomeEstimate[[i]] <- outMatrixList[[useIndex]][[i]]
    incomeb0[[i]]<- b0Vect[useIndex]
  }
  else{
    incomeEstimate[[i]] <- rep(-3, 82)
    incomeb0[[i]]<--3
  }
}

write.csv(incomeEstimate, "C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/incomeEsts.csv")
write.csv(incomeb0, "C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/incomeb0.csv")

#add b0 to data points
ENROLL_DATA$b0 <- unlist(incomeb0)
write.csv(ENROLL_DATA, "C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/datawithb0.csv")

#extract data on differences in betas===========================================
#categories (currently using 5)
ENROLL_DATA$cat <-NA
admit_cats <- c('1','2', '3', '4', '6', '7')
apply_cats <- c('-3','1','2', '3', '4', '6', '7')
cat_vector <- rep(NA, length(admit_cats)*length(apply_cats))
pop_counter <- rep(0, length(cat_vector))
avg_beta0 <- rep(0, length(cat_vector))
var_beta0 <- rep(0, length(cat_vector))
beta0byCat <- list()
length(beta0byCat) <- length(cat_vector)

k=1
for (i in 1:length(admit_cats)){#populate what will be the "lookup vector"
  for (j in 1:length(apply_cats)){
    cat_vector[k]= paste(admit_cats[i],apply_cats[j], sep = "")
    k = k+1
  }
}

for (i in 1:nrow(ENROLL_DATA)){
  curCat = toString(paste(ENROLL_DATA$BestAd5[i],ENROLL_DATA$BestAtt5[i], sep = ""))
  ENROLL_DATA$cat[i]=curCat
  curIndex = match(curCat, cat_vector)
  if (ENROLL_DATA$b0[i]!=-3){
    #add it to corresponding list
    beta0byCat[[curIndex]][[length(beta0byCat[[curIndex]])+1]]<-ENROLL_DATA$b0[i]
  }
}

for (i in 1:length(beta0byCat)){
  curbetalist = beta0byCat[[i]]
  pop_counter[i]<- length(curbetalist)
  if (pop_counter[i]> 0){
    avg_beta0[i] <- sum(curbetalist)
    var_beta0[i] <- var(curbetalist)
  }  
}

outdat <- data.frame(name = cat_vector, number = pop_counter, average = avg_beta0, variance = var_beta0)
write.csv(ENROLL_DATA, "C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/datawithb0.csv")
write.csv(outdat, "C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/b0output.csv")

#regress b0 on category================================================
b0ProjectData <- data.frame(b0 = ENROLL_DATA$b0, cat = ENROLL_DATA$cat, admit = ENROLL_DATA$BestAd5, attend = ENROLL_DATA$BestAtt5)
#clean data--remove 7's so that we have ordinal numbers
#-3 means no school attendance, which is worse than some shcool attendance, so also ordinal
b0ProjectData <- b0ProjectData[b0ProjectData$admit != 7 & b0ProjectData$attend != 7,]
b0ProjectData[b0ProjectData == -3] <- NA #replace with NAs
#factor model
b0ProjectModel <- lm(b0~factor(cat), data=na.exclude(b0ProjectData))
b0ProjectModel <- lm(b0~cat, data=na.exclude(b0ProjectData))
b0ProjectModel <- lm(b0~admit+attend, data=na.exclude(b0ProjectData))
b0ProjectModel <- lm(b0~factor(attend), data=na.exclude(b0ProjectData))
b0ProjectModel <- lm(b0~factor(admit), data=na.exclude(b0ProjectData))

#save this workspace for later loading
#save.image(file="inddata2.RData")
#load("inddata2.RData")

#read in other relevant predictor information
INCOME_PREDS<- read.csv("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/incomepredictors.csv")
ENROLL_DATA2<- merge(x = ENROLL_DATA, y = INCOME_PREDS, by = "PUBID_1997", all.x = TRUE)
COLLEGE_NUM<- read.csv("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/collegenumber.csv")
ENROLL_DATA2<- merge(x = ENROLL_DATA2, y = COLLEGE_NUM, by = "PUBID_1997", all.x = TRUE)
LOC_DATA <- read.csv("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/desensitizedloc.csv")
ENROLL_DATA2<- merge(x = ENROLL_DATA2, y = LOC_DATA, by = "PUBID_1997", all.x = TRUE)
ENROLL_DATA<-ENROLL_DATA2

#populate major
  #use latest major at "real school"
ENROLL_DATA$MAJOR <- -3
for (i in 1:nrow(ENROLL_DATA)){
  colNumString = paste("YSCH_21300", ENROLL_DATA$COLLEGEID_ROSTERNUM2[i], "_", sep ="")
  varVector = c()
  curYear = ENROLL_DATA$COLLEGEID_YEAR2[i]
  yearString = paste("_", ENROLL_DATA$COLLEGEID_YEAR2[i], sep ="")
  termVect = c("01","02", "03", "04","05", "06", "07","08","09","10","11","12", "13")
  for (j in 1:length(termVect)){
    curstr = paste(colNumString, termVect[j], yearString, sep = "")
    if (curstr %in% colnames(ENROLL_DATA)){
      varVector[length(varVector)+1] = curstr
    }
  }
  #now have vector of variables to check, use most recent
  if (length(varVector)>0){
    for (j in length(varVector):1){
      curStr = paste("ENROLL_DATA$", varVector[j], sep = "")
      curMajor = eval(parse(text =curStr))[i]
      if (ENROLL_DATA$MAJOR[i] == -3 & curMajor >= 0){
        ENROLL_DATA$MAJOR[i]= curMajor
      }
    }
  }
  else {
    ENROLL_DATA$MAJOR[i]= -3
  }
}

#prediction with major
b0MajorData <- data.frame(b0 = ENROLL_DATA$b0, major = ENROLL_DATA$MAJOR)
b0MajorData[b0MajorData == -3] <- NA #replace with NAs
#99 means other, 0 means no field
b0MajorData <- b0MajorData[b0MajorData$major != 99 & b0MajorData$major != 0,]
#factor model
b0MajorModel <- lm(b0~factor(major), data=na.exclude(b0MajorData))
hardSci <- c(6, 21, 25)
softSci <- c(3, 10, 11, 31, 32)
bus <- c(7, 8, 9, 13)
health <- c(22, 23, 27, 29, 30, 28)
b0MajorData$major2
#reduce categories
for (i in 1:nrow(b0MajorData)){
  if (b0MajorData$major[i] %in% hardSci){
    b0MajorData$major2[i]= 1
  }
  else if (b0MajorData$major[i] %in% softSci){
    b0MajorData$major2[i]= 2
  }
 else if (b0MajorData$major[i] %in% bus){
   b0MajorData$major2[i]= 3
 }
  else if (b0MajorData$major[i] %in% health){
    b0MajorData$major2[i]= 4
  }
 else if (is.na(b0MajorData$major[i])){
   b0MajorData$major2[i]= -3
 }
  else{
    b0MajorData$major2[i]= 5
  }
}
b0MajorModel2 <- lm(b0~factor(major2), data=na.exclude(b0MajorData))

#populate area of residence
#geo variables are pretty straight forward, GEO03--use desensitized
#get mode, and if all are different use the latest avaialble
ENROLL_DATA$GEO = -3
for (i in 1:nrow(ENROLL_DATA)){
  geoVector = c()
  if (ENROLL_DATA$GEO03_2010[i]>=0){
    geoVector[length(geoVector)+1]= ENROLL_DATA$GEO03_2010[i]
  }
  if (ENROLL_DATA$GEO03_2009[i]>=0){
    geoVector[length(geoVector)+1]= ENROLL_DATA$GEO03_2009[i]
  }
  if (ENROLL_DATA$GEO03_2008[i]>=0){
    geoVector[length(geoVector)+1]= ENROLL_DATA$GEO03_2008[i]
  }
  if (ENROLL_DATA$GEO03_2009[i]>=0){
    geoVector[length(geoVector)+1]= ENROLL_DATA$GEO03_2009[i]
  }
  if (length(geoVector)>0){
    uniques <- unique(geoVector)
    ENROLL_DATA$GEO[i]<- uniques[which.max(tabulate(match(geoVector, uniques)))]
  }
}

b0GeoData <- data.frame(b0 = ENROLL_DATA$b0, geo = ENROLL_DATA$GEO)
b0GeoData[b0GeoData == -3] <- NA #replace with NAs
b0GeoModel <- lm(b0~factor(geo), data=na.exclude(b0GeoData))

#populate GPA
#use the one GPA variable (), #YSCH-7300 (dont worry about recode, etc)
#value of 1 is badm 8 is good, above 8 should be discarded
yearVect  = c("2007", "2006","2005","2004","2003","2002","2001","2000","1999","1998","1997")
ENROLL_DATA$GRADES = -3
for (i in 1:nrow(ENROLL_DATA)){
  for (j in 1:length(yearVect)){
    curStr = paste("ENROLL_DATA$YSCH_7300_", yearVect[j], sep = "")
    curValue  = eval(parse(text =curStr))[i]
    if (ENROLL_DATA$GRADES[i] == -3 & curValue >= 0 & curValue <9){
      ENROLL_DATA$GRADES[i] = curValue
    }
  }
}

b0GPAData <- data.frame(b0 = ENROLL_DATA$b0, gpa = ENROLL_DATA$GRADES)
b0GPAData[b0GPAData == -3] <- NA #replace with NAs
b0GPAModel <- lm(b0~factor(gpa), data=na.exclude(b0GPAData))
b0GPAModel <- lm(b0~gpa, data=na.exclude(b0GPAData))

#try to get industry in addition to major??
#YEMP_55505_COD or YEMP_INDCODE-2002, but lots of employers, etc.

#populate data on schooling completion (CVC_HIGHEST_DEGREE_EVER_XRND)
ENROLL_DATA$COLLEGECOMPLETED = -3
for (i in 1:nrow(ENROLL_DATA)){
  if (ENROLL_DATA$stillInCollege[i]==0){
    if (ENROLL_DATA$CVC_HIGHEST_DEGREE_EVER_XRND[i]>3){ #4 is bachelors
      ENROLL_DATA$COLLEGECOMPLETED[i] = 1
    }
    else{
      ENROLL_DATA$COLLEGECOMPLETED[i] = 0
    }
  }
}

b0GradData <- data.frame(b0 = ENROLL_DATA$b0, grad = ENROLL_DATA$COLLEGECOMPLETED)
b0GradData[b0GradData == -3] <- NA #replace with NAs
b0GradModel <- lm(b0~factor(grad), data=na.exclude(b0GradData))

#project with all ifno============================================================
b0ProjectData <- data.frame(b0 = ENROLL_DATA$b0, cat = ENROLL_DATA$cat, admit = ENROLL_DATA$BestAd5, attend = ENROLL_DATA$BestAtt5, major = ENROLL_DATA$MAJOR, gpa = ENROLL_DATA$GRADES)
b0ProjectData[b0ProjectData == -3] <- NA #replace with NAs

#get major categories
hardSci <- c(6, 21, 25)
softSci <- c(3, 10, 11, 31, 32)
bus <- c(7, 8, 9, 13)
health <- c(22, 23, 27, 29, 30, 28)
b0ProjectData$major2
b0MajorData <- b0MajorData[b0MajorData$major != 99 & b0MajorData$major != 0,]

#reduce categories
for (i in 1:nrow(b0ProjectData)){
  if (b0ProjectData$major[i] %in% hardSci){
    b0ProjectData$major2[i]= 1
  }
  else if (b0ProjectData$major[i] %in% softSci){
    b0ProjectData$major2[i]= 2
  }
  else if (b0ProjectData$major[i] %in% bus){
    b0ProjectData$major2[i]= 3
  }
  else if (b0ProjectData$major[i] %in% health){
    b0ProjectData$major2[i]= 4
  }
  else if (is.na(b0ProjectData$major[i])){
    b0ProjectData$major2[i]= -3
  }
  else{
    b0ProjectData$major2[i]= 5
  }
}
#clean data--remove 7's so that we have ordinal numbers
b0ProjectData <- b0ProjectData[b0ProjectData$admit != 7 & b0ProjectData$attend != 7,]
#factor model
AllFactor <- lm(b0~factor(cat)+ factor(gpa) + factor(major2), data=na.exclude(b0ProjectData))
AllFactor <- lm(b0~factor(cat)+ factor(major2), data=na.exclude(b0ProjectData))
AllFactor <- lm(b0~factor(admit)+ factor(major2), data=na.exclude(b0ProjectData))
AllFactor <- lm(b0~factor(admit), data=na.exclude(b0ProjectData))
AllFactor <- lm(b0~factor(attend), data=na.exclude(b0ProjectData))


#save this workspace for later loading
#save.image(file="noenrollmentrest.RData")
#load("inddata2.RData")
write.csv(ENROLL_DATA,"C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/noenrollmentrest.csv") 

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