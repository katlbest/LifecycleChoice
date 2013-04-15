library(plyr)
library(ggplot2)

#data i/o=======================================================================
INCOME_DATA <- read.csv("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/INCOME_DATA.csv")
#this file has a lot of the information necessary for projections;can always add needed columns from compileddata_allways.csv
#setup variables================================================================
INCOME_DATA$LAB_1996b <- 0
INCOME_DATA$LAB_1997b <- 0
INCOME_DATA$LAB_1998b <- 0
INCOME_DATA$LAB_1999b <- 0
INCOME_DATA$LAB_2000b <- 0
INCOME_DATA$LAB_2001b <- 0
INCOME_DATA$LAB_2002b <- 0
INCOME_DATA$LAB_2003b <- 0
INCOME_DATA$LAB_2004b <- 0
INCOME_DATA$LAB_2005b <- 0
INCOME_DATA$LAB_2006b <- 0
INCOME_DATA$LAB_2007b <- 0
INCOME_DATA$LAB_2008b <- 0
INCOME_DATA$LAB_2009b <- 0

#functions=====================================================================
getTotal <- function(data, indicator, mainvar, secondvar, clarify, lookupType) { #data frame, indicator var name, main var name, secondary var name, clarify var name or none
  totalVect <- rep(0, nrow(data))
  for (j in 1:nrow(data)){
    #if indicator is positive
    if (clarify == "None"){#this means we dont have a clarifying question
      realIndicator = data[j, indicator]
    } else {
      realIndicator = max(data[j, indicator],data[j, clarify])
    }
    if (realIndicator == 1){#some income received
      if (data[j, mainvar] > 0){
        totalVect[j]= data[j, mainvar]
      }
      else if(data[j, secondvar] > 0){
        totalVect[j]= lookupCategory(lookupType, data[j, secondvar])
      }
    }
  }
  invisible(return(totalVect))
}

lookupCategory <- function(varType, curValue){ #varType 1 =  biggest range, 3 = smallers
  if (varType ==1){
    lookupVect = c(2501, 7501, 17501, 37501, 75001, 175001, 300000)
  } else if (varType ==2){
    lookupVect = c(251, 751, 1751, 3751, 6251, 8751, 12000)
  } else if (varType == 3){
    lookupVect = c(251, 751, 1751, 3751, 6251, 8751, 12000)
  } else {
    lookupVect =  rep(0, 7)
    print("Error: Unknown variable lookup type")
  }
  if (curValue < 0){
    invisible(return(0))
  } else {
    invisible(return(lookupVect[curValue]))
  }
}

#POPULATE INCOMES WITH IGNORING MISSINGS==========================================================
#populate incomes 1996

#syntax: data frame, indicator, main variable, secondary (refuser) variable, clarification question indicator or "None"
salary_96 = getTotal(INCOME_DATA, "P5_010_1997", "P5_016_1997", "P5_017_1997", "P5_011_1997", 1)
INCOME_DATA$LAB_1996b <- salary_96

#populate incomes 1997-2010
#syntax: data frame, indicator, main variable, secondary (refuser) variable, clarification question indicator or "None"
year_vect = c("1997","1998","1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010")
for (i in 1:length(year_vect)){
  #salary
  salIndicator=paste("YINC_1400_", year_vect[i], sep = "") #this only exists until 2002
  salMain=paste("YINC_1700_", year_vect[i], sep = "")
  salSecond=paste("YINC_1800_", year_vect[i], sep = "")
  salClarify=paste("YINC_1500_", year_vect[i], sep = "") 
  if (as.integer(year_vect[i])< 2003){
    salary_cur = getTotal(INCOME_DATA, salIndicator, salMain, salSecond, salClarify, 1)
  } else {
    salary_cur = getTotal(INCOME_DATA, salIndicator, salMain, salSecond, "None", 1)
  }
  
  #salary from two years ago
  if (as.integer(year_vect[i])> 1998){
    salTwoYearInd=paste("YINC_1400A_", year_vect[i], sep = "")
    salTwoYearMain = paste("YINC_1700A_", year_vect[i], sep = "") 
    salTwoYearSecond = paste("YINC_1800A_", year_vect[i], sep = "")
    salary_twoyear = getTotal(INCOME_DATA, salTwoYearInd, salTwoYearMain, salTwoYearSecond, "None", 1)
  }
  
  
  outString = paste("LAB_", toString(as.integer(year_vect[i])-1),"b", sep = "") #store in last year's income variable
  INCOME_DATA[,outString]<- salary_cur
  #print(paste("salary",year_vect[i], salary_cur, sep = ","))
  #print(paste("farm",year_vect[i], farm_cur, sep = ","))
  #print(paste("other",year_vect[i], other_cur, sep = ","))
  
  if (as.integer(year_vect[i])> 1998){
    outString2 = paste("LAB_", toString(as.integer(year_vect[i])-2),"b", sep = "") #store in last year's income variable
    for (k in 1: nrow(INCOME_DATA)){
      INCOME_DATA[k,outString2]<- max(0,INCOME_DATA[k,outString2]) + salary_twoyear[k]
    }
  }
}

#replace all negative numbers with negative three
year_vect = c("1996","1997","1998","1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009")
for (i in 1:length(year_vect)){
  outString = paste("LAB_", toString(as.integer(year_vect[i])),"b", sep = "")
  INCOME_DATA[,outString][INCOME_DATA[,outString]<0] = -3
}

#populate enrollment
  INCOME_DATA$enroll2<-0
  INCOME_DATA$enroll3<-0
  INCOME_DATA$enroll4<-0
  INCOME_DATA$enroll5<-0
  INCOME_DATA$enroll6<-0
  INCOME_DATA$enroll7<-0
  INCOME_DATA$enroll8<-0
  INCOME_DATA$enroll9<-0
  INCOME_DATA$stillInSchool<--3
  INCOME_DATA$lastSchoolType<--3
  
  #populate
  year_vect = c("1997","1998","1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010")
  for (i in 1:length(year_vect)){
    intYear = as.integer(year_vect[i])
    if (intYear < 2005 & intYear != 1997){
      enrollVar = paste("CV_ENROLLSTAT_EDT_", year_vect[i], sep = "")
    } else {
      enrollVar = paste("CV_ENROLLSTAT_", year_vect[i], sep = "")
    }
    
    for (j in 1:nrow(INCOME_DATA)){
      yearNum = intYear-INCOME_DATA$START_YEAR[j]
      if (yearNum >= 0 & yearNum < 8){#this means you are in a year that is relevant for this person
        counter = yearNum +2
        #set corresponding variable
        pasteStr = paste('enroll',counter, sep = "")
        if (INCOME_DATA[j, enrollVar] <0){
          INCOME_DATA[j,pasteStr] <- INCOME_DATA[j, enrollVar] #keep missingness information
        }
        else if (INCOME_DATA[j, enrollVar] <8){
          INCOME_DATA[j,pasteStr] <- 0 #not enrolled
        }
        else {
          INCOME_DATA[j,pasteStr] <- 1 #enrolled
          if (i == length(year_vect)){
            INCOME_DATA[j,"lastSchoolType"] <- INCOME_DATA[j, enrollVar]
          }
        }
      }
    }
  }
  
  #fill in missing enroll values
  startEnroll = grep("enroll2", colnames(INCOME_DATA))
  #endEnroll = startEnroll + 7
  for (j in 1:nrow(INCOME_DATA)){
    changing = 0
    last = 0
    enrollVect = c(INCOME_DATA[j,startEnroll], INCOME_DATA[j,startEnroll+1], INCOME_DATA[j,startEnroll+2], INCOME_DATA[j,startEnroll+ 3], INCOME_DATA[j,startEnroll+4], INCOME_DATA[j,startEnroll+5], INCOME_DATA[j,startEnroll+6], INCOME_DATA[j,startEnroll+7])
    for (i in 2:length(enrollVect)){
      if (changing ==0)  {
        if (enrollVect[i-1]>=0 & enrollVect[i]<0){
          last = enrollVect[i-1]
          change_start_ind = i
          changing = 1
          #print(paste("starting at ",i, sep = ""))
        }
      }
      else{
        if (enrollVect[i]== last){
          for (k in (change_start_ind:i)){
            enrollVect[k] = last
          }
          changing = 0
        }
        else if (enrollVect[i] >= 0){
          changing = 0
        }
      }
    }
    for (i in 1:length(enrollVect)){
      INCOME_DATA[j,startEnroll+i-1]<-enrollVect[i]
    }
    i = length(enrollVect)
    while (INCOME_DATA$stillInSchool[j] <0 & i < 9){
      INCOME_DATA$stillInSchool[j] = max(INCOME_DATA$stillInSchool[j], INCOME_DATA[j,startEnroll+i-1])
      i = i+1
    }
    if (INCOME_DATA$stillInSchool[j] ==-3){
      INCOME_DATA$stillInSchool[j]<- 0
    }
  }

#add in attendance category data =============================================================
CATEGORY_DATA <- read.csv("C://Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/categories.csv")
INCOME_DATA2 <- merge(x = CATEGORY_DATA, y = INCOME_DATA, by = "PUBID_1997", all.x = TRUE)

#adjust timing of income data =======================================================================
#INCOME_DATA2 <- read.csv("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/INCOME_DATA2input.csv")
INCOME_DATA2$START_YEAR <- 0
INCOME_DATA2$znm1 <- 0
INCOME_DATA2$znm2 <- 0
INCOME_DATA2$znm3 <- 0
INCOME_DATA2$znm4 <- 0
INCOME_DATA2$znm5 <- 0
INCOME_DATA2$znm6 <- 0
INCOME_DATA2$znm7 <- 0
INCOME_DATA2$znm8 <- 0

#if not an attender, first year of earning is choice year plus 1
INCOME_DATA2[INCOME_DATA2$Best.Attended == -3,]$START_YEAR <- INCOME_DATA2[INCOME_DATA2$Best.Attended == -3,]$CHOICE_YEAR +1 
#otherwise, it is school ID year
INCOME_DATA2[INCOME_DATA2$Best.Attended != -3,]$START_YEAR <- INCOME_DATA2[INCOME_DATA2$Best.Attended != -3,]$COLLEGEID_YEAR2 +1

for (i in 1:nrow(INCOME_DATA2)){
  curStrNM = paste('LAB_',toString(INCOME_DATA2$START_YEAR[i]),'b',sep = "") #pulls data with no missing value
  colIndexNM = grep(curStrNM, colnames(INCOME_DATA2))[1] #use the first occurence
  firstIndexNM = grep("LAB_1996b", colnames(INCOME_DATA2)) #use if doing no missing
  startYsNM = grep("znm1", colnames(INCOME_DATA2))
  for (j in 0:7){
    if (colIndexNM + j <= firstIndexNM + 13){ 
      INCOME_DATA2[i,startYsNM+j] <- INCOME_DATA2[i, colIndexNM + j]
    }
    else {
      INCOME_DATA2[i,startYsNM+j] <- -4 
    }
  }
}
  
#write.csv(INCOME_DATA2, "C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/allindividuals.csv")
write.csv(INCOME_DATA2[c("PUBID_1997","znm1","znm2","znm3","znm4","znm5","znm6","znm7","znm8")], "C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/testconservative.csv")

#begin creation of individual income data ==============================================
ENROLL_DATA<-INCOME_DATA2
#functions===============================================================================================
removeZeros <- function(IncomeVectorList, AgeVectorList, EnrollmentVectorList, EmploymentVectorList){ #this one does not change the intermediate values
  outInc = list()
  outAge = list()
  outEnroll = list()
  outEmploy= list()
  for (i in 1:length(IncomeVectorList)){
    firstBigIndex = 0
    outInc[[i]] = IncomeVectorList[[i]]
    outAge[[i]] = AgeVectorList[[i]]
    outEnroll[[i]] = EnrollmentVectorList[[i]]
    outEmploy[[i]]= EmploymentVectorList[[i]]
    if (length(IncomeVectorList[[i]]) >1){ #we dont have a -3
      if (IncomeVectorList[[i]][1]>=10000){ #changed this to 10000, start with first 10000 value
        firstBigIndex =1
      }
      for (j in 2:length(IncomeVectorList[[i]])){
        if (IncomeVectorList[[i]][j]<IncomeVectorList[[i]][j-1]){
          IncomeVectorList[[i]][j]=IncomeVectorList[[i]][j-1]
        }
      }
      for (j in 2:length(IncomeVectorList[[i]])){
        if (firstBigIndex == 0){
          if (IncomeVectorList[[i]][j] >= 10000){
            firstBigIndex = j
          }
        }
      }
      if (firstBigIndex ==0){
        outInc[[i]]= c(-3)
        outAge[[i]]= c(-3)
        outEnroll[[i]]= c(-3)
        outEmploy[[i]]= c(-3)
      }
      else{
        outInc[[i]]= IncomeVectorList[[i]][firstBigIndex:length(IncomeVectorList[[i]])]
        outAge[[i]]= AgeVectorList[[i]][firstBigIndex:length(AgeVectorList[[i]])]
        outEnroll[[i]]= EnrollmentVectorList[[i]][firstBigIndex:length(EnrollmentVectorList[[i]])]
        outEmploy[[i]]= EmploymentVectorList[[i]][firstBigIndex:length(EmploymentVectorList[[i]])]
      }
    }
    else{
      outInc[[i]]= IncomeVectorList[[i]][firstBigIndex:length(IncomeVectorList[[i]])]
      outAge[[i]]= AgeVectorList[[i]][firstBigIndex:length(AgeVectorList[[i]])]
      outEnroll[[i]]= EnrollmentVectorList[[i]][firstBigIndex:length(EnrollmentVectorList[[i]])]
      outEmploy[[i]]= EmploymentVectorList[[i]][firstBigIndex:length(EmploymentVectorList[[i]])]
    }
  }
  outList = list(outInc, outAge, outEnroll, outEmploy)
  return(outList)
}

removeNotFT <- function(IncomeVectorList, AgeVectorList, EnrollmentVectorList, EmploymentVectorList){ #this one does not change the intermediate values
  outInc = list()
  outAge = list()
  outEnroll = list()
  outEmploy= list()
  for (i in 1:length(IncomeVectorList)){
    outInc[[i]] = IncomeVectorList[[i]]
    outAge[[i]] = AgeVectorList[[i]]
    outEnroll[[i]] = EnrollmentVectorList[[i]]
    outEmploy[[i]]= EmploymentVectorList[[i]]
    if (length(IncomeVectorList[[i]]) >1){ #we dont have a -3
      removeVect = c()
      for (j in 1:length(IncomeVectorList[[i]])){
        if (EmploymentVectorList[[i]][j]<1200){# did not work enough hours
          outInc[[i]][j] = -3
          #outAge[[i]][j] = -3 # we dont change htis one so that we can index properly at end
          outEnroll[[i]][j] = -3
          outEmploy[[i]][j] = -3
          #removeVect[length(removeVect)+1]<- -j
        }
      }
      #outInc[[i]] = outInc[[i]][removeVect]
      #outAge[[i]] = AgeVectorList[[i]][removeVect]
      #outEnroll[[i]] = EnrollmentVectorList[[i]][removeVect]
      #outEmploy[[i]]= EmploymentVectorList[[i]][removeVect]
    }
  }
  outList = list(outInc, outAge, outEnroll, outEmploy)
  return(outList)
}

#set up vector lists of input data==============================================================================
ageVectListLabNm <- list()
incomeVectListLabNm <- list()
enrollVectListLabNm <- list()
employVectListLabNm <- list()

ageVectListLabNmEmploy <- list()
incomeVectListLabNmEmploy <- list()
enrollVectListLabNmEmploy <- list()
employVectListLabNmEmploy <- list()

#populate list===============
EMPLOY_DATA <- read.csv("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/income/hrsworked.csv")
ENROLL_DATA <- merge(x = ENROLL_DATA, y = EMPLOY_DATA, by = "PUBID_1997", all.x = TRUE)

for (i in 1:nrow(ENROLL_DATA)){
  
  incVectLabNm = c(ENROLL_DATA$znm1[i], ENROLL_DATA$znm2[i], ENROLL_DATA$znm3[i], ENROLL_DATA$znm4[i], ENROLL_DATA$znm5[i], ENROLL_DATA$znm6[i],ENROLL_DATA$znm7[i],ENROLL_DATA$znm8[i])
  enrollVect = c(ENROLL_DATA$enroll2[i], ENROLL_DATA$enroll3[i], ENROLL_DATA$enroll4[i], ENROLL_DATA$enroll5[i], ENROLL_DATA$enroll6[i], ENROLL_DATA$enroll7[i], ENROLL_DATA$enroll8[i], ENROLL_DATA$enroll9[i])
  
  #set up employment vector
  employVect = c()
  curStr= paste('CVC_HOURS_WK_YR_ALL_',toString(ENROLL_DATA$START_YEAR[i]),'_XRND',sep = "")
  colIndex = grep(curStr, colnames(ENROLL_DATA))[1] #use the first occurence
  for (j in 0:7){
    if (colIndex + j <= ncol(ENROLL_DATA)){ #have to make sure that we aren't out of bounds!! these should be last columns since i added them
      curEmploy = ENROLL_DATA[i, colIndex+j]
      employVect[length(employVect)+1]= curEmploy
    }
    else {
      employVect[length(employVect)+1]= -3
    }
  }
  
  #store in vector list
  incomeVectListLabNm[[i]]<-incVectLabNm
  ageVectListLabNm[[i]]<-c(1:length(incVectLabNm))
  enrollVectListLabNm[[i]]<-enrollVect
  employVectListLabNm[[i]]<-employVect
}

#must add 18 to each age number so that tau is correct==========================
for (i in 1:length(ageVectListLabNm)){
  if (ageVectListLabNm[[i]][1]>0){
    ageVectListLabNm[[i]] = ageVectListLabNm[[i]] + 18
  }
}

#set to missing if only zeros available======================
for (i in 1:length(incomeVectListLabNm)){
  #set to missing if only zeros available
  if (sum(incomeVectListLabNm[[i]])<= 0){
    incomeVectListLabNm[[i]]<-c(-3)
    ageVectListLabNm[[i]]<-c(-3)
    enrollVectListLabNm[[i]]<-c(-3)
    employVectListLabNm[[i]]<-c(-3)
  }
}

#remove entries where not much work occured=======================
LabEmployReturn <-removeNotFT(incomeVectListLabNm, ageVectListLabNm, enrollVectListLabNm, employVectListLabNm)
incomeVectListLabEmploy<-LabEmployReturn[[1]]
ageVectListLabEmploy<-LabEmployReturn[[2]]
enrollVectListLabEmploy<-LabEmployReturn[[3]]
employVectListLabEmploy <- LabEmployReturn[[4]]

save.image(file="commonStartingPoint.RData")
#load("commonStartingPoint.RData")

#OPTION1 if lower than previous, set to previous, and remove leading zeros======================
#remove entries below 1000
#NOTE: this leaves you with vectors without zeros
LabNmReturn <- removeZeros(incomeVectListLabNm, ageVectListLabNm, enrollVectListLabNm, employVectListLabNm)
incomeVectListLabNm<-LabNmReturn[[1]]
ageVectListLabNm<-LabNmReturn[[2]]
enrollVectListLabNm<-LabNmReturn[[3]]
employVectListLabNm<-LabNmReturn[[4]]

LabEmployReturn <-removeZeros(incomeVectListLabEmploy, ageVectListLabEmploy, enrollVectListLabEmploy, employVectListLabEmploy)
incomeVectListLabEmploy<-LabEmployReturn[[1]]
ageVectListLabEmploy<-LabEmployReturn[[2]]
enrollVectListLabEmploy<-LabEmployReturn[[3]]
employVectListLabEmploy<- LabEmployReturn[[4]]

#Project======================================================================================
#project with fixed relationship between b2 and b0
#set up
#LabNm
  #ageVectList <- ageVectListLabNm
  #incomeVectList <- incomeVectListLabNm
  #enrollVectList <- enrollVectListLabNm
  #employVectList <- employVectListLabNm
#LabEmploy
  ageVectList <- ageVectListLabEmploy
  incomeVectList <- incomeVectListLabEmploy
  enrollVectList <- enrollVectListLabEmploy
  employVectList <- employVectListLabEmploy

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
  curData = data.frame(age = ageVectList[[i]], income = incomeVectList[[i]], enroll = enrollVectList[[i]])
  curData[curData == -3] <- NA 
  curData<- na.exclude(curData)
  if (dim(curData)[1]>2){
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
    outMatrix[,i] <- c(rep(-3, firstDataYear-19), incomeVectList[[i]], newIncs)
  }
  else{
    outMatrix[,i] <- rep(-3,82)
  }
}  
colnames(outMatrix)=ENROLL_DATA$PUBID_1997
write.csv(outMatrix, "C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/individualoutput.csv")
write.csv(coeffVect, "C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/individualcoefficients.csv")

#store coefficient lists and output matrices (manual)
  coeffVectNmFillMiddle<- coeffVect
  outMatrixNmFillMiddle <- outMatrix
  coeffVectEmployFillMiddle<- coeffVect
  outMatrixEmployFillMiddle <- outMatrix

  coeffVectNmNoFill<- coeffVect
  outMatrixNmNoFill <- outMatrix
  coeffVectEmployNoFill<- coeffVect
  outMatrixEmployNoFill <- outMatrix

  #save this workspace for later loading
  save.image(file="alloptions.RData")

#check which method predicts (if any)=====================================================================
  #add all possibilities to data set
  ENROLL_DATA$b0NmFillMiddle<- coeffVectNmFillMiddle[,1]
  ENROLL_DATA$b0EmployFillMiddle<-coeffVectEmployFillMiddle[,1]
  ENROLL_DATA$b0NmNoFill<-coeffVectNmNoFill[,1]
  ENROLL_DATA$b0EmployNoFill<-coeffVectEmployNoFill[,1]
  ENROLL_DATA$cat <- -3
  for (i in 1:nrow(ENROLL_DATA)){
    ENROLL_DATA$cat[i] <- toString(paste(ENROLL_DATA$BestAd5b[i],ENROLL_DATA$BestAtt5b[i], sep = ""))
  }
  ENROLL_DATA[is.na(ENROLL_DATA)] <- -3

  #do check for each data set type
  source("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Data manipulation/fun_checkPredictionAbility.R")

  checkPredictionAbility(b0NmFillMiddle, ENROLL_DATA$b0NmFillMiddle, "b0NmFillMiddle")
  checkPredictionAbility(b0EmployFillMiddle, ENROLL_DATA$b0EmployFillMiddle, "b0EmployFillMiddle")
  checkPredictionAbility(b0NmNoFill, ENROLL_DATA$b0NmNoFill, "b0NmNoFill")
  checkPredictionAbility(b0EmployNoFill, ENROLL_DATA$b0EmployNoFill, "b0EmployNoFill")
