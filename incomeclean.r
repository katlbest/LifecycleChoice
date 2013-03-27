library(plyr)

#notes=======================================================================
#to clear data: rm(list = ls(all = TRUE))
#to replace missing values: MERGED_DATA[MERGED_DATA < 0] <- NA
#sapply(PYTHON_OUT, class)

#data i/o=======================================================================
INCOME_DATA <- read.csv("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/INCOME_DATA.csv")
#store_data <- INCOME_DATA
#INCOME_DATA<- INCOME_DATA[1,]

#setup variables================================================================
INCOME_DATA$INC_1996 <- 0
INCOME_DATA$INC_1997 <- 0
INCOME_DATA$INC_1998 <- 0
INCOME_DATA$INC_1999 <- 0
INCOME_DATA$INC_2000 <- 0
INCOME_DATA$INC_2001 <- 0
INCOME_DATA$INC_2002 <- 0
INCOME_DATA$INC_2003 <- 0
INCOME_DATA$INC_2004 <- 0
INCOME_DATA$INC_2005 <- 0
INCOME_DATA$INC_2006 <- 0
INCOME_DATA$INC_2007 <- 0
INCOME_DATA$INC_2008 <- 0
INCOME_DATA$INC_2009 <- 0

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
     #else if (lookupType ==1){ #we don't use this check
      #totalVect[j]= -100000000 #very negativenumber so it doesn't become positive
      #print("yes")
    #}
  }
    #if (realIndicator==-1 | realIndicator == -2 | realIndicator == -5){ #when this is used, we check that no one refused or don't know for income variable indicator
     # if (lookupType ==1){ 
      #  totalVect[j]= -100000000 #very negativenumber so it doesn't become positive
       # print(paste("Missing data in", toString(j),year_vect[i],sep = ",")) #this doesn't happen, no unjustified skip values
      #}
    #}
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

fillMissing <- function(missingVect, fillVect, logicType){
  counter = 0
  storeVect <- missingVect
  for (j in 1:length(missingVect)){
    if (missingVect[j]==-3){
      if (fillVect[j] > 0){
        useOther = 0
        if (j == 1){
          if(fillVect[j] <= missingVect[j+1]*1.2){
            useOther = 1
          }
        }
        else if (j == length(missingVect)){
          if(missingVect[j-1]*.80 <= fillVect[j]){
            useOther = 1
          }
        }
        else if (logicType == "or"){
          if (missingVect[j-1]*.80 <= fillVect[j] | fillVect[j] <= missingVect[j+1]*1.2){ 
            useOther = 1
          }
        }
        else if (logicType == "and"){
          if (missingVect[j-1]*.80 <= fillVect[j] & fillVect[j] <= missingVect[j+1]*1.2){
            useOther = 1
          }
        }
        if (useOther == 1) {
          storeVect[j] = fillVect[j]
          counter = 1
        }
      }
    } 
  }
  return(c(storeVect, counter))
}

#populate incomes 1996==================================================================
#syntax: data frame, indicator, main variable, secondary (refuser) variable, clarification question indicator or "None"
salary_96 = getTotal(INCOME_DATA, "P5_010_1997", "P5_016_1997", "P5_017_1997", "P5_011_1997", 1)
farm_96 = getTotal(INCOME_DATA, "P5_018_1997", "P5_019_1997", "P5_020_1997", "None", 1)
other_96 = getTotal(INCOME_DATA, "P5_055_1997", "P5_056_1997", "P5_057_1997", "None", 2)+getTotal(INCOME_DATA, "P5_067_1997", "P5_068_1997", "P5_069_1997", "None", 3)+ getTotal(INCOME_DATA, "P5_048_1997", "P5_049_1997", "P5_050_1997", "None", 2)+ getTotal(INCOME_DATA, "P5_052_1997", "P5_053_1997", "P5_054_1997", "None", 2)
INCOME_DATA$INC_1996 <- salary_96+ farm_96 + other_96

#populate incomes 1997-2010==================================================================
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
  
  #farm
  farmIndicator =paste("YINC_1900_", year_vect[i], sep = "") 
  farmMain = paste("YINC_2100_", year_vect[i], sep = "") 
  farmSecond =paste("YINC_2200_", year_vect[i], sep = "") 
  farmClarify = paste("YINC_2000_", year_vect[i], sep = "") 
  
  farm_cur = getTotal(INCOME_DATA,farmIndicator, farmMain, farmSecond, farmClarify, 1)
  #other
  ssIndicator =paste("YINC_7600_", year_vect[i], sep = "") 
  ssMain = paste("YINC_7700_", year_vect[i], sep = "") #i forgot these
  ssSecond =paste("YINC_7800_", year_vect[i], sep = "")
  other_cur = getTotal(INCOME_DATA, ssIndicator, ssMain, ssSecond, "None", 3)
  if (as.integer(year_vect[i])> 2001){
    wcIndicator =paste("YINC_2250_", year_vect[i], sep = "")#start in 2002
    wcMain = paste("YINC_2260_", year_vect[i], sep = "") 
    wcSecond =paste("YINC_2270_", year_vect[i], sep = "")
    other_cur2 =getTotal(INCOME_DATA, wcIndicator, wcMain, wcSecond, "None", 1)
    other_cur = other_cur + other_cur2
  }
  
  outString = paste("INC_", toString(as.integer(year_vect[i])-1), sep = "") #store in last year's income variable
  INCOME_DATA[,outString]<- salary_cur + farm_cur + other_cur
  #print(paste("salary",year_vect[i], salary_cur, sep = ","))
  #print(paste("farm",year_vect[i], farm_cur, sep = ","))
  #print(paste("other",year_vect[i], other_cur, sep = ","))
  
  if (as.integer(year_vect[i])> 1998){
    outString2 = paste("INC_", toString(as.integer(year_vect[i])-2), sep = "") #store in last year's income variable
    for (k in 1: nrow(INCOME_DATA)){
      INCOME_DATA[k,outString2]<- max(0,INCOME_DATA[k,outString2]) + salary_twoyear[k]
    }
  }
}

#write to file ========================================================================
write.csv(INCOME_DATA, "C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/INCOME_DATA2.csv")

#adjust timing of income data =======================================================================
INCOME_DATA2 <- read.csv("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/INCOME_DATA2input.csv")
INCOME_DATA2$START_YEAR <- 0
INCOME_DATA2$y1 <- 0
INCOME_DATA2$y2 <- 0
INCOME_DATA2$y3 <- 0
INCOME_DATA2$y4 <- 0
INCOME_DATA2$y5 <- 0
INCOME_DATA2$y6 <- 0
INCOME_DATA2$y7 <- 0
INCOME_DATA2$y8 <- 0
INCOME_DATA2$ynm1 <- 0
INCOME_DATA2$ynm2 <- 0
INCOME_DATA2$ynm3 <- 0
INCOME_DATA2$ynm4 <- 0
INCOME_DATA2$ynm5 <- 0
INCOME_DATA2$ynm6 <- 0
INCOME_DATA2$ynm7 <- 0
INCOME_DATA2$ynm8 <- 0

#if not an attender, first year of earning is choice year plus 1
INCOME_DATA2[INCOME_DATA2$Best.Attended == -3,]$START_YEAR <- INCOME_DATA2[INCOME_DATA2$Best.Attended == -3,]$CHOICE_YEAR +1 
#otherwise, it is school ID year
INCOME_DATA2[INCOME_DATA2$Best.Attended != -3,]$START_YEAR <- INCOME_DATA2[INCOME_DATA2$Best.Attended != -3,]$COLLEGEID_YEAR2 +1

for (i in 1:nrow(INCOME_DATA2)){
  curStrNM = paste('INC_',toString(INCOME_DATA2$START_YEAR[i]),'b',sep = "") #pulls data with no missing values
  curStr = paste('INC_',toString(INCOME_DATA2$START_YEAR[i]),sep = "") #pulls data with missing values
  colIndex = grep(curStr, colnames(INCOME_DATA2))[1] #use the first occurence
  colIndexNM = grep(curStrNM, colnames(INCOME_DATA2))[1] #use the first occurence
  firstIndex = grep("INC_1996", colnames(INCOME_DATA2))[1] #use if doing with missing
  firstIndexNM = grep("INC_1996b", colnames(INCOME_DATA2)) #use if doing no missing
  startYs = grep("y1", colnames(INCOME_DATA2))
  startYsNM = grep("ynm1", colnames(INCOME_DATA2))
  for (j in 0:7){
    if (colIndex + j <= firstIndex + 13){ #this adjusment needs to be better
      #print(colIndex)
      #print(colIndex+j)
      #print(colnames(INCOME_DATA2)[colIndex+j])
      INCOME_DATA2[i,startYs+j] <- INCOME_DATA2[i, colIndex + j]
      INCOME_DATA2[i,startYsNM+j] <- INCOME_DATA2[i, colIndexNM + j]
    }
    else {
      INCOME_DATA2[i,startYs+j] <- -4
      INCOME_DATA2[i,startYsNM+j] <- -4 
    }
  }
}
write.csv(INCOME_DATA2, "C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/incomeadjusted.csv") #saves with no missing values

#fill in missing income======================================================================
INCOME_DATA2<- read.csv("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/incomeadjusted.csv")
INCOME_DATA2$COMPLETE_INC <- 0
misCount = 0
missCompletelyCount = 0
fixedCount = 0
for (i in 1:nrow(INCOME_DATA2)){
  incVect <- c(INCOME_DATA2$y1[i], INCOME_DATA2$y2[i], INCOME_DATA2$y3[i], INCOME_DATA2$y4[i], INCOME_DATA2$y5[i], INCOME_DATA2$y6[i], INCOME_DATA2$y7[i], INCOME_DATA2$y8[i])
  incVectNM <- c(INCOME_DATA2$ynm1[i], INCOME_DATA2$ynm2[i], INCOME_DATA2$ynm3[i], INCOME_DATA2$ynm4[i], INCOME_DATA2$ynm5[i], INCOME_DATA2$ynm6[i], INCOME_DATA2$ynm7[i], INCOME_DATA2$ynm8[i])
  #drop terminal negative 4's
  for (j in length(incVect):1){
    if (incVect[j]==-4){
      incVect <- incVect[1:length(incVect)-1]
    } 
  }
  if (min(incVect)>=0){
    INCOME_DATA2$COMPLETE_INC[i]<- 1 #this person has complete data
  }
  else { #try to fill in missing values with non-missing ones, pass 1
    returnObj <-fillMissing(incVect, incVectNM, "or") #last argument and indicates you must be between two feasible values, or says you must be next to one
    print(returnObj[1:length(returnObj)-1])
    incVect <- returnObj[1:length(returnObj)-1] #note: a second pass doesn't help if you OR the requirement
    fixedCount = fixedCount + returnObj[length(returnObj)]
  }
  if (min(incVect)>=0){
    INCOME_DATA2$COMPLETE_INC[i]<- 1 #if incVect now has no zeros, we are complete!
  }
  else { #must project income
    misCount = misCount +1
    #check if only terminal values are missing
    checkRestFlag = 0
    lengthCounter = 0
    for (j in length(incVect):1){
      if (incVect[j]==-3){
        checkRest <- max(incVect[min(j+1, length(incVect)):length(incVect)])
        if (checkRest <0){
          checkRestFlag = 1
        }
      } 
      else {
        lengthCounter = lengthCounter + 1
      }
    }
    if (checkRestFlag ==1 & lengthCounter >3){ #has at least four values
      INCOME_DATA2$COMPLETE_INC[i]<- 1 
    }
    else{
      missCompletelyCount = missCompletelyCount +1
    }
  }
}
print(fixedCount) #with OR you fix 127, with AND you only fix 50; number of people for whom at least one enty is filled in
print(missCompletelyCount)
print(misCount)

write.csv(INCOME_DATA2[INCOME_DATA2$COMPLETE_INC ==0,], "C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/projectincome.csv")

#write everyone to file so we can pull in manually updated by vlookup
write.csv(INCOME_DATA2, "C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/allindividuals.csv")
#complete inc is an indicator of whether you should be used for gamma projetion (have at least 4 entries, not necessarly starting at beginning)

#calculate growth rates==============================================================================
PROJECT_DATA<- read.csv("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/allindividuals-incomefilled.csv")
#repopulated COMPLETE_INC, flag for determining if you have four entries
PROJECT_DATA$COMPLETE_INC <- 0
for (i in 1:nrow(PROJECT_DATA)){
  incVect <- c(PROJECT_DATA$y1[i], PROJECT_DATA$y2[i], PROJECT_DATA$y3[i], PROJECT_DATA$y4[i], PROJECT_DATA$y5[i], PROJECT_DATA$y6[i], PROJECT_DATA$y7[i], PROJECT_DATA$y8[i])
  lengthCount = 0
  for (j in length(incVect):1){
    if(incVect[j]>= 0) {
      lengthCount = lengthCount +1
    }
  }
  if (lengthCount > 3){
    PROJECT_DATA$COMPLETE_INC[i]<- 1
  }
}

PROJECT_DATA$g1 <- 0
PROJECT_DATA$g2 <- 0
PROJECT_DATA$g3 <- 0
PROJECT_DATA$g4 <- 0
PROJECT_DATA$g5 <- 0
PROJECT_DATA$g6 <- 0
PROJECT_DATA$g7 <- 0

for (i in 1:nrow(PROJECT_DATA)){
  startYs = grep("y1", colnames(PROJECT_DATA))
  startGs = grep("g1", colnames(PROJECT_DATA))
  for (j in 0:6){
    if (PROJECT_DATA[i,startYs+j] >0 & PROJECT_DATA[i,startYs+j+1] >0){
      PROJECT_DATA[i,startGs+j]= PROJECT_DATA[i,startYs+j+1]/PROJECT_DATA[i,startYs+j]
    }
    else {
      PROJECT_DATA[i,startGs+j]=-3
    }
  }
}

#populate categories=======================================================================
admit_cats <- c('1','2', '3', '4', '5', '6', '7')
apply_cats <- c('-3','1','2', '3', '4', '5', '6', '7')
cat_vector <- rep(NA, length(admit_cats)*length(apply_cats))
pop_counter <- matrix(ncol = 7, nrow = length(admit_cats)*length(apply_cats))
pop_counter[is.na(pop_counter)]<-0
#pop_counter <- rep(rep(0,  length(admit_cats)*length(apply_cats)),7) #counts the number of people in each category
sum_gamma <- matrix(ncol = 7, nrow = length(admit_cats)*length(apply_cats))
sum_gamma[is.na(sum_gamma)] <- 0

k = 1
for (i in 1:length(admit_cats)){#populate what will be the "lookup vector"
  for (j in 1:length(apply_cats)){
    cat_vector[k]= paste(admit_cats[i],apply_cats[j], sep = "")
    k = k+1
  }
}

startGs = grep("g1", colnames(PROJECT_DATA))
for (i in 1:nrow(PROJECT_DATA)){
  curCat = toString(paste(PROJECT_DATA$Best.Admitted[i],PROJECT_DATA$Best.Attended[i], sep = ""))
  curIndex = match(curCat, cat_vector)
  for (j in 0:6){
    if (PROJECT_DATA[i,startGs+j] >0){
      pop_counter[curIndex,j+1]= pop_counter[curIndex,j+1]+1
      sum_gamma[curIndex,j+1] =sum_gamma[curIndex,j+1]+PROJECT_DATA[i,startGs+j]
    }
  }
}

avg_gamma <- matrix(ncol = 7, nrow = length(admit_cats)*length(apply_cats))
avg_gamma[is.na(avg_gamma)] <- 0
avg_gamma = sum_gamma/pop_counter

write.csv(avg_gamma, "C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/avg_gamma.csv")
write.csv(pop_counter, "C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/pop_counter.csv")

#populate 5 categories
admit_cats <- c('1','2', '3', '4', '6', '7')
apply_cats <- c('-3','1','2', '3', '4', '6', '7')
cat_vector <- rep(NA, length(admit_cats)*length(apply_cats))
pop_counter <- matrix(ncol = 7, nrow = length(admit_cats)*length(apply_cats))
pop_counter[is.na(pop_counter)]<-0
#pop_counter <- rep(rep(0,  length(admit_cats)*length(apply_cats)),7) #counts the number of people in each category
sum_gamma <- matrix(ncol = 7, nrow = length(admit_cats)*length(apply_cats))
sum_gamma[is.na(sum_gamma)] <- 0

k = 1
for (i in 1:length(admit_cats)){#populate what will be the "lookup vector"
  for (j in 1:length(apply_cats)){
    cat_vector[k]= paste(admit_cats[i],apply_cats[j], sep = "")
    k = k+1
  }
}

startGs = grep("g1", colnames(PROJECT_DATA))
for (i in 1:nrow(PROJECT_DATA)){
  curCat = toString(paste(PROJECT_DATA$BestAd5[i],PROJECT_DATA$BestAtt5[i], sep = ""))
  curIndex = match(curCat, cat_vector)
  for (j in 0:6){
    if (PROJECT_DATA[i,startGs+j] >0){
      pop_counter[curIndex,j+1]= pop_counter[curIndex,j+1]+1
      sum_gamma[curIndex,j+1] =sum_gamma[curIndex,j+1]+PROJECT_DATA[i,startGs+j]
    }
  }
}

avg_gamma <- matrix(ncol = 7, nrow = length(admit_cats)*length(apply_cats))
avg_gamma[is.na(avg_gamma)] <- 0
avg_gamma = sum_gamma/pop_counter

write.csv(avg_gamma, "C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/avg_gamma.csv")
write.csv(pop_counter, "C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/pop_counter.csv")

#populate 3 categories
admit_cats <- c('1','2', '3','7')
apply_cats <- c('-3','1','2', '3', '7')
cat_vector <- rep(NA, length(admit_cats)*length(apply_cats))
pop_counter <- matrix(ncol = 7, nrow = length(admit_cats)*length(apply_cats))
pop_counter[is.na(pop_counter)]<-0
#pop_counter <- rep(rep(0,  length(admit_cats)*length(apply_cats)),7) #counts the number of people in each category
sum_gamma <- matrix(ncol = 7, nrow = length(admit_cats)*length(apply_cats))
sum_gamma[is.na(sum_gamma)] <- 0

k = 1
for (i in 1:length(admit_cats)){#populate what will be the "lookup vector"
  for (j in 1:length(apply_cats)){
    cat_vector[k]= paste(admit_cats[i],apply_cats[j], sep = "")
    k = k+1
  }
}

startGs = grep("g1", colnames(PROJECT_DATA))
for (i in 1:nrow(PROJECT_DATA)){
  curCat = toString(paste(PROJECT_DATA$BestAd3[i],PROJECT_DATA$BestAtt3[i], sep = ""))
  curIndex = match(curCat, cat_vector)
  for (j in 0:6){
    if (PROJECT_DATA[i,startGs+j] >0){
      pop_counter[curIndex,j+1]= pop_counter[curIndex,j+1]+1
      sum_gamma[curIndex,j+1] =sum_gamma[curIndex,j+1]+PROJECT_DATA[i,startGs+j]
    }
  }
}

avg_gamma <- matrix(ncol = 7, nrow = length(admit_cats)*length(apply_cats))
avg_gamma[is.na(avg_gamma)] <- 0
avg_gamma = sum_gamma/pop_counter

write.csv(avg_gamma, "C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/avg_gamma.csv")
write.csv(pop_counter, "C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/pop_counter.csv")

#pull schooling versus working info==========================================================
#goal: get vector of school status and populate for start year
ENROLL_DATA <- read.csv("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/enrollstat/enrollstat.csv")
ENROLL_DATA <- merge(x = PROJECT_DATA, y = ENROLL_DATA, by = "PUBID_1997", all.x = TRUE)

#holders for enrollment info. note that we do not ask about 1996 (as we do with income, where questions are about last yera)
ENROLL_DATA$enroll2<-0
ENROLL_DATA$enroll3<-0
ENROLL_DATA$enroll4<-0
ENROLL_DATA$enroll5<-0
ENROLL_DATA$enroll6<-0
ENROLL_DATA$enroll7<-0
ENROLL_DATA$enroll8<-0
ENROLL_DATA$enroll9<-0
ENROLL_DATA$stillInSchool<--3
ENROLL_DATA$lastSchoolType<--3

#populate
year_vect = c("1997","1998","1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010")
for (i in 1:length(year_vect)){
  intYear = as.integer(year_vect[i])
  if (intYear < 2005 & intYear != 1997){
    enrollVar = paste("CV_ENROLLSTAT_EDT_", year_vect[i], sep = "")
  }
  else {
    enrollVar = paste("CV_ENROLLSTAT_", year_vect[i], sep = "")
  }
  
  for (j in 1:nrow(ENROLL_DATA)){
    yearNum = intYear-ENROLL_DATA$START_YEAR[j]
    if (yearNum >= 0 & yearNum < 8){#this means you are in a year that is relevant for this person
      counter = yearNum +2
      #set corresponding variable
      pasteStr = paste('enroll',counter, sep = "")
      if (ENROLL_DATA[j, enrollVar] <0){
        ENROLL_DATA[j,pasteStr] <- ENROLL_DATA[j, enrollVar] #keep missingness information
      }
      else if (ENROLL_DATA[j, enrollVar] <8){
        ENROLL_DATA[j,pasteStr] <- 0 #not enrolled
      }
      else {
        ENROLL_DATA[j,pasteStr] <- 1 #enrolled
        if (i == length(year_vect)){
          ENROLL_DATA[j,"lastSchoolType"] <- ENROLL_DATA[j, enrollVar]
        }
      }
    }
  }
}

#fill in missing enroll values
startEnroll = grep("enroll2", colnames(ENROLL_DATA))
#endEnroll = startEnroll + 7
for (j in 1:nrow(ENROLL_DATA)){
  changing = 0
  last = 0
  enrollVect = c(ENROLL_DATA[j,startEnroll], ENROLL_DATA[j,startEnroll+1], ENROLL_DATA[j,startEnroll+2], ENROLL_DATA[j,startEnroll+ 3], ENROLL_DATA[j,startEnroll+4], ENROLL_DATA[j,startEnroll+5], ENROLL_DATA[j,startEnroll+6], ENROLL_DATA[j,startEnroll+7])
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
    ENROLL_DATA[j,startEnroll+i-1]<-enrollVect[i]
  }
  i = length(enrollVect)
  while (ENROLL_DATA$stillInSchool[j] <0 & i < 9){
    ENROLL_DATA$stillInSchool[j] = max(ENROLL_DATA$stillInSchool[j], ENROLL_DATA[j,startEnroll+i-1])
    i = i+1
  }
  if (ENROLL_DATA$stillInSchool[j] ==-3){
    ENROLL_DATA$stillInSchool[j]<- 0
  }
}

#create still in college flag
ENROLL_DATA$stillInCollege<-0
for (j in 1:nrow(ENROLL_DATA)){
  if (ENROLL_DATA$stillInSchool[j] ==1 & ENROLL_DATA$lastSchoolType[j] < 0){
    ENROLL_DATA$stillInCollege[j] =-3
  }
  else if (ENROLL_DATA$stillInSchool[j] ==1 & ENROLL_DATA$lastSchoolType[j] < 11){
    ENROLL_DATA$stillInCollege[j] =1
  }
}

write.csv(ENROLL_DATA, "C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/with_enrolldata.csv")

#attmempt projection of income dynamics--quadratic and NS=====================================
ENROLL_DATA<-read.csv("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/with_enrolldata.csv")
stringVect = rep(NA, nrow(ENROLL_DATA))
longAgeVect = vector()
longIncVect = vector()
coeffVect = data.frame(matrix(ncol = 2, nrow = dim(ENROLL_DATA)[1]))
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
  #do cleanup to ensure only runs of 4+ usable variables are included. must adjust for age
  startIndex = 0
  endIndex = 0
  for (j in 1:length(incVect)){
    if (j == length(incVect)){
      #if (incVect[j]>=0 & startIndex ==0 & enrollVect[j]!= 1){ #with enrollment modification
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
   # if (incVect[j]>=0 & enrollVect[j]!= 1){ #with enrollment modification: 
    if (incVect[j]>=0){#without enrollment modification: 
      if (startIndex ==0){
        startIndex = j
      }
      endIndex = j
    }
    else {
      if (endIndex - startIndex >= 4 & startIndex > 0){
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
    #print(toString(ENROLL_DATA$PUBID_1997[i]))
    incVectOut = c(-3)
    ageVectOut = c(-3)
    enrollVectOut = c(-3)
    stringVect[i]= -3
    coeffVect[i,1] =-3
    coeffVect[i,2] =-3
  }

  else{
    #predict with quadratic and NS
    startIndex = ageVectOut[1]
    endIndex = ageVectOut[length(ageVectOut)]
    
    #transformed model
    #quadMod <- lm(log(incVectOut + 1)~ageVectOut+ I(ageVectOut^2))
    #non-transformedmodel
    #quadMod <- lm(incVectOut~ageVectOut+ I(ageVectOut^2))
    #transformed and non-transformed
    #new <- data.frame(ageVectOut = c((endIndex+1):81))
    #newIncs <- predict(quadMod,new)
    
    #NS model 
    tau = 26.2089
    b1 = 10118.85
    input1 = (1-exp(-ageVectOut/tau))/(ageVectOut/tau)
    input2 = input1 - exp(-ageVectOut/tau)
    #quadMod <- lm(incVectOut~input1+ input2)
    #quadMod <- lm(incVectOut~input2)
    output = incVectOut - b1 * input1
    quadMod = lm(output~input2)
    coeffVect[i,1] =quadMod$coefficients[[1]]
    coeffVect[i,2] =quadMod$coefficients[[2]]
    new <-  c((endIndex+1):100)
    new1 <-(1-exp(-new/tau))/(new/tau)
    new2 <- new1 - exp(-new/tau)
    #new <- data.frame(input1 = new1, input2 = new2)
    new <- data.frame(input2 = new2)
    newIncs <- predict(quadMod,new)
    newIncs <- newIncs - b1 * new1
    
    #NS model with fixed beta1
    
    #transformed model
    #incVectFull <- c(rep(-3, startIndex-1), incVectOut, exp(newIncs))
    #non-transformed model and NS model
    incVectFull <- c(rep(-3, startIndex-1), incVectOut, newIncs)
    
    stringVect[i]= paste(toString(ENROLL_DATA$PUBID_1997[i]), "\t", toString(incVectFull), sep = "")
    stringVect[i] = gsub(", ", "\t", stringVect[i])
    
    #get whole sample for nonlinear estimation
    longIncVect = c(longIncVect,incVectOut)
    longAgeVect = c(longAgeVect, ageVectOut)
  }
}
fileConn<-file("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/quadraticoutput.txt")
writeLines(stringVect, fileConn)
close(fileConn)

write.csv(coeffVect, "C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/coeffVect.txt")

#attmempt projection of income dynamics--nested quadratic=====================================
getDelt <- nls(log(longIncVect+1)~a0 + a1 * longAgeVect+(a2+a1*d)* longAgeVect^2 + 2 * d * a2 * longAgeVect^3 + d^2 *a2*longAgeVect^4)
delta = summary(getDelt)$coefficients[4,1]

stringVect = rep(NA, nrow(ENROLL_DATA))
for (i in 1:nrow(ENROLL_DATA)){
  incVectOut = NULL
  ageVectOut= NULL
  incVectFull = NULL
  incVect = c(ENROLL_DATA$y1[i], ENROLL_DATA$y2[i], ENROLL_DATA$y3[i], ENROLL_DATA$y4[i], ENROLL_DATA$y5[i], ENROLL_DATA$y6[i],ENROLL_DATA$y7[i],ENROLL_DATA$y8[i])
  #do cleanup to ensure only runs of 4+ usable variables are included. must adjust for age
  startIndex = 0
  endIndex = 0
  for (j in 1:length(incVect)){
    if (j == length(incVect)){
      if (incVect[j]>=0 & startIndex ==0){
        endIndex = j
      }
      if (endIndex - startIndex >= 4 & startIndex > 0){
        incVectOut = incVect[startIndex:endIndex]
        ageVectOut = c(startIndex:endIndex)
      }
    }
    if (incVect[j]>=0){
      if (startIndex ==0){
        startIndex = j
      }
      endIndex = j
    }
    else {
      if (endIndex - startIndex >= 4 & startIndex > 0){
        incVectOut = incVect[startIndex:endIndex]
        ageVectOut = c(startIndex:endIndex)
      }
      startIndex = 0
      endIndex = 0
    }
  }
  if (is.null(incVectOut)){
    incVectOut = c(-3)
    ageVectOut = c(-3)
    stringVect[i]= -3
  }
  else{
    #predict with nested quadratic
    startIndex = ageVectOut[1]
    endIndex = ageVectOut[length(ageVectOut)]
    
    #transformed model
    ageVectOut = ageVectOut + delta * ageVectOut^2
    quadNestMod <- lm(log(incVectOut + 1)~ageVectOut+ I(ageVectOut^2))
    new <- data.frame(ageVectOut = c((endIndex+1):81))
    newIncs <- predict(quadMod,new)
    
    #transformed model
    incVectFull <- c(rep(-3, startIndex-1), incVectOut, exp(newIncs))
    stringVect[i]= paste(toString(ENROLL_DATA$PUBID_1997[i]), "\t", toString(incVectFull), sep = "")
    stringVect[i] = gsub(", ", "\t", stringVect[i])

  }
}
fileConn<-file("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/quadraticoutput2.txt")
writeLines(stringVect, fileConn)
close(fileConn)

#get shape of census data ======================================================
CENSUS_DATA <- read.csv("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/avgCensus.csv")
b0Vect = rep(NA,8)
b1Vect = rep(NA,8)
b2Vect = rep(NA,8)
out = data.frame(matrix(ncol = 8, nrow = 82))
colnames(out)=c("Average", "HS", "SomeCollege", "Associates", "Bachelors", "Masters", "Professional", "Doctoral")
#NS model
tau = 26.2089
input1 = (1-exp(-CENSUS_DATA$Age/tau))/(CENSUS_DATA$Age/tau)
input2 = input1 - exp(-CENSUS_DATA$Age/tau)
quadModAvg <- lm(CENSUS_DATA$IncomeAvg~input1+ input2)
b0Vect[1] = quadModAvg$coefficients[1]
b1Vect[1] = quadModAvg$coefficients[2]
b2Vect[1] = quadModAvg$coefficients[3]
quadModHS <- lm(CENSUS_DATA$IncomeHS~input1+ input2)
b0Vect[2] = quadModHS$coefficients[1]
b1Vect[2] = quadModHS$coefficients[2]
b2Vect[2] = quadModHS$coefficients[3]
quadModSC <- lm(CENSUS_DATA$IncomeSC~input1+ input2)
b0Vect[3] = quadModSC$coefficients[1]
b1Vect[3] = quadModSC$coefficients[2]
b2Vect[3] = quadModSC$coefficients[3]
quadModAS <- lm(CENSUS_DATA$IncomeAS~input1+ input2)
b0Vect[4] = quadModAS$coefficients[1]
b1Vect[4] = quadModAS$coefficients[2]
b2Vect[4] = quadModAS$coefficients[3]
quadModBS <- lm(CENSUS_DATA$IncomeBS~input1+ input2)
b0Vect[5] = quadModBS$coefficients[1]
b1Vect[5] = quadModBS$coefficients[2]
b2Vect[5] = quadModBS$coefficients[3]
quadModMA <- lm(CENSUS_DATA$IncomeMA~input1+ input2)
b0Vect[6] = quadModMA$coefficients[1]
b1Vect[6] = quadModMA$coefficients[2]
b2Vect[6] = quadModMA$coefficients[3]
quadModPR <- lm(CENSUS_DATA$IncomePR~input1+ input2)
b0Vect[7] = quadModPR$coefficients[1]
b1Vect[7] = quadModPR$coefficients[2]
b2Vect[7] = quadModPR$coefficients[3]
quadModDR <- lm(CENSUS_DATA$IncomeDR~input1+ input2)
b0Vect[8] = quadModDR$coefficients[1]
b1Vect[8] = quadModDR$coefficients[2]
b2Vect[8] = quadModDR$coefficients[3]

new <-  c(19:100)
new1 <-(1-exp(-new/tau))/(new/tau)
new2 <- new1 - exp(-new/tau)
new <- data.frame(input1 = new1, input2 = new2)
out[1] <- predict(quadModAvg,new)
out[2] <- predict(quadModHS,new)
out[3] <- predict(quadModSC,new)
out[4] <- predict(quadModAS,new)
out[5] <- predict(quadModBS,new)
out[6] <- predict(quadModMA,new)
out[7] <- predict(quadModPR,new)
out[8] <- predict(quadModDR,new)

write.csv(out,"C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/avgCensusOut.csv")

#quadratic
quadMod <- lm(log(Income)~Age+ I(Age^2), data = CENSUS_DATA)
quadMod <- lm(Income~Age+ I(Age^2), data = CENSUS_DATA)

#attmempt NS projection of income dynamics for each education group=====================================
ENROLL_DATA<-read.csv("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/with_enrolldata.csv")

admit_cats <- c('1','2', '3', '4', '6', '7')
apply_cats <- c('-3','1','2', '3', '4', '6', '7')
cat_vector <- rep(NA, length(admit_cats)*length(apply_cats))

k = 1
for (i in 1:length(admit_cats)){#populate what will be the "lookup vector"
  for (j in 1:length(apply_cats)){
    cat_vector[k]= paste(admit_cats[i],apply_cats[j], sep = "")
    k = k+1
  }
}

outputList <- list()

#store data points by category
for (i in 1:length(cat_vector)){
  outputList[[i]]<-data.frame(matrix(ncol = 3, nrow = 0))
  colnames(outputList[[i]])= c("age", "income", "enroll")
}
names(outputList)<-cat_vector

for (i in 1:nrow(ENROLL_DATA)){
  curCat = toString(paste(ENROLL_DATA$BestAd5[i],ENROLL_DATA$BestAtt5[i], sep = ""))
  curIndex = match(curCat, cat_vector)
  tempDF <- data.frame(age = ageVectList[[i]], income = incomeVectList[[i]], enroll = enrollVectList[[i]])
  outputList[[curIndex]] = rbind(outputList[[curIndex]], tempDF)
}

#NS with all available datapoint
b0Vect = rep(NA,length(cat_vector))
b1Vect = rep(NA,length(cat_vector))
b2Vect = rep(NA,length(cat_vector))
numObsVect = rep(NA,length(cat_vector))
R2Vect= rep(NA,length(cat_vector))
byCatOut = data.frame(matrix(ncol = length(cat_vector), nrow = 82))
colnames(byCatOut) = cat_vector

for (i in 1:length(cat_vector)){
  #NS model
  tau = 26.2089
  #curData = outputList[[i]][outputList[[i]]$enroll == 0,] #enroll only
  curData = outputList[[i]] #all data
  numObsVect[i] = dim(curData)[1]
  if (numObsVect[i] > 3){
    input1 = (1-exp(-curData$age/tau))/(curData$age/tau)
    input2 = input1 - exp(-curData$age/tau)
    quadMod <- lm(curData$income~input1+ input2)
    b0Vect[i] = quadMod$coefficients[1]
    b1Vect[i] = quadMod$coefficients[2]
    b2Vect[i] = quadMod$coefficients[3]
    R2Vect[i]= summary(quadMod)$r.squared
    new <-  c(19:100)
    new1 <-(1-exp(-new/tau))/(new/tau)
    new2 <- new1 - exp(-new/tau)
    new <- data.frame(input1 = new1, input2 = new2)
    byCatOut[,i] <- predict(quadMod,new)
  }
 else{
   b0Vect[i] = -3
   b1Vect[i] = -3
   b2Vect[i] = -3
 }
}

write.csv(byCatOut,"C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/byCatOut.csv")


#NS with fixed beta1
b0Vect = rep(NA,length(cat_vector))
b1Vect = rep(NA,length(cat_vector))
b2Vect = rep(NA,length(cat_vector))
numObsVect = rep(NA,length(cat_vector))
R2Vect= rep(NA,length(cat_vector))
byCatOut = data.frame(matrix(ncol = length(cat_vector), nrow = 82))
colnames(byCatOut) = cat_vector

for (i in 1:length(cat_vector)){
  #NS model
  tau = 26.2089
  b1 = 10118.85
  curData = outputList[[i]][outputList[[i]]$enroll == 0,]
  numObsVect[i] = dim(curData)[1]
  if (numObsVect[i] > 3){
    input1 = (1-exp(-curData$age/tau))/(curData$age/tau)
    input2 = input1 - exp(-curData$age/tau)
    output = curData$income - b1 * input1
    quadMod <- lm(output~input1+ input2)
    b0Vect[i] = quadMod$coefficients[1]
    b1Vect[i] = quadMod$coefficients[2]
    b2Vect[i] = quadMod$coefficients[3]
    R2Vect[i]= summary(quadMod)$r.squared
    new <-  c(19:100)
    new1 <-(1-exp(-new/tau))/(new/tau)
    new2 <- new1 - exp(-new/tau)
    new <- data.frame(input1 = new1, input2 = new2)
    byCatOut[,i] <- predict(quadMod,new)
    byCatOut[,i]<- byCatOut[,i]- b1 * new1
  }
  else{
    b0Vect[i] = -3
    b1Vect[i] = -3
    b2Vect[i] = -3
  }
}

write.csv(byCatOut,"C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/byCatOut.csv")