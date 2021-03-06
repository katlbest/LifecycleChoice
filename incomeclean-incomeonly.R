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
INCOME_DATA$LAB_1996 <- 0
INCOME_DATA$LAB_1997 <- 0
INCOME_DATA$LAB_1998 <- 0
INCOME_DATA$LAB_1999 <- 0
INCOME_DATA$LAB_2000 <- 0
INCOME_DATA$LAB_2001 <- 0
INCOME_DATA$LAB_2002 <- 0
INCOME_DATA$LAB_2003 <- 0
INCOME_DATA$LAB_2004 <- 0
INCOME_DATA$LAB_2005 <- 0
INCOME_DATA$LAB_2006 <- 0
INCOME_DATA$LAB_2007 <- 0
INCOME_DATA$LAB_2008 <- 0
INCOME_DATA$LAB_2009 <- 0
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

getTotal2 <- function(data, indicator, mainvar, secondvar, clarify, lookupType) { #data frame, indicator var name, main var name, secondary var name, clarify var name or none
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
      else if (lookupType ==1){ #we don't use this check
        totalVect[j]= -100000000 #very negativenumber so it doesn't become positive
        print("yes")
      }
    }
    #if (realIndicator==-1 | realIndicator == -2 | realIndicator == -5){ #when this is used, we check that no one refused or don't know for income variable indicator
    # if (lookupType ==1){ 
    #  totalVect[j]= -100000000 #very negativenumber so it doesn't become positive
    # print(paste("Missing data in", toString(j),year_vect[i],sep = ",")) #this doesn't happen, no unjustified skip values
    #}
    #}
  }
  invisible(return(totalVect))
  print(totalVect)
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

#POPULATE INCOMES WITH KEEPING MISSINGS==========================================================
#populate incomes 1996

#syntax: data frame, indicator, main variable, secondary (refuser) variable, clarification question indicator or "None"
salary_96 = getTotal2(INCOME_DATA, "P5_010_1997", "P5_016_1997", "P5_017_1997", "P5_011_1997", 1)
INCOME_DATA$LAB_1996 <- salary_96

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
    salary_cur = getTotal2(INCOME_DATA, salIndicator, salMain, salSecond, salClarify, 1)
  } else {
    salary_cur = getTotal2(INCOME_DATA, salIndicator, salMain, salSecond, "None", 1)
  }
  
  #salary from two years ago
  if (as.integer(year_vect[i])> 1998){
    salTwoYearInd=paste("YINC_1400A_", year_vect[i], sep = "")
    salTwoYearMain = paste("YINC_1700A_", year_vect[i], sep = "") 
    salTwoYearSecond = paste("YINC_1800A_", year_vect[i], sep = "")
    salary_twoyear = getTotal2(INCOME_DATA, salTwoYearInd, salTwoYearMain, salTwoYearSecond, "None", 1)
  }
  
  
  outString = paste("LAB_", toString(as.integer(year_vect[i])-1), sep = "") #store in last year's income variable
  INCOME_DATA[,outString]<- salary_cur
  #print(paste("salary",year_vect[i], salary_cur, sep = ","))
  #print(paste("farm",year_vect[i], farm_cur, sep = ","))
  #print(paste("other",year_vect[i], other_cur, sep = ","))
  
  if (as.integer(year_vect[i])> 1998){
    outString2 = paste("LAB_", toString(as.integer(year_vect[i])-2), sep = "") #store in last year's income variable
    for (k in 1: nrow(INCOME_DATA)){
      INCOME_DATA[k,outString2]<- max(0,INCOME_DATA[k,outString2]) + salary_twoyear[k]
    }
  }
}

#replace all negative numbers with negative three
year_vect = c("1996","1997","1998","1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009")
for (i in 1:length(year_vect)){
  outString = paste("LAB_", toString(as.integer(year_vect[i])), sep = "")
  INCOME_DATA[,outString][INCOME_DATA[,outString]<0] = -3
}

#write to file ========================================================================
write.csv(INCOME_DATA, "C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/INCOME_DATA2.csv")

#add in attendance category data =============================================================
CATEGORY_DATA <- read.csv("C://Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/categories.csv")
INCOME_DATA2 <- merge(x = CATEGORY_DATA, y = INCOME_DATA, by = "PUBID_1997", all.x = TRUE)

#adjust timing of income data =======================================================================
#INCOME_DATA2 <- read.csv("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/INCOME_DATA2input.csv")
INCOME_DATA2$START_YEAR <- 0
INCOME_DATA2$z1 <- 0
INCOME_DATA2$z2 <- 0
INCOME_DATA2$z3 <- 0
INCOME_DATA2$z4 <- 0
INCOME_DATA2$z5 <- 0
INCOME_DATA2$z6 <- 0
INCOME_DATA2$z7 <- 0
INCOME_DATA2$z8 <- 0
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
  curStrNM = paste('LAB_',toString(INCOME_DATA2$START_YEAR[i]),'b',sep = "") #pulls data with no missing values
  curStr = paste('LAB_',toString(INCOME_DATA2$START_YEAR[i]),sep = "") #pulls data with missing values
  colIndex = grep(curStr, colnames(INCOME_DATA2))[1] #use the first occurence
  colIndexNM = grep(curStrNM, colnames(INCOME_DATA2))[1] #use the first occurence
  firstIndex = grep("LAB_1996", colnames(INCOME_DATA2))[1] #use if doing with missing
  firstIndexNM = grep("LAB_1996b", colnames(INCOME_DATA2)) #use if doing no missing
  startYs = grep("z1", colnames(INCOME_DATA2))
  startYsNM = grep("znm1", colnames(INCOME_DATA2))
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
#INCOME_DATA2<- read.csv("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/incomeadjusted.csv")
INCOME_DATA2$COMPLETE_INC <- 0
misCount = 0
missCompletelyCount = 0
fixedCount = 0
for (i in 1:nrow(INCOME_DATA2)){
  incVect <- c(INCOME_DATA2$z1[i], INCOME_DATA2$z2[i], INCOME_DATA2$z3[i], INCOME_DATA2$z4[i], INCOME_DATA2$z5[i], INCOME_DATA2$z6[i], INCOME_DATA2$z7[i], INCOME_DATA2$z8[i])
  incVectNM <- c(INCOME_DATA2$znm1[i], INCOME_DATA2$znm2[i], INCOME_DATA2$znm3[i], INCOME_DATA2$znm4[i], INCOME_DATA2$znm5[i], INCOME_DATA2$znm6[i], INCOME_DATA2$znm7[i], INCOME_DATA2$znm8[i])
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
    #print(returnObj[1:length(returnObj)-1])
    incVect <- returnObj[1:length(returnObj)-1] #note: a second pass doesn't help if you OR the requirement
    fixedCount = fixedCount + returnObj[length(returnObj)]
  }
  if (min(incVect)>=0){
    INCOME_DATA2$COMPLETE_INC[i]<- 1 #if incVect now has no zeros, we are complete!
  }
  else { #must project income; also project if it is zero
    misCount = misCount +1
    #check if only terminal values are missing
    checkRestFlag = 0
    lengthCounter = 0
    for (j in length(incVect):1){
      if (incVect[j]==-3 | incVect[j]==0){
        checkRest <- max(incVect[min(j+1, length(incVect)):length(incVect)])
        if (checkRest <=0){
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
#print(fixedCount) #with OR you fix 127, with AND you only fix 50; number of people for whom at least one enty is filled in
#print(missCompletelyCount)
#print(misCount)

#write.csv(INCOME_DATA2[INCOME_DATA2$COMPLETE_INC ==0,], "C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/projectincome.csv")

#write everyone to file so we can pull in manually updated by vlookup
write.csv(INCOME_DATA2, "C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/allindividuals.csv")
write.csv(INCOME_DATA2[c("PUBID_1997", "z1","z2","z3","z4","z5","z6","z7","z8","znm1","znm2","znm3","znm4","znm5","znm6","znm7","znm8")], "C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/indivdata_laboronly.csv")




#OLD=======================================================================================

#calculate growth rates===
PROJECT_DATA<-INCOME_DATA2
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

#populate categories===
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

#pull schooling versus working info
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

write.csv(ENROLL_DATA, "C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/indivdata_incomeonly_nomissing.csv")
