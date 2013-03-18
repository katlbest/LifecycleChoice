library(plyr)

#notes=======================================================================
#to clear data: rm(list = ls(all = TRUE))
#to replace missing values: MERGED_DATA[MERGED_DATA < 0] <- NA
#sapply(PYTHON_OUT, class)

#data i/o=======================================================================
INCOME_DATA <- read.csv("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/INCOME_DATA.csv")

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
  for (i in 1:nrow(data)){
    #if indicator is positive
    if (clarify == "None"){#this means we dont have a clarifying question
      realIndicator = data[i, indicator]
    } else {
        realIndicator = max(data[i, indicator],data[i, clarify])
    }
    if (realIndicator == 1){#some income received
      if (data[i, mainvar] > 0){
        totalVect[i]= data[i, mainvar]
      }
      else if(data[i, secondvar] > 0){
        totalVect[i]= lookupCategory(lookupType, data[i, secondvar])
      }
    }
  }
  invisible(return(totalVect))
}


lookupCategory <- function(varType, curValue){ #varType 1 =  biggest range, 3 = smallers
  if (varType ==1){
    lookupvect = c(2501, 7501, 17501, 37501, 75001, 175001, 300000)
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
  if (as.integer(year_vect[i])> 1998)
  salTwoYearInd=paste("YINC_1400A_", year_vect[i], sep = "")
  salTwoYearMain = paste("YINC_1700A_", year_vect[i], sep = "") 
  salTwoYearSecond = paste("YINC_1800A_", year_vect[i], sep = "")
  salary_twoyear = getTotal(INCOME_DATA, salTwoYearInd, salTwoYearMain, salTwoYearSecond, "None", 1)
  
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
  
  if (as.integer(year_vect[i])> 1998){
    outString2 = paste("INC_", toString(as.integer(year_vect[i])-2), sep = "") #store in last year's income variable
    INCOME_DATA[,outString2]<- INCOME_DATA[,outString2] + salary_twoyear
  }
}


#write to file ========================================================================
write.csv(INCOME_DATA, "C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/INCOME_DATA2.csv")