library(reshape)

INCOME_DATA <- read.csv("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Stock market correlation/79income.csv")

#melt data appropriately
#data setup, create age/gender variables based on our info and their audit
melted <- melt(INCOME_DATA, id = c("CASEID_1979", "BIRTH_YEAR", "SAMPLE_RACE_1979", "SAMPLE_SEX_1979" ))
nielsenmain <- read.csv("~/comscore/nielsenmain.csv", header=T)
clean_data <- nielsenmain[nielsenmain$age != "U" & nielsenmain$gender != "n",]
clean_dataP <- nielsenprog[nielsenprog$age != "U" & nielsenprog$gender != "n",]
melted <- melt(clean_data, id = c("campaign", "vendor", "age", "gender"))
meltedP <- melt(clean_dataP, id = c("campaign","vendor","age","gender"))
melted$age <-as.character(melted$age)
meltedP$age <-as.character(meltedP$age)
melted$variable <- as.character(melted$variable)
melted$nielsenGen <- factor(substr(melted$variable,1,1))
melted$nielsenAge <- factor(substr(melted$variable,2,nchar(melted$variable)))
meltedP$variable <- as.character(meltedP$variable)
meltedP$nielsenGen <- factor(substr(meltedP$variable,1,1))
meltedP$nielsenAge <- factor(substr(meltedP$variable,2,nchar(meltedP$variable)))

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
