#NOTES:======================================================================
  #this file pulls income data, does prediction, and assesses the fit of that prediction using four methods:
    #NmFillMiddle: fill in middle values and throw out 10K or fewer entries
    #NmNoFill: don't fill in middle values or throw out 10K or fewer entries
    #EmployFillMiddle: fill in middle values and throw out 10K or fewer entries, include only those years worked nmore than 1200 hrs
    #EmployNoFill: don't fill in middle values or throw out 10K or fewer entries, include only those years worked nmore than 1200 hrs
    #Employ10K: throw out 10K and use employment restriction (1200 hrs)

#libraries ====================================================================
  library(plyr)
  library(ggplot2)

#clear workspace ==============================================================
  rm(list = ls())

#data i/o=======================================================================
  INCOME_DATA <- read.csv("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/INCOME_DATA.csv")
  #this file has a lot of the information necessary for projections;can always add needed columns from compileddata_allways.csv

#set up income variables================================================================
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

#POPULATE INCOMES==========================================================
  #note: use all available information, even if some info is missing (maps to old "NM variables")

  #load function for extracting incomes, syntax: data frame, indicator, main variable, secondary (refuser) variable, clarification question indicator or "None"
    source("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Data manipulation/fun_getTotal.R")

  #populate incomes 1996
    salary_96 = getTotal(INCOME_DATA, "P5_010_1997", "P5_016_1997", "P5_017_1997", "P5_011_1997", 1)
    INCOME_DATA$LAB_1996b <- salary_96

  #populate incomes 1997-2010
    year_vect = c("1997","1998","1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010")
    for (i in 1:length(year_vect)){
      #salary this year
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
  
      if (as.integer(year_vect[i])> 1998){
        outString2 = paste("LAB_", toString(as.integer(year_vect[i])-2),"b", sep = "") #store in last year's income variable
        for (k in 1: nrow(INCOME_DATA)){
          INCOME_DATA[k,outString2]<- max(0,INCOME_DATA[k,outString2]) + salary_twoyear[k]
        }
      }
    }

  #replace all negative numbers with negative three (missing indicator)
    year_vect = c("1996","1997","1998","1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009")
    for (i in 1:length(year_vect)){
      outString = paste("LAB_", toString(as.integer(year_vect[i])),"b", sep = "")
      INCOME_DATA[,outString][INCOME_DATA[,outString]<0] = -3
    }

#populate enrollment==============================================================================
  #set up variables  
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
            INCOME_DATA[j,pasteStr] <- INCOME_DATA[j, enrollVar] #settin gto value of current variable preserves missingness information
          }
          else if (INCOME_DATA[j, enrollVar] <8){
            INCOME_DATA[j,pasteStr] <- 0 #not enrolled
          }
          else {
            INCOME_DATA[j,pasteStr] <- 1 #enrolled
            if (i == length(year_vect)){
              INCOME_DATA[j,"lastSchoolType"] <- INCOME_DATA[j, enrollVar] #not used, but may be useful in future
            }
          }
        }
      }
    }
  
  #fill in missing enroll values (is a string of missing values is bounded by two of the same value on either side, set to that value)
    startEnroll = grep("enroll2", colnames(INCOME_DATA))
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
  INCOME_DATA2$znm1 <- 0
  INCOME_DATA2$znm2 <- 0
  INCOME_DATA2$znm3 <- 0
  INCOME_DATA2$znm4 <- 0
  INCOME_DATA2$znm5 <- 0
  INCOME_DATA2$znm6 <- 0
  INCOME_DATA2$znm7 <- 0
  INCOME_DATA2$znm8 <- 0

  for (i in 1:nrow(INCOME_DATA2)){
    curStrNM = paste('LAB_',toString(INCOME_DATA2$START_YEAR[i]),'b',sep = "")
    colIndexNM = grep(curStrNM, colnames(INCOME_DATA2))[1] #use the first occurence
    firstIndexNM = grep("LAB_1996b", colnames(INCOME_DATA2))
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

#set up vector lists for output ==============================================
  #rename data to preserve INCOME_DATA2 for future use
  ENROLL_DATA<-INCOME_DATA2

  #vector lists
  ageVectListLabNm <- list()
  incomeVectListLabNm <- list()
  enrollVectListLabNm <- list()
  employVectListLabNm <- list()
  
  ageVectListLabNmEmploy <- list()
  incomeVectListLabNmEmploy <- list()
  enrollVectListLabNmEmploy <- list()
  employVectListLabNmEmploy <- list()

  #read in employment data
  EMPLOY_DATA <- read.csv("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/income/hrsworked.csv")
  ENROLL_DATA <- merge(x = ENROLL_DATA, y = EMPLOY_DATA, by = "PUBID_1997", all.x = TRUE)

  #populate employment data
    for (i in 1:nrow(ENROLL_DATA)){
      incVectLabNm = c(ENROLL_DATA$znm1[i], ENROLL_DATA$znm2[i], ENROLL_DATA$znm3[i], ENROLL_DATA$znm4[i], ENROLL_DATA$znm5[i], ENROLL_DATA$znm6[i],ENROLL_DATA$znm7[i],ENROLL_DATA$znm8[i])
      enrollVect = c(ENROLL_DATA$enroll2[i], ENROLL_DATA$enroll3[i], ENROLL_DATA$enroll4[i], ENROLL_DATA$enroll5[i], ENROLL_DATA$enroll6[i], ENROLL_DATA$enroll7[i], ENROLL_DATA$enroll8[i], ENROLL_DATA$enroll9[i])
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
    #store results in vector lists
    incomeVectListLabNm[[i]]<-incVectLabNm
    ageVectListLabNm[[i]]<-c(1:length(incVectLabNm))
    enrollVectListLabNm[[i]]<-enrollVect
    employVectListLabNm[[i]]<-employVect
  }

  #must add 18 to each age number so that tau is correct
    for (i in 1:length(ageVectListLabNm)){
      if (ageVectListLabNm[[i]][1]>0){
        ageVectListLabNm[[i]] = ageVectListLabNm[[i]] + 18
      }
    }

  #set vectors to missing if only zeros available
    for (i in 1:length(incomeVectListLabNm)){
      #set to missing if only zeros available
      if (sum(incomeVectListLabNm[[i]])<= 0){
        incomeVectListLabNm[[i]]<-c(-3)
        ageVectListLabNm[[i]]<-c(-3)
        enrollVectListLabNm[[i]]<-c(-3)
        employVectListLabNm[[i]]<-c(-3)
      }
    }

  #set up vectors with 1200 hr employment restriction
    source("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Data manipulation/fun_removeNotFT.R")
    LabEmployReturn <-removeNotFT(incomeVectListLabNm, ageVectListLabNm, enrollVectListLabNm, employVectListLabNm)
    incomeVectListLabEmploy<-LabEmployReturn[[1]]
    ageVectListLabEmploy<-LabEmployReturn[[2]]
    enrollVectListLabEmploy<-LabEmployReturn[[3]]
    employVectListLabEmploy <- LabEmployReturn[[4]]

  #save workspace image
    save.image(file="commonStartingPoint.RData")
    #load("commonStartingPoint.RData")

#create vectors with filled middle values==================================================================
  #note: if lower than previous, set to previous, and remove entries < 10000

source("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Data manipulation/fun_fillMissingMiddle.R")

  LabNmReturnFilled <- fillMissingMiddle(incomeVectListLabNm, ageVectListLabNm, enrollVectListLabNm, employVectListLabNm)
  incomeVectListLabNmFilled<-LabNmReturnFilled[[1]]
  ageVectListLabNmFilled<-LabNmReturnFilled[[2]]
  enrollVectListLabNmFilled<-LabNmReturnFilled[[3]]
  employVectListLabNmFilled<-LabNmReturnFilled[[4]]

  LabEmployReturnFilled <-fillMissingMiddle(incomeVectListLabEmploy, ageVectListLabEmploy, enrollVectListLabEmploy, employVectListLabEmploy)
  incomeVectListLabEmployFilled<-LabEmployReturnFilled[[1]]
  ageVectListLabEmployFilled<-LabEmployReturnFilled[[2]]
  enrollVectListLabEmployFilled<-LabEmployReturnFilled[[3]]
  employVectListLabEmployFilled<- LabEmployReturnFilled[[4]]

#create employ restricted vector with 10K restriction onlys==================================================================
#note: remove entries < 10000

source("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Data manipulation/fun_TenKMin.R")

LabEmployReturn10K <-TenKMin(incomeVectListLabEmploy, ageVectListLabEmploy, enrollVectListLabEmploy, employVectListLabEmploy)
incomeVectListLabEmploy10K<-LabEmployReturn10K[[1]]
ageVectListLabEmploy10K<-LabEmployReturn10K[[2]]
enrollVectListLabEmploy10K<-LabEmployReturn10K[[3]]
employVectListLabEmploy10K<- LabEmployReturn10K[[4]]

#Project======================================================================================
  #inputs
    tau =27.8818
    m = -3.8149
    b = 36241
    n =  -0.2445
    a = 2234.3  

  #project
    source("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Data manipulation/fun_projectIncomes.R")
      outListLabNm <- projectIncomes(ageVectListLabNm, incomeVectListLabNm, enrollVectListLabNm, "NmNoFill")
        coeffVectLabNm<- outListLabNm[[1]]
        outMatrixLabNm<- outListLabNm[[2]]
      outListLabEmploy <- projectIncomes(ageVectListLabEmploy, incomeVectListLabEmploy, enrollVectListLabEmploy, "EmployNoFill")
        coeffVectLabEmploy<- outListLabEmploy[[1]]
        outMatrixLabEmploy<- outListLabEmploy[[2]]
      outListLabNmFilled <- projectIncomes(ageVectListLabNmFilled, incomeVectListLabNmFilled, enrollVectListLabNmFilled, "NmFillMiddle")
        coeffVectLabNmFilled<- outListLabNmFilled[[1]]
        outMatrixLabNmFilled<- outListLabNmFilled[[2]]
      outListLabEmployFilled <- projectIncomes(ageVectListLabEmployFilled, incomeVectListLabEmployFilled, enrollVectListLabEmployFilled, "EmployFillMiddle")
        coeffVectLabEmployFilled<- outListLabEmployFilled[[1]]
        outMatrixLabEmployFilled<- outListLabEmployFilled[[2]]
      outListLabEmploy10K <- projectIncomes(ageVectListLabEmploy10K, incomeVectListLabEmploy10K, enrollVectListLabEmploy10K, "Employ10K")
        coeffVectLabEmploy10K<- outListLabEmploy10K[[1]]
        outMatrixLabEmploy10K<- outListLabEmploy10K[[2]]

  #save this workspace for later loading
    save.image(file="alloptions.RData")

#check fit of each method=====================================================================
  #add all possibilities to data set
    ENROLL_DATA$b0NmFilled<- coeffVectLabNmFilled[,1]
    ENROLL_DATA$b0EmployFilled<-coeffVectLabEmployFilled[,1]
    ENROLL_DATA$b0Nm<-coeffVectLabNm[,1]
    ENROLL_DATA$b0Employ<-coeffVectLabEmploy[,1]
    ENROLL_DATA$b0Employ10K<-coeffVectLabEmploy10K[,1]
  
  #add category variable
    ENROLL_DATA$cat <- -3
    for (i in 1:nrow(ENROLL_DATA)){
      ENROLL_DATA$cat[i] <- toString(paste(ENROLL_DATA$BestAd5b[i],ENROLL_DATA$BestAtt5b[i], sep = ""))
    }

  #change NAs to -3 for easier manipulation
    ENROLL_DATA[is.na(ENROLL_DATA)] <- -3

  #do check for each data set type
    #this check now excludes entries where b0 is out of range
    source("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Data manipulation/fun_checkPredictionAbility.R")
    checkPredictionAbility(ENROLL_DATA$b0NmFilled, "b0NmFillMiddle")
    checkPredictionAbility(ENROLL_DATA$b0EmployFilled, "b0EmployFillMiddle")
    checkPredictionAbility(ENROLL_DATA$b0Nm, "b0NmNoFill")
    checkPredictionAbility(ENROLL_DATA$b0Employ, "b0EmployNoFill")
    checkPredictionAbility(ENROLL_DATA$b0Employ10K, "b0EmployNoFill10K")

#investigate standard errors for each category=============================================================
  #pull standard deviations and errors using all data
    source("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Data manipulation/fun_getStError.R")
    stErListLabNm = getStError(outMatrixLabNm, coeffVectLabNm[1])
      stDevLabNm = stErListLabNm[[1]]
      nLabNm = stErListLabNm[[2]]
      stErLabNm = stDevLabNm/(sqrt(nLabNm))
    stErListLabEmploy = getStError(outMatrixLabEmploy, coeffVectLabEmploy[1])
      stDevLabEmploy = stErListLabEmploy[[1]]
      nLabEmploy = stErListLabEmploy[[2]]
      stErLabEmploy = stDevLabEmploy/(sqrt(nLabEmploy))
    stErListLabNmFilled = getStError(outMatrixLabNmFilled, coeffVectLabNmFilled[1])
      stDevLabNmFilled = stErListLabNmFilled[[1]]
      nLabNmFilled = stErListLabNmFilled[[2]]
      stErLabNmFilled = stDevLabNmFilled/(sqrt(nLabNmFilled))
    stErListLabEmployFilled = getStError(outMatrixLabEmployFilled, coeffVectLabEmployFilled[1])
      stDevLabEmployFilled = stErListLabEmployFilled[[1]]
      nLabEmployFilled = stErListLabEmployFilled[[2]]
      stErLabEmployFilled = stDevLabEmployFilled/(sqrt(nLabEmployFilled))
    stErListLabEmploy10K = getStError(outMatrixLabEmploy10K, coeffVectLabEmploy10K[1])
      stDevLabEmploy10K = stErListLabEmploy10K[[1]]
      nLabEmploy10K = stErListLabEmploy10K[[2]]
      stErLabEmploy10K = stDevLabEmploy10K/(sqrt(nLabEmploy10K))

  #save this workspace for later loading and save output to file
    save.image(file="wsterr.RData")
    outDat = data.frame(stErLabNmFilled, stErLabEmployFilled, stErLabNm, stErLabEmploy, stErLabEmploy10K, stDevLabNmFilled, stDevLabEmployFilled, stDevLabNm, stDevLabEmploy, stDevLabEmploy10K,nLabNmFilled, nLabEmployFilled, nLabNm, nLabEmploy, nLabEmploy10K)
    write.csv(outDat, "C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/stErOut.csv")

  #calculate standard errors using only "real" values
    source("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Data manipulation/fun_getStErrorReals.R")
    stErListLabNm = getStErrorReals(outMatrixLabNm, coeffVectLabNm[1])
      stDevLabNm = stErListLabNm[[1]]
      nLabNm = stErListLabNm[[2]]
      stErLabNm = stDevLabNm/(sqrt(nLabNm))
    stErListLabEmploy = getStErrorReals(outMatrixLabEmploy, coeffVectLabEmploy[1])
      stDevLabEmploy = stErListLabEmploy[[1]]
      nLabEmploy = stErListLabEmploy[[2]]
      stErLabEmploy = stDevLabEmploy/(sqrt(nLabEmploy))
    stErListLabNmFilled = getStErrorReals(outMatrixLabNmFilled, coeffVectLabNmFilled[1])
      stDevLabNmFilled = stErListLabNmFilled[[1]]
      nLabNmFilled = stErListLabNmFilled[[2]]
      stErLabNmFilled = stDevLabNmFilled/(sqrt(nLabNmFilled))
    stErListLabEmployFilled = getStErrorReals(outMatrixLabEmployFilled, coeffVectLabEmployFilled[1])
      stDevLabEmployFilled = stErListLabEmployFilled[[1]]
      nLabEmployFilled = stErListLabEmployFilled[[2]]
      stErLabEmployFilled = stDevLabEmployFilled/(sqrt(nLabEmployFilled))
    stErListLabEmploy10K = getStErrorReals(outMatrixLabEmploy10K, coeffVectLabEmploy10K[1])
      stDevLabEmploy10K = stErListLabEmploy10K[[1]]
      nLabEmploy10K = stErListLabEmploy10K[[2]]
      stErLabEmploy10K = stDevLabEmploy10K/(sqrt(nLabEmploy10K))

    outDat = data.frame(stErLabNmFilled, stErLabEmployFilled, stErLabNm, stErLabEmploy, stErLabEmploy10K, stDevLabNmFilled, stDevLabEmployFilled, stDevLabNm, stDevLabEmploy, stDevLabEmploy10K,nLabNmFilled, nLabEmployFilled, nLabNm, nLabEmploy, nLabEmploy10K)
    write.csv(outDat, "C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/stErOutReals.csv")
    save.image(file="wsterr.RData")

#investigate getting stronger predictor using best strategy and other variables==========================
  #best strategy is NmEmploy10K

  #read in other relevant predictor information
    INCOME_PREDS<- read.csv("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/incomepredictors.csv")
    ENROLL_DATA2<- merge(x = ENROLL_DATA, y = INCOME_PREDS, by = "PUBID_1997", all.x = TRUE)
    COLLEGE_NUM<- read.csv("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/collegenumber.csv")
    ENROLL_DATA2<- merge(x = ENROLL_DATA2, y = COLLEGE_NUM, by = "PUBID_1997", all.x = TRUE)
    LOC_DATA <- read.csv("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/desensitizedloc.csv")
    ENROLL_DATA2<- merge(x = ENROLL_DATA2, y = LOC_DATA, by = "PUBID_1997", all.x = TRUE)
    SIC_DATA <- read.csv("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/sic.csv")
    ENROLL_DATA2<- merge(x = ENROLL_DATA2, y = SIC_DATA, by = "PUBID_1997", all.x = TRUE)
    ENROLL_DATA<-ENROLL_DATA2

  #create neceessary variables
    #MAJOR gives major
    #GEO gives desensitized geography code
    #GRADES gives highschool grades
    #COLLEGECOMPLETED says whether college has been completed; those still enrolled in original college have a value of -3
    #MAJOR2 is a categorical variable of majors
    source("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Data manipulation/fun_fillIncomePredictors.R")
    PREDICT_DATA = fillIncomePredictors(ENROLL_DATA)

  #create prediction datasets (only for Employ10K here)
      b0ProjectData <- data.frame(b0 = PREDICT_DATA$b0Employ10K, cat = PREDICT_DATA$cat, admit = PREDICT_DATA$BestAd5, attend = PREDICT_DATA$BestAtt5, major = PREDICT_DATA$MAJOR, major2 = PREDICT_DATA$MAJOR2, gpa = PREDICT_DATA$GRADES, geo = PREDICT_DATA$GEO, collgrad = PREDICT_DATA$COLLEGECOMPLETE, satm <- PREDICT_DATA$CVC_SAT_MATH_SCORE_2007_XRND, satv <- PREDICT_DATA$CVC_SAT_VERBAL_SCORE_2007_XRND)
      levels(b0ProjectData$cat) <- c(levels(b0ProjectData$cat),-3)
      b0ProjectData[b0ProjectData$attend == 7,]$cat <- -3
      b0ProjectData[b0ProjectData$admit == 7,]$cat <- -3
      b0ProjectData[b0ProjectData$admit == 7,]$admit <- -3
      b0ProjectData[b0ProjectData$attend == 7,]$attend <- -3
      b0ProjectData[b0ProjectData == -3] <- NA
      b0ProjectData[b0ProjectData == -4] <- NA




      #7 values for admission/attendance should not be included, and associated categories should be discarded
      b0ProjectData[b0ProjectData$admit == 7,]$cat <- -3
      b0ProjectData[b0ProjectData$attend == 7,]$cat <- -3
      b0ProjectData[b0ProjectData$admit == 7,]$admit <- -3
      b0ProjectData[b0ProjectData$attend == 7,]$attend <- -3
      b0ProjectData[b0ProjectData == -3] <- NA 

IncProjectData$SATM <- ENROLL_DATA$CVC_SAT_MATH_SCORE_2007_XRND
IncProjectData$SATV <- ENROLL_DATA$CVC_SAT_VERBAL_SCORE_2007_XRND
IncProjectData[IncProjectData$SATM <0,]$SATM <- -3
IncProjectData[IncProjectData$SATV <0,]$SATV <- -3
IncProjectData[IncProjectData == -3] <- NA

  
