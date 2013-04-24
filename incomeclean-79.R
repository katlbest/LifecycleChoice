#NOTES:======================================================================

#libraries ====================================================================
  library(reshape)

#clear workspace ==============================================================
  rm(list = ls())

#data i/o=======================================================================
  INCOME_DATA <- read.csv("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Stock market correlation/79income_revisednames.csv")

#clean data=======================================================================
  #income is in Q13_5_YEAR variables (previous calendar year)
  #enrollment is in ENROLLMTREVYY_YYYY
  #highest degree ever received is q3-10b
  #job industry is in QES_55E_CODE_##_2000
  #HRSWK_PCY_YYYY gives hours worked (previous calendar year)
  
  #set up vector lists for storing data needed for projection
    ageVectList <- list()
    incomeVectList <- list()
    employVectList <- list()

  #populate income vector
  yearVect = c("1979", "1980", "1981", "1982", "1983", "1984", "1985", "1986", "1987", "1988", "1989", "1990", "1991", "1992", "1994", "1996", "1998", "2000", "2002", "2004", "2006", "2008", "2010")
  for (i in 1:nrow(INCOME_DATA)){
    incVect = rep(NA, length(yearVect))
    for (j in 1: length(yearVect)){
      curStr = paste("Q13_5_", yearVect[j], sep = "")
      curInc = INCOME_DATA[i,curStr]
      if (curInc >= 10000){ #this is 10K restriction we imposed on our own data, TBD add hours worked description here
        incvect[j]==curInc
      }
    }
  }
#next steps
  fix above to make sure all restrictins met
want correlation of errors
take the correlation of each time series with the errors and then take avg correlation value or plot by characteristics
see if ed or major predicts correlation
    
    
    
    incVect = c(ENROLL_DATA$znm1[i], ENROLL_DATA$znm2[i], ENROLL_DATA$znm3[i], ENROLL_DATA$znm4[i], ENROLL_DATA$znm5[i], ENROLL_DATA$znm6[i],ENROLL_DATA$znm7[i],ENROLL_DATA$znm8[i])
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

  
  