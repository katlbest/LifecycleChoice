fillIncomePredictors<- function(inData){

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
  
  #populate GPA
    #use the one GPA variable (), #YSCH-7300 (dont worry about recode, etc)
    #value of 1 is bad, 8 is good, above 8 should be discarded
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
    
  #create second major variable (categorical)
  hardSci <- c(6, 21, 25)
  softSci <- c(3, 10, 11, 31, 32)
  bus <- c(7, 8, 9, 13)
  health <- c(22, 23, 27, 29, 30, 28)
  hum <- c(1,2,5,12,14,15,17,19,18,20,24,26,33,4,16)
  ENROLL_DATA$MAJOR2
  #reduce categories
  for (i in 1:nrow(ENROLL_DATA)){
    if (ENROLL_DATA$MAJOR[i] %in% hardSci){
      ENROLL_DATA$MAJOR2[i]= 1
    }
    else if (ENROLL_DATA$MAJOR[i] %in% softSci){
      ENROLL_DATA$MAJOR2[i]= 2
    }
    else if (ENROLL_DATA$MAJOR[i] %in% bus){
      ENROLL_DATA$MAJOR2[i]= 3
    }
    else if (ENROLL_DATA$MAJOR[i] %in% health){
      ENROLL_DATA$MAJOR2[i]= 4
    }
    else if (ENROLL_DATA$MAJOR[i] %in% hum){
      ENROLL_DATA$MAJOR2[i]= 5
    }
    else {
      ENROLL_DATA$MAJOR2[i]= -3
    }
  }
    
  return(outData)

}