fillIncomePredictors<- function(inData){

  outData = inData  
  
  #populate major
    #use latest major at "real school"
    outData$MAJOR <- -3
    for (i in 1:nrow(outData)){
      colNumString = paste("YSCH_21300", outData$COLLEGEID_ROSTERNUM2[i], "_", sep ="")
      varVector = c()
      curYear = outData$COLLEGEID_YEAR2[i]
      yearString = paste("_", outData$COLLEGEID_YEAR2[i], sep ="")
      termVect = c("01","02", "03", "04","05", "06", "07","08","09","10","11","12", "13")
      for (j in 1:length(termVect)){
        curstr = paste(colNumString, termVect[j], yearString, sep = "")
        if (curstr %in% colnames(outData)){
          varVector[length(varVector)+1] = curstr
        }
      }
      #now have vector of variables to check, use most recent
      if (length(varVector)>0){
        for (j in length(varVector):1){
          curStr = paste("outData$", varVector[j], sep = "")
          curMajor = eval(parse(text =curStr))[i]
          if (outData$MAJOR[i] == -3 & curMajor >= 0){
            outData$MAJOR[i]= curMajor
          }
        }
      }
      else {
        outData$MAJOR[i]= -3
      }
    }
  
  #populate area of residence
    #geo variables are pretty straight forward, GEO03--use desensitized
    #get mode, and if all are different use the latest avaialble
    outData$GEO = -3
    for (i in 1:nrow(outData)){
      geoVector = c()
      if (outData$GEO03_2010[i]>=0){
        geoVector[length(geoVector)+1]= outData$GEO03_2010[i]
      }
      if (outData$GEO03_2009[i]>=0){
        geoVector[length(geoVector)+1]= outData$GEO03_2009[i]
      }
      if (outData$GEO03_2008[i]>=0){
        geoVector[length(geoVector)+1]= outData$GEO03_2008[i]
      }
      if (outData$GEO03_2009[i]>=0){
        geoVector[length(geoVector)+1]= outData$GEO03_2009[i]
      }
      if (length(geoVector)>0){
        uniques <- unique(geoVector)
        outData$GEO[i]<- uniques[which.max(tabulate(match(geoVector, uniques)))]
      }
    }
  
  #populate GPA
    #use the one GPA variable (), #YSCH-7300 (dont worry about recode, etc)
    #value of 1 is bad, 8 is good, above 8 should be discarded
    yearVect  = c("2007", "2006","2005","2004","2003","2002","2001","2000","1999","1998","1997")
    outData$GRADES = -3
    for (i in 1:nrow(outData)){
      for (j in 1:length(yearVect)){
        curStr = paste("outData$YSCH_7300_", yearVect[j], sep = "")
        curValue  = eval(parse(text =curStr))[i]
        if (outData$GRADES[i] == -3 & curValue >= 0 & curValue <9){
          outData$GRADES[i] = curValue
        }
      }
    }
  
  #populate data on schooling completion (CVC_HIGHEST_DEGREE_EVER_XRND)
    outData$COLLEGECOMPLETED = -3
    for (i in 1:nrow(outData)){
      if (outData$stillInCollege[i]==0){
        if (outData$CVC_HIGHEST_DEGREE_EVER_XRND[i]>3){ #4 is bachelors
          outData$COLLEGECOMPLETED[i] = 1
        }
        else{
          outData$COLLEGECOMPLETED[i] = 0
        }
      }
    }
    
  #create second major variable (categorical)
  
  hardSci <- c(6, 21, 25)
  softSci <- c(3, 10, 11, 31, 32)
  bus <- c(7, 8)
  engineering <- c(9,13)
  health <- c(22, 23, 27, 29, 30, 28)
  hum <- c(1,2,5,12,14,15,17,19,18,20,24,26,33,4,16)
  
  outData$MAJOR2
  #reduce categories
  for (i in 1:nrow(outData)){
    if (outData$MAJOR[i] %in% hardSci){
      outData$MAJOR2[i]= 1
    }
    else if (outData$MAJOR[i] %in% softSci){
      outData$MAJOR2[i]= 2
    }
    else if (outData$MAJOR[i] %in% bus){
      outData$MAJOR2[i]= 3
    }
    else if (outData$MAJOR[i] %in% engineering){
      outData$MAJOR2[i]= 4
    }
    else if (outData$MAJOR[i] %in% health){
      outData$MAJOR2[i]= 5
    }
    else if (outData$MAJOR[i] %in% hum){
      outData$MAJOR2[i]= 6
    }
    else {
      outData$MAJOR2[i]= -3
    }
  }
    
  return(outData)

}