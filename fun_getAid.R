getAid<- function(varList, k){
  maxK = length(varList)
  varString = ""
  strList = NA
  doneCheck = 0
  varString = varList[k]
  strList = strsplit(varString, "_", fixed = TRUE)
  strList = strList[[1]]
  strList = strList[strList != "000001"]
  #debug
    TEMP_LONG_DATA$loop[i]= strList[3]
    TEMP_LONG_DATA$school[i] = strList[4]
    TEMP_LONG_DATA$year[i] = strList[5]
  schoolAidInd =paste("YCOC_055_", strList[3], "_", strList[4], "_", strList[5], sep= "")  
  schoolAidStr= paste("YCOC_055B_", strList[3], "_", strList[4], "_", strList[5], sep= "")  
  #extract aid amount
  if (schoolAidInd %in% colnames(curData)){ #indicator variable found
    curInd= curData[1, schoolAidInd]
    if (curInd==0){ #no aid received from this school
      TEMP_LONG_DATA$SCHOOLAID[i] = 0
      doneCheck = 1
    } 
    
    else if (curInd == 1){ #aid offer received
      if (schoolAidStr %in% colnames(curData)){ #aid offer variable exists
        if (curData[1,schoolAidStr] >=0){
          TEMP_LONG_DATA$SCHOOLAID[i] =curData[1,schoolAidStr]
          doneCheck = 1
        } else {
          if (k <maxK){
            getAid(varList, k+1)
          } else{
          TEMP_LONG_DATA$SCHOOLAID[i] = -6 #aid amount variable B is <0 (missing indicator), TBD may want to make valid skips yield a zero here
          }
        }
      } else {
        if (k <maxK){
          getAid(varList, k+1)
        } else{
          TEMP_LONG_DATA$SCHOOLAID[i] = -7 #B variable (amount) not found
        }
      }
    } 
    
    else if(curInd == 2){
      if (k <maxK){
        getAid(varList, k+1)
      } else{
        TEMP_LONG_DATA$SCHOOLAID[i] = -8 #no aid decision received yet
      }
    } 
  } 
  
  else{ #there was no aid indicator variable for this loop
    if (k <maxK){
      getAid(varList, k+1)
    } else{
      TEMP_LONG_DATA$SCHOOLAID[i] = -9 # no indicator variable found
    }
  }
  return(TEMP_LONG_DATA$SCHOOLAID[i])
}
