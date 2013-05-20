fillMiss<- function(var, index, futureCheck, indexYear){
  varStart = substr(var, 1, nchar(var)-4)
  if (indexYear == "CHOICE"){
    choiceYr = CHOICE_DATA$CHOICE_YEAR[index]
    varList = colnames(MANIP_DATA)[grep(varStart, colnames(MANIP_DATA))] 
    lookupData= MANIP_DATA
  } else if (indexYear == "START"){
    choiceYr = CHOICE_DATA$COLLEGEID_YEAR2[index]
    varList = colnames(RES_DATA)[grep(varStart, colnames(RES_DATA))] 
    lookupData  = RES_DATA
  }
  colIndex = which(varList==var)
  #search backwards first, pull first non-missing
    output = lookupData[i, var]
    j = colIndex-1
    while (j > 0 & output < 0){
      output =  lookupData[index, varList[j]]
      j = j-1
    }
  #search forward, pull first non-missing
    if (futureCheck == TRUE){
      j = colIndex+1
      while (j <= length(varList) & output < 0){
        output =  lookupData[index, varList[j]]
        j = j+1
      }
    }
  return(output)
}