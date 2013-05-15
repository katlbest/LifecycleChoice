fillMiss<- function(var, index, futureCheck){
  choiceYr = CHOICE_DATA$CHOICE_YEAR[index]
  varStart = substr(var, 1, nchar(var)-4)
  varList = colnames(MANIP_DATA)[grep(varStart, colnames(MANIP_DATA))] 
  colIndex = which(varList==var)
  #search backwards first, pull first non-missing
    output = MANIP_DATA[i, var]
    j = colIndex-1
    while (j > 0 & output < 0){
      output =  MANIP_DATA[index, varList[j]]
      j = j-1
    }
  #search forward, pull first non-missing
    if (futureCheck == TRUE){
      j = colIndex+1
      while (j <= length(varList) & output < 0){
        output =  MANIP_DATA[index, varList[j]]
        j = j+1
      }
    }
  return(output)
}