getAidYSCH<- function(varList){
  outList = rep(-3, length(varList))
  for (k in 1:length(varList)){
    varString = varList[k]
    strList = strsplit(varString, "_", fixed = TRUE)
    strList = strList[[1]]
    
    #get maxTerm
      termStr = paste("YSCH_26980_", strList[2], "_", strList[3], sep = "")
      if (termStr %in% colnames(curData)){
        myMax = curData[1, termStr]
        if (myMax == 1){
          maxTerm = 2
        }else if (myMax == 2){
          maxTerm = 4
        } else if (myMax == 3){
          maxTerm = 3
        } else{
          maxTerm = 2
        }
      } else{
        maxTerm = 2 #since all the found ones have semesters, we assume missing do too
      }
    
    #get grant amount
      grantStr = "YSCH_25400"
      grantVarList = colnames(curData)[grep(grantStr, colnames(curData))]
      grantStr = paste(strList[2], strList[3], sep = "_")
      grantVarList= grantVarList[grep(grantStr, grantVarList)]
      for (j in 1:(min(maxTerm, length(grantVarList)))){
        if (curData[1,grantVarList[j]]>=0){
          outList[k] =  max(outList[k], 0)+curData[1,grantVarList[j]]
        }
      }
    
    #get loan amount
      loanStr = "YSCH_25600"
      loanVarList = colnames(curData)[grep(loanStr, colnames(curData))]
      loanStr = paste(strList[2], strList[3], sep = "_")
      loanVarList= loanVarList[grep(loanStr, loanVarList)]
      for (j in 1:(min(maxTerm, length(loanVarList)))){
        if (curData[1,loanVarList[j]]>=0){
          outList[k] =  max(outList[k], 0)+curData[1,loanVarList[j]]
        }
      }
    
    #get other amount
      otherStr = "YSCH_26400"
      otherVarList = colnames(curData)[grep(otherStr, colnames(curData))]
      otherStr = paste(strList[2], strList[3], sep = "_")
      otherVarList= otherVarList[grep(otherStr, otherVarList)]
      if (length(otherVarList >0)){
        for (j in 1:(min(maxTerm, length(otherVarList)))){
          if (curData[1,otherVarList[j]]>=0){
            outList[k] =  max(outList[k], 0)+curData[1,otherVarList[j]]
          } 
        }
      }
    
  }
  return(outList)
}
