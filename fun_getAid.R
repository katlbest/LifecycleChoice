getAid<- function(varList){
  outList = rep(NA, length(varList))
  for (k in 1:length(varList)){
    varString = ""
    strList = NA
    varString = varList[k]
    strList = strsplit(varString, "_", fixed = TRUE)
    strList = strList[[1]]
    strList = strList[strList != "000001"]
    schoolAidInd =paste("YCOC_055_", strList[3], "_", strList[4], "_", strList[5], sep= "")  
    schoolAidStr= paste("YCOC_055B_", strList[3], "_", strList[4], "_", strList[5], sep= "")  
    #extract aid amount
    if (schoolAidInd %in% colnames(curData)){ #indicator variable found-
      curInd= curData[1, schoolAidInd]
      if (curInd==0){ #no aid received from this school
        outList[k] = 0
      } 
      
      else if (curInd == 1){ #aid offer received
        if (schoolAidStr %in% colnames(curData)){ #aid offer variable exists
          outList[k] = curData[1,schoolAidStr]
          } 
        else {
          outList[k] = -7 #B variable (amount) not found
            }
        }
      
      else if(curInd == 2){
          outList[k] = -8 #no aid decision received yet
      }
      else { #curind was a negative value
        outList[k] = curData[1, schoolAidInd]*100 #to be able to distinguish these missing inficators from value missing indicators
      }
    }
    else{
      outList[k] = -9 #no indicator found
    }
  }
  return(outList)
}
