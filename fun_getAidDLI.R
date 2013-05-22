getAidDLI<- function(varList){
  outList = rep(NA, length(varList))
  for (k in 1:length(varList)){
    varString = ""
    strList = NA
    varString = varList[k]
    strList = strsplit(varString, "_", fixed = TRUE)
    strList = strList[[1]]
    schoolAidInd =paste("YCOC_DLI_040_", strList[5], "_", strList[6], sep= "")  
    schoolAidStr= paste("YCOC_DLI_047_", strList[5], "_", strList[6], sep= "")  
    changedMindInd= paste("YCOC_DLI_045_", strList[5], "_", strList[6], sep= "") 
    #extract aid amount
    if (schoolAidInd %in% colnames(curData)){ #indicator variable found-
      curInd= curData[1, schoolAidInd]
      if (curInd==0){ #no aid received from this school
        print("ind0")
        print(i)
        outList[k] = 0
      } 
      
      else if (curInd == 1){ #aid offer received
        print("ind1")
        print(i)
        if (schoolAidStr %in% colnames(curData)){ #aid offer variable exists
          outList[k] = curData[1,schoolAidStr]
        } 
        else {
          outList[k] = -7 #B variable (amount) not found
        }
      }
      
      else if(curInd == 2){
        print("ind2")
        print(i)
        outList[k] = -8 #no aid decision received yet
      }
      
      else if(curInd == -4 & strList[6]=="2004"){ #then we may be in a mind changing situation
        if (curData[1,changedMindInd]==2){
          outList[k]=0 #no aid received after all
        } 
        else{
          outList[k] = curData[1, schoolAidInd]*100 #keep track of missingness indicator like below
        }
      }
      else { #curind was another negative value
        outList[k] = curData[1, schoolAidInd]*100 #to be able to distinguish these missing inficators from value missing indicators
      }
    }
    else{ #no indicator found
      if(strList[6]=="2004") { #we should check for the 2 entry in changed mind indicator
        if (curData[1,changedMindInd]==2){
          outList[k]=0 #no aid received after all
        } 
        else{
          outList[k] = -9 #no indicator found and missin
        }
      } 
      else{
        outList[k] = -9 #no indicator found and missing
      }
    }
  }
  return(outList)
}
