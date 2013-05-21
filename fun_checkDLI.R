checkDLI<- function(school, year){
  curYear = as.integer(year)
  varYear = curYear + 1
  DLIAidInd = paste("YCOC_DLI_040_", school, "_", curYear, sep = "")
  DLIAidStr = paste("YCOC_DLI_047_", school, "_", curYear, sep = "")
  if (DLIAidInd %in% colnames(curData)){ #indicator variable found
    curInd2= curData[1, DLIAidInd]
    if (curInd==0){ #no aid received from this school
      TEMP_LONG_DATA$SCHOOLAID[i] = 0
    } else if (curInd == 1){ #aid offer received
      if (DLIAidStr %in% colnames(curData)){ #aid offer amount received
        TEMP_LONG_DATA$SCHOOLAID[i] =curData[1,DLIAidStr] #TBD update for categoricsl
      } else{ #no amount present, look in DLI variables
        TEMP_LONG_DATA$SCHOOLAID[i] =-6
      }
    } else if(curInd == 2 & varYear < 2010){
      checkDLI(school, curYear)
    } else {
      TEMP_LONG_DATA$SCHOOLAID[i] = -7
    }
  } else{ 
    TEMP_LONG_DATA$SCHOOLAID[i] = -8 
  }
  return(TEMP_LONG_DATA$SCHOOLAID[i])
}