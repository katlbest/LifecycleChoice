#NOTES:======================================================================
  #this file compiles the college choice model data that is derived from BLS and other sources best handled in R (non-python)
  
#libraries ====================================================================
  library(plyr)
  #library(ggplot2)
  #library(MASS)
  #li brary(Hmisc)
  #library(reshape2)

#clear workspace ==============================================================
  rm(list = ls())

#read basis and individual characteristic merging file ==============================================================
  #main file--clean data
    CHOICE_DATA = read.csv("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Choice model inputs/inputs.csv")
  #data on individuals directly from BLS
    BLS_DATA = read.csv("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Choice model inputs/blsvarsforchoice.csv")
  #state residency data
    RES_DATA = read.csv("D:/StateOfRes.csv")
  #data already compiled from other parts of the code
    MERGE_DATA = read.csv("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Choice model inputs/mergedata.csv")
    CHOICE_DATA = merge(x = CHOICE_DATA, y = MERGE_DATA, by = "PUBID_1997", all.x = TRUE)
    colnames(CHOICE_DATA)[9] = "SAT_MATH"
    colnames(CHOICE_DATA)[10] = "SAT_VERBAL"
  #main dataset to be manipulated
    MANIP_DATA = data.frame(PUBID_1997 = CHOICE_DATA$PUBID_1997)
  #pull relevant from BLS
    MANIP_DATA = merge(x = MANIP_DATA, y = BLS_DATA, by = "PUBID_1997", all.x = TRUE)

#populate CHOICE_DATA from BLS data and RES data
  #religious preference
    CHOICE_DATA$STUDENT_RELIGION = -3
    CHOICE_DATA[MANIP_DATA$WHAT_CURR_RELIGIOUS_PREFERENCE_1997 >=0,]$STUDENT_RELIGION = MANIP_DATA[MANIP_DATA$WHAT_CURR_RELIGIOUS_PREFERENCE_1997 >=0,]$WHAT_CURR_RELIGIOUS_PREFERENCE_1997
    CHOICE_DATA[MANIP_DATA$WHAT_CURR_RELIGIOUS_PREFERENCE_1997 < 0,]$STUDENT_RELIGION = MANIP_DATA[MANIP_DATA$WHAT_CURR_RELIGIOUS_PREFERENCE_1997 < 0,]$R_CURR_REL_PREF_2005
  #parent's highest education
    CHOICE_DATA$DAD_ED = MANIP_DATA$CV_HGC_RES_DAD_1997
    CHOICE_DATA$MOM_ED = MANIP_DATA$CV_HGC_RES_MOM_1997
  #yearly variables
    CHOICE_DATA$HH_SIZE = -3
    CHOICE_DATA$HH_INCOME = -3
    CHOICE_DATA$URBAN_RURAL = -3
    CHOICE_DATA$SCHOOL_TYPE = -3
    CHOICE_DATA$RES_STATE = -3
    source("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Data manipulation/fun_fillMiss.R")
    for (i in 1:nrow(MANIP_DATA)){
      #household size
        hhSizeVar = paste("CV_HH_SIZE_", toString(CHOICE_DATA$CHOICE_YEAR[i]), sep = "")
        if(hhSizeVar %in% colnames(MANIP_DATA)){
          if(MANIP_DATA[i, hhSizeVar]>0){
            CHOICE_DATA$HH_SIZE[i]= MANIP_DATA[i, hhSizeVar]
          } else{
            CHOICE_DATA$HH_SIZE[i]= fillMiss(hhSizeVar, i, TRUE, "CHOICE")
          }
        }
      #household income
        hhIncomeVar = paste("CV_INCOME_GROSS_YR_", toString(CHOICE_DATA$CHOICE_YEAR[i]), sep = "")
        if(hhIncomeVar %in% colnames(MANIP_DATA)){
          if(MANIP_DATA[i, hhIncomeVar]>0){
            CHOICE_DATA$HH_INCOME[i]= MANIP_DATA[i, hhIncomeVar]
          } else{
            CHOICE_DATA$HH_INCOME[i]= fillMiss(hhIncomeVar, i, TRUE, "CHOICE")
          }
        }
      #urban rural
        urbanRuralVar = paste("CV_URBAN_RURAL_", toString(CHOICE_DATA$CHOICE_YEAR[i]), sep = "")
        if(urbanRuralVar %in% colnames(MANIP_DATA)){
          if(MANIP_DATA[i, urbanRuralVar]>0){
            CHOICE_DATA$URBAN_RURAL[i]= MANIP_DATA[i, urbanRuralVar]
          } else{
            CHOICE_DATA$URBAN_RURAL[i]= fillMiss(urbanRuralVar, i, TRUE, "CHOICE")
          }
        }
      #high school type
        schoolTypeVar = paste("NEWSCH_TYPE_.ROS_ITEM._L1_", toString(CHOICE_DATA$CHOICE_YEAR[i]), sep = "")
        if(schoolTypeVar %in% colnames(MANIP_DATA)){
          if(MANIP_DATA[i,schoolTypeVar]>0){
            CHOICE_DATA$SCHOOL_TYPE[i]= MANIP_DATA[i, schoolTypeVar]
          }else{
            CHOICE_DATA$SCHOOL_TYPE[i]= fillMiss(schoolTypeVar, i, FALSE, "CHOICE")
          }
        }       
      #state of residence in SCHOOL ATTENDANCE YEAR!
        resStateVar = paste("GEO02_", toString(CHOICE_DATA$COLLEGEID_YEAR2[i]), sep = "")
        if(resStateVar %in% colnames(RES_DATA)){
          if(RES_DATA[i,resStateVar]>0){
            CHOICE_DATA$RES_STATE[i]= RES_DATA[i, resStateVar]
          }else{
            CHOICE_DATA$RES_STATE[i]= fillMiss(resStateVar, i, TRUE, "START")
          }
        } 
    }
    #save.image(file = "D:/compileChoiceData.RData") #cannot save to D drive right now

#CREATE CHOICE FILE WITH EACH SCHOOL/STUDENT COMBO ON A LINE=======================================================
  LONG_DATA = read.table("D:/studentadmitdata.txt", header = TRUE)
  LONG_DATA = merge(x = LONG_DATA, y = CHOICE_DATA, by = "PUBID_1997", all.x = TRUE)
  LONG_DATA = LONG_DATA[!(is.na(LONG_DATA$MAJOR)),]
  #sanity check to make sure we have correct counts
    sum(LONG_DATA$AttendedIndicator) #400 attenders, which is correct
  #read financial data
    FIN_DATA = read.csv("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Choice model inputs/finaid.csv")
    REL_IDS = data.frame(PUBID_1997 = CHOICE_DATA$PUBID_1997)
    FIN_DATA = merge(x = REL_IDS, y = FIN_DATA, by = "PUBID_1997", all.x = TRUE)
    SECRET_DATA = read.csv("D:/MERGED_DATA_REDUCED.csv")
    strPREVCOL = "PREV_COL_APP_ID"
    strYCOC = "YCOC_050P"
    strGEO = "GEO69"
    varListPREVCOL= colnames(SECRET_DATA)[grep(strPREVCOL, colnames(SECRET_DATA))] 
    varListYCOC= colnames(SECRET_DATA)[grep(strYCOC, colnames(SECRET_DATA))]
    varListGEO= colnames(SECRET_DATA)[grep(strGEO, colnames(SECRET_DATA))] 
    varList = c(varListPREVCOL, varListYCOC, varListGEO)
    SECRET_DATA = SECRET_DATA[,c("PUBID_1997",varList)]
    FIN_DATA = merge(x=FIN_DATA, y = SECRET_DATA, by = "PUBID_1997", all.x = TRUE)

  #loop through long data and fill financial aid information for each school
    TEMP_LONG_DATA = LONG_DATA
    TEMP_LONG_DATA$SCHOOLAID= -6
    TEMP_LONG_DATA$INDEPAID = -6
    TEMP_LONG_DATA$ATTENDEDAID = -6
    TEMP_LONG_DATA$ATTENDEDAIDMISS = 0
    TEMP_LONG_DATA$loop= NA
    TEMP_LONG_DATA$school =NA
    TEMP_LONG_DATA$year = NA
    TEMP_LONG_DATA$MAXTERM = 0
    for (i in 1: nrow(TEMP_LONG_DATA)){
      varString = ""
      strList = NA
      curData = FIN_DATA[FIN_DATA$PUBID_1997 == TEMP_LONG_DATA$PUBID_1997[i],]
      curSchool = TEMP_LONG_DATA$AdmittedSchool[i]
      #search for YCOC
        for (j in 1:length(varListYCOC)){
          if (curData[1,varListYCOC[j]]==curSchool){
            varString = varListYCOC[j]
            strList = strsplit(varString, "_", fixed = TRUE)
            strList = strList[[1]]
            strList = strList[strList != "000001"]
            TEMP_LONG_DATA$loop[i]= strList[3]
            TEMP_LONG_DATA$school[i] = strList[4]
            TEMP_LONG_DATA$year[i] = strList[5]
            schoolAidStr= paste("YCOC_055B_", strList[3], "_", strList[4], sep= "")  
            #get variables that have this string in them
              varList055B= colnames(curData)[grep(schoolAidStr, colnames(curData))]
              k = 1
              while (k <= length(varList055B) & TEMP_LONG_DATA$SCHOOLAID[i]<0){
                TEMP_LONG_DATA$SCHOOLAID[i] = max(curData[1,varList055B[k]], TEMP_LONG_DATA$SCHOOLAID[i]) #if we already have a -4, we don't want to replace it with a -5 so we can know that we had a valid skip
                k = k+1
              }
            #get variables that have other aid string
              otherAidStr = paste("YCOC_022_01_",sep = "") #no school or loop number here since this is only asked once (it is not school specific)
              #question asks only about the first year of college
              varList022= colnames(curData)[grep(otherAidStr, colnames(curData))]
              k = 1
              while (k <= length(varList022) & TEMP_LONG_DATA$INDEPAID[i]<0){
                TEMP_LONG_DATA$INDEPAID[i] = max(curData[1,varList022[k]], TEMP_LONG_DATA$INDEPAID[i]) #if we already have a -4, we don't want to replace it with a -5 so we can know that we had a valid skip
                k = k+1
              }
          }
        }
      #if not found, search for GEO69, since this means the school is an attended school which was not found in the admitted list
        if (is.na(strList[1])){  #condition based on whether you found var in YCOC
          #note we could also condition on whether you actually found the fin aid data on the schools in YCOC, but this would bias towards the school actually attended. also, it doensn't actually help fill in any missing data
          #print(curData$PUBID_1997[1])
        #if (TEMP_LONG_DATA$SCHOOLAID[i] %in% c(-2, -5,-6)){#condition on whether you found fin aid data above
          for (j in 1:length(varListGEO)){
            if (curData[1,varListGEO[j]]==curSchool){
              varString = varListGEO[j]
              strList = strsplit(varString, "_", fixed = TRUE)
              strList = strList[[1]]
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
              TEMP_LONG_DATA$MAXTERM[i] = maxTerm
              grantStr = "YSCH_25400"
              grantVarList = colnames(curData)[grep(grantStr, colnames(curData))]
              grantStr = paste(strList[2], strList[3], sep = "_")
              grantVarList= grantVarList[grep(grantStr, grantVarList)]
              for (k in 1:(min(maxTerm, length(grantVarList)))){
                if (curData[1,grantVarList[k]]>=0){
                  TEMP_LONG_DATA$ATTENDEDAID[i] = max(TEMP_LONG_DATA$ATTENDEDAID[i], 0)+curData[1,grantVarList[k]]
                } else if (curData[1,grantVarList[k]] %in% c(-1,-2,-5)){
                  TEMP_LONG_DATA$ATTENDEDAIDMISS[i] = 1
                }
              }
              loanStr = "YSCH_25600"
              loanVarList = colnames(curData)[grep(loanStr, colnames(curData))]
              loanStr = paste(strList[2], strList[3], sep = "_")
              loanVarList= loanVarList[grep(loanStr, loanVarList)]
              for (k in 1:(min(maxTerm, length(loanVarList)))){
                if (curData[1,loanVarList[k]]>=0){
                  TEMP_LONG_DATA$ATTENDEDAID[i] = max(TEMP_LONG_DATA$ATTENDEDAID[i], 0)+curData[1,loanVarList[k]]
                } else if (curData[1,loanVarList[k]] %in% c(-1,-2,-5)){
                  TEMP_LONG_DATA$ATTENDEDAIDMISS[i] = 1
                }
              }
              otherStr = "YSCH_26400"
              otherVarList = colnames(curData)[grep(otherStr, colnames(curData))]
              otherStr = paste(strList[2], strList[3], sep = "_")
              otherVarList= otherVarList[grep(otherStr, otherVarList)]
              if (length(otherVarList >0)){
                for (k in 1:(min(maxTerm, length(otherVarList)))){
                  if (curData[1,otherVarList[k]]>=0){
                    TEMP_LONG_DATA$ATTENDEDAID[i] = max(TEMP_LONG_DATA$ATTENDEDAID[i], 0)+curData[1,otherVarList[k]]  
                  } else if (curData[1,otherVarList[k]] %in% c(-1,-2,-5)){
                    TEMP_LONG_DATA$ATTENDEDAIDMISS[i] = 1
                  }
                }
              }
              TEMP_LONG_DATA$geoschool[i] = strList[2] #there is a problem here where 1997 has no school number, but it does not occur so is not handled
              TEMP_LONG_DATA$geoyear[i] = strList[3]
            }
          }
        }
    }
      
    #combine financial aid estimates
      for (i in 1:nrow(LONG_DATA)){
        LONG_DATA$SCHOOL_AID[i] = max(TEMP_LONG_DATA$SCHOOLAID[i], TEMP_LONG_DATA$ATTENDEDAID[i])
      }
      LONG_DATA$SHARED_AID = TEMP_LONG_DATA$INDEPAID
      write.csv(TEMP_LONG_DATA[,c("PUBID_1997", "loop", "school", "year","geoschool", "geoyear","SCHOOLAID","INDEPAID", "ATTENDEDAID", "ATTENDEDAIDMISS", "MAXTERM")], "D:/test.csv")
      write.csv(LONG_DATA, "D:/test2.csv")




