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
    FIN_IND = read.csv("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Choice model inputs/finaidind.csv")
    FIN_DATA = merge(x=FIN_DATA, y = FIN_IND, by = "PUBID_1997", all.x = TRUE)
    YCOC_DLI = read.csv("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Choice model inputs/ycocdli.csv")
    FIN_DATA = merge(x=FIN_DATA, y = YCOC_DLI, by = "PUBID_1997", all.x = TRUE)

  #loop through long data and fill financial aid information for each school
    source("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Data manipulation/fun_checkDLI.R")
    source("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Data manipulation/fun_getAid.R")
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
      no055Match = 0
      curData = FIN_DATA[FIN_DATA$PUBID_1997 == TEMP_LONG_DATA$PUBID_1997[i],]
      curSchool = TEMP_LONG_DATA$AdmittedSchool[i]
      varListMATCH = c()
      for (j in 1:length(varListYCOC)){ #look for current school index in all YCOC variables, beginning with earliest
        if (curData[1,varListYCOC[j]]==curSchool){
          varListMATCH[length(varListMATCH)+1]= varListYCOC[j]
        }
      }
      if (length(varListMATCH>0)){
        getAid(varListMATCH, 1)
      } else{
        no055Match = 1
      }
    }

    #combine financial aid estimates
      for (i in 1:nrow(LONG_DATA)){
        LONG_DATA$SCHOOL_AID[i] = max(TEMP_LONG_DATA$SCHOOLAID[i], TEMP_LONG_DATA$ATTENDEDAID[i])
      }
      LONG_DATA$SHARED_AID = TEMP_LONG_DATA$INDEPAID
      write.csv(TEMP_LONG_DATA[,c("PUBID_1997", "loop", "school", "year","geoschool", "geoyear","SCHOOLAID","INDEPAID", "ATTENDEDAID", "ATTENDEDAIDMISS", "MAXTERM")], "D:/test.csv")
      write.csv(TEMP_LONG_DATA[,c("PUBID_1997", "loop", "school", "year","SCHOOLAID")], "D:/test3.csv")
      write.csv(LONG_DATA, "D:/test2.csv")




