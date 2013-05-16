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
  #data already compiled from other parts of the code
    MERGE_DATA = read.csv("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Choice model inputs/mergedata.csv")
    CHOICE_DATA = merge(x = CHOICE_DATA, y = MERGE_DATA, by = "PUBID_1997", all.x = TRUE)
    colnames(CHOICE_DATA)[9] = "SAT_MATH"
    colnames(CHOICE_DATA)[10] = "SAT_VERBAL"
  #main dataset to be manipulated
    MANIP_DATA = data.frame(PUBID_1997 = CHOICE_DATA$PUBID_1997)
  #pull relevant from BLS
    MANIP_DATA = merge(x = MANIP_DATA, y = BLS_DATA, by = "PUBID_1997", all.x = TRUE)

#populate CHOICE_DATA from BLS data
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
    source("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Data manipulation/fun_fillMiss.R")
    for (i in 1:nrow(MANIP_DATA)){
      #household size
        hhSizeVar = paste("CV_HH_SIZE_", toString(CHOICE_DATA$CHOICE_YEAR[i]), sep = "")
        if(hhSizeVar %in% colnames(MANIP_DATA)){
          if(MANIP_DATA[i, hhSizeVar]>0){
            CHOICE_DATA$HH_SIZE[i]= MANIP_DATA[i, hhSizeVar]
          } else{
            CHOICE_DATA$HH_SIZE[i]= fillMiss(hhSizeVar, i, TRUE)
          }
        }
      #household income
        hhIncomeVar = paste("CV_INCOME_GROSS_YR_", toString(CHOICE_DATA$CHOICE_YEAR[i]), sep = "")
        if(hhIncomeVar %in% colnames(MANIP_DATA)){
          if(MANIP_DATA[i, hhIncomeVar]>0){
            CHOICE_DATA$HH_INCOME[i]= MANIP_DATA[i, hhIncomeVar]
          } else{
            CHOICE_DATA$HH_INCOME[i]= fillMiss(hhIncomeVar, i, TRUE)
          }
        }
      #urban rural
        urbanRuralVar = paste("CV_URBAN_RURAL_", toString(CHOICE_DATA$CHOICE_YEAR[i]), sep = "")
        if(urbanRuralVar %in% colnames(MANIP_DATA)){
          if(MANIP_DATA[i, urbanRuralVar]>0){
            CHOICE_DATA$URBAN_RURAL[i]= MANIP_DATA[i, urbanRuralVar]
          } else{
            CHOICE_DATA$URBAN_RURAL[i]= fillMiss(urbanRuralVar, i, TRUE)
          }
        }
      #high school type
        schoolTypeVar = paste("NEWSCH_TYPE_.ROS_ITEM._L1_", toString(CHOICE_DATA$CHOICE_YEAR[i]), sep = "")
        if(schoolTypeVar %in% colnames(MANIP_DATA)){
          if(MANIP_DATA[i,schoolTypeVar]>0){
            CHOICE_DATA$SCHOOL_TYPE[i]= MANIP_DATA[i, schoolTypeVar]
          }else{
            CHOICE_DATA$SCHOOL_TYPE[i]= fillMiss(schoolTypeVar, i, FALSE)
          }
        }       
    }

#CREATE CHOICE FILE WITH EACH SCHOOL ON A LINE=======================================================
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
    #LONG_DATA$loop = NA
    #LONG_DATA$school = NA
    #LONG_DATA$year = NA
    #LONG_DATA$geoschool = NA
    #LONG_DATA$geoyear = NA
    LONG_DATA$SCHOOLAID= NA
    LONG_DATA$INDEPAID = NA
    for (i in 1: nrow(LONG_DATA)){
      strList = NA
      curData = FIN_DATA[FIN_DATA$PUBID_1997 == LONG_DATA$PUBID_1997[i],]
      curSchool = LONG_DATA$AdmittedSchool[i]
      #search for YCOC
        for (j in 1:length(varListYCOC)){
          if (curData[1,varListYCOC[j]]==curSchool){
            varString = varListYCOC[j]
            strList = strsplit(varString, "_", fixed = TRUE)
            strList = strList[[1]]
            strList = strList[strList != "000001"]
            #LONG_DATA$loop[i]= strList[3]
            #LONG_DATA$school[i] = strList[4]
            #LONG_DATA$year[i] = strList[5]
            schoolAidStr = paste("YCOC_055B_", strList[3], "_", strList[4], "_", strList[5], sep= "")
            otherAidStr = paste("YCOC_022_", strList[3], "_", strList[5],sep = "")
            LONG_DATA$SCHOOLAID[i] = curData[1,schoolAidStr]
            LONG_DATA$INDEPAID[i] = curData[1,otherAidStr]
          }
        }
      #if not found, search for GEO69
        if (is.na(strList)){
          for (j in 1:length(varListGEO)){
            if (curData[1,varListGEO[j]]==curSchool){
              varString = varListGEO[j]
              strList = strsplit(varString, "_", fixed = TRUE)
              strList = strList[[1]]
              LONG_DATA$geoschool[i] = strList[2] #there is a problem here where 1997 has no school number, but it does not occur so is not handled
              LONG_DATA$geoyear[i] = strList[3]
            }
          }
        }
    }
write.csv(LONG_DATA[,c("loop","school","year","geoschool", "geoyear")], "D:/test.csv")



