#NOTES:======================================================================
  #this file compiles the college choice model data that is derived from BLS and other sources best handled in R (non-python)
  
#libraries ====================================================================
  library(plyr)
  #library(ggplot2)
  #library(MASS)
  #library(Hmisc)
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
    source("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Data manipulation/fun_getAidDLI.R")
    source("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Data manipulation/fun_getAidYSCH.R")

    #get aid from YCOC-055 variables (main)
      aidList055 = list()
      for (i in 1: nrow(LONG_DATA)){
        curData = FIN_DATA[FIN_DATA$PUBID_1997 == LONG_DATA$PUBID_1997[i],]
        curSchool = LONG_DATA$AdmittedSchool[i]
        varListMATCH = c()
        for (j in 1:length(varListYCOC)){ #look for current school index in all YCOC variables, beginning with earliest
          if (curData[1,varListYCOC[j]]==curSchool | curData[1,varListYCOC[j]]*100==curSchool| curData[1,varListYCOC[j]]==curSchool*100){
            varListMATCH[length(varListMATCH)+1]= varListYCOC[j]
          }
        }
        if (length(varListMATCH>0)){
          aidList055[[i]] = getAid(varListMATCH)
        } else{
          aidList055[[i]]= -3
        }
      }

    #get aid from DLI variables (date of last interview), this only yields one result for i = 973, pubid = 6412
      #build varListDLI
      varListDLI= colnames(SECRET_DATA)[grep("PREV_COL_APP", colnames(SECRET_DATA))]
      aidListDLI = list()
      matchList = rep(NA, nrow(LONG_DATA))
      for (i in 1: nrow(LONG_DATA)){
        curData = FIN_DATA[FIN_DATA$PUBID_1997 == LONG_DATA$PUBID_1997[i],]
        curSchool = LONG_DATA$AdmittedSchool[i]
        varListMATCH = c()
        for (j in 1:length(varListDLI)){ 
          if (curData[1,varListDLI[j]]==curSchool| curData[1,varListDLI[j]]*100==curSchool | curData[1,varListDLI[j]]==curSchool* 100){
            varListMATCH[length(varListMATCH)+1]= varListDLI[j]
          }
        }
        if (length(varListMATCH>0)){
          aidListDLI[[i]] = getAidDLI(varListMATCH)
          matchList[i] = 1
        } else{
          aidListDLI[[i]]= -3
          matchList[i] = 0
        }
      }

    #get school-independent aid
      otherAidStr = paste("YCOC_022_01_",sep = "")
      varList022= colnames(FIN_DATA)[grep(otherAidStr, colnames(FIN_DATA))]
      aidListALLSCHOOL = list()
      for (i in 1:nrow(LONG_DATA)){
        aidListALLSCHOOL[[i]]= rep(NA, length(varList022))
        curData = FIN_DATA[FIN_DATA$PUBID_1997 == LONG_DATA$PUBID_1997[i],]
        for (k in 1:length(varList022)){
          aidListALLSCHOOL[[i]][k] = curData[1,varList022[k]]
        }
      }

    #get aid data from YSCH for attended schools
    aidListYSCH = list()
    for (i in 1:nrow(LONG_DATA)){
      curData = FIN_DATA[FIN_DATA$PUBID_1997 == LONG_DATA$PUBID_1997[i],]
      curSchool = LONG_DATA$AdmittedSchool[i]
      varListMATCH = c()
      for (j in 1:length(varListGEO)){
        if (curData[1,varListGEO[j]]==curSchool | curData[1,varListGEO[j]]*100==curSchool | curData[1,varListGEO[j]]==curSchool*100){
          varListMATCH[length(varListMATCH)+1]= varListGEO[j]
        }
      }
      if (length(varListMATCH>0)){
        aidListYSCH[[i]] = getAidYSCH(varListMATCH)
      } else{
        aidListYSCH[[i]]= -3
      }
    }

    #combine financial aid estimates
      YCOCEst = rep(-3, nrow(LONG_DATA))
      TOTEst = rep(-3, nrow(LONG_DATA))
      #aggregate YCOC data
      for (i in 1:nrow(LONG_DATA)){
        YCOCEst[i] = max(aidList055[[i]])
        TOTEst[i] = YCOCEst[i]
        if (YCOCEst[i]<0){
          #check DLI data and replace if better
          DLIEst = max(aidListDLI[[i]])
          TOTEst[i]=max(YCOCEst[i], DLIEst) #less negative missing values are more informative
        }
      }

    #get best estimate of YSCH data
      YSCHEst = rep(-3, nrow(LONG_DATA))
      #aggregate YSCH data
        for (i in 1:nrow(LONG_DATA)){
          YSCHEst[i] = max(aidListYSCH[[i]])
        }

    #fill in all possible data using YSCH (attended school) data, since this is the best estimate we have
      count = 0
      for (i in 1:nrow(LONG_DATA)){
        if (TOTEst[i]<0 & YSCHEst[i] >= 0){ #we may want to do this only where YCOCEst == -3 implying that school was never included in applied list
          count = count+1
          TOTEst[i] = YSCHEst[i]
        }
      }

    #update people in weird categories
    for (i in 1:nrow(LONG_DATA)){
      if (TOTEst[i]==-4 & YCOCEst[i]==-200){ #these are 2003 people who said they did not know if they got aid in initial interview, then were ineligible for followup, coded to dont know
        TOTEst[i] = -200 #don't know about their aid when asked IF THEY GOT ANY
      }
      if (TOTEst[i]==-3 & YCOCEst[i]==-200){ #these are 2003 people who said they did not know if they got aid in initial interview, then their school did not appear in the DLI variables
        TOTEst[i] = -200 #don't know about their aid when asked IF THEY GOT ANY
      }
      if (TOTEst[i]== -3 & YCOCEst[i]==-200){#one person who was not eligible for aid
        TOTEst[i]= 0
      }
    }

#test other improvements =========================================================

  #check accuracy of aid data for attended schools
    diffList = rep(NA, nrow(LONG_DATA))
    for (i in 1:nrow(LONG_DATA)){
      if (YSCHEst[i]>= 0 & YCOCEst[i]>= 0){
        diffList[i] = YSCHEst[i]-YCOCEst[i]
      }
    }
    mean(na.exclude(diffList))


  #check whether missing 2003 schools appear in the PREV roster