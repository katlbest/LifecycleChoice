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

    #estimate all school aid
      ALLSCHOOLEst = rep(-3, nrow(LONG_DATA))
      for (i in 1:nrow(LONG_DATA)){
        ALLSCHOOLEst[i] = max(aidListALLSCHOOL[[i]])
      }
      #set -4's to zero since they mean not eligible for all school aid
        #-2 indicates don't know
        ALLSCHOOLEst[ALLSCHOOLEst==-4]=0
      
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

  LONG_DATA$FINAIDEST = TOTEst
  LONG_DATA$AIDALLSCHOOL = ALLSCHOOLEst
  write.csv(LONG_DATA, "D:/longdata.csv")
  save.image(file= "studentandint.RData")

#create merged data files for college data, by year====================================================
yearVect = c(2011,2002,2003,2004,2005,2006)
for(i in 1:length(yearVect)){
  myYear= yearVect[i]  
  #SFA
    readStr = paste("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/ycoc/schooldata/",myYear,"/sfa", myYear, ".csv", sep = "")
    SFA.dat = read.csv(readStr)
    if (("ufloanp" %in% colnames(SFA.dat))| ("UFLOANP" %in% colnames(SFA.dat))){
      keepVect = c("unitid","ufloanp","fgrnt_p", "loan_p")
    }else{
      keepVect = c("unitid","fgrnt_p", "loan_p")
    }
    if (myYear >2010){
      keepVect = toupper(keepVect)
    }
    SFA.dat = SFA.dat[,keepVect]
  #GR
    readStr = paste("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/ycoc/schooldata/",myYear,"/gr", myYear, ".csv", sep = "")
    GR.dat = read.csv(readStr)
    if (("grtotlt" %in% colnames(GR.dat))| ("GRTOTLT" %in% colnames(GR.dat))){
      keepVect = c("unitid", "grtotlt")
    } else{
      keepVect = c("unitid", "grrace24")
    }
    selectVar = "grtype"
    if (myYear >2010){
      keepVect = toupper(keepVect)
      selectVar = toupper(selectVar)
    }
    GR.dat = GR.dat[GR.dat[,selectVar] == 3,]
    GR.dat = GR.dat[,keepVect]
  #F
    readStr = paste("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/ycoc/schooldata/",myYear,"/f1a", myYear, ".csv", sep = "")  
    F1A.dat = read.csv(readStr)
    keepVect1 = c("unitid","f1c011", "f1c191")
    nameVect1 = c("unitid", "instspend", "totalexp")
    keepVect2 = c("unitid", "f2e011", "f2e131")
    keepVect3 = c("unitid", "f3e01", "f3e07")
    idName = "unitid"
    if (myYear >2010){
      keepVect1 = toupper(keepVect1)
      nameVect1 = toupper(nameVect1)
      keepVect2 = toupper(keepVect2)
      keepVect3 = toupper(keepVect3)
      idName = toupper(idName)
    }
    F1A.dat = F1A.dat[,keepVect1]
    colnames(F1A.dat)= nameVect1
    readStr = paste("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/ycoc/schooldata/",myYear,"/f2", myYear, ".csv", sep = "")
    F2.dat = read.csv(readStr)
    F2.dat = F2.dat[,keepVect2]
    colnames(F2.dat)= nameVect1
    readStr = paste("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/ycoc/schooldata/",myYear,"/f3", myYear, ".csv", sep = "")
    F3.dat = read.csv(readStr) 
    F3.dat = F3.dat[,keepVect3]
    colnames(F3.dat)= nameVect1
    FAll.dat = rbind(F1A.dat, F2.dat, F3.dat)
    #check for duplicates, max should be 1
      if(max(table(FAll.dat[,idName]))>1){
        print("ERROR: Duplicate entries in F tables")
      }
  #ICAY
    readStr = paste("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/ycoc/schooldata/",myYear,"/icay", myYear, ".csv", sep = "")
    ICAY.dat = read.csv(readStr)
    keepVect= c("unitid", "tuition2", "fee2", "tuition3", "fee3", "chg2ay3", "chg3ay3")
    if (myYear >2010){
      keepVect = toupper(keepVect)
    }
    ICAY.dat= ICAY.dat[,keepVect]
  #SAL
    readStr = paste("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/ycoc/schooldata/",myYear,"/sal", myYear, ".csv", sep = "")
    SAL.dat = read.csv(readStr)
    keepVect= c("unitid", "contract", "arank", "avesalt", "empcntt")
    if (myYear >2010){
      keepVect = toupper(keepVect)
    }
    SAL.dat = SAL.dat[,keepVect]
    if ("AVESALT" %in% colnames(SAL.dat)){
      SAL.dat = rename(SAL.dat, c("AVESALT"="avesalt", "EMPCNTT"="empcntt"))
    } 
    outSAL.dat = ddply(SAL.dat,idName,function(X) data.frame(avesalt=weighted.mean(X$avesalt,X$empcntt), empcntt=sum(X$empcntt)))
  #aggregate all
    AGG.dat =  merge(x = SFA.dat, y = GR.dat, by = idName, all.x = TRUE, all.y= TRUE)
    AGG.dat = merge(x=AGG.dat, y = FAll.dat,  by = idName, all.x = TRUE, all.y= TRUE)
    AGG.dat = merge(x=AGG.dat, y = ICAY.dat,  by = idName, all.x = TRUE, all.y= TRUE)
    AGG.dat = merge(x=AGG.dat, y = outSAL.dat,  by = idName, all.x = TRUE, all.y= TRUE)
  #write
    writeStr = paste("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/ycoc/schooldata/",myYear,"/agg", myYear, ".csv", sep = "")
    write.csv(AGG.dat,writeStr)
}

#read in school data and calculate school-specific variables================================================
  SCHOOLDATA = read.table(file = "C:/Users/Katharina/Documents/UMICH/Lifecycle choice/Data/ycoc/collegedataoutput.txt", header = TRUE, sep = "\t")
  LONG_DATA = merge(x=LONG_DATA, y = SCHOOLDATA,  by.x = "AdmittedSchool", by.y= "collegeID", all.x = TRUE)
  LONG_DATA$expperstudent = LONG_DATA$totalexp/(LONG_DATA$fulltime_UG+LONG_DATA$fulltime_GRAD)
  LONG_DATA$instperstudent = LONG_DATA$instspend/(LONG_DATA$fulltime_UG+LONG_DATA$fulltime_GRAD)
  LONG_DATA$facperstudent = LONG_DATA$numfaculty/(LONG_DATA$fulltime_UG+LONG_DATA$fulltime_GRAD)
  LONG_DATA$genderratio= LONG_DATA$male_enrolled/LONG_DATA$female_enrolled
  urbanInputs = c(11,12,13,21,22,23,31,32,33,41,42,43,-3,1,2,3,4,5,6,7,9,-3)
  urbanOutputs = c(1,1,1,1,2,2,2,2,2,3,3,3,-3,1,1,1,1,2,2,3,-3,-3)
  LONG_DATA$urbanrural = -3
  for (i in 1:nrow(LONG_DATA)){
    LONG_DATA$urbanrural[i]= urbanOutputs[match(LONG_DATA$locale[i],urbanInputs)]
  }

#calculate interaction terms
  #instate
    LONG_DATA$instate = -3
    for (i in 1:nrow(LONG_DATA)){
      if(LONG_DATA$RES_STATE[i]>0 & LONG_DATA$state[i]>0){
        if(LONG_DATA$RES_STATE[i] == LONG_DATA$state[i]){
          LONG_DATA$instate[i] = 1
        } else{
          LONG_DATA$instate[i] = 0
        }
      }
    }
  #realtui
    LONG_DATA$realtui = -3
    for (i in 1:nrow(LONG_DATA)){
      if (LONG_DATA$AIDALLSCHOOL[i]>0){ #set all school aid to zero if it is missing, since this has no relative effect anyway
        
        allAid = LONG_DATA$AIDALLSCHOOL[i]
      }else{
        allAid = 0
      }
      if(LONG_DATA$FINAIDEST[i]>0){ #we have aid data
        if(LONG_DATA$instate[i]==1){
          LONG_DATA$realtui[i] = LONG_DATA$tuiinlist[i]+ LONG_DATA$feein[i]- LONG_DATA$FINAIDEST[i]-allAid
        }else if (LONG_DATA$instate[i]==0){
          LONG_DATA$realtui[i] = LONG_DATA$tuioutlist[i]+ LONG_DATA$feeout[i]- LONG_DATA$FINAIDEST[i]- allAid
        }
      } #else we have to leave it as -3
    }
  #sporting division
  #demographic (ethnic) match
  #ability match
  #religious match
  #urban/rural match
  #distance from college to home
    


#retrieve only relevant data points
