#NOTES:======================================================================
  #this file compiles the college choice model data that is derived from BLS and other sources best handled in R (non-python)
  
#libraries ====================================================================
  library(plyr)
  #library(ggplot2)
  #library(MASS)
  #library(Hmisc)
  library(zipcode)
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
      if(LONG_DATA$FINAIDEST[i]>=0){ #we have aid data
        if(LONG_DATA$instate[i]==1){
          LONG_DATA$realtui[i] = LONG_DATA$tuiinlist[i]+ LONG_DATA$feein[i]- LONG_DATA$FINAIDEST[i]-allAid
        }else if (LONG_DATA$instate[i] %in% c(0, -3)){ #treat -3 missing state as in-state
          LONG_DATA$realtui[i] = LONG_DATA$tuioutlist[i]+ LONG_DATA$feeout[i]- LONG_DATA$FINAIDEST[i]- allAid
        }
      } #else we have to leave it as -3
    }
  #sporting division
    confInputs = c(103,102,104,112,113,114,362,117,127,204,306,198,133,111,200,126,155,191,359,318,119,356,213,323,141,144,148,153,161,163,195,192,308,159,130,366,354,201,205,121,320,321,182,175,115,167,170,172,176,180,183,185,197,202,134,171,128,193,351,137,181,352,302,311,337,309,315,353)
    confOutputs =c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4)
    LONG_DATA$division = -3
    for (i in 1:nrow(LONG_DATA)){
      matchInd = match(LONG_DATA$conference_no[i], confInputs)
      if (!is.na(matchInd)){
        LONG_DATA$division[i]= confOutputs[matchInd]  
      }
    }
  #ability match
    LONG_DATA$SATDiff = -3
    satInput = c(1,2,3,4,5,6,0, -3)
    satOutput = c(250,350,450,550,670,750,-3, -3)
    LONG_DATA$SAT_MATH2 = satOutput[match(LONG_DATA$SAT_MATH, satInput)]
    LONG_DATA$SAT_VERBAL2 = satOutput[match(LONG_DATA$SAT_VERBAL, satInput)]
    for (i in 1:nrow(LONG_DATA)){
      if(!is.na(LONG_DATA$SAT_MATH2[i]) & !is.na(LONG_DATA$SAT_VERBAL2[i]) & !is.na(LONG_DATA$sat75[i])){
      LONG_DATA$SATDiff[i] = LONG_DATA$sat75[i]-sum(LONG_DATA$SAT_MATH[i], LONG_DATA$SAT_VERBAL[i])
      }
    }
  #urban/rural match
    LONG_DATA$urbanruralmatch = -3
    for (i in 1:nrow(LONG_DATA)){
      if (!is.na(LONG_DATA$urbanrural[i]) & !is.na(LONG_DATA$URBAN_RURAL[i])){
        if(LONG_DATA$urbanrural[i] ==3 & LONG_DATA$URBAN_RURAL[i]==0){
          LONG_DATA$urbanruralmatch[i]=1
        } else if (LONG_DATA$urbanrural[i] %in% c(1,2) & LONG_DATA$URBAN_RURAL[i]==1){
          LONG_DATA$urbanruralmatch[i]=1
        }
        else if (LONG_DATA$urbanrural[i]>= 0 & LONG_DATA$URBAN_RURAL[i]>= 0){
          LONG_DATA$urbanruralmatch[i]=0
        }
      }
    }
  #distance from college to home
    #get students' lat and longitude
      LONG_DATA$latstudent = -3
      LONG_DATA$longstudent = -3
      #run in console
      fakeCBSA = c(2,3,4,5,6,7,8,9,10,12,13,15,16,17,20,21,22,23,25,26,27,28,30,31,32,33,34,35,36,37,38,39,40,42,43,44,45,46,48,49,50,51,52,53,54,55,56,57,58,59,60,61,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,256,257,258,259,260,261,262,263,264,265,266,267,268,269,270,271,272,273,274,275,276,277,278,279,280,281,282,283,284,285,286,287,288,289,290,291,292,293,294,295,296,297,298,299,300,301,302,303,304,305,306,307,308,309,310,311,312,313,314,315,316,317,318,319,320,321,322,323,324,325,326,327,328,329,330,331,332,333,334,335,336,337,338,339,340,341,342,343,344,345,346,347,348,349,350,351,352,353,354,355,356,357,358,359,360,361,362,363,364,365,366,367,368,369,370,371,372,373,374,375,376,377,378,379,380,381,382,383,384,385,386,387,388,389,390,391,392,393,394,395,396,397,398,399,400,401,402,403,404,405,407,408,409,410,411,412,413,414,415,416,417,418,419,420,421,422,423,424,425,426,427,428,429,430,431,432,433,434,435,436,437,438,439,440,441,442,443,444,445,446,447,448,449,450,451,452,453,454,455,456,457,459,460,461,462,463,464,465,466,467,468,469,470,471,472,473,474,475,476,477,478,479,480,481,482,483,484,485,486,487,488,489,490,491,492,493,494,495,496,497,498,499,500,501,502,503,504,505,506,507,508,509,510,511,512,513,514,515,516,517,518,519,520,521,522,523,524,525,526,527,528,529,530,531,532,533,534,535,536,537,538,539,540,541,542,543,544,545,546,547,548,549,550,551,552,553,554,555,556,557,558,559,560,561,562,563,564,565,566,567,568,569,570,571,572,573,574,575,576,577,578,579,580,581,582, 24,19,41,47,1,11,96,29,18,62,406, 14)
      latout = c(32.93,29.19,42.73,41.33,28.58,33.76,35.20,26.96,39.27,26.57,43.11,41.22,41.39,41.56,40.74,61.32,38.97,43.06,38.26,32.48,40.57,35.77,40.27,36.98,26.08,40.05,41.52,35.13,28.03,30.18,35.06,39.68,34.13,37.67,38.69,33.38,29.45,35.34,34.18,41.12,36.30,36.10,34.34,35.03,40.75,41.18,42.47,39.74,30.12,37.24,44.24,32.96,37.03,32.27,40.22,40.13,32.57,43.09,40.49,40.66,41.72,40.49,41.11,29.81,35.99,36.18,41.42,42.06,38.19,37.53,32.10,41.53,34.55,40.51,39.77,38.23,29.77,42.26,37.96,30.22,35.32,38.42,36.23,35.42,38.46,45.03,39.34,35.67,40.34,35.98,29.98,42.98,42.28,36.12,42.31,42.73,34.78,44.90,37.33,43.02,26.70,42.25,30.31,39.00,30.52,32.66,31.56,31.13,38.25,37.78,43.81,38.89,35.18,35.81,38.39,39.03,41.89,33.34,40.16,33.64,39.81,30.19,32.12,34.76,43.71,41.74,41.73,42.31,32.82,33.96,30.68,44.93,45.51,40.47,32.97,32.47,32.40,28.00,32.66,44.02,31.74,28.80,38.68,40.32,39.06,30.43,40.07,32.42,27.81,45.58,32.29,37.24,35.61,48.14,31.50,30.21,34.77,38.27,32.33,40.36,41.12,47.63,46.97,30.89,44.74,44.63,44.28,43.30,36.62,39.13,41.79,30.56,42.89,39.46,40.76,33.86,41.59,27.25,34.14,40.41,36.04,41.06,39.91,34.20,31.39,31.23,39.91,39.96,40.74,38.63,40.95,33.49,43.80,47.08,42.70,43.12,45.43,37.98,33.50,29.05,41.91,42.74,42.57,38.98,37.30,43.28,39.53,46.47,41.01,31.21,41.42,31.28,40.80,41.34,41.62,35.40,40.12,36.64,39.19,46.41,42.23,35.11,37.94,36.44,34.73,34.61,43.04,47.08,39.18,44.78,44.55,19.69,37.35,43.39,43.68,35.54,43.68,43.64,43.70,37.81,35.27,39.47,38.66,41.24,44.08,41.52,43.51,36.74,37.29,34.49,44.39,30.48,44.44,45.63,47.66,35.15,35.25,41.01,36.73,36.23,44.07,27.38,34.06,39.31,28.24,34.89,36.63,35.29,35.65,34.66,36.05,36.23,36.13,36.79,37.17,43.19,42.33,40.77,36.70,41.26,38.40,24.89,35.82,32.95,48.28,36.30,35.61,38.25,30.74,37.11,35.89,36.12,36.36,35.45,36.09,38.39,39.09,35.19,34.63,35.28,44.29,30.67,30.88,45.12,33.38,47.37,28.75,35.20,27.75,23.15,31.85,27.32,38.15,33.51,36.64,40.45,43.00,41.49,45.92,39.09,48.81,48.44,40.61,44.01,46.85,37.64,31.80,46.89,37.31,36.46,43.62,33.58,37.24,37.97,40.76,41.46,41.83,43.35,40.06,44.69,42.26,44.75,44.53,31.78,40.00,42.92,42.88,44.15,41.72,41.81,42.32,42.07,41.44,39.51,40.93,38.85,38.97,40.16,39.32,35.53,39.84,39.90,46.67,42.62,46.91,32.43,39.21,40.40,44.15,38.65,37.10,47.66,39.78,34.37,30.12,47.86,46.29,58.34,42.49,42.30,38.28,46.85,32.32,48.42,41.37,41.43,41.16,41.13,35.42,35.64,38.98,44.93,35.52,43.41,44.46,42.59,32.45,33.09,33.74,30.48,34.38,37.51,36.96,38.06,30.62,31.27,30.29,31.52,31.26,30.62,33.77,32.42,35.77,36.23,40.85,38.49,35.89,35.59,35.69,33.48,37.67,36.14,36.21,39.09,38.10,31.58,33.14,34.92,32.42,33.45,35.28,41.24,41.92,39.60,45.71,46.97,33.90,44.38,47.43,42.53,44.75,39.54,26.27,44.51,45.67,45.62,44.05,43.26,42.99,41.07,40.45,38.13,30.83,31.83,32.62,34.08,33.21,32.47,34.03,42.43,36.00,28.87,32.51,39.86,29.93,33.50,32.82,33.27,38.39,35.52,38.81,34.94,38.75,29.29,26.11,35.76,28.71,38.51,43.60,34.32,35.47,36.73,41.72,33.82,41.80,37.76,41.89,37.77,37.84,44.35,39.62,35.89,39.27,39.91,41.32,44.45,35.35,40.79,44.37,43.06,35.36,43.06,31.62,46.33,38.75,64.80,41.02,22.03,38.92,37.26,37.61,33.59,30.59,34.09,37.36,30.03,37.21,31.58,36.41,35.03,34.26,30.30,32.90,35.03,47.11,41.89,37.51,42.19,33.22,27.26,32.23,40.06,42.64,44.98,38.84,29.68,47.72,36.39,35.48,40.35,35.20,34.58,32.82,40.83,36.28,30.73,28.44,44.04,38.60,42.53,46.16,30.59,48.30,39.70,41.56,44.46,43.78,34.21,34.32,42.07,45.31,43.86,31.73,44.63,44.00,40.95,26.45,39.93,47.25,33.77,37.8,40.71,38.88,25.79,32.78,42.36,42.37,44.38, 41.79)
      longout = c(-116.96,-82.11,-73.92,-75.84,-81.41,-84.39,-80.86,-82.11,-76.58,-81.87,-76.01,-73.38,-72.94,-83.60,-75.42,-149.73,-95.38,-77.56,-75.73,-84.92,-105.23,-81.41,-74.71,-76.36,-81.59,-82.89,-74.12,-106.57,-82.53,-91.91,-85.29,-104.90,-116.74,-97.28,-121.24,-112.01,-98.50,-118.82,-119.10,-111.94,-85.48,-115.06,-85.21,-78.91,-89.55,-80.63,-96.39,-119.71,-94.19,-121.80,-103.00,-115.42,-122.01,-90.15,-88.26,-89.41,-92.26,-88.07,-90.20,-111.95,-86.13,-88.82,-87.91,-95.42,-83.94,-119.74,-81.71,-91.64,-88.85,-77.45,-81.19,-86.64,-87.15,-86.87,-86.18,-122.05,-91.42,-85.05,-121.26,-81.67,-120.62,-122.39,-119.08,-78.02,-122.88,-93.26,-75.11,-75.65,-76.93,-79.03,-90.06,-86.08,-83.76,-86.68,-72.59,-84.50,-82.33,-91.30,-79.27,-83.70,-81.29,-84.43,-97.72,-94.54,-91.11,-114.36,-97.14,-93.28,-76.58,-89.36,-72.26,-104.75,-77.60,-78.53,-108.16,-95.78,-111.88,-86.21,-111.79,-95.53,-84.22,-96.46,-111.22,-86.65,-70.46,-72.64,-71.38,-71.86,-83.64,-83.31,-88.15,-122.85,-122.64,-85.63,-80.05,-83.65,-95.33,-81.72,-100.88,-75.92,-106.35,-82.06,-90.32,-92.56,-92.42,-84.31,-105.28,-80.80,-97.43,-94.48,-106.80,-107.91,-111.62,-122.55,-85.88,-85.65,-92.33,-75.35,-97.86,-79.85,-81.41,-122.63,-122.82,-81.71,-88.06,-73.04,-72.55,-71.63,-82.40,-84.47,-73.21,-87.22,-78.80,-74.70,-81.86,-78.93,-85.88,-80.34,-79.83,-75.91,-79.87,-75.32,-77.18,-78.09,-100.50,-85.51,-76.69,-77.68,-76.22,-75.31,-78.46,-88.93,-91.25,-92.38,-89.06,-89.57,-89.66,-87.55,-86.84,-81.16,-84.07,-87.96,-92.48,-119.84,-89.70,-86.30,-77.89,-87.66,-85.19,-97.61,-85.48,-81.62,-86.26,-85.07,-85.08,-78.84,-85.71,-95.90,-85.83,-116.79,-85.78,-89.89,-78.60,-84.16,-77.42,-103.39,-85.50,-88.54,-86.76,-85.67,-122.86,-155.43,-93.24,-83.97,-83.98,-86.69,-84.42,-85.32,-84.87,-92.17,-79.49,-92.49,-93.77,-104.63,-92.39,-105.72,-96.90,-119.69,-79.95,-119.98,-96.80,-89.23,-103.76,-111.15,-117.41,-84.56,-94.42,-92.43,-93.23,-94.20,-88.69,-81.38,-80.98,-80.36,-80.67,-80.83,-87.52,-81.57,-80.55,-81.02,-79.44,-81.74,-80.27,-79.37,-80.57,-75.30,-123.48,-84.08,-79.97,-95.99,-97.61,-81.06,-83.56,-80.74,-101.20,-82.33,-87.13,-85.70,-86.66,-86.35,-84.38,-96.02,-97.79,-97.49,-96.90,-96.26,-75.56,-96.90,-98.44,-99.68,-88.36,-96.43,-83.22,-68.74,-94.13,-119.36,-97.02,-101.77,-98.04,-163.00,-102.50,-97.83,-104.53,-105.45,-108.24,-104.56,-106.54,-91.63,-112.71,-108.53,-122.35,-122.20,-123.95,-123.13,-116.75,-120.95,-81.56,-102.83,-120.67,-121.49,-116.36,-92.68,-113.43,-120.25,-122.10,-72.09,-71.97,-72.17,-76.22,-74.36,-73.64,-73.65,-75.14,-95.66,-78.98,-72.27,-71.64,-70.26,-89.38,-89.99,-89.08,-89.27,-90.48,-89.31,-88.59,-86.50,-85.64,-87.72,-96.58,-88.65,-94.82,-85.01,-94.21,-85.86,-96.99,-99.75,-121.46,-109.72,-94.10,-93.28,-94.31,-116.73,-89.64,-86.29,-93.25,-96.93,-96.83,-134.38,-90.87,-77.38,-81.62,-100.82,-86.36,-103.37,-83.22,-82.57,-82.65,-83.15,-79.22,-105.96,-97.66,-89.77,-82.56,-88.67,-89.39,-88.50,-88.70,-87.64,-85.85,-88.60,-85.40,-84.26,-85.79,-84.42,-92.00,-92.55,-92.43,-90.43,-89.22,-90.41,-90.85,-90.81,-80.86,-81.14,-82.32,-78.86,-77.82,-77.43,-77.89,-81.93,-85.93,-83.35,-82.87,-74.80,-84.96,-94.64,-95.53,-95.70,-94.84,-80.80,-114.11,-110.53,-86.50,-106.08,-108.57,-114.02,-80.34,-105.46,-111.38,-114.59,-106.94,-106.62,-98.18,-123.40,-119.04,-121.64,-121.22,-123.39,-84.15,-98.43,-86.10,-79.13,-95.55,-85.34,-85.30,-91.90,-92.63,-93.78,-93.16,-122.83,-101.91,-82.47,-82.83,-88.98,-91.02,-90.10,-90.34,-90.97,-105.39,-105.19)
      longout = c(longout,-83.01,-81.97,-87.33,-96.16,-97.58,-90.66,-100.47,-77.94,-73.05,-83.80,-84.59,-90.39,-70.28,-98.49,-76.50,-113.22,-83.51,-88.99,-90.49,-93.34,-110.71,-83.11,-120.83,-85.40,-89.23,-100.32,-81.86,-80.73,-92.56,-95.14,-80.30,-88.71,-84.08,-119.31,-85.80,-147.49,-76.45,-159.51,-99.23,-81.41,-92.60,-101.81,-87.73,-85.22,-85.28,-92.16,-96.80,-81.99,-80.68,-90.79,-88.73,-92.88,-103.37,-81.77,-123.84,-74.20,-84.75,-76.01,-95.01,-82.41,-95.84,-91.30,-76.08,-92.00,-85.45,-82.38,-120.21,-99.31,-81.24,-80.68,-82.79,-79.10,-105.83,-96.81,-76.33,-98.21,-97.78,-111.59,-92.15,-121.67,-118.40,-86.58,-114.27,-90.23,-93.81,-89.98,-88.49,-97.33,-89.51,-80.10,-121.13,-117.36,-109.95,-71.53,-91.74,-77.03,-97.67, -75.12,-122.44,-118.2,-122.27,-74.01,-77.11,-80.23,-96.8,-71.06,-83.35,-89.82, -88.15)
      #average lat and long across zip codes within this CBSA
      for (i in 1:nrow(LONG_DATA)){
        LONG_DATA$latstudent[i] = latout[match(LONG_DATA$GEO[i], fakeCBSA)]
        LONG_DATA$longstudent[i] = longout[match(LONG_DATA$GEO[i], fakeCBSA)]
      }
      LONG_DATA$distance = -3
      for (i in 1:nrow(LONG_DATA)){
        if (LONG_DATA$latitude[i] != -3 & !is.na(LONG_DATA$latstudent[i]))
        LONG_DATA$distance[i] = sqrt((LONG_DATA$latitude[i] -LONG_DATA$latstudent[i])^2+ (LONG_DATA$longitude[i] - LONG_DATA$longstudent[i])^2)
      }

#retrieve only relevant data points
keepVect = c("PUBID_1997","b0", "KEYSEX_1997", "KEYRACE_ETHNICITY_1997","GRADES","SAT_MATH", "SAT_VERBAL", "MAJOR2", "DAD_ED","MOM_ED","HH_SIZE", "HH_INCOME", "URBAN_RURAL", "SCHOOL_TYPE", "AttendedIndicator", "FINAIDEST", "AIDALLSCHOOL", "instate","SATDiff","urbanruralmatch", "realtui", "tuiin", "tuiout", "tuiinlist", "tuioutlist", "sat25", "sat75", "avgsal", "control", "fulltime_UG", "fulltime_GRAD", "carnegie", "selectivity", "fedgrantp", "gradrate", "loanp", "admitperc", "expperstudent", "instperstudent", "facperstudent", "genderratio", "division", "urbanrural", "distance")
LONG_OUT = LONG_DATA[,keepVect]
write.csv(LONG_OUT, "D:/longout.csv")