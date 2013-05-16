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

#CREATE CHOICE FILE WITH EACH SCHOOL ON A LINE
  LONG_DATA = read.csv("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Choice model inputs/inputs.csv")
  


