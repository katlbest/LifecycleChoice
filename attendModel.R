#NOTES:======================================================================
#this file analyzes the attendance and runs the appropriate models

#libraries ====================================================================
  library(plyr)
  library(ggplot2)
  library(mlogit)
  #library(MASS)
  #library(Hmisc)
  #library(reshape2)
  library(descr)
  library(glm)
  library(mclogit)
  #library(safeBinaryRegression)
  library(car)
  library(survival)

#clear workspace ===================================================================
  rm(list = ls())

#data i/o ==========================================================================
  attend.dat = read.csv("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Choice Model Inputs/anon_attend.csv", stringsAsFactors=FALSE)
    #this is out.df from choiceModel.R with some variable choice modifications, anonimized
    #keep only relevant columns
      attend.dat = attend.dat[,c("pubid_anon","school_anon","Admit","Attend","b0","KEYSEX_1997","KEYRACE_ETHNICITY_1997","SAT_MATH","SAT_VERBAL","MAJOR2","DAD_ED","MOM_ED","HH_SIZE","HH_INCOME","URBAN_RURAL","SCHOOL_TYPE","AttendedIndicator","AIDALLSCHOOL","SATDiff","tuiin","tuiout","tuiinlist","tuioutlist","sat25","sat75","urbanrural","lifeEarnings")]
  merge.dat = read.csv("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Choice Model Inputs/anon_dat_mergedata.csv")
  #merge
    attend.dat$id = paste(attend.dat$pubid_anon, attend.dat$school_anon)
    merge.dat$id = paste(merge.dat$pubid_anon, merge.dat$school_anon)
    merge.dat=merge.dat[c("id","attendedIndicator","tuioutlist","realtui2","finaidest2","distance","instate","urbanruralmatch","loanp","fedgrantp","control","carnegie2","avgsal","division2","gradrate","expperstudent2","instperstudent2","facperstudent2","genderratio2","totstudents2","nonAttendAid","realtuiApply","selectdiffInt","selectInt","finaidwstatedisc")]
    attend.dat = merge(x = attend.dat, y = merge.dat, by = "id", all.x = TRUE)

#create student-level input dataset======================================================
  studentList = as.numeric(levels(as.factor(attend.dat$pubid_anon)))
  model.dat = data.frame(pubid_anon = studentList)
  #select attended where possible
    attenders.dat = attend.dat[attend.dat$attendedIndicator==1,]
    attenderList = as.numeric(levels(as.factor(attenders.dat$pubid_anon)))
    model.dat = merge(x = model.dat, y = attenders.dat, by = "pubid_anon", all.x = TRUE)
  #project attended school for non-attenders
    nonattenders.dat = attend.dat[!(attend.dat$pubid_anon %in% attenderList),]
    frequency.dat = data.frame(freq(ordered(nonattenders.dat$pubid_anon), plot=FALSE))
    frequency.dat$pubid_anon= rownames(frequency.dat)
    nonattenders.dat = merge(x = nonattenders.dat, y = frequency.dat, by = "pubid_anon", all.x = TRUE)
    nonattendone.dat = nonattenders.dat[nonattenders.dat$Frequency==1,]
    nonattendone.dat=nonattendone.dat[,c(1:52)]
    #replace the corresponding columns in model.dat
    for (i in 1:nrow(nonattendone.dat)){
      curID = nonattendone.dat$pubid_anon[i]
      replaceVect = unname(unlist(nonattendone.dat[i,]))
      model.dat[model.dat$pubid_anon==curID,]=replaceVect 
    }
   
    TBD: multi.dat = attenders.dat[attenders.dat$Frequency>1,]


#set factor levels
#set categorical variables since this information may have gotten lost
model.dat$instate = as.factor(model.dat$instate)
model.dat$urbanruralmatch = as.factor(model.dat$urbanruralmatch)
model.dat$control = as.factor(model.dat$control)
model.dat$carnegie2 = as.factor(model.dat$carnegie2)
model.dat$attend = as.factor(model.dat$attend)
#outlier testing and data cleaning=========================================================================

#remove outliers--check that these are the correct ones
#locate
relVarsPCA.dat$pubid_anon[which.max(relVarsPCA.dat[,c("instperstudent2"),])]
relVarsNoPCA.dat$pubid_anon[which.max(relVarsNoPCA.dat[,c("instperstudent2"),])]
#exists in both datasets
#remove
relVarsPCA.dat = relVarsPCA.dat[-(which.max(relVarsPCA.dat[,c("instperstudent2"),])),]
relVarsNoPCA.dat = relVarsNoPCA.dat[-(which.max(relVarsNoPCA.dat[,c("instperstudent2"),])),]
#tuioutlist 
#locate
relVarsPCA.dat$pubid_anon[which.max(relVarsPCA.dat[,c("tuioutlist"),])]
relVarsNoPCA.dat$pubid_anon[which.max(relVarsNoPCA.dat[,c("tuioutlist"),])]
#exists in both datasets, different from other outlier
#remove
relVarsPCA.dat = relVarsPCA.dat[-(which.max(relVarsPCA.dat[,c("tuioutlist"),])),]
relVarsNoPCA.dat = relVarsNoPCA.dat[-(which.max(relVarsNoPCA.dat[,c("tuioutlist"),])),]
#school quality 
#locate
relVarsPCA.dat$pubid_anon[which.max(relVarsPCA.dat[,c("schoolqual"),])]
#different from other outliers
#remove
relVarsPCA.dat = relVarsPCA.dat[-(which.max(relVarsPCA.dat[,c("schoolqual"),])),]
#finaidest
#locate
relVarsPCA.dat$pubid_anon[which.max(relVarsPCA.dat[,c("finaidest2"),])]
relVarsNoPCA.dat$pubid_anon[which.max(relVarsNoPCA.dat[,c("finaidest2"),])]
#in both datasets, different from other outliers
#remove
relVarsPCA.dat = relVarsPCA.dat[-(which.max(relVarsPCA.dat[,c("finaidest2"),])),]
relVarsNoPCA.dat = relVarsNoPCA.dat[-(which.max(relVarsNoPCA.dat[,c("finaidest2"),])),]
#d

#model============================================================================
