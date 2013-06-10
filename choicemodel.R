#NOTES:======================================================================
  #this file analyzes the choice data and runs the appropriate models

#libraries ====================================================================
  #library(plyr)
  library(ggplot2)
  library(mlogit)
  #library(MASS)
  #library(Hmisc)
  #library(reshape2)

#clear workspace ===================================================================
  rm(list = ls())
  
#data i/o ==========================================================================
  choice.dat = read.csv("D:choiceinput.csv")
  #dataset with NAs coded correctly
    na.dat= choice.dat
    na.dat[na.dat==-3]=NA

#preliminary visualization===========================================================
  #correlation and plot
    write.csv(cor(na.exclude(na.dat)), "C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/choicemodel/inputcorr.csv")
  #correlation of attended only with income
    attend.dat = na.dat[na.dat$AttendedIndicator == 1,]
    drop = c("AttendedIndicator")
    attend.dat= attend.dat[,!(colnames(attend.dat) %in% drop)]
    write.csv(cor(na.exclude(attend.dat)), "C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/choicemodel/inputcorrincome.csv")

#setup data==========================================================================
  #create choice set information
    #PUBID_1997 is our individual factor variable identifying which alternative goes with each individual
    #AdmittedSchool is our choice set variable, identifying the name of alternatives
    test = na.dat[,c("PUBID_1997","AdmittedSchool","b0","AttendedIndicator")]
    mlogit.na.dat = mlogit.data(test, shape = "long", choice = "AttendedIndicator", alt.var= "AdmittedSchool", chid.var = "PUBID_1997")