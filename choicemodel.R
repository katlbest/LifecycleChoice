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
  choice.dat = read.csv("D:choiceinput2.csv")
  #delete schools with selectivity 7--people make these decisions differently
  choice.dat = choice.dat[choice.dat$selectivity != 7,]
  #dataset with NAs coded correctly
    na.dat= choice.dat
    na.dat[na.dat==-3]=NA

#fill missing variables===========================================================
  #selectivity
    haveSelect = na.dat[!is.na(na.dat$selectivity2),]
    noSelect =  na.dat[is.na(na.dat$selectivity2),]
    selectPred.mod = lm(selectivity2 ~ avgsal+carnegie+admitperc + sat25+sat75, data = haveSelect)
    preAll = noSelect[!is.na(noSelect$avgsal)&!is.na(noSelect$carnegie)&!is.na(noSelect$admitperc)&!is.na(noSelect$sat25)& !is.na(noSelect$sat75),]
    select2Pred = round(predict(selectPred.mod,preAll))
    na.dat[is.na(na.dat$selectivity2)&!is.na(na.dat$avgsal)&!is.na(na.dat$carnegie)&!is.na(na.dat$admitperc)&!is.na(na.dat$sat25)& !is.na(na.dat$sat75),]$selectivity2 = select2Pred
    noSelect =  na.dat[is.na(na.dat$selectivity2),]
    preAll = noSelect[!is.na(noSelect$avgsal)&!is.na(noSelect$carnegie),]
    selectPred.mod = lm(selectivity2 ~ avgsal+carnegie, data = haveSelect)
    select2Pred = round(predict(selectPred.mod,preAll))
    na.dat[is.na(na.dat$selectivity2)&!is.na(na.dat$avgsal)&!is.na(na.dat$carnegie),]$selectivity2 = select2Pred
    #delete 4 with remaining no selectivity
      na.dat = na.dat[!is.na(na.dat$selectivity2),]
    #fill selectivity difference
      na.dat$selectdiff = na.dat$selectivity2-na.dat$Admitted
  #financial aid variables
    na.dat[is.na(na.dat$AIDALLSCHOOL),]$AIDALLSCHOOL = 0
    na.dat[is.na(na.dat$FINAIDEST),]$FINAIDEST = 0
    na.dat[is.na(na.dat$instate),]$instate = 0
    for (i in 1:nrow(na.dat)){
        if(na.dat$instate[i]==1){
          na.dat$realtui[i] = na.dat$tuiinlist[i]+ na.dat$feein[i]- na.dat$FINAIDEST[i]-na.dat$AIDALLSCHOOL[i]
        }else if (na.dat$instate[i] == 0){ #treat -3 missing state as in-state
          na.dat$realtui[i] = na.dat$tuioutlist[i]+ na.dat$feeout[i]- na.dat$FINAIDEST[i]-na.dat$AIDALLSCHOOL[i]
        }
    }
  #distance
    #lat/long sometimes filled in by state or google maps location of campuses
    na.dat$distance = NA
    for (i in 1:nrow(na.dat)){
      if (na.dat$latitude[i] != -3 & !is.na(na.dat$latstudent[i]))
        na.dat$distance[i] = sqrt((na.dat$latitude[i] -na.dat$latstudent[i])^2+ (na.dat$longitude[i] - na.dat$longstudent[i])^2)
    }


#preliminary visualization===========================================================
  #correlation and plot
    write.csv(cor(na.exclude(na.dat)), "C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/choicemodel/inputcorr.csv")
  #correlation of the school actually attended only with income
    attend.dat = na.dat[na.dat$AttendedIndicator == 1,]
    drop = c("AttendedIndicator")
    attend.dat= attend.dat[,!(colnames(attend.dat) %in% drop)]
    write.csv(cor(na.exclude(attend.dat)), "C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/choicemodel/inputcorrincome.csv")

#setup data==========================================================================
  #create lifetime income estimate
    tau =27.8818
    m = -3.8149
    b = 36241
    n =  -0.2445
    a = 2234.3
    source("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Data manipulation/fun_getNS.R")
    na.dat$lifeEarnings = NAgoo
    for (i in 1:nrow(na.dat)){
      if(na.dat$Attend[i]==-10){#did not attned school so begin earning at 18
        na.dat$lifeEarnings[i]= getNS(tau, m, b, n, a, na.dat$b0[i], 0)
      } else{
        na.dat$lifeEarnings[i]= getNS(tau, m, b, n, a, na.dat$b0[i], 1)
      }
    }


    

  #select/create relevant variables

  #create choice set information
    #PUBID_1997 identifies decision-maker ID
    #AdmittedSchool identifies alternative ID
    #AttendedIndicator identifies choice made
    test = na.dat[,c("PUBID_1997","AdmittedSchool","b0","AttendedIndicator")]
    mlogit.na.dat = mlogit.data(test, shape = "long", choice = "AttendedIndicator", alt.var= "AdmittedSchool", chid.var = "PUBID_1997")

#model of attenders only===============================================================
  
