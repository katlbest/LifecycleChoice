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

  #simplified sports variable
    na.dat$division2 = na.dat$division
    na.dat[is.na(na.dat$division2),]$division2 = 0
    na.dat[na.dat$division2>1,]$division2 = 0
  
  #missing enrollments and related numbers
    na.dat[is.na(na.dat$expperstudent),]$instperstudent =c(4520.61506,1150.341232,1504.649105,NA,31187.00164,3394.802632,NA,NA,3891.508412,NA,4945.538738,5024.843797,5871.471399,5871.471399,5871.471399,5871.471399,5871.471399,5867.027995,NA,NA,NA,NA,1367.668782,1367.668782,1367.668782,2250.383634,2250.383634,2275.225379,NA,NA) 
    na.dat[is.na(na.dat$expperstudent),]$facperstudent = c(0.214457831,0.003041054,0.06361829,NA,0.36078145,0.157894737,NA,NA,0.133243607,NA,0.124743614,0.122571001,0.134990774,0.134990774,0.134990774,0.134990774,0.134990774,0.2421875,NA,NA,NA,NA,0.050761421,0.050761421,0.050761421,0.094404957,0.094404957,0.060606061,NA,NA)
    na.dat[is.na(na.dat$expperstudent),]$totstudents = c(415,1973,251.5,NA,1369.25,209,2616.25,2616.25,743,NA,5363,334.5,5148.5,5148.5,5148.5,5148.5,5148.5,384,607,306.25,800.5,499,394,394,394,2743.5,2743.5,132,NA,2392)
    na.dat[is.na(na.dat$expperstudent),]$expperstudent = c(10701.30783,4957.191586,7838.678926,NA,103966.2806,10630.63278,NA,NA,12222.84791,NA,15083.20231,9542.077728,15215.34976,15215.34976,15215.34976,15215.34976,15215.34976,22338.86719,NA,NA,NA,NA,7082.442893,7082.442893,7082.442893,6845.025241,6845.025241,7493.844697,NA,NA)
    na.dat[na.dat$AdmittedSchool == 186399,]$gradrate = .51
    na.dat[na.dat$AdmittedSchool == 228644,]$gradrate = 1
  
  #simplified carnegie
    na.dat[is.na(na.dat$carnegie),]$carnegie = c(32, 32, 32, 32, 54, 15)
    na.dat$carnegie2 = NA
    for (i in 1:nrow(na.dat)){
      if (na.dat$carnegie[i] %in% c(15, 16)){
        na.dat$carnegie2[i] =1
      } else if(na.dat$carnegie[i] %in% c(22, 21)){
        na.dat$carnegie2[i] =2
      } else if(na.dat$carnegie[i] %in% c(31, 32, 33)){
        na.dat$carnegie2[i] =3
      } else if(na.dat$carnegie[i] %in% c(40, 51, 52:59)){ #associates schools with some bach, specialty, seminary
        na.dat$carnegie2[i] =4
      }
    }

keyVars = c("PUBID_1997", "AdmittedSchool", "tuioutlist", "realtui", "FINAIDEST", "distance", "instate", "urbanruralmatch", "selectdiff", "loanp", "fedgrantp", "genderratio", "totstudents", "control", "carnegie2", "selectivity2", "expperstudent", "instperstudent", "facperstudent", "avgsal", "division2", "gradrate", "AttendedIndicator")
out.df = na.dat[,keyVars]
write.csv(out.df, "D:mainvars.csv")

#preliminary visualization===========================================================
  #correlation and plot
    write.csv(cor(na.exclude(na.dat)), "C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/choicemodel/inputcorr.csv")
  #correlation of the school actually attended only with income
    attend.dat = na.dat[na.dat$AttendedIndicator == 1,]
    drop = c("AttendedIndicator")
    attend.dat= attend.dat[,!(colnames(attend.dat) %in% drop)]
    write.csv(cor(na.exclude(attend.dat)), "C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/choicemodel/inputcorrincome.csv")

#handle interaction variables==========================================================================
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

#ANONYMIZED MODEL SETUP===============================================================
  #read anon data
  anon.dat = read.csv("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Choice Model Inputs/mainvars-anon.csv")
  #setup model inputs
    #create choice set information
      #pubid_anon identifies decision-maker ID
      #school_anon identifies alternative ID
      #AttendedIndicator identifies choice made
      mlogit.anon.dat = mlogit.data(anon.dat, shape = "long", choice = "attendedIndicator", alt.var= "school_anon", chid.var = "pubid_anon")

