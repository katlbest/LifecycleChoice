#NOTES:======================================================================
  #this file analyzes the choice data and runs the appropriate models

#libraries ====================================================================
  #library(plyr)
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
      na.dat$selectdiff = na.dat$selectivity2-na.dat$Admit
      print(na.dat[na.dat$selectdiff<0,]$selectivity2)
      print(na.dat[na.dat$selectdiff<0,]$Admit)
      print(na.dat[na.dat$selectdiff<0,]$PUBID_1997)
      print(na.dat[na.dat$selectdiff>0,]$AdmittedSchool)
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

keyVars = c("PUBID_1997", "AdmittedSchool", "tuioutlist", "realtui", "FINAIDEST", "distance", "instate", "urbanruralmatch", "selectdiff", "loanp", "fedgrantp", "genderratio", "totstudents", "control", "carnegie2", "selectivity2", "expperstudent", "instperstudent", "facperstudent", "avgsal", "division2", "gradrate", "AttendedIndicator", "Attend", "lifeEarnings")
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
    na.dat$lifeEarnings = NA
    for (i in 1:nrow(na.dat)){
      if(na.dat$Attend[i]==-10){#did not attned school so begin earning at 18
        na.dat$lifeEarnings[i]= getNS(tau, m, b, n, a, na.dat$b0[i], 0)
      } else{
        na.dat$lifeEarnings[i]= getNS(tau, m, b, n, a, na.dat$b0[i], 1)
      }
    }

#ANONYMIZED MODEL SETUP===============================================================
  #read anon data
    anon.dat = read.csv("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Choice Model Inputs/anon_data2.csv")
  #drop nonsensical observation
    anon.dat = anon.dat[anon.dat$school_anon != 24873,]
  #get attenders
    attenders.dat = anon.dat[anon.dat$attend != -10,]
  #delete those with only 1 appearance
    frequency.dat = data.frame(freq(ordered(attenders.dat$pubid_anon), plot=FALSE))
    frequency.dat$pubid_anon= rownames(frequency.dat)
    attenders.dat = merge(x = attenders.dat, y = frequency.dat, by = "pubid_anon", all.x = TRUE)
    multi.dat = attenders.dat[attenders.dat$Frequency>1,]
    #test = na.exclude(anon.dat[1:10,])
  #investigating degeneracy
    write.csv(cor(na.exclude(multi.dat)), "C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/choicemodel/multicorr.csv")
    for (i in 1:ncol(input.dat)){
      myPlot = ggplot(data=input.dat, aes(x=attendedIndicator, y=input.dat[,i])) + geom_point()+ labs(title=colnames(input.dat)[i])
      ggsave(paste("C:/Users/Katharina/Documents/Umich/lifecycle choice/data/plots/", i , ".pdf", sep = ""))
    }
    output= data.frame(matrix(nrow = 0, ncol = 3))
    colnames(output)= c("var", "R2", "p")
    for (i in 1:ncol(input.dat)){
      myMod = lm(input.dat$attendedIndicator~input.dat[,i])
      curoutput = data.frame(var = colnames(input.dat)[i], R2 = summary(myMod)$r.squared, p = summary(myMod)$coefficients[2,4])
      colnames(curoutput)= colnames(output)
      output = rbind(output, curoutput)
    }
    #kappa testing--uncorrect
      test.dat = na.exclude(multi.dat)
      costVars = c("tuioutlist", "realtui", "finaidest") #not colin
      distVars = c("distance", "instate", "urbanruralmatch") #pretty bad
      selectVars = c("selectdiff", "selectivity2", "gradrate") #not bad
      wealthVars= c("loanp", "fedgrantp", "expperstudent", "instperstudent", "facperstudent", "avgsal") #very bad
      wealthVars2= c("loanp", "fedgrantp", "facperstudent")#, "expperstudent", "instperstudent", "facperstudent", "avgsal") #very bad
      wealthVars3= c("expperstudent", "instperstudent", "avgsal") #very bad
      catVars= c("control", "carnegie2", "division2", "instate", "urbanruralmatch", "selectivity2") #also instate and urbanruralmatch and selectivity2, not bad
        #gets a little bit bad when we include ALL categoricals
      allVars = c("tuioutlist", "realtui", "finaidest","distance", "instate", "urbanruralmatch", "selectdiff", "selectivity2", "gradrate", "loanp", "fedgrantp", "expperstudent", "instperstudent", "facperstudent", "avgsal", "control", "carnegie2", "division2")
      redVars = c("tuioutlist", "realtui", "finaidest","distance", "urbanruralmatch", "selectdiff", "selectivity2", "gradrate", "loanp", "fedgrantp", "expperstudent", "instperstudent", "avgsal", "control", "carnegie2", "division2")
      buildupVars =  c("tuioutlist", "realtui", "finaidest"), "urbanruralmatch")
    #kappa test--correct
      kappaMatrix = matrix(nrow = ncol(test.dat), ncol = ncol(test.dat))
      reord.dat = test.dat[c(6,7,8,9,10,11,12,14,15,16,19,21,22,23,24,13,1,2,3,4,5,17,18,20)]
      for (i in 1:ncol(reord.dat)){
        for (j in 1:ncol(reord.dat)){
        #curlist = c(colnames(test.dat)[i], colnames(test.dat)[j])
        curMod = model.matrix(~reord.dat[,i]+ reord.dat[,j])
        #kapout = kappa(test.dat[,curlist])
        kapout = kappa(curMod)
        kappaMatrix[i,j]= kapout
        }
      }
  #inputs thtat are not collinear pairwise
    indep.dat = test.dat[,c("school_anon", "pubid_anon", "distance","instate","urbanruralmatch","selectdiff","loanp","fedgrantp","genderratio","control","carnegie2","selectivity2","facperstudent","division2","gradrate", "attendedIndicator", "attend", "totstudents")]

#models
  #create choice set information
    #pubid_anon identifies decision-maker ID
    #school_anon identifies alternative ID
    #AttendedIndicator identifies choice made
    mlogit.attenders.dat = mlogit.data(test.dat, shape = "long", choice = "attendedIndicator", alt.var= "school_anon", chid.var = "pubid_anon")
    mlogit.attenders.dat = mlogit.data(indep.dat, shape = "long", choice = "attendedIndicator", alt.var= "school_anon", chid.var = "pubid_anon")
  #mlogit
    mlogit.mod = mlogit(attendedIndicator~-1 + carnegie2+ tuioutlist + realtui + finaidest + distance + urbanruralmatch + selectdiff + loanp + fedgrantp + genderratio+ totstudents + control+ carnegie2 + selectivity2 + expperstudent +instperstudent + facperstudent + avgsal + division2+ gradrate, data = na.exclude(multi.dat), shape = "long", choice = "attendedIndicator", alt.var= "school_anon", chid.var = "pubid_anon")
    mlogit.mod = mlogit(attendedIndicator~-1 + tuioutlist + realtui + distance + urbanruralmatch + selectdiff + loanp + fedgrantp + genderratio+ totstudents + control+ carnegie2 + expperstudent + avgsal + division2, data = na.exclude(multi.dat), shape = "long", choice = "attendedIndicator", alt.var= "school_anon", chid.var = "pubid_anon")
    mlogit.mod = mlogit(attendedIndicator~-1 + tuioutlist  + loanp + fedgrantp + genderratio+ totstudents + control+ carnegie2 + expperstudent + avgsal + division2, data = na.exclude(multi.dat), shape = "long", choice = "attendedIndicator",  chid.var = "pubid_anon")
    mlogit.mod = mlogit(attendedIndicator~-1 + tuioutlist  + loanp + fedgrantp + genderratio+ totstudents + control+ carnegie2 + expperstudent + avgsal + division2, data = test.dat, shape = "long", choice = "attendedIndicator", chid.var = "pubid_anon", alt.var = "school_anon")
    mlogit.reduced.mod = mlogit(attendedIndicator~-1 + distance+instate+urbanruralmatch+selectdiff+genderratio+control+division2+attend+loanp+fedgrantp+facperstudent,  data = test.dat, shape = "long", choice = "attendedIndicator", chid.var = "pubid_anon", alt.var = "school_anon") #totstudents could also be added
    mlogit.smallestwithsing.mod = mlogit(attendedIndicator~-1+attend+facperstudent,  data = test.dat, shape = "long", choice = "attendedIndicator", chid.var = "pubid_anon", alt.var = "school_anon") #totstudents could also be added
      #TBD: may be able to build a working model with attend and one other variable
      #took out: totstudents, selectivity2, gradrate, carnegie2
  #simple glm model
    glm.mod = glm(attendedIndicator~tuioutlist  + loanp + fedgrantp + genderratio+ totstudents + control+ carnegie2 + expperstudent + avgsal + division2,data=na.exclude(multi.dat),family=binomial())
    glm.reduced.mod = glm(attendedIndicator~distance+instate+urbanruralmatch+selectdiff+genderratio+control+carnegie2+selectivity2+division2+attend+loanp+fedgrantp+facperstudent+gradrate+totstudents,data=test.dat,family=binomial())
    glm.test.mod = glm(attendedIndicator~distance+instate+urbanruralmatch+selectdiff+genderratio+control+division2+attend+loanp+fedgrantp+facperstudent,data=test.dat,family=binomial())
    vif(glm.test.mod)
  #mclogit
    input.dat = na.exclude(multi.dat)
    lhs = matrix(c(input.dat$attendedIndicator, input.dat$pubid_anon), nrow(input.dat), 2)
    mclogit.mod = mclogit(lhs~ tuioutlist  + loanp + fedgrantp + genderratio+ totstudents + control+ carnegie2 + expperstudent + avgsal + division2,data=na.exclude(input.dat), model = TRUE, )
  
      
    