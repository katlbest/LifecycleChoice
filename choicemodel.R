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
  library(survival)

#clear workspace ===================================================================
  rm(list = ls())
  
#data i/o ==========================================================================
  choice.dat = read.csv("D:choiceinput2.csv")
  #delete schools with selectivity 7--people make these decisions differently
  choice.dat = choice.dat[choice.dat$selectivity != 7,]
  #dataset with NAs coded correctly
    na.dat= choice.dat
    na.dat[na.dat==-3]=NA

#fill missing variables and set up derived===========================================================
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
  
  #salary
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

  #output
    keyVars = c("PUBID_1997", "AdmittedSchool", "tuioutlist", "realtui", "FINAIDEST", "distance", "instate", "urbanruralmatch", "selectdiff", "loanp", "fedgrantp", "genderratio", "totstudents", "control", "carnegie2", "selectivity2", "expperstudent", "instperstudent", "facperstudent", "avgsal", "division2", "gradrate", "AttendedIndicator", "Attend", "lifeEarnings")
    out.df = na.dat[,keyVars]
    write.csv(out.df, "D:mainvars.csv")

#preliminary visualization===========================================================
  #correlation
    write.csv(cor(na.exclude(na.dat)), "C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/choicemodel/inputcorr.csv")
  #correlation of the school actually attended only with income
    attend.dat = na.dat[na.dat$AttendedIndicator == 1,]
    drop = c("AttendedIndicator")
    attend.dat= attend.dat[,!(colnames(attend.dat) %in% drop)]
    write.csv(cor(na.exclude(attend.dat)), "C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/choicemodel/inputcorrincome.csv")

#Anonymized data===============================================================
  #read anon data
    anon.dat = read.csv("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Choice Model Inputs/anon_data2.csv", stringsAsFactors=FALSE)
    #lapply(anon.dat, class) 
  #drop nonsensical observation
    #anon.dat = anon.dat[anon.dat$school_anon != 24873,]
    #anon.dat = anon.dat[!(anon.dat$school_anon %in% c(82379,67439)),]
    #anon.dat = anon.dat[anon.dat$school_anon !=174792,]
  #adjust control entry
    anon.dat[anon.dat$control==3,]$control = 2
  #replace negative tuition values with 0?
    #TBD--not now
  #get attenders
    attenders.dat = anon.dat[anon.dat$attend != -10,]
  #check agreement of financial aid data and clean it
    aidcheck.dat = attenders.dat[,c("pubid_anon", "school_anon", "finaidest", "nonAttendAid", "attendAid")]
    aidcheck2.dat= aidcheck.dat[!(is.na(aidcheck.dat$attendAid)) & !(is.na(aidcheck.dat$nonAttendAid)),]
    myPlot = ggplot(data = aidcheck2.dat, aes(x=attendAid, y=nonAttendAid))+geom_point() + theme_bw() + labs(title="Aid from application versus attendance data")+ xlab("Aid from attendance data") + ylab("Aid from admissions data") +  geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1))
    test = lm(aidcheck2.dat$nonAttendAid~aidcheck2.dat$attendAid)
    #due to differences include financial aid from admission process only as a variable
      attenders.dat[is.na(attenders.dat$nonAttendAid),]$nonAttendAid=0
      attenders.dat = attenders.dat[,-which(names(attenders.dat) %in% c("attendAid"))]
  #delete those with only 1 appearance
    frequency.dat = data.frame(freq(ordered(attenders.dat$pubid_anon), plot=FALSE))
    frequency.dat$pubid_anon= rownames(frequency.dat)
    attenders.dat = merge(x = attenders.dat, y = frequency.dat, by = "pubid_anon", all.x = TRUE)
    multi.dat = attenders.dat[attenders.dat$Frequency>1,]
  #test dataset excluding na's and make categoricals
    noNA.dat = na.exclude(multi.dat)
    #set up interval selectivities
      noNA.dat$selectdiffInt = NA
      noNA.dat$selectInt = NA
      lookup = data.frame(id = c(1,2,3,4,5), percent = c(33, 60, 75, 85, 100))
      for (i in 1:nrow(noNA.dat)){
        admitInt = lookup[(1:dim(lookup)[1])[lookup[,1]==noNA.dat$admit[i]],2]
        noNA.dat$selectInt[i] = lookup[(1:dim(lookup)[1])[lookup[,1]==noNA.dat$selectivity2[i]],2]
        noNA.dat$selectdiffInt[i]=noNA.dat$selectInt[i]-admitInt
      } #TBD need to add admit!
    noNA.dat$instate = as.factor(noNA.dat$instate)
    noNA.dat$urbanruralmatch = as.factor(noNA.dat$urbanruralmatch)
    #noNA.dat$selectdiff = as.factor(noNA.dat$selectdiff)
    noNA.dat$control = as.factor(noNA.dat$control)
    noNA.dat$carnegie2 = as.factor(noNA.dat$carnegie2)
    #noNA.dat$selectivity2 = as.factor(noNA.dat$selectivity2)
    noNA.dat$attend = as.factor(noNA.dat$attend)
  
#Investigating degeneracy===============================================================
  #plot data versus attended indicator
    for (i in 1:ncol(multi.dat)){
      myPlot = ggplot(data=multi.dat, aes(x=attendedIndicator, y=multi.dat[,i])) + geom_point()+ labs(title=colnames(multi.dat)[i])
      ggsave(paste("C:/Users/Katharina/Documents/Umich/lifecycle choice/data/plots/", i , ".pdf", sep = ""))
    }
    output= data.frame(matrix(nrow = 0, ncol = 3))
    colnames(output)= c("var", "R2", "p")
    for (i in 1:ncol(multi.dat)){
      myMod = lm(multi.dat$attendedIndicator~multi.dat[,i])
      curoutput = data.frame(var = colnames(multi.dat)[i], R2 = summary(myMod)$r.squared, p = summary(myMod)$coefficients[2,4])
      colnames(curoutput)= colnames(output)
      output = rbind(output, curoutput)
    }
  #kappa test
    kappaMatrix = matrix(nrow = ncol(noNA.dat), ncol = ncol(noNA.dat))
    #reord.dat = noNA.dat
    reord.dat = noNA.dat[c(6,7,8,9,10,11,12,14,15,16,19,21,22,23,24,27,28,13,5,26,1,2,3,4,17,18,20, 25, 29)]
    for (i in 1:ncol(reord.dat)){
      for (j in 1:ncol(reord.dat)){
      #curlist = c(colnames(noNA.dat)[i], colnames(noNA.dat)[j])
      curMod = model.matrix(~reord.dat[,i]+ reord.dat[,j])
      #kapout = kappa(noNA.dat[,curlist])
      kapout = kappa(curMod)
      kappaMatrix[i,j]= kapout
      }
    }
  #testing with simple models
    #glm models and VIF--do not take choice situations (distinction between individuals) into account
      #all variables
        glm.mod = glm(attendedIndicator~tuioutlist+realtui+finaidest+distance+instate+urbanruralmatch+loanp+fedgrantp+control+carnegie2+avgsal+division2+gradrate+attend+expperstudent2+ instperstudent2+facperstudent2+genderratio2+totstudents2+selectdiffInt + selectInt,data=noNA.dat,family=binomial())
        vif(glm.mod)
      glm.reduced.mod = glm(attendedIndicator~distance+instate+urbanruralmatch+selectdiff+genderratio+control+carnegie2+selectivity2+division2+attend+loanp+fedgrantp+facperstudent+gradrate+totstudents,data=noNA.dat,family=binomial())
      vif(glm.reduced.mod)
    #mclogit models--MODEL SELECTION
      lhs = matrix(c(noNA.dat$attendedIndicator, noNA.dat$pubid_anon), nrow(noNA.dat), 2)
      #all--ld vars
        mclogit.all.mod = mclogit(lhs~tuioutlist+realtui+finaidest+distance+instate+urbanruralmatch+selectdiff+loanp+fedgrantp+genderratio+totstudents+control+carnegie2+selectivity2+expperstudent+instperstudent+facperstudent+avgsal+division2+gradrate+attend+expperstudent2+instperstudent2+facperstudent2+genderratio2+totstudents2,data=noNA.dat, model = TRUE, )
          #singular
      #all with new vars only
        mclogit.all.mod = mclogit(lhs~tuioutlist+realtui+finaidest+distance+instate+urbanruralmatch+selectdiffInt+loanp+fedgrantp+control+carnegie2+selectInt+avgsal+division2+gradrate+attend+expperstudent2+instperstudent2+facperstudent2+genderratio2+totstudents2+nonAttendAid,data=noNA.dat, model = TRUE, )
          #still singular
      #remove obvious collinearity until a non-singular model is run
          #finaidest bc it is linear combination of other variables
          #instperstudent because it is highly correlated with expperstudent and info is captured by avg salary
          #instate because distance is captured by distance, in-state by the tuition amount
          #attend can't be in the model since we do not know its value until after attendance decision
          #selectdiff because it is less significant that selectivity2
          mclogit.starting.mod = mclogit(lhs~selectivity2+ tuioutlist+realtui+distance+urbanruralmatch+loanp+fedgrantp+control+carnegie2+avgsal+division2+gradrate+expperstudent2+facperstudent2+genderratio2+totstudents2,data=noNA.dat, model = TRUE, )
      #model selection
          #obvious
            #distance insignificant but BTL finds it significant
            #control not significant
            #tuition not significant
            #division not significant
            #urbanruralmatch not significant
            mclogit.small.mod = mclogit(lhs~selectivity2+realtui+loanp+fedgrantp+carnegie2+avgsal+gradrate+expperstudent2+facperstudent2+genderratio2+totstudents2,data=noNA.dat, model = TRUE, )
          #less obvious
            #gender ratio not significant
            #facperstudent not significant, note that removing it increases sig. of experstudent
            #loanp not significant, note that removing it does not increase significance of fedgrantp
            #totstudents not siginificant, and removing it makes avgsal less significant (very slightly)
            #expperstudent and avgsal both not significant, no matter in what order they are removed
            #selectivity was only very slightly significant in group 5, and stops being so in smallest model
            mclogit.tiny.mod = mclogit(lhs~realtui+fedgrantp+carnegie2+gradrate,data=noNA.dat, model = TRUE, )
            mclogit.tiny2.mod = mclogit(lhs~tuioutlist+finaidest+fedgrantp+carnegie2+gradrate,data=noNA.dat, model = TRUE, )
    #clogit in survival
      #mclogit model revisitied
      clogit.tiny.mod = clogit(attendedIndicator ~ realtui+fedgrantp+carnegie2+gradrate+ strata(pubid_anon), noNA.dat)
    #mlogit
      mlogit.tiny.mod = mlogit(attendedIndicator~realtui+fedgrantp+carnegie2+gradrate|0|0, data=noNA.dat,shape="long",choice="attendedIndicator",alt.var="school_anon",chid.var="pubid_anon")
        #error missing value where TRUE/FALSE needed
    #BTL model
      clogit.btl.mod = clogit(attendedIndicator ~ realtui+tuioutlist+finaidest+distance+selectdiff+expperstudent+instperstudent+facperstudent+avgsal+ strata(pubid_anon), noNA.dat)
  
#tests =========================================================================
  #mclogit.tiny.mod is the base model
  #heteroskedasticity
    plot(clogit.tiny.mod$residuals)
    plot(log(mclogit.tiny.mod$residuals))
    plot(mclogit.tiny2.mod$residuals)
  #outlier--these don't work, can test outliers manually
    # added variable plots 
    avPlots(mclogit.tiny.mod)
    # Influence Plot 
    influencePlot(mclogit.tiny.mod,   id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )

#pairwise plots
  outdata = data.frame(matrix(nrow = 0, ncol = 3))
  colnames(outdata) = c("ts1", "ts2", "R2")
  relVars.dat = noNA.dat[,c("tuioutlist","realtui","finaidest","distance","instate","urbanruralmatch","loanp","fedgrantp","control","carnegie2","avgsal","division2","gradrate","expperstudent2","instperstudent2","facperstudent2","genderratio2","totstudents2","nonAttendAid","selectdiffInt","selectInt")]
  for (i in 1:ncol(relVars.dat)){
    for (j in 1:ncol(relVars.dat)){
      myPlot = ggplot(data=relVars.dat, aes(x= relVars.dat[,i], y = relVars.dat[,j]))+geom_point()+theme_bw()+ xlab(colnames(relVars.dat)[i])+ ylab(colnames(relVars.dat)[j])
      nameStr = paste(colnames(relVars.dat)[i], colnames(relVars.dat)[j], sep ="")
      ggsave(paste("C:/Users/Katharina/Documents/Umich/lifecycle choice/data/plots/pairwise",nameStr, ".pdf", sep = ""))
      curMod = lm(relVars.dat[,j]~relVars.dat[,i])
      curout = data.frame(ts1 = colnames(relVars.dat)[i], ts2 =colnames(relVars.dat)[j], R2 = summary(curMod)$r.squared)
      colnames(curout)=colnames(outdata)
      outdata = rbind(outdata, curout)
    }
  }
  write.csv(outdata,"C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Choice Model Inputs/test.csv")