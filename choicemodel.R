#NOTES:======================================================================
  #this file analyzes the choice data and runs the appropriate models

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
  #fix financial aid variables
    #nonattendaid is zero if not reports
      anon.dat[is.na(anon.dat$nonAttendAid),]$nonAttendAid=0
    #create finaidest2 using rules about when to use attended data
      anon.dat[anon.dat$nonAttendMiss==-2,]$nonAttendMiss=0 #unknowns are treated as 0
      for (i in 1:nrow(anon.dat)){
        if (anon.dat$nonAttendMiss[i] %in% c(-3, -4)){ #if data is truly missing or valid skip (possibly indicating that no decision has been made)
          #check for attended aid
          if (is.na(anon.dat$attendAid[i])){
            anon.dat$nonAttendMiss[i]= 0
          }
          else if (anon.dat$attendAid[i]>0){
            #this is based on the model "test" below
            #anon.dat$nonAttendMiss[i]= anon.dat$attendAid[i]*(.50291)
            anon.dat$nonAttendMiss[i]= anon.dat$attendAid[i]*(0.44536)+963.86273
            #print(anon.dat$pubid_anon[i])
            #print(paste(anon.dat$nonAttendMiss[i], anon.dat$nonAttendAid[i], sep = ","))
          }else{
            anon.dat$nonAttendMiss[i]= 0
          }
        }
      }
      anon.dat$finaidest2 = anon.dat$nonAttendMiss
    #adjust both net tuition variables as necessary
      anon.dat$costbeforeaid = anon.dat$realtui + anon.dat$finaidest #this works correctly
      anon.dat$realtui2 = anon.dat$costbeforeaid- anon.dat$finaidest2 #this works correctly
      anon.dat$realtuiApply = anon.dat$costbeforeaid-anon.dat$nonAttendAid #this works correctly
      anon.dat$finaidwstatedisc[i]=NA
      anon.dat$finaidwstatedisc2[i]=NA
      anon.dat[anon.dat$aidallschool==-3,]$aidallschool = 0
      for (i in 1:nrow(anon.dat)){
        if (anon.dat$instate[i]==1){
          anon.dat$finaidwstatedisc2[i] = anon.dat$tuioutlist[i]+anon.dat$feeout[i]-(anon.dat$tuiinlist[i]+anon.dat$feein[i])+ anon.dat$finaidest2[i]+ anon.dat$aidallschool[i]
          anon.dat$finaidwstatedisc[i] = anon.dat$tuioutlist[i]+anon.dat$feeout[i]-(anon.dat$tuiinlist[i]+anon.dat$feein[i])+ anon.dat$finaidest[i]+ anon.dat$aidallschool[i]
        } else{
          anon.dat$finaidwstatedisc2[i] =anon.dat$finaidest2[i]+ anon.dat$aidallschool[i]
          anon.dat$finaidwstatedisc[i] =anon.dat$finaidest[i]+ anon.dat$aidallschool[i]
        }
      }
      anon.dat$listwfees = anon.dat$tuioutlist + anon.dat$feeout
      anon.dat$listminusactual = anon.dat$listwfees-anon.dat$realtui2
      anon.dat$goodVar = anon.dat$tuioutlist-anon.dat$realtui- anon.dat$finaidest+ anon.dat$finaidest2
      anon.dat$goodVar2 = anon.dat$tuioutlist-anon.dat$realtui+ anon.dat$finaidest- anon.dat$finaidest2
  #get attenders
    attenders.dat = anon.dat[anon.dat$attend != -10,]
    #attenders.dat = anon.dat
  #check agreement of financial aid data and clean it
    aidcheck.dat = attenders.dat[,c("pubid_anon", "school_anon", "finaidest", "nonAttendAid", "attendAid")]
    aidcheck2.dat= aidcheck.dat[!(is.na(aidcheck.dat$attendAid)) & !(is.na(aidcheck.dat$nonAttendAid)),]
    myPlot = ggplot(data = aidcheck2.dat, aes(x=attendAid, y=nonAttendAid))+geom_point() + theme_bw() + labs(title="Aid from application versus attendance data")+ xlab("Aid from attendance data") + ylab("Aid from admissions data") +  geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1))
    test = lm(aidcheck2.dat$nonAttendAid~aidcheck2.dat$attendAid)
  #delete those with only 1 appearance
    frequency.dat = data.frame(freq(ordered(attenders.dat$pubid_anon), plot=FALSE))
    frequency.dat$pubid_anon= rownames(frequency.dat)
    attenders.dat = merge(x = attenders.dat, y = frequency.dat, by = "pubid_anon", all.x = TRUE)
    multi.dat = attenders.dat[attenders.dat$Frequency>1,]
    #multi.dat = attenders.dat
  #create new selectivity variables
    multi.dat$selectdiffInt = NA
    multi.dat$selectInt = NA
    lookup = data.frame(id = c(1,2,3,4,5,6), percent = c(33, 60, 75, 85, 100, 100))
    for (i in 1:nrow(multi.dat)){
      admitInt = lookup[(1:dim(lookup)[1])[lookup[,1]==multi.dat$admit[i]],2]
      multi.dat$selectInt[i] = lookup[(1:dim(lookup)[1])[lookup[,1]==multi.dat$selectivity2[i]],2]
      multi.dat$selectdiffInt[i]=multi.dat$selectInt[i]-admitInt
    } 
  #set categorical variables
    multi.dat$instate = as.factor(multi.dat$instate)
    multi.dat$urbanruralmatch = as.factor(multi.dat$urbanruralmatch)
    multi.dat$control = as.factor(multi.dat$control)
    multi.dat$carnegie2 = as.factor(multi.dat$carnegie2)
    multi.dat$attend = as.factor(multi.dat$attend)
  #remove totally irrelevant variables
    relVars.dat = multi.dat[,c("pubid_anon", "school_anon","attendedIndicator","tuioutlist","realtui2","finaidest2","distance","instate","urbanruralmatch","loanp","fedgrantp","control","carnegie2","avgsal","division2","gradrate","expperstudent2","instperstudent2","facperstudent2","genderratio2","totstudents2","nonAttendAid","realtuiApply", "selectdiffInt","selectInt", "fedgrantp", "finaidwstatedisc", "listminusactual", "listwfees", "realtui2", "feeout", "goodVar", "goodVar2", "finaidwstatedisc2")]
  #if you are running this without the deletion of non-attenders, save input data for second stage
    #write.csv(relVars.dat, "C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Choice Model Inputs/anon_dat_mergedata.csv")
  #test dataset excluding na's and make categoricals
    noNA.dat = na.exclude(relVars.dat)
      
#Investigating degeneracy and correlation===============================================================
  #plot data versus attended indicator
    for (i in 1:ncol(noNA.dat)){
      myPlot = ggplot(data=noNA.dat, aes(x=attendedIndicator, y=noNA.dat[,i])) + geom_point()+ labs(title=colnames(noNA.dat)[i])
      ggsave(paste("C:/Users/Katharina/Documents/Umich/lifecycle choice/data/plots/", i , ".pdf", sep = ""))
    }
    output= data.frame(matrix(nrow = 0, ncol = 3))
    colnames(output)= c("var", "R2", "p")
    for (i in 1:ncol(relVars.dat)){
      myMod = lm(relVars.dat$attendedIndicator~relVars.dat[,i])
      curoutput = data.frame(var = colnames(relVars.dat)[i], R2 = summary(myMod)$r.squared, p = summary(myMod)$coefficients[2,4])
      colnames(curoutput)= colnames(output)
      output = rbind(output, curoutput)
    }
  #kappa test
    kappaMatrix = matrix(nrow = ncol(noNA.dat), ncol = ncol(noNA.dat))
    #reord.dat = noNA.dat
    reord.dat = noNA.dat[c(5, 6, 7, 8, 9, 10, 11, 13, 14, 17, 18, 22, 23, 20, 16, 4, 2, 3, 12, 15, 19, 21)]
    for (i in 1:ncol(reord.dat)){
      for (j in 1:ncol(reord.dat)){
      #curlist = c(colnames(noNA.dat)[i], colnames(noNA.dat)[j])
      curMod = model.matrix(~reord.dat[,i]+ reord.dat[,j])
      #kapout = kappa(noNA.dat[,curlist])
      kapout = kappa(curMod)
      kappaMatrix[i,j]= kapout
      }
    }
  #pairwise regressions and plots
    outdata = data.frame(matrix(nrow = 0, ncol = 3))
    colnames(outdata) = c("ts1", "ts2", "R2")
    for (i in 1:ncol(noNA.dat)){
      for (j in 1:ncol(noNA.dat)){
        myPlot = ggplot(data=noNA.dat, aes(x= noNA.dat[,i], y = noNA.dat[,j]))+geom_point()+theme_bw()+ xlab(colnames(noNA.dat)[i])+ ylab(colnames(noNA.dat)[j])
        nameStr = paste(colnames(noNA.dat)[i], colnames(noNA.dat)[j], sep ="")
        ggsave(paste("C:/Users/Katharina/Documents/Umich/lifecycle choice/data/plots/pairwise",nameStr, ".pdf", sep = ""))
        curMod = lm(noNA.dat[,j]~noNA.dat[,i])
        curout = data.frame(ts1 = colnames(noNA.dat)[i], ts2 =colnames(noNA.dat)[j], R2 = summary(curMod)$r.squared)
        colnames(curout)=colnames(outdata)
        outdata = rbind(outdata, curout)
      }
    }
    write.csv(outdata,"C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Choice Model Inputs/test.csv")
  #glm models and VIF--do not take choice situations (distinction between individuals) into account
    #all variables
      glm.mod = glm(attendedIndicator~tuioutlist+realtui2+finaidest2+distance+instate+urbanruralmatch+loanp+fedgrantp+control+carnegie2+avgsal+division2+gradrate+expperstudent2+instperstudent2+facperstudent2+genderratio2+totstudents2+nonAttendAid+realtuiApply+selectdiffInt+selectInt,data=noNA.dat,family=binomial())
        #singular
      #remove realtuiapply and nonattendaid
        glm.reduced1.mod = glm(attendedIndicator~tuioutlist+realtui2+finaidest2+distance+instate+urbanruralmatch+loanp+fedgrantp+control+carnegie2+avgsal+division2+gradrate+expperstudent2+instperstudent2+facperstudent2+genderratio2+totstudents2+selectdiffInt+selectInt,data=noNA.dat,family=binomial())
        vif(glm.reduced1.mod)
      #remove realtui2 and finaidest2
        glm.reduced2.mod = glm(attendedIndicator~tuioutlist+distance+instate+urbanruralmatch+loanp+fedgrantp+control+carnegie2+avgsal+division2+gradrate+expperstudent2+instperstudent2+facperstudent2+genderratio2+totstudents2+nonAttendAid+realtuiApply+selectdiffInt+selectInt,data=noNA.dat,family=binomial())
        vif(glm.reduced2.mod)
      #pick one of each redundant variable
        glm.reduced3.mod = glm(attendedIndicator~tuioutlist+finaidest2+distance+instate+urbanruralmatch+loanp+fedgrantp+control+carnegie2+avgsal+division2+gradrate+expperstudent2+genderratio2+totstudents2+selectdiffInt+selectInt,data=noNA.dat,family=binomial())
        vif(glm.reduced3.mod)
  #running kappa again on reduced models
    testVars = noNA.dat[c("tuioutlist","finaidest2","distance","instate","urbanruralmatch","loanp","fedgrantp","control","carnegie2","avgsal","division2","gradrate","expperstudent2","genderratio2","totstudents2","selectdiffInt","selectInt")]
    kappaMatrix = matrix(nrow = ncol(testVars), ncol = ncol(testVars))
    for (i in 1:ncol(testVars)){
      for (j in 1:ncol(testVars)){
        curMod = model.matrix(~testVars[,i]+ testVars[,j])
        kapout = kappa(curMod)
        kappaMatrix[i,j]= kapout
      }
    }

#model selection=======================================================================================
  #reduce dataset based on preliminary analysis and limit impact of NAs, keep relVars for further analysis
    relVarsRed.dat = relVars.dat[,c("pubid_anon", "school_anon","attendedIndicator","tuioutlist","realtui2","finaidest2","distance","instate","urbanruralmatch","loanp","control","division2","gradrate","expperstudent2","instperstudent2","facperstudent2","genderratio2","totstudents2","nonAttendAid","realtuiApply", "selectdiffInt", "selectInt", "avgsal", "fedgrantp", "finaidwstatedisc", "listminusactual", "listwfees", "realtui2", "feeout", "goodVar", "goodVar2", "finaidwstatedisc2")]
  #remove NAs for now
    noNARed.dat = na.exclude(relVarsRed.dat)
  #mclogit models--MODEL SELECTION
    lhs = matrix(c(noNARed.dat$attendedIndicator, noNARed.dat$pubid_anon), nrow(noNARed.dat), 2)
  #testing final models with more vars
    mclogit.test.mod = mclogit(lhs~tuioutlist+finaidwstatedisc+gradrate+instperstudent2+selectdiffInt,data=noNARed.dat, model = TRUE, )
  #all variables
      mclogit.all.mod = mclogit(lhs~tuioutlist+realtui2+finaidest2+nonAttendAid+realtuiApply+totstudents2+gradrate+expperstudent2+instperstudent2+facperstudent2+genderratio2+selectdiffInt+loanp+control+division2+distance+instate+urbanruralmatch,data=noNARed.dat, model = TRUE, )
      mclogit.almostall.mod = mclogit(lhs~tuioutlist+realtui2+totstudents2+gradrate+expperstudent2+instperstudent2+facperstudent2+genderratio2+selectdiffInt+loanp+control+division2+distance+instate+urbanruralmatch,data=noNARed.dat, model = TRUE, )
      clogit.almostall.mod = clogit(attendedIndicator~tuioutlist+realtui2+totstudents2+gradrate+expperstudent2+instperstudent2+facperstudent2+genderratio2+selectdiffInt+loanp+control+division2+distance+instate+urbanruralmatch+ strata(pubid_anon), noNARed.dat)
        ll = clogit.almostall.mod$loglik
        McFR2= 1-ll[2]/ll[1]
      #squares
        clogit.squares.mod = clogit(attendedIndicator~I(tuioutlist^2)+I(realtui2^2)+I(totstudents2^2)+I(gradrate)^2+I(expperstudent2^2)+I(instperstudent2^2)+I(facperstudent2^2)+I(genderratio2^2)+I(selectdiffInt^2)+I(loanp^2)+control+division2+I(distance^2)+instate+urbanruralmatch+ strata(pubid_anon), noNARed.dat)
          ll = clogit.squares.mod$loglik
          McFR2= 1-ll[2]/ll[1]

        #runs, warning
    #config A.B.A
      #all
        mclogit.A.B.A.mod = mclogit(lhs~realtui2+totstudents2+gradrate+instperstudent2+genderratio2+selectdiffInt+loanp+control+division2+distance+instate+urbanruralmatch,data=noNARed.dat, model = TRUE, )
        #runs, no warning
      #take out instate
        mclogit.A.B.A.mod = mclogit(lhs~realtui2+totstudents2+gradrate+instperstudent2+genderratio2+selectdiffInt+loanp+control+division2+distance+urbanruralmatch,data=noNARed.dat, model = TRUE, )
        #warning
      #take out division2
        mclogit.A.B.A.mod = mclogit(lhs~realtui2+totstudents2+gradrate+instperstudent2+genderratio2+selectdiffInt+loanp+control+distance+urbanruralmatch,data=noNARed.dat, model = TRUE, )
        #warning
      #test for distance versus urban rural match effect
        mclogit.A.B.A.mod = mclogit(lhs~realtui2+totstudents2+gradrate+instperstudent2+genderratio2+selectdiffInt+loanp+control+distance,data=noNARed.dat, model = TRUE, )
        #warnings, neither is significant and no effect from removing one--remove both
        mclogit.A.B.A.mod = mclogit(lhs~realtui2+totstudents2+gradrate+instperstudent2+genderratio2+selectdiffInt+loanp+control,data=noNARed.dat, model = TRUE, )
      #remove loanp
        mclogit.A.B.A.mod = mclogit(lhs~realtui2+totstudents2+gradrate+instperstudent2+genderratio2+selectdiffInt+control,data=noNARed.dat, model = TRUE, )
      #remove gender ratio
        mclogit.A.B.A.mod = mclogit(lhs~realtui2+totstudents2+gradrate+instperstudent2+selectdiffInt+control,data=noNARed.dat, model = TRUE, )
      #remove control
        mclogit.A.B.A.mod = mclogit(lhs~realtui2+totstudents2+gradrate+instperstudent2+selectdiffInt,data=noNARed.dat, model = TRUE, )
      #create model mclogit.A.B.A.A (remove gradrate and keep totstudents)
        mclogit.A.B.A.A.mod = mclogit(lhs~realtui2+totstudents2+instperstudent2+selectdiffInt,data=noNARed.dat, model = TRUE, )
          #totstudents does not become significant
      #create model mclogit.A.B.A.B (remove totstudends and keep gradrate)
        mclogit.A.B.A.B.mod = mclogit(lhs~realtui2+gradrate+instperstudent2+selectdiffInt,data=noNARed.dat, model = TRUE, )
          #significant, try in clogit
          clogit.A.B.A.B.mod = clogit(attendedIndicator ~realtui2+gradrate+instperstudent2+selectdiffInt+ strata(pubid_anon), noNARed.dat)
    #config A.B.B
      #all
        mclogit.A.B.B.mod = mclogit(lhs~realtui2+totstudents2+gradrate+expperstudent2+genderratio2+selectdiffInt+loanp+control+division2+distance+instate+urbanruralmatch,data=noNARed.dat, model = TRUE, )
      #drop instate, division2, distance, urbanruralmatch, gender ratio, loanp, control
        mclogit.A.B.B.mod = mclogit(lhs~realtui2+totstudents2+gradrate+expperstudent2+selectdiffInt,data=noNARed.dat, model = TRUE, )
      #test gradrate versus totstudents
        mclogit.A.B.B.mod = mclogit(lhs~realtui2+gradrate+expperstudent2+selectdiffInt,data=noNARed.dat, model = TRUE, )
          #totstudents does nto become significant
      #reduced model
        mclogit.A.B.B.B.mod = mclogit(lhs~realtui2+gradrate+expperstudent2+selectdiffInt,data=noNARed.dat, model = TRUE, )
          #expperstudents is NOT signidsddsdficant!!
        clogit.A.B.B.B.mod = clogit(attendedIndicator ~realtui2+gradrate+expperstudent2+selectdiffInt+ strata(pubid_anon), noNARed.dat)
    #config A.B.C
      #all
        mclogit.A.B.C.mod = mclogit(lhs~realtui2+totstudents2+gradrate+facperstudent2+genderratio2+selectdiffInt+loanp+control+division2+distance+instate+urbanruralmatch,data=noNARed.dat, model = TRUE, )
      #drop instate, division2, distance, urbanruralmatch, gender ratio, loanp, control, totstudents2
        mclogit.A.B.C.B.mod = mclogit(lhs~realtui2+gradrate+facperstudent2+selectdiffInt,data=noNARed.dat, model = TRUE, )
          #facperstudent barely significant
        clogit.A.B.C.B.mod = clogit(attendedIndicator ~realtui2+gradrate+facperstudent2+selectdiffInt+ strata(pubid_anon), noNARed.dat)
    #config B.B.A
      #all
        mclogit.B.B.A.mod = mclogit(lhs~tuioutlist+finaidest2+totstudents2+gradrate+instperstudent2+genderratio2+selectdiffInt+loanp+control+division2+distance+instate+urbanruralmatch,data=noNARed.dat, model = TRUE, )
      #drop instate, division2, distance, urbanruralmatch, gender ratio, loanp, control, totstudents2
        mclogit.B.B.A.B.mod = mclogit(lhs~tuioutlist+finaidest2+gradrate+instperstudent2+selectdiffInt,data=noNARed.dat, model = TRUE, )
        clogit.B.B.A.B.mod = clogit(attendedIndicator ~tuioutlist+finaidest2+gradrate+instperstudent2+selectdiffInt+strata(pubid_anon), noNARed.dat)
        #expperstudent is not significant, facperstudent is very slightly significant
    #config C.B.A.B
      mclogit.C.B.A.B.mod = mclogit(lhs~tuioutlist+nonAttendAid+gradrate+gradrate+instperstudent2+selectdiffInt,data=noNARed.dat, model = TRUE, )
      #instperstudent less significant here than other configurations
    #config D.B.A.B
      mclogit.D.B.A.B.mod = mclogit(lhs~realtuiApply+gradrate+gradrate+instperstudent2+selectdiffInt,data=noNARed.dat, model = TRUE, )
  #review baseline models
    #main
      summary(mclogit.A.B.A.B.mod)
      summary(mclogit.B.B.A.B.mod)
      summary(mclogit.C.B.A.B.mod)
      summary(mclogit.D.B.A.B.mod)
    #alternate--show that expperstudent and facperstudent are also sometimes important    
      summary(mclogit.A.B.B.B.mod)
      summary(mclogit.A.B.C.B.mod)
    #best model based on fit is mclogit.B.B.A.B.mod, so start here
      #attempt using PCA
        schoolqual.dat = noNARed.dat[,c("totstudents2", "gradrate", "fedgrantp", "selectInt", "avgsal")]
        fit <- princomp(schoolqual.dat, cor=T)
        plot(fit,type="lines") #plot
        noNARed.dat$schoolqual = fit$scores[,1]
        mclogit.B.B.A.C.mod = mclogit(lhs~tuioutlist+finaidest2+schoolqual+instperstudent2+selectdiffInt,data=noNARed.dat, model = TRUE, )
        clogit.B.B.A.C.mod = clogit(attendedIndicator~tuioutlist+finaidest2+schoolqual+instperstudent2+selectdiffInt+strata(pubid_anon), noNARed.dat)
    #test adding variables to best model
      mclogit.add.mod = mclogit(lhs ~tuioutlist+finaidest2+gradrate+instperstudent2+selectdiffInt+selectInt+avgsal+fedgrantp,data=noNARed.dat, model = TRUE, )
    #test adding only distance
      mclogit.add.mod = mclogit(lhs~distance + tuioutlist+finaidest2+gradrate+instperstudent2+selectdiffInt,data=noNARed.dat, model = TRUE, )
        #additional variables are not significant
    #testing with the finaidwstatedisc variable
      mclogit.newaid.mod = mclogit(lhs ~tuioutlist+finaidwstatedisc+gradrate+instperstudent2+selectdiffInt,data=noNARed.dat, model = TRUE, )
        #yes this is better

#run seleted models, do outlier tests===============================================================
  #reduce dataset to those needed in final models and remove NAs
    relVarsPCA.dat = relVars.dat[,c("totstudents2", "gradrate", "fedgrantp", "selectInt", "avgsal", "pubid_anon","attendedIndicator","tuioutlist","finaidest2","instperstudent2","selectdiffInt", "finaidwstatedisc", "distance", "listminusactual", "listwfees", "realtui2", "feeout", "goodVar", "goodVar2", "finaidwstatedisc2")]
    #fix selectivity errors
      relVarsPCA.dat[relVarsPCA.dat$selectdiffInt < 0,]$selectdiffInt =0
    PCAtest = data.frame(is.na(relVarsPCA.dat))
    PCAtest$miss=FALSE
    for (i in 1:nrow(PCAtest)){
      myVect = PCAtest[i,]
      PCAtest$miss[i]=any(as.logical(PCAtest[i,]))
    }
    missVals = relVarsPCA.dat[PCAtest$miss==TRUE,]
    relVarsPCA.dat = na.exclude(relVarsPCA.dat)
    schoolqual.dat = relVarsPCA.dat[,c("totstudents2", "gradrate", "fedgrantp", "selectInt", "avgsal")]
    fit <- princomp(schoolqual.dat, cor=T, na.action= "exclude")
    relVarsPCA.dat$schoolqual = fit$scores[,1]
    relVarsNoPCA.dat = relVars.dat[,c("pubid_anon","attendedIndicator","tuioutlist","finaidest2","gradrate","instperstudent2","selectdiffInt","finaidwstatedisc", "distance", "listminusactual", "listwfees", "realtui2", "feeout", "goodVar", "goodVar2", "finaidwstatedisc2")]
    relVarsNoPCA.dat = na.exclude(relVarsNoPCA.dat)
  #plots for further outlier detection
    #test.dat = relVarsPCA.dat[,c("attendedIndicator","instperstudent2", "tuioutlist", "finaidest2", "schoolqual")]
    test.dat = relVarsNoPCA.dat[,c("attendedIndicator","instperstudent2", "tuioutlist", "gradrate", "finaidwstatedisc", "selectdiffInt")]
    for (i in 1:ncol(test.dat)){
      myPlot = ggplot(data=test.dat, aes(x=attendedIndicator, y=test.dat[,i])) + geom_point()+ labs(title=colnames(test.dat)[i])
      ggsave(paste("C:/Users/Katharina/Documents/Umich/lifecycle choice/data/plots/reduced2", i , ".pdf", sep = ""))
    }
  #remove outliers
    #instperstudent
      #locate
        relVarsPCA.dat$pubid_anon[which.max(relVarsPCA.dat[,c("instperstudent2"),])]
        relVarsNoPCA.dat$pubid_anon[which.max(relVarsNoPCA.dat[,c("instperstudent2"),])]
        #exists in both datasets
      #remove
        relVarsPCA.dat = relVarsPCA.dat[-(which.max(relVarsPCA.dat[,c("instperstudent2"),])),]
        relVarsNoPCA.dat = relVarsNoPCA.dat[-(which.max(relVarsNoPCA.dat[,c("instperstudent2"),])),]
      #throw out all those with instperstudent above 40K 
        tooBig = relVarsNoPCA.dat[relVarsNoPCA.dat$instperstudent2>=40000,]$pubid_anon
        relVarsNoPCA.dat= relVarsNoPCA.dat[!relVarsNoPCA.dat$pubid_anon %in% tooBig,]
      #test if these help model--no
        #tooBig = relVarsNoPCA.dat[relVarsNoPCA.dat$instperstudent2>=20000,]$pubid_anon
        #relVarsNoPCA.dat= relVarsNoPCA.dat[!relVarsNoPCA.dat$pubid_anon %in% tooBig,]
        #tooBig = relVarsNoPCA.dat[relVarsNoPCA.dat$tuioutlist>=32000,]$pubid_anon
        #relVarsNoPCA.dat= relVarsNoPCA.dat[!relVarsNoPCA.dat$pubid_anon %in% tooBig,]
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
  #delete those without an attended school or with only a single data point again
    #delete without attended school
      sumPCA.dat = ddply(relVarsPCA.dat,~pubid_anon,summarise,sum=sum(attendedIndicator))
      relVarsPCA.dat = merge(x = relVarsPCA.dat, y = sumPCA.dat, by = "pubid_anon", all.x = TRUE)
      relVarsPCA.dat = relVarsPCA.dat[relVarsPCA.dat$sum>0,]
      sumNoPCA.dat= ddply(relVarsNoPCA.dat,~pubid_anon,summarise,sum=sum(attendedIndicator))
      relVarsNoPCA.dat = merge(x = relVarsNoPCA.dat, y = sumNoPCA.dat, by = "pubid_anon", all.x = TRUE)
      relVarsNoPCA.dat = relVarsNoPCA.dat[relVarsNoPCA.dat$sum>0,]
    #delete with only single entry
      frequencyPCA.dat = data.frame(freq(ordered(relVarsPCA.dat$pubid_anon), plot=FALSE))
      frequencyPCA.dat$pubid_anon= rownames(frequencyPCA.dat)
      relVarsPCA.dat = merge(x = relVarsPCA.dat, y = frequencyPCA.dat, by = "pubid_anon", all.x = TRUE)
      relVarsPCA.dat = relVarsPCA.dat[relVarsPCA.dat$Frequency>1,]
      frequencyNoPCA.dat = data.frame(freq(ordered(relVarsNoPCA.dat$pubid_anon), plot=FALSE))
      frequencyNoPCA.dat$pubid_anon= rownames(frequencyNoPCA.dat)
      relVarsNoPCA.dat = merge(x = relVarsNoPCA.dat, y = frequencyNoPCA.dat, by = "pubid_anon", all.x = TRUE)
      relVarsNoPCA.dat = relVarsNoPCA.dat[relVarsNoPCA.dat$Frequency>1,]
    #drop useless columns
      relVarsNoPCA.dat= relVarsNoPCA.dat[,!names(relVarsNoPCA.dat) %in% c("sum", "Frequency", "Percent", "Cum.Percent")]
      relVarsPCA.dat= relVarsPCA.dat[,!names(relVarsPCA.dat) %in% c("sum", "Frequency", "Percent", "Cum.Percent")]
  #run models with these datasets
      #mclogit.B.B.A.B.mod
        lhsNoPCA = matrix(c(relVarsNoPCA.dat$attendedIndicator, relVarsNoPCA.dat$pubid_anon), nrow(relVarsNoPCA.dat), 2)
        mclogit.noPCA.mod = mclogit(lhsNoPCA~tuioutlist+finaidest2+gradrate+instperstudent2+selectdiffInt,data=relVarsNoPCA.dat, model = TRUE, )
        clogit.noPCA.mod = clogit(attendedIndicator~tuioutlist+finaidest2+gradrate+instperstudent2+selectdiffInt+strata(pubid_anon),relVarsNoPCA.dat)
          ll = clogit.noPCA.mod$loglik #first entry is intercept-only model, second is full model
          McFR2= 1-ll[2]/ll[1]
      #mclogit.B.B.A.C.mod
        lhsPCA = matrix(c(relVarsPCA.dat$attendedIndicator, relVarsPCA.dat$pubid_anon), nrow(relVarsPCA.dat), 2)
        mclogit.PCA.mod = mclogit(lhsPCA~tuioutlist+finaidest2+schoolqual+instperstudent2+selectdiffInt,data=relVarsPCA.dat, model = TRUE, )
        clogit.PCA.mod = clogit(attendedIndicator~tuioutlist+finaidest2+schoolqual+instperstudent2+selectdiffInt+strata(pubid_anon),relVarsPCA.dat)
          ll = clogit.PCA.mod$loglik #first entry is intercept-only model, second is full model
          McFR2= 1-ll[2]/ll[1]
      #mclogit.E.B.A.C.mod
        lhsPCA = matrix(c(relVarsPCA.dat$attendedIndicator, relVarsPCA.dat$pubid_anon), nrow(relVarsPCA.dat), 2)
        mclogit.PCA2.mod = mclogit(lhsPCA~tuioutlist+finaidwstatedisc+schoolqual+instperstudent2+selectdiffInt,data=relVarsPCA.dat, model = TRUE, )
        clogit.PCA2.mod = clogit(attendedIndicator~tuioutlist+finaidwstatedisc+schoolqual+instperstudent2+selectdiffInt+strata(pubid_anon),relVarsPCA.dat)
          ll = clogit.PCA2.mod$loglik #first entry is intercept-only model, second is full model
          McFR2= 1-ll[2]/ll[1]
      #mclogit.E.B.A.B.mod
        lhsNoPCA = matrix(c(relVarsNoPCA.dat$attendedIndicator, relVarsNoPCA.dat$pubid_anon), nrow(relVarsNoPCA.dat), 2)
        mclogit.noPCA2.mod = mclogit(lhsNoPCA~tuioutlist+finaidwstatedisc+gradrate+instperstudent2+selectdiffInt,data=relVarsNoPCA.dat, model = TRUE, )
        clogit.noPCA2.mod = clogit(attendedIndicator~tuioutlist+finaidwstatedisc+gradrate+instperstudent2+selectdiffInt+strata(pubid_anon),relVarsNoPCA.dat)
          ll = clogit.noPCA2.mod$loglik #first entry is intercept-only model, second is full model
          McFR2= 1-ll[2]/ll[1]

  #attempt E.B.A.B with interaction terms
    lhsNoPCA = matrix(c(relVarsNoPCA.dat$attendedIndicator, relVarsNoPCA.dat$pubid_anon), nrow(relVarsNoPCA.dat), 2)
    mclogit.noPCA2.mod = mclogit(lhsNoPCA~tuioutlist+finaidwstatedisc+gradrate+instperstudent2+selectdiffInt,data=relVarsNoPCA.dat, model = TRUE, )
    clogit.noPCA2.mod = clogit(attendedIndicator~tuioutlist+finaidwstatedisc+gradrate+instperstudent2+selectdiffInt+strata(pubid_anon),relVarsNoPCA.dat)
    ll = clogit.noPCA2.mod$loglik #first entry is intercept-only model, second is full model
    McFR2= 1-ll[2]/ll[1]
    #all
      mclogit.all.mod = mclogit(lhsNoPCA~tuioutlist+finaidwstatedisc+gradrate+instperstudent2+selectdiffInt + distance,data=relVarsNoPCA.dat, model = TRUE, )
      clogit.all.mod = clogit(attendedIndicator~distance + tuioutlist+finaidwstatedisc+gradrate+instperstudent2+selectdiffInt+strata(pubid_anon),relVarsNoPCA.dat)
        ll = clogit.all.mod$loglik #first entry is intercept-only model, second is full model
        McFR2= 1-ll[2]/ll[1]
    
    #add interactions
      #relVarsNoPCA.dat$value = relVarsNoPCA.dat$realtui2*relVarsNoPCA.dat$tuioutlist
      #relVarsNoPCA.dat$realtui = relVarsNoPCA.dat$tuioutlist - relVarsNoPCA.dat$finaidwstatedisc
      mclogit.noPCA2Int.mod = mclogit(lhsNoPCA~tuioutlist+finaidwstatedisc2+gradrate+instperstudent2+selectdiffInt,data=relVarsNoPCA.dat, model = TRUE, )
      clogit.noPCA2Int.mod = clogit(attendedIndicator~tuioutlist+finaidwstatedisc2+gradrate+instperstudent2+selectdiffInt+strata(pubid_anon),relVarsNoPCA.dat)
      #clogit.noPCA2Int.mod = clogit(attendedIndicator~listwfees+finaidwstatedisc+gradrate+instperstudent2+selectdiffInt+strata(pubid_anon),relVarsNoPCA.dat)
      ll = clogit.noPCA2Int.mod$loglik #first entry is intercept-only model, second is full model
        McFR2= 1-ll[2]/ll[1]
        save(mclogit.noPCA2Int.mod, file = "mclogit.noPCA2Int.saved")
        save(clogit.noPCA2Int.mod, file = "clogit.noPCA2Int.saved")
        save(relVarsNoPCA.dat, file = "stage1input.saved")
        save.image("finalmodels.RData")
    #same model with more fit and bad finaid
       mclogit.noPCA2Intwrong.mod = mclogit(lhsNoPCA~tuioutlist*finaidwstatedisc+tuioutlist+finaidwstatedisc+gradrate+instperstudent2+selectdiffInt,data=relVarsNoPCA.dat, model = TRUE, )
      clogit.noPCA2Intwrong.mod = clogit(attendedIndicator~tuioutlist*finaidwstatedisc+tuioutlist+finaidwstatedisc+gradrate+instperstudent2+selectdiffInt+strata(pubid_anon),relVarsNoPCA.dat)
      #clogit.noPCA2Int.mod = clogit(attendedIndicator~listwfees+finaidwstatedisc+gradrate+instperstudent2+selectdiffInt+strata(pubid_anon),relVarsNoPCA.dat)
        ll = clogit.noPCA2Intwrong.mod$loglik #first entry is intercept-only model, second is full model
        McFR2= 1-ll[2]/ll[1]
    #PCA model
      mclogit.PCA2Int.mod = mclogit(lhsPCA~tuioutlist+finaidwstatedisc2+schoolqual+instperstudent2+selectdiffInt,data=relVarsPCA.dat, model = TRUE, )
      clogit.PCA2Int.mod = clogit(attendedIndicator~tuioutlist+finaidwstatedisc2+schoolqual+instperstudent2+selectdiffInt+strata(pubid_anon),relVarsPCA.dat)
      ll = clogit.PCA2Int.mod$loglik #first entry is intercept-only model, second is full model
      McFR2= 1-ll[2]/ll[1]
    #model with good fit that is unfortunately nonsense
      #relVarsNoPCA.dat$goodVar = relVarsNoPCA.dat$tuioutlist-relVarsNoPCA.dat$realtui2
      #relVarsNoPCA.dat$realtui = relVarsNoPCA.dat$tuioutlist - relVarsNoPCA.dat$finaidwstatedisc
      mclogit.good.mod = mclogit(lhsNoPCA~goodVar2*tuioutlist +goodVar2+tuioutlist+gradrate+instperstudent2+selectdiffInt,data=relVarsNoPCA.dat, model = TRUE, )
      clogit.good.mod = clogit(attendedIndicator~goodVar2*tuioutlist +goodVar2+tuioutlist+gradrate+instperstudent2+selectdiffInt+strata(pubid_anon),relVarsNoPCA.dat)
      ll = clogit.good.mod$loglik #first entry is intercept-only model, second is full model
      McFR2= 1-ll[2]/ll[1]
    #plots to determine why good model is good
      #TBD
      plot(relVarsNoPCA.dat$goodVar2, relVarsNoPCA.dat$goodVar)
    #add higher order
      mclogit.noPCAHO.mod = mclogit(lhsNoPCA~finaidwstatedisc/tuioutlist+tuioutlist+finaidwstatedisc+gradrate+instperstudent2+selectdiffInt,data=relVarsNoPCA.dat, model = TRUE, )
      clogit.noPCA2HO.mod = clogit(attendedIndicator~finaidwstatedisc/tuioutlist+tuioutlist+finaidwstatedisc+gradrate+instperstudent2+selectdiffInt+strata(pubid_anon),relVarsNoPCA.dat)
      ll = clogit.noPCA2HO.mod$loglik #first entry is intercept-only model, second is full model
        McFR2= 1-ll[2]/ll[1]
  #check outliers on 0 side seen in plot
    #starting model is : mclogit.noPCA2Int.mod = mclogit(lhsNoPCA~tuioutlist+finaidwstatedisc2+gradrate+instperstudent2+selectdiffInt,data=relVarsNoPCA.dat, model = TRUE, )
    #predicted probabilities
      predPCA= predict(clogit.noPCA2Int.mod, type = "expected")
      myRes = predPCA-relVarsNoPCA.dat$attendedIndicator
      theirRes = residuals(mclogit.noPCA2Int.mod)
      outData = data.frame(myRes = myRes, theirRes = theirRes, id = relVarsNoPCA.dat$pubid_anon, attend = relVarsNoPCA.dat$attendedIndicator)
      #write.csv(outData, "C:/Users/Katharina/Documents/Umich/lifecycle choice/data/test.csv")
    noOuts.dat = relVarsNoPCA.dat[abs(myRes) <.6,]  
    lhsNoOuts = matrix(c(noOuts.dat$attendedIndicator, noOuts.dat$pubid_anon), nrow(noOuts.dat), 2)
    mclogit.noOuts.mod = mclogit(lhsNoOuts~tuioutlist+finaidwstatedisc+gradrate+instperstudent2+selectdiffInt,data=noOuts.dat, model = TRUE, )
    clogit.noOuts.mod = clogit(attendedIndicator~tuioutlist+finaidwstatedisc+gradrate+instperstudent2+selectdiffInt+strata(pubid_anon),noOuts.dat)
      ll = clogit.noOuts.mod$loglik #first entry is intercept-only model, second is full model
      McFR2= 1-ll[2]/ll[1]
    #influence plot
      influencePlot(mclogit.noPCA2Int.mod,   id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )
#Dfbetas = change in the estimated coefficients divided by its SE. 
#Dffits = change in the fitted value divided by its SE. 
#Cook's distance = standardized sum of squares of change in all fitted values. 
#covratio = change in covariance matrix of the estimates. 
#Most of these can be obtained in R using influence.measures(fit), where fit contains the glm fit.

#model diagnostics=====================================================================
  clogit.noPCA.mod = clogit.noPCA2Int.mod
  mclogit.noPCA.mod = mclogit.noPCA2Int.mod
  #R2's already calculated
  #AIC
    extractAIC(clogit.PCA.mod) #returns equivalent dof, then AIC
    extractAIC(clogit.noPCA.mod)
  #count of cases classified correctly is not relevant when data is not binary
  #predicted probabilities
    predPCA= predict(clogit.PCA.mod, type = "expected") #this should maybe be lp or risk type, TBD
predPCA= predict(clogit.PCA.mod, type = "expected")
  #residuals
    resPCA=residuals(clogit.PCA.mod)
    resNoPCA=residuals(clogit.noPCA.mod)
    #note residual types are "martingale", "deviance", "score", "schoenfeld", "dfbeta", "dfbetas", "scaledsch", "partial"
    devresPCA = residuals(clogit.PCA.mod, "deviance")
    devresNoPCA = residuals(clogit.noPCA.mod, "deviance")
    partresPCA = residuals(clogit.PCA.mod, "partial")
    partresNoPCA = residuals(clogit.noPCA.mod, "partial")
  #plot predicted probabilities versus attendance
    ggplot(data = relVarsPCA.dat, aes(x=attendedIndicator, y=predPCA))+geom_point() + theme_bw() + labs(title="Predicted probability by attendance group, PCA model")+ xlab("Attended indicator") + ylab("Predicted probability") 
    ggplot(relVarsPCA.dat, aes(factor(attendedIndicator), predPCA))+geom_boxplot()+ theme_bw() + labs(title="Predicted probability by attendance group, PCA model")+ xlab("Attended indicator") + ylab("Predicted probability") 
    ggplot(data = relVarsNoPCA.dat, aes(x=attendedIndicator, y=predNoPCA))+geom_point() + theme_bw() + labs(title="Predicted probability by attendance group, no PCA model")+ xlab("Attended indicator") + ylab("Predicted probability") 
    ggplot(relVarsNoPCA.dat, aes(factor(attendedIndicator), predNoPCA))+geom_boxplot()+ theme_bw() + labs(title="Predicted probability by attendance group, no PCA model")+ xlab("Attended indicator") + ylab("Predicted probability") 
  #plot residuals versus index
    #partial residuals
      index = c(1:nrow(partresNoPCA))
      for (i in 1:ncol(partresNoPCA)){
        curPlot = ggplot() + geom_point(aes(x= index, y = partresNoPCA[,i]))+ theme_bw() + labs(title=paste("Partial residuals by index ",colnames(partresNoPCA)[i],", no PCA model", sep = ""))+ xlab("Index") + ylab("Partial residuals") 
        ggsave(paste("C:/Users/Katharina/Documents/Umich/lifecycle choice/data/plots/partresVindex", colnames(partresNoPCA)[i] , ".pdf", sep = ""))
      }
      index = c(1:nrow(partresPCA))
      for (i in 1:ncol(partresPCA)){
        curPlot = ggplot() + geom_point(aes(x= index, y = partresPCA[,i]))+ theme_bw() + labs(title=paste("Partial residuals by index ",colnames(partresPCA)[i],", PCA model", sep = ""))+ xlab("Index") + ylab("Partial residuals") 
        ggsave(paste("C:/Users/Katharina/Documents/Umich/lifecycle choice/data/plots/partresVindexPCA", colnames(partresPCA)[i] , ".pdf", sep = ""))
      }
    #deviance residuals
      index = c(1:nrow(partresNoPCA))
      curPlot = ggplot() + geom_point(aes(x= index, y = devresNoPCA))+ theme_bw() + labs(title="Deviance residuals by index")+ xlab("Index") + ylab("Deviance residuals") 
      index = c(1:nrow(partresPCA))    
      curPlot = ggplot() + geom_point(aes(x= index, y = devresPCA))+ theme_bw() + labs(title="Deviance residuals by index")+ xlab("Index") + ylab("Deviance residuals") 
      
  #plot residuals versus fitted values
    #partial residuals
      for (i in 1:ncol(partresNoPCA)){
        curPlot = ggplot() + geom_point(aes(x= predNoPCA, y = partresNoPCA[,i]))+ theme_bw() + labs(title=paste("Partial residuals by predicted probabilities ",colnames(partresNoPCA)[i],", no PCA model", sep = ""))+ xlab("Predicted probability") + ylab("Partial residuals") 
        ggsave(paste("C:/Users/Katharina/Documents/Umich/lifecycle choice/data/plots/partresVfit", colnames(partresNoPCA)[i] , ".pdf", sep = ""))
      }
      for (i in 1:ncol(partresPCA)){
        curPlot = ggplot() + geom_point(aes(x= predPCA, y = partresPCA[,i]))+ theme_bw() + labs(title=paste("Partial residuals by predicted probabilities ",colnames(partresPCA)[i],", PCA model", sep = ""))+ xlab("Predicted probability") + ylab("Partial residuals") 
        ggsave(paste("C:/Users/Katharina/Documents/Umich/lifecycle choice/data/plots/partresVfitnoPCA", colnames(partresPCA)[i] , ".pdf", sep = ""))
      }
    #deviance residuals
      curPlot = ggplot() + geom_point(aes(x= predNoPCA, y = devresNoPCA))+ theme_bw() + labs(title="Deviance residuals by predicated probability")+ xlab("Predicted proability") + ylab("Deviance residuals") 
      curPlot = ggplot() + geom_point(aes(x= predPCA, y = devresPCA))+ theme_bw() + labs(title="Deviance residuals by predicated probability")+ xlab("Predicted proability") + ylab("Deviance residuals") 
      

     #partial residuals versus attended indicator

                       
                    
    
#BTL model=================================================================
    clogit.btl.mod = clogit(attendedIndicator~realtui2+I(realtui2^2)+distance+I(distance^2)+instperstudent2+I(instperstudent2^2)+avgsal+selectdiffInt+ strata(pubid_anon), noNARed.dat)
  
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

#other predictors for testing        
#testProbslp= predict(clogit2.mod, type = "lp") #should be equivalent in normal data
#testProbsrisk= predict(clogit2.mod, type = "risk")
#testProbsexpected= predict(clogit2.mod, type = "expected")
#testProbsterms= predict(clogit2.mod, type = "expected")
#clogit2.mod = clogit(attendedIndicator~tuioutlist+finaidwstatedisc+gradrate+instperstudent2+selectdiffInt+strata(pubid_anon),relVarsNoPCA.dat)
#mclogit2.mod = mclogit(lhsNoPCA~tuioutlist+finaidwstatedisc+gradrate+instperstudent2+selectdiffInt,data=relVarsNoPCA.dat, model = TRUE, )
#test = relVarsNoPCA.dat
#predProbs= predict(mclogit.mod, newdata = test, type = "link") #gives linear predictor as expected
#test$predProbs = exp(predProbs)
#sums.dat = ddply(test,~pubid_anon,summarise,probSum=sum(predProbs))
#test = merge(x = test, y = sums.dat, by = "pubid_anon", all.x = TRUE)
#test$prob = test$predProbs/test$probSum
#model============================================================================

#outliers
for (i in 1:ncol(relVarsNoPCA.dat)){
  myPlot = ggplot(data=relVarsNoPCA.dat, aes(x=attendedIndicator, y=relVarsNoPCA.dat[,i])) + geom_point()+ labs(title=colnames(relVarsNoPCA.dat)[i])
  ggsave(paste("C:/Users/Katharina/Documents/Umich/lifecycle choice/data/plots/check", i , ".pdf", sep = ""))
}
