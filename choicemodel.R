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
  #fix financial aid variables
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
            #print(paste(anon.dat$nonAttendMiss[i], anon.dat$attendAid[i], sep = ","))
          }else{
            anon.dat$nonAttendMiss[i]= 0
          }
        }
      }
      anon.dat$finaidest2 = anon.dat$nonAttendMiss
    #adjust both net tuition variables as necessary
      anon.dat$costbeforeaid = anon.dat$realtui + anon.dat$finaidest
      anon.dat$realtui2 = anon.dat$costbeforeaid- anon.dat$finaidest2
      anon.dat$realtuiApply = anon.dat$costbeforeaid-anon.dat$nonAttendAid
  #replace negative tuition values with 0?
    #not for now!
  #get attenders
    attenders.dat = anon.dat[anon.dat$attend != -10,]
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
  #create new selectivity variables
    multi.dat$selectdiffInt = NA
    multi.dat$selectInt = NA
    lookup = data.frame(id = c(1,2,3,4,5), percent = c(33, 60, 75, 85, 100))
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
    relVars.dat = multi.dat[,c("attendedIndicator","tuioutlist","realtui2","finaidest2","distance","instate","urbanruralmatch","loanp","fedgrantp","control","carnegie2","avgsal","division2","gradrate","expperstudent2","instperstudent2","facperstudent2","genderratio2","totstudents2","nonAttendAid","realtuiApply", "selectdiffInt","selectInt")]
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
  #testing with simple models--TBD starts here!
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
        mclogit.all.mod = mclogit(lhs~tuioutlist+realtui+finaidest+distance+instate+urbanruralmatch+selectdiffInt+loanp+fedgrantp+control+carnegie2+selectInt+avgsal+division2+gradrate+expperstudent2+instperstudent2+facperstudent2+genderratio2+totstudents2+nonAttendAid,data=noNA.dat, model = TRUE, )
          #still singular
      #based on choices in excel
        mclogit.A.A.mod = mclogit(lhs~realtui+distance+instate+urbanruralmatch+loanp+fedgrantp+control+carnegie2+selectInt+avgsal+division2+gradrate+expperstudent2+instperstudent2+facperstudent2+genderratio2+totstudents2,data=noNA.dat, model = TRUE, )
        mclogit.B.A.mod = mclogit(lhs~tuioutlist+finaidest+distance+instate+urbanruralmatch+loanp+fedgrantp+control+carnegie2+selectInt+avgsal+division2+gradrate+expperstudent2+instperstudent2+facperstudent2+genderratio2+totstudents2,data=noNA.dat, model = TRUE, )
        mclogit.C.A.mod = mclogit(lhs~tuioutlist+nonAttendAid+distance+instate+urbanruralmatch+loanp+fedgrantp+control+carnegie2+selectInt+avgsal+division2+gradrate+expperstudent2+instperstudent2+facperstudent2+genderratio2+totstudents2,data=noNA.dat, model = TRUE, )
        mclogit.A.B.mod = mclogit(lhs~realtui+distance+instate+urbanruralmatch+loanp+fedgrantp+control+carnegie2+selectdiffInt+avgsal+division2+gradrate+expperstudent2+instperstudent2+facperstudent2+genderratio2+totstudents2,data=noNA.dat, model = TRUE, )
        mclogit.B.B.mod = mclogit(lhs~tuioutlist+finaidest+distance+instate+urbanruralmatch+loanp+fedgrantp+control+carnegie2+selectdiffInt+avgsal+division2+gradrate+expperstudent2+instperstudent2+facperstudent2+genderratio2+totstudents2,data=noNA.dat, model = TRUE, )
        mclogit.C.B.mod = mclogit(lhs~tuioutlist+nonAttendAid+distance+instate+urbanruralmatch+loanp+fedgrantp+control+carnegie2+selectdiffInt+avgsal+division2+gradrate+expperstudent2+instperstudent2+facperstudent2+genderratio2+totstudents2,data=noNA.dat, model = TRUE, )
      #remove consistently insignificant variables
        mclogit.A.A.mod = mclogit(lhs~realtui+loanp+fedgrantp+carnegie2+selectInt+avgsal+division2+gradrate+expperstudent2+instperstudent2+facperstudent2+genderratio2+totstudents2,data=noNA.dat, model = TRUE, )
        mclogit.B.A.mod = mclogit(lhs~tuioutlist+finaidest+loanp+fedgrantp+carnegie2+selectInt+avgsal+division2+gradrate+expperstudent2+instperstudent2+facperstudent2+genderratio2+totstudents2,data=noNA.dat, model = TRUE, )
        mclogit.C.A.mod = mclogit(lhs~tuioutlist+nonAttendAid+loanp+fedgrantp+carnegie2+selectInt+avgsal+division2+gradrate+expperstudent2+instperstudent2+facperstudent2+genderratio2+totstudents2,data=noNA.dat, model = TRUE, )
        mclogit.A.B.mod = mclogit(lhs~realtui+loanp+fedgrantp+carnegie2+selectdiffInt+avgsal+division2+gradrate+expperstudent2+instperstudent2+facperstudent2+genderratio2+totstudents2,data=noNA.dat, model = TRUE, )
        mclogit.B.B.mod = mclogit(lhs~tuioutlist+finaidest+loanp+fedgrantp+carnegie2+selectdiffInt+avgsal+division2+gradrate+expperstudent2+instperstudent2+facperstudent2+genderratio2+totstudents2,data=noNA.dat, model = TRUE, )
        mclogit.C.B.mod = mclogit(lhs~tuioutlist+nonAttendAid+loanp+fedgrantp+carnegie2+selectdiffInt+avgsal+division2+gradrate+expperstudent2+instperstudent2+facperstudent2+genderratio2+totstudents2,data=noNA.dat, model = TRUE, )
      #model select each one
        mclogit.A.A.mod = mclogit(lhs~realtui+fedgrantp+carnegie2+selectInt+avgsal+gradrate+expperstudent2+instperstudent2+facperstudent2,data=noNA.dat, model = TRUE, )
        mclogit.B.A.mod = mclogit(lhs~tuioutlist+finaidest+loanp+fedgrantp+carnegie2+selectInt+avgsal+division2+gradrate+expperstudent2+instperstudent2+facperstudent2+genderratio2+totstudents2,data=noNA.dat, model = TRUE, )
        mclogit.C.A.mod = mclogit(lhs~tuioutlist+nonAttendAid+loanp+fedgrantp+carnegie2+selectInt+avgsal+division2+gradrate+expperstudent2+instperstudent2+facperstudent2+genderratio2+totstudents2,data=noNA.dat, model = TRUE, )
        mclogit.A.B.mod = mclogit(lhs~realtui+loanp+fedgrantp+carnegie2+selectdiffInt+avgsal+division2+gradrate+expperstudent2+instperstudent2+facperstudent2+genderratio2+totstudents2,data=noNA.dat, model = TRUE, )
        mclogit.B.B.mod = mclogit(lhs~tuioutlist+finaidest+loanp+fedgrantp+carnegie2+selectdiffInt+avgsal+division2+gradrate+expperstudent2+instperstudent2+facperstudent2+genderratio2+totstudents2,data=noNA.dat, model = TRUE, )
        mclogit.C.B.mod = mclogit(lhs~tuioutlist+nonAttendAid+loanp+fedgrantp+carnegie2+selectdiffInt+avgsal+division2+gradrate+expperstudent2+instperstudent2+facperstudent2+genderratio2+totstudents2,data=noNA.dat, model = TRUE, )

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

