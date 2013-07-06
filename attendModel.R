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
  #library(glm)
  library(mclogit)
  #library(safeBinaryRegression)
  library(car)
  library(survival)

#clear workspace ===================================================================
  rm(list = ls())
  load("finalmodels.RData")

#data i/o ==========================================================================
  attend.dat = read.csv("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Choice Model Inputs/anon_attend.csv", stringsAsFactors=FALSE)
    #this is out.df from choiceModel.R with some variable choice modifications, anonimized
    #keep only relevant columns
      attend.dat = attend.dat[,c("pubid_anon","school_anon","Admit","Attend","b0","KEYSEX_1997","KEYRACE_ETHNICITY_1997","SAT_MATH","SAT_VERBAL","MAJOR2","DAD_ED","MOM_ED","HH_SIZE","HH_INCOME","URBAN_RURAL","SCHOOL_TYPE","AttendedIndicator","AIDALLSCHOOL","SATDiff","tuiin","tuiout","tuiinlist","sat25","sat75","urbanrural","lifeEarnings")]
  merge.dat = read.csv("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Choice Model Inputs/anon_dat_mergedata2.csv")
  #merge
    attend.dat$id = paste(attend.dat$pubid_anon, attend.dat$school_anon)
    merge.dat$id = paste(merge.dat$pubid_anon, merge.dat$school_anon)
    merge.dat=merge.dat[c("id","attendedIndicator","tuioutlist","realtui2","finaidest2","distance","instate","urbanruralmatch","loanp","fedgrantp","control","carnegie2","avgsal","division2","gradrate","expperstudent2","instperstudent2","facperstudent2","genderratio2","totstudents2","nonAttendAid","realtuiApply","selectdiffInt","selectInt","finaidwstatedisc","finaidwstatedisc2")]
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
    #those with only one school
      nonattendone.dat = nonattenders.dat[nonattenders.dat$Frequency==1,]
      nonattendone.dat=nonattendone.dat[,c(1:52)]
      #replace the corresponding rows in model.dat
      for (i in 1:nrow(nonattendone.dat)){
        curID = nonattendone.dat$pubid_anon[i]
        replaceVect = unname(unlist(nonattendone.dat[i,]))
        model.dat[model.dat$pubid_anon==curID,]=replaceVect 
      }
    #those with multiple schools
      nonattendmulti.dat = nonattenders.dat[nonattenders.dat$Frequency>1,]
      #predict.dat = nonattendmulti.dat[,c("pubid_anon", "tuioutlist", "finaidwstatedisc2", "gradrate", "instperstudent2", "selectdiffInt")]
      predict.dat = nonattendmulti.dat
      predict.dat = predict.dat[predict.dat$pubid_anon != 43204 | predict.dat$tuioutlist != 15847,] #drop bad observation
      #predict.dat = na.exclude(predict.dat) #this drops one nonsensical observation with ID 43204
        #TBD fill missing      
      #get models
        mclogit.mod = mclogit.noPCA2Int.mod
        clogit.mod = clogit.noPCA2Int.mod
      #predict
        predProbs= predict(mclogit.mod, newdata = predict.dat, type = "link") #gives linear predictor as expected
        predict.dat$predProbs= exp(predProbs)
      #calculate probability of attending each for checks
        #these are close to the "expected" type of probability when possible
        sums.dat = ddply(predict.dat,~pubid_anon,summarise,probSum=sum(predProbs))
        predict.dat = merge(x = predict.dat, y = sums.dat, by = "pubid_anon", all.x = TRUE)
        predict.dat$prob = predict.dat$predProbs/predict.dat$probSum
      #select max probability row
        bestSchool.dat = ddply(predict.dat, .(pubid_anon), function(subdf) {subdf[which.max(subdf$prob),]})
        bestSchool.dat = bestSchool.dat[,c(1:52)]
      #replace the corresponding rows in model.dat
        for (i in 1:nrow(bestSchool.dat)){
          curID = bestSchool.dat$pubid_anon[i]
          replaceVect = unname(unlist(bestSchool.dat[i,]))
          model.dat[model.dat$pubid_anon==curID,]=replaceVect 
        }
    #write.csv(model.dat,"C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Choice Model Inputs/model.dat.csv" )

#fill candidate variables
  #throw out very high missingness variables
    drops = c("SAT_MATH", "SAT_VERBAL", "MAJOR2", "SATDiff", "sat25", "sat75", "DAD_ED", "urbanrural", "AttendedIndicator")
    model.dat = model.dat[!colnames(model.dat) %in% drops]
  #variables that have some missingness: household income, school type, momed, distance, gradrate, experstudent, instperstudent, facperstudent, genderration, totstudents

#set factor levels
  #set categorical variables since this information may have gotten lost
  model.dat$instate = as.factor(model.dat$instate)
  model.dat$urbanruralmatch = as.factor(model.dat$urbanruralmatch)
  model.dat$control = as.factor(model.dat$control)
  model.dat$carnegie2 = as.factor(model.dat$carnegie2)
  model.dat$division2 = as.factor(model.dat$division2)
  model.dat$Attend = as.factor(model.dat$Attend)
  model.dat$Admit = as.factor(model.dat$Admit)
  model.dat$KEYSEX_1997 = as.factor(model.dat$KEYSEX_1997)
  model.dat$KEYRACE_ETHNICITY_1997 = as.factor(model.dat$KEYRACE_ETHNICITY_1997)
  model.dat$URBAN_RURAL= as.factor(model.dat$URBAN_RURAL)
  model.dat$SCHOOL_TYPE= as.factor(model.dat$SCHOOL_TYPE)

#rough models==============================================================================
  input.dat = na.exclude(model.dat)
  lapply(input.dat, class) 
  #TBD all my variables are characters for some reason, find where this problem occurs
  #glm
    first.mod <- glm(attendedIndicator ~ tuioutlist+finaidwstatedisc2+gradrate+instperstudent2+selectdiffInt+KEYSEX_1997+b0+KEYRACE_ETHNICITY_1997+MOM_ED+HH_SIZE+HH_INCOME+SCHOOL_TYPE+URBAN_RURAL+lifeEarnings+distance+loanp+fedgrantp+control+division2+genderratio2+totstudents2+selectInt, data = input.dat, family = "binomial")
    main.mod <- glm(attendedIndicator ~ b0+ tuioutlist+finaidwstatedisc2+gradrate+instperstudent2+selectdiffInt+KEYSEX_1997+b0+KEYRACE_ETHNICITY_1997+HH_INCOME+SCHOOL_TYPE+lifeEarnings+selectInt, data = input.dat, family = "binomial")
  input.dat = mlogit.data(input.dat, shape = "wide", choice = "attendIndicator")


#outlier testing and data cleaning=========================================================================

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
