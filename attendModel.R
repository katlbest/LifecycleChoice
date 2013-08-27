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
  library(pscl)
  library(gridExtra)

#clear workspace ===================================================================
  rm(list = ls())
  load("inputs/finalmodels.RData")

#data i/o ==========================================================================
  attend.dat = read.csv("inputs/anon_attend.csv", stringsAsFactors=FALSE)
    #this is out.df from choiceModel.R with some variable choice modifications, anonimized
    #keep only relevant columns
      attend.dat = attend.dat[,c("pubid_anon","school_anon","Admit","Attend","b0","KEYSEX_1997","KEYRACE_ETHNICITY_1997","SAT_MATH","SAT_VERBAL","MAJOR2","DAD_ED","MOM_ED","HH_SIZE","HH_INCOME","URBAN_RURAL","SCHOOL_TYPE","AttendedIndicator","AIDALLSCHOOL","SATDiff","tuiin","tuiout","tuiinlist","sat25","sat75","urbanrural","lifeEarnings")]
  merge.dat = read.csv("inputs/anon_dat_mergedata2.csv")
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
        for (j in 1:length(nonattendone.dat[i,])){
          model.dat[model.dat$pubid_anon==curID,j]= nonattendone.dat[i,j]
        }
      }
    #those with multiple schools
      nonattendmulti.dat = nonattenders.dat[nonattenders.dat$Frequency>1,]
      #predict.dat = nonattendmulti.dat[,c("pubid_anon", "tuioutlist", "finaidwstatedisc2", "gradrate", "instperstudent2", "selectdiffInt")]
      predict.dat = nonattendmulti.dat
      predict.dat = predict.dat[predict.dat$pubid_anon != 43204 | predict.dat$tuioutlist != 15847,] #drop bad observation
      #predict.dat = na.exclude(predict.dat) #this drops one nonsensical observation with ID 43204     
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
          for (j in 1:length(bestSchool.dat[i,])){
            model.dat[model.dat$pubid_anon==curID,j]= bestSchool.dat[i,j]
          }
        }
    #write.csv(model.dat,"C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Choice Model Inputs/model.dat.csv" )

  #fill candidate variables
    #throw out very high missingness variables
      drops = c("SAT_MATH", "SAT_VERBAL", "MAJOR2", "SATDiff", "sat25", "sat75", "DAD_ED", "urbanrural", "AttendedIndicator")
      model.dat = model.dat[!colnames(model.dat) %in% drops]
    #variables that have some missingness: household income, school type, momed, distance, gradrate, experstudent, instperstudent, facperstudent, genderration, totstudents
  
  #check collinearity
    #mostVars = KEYSEX_1997+KEYRACE_ETHNICITY_1997+MOM_ED+HH_SIZE+HH_INCOME+URBAN_RURAL+SCHOOL_TYPE+earnDiff+tuioutlist+distance+gradrate+instperstudent2+totstudents2+selectdiffInt+selectInt+finaidwstatedisc2
    #keyVars = KEYSEX_1997+KEYRACE_ETHNICITY_1997+MOM_ED+HH_INCOME+SCHOOL_TYPE+earnDiff+tuioutlist+gradrate+instperstudent2+selectdiffInt+selectInt+finaidwstatedisc2
    #pairs(~KEYSEX_1997+KEYRACE_ETHNICITY_1997+MOM_ED+HH_INCOME+SCHOOL_TYPE+earnDiff+tuioutlist+gradrate+instperstudent2+selectdiffInt+selectInt+finaidwstatedisc2, data=na.exclude(model.dat))
    #subSet= model.dat[,c("KEYSEX_1997","KEYRACE_ETHNICITY_1997","MOM_ED","HH_SIZE","HH_INCOME","URBAN_RURAL","SCHOOL_TYPE","earnDiff","tuioutlist","distance","gradrate","instperstudent2","totstudents2","selectdiffInt","selectInt","finaidwstatedisc2")]
    #write.csv(cor(na.exclude(subSet)), "C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/choicemodel/2stginputcorr.csv")

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
  
  #fix lifetime earnings
    #b0NoAttendVect = c(-192515.8851,-140025.3721,-182087.547,-171195.9979) #b0 coefficients for categories 1,2,3,4,5 if attened
    #b0AttendVect = c(-276972.9751,-281219.0721,-230315.227,-214659.9479) #b0 coefficients for categories 1,2,3,4,5 if not attended
    b0AttendVect= c(-279411.3, -279411.3,-217284.7, -217284.7)
    b0NoAttendVect = rep(-177856.4, 4)
    attendEarn = rep(NA, 4)
    nonAttendEarn = rep(NA,4)
    earnDiff = c(rep(NA, 4), 0) #the difference for the worst attendance category is 0
    tau =27.8818
    m = -3.8149
    b = 36241
    n =  -0.2445
    a = 2234.3
    source("fun_getNS.R")
    for (i in 1:4){
      attendEarn[i] = getNS(tau, m, b, n, a, b0AttendVect[i], 1) #earnings start at age 22
      nonAttendEarn[i] = getNS(tau, m, b, n, a,b0NoAttendVect[i], 0) #earnings start at age 18
      earnDiff[i]=attendEarn[i]-nonAttendEarn[i]
    }
    model.dat$earnDiff = NA
    for (i in 1:nrow(model.dat)){
      model.dat$earnDiff[i]=earnDiff[model.dat$Admit[i]]
    }

#rough models==============================================================================
  input.dat = na.exclude(model.dat)
  #add school value variable
  predSchoolVal= predict(mclogit.mod, newdata = input.dat, type = "link") #gives linear predictor as expected
  input.dat$predSchoolVal= exp(predSchoolVal)
  #lapply(input.dat, class) 
  
  #glm
    first.mod = glm(attendedIndicator ~ tuioutlist+finaidwstatedisc2+gradrate+instperstudent2+selectdiffInt+KEYSEX_1997+KEYRACE_ETHNICITY_1997+MOM_ED+HH_SIZE+HH_INCOME+SCHOOL_TYPE+URBAN_RURAL+earnDiff+distance+loanp+fedgrantp+control+division2+genderratio2+totstudents2+selectInt, data = input.dat, family = "binomial")
    main.mod = glm(attendedIndicator ~ tuioutlist+finaidwstatedisc2+gradrate+instperstudent2+selectdiffInt+KEYSEX_1997+KEYRACE_ETHNICITY_1997+HH_INCOME+SCHOOL_TYPE+earnDiff+selectInt, data = input.dat, family = "binomial")
    starting.mod = glm(attendedIndicator ~ tuioutlist+finaidwstatedisc2+gradrate+instperstudent2+selectdiffInt+KEYSEX_1997+KEYRACE_ETHNICITY_1997+MOM_ED+HH_INCOME+SCHOOL_TYPE+earnDiff, data = input.dat, family = "binomial")

    stepwise.mod = glm(attendedIndicator ~ gradrate+earnDiff, data = input.dat, family = "binomial")
      pR2(stepwise.mod)
    with(stepwise.mod, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
      #significantly better fit than null model
  #keep relevant and reestimate
    #input.dat = model.dat[,c("gradrate", "earnDiff","KEYSEX_1997", "attendedIndicator","tuioutlist", "finaidwstatedisc2","instperstudent2","selectdiffInt")]
    input.dat = model.dat[,c("gradrate", "earnDiff","KEYSEX_1997", "attendedIndicator", "b0")]
    input.dat = na.exclude(input.dat)
    #predSchoolVal= predict(mclogit.mod, newdata = input.dat, type = "link") #gives linear predictor as expected
    #input.dat$predSchoolVal= exp(predSchoolVal)
    reduced.mod =glm(attendedIndicator ~ KEYSEX_1997+earnDiff+ gradrate, data = input.dat, family = "binomial")
    #reduced.mod =glm(attendedIndicator ~ KEYSEX_1997+earnDiff+predSchoolVal, data = input.dat, family = "binomial")
      pR2(reduced.mod)
  #BTL
    input.dat = model.dat[,c("attendedIndicator","tuioutlist", "distance","instperstudent2")]
    input.dat = na.exclude(input.dat)
    btl.mod =glm(attendedIndicator ~ tuioutlist+I(tuioutlist^2)+distance+I(distance^2)+instperstudent2+I(instperstudent2^2), data = input.dat, family = "binomial")
      pR2(btl.mod)
      coefs = btl.mod$coefficients
      coefs = exp(coefs)

#outlier testing and data cleaning=========================================================================
  for (i in 1:ncol(input.dat)){
    myPlot = ggplot(data=input.dat, aes(x=attendedIndicator, y=input.dat[,i])) + geom_point()+ labs(title=colnames(input.dat)[i])
    ggsave(paste("C:/Users/Katharina/Documents/Umich/lifecycle choice/data/plots/secondstageoutliers", i , ".pdf", sep = ""))
  }
  #no severe issues detected

#diagnostics=============================================================================================
  #aic
    extractAIC(reduced.mod) #returns equivalent dof, then AIC
  #count of cases classified correctly is not relevant when data is not binary
    predProb= predict(reduced.mod, type = "response")
    ggplot(input.dat, aes(factor(attendedIndicator), predProb))+geom_boxplot()+ theme_bw() + labs(title="Predicted probability by attendance group, PCA model")+ xlab("Attended indicator") + ylab("Predicted probability")
    percentile = quantile(predProb, c(.5))
    input.dat$predAtt = 1
    input.dat[predProb <.6,]$predAtt = 0
    sum<-ddply(input.dat,.(attendedIndicator, predAtt),summarize,freq = length(attendedIndicator))
  #residuals
    devRes=residuals(reduced.mod) #deviance
    pearRes=residuals(reduced.mod, type = "pearson") #pearson
    partRes=residuals(reduced.mod, type = "partial") #partial
  #plot residuals versus index
    #partial residuals
      index = c(1:nrow(partRes))
      for (i in 1:ncol(partRes)){
        curPlot = ggplot() + geom_point(aes(x= index, y = partRes[,i]))+ theme_bw() + labs(title=paste("Partial residuals by index ",colnames(partRes)[i], sep = ""))+ xlab("Index") + ylab("Partial residuals") 
        ggsave(paste("C:/Users/Katharina/Documents/Umich/lifecycle choice/data/plots/partRessecondstage", colnames(partRes)[i] , ".pdf", sep = ""))
      }
    #deviance residuals
      index = c(1:length(devRes))
      curPlot = ggplot() + geom_point(aes(x= index, y = devRes))+ theme_bw() + labs(title="Deviance residuals by index")+ xlab("Index") + ylab("Deviance residuals") 
  #plot residuals versus index
    #partial residuals
      for (i in 1:ncol(partRes)){
        curPlot = ggplot() + geom_point(aes(x= predProb, y = partRes[,i]))+ theme_bw() + labs(title=paste("Partial residuals by predicted probability ",colnames(partRes)[i], sep = ""))+ xlab("Predicted proability") + ylab("Partial residuals") 
        ggsave(paste("C:/Users/Katharina/Documents/Umich/lifecycle choice/data/plots/partRessecondstagebyProb", colnames(partRes)[i] , ".pdf", sep = ""))
      }
    #deviance residuals
      curPlot = ggplot() + geom_point(aes(x= predProb, y = devRes))+ theme_bw() + labs(title="Deviance residuals by predicted probability")+ xlab("Predicted probability") + ylab("Deviance residuals") 

#showing discrepancy=========================================================================================================
  #plot people's probability versus b0
    pred.dat = model.dat[,c("Admit", "gradrate", "earnDiff","KEYSEX_1997", "attendedIndicator", "lifeEarnings", "b0")]
    pred.dat=na.exclude(pred.dat) #only 1 deleted
    predProbAll = predict(reduced.mod, newData = pred.dat, type = "response")
    pred.dat$predicted = predProbAll
    curPlot = ggplot() + geom_point(data = pred.dat,aes(x= predicted, y =lifeEarnings))+ theme_bw() + labs(title="Lifetime earnings by attendance probability")+ xlab("Predicted probability") + ylab("Lifetime earnings") #+ geom_smooth(method = lm, se = FALSE)
    #does predicted prob of attendance predict earnings?
      predProb.mod = lm(lifeEarnings~predicted, data = pred.dat)
  #by group
    curPlot = ggplot() + geom_point(data = pred.dat,aes(x= predicted, y =lifeEarnings,color=factor(attendedIndicator)))+ theme_bw() + labs(title="Lifetime earnings by attendance probability")+ xlab("Predicted probability") + ylab("Lifetime earnings") #+ geom_smooth(method = lm, se = FALSE)
  #by top admitted group  
    datList = list()
    for (i in levels(as.factor(pred.dat$Admit))){
      datList[[i]]= pred.dat[pred.dat$Admit ==i,]
    }
    for (i in levels(as.factor(pred.dat$Admit))){
      plotName = paste("p", i, sep = "")
      curPlot = ggplot() + geom_point(data = datList[[i]],aes(x= predicted, y =lifeEarnings,color=factor(attendedIndicator)))+ theme_bw() + labs(title=paste("Lifetime earnings by attendance probability, Admit = ", i, sep = ""))+ xlab("Predicted probability") + ylab("Lifetime earnings")+theme(axis.text.x=element_text(size=8) ,  axis.title.x = element_text(size = 8), axis.text.y=element_text(size=8) ,  axis.title.y = element_text(size = 8), legend.text = element_text(size = 8), title= element_text(size = 10), legend.position ="none")
      assign(plotName,curPlot)
    }
  #regression
    attendEffect.mod = lm(lifeEarnings~predicted+attendedIndicator,data = pred.dat)
    for (i in levels(as.factor(pred.dat$Admit))){
      modName = paste("group",i, ".mod", sep = "")
      curMod = lm(lifeEarnings~predicted+attendedIndicator,data = datList[[i]])
      assign(modName, curMod)
    }
  #ANOVA by selecting marginal
    marginal.dat = pred.dat[(pred.dat$predicted > .4 & pred.dat$predicted < .7),]
      #this is only 36 people
    summary(aov(lifeEarnings~attendedIndicator, data = marginal.dat))

#by group==============================================================================================
  cat.dat = pred.dat
  cat.dat$priorCat = NA
  cat.dat$predicted = predProb
  cat.dat[cat.dat$predicted>.9,]$priorCat = 1
  cat.dat[cat.dat$predicted<.6,]$priorCat = 0
  cat.dat = na.exclude(cat.dat)
  summary(aov(lifeEarnings~factor(priorCat), data = cat.dat))
  ggplot(cat.dat, aes(factor(priorCat), lifeEarnings))+geom_boxplot()+ theme_bw() + labs(title="Lifetime earnings by attendance likelihood")+ xlab("Attendance likelihood") + ylab("Lifetime earnings")
  summary(aov(lifeEarnings~factor(attendedIndicator), data = pred.dat))
  ggplot(pred.dat, aes(factor(attendedIndicator), lifeEarnings))+geom_boxplot()+ theme_bw() + labs(title="Lifetime earnings by actual attendance")+ xlab("Attendance indicator") + ylab("Lifetime earnings")

  

#multiplot===========================================================================================
  multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
    require(grid)
    
    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    
    numPlots = length(plots)
    
    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
      # Make the panel
      # ncol: Number of columns of plots
      # nrow: Number of rows needed, calculated from # of cols
      layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                       ncol = cols, nrow = ceiling(numPlots/cols))
    }
    
    if (numPlots==1) {
      print(plots[[1]])
      
    } else {
      # Set up the page
      grid.newpage()
      pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
      
      # Make each plot, in the correct location
      for (i in 1:numPlots) {
        # Get the i,j matrix positions of the regions that contain this subplot
        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
        
        print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                        layout.pos.col = matchidx$col))
      }
    }
  }
