#NOTES:======================================================================
  #this file redoes the analysis without throwing out the people discarded by the old projection method

#libraries===================================================================
  library(ggplot2)
  library(plyr)
  library(scales)
  library(descr)
  library(mclogit)
  #library(car)
  library(survival)
  library(pscl)

#clear workspace ===================================================================
  rm(list = ls())

#get income data=============================================================
  INCOME_DATA <- read.csv("INCOME_DATA.csv")
  #set up income variables
    INCOME_DATA$LAB_1996b <- 0
    INCOME_DATA$LAB_1997b <- 0
    INCOME_DATA$LAB_1998b <- 0
    INCOME_DATA$LAB_1999b <- 0
    INCOME_DATA$LAB_2000b <- 0
    INCOME_DATA$LAB_2001b <- 0
    INCOME_DATA$LAB_2002b <- 0
    INCOME_DATA$LAB_2003b <- 0
    INCOME_DATA$LAB_2004b <- 0
    INCOME_DATA$LAB_2005b <- 0
    INCOME_DATA$LAB_2006b <- 0
    INCOME_DATA$LAB_2007b <- 0
    INCOME_DATA$LAB_2008b <- 0
    INCOME_DATA$LAB_2009b <- 0
  #populate incomes
    #load function for extracting incomes, syntax: data frame, indicator, main variable, secondary (refuser) variable, clarification question indicator or "None"
      source("fun_getTotal.R")
    #populate incomes 1996
      salary_96 = getTotal(INCOME_DATA, "P5_010_1997", "P5_016_1997", "P5_017_1997", "P5_011_1997", 1)
      INCOME_DATA$LAB_1996b <- salary_96
    #populate incomes 1997-2010
      year_vect = c("1997","1998","1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010")
      for (i in 1:length(year_vect)){
        #salary this year
          salIndicator=paste("YINC_1400_", year_vect[i], sep = "") #this only exists until 2002
          salMain=paste("YINC_1700_", year_vect[i], sep = "")
          salSecond=paste("YINC_1800_", year_vect[i], sep = "")
          salClarify=paste("YINC_1500_", year_vect[i], sep = "") 
        if (as.integer(year_vect[i])< 2003){
          salary_cur = getTotal(INCOME_DATA, salIndicator, salMain, salSecond, salClarify, 1)
        } else {
          salary_cur = getTotal(INCOME_DATA, salIndicator, salMain, salSecond, "None", 1)
        }
        #salary from two years ago
        if (as.integer(year_vect[i])> 1998){
          salTwoYearInd=paste("YINC_1400A_", year_vect[i], sep = "")
          salTwoYearMain = paste("YINC_1700A_", year_vect[i], sep = "") 
          salTwoYearSecond = paste("YINC_1800A_", year_vect[i], sep = "")
          salary_twoyear = getTotal(INCOME_DATA, salTwoYearInd, salTwoYearMain, salTwoYearSecond, "None", 1)
        }
        outString = paste("LAB_", toString(as.integer(year_vect[i])-1),"b", sep = "") #store in last year's income variable
        INCOME_DATA[,outString]<- salary_cur
        if (as.integer(year_vect[i])> 1998){
          outString2 = paste("LAB_", toString(as.integer(year_vect[i])-2),"b", sep = "") #store in last year's income variable
          for (k in 1: nrow(INCOME_DATA)){
            INCOME_DATA[k,outString2]<- max(0,INCOME_DATA[k,outString2]) + salary_twoyear[k]
          }
        }
      }
  #replace all negative numbers with negative three (missing indicator)
    year_vect = c("1996","1997","1998","1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009")
    for (i in 1:length(year_vect)){
      outString = paste("LAB_", toString(as.integer(year_vect[i])),"b", sep = "")
      INCOME_DATA[,outString][INCOME_DATA[,outString]<0] = -3
    }

#get enrollment data========================================================
  #set up variables
    INCOME_DATA$enroll2<-0
    INCOME_DATA$enroll3<-0
    INCOME_DATA$enroll4<-0
    INCOME_DATA$enroll5<-0
    INCOME_DATA$enroll6<-0
    INCOME_DATA$enroll7<-0
    INCOME_DATA$enroll8<-0
    INCOME_DATA$enroll9<-0
    INCOME_DATA$stillInSchool<--3
    INCOME_DATA$lastSchoolType<--3
  #populate
    year_vect = c("1997","1998","1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010")
    for (i in 1:length(year_vect)){
      intYear = as.integer(year_vect[i])
      if (intYear < 2005 & intYear != 1997){
        enrollVar = paste("CV_ENROLLSTAT_EDT_", year_vect[i], sep = "")
      } else {
        enrollVar = paste("CV_ENROLLSTAT_", year_vect[i], sep = "")
      }
      
      for (j in 1:nrow(INCOME_DATA)){
        yearNum = intYear-INCOME_DATA$START_YEAR[j]
        if (yearNum >= 0 & yearNum < 8){#this means you are in a year that is relevant for this person
          counter = yearNum +2
          #set corresponding variable
          pasteStr = paste('enroll',counter, sep = "")
          if (INCOME_DATA[j, enrollVar] <0){
            INCOME_DATA[j,pasteStr] <- INCOME_DATA[j, enrollVar] #settin gto value of current variable preserves missingness information
          }
          else if (INCOME_DATA[j, enrollVar] <8){
            INCOME_DATA[j,pasteStr] <- 0 #not enrolled
          }
          else {
            INCOME_DATA[j,pasteStr] <- 1 #enrolled
            if (i == length(year_vect)){
              INCOME_DATA[j,"lastSchoolType"] <- INCOME_DATA[j, enrollVar] #not used, but may be useful in future
            }
          }
        }
      }
    }
  #fill in missing enroll values (is a string of missing values is bounded by two of the same value on either side, set to that value)
    startEnroll = grep("enroll2", colnames(INCOME_DATA))
    for (j in 1:nrow(INCOME_DATA)){
      changing = 0
      last = 0
      enrollVect = c(INCOME_DATA[j,startEnroll], INCOME_DATA[j,startEnroll+1], INCOME_DATA[j,startEnroll+2], INCOME_DATA[j,startEnroll+ 3], INCOME_DATA[j,startEnroll+4], INCOME_DATA[j,startEnroll+5], INCOME_DATA[j,startEnroll+6], INCOME_DATA[j,startEnroll+7])
      for (i in 2:length(enrollVect)){
        if (changing ==0)  {
          if (enrollVect[i-1]>=0 & enrollVect[i]<0){
            last = enrollVect[i-1]
            change_start_ind = i
            changing = 1
          }
        }
        else{
          if (enrollVect[i]== last){
            for (k in (change_start_ind:i)){
              enrollVect[k] = last
            }
            changing = 0
          }
          else if (enrollVect[i] >= 0){
            changing = 0
          }
        }
      }
      for (i in 1:length(enrollVect)){
        INCOME_DATA[j,startEnroll+i-1]<-enrollVect[i]
      }
      i = length(enrollVect)
      while (INCOME_DATA$stillInSchool[j] <0 & i < 9){
        INCOME_DATA$stillInSchool[j] = max(INCOME_DATA$stillInSchool[j], INCOME_DATA[j,startEnroll+i-1])
        i = i+1
      }
      if (INCOME_DATA$stillInSchool[j] ==-3){
        INCOME_DATA$stillInSchool[j]<- 0
      }
    }

#add in attendance category data =============================================================
  CATEGORY_DATA <- read.csv("categories.csv")
  INCOME_DATA2 <- merge(x = CATEGORY_DATA, y = INCOME_DATA, by = "PUBID_1997", all.x = TRUE)

#adjust timing of income data =======================================================================
  INCOME_DATA2$znm1 <- 0
  INCOME_DATA2$znm2 <- 0
  INCOME_DATA2$znm3 <- 0
  INCOME_DATA2$znm4 <- 0
  INCOME_DATA2$znm5 <- 0
  INCOME_DATA2$znm6 <- 0
  INCOME_DATA2$znm7 <- 0
  INCOME_DATA2$znm8 <- 0
  
  for (i in 1:nrow(INCOME_DATA2)){
    curStrNM = paste('LAB_',toString(INCOME_DATA2$START_YEAR[i]),'b',sep = "")
    colIndexNM = grep(curStrNM, colnames(INCOME_DATA2))[1] #use the first occurence
    firstIndexNM = grep("LAB_1996b", colnames(INCOME_DATA2))
    startYsNM = grep("znm1", colnames(INCOME_DATA2))
    for (j in 0:7){
      if (colIndexNM + j <= firstIndexNM + 13){ 
        INCOME_DATA2[i,startYsNM+j] <- INCOME_DATA2[i, colIndexNM + j]
      }
      else {
        INCOME_DATA2[i,startYsNM+j] <- -4 
      }
    }
  }

#extract income data using restrictions on enrollment, etc ==============================================
  #rename data to preserve INCOME_DATA2 for future use
    ENROLL_DATA<-INCOME_DATA2
  #vector lists for storing output
    ageVectListLabNm <- list()
    incomeVectListLabNm <- list()
    enrollVectListLabNm <- list()
    employVectListLabNm <- list()
    ageVectListLabNmEmploy <- list()
    incomeVectListLabNmEmploy <- list()
    enrollVectListLabNmEmploy <- list()
    employVectListLabNmEmploy <- list()
  #read in employment data
    EMPLOY_DATA <- read.csv("hrsworked.csv")
    ENROLL_DATA <- merge(x = ENROLL_DATA, y = EMPLOY_DATA, by = "PUBID_1997", all.x = TRUE)
  #populate employment data
    for (i in 1:nrow(ENROLL_DATA)){
      incVectLabNm = c(ENROLL_DATA$znm1[i], ENROLL_DATA$znm2[i], ENROLL_DATA$znm3[i], ENROLL_DATA$znm4[i], ENROLL_DATA$znm5[i], ENROLL_DATA$znm6[i],ENROLL_DATA$znm7[i],ENROLL_DATA$znm8[i])
      enrollVect = c(ENROLL_DATA$enroll2[i], ENROLL_DATA$enroll3[i], ENROLL_DATA$enroll4[i], ENROLL_DATA$enroll5[i], ENROLL_DATA$enroll6[i], ENROLL_DATA$enroll7[i], ENROLL_DATA$enroll8[i], ENROLL_DATA$enroll9[i])
      employVect = c()
      curStr= paste('CVC_HOURS_WK_YR_ALL_',toString(ENROLL_DATA$START_YEAR[i]),'_XRND',sep = "")
      colIndex = grep(curStr, colnames(ENROLL_DATA))[1] #use the first occurence
      for (j in 0:7){
        if (colIndex + j <= ncol(ENROLL_DATA)){ #have to make sure that we aren't out of bounds!! these should be last columns since i added them
          curEmploy = ENROLL_DATA[i, colIndex+j]
          employVect[length(employVect)+1]= curEmploy
        }
        else {
          employVect[length(employVect)+1]= -3
        }
      }
      #store results in vector lists
      incomeVectListLabNm[[i]]<-incVectLabNm
      ageVectListLabNm[[i]]<-c(1:length(incVectLabNm))
      enrollVectListLabNm[[i]]<-enrollVect
      employVectListLabNm[[i]]<-employVect
    }

  #must add 18 to each age number so that tau is correct
    for (i in 1:length(ageVectListLabNm)){
      if (ageVectListLabNm[[i]][1]>0){
        ageVectListLabNm[[i]] = ageVectListLabNm[[i]] + 18
      }
    }

  #set vectors to missing if only zeros available
    for (i in 1:length(incomeVectListLabNm)){
      #set to missing if only zeros available
      if (sum(incomeVectListLabNm[[i]])<= 0){
        incomeVectListLabNm[[i]]<-c(-3)
        ageVectListLabNm[[i]]<-c(-3)
        enrollVectListLabNm[[i]]<-c(-3)
        employVectListLabNm[[i]]<-c(-3)
      }
    }
  #set up vectors with 1200 hr employment restriction
    source("fun_removeNotFT.R")
    LabEmployReturn <-removeNotFT(incomeVectListLabNm, ageVectListLabNm, enrollVectListLabNm, employVectListLabNm)
    incomeVectListLabEmploy<-LabEmployReturn[[1]]
    ageVectListLabEmploy<-LabEmployReturn[[2]]
    enrollVectListLabEmploy<-LabEmployReturn[[3]]
    employVectListLabEmploy <- LabEmployReturn[[4]]
  #create employ restricted vector with 10K restriction only
    source("fun_TenKMin.R")
    LabEmployReturn10K <-TenKMin(incomeVectListLabEmploy, ageVectListLabEmploy, enrollVectListLabEmploy, employVectListLabEmploy)
    incomeVectListLabEmploy10K<-LabEmployReturn10K[[1]]
    ageVectListLabEmploy10K<-LabEmployReturn10K[[2]]
    enrollVectListLabEmploy10K<-LabEmployReturn10K[[3]]
    employVectListLabEmploy10K<- LabEmployReturn10K[[4]]
  #get clean income vectors
  source("fun_adjustIncomes.R")
  outListLabNm <- adjustIncomes(ageVectListLabNm, incomeVectListLabNm, enrollVectListLabNm)
  outListLabEmploy <- adjustIncomes(ageVectListLabEmploy, incomeVectListLabEmploy, enrollVectListLabEmploy)
  outListLabEmploy10K <- adjustIncomes(ageVectListLabEmploy10K, incomeVectListLabEmploy10K, enrollVectListLabEmploy10K)
  rownames(outListLabEmploy10K)= c(19:26)
  rownames(outListLabEmploy)= c(19:26)
  rownames(outListLabNm)= c(19:26)
 #save workspace
  save.image("incomes.RData")
#create raw plot for introduction============================================
  #se function
    se <- function(data) {
      sqrt(var(data)/length(data))
    }
    #set up output dataset
      inc_dat = data.frame(pubid = colnames(outListLabNm), average = NA, averageEmploy = NA, averageEmploy10K = NA)
    #get incomes from each type
      for (i in 1:ncol(outListLabNm)){
        curVect = outListLabNm[4:7,i]
        curVectEmploy = outListLabEmploy[4:7,i]
        curVectEmploy10K = outListLabEmploy10K[4:7,i]
        curVect[curVect ==-4]=NA
        curVectEmploy[curVectEmploy ==-4]=NA
        curVectEmploy10K[curVectEmploy10K ==-4]=NA
        curVect[curVect ==-3]=NA
        curVectEmploy[curVectEmploy ==-3]=NA
        curVectEmploy10K[curVectEmploy10K ==-3]=NA
        if(length(na.exclude(curVect)>2)){
          inc_dat$average[i] = mean(na.exclude(curVect))  
        }
        else{
          inc_dat$average[i] = NA
        }
        if(length(na.exclude(curVectEmploy)>2)){
          inc_dat$averageEmploy[i] = mean(na.exclude(curVectEmploy))  
        }
        else{
          inc_dat$averageEmploy[i]= NA
        }
        if(length(na.exclude(curVectEmploy10K)>2)){
          inc_dat$averageEmploy10K[i] = mean(na.exclude(curVectEmploy10K))  
        }
        else{
          inc_dat$averageEmploy[i]= NA
        }
      }
  #add admissions, attendance data
    CATEGORY_DATA <- read.csv("admitcats.csv")
    inc_dat <- merge(x = inc_dat, y = CATEGORY_DATA, by = "pubid", all.x = TRUE)
  #delete those in category 7--specialty schools only
    inc_dat = inc_dat[inc_dat$admit != 7,]
    inc_dat = inc_dat[inc_dat$attend != 7,]
  #remove rows that are missing averages
    inc_dat_keep = inc_dat
    inc_dat = inc_dat_keep[!(is.na(inc_dat_keep$average)),]
    inc_dat_employ = inc_dat_keep[!(is.na(inc_dat_keep$averageEmploy)),]
    inc_dat_employ10K = inc_dat_keep[!(is.na(inc_dat_keep$averageEmploy10K)),]
  #find mean/variance of average salary
    #by attendance
      means = data.frame(attend = c("none","1","2","3","4","5/6"),
        avgInc = aggregate(inc_dat$average, list(gp=inc_dat$attend), mean)$x,
        avgIncse = aggregate(inc_dat$average, list(gp=inc_dat$attend), se)$x)
      meansEmploy = data.frame(attend = c("none","1","2","3","4","5/6"),
        avgInc = aggregate(inc_dat_employ$averageEmploy, list(gp=inc_dat_employ$attend), mean)$x,
        avgIncse = aggregate(inc_dat_employ$averageEmploy, list(gp=inc_dat_employ$attend), se)$x)
      meansEmploy10K = data.frame(attend = c("none","1","2","3","4","5/6"),
        avgInc = aggregate(inc_dat_employ10K$averageEmploy10K, list(gp=inc_dat_employ10K$attend), mean)$x,
        avgIncse = aggregate(inc_dat_employ10K$averageEmploy10K, list(gp=inc_dat_employ10K$attend), se)$x)
    #by admission
      means_byadmit = data.frame(admit = c("1","2","3","4","5/6"),
         avgInc = aggregate(inc_dat$average, list(gp=inc_dat$admit), mean)$x,
         avgIncse = aggregate(inc_dat$average, list(gp=inc_dat$admit), se)$x)
      meansEmploy_byadmit = data.frame(admit = c("1","2","3","4","5/6"),
         avgInc = aggregate(inc_dat_employ$averageEmploy, list(gp=inc_dat_employ$admit), mean)$x,
         avgIncse = aggregate(inc_dat_employ$averageEmploy, list(gp=inc_dat_employ$admit), se)$x)
      meansEmploy10K_byadmit = data.frame(admit = c("1","2","3","4","5/6"),
        avgInc = aggregate(inc_dat_employ10K$averageEmploy10K, list(gp=inc_dat_employ10K$admit), mean)$x,
        avgIncse = aggregate(inc_dat_employ10K$averageEmploy10K, list(gp=inc_dat_employ10K$admit), se)$x)
    #by admission using attenders only
      inc_dat_attenders= inc_dat[inc_dat$attend != -10,]
      inc_dat_employ_attenders= inc_dat_employ[inc_dat_employ$attend != -10,]
      inc_dat_employ10K_attenders= inc_dat_employ10K[inc_dat_employ10K$attend != -10,]
      means_byadmit_attenders = data.frame(admit = c("1","2","3","4","5/6"),
        avgInc = aggregate(inc_dat_attenders$average, list(gp=inc_dat_attenders$admit), mean)$x,
        avgIncse = aggregate(inc_dat_attenders$average, list(gp=inc_dat_attenders$admit), se)$x)
      meansEmploy_byadmit_attenders = data.frame(attend = c("1","2","3","4","5/6"),
        avgInc = aggregate(inc_dat_employ_attenders$averageEmploy, list(gp=inc_dat_employ_attenders$admit), mean)$x,
        avgIncse = aggregate(inc_dat_employ_attenders$averageEmploy, list(gp=inc_dat_employ_attenders$admit), se)$x)
      meansEmploy10K_byadmit_attenders = data.frame(attend = c("1","2","3","4","5/6"),
        avgInc = aggregate(inc_dat_employ10K_attenders$averageEmploy10K, list(gp=inc_dat_employ10K_attenders$admit), mean)$x,
        avgIncse = aggregate(inc_dat_employ10K_attenders$averageEmploy10K, list(gp=inc_dat_employ10K_attenders$admit), se)$x)
  #set up plot data
    plotdata=data.frame(attend = means$attend, avgInc= means$avgInc, avgIncse = means$avgIncse, type = 0)
    plotdata_admit=data.frame(admit = means_byadmit$admit, avgInc= means_byadmit$avgInc, avgIncse = means_byadmit$avgIncse, avgInc_attenders = means_byadmit_attenders$avgInc, avgIncse_attenders = means_byadmit_attenders$avgIncse, type = 0)
    plotdataEmploy=data.frame(attend = meansEmploy$attend, avgInc= meansEmploy$avgInc, avgIncse = meansEmploy$avgIncse, type = 0)
    plotdataEmploy_admit=data.frame(admit = meansEmploy_byadmit$admit, avgInc= meansEmploy_byadmit$avgInc, avgIncse = meansEmploy_byadmit$avgIncse, avgInc_attenders = meansEmploy_byadmit_attenders$avgInc, avgIncse_attenders = meansEmploy_byadmit_attenders$avgIncse, type = 0)
    plotdataEmploy10K=data.frame(attend = meansEmploy10K$attend, avgInc= meansEmploy10K$avgInc, avgIncse = meansEmploy10K$avgIncse, type = 0)
    plotdataEmploy10K_admit=data.frame(admit = meansEmploy10K_byadmit$admit, avgInc= meansEmploy10K_byadmit$avgInc, avgIncse = meansEmploy10K_byadmit$avgIncse, avgInc_attenders = meansEmploy10K_byadmit_attenders$avgInc, avgIncse_attenders = meansEmploy10K_byadmit_attenders$avgIncse, type = 0)
  #plain plots
    ggplot(plotdata, aes(x=attend, y=avgInc, group =0)) + geom_line()+theme_bw()+xlab("Selectivity of school attended")+ylab("Average post-college income over sample") +theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(), text = element_text(size=20))+geom_ribbon(data=plotdata,aes(ymin =avgInc - avgIncse*1.96, ymax = avgInc+avgIncse*1.96),alpha=0.3) + ggtitle("By attended, no restrictions") 
    ggplot(plotdataEmploy, aes(x=attend, y=avgInc, group =0)) + geom_line()+theme_bw()+xlab("Selectivity of school attended")+ylab("Average post-college income over sample") +theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(), text = element_text(size=20))+geom_ribbon(data=plotdataEmploy,aes(ymin =avgInc - avgIncse*1.96, ymax = avgInc+avgIncse*1.96),alpha=0.3) + ggtitle("By attended, employment restriction") 
    ggplot(plotdataEmploy10K, aes(x=attend, y=avgInc, group =0)) + geom_line()+theme_bw()+xlab("Selectivity of school attended")+ylab("Average post-college income over sample") +theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(), text = element_text(size=20))+geom_ribbon(data=plotdataEmploy10K,aes(ymin =avgInc - avgIncse*1.96, ymax = avgInc+avgIncse*1.96),alpha=0.3) + ggtitle("By attended, employment and 10K restrictions") 
    ggplot(plotdata_admit, aes(x=admit, y=avgInc, group =0)) + geom_line()+theme_bw()+xlab("Selectivity of top school admitted to")+ylab("Average post-college income over sample") +theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(), text = element_text(size=20))+geom_ribbon(data=plotdata_admit,aes(ymin =avgInc - avgIncse*1.96, ymax = avgInc+avgIncse*1.96),alpha=0.3) + ggtitle("By top admitted, no restrictions") 
    ggplot(plotdataEmploy_admit, aes(x=admit, y=avgInc, group =0)) + geom_line()+theme_bw()+xlab("Selectivity of top school admitted to")+ylab("Average post-college income over sample") +theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(), text = element_text(size=20))+geom_ribbon(data=plotdataEmploy_admit,aes(ymin =avgInc - avgIncse*1.96, ymax = avgInc+avgIncse*1.96),alpha=0.3) + ggtitle("By top admitted, employment restriction") 
    ggplot(plotdataEmploy10K_admit, aes(x=admit, y=avgInc, group =0)) + geom_line()+theme_bw()+xlab("Selectivity of top school admitted to")+ylab("Average post-college income over sample") +theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(), text = element_text(size=20))+geom_ribbon(data=plotdataEmploy10K_admit,aes(ymin =avgInc - avgIncse*1.96, ymax = avgInc+avgIncse*1.96),alpha=0.3) + ggtitle("By top admitted, employment and 10K") 
    ggplot(plotdata_admit, aes(x=admit, y=avgInc_attenders, group =0)) + geom_line()+theme_bw()+xlab("Selectivity of top school admitted to")+ylab("Average post-college income over sample") +theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(), text = element_text(size=20))+geom_ribbon(data=plotdata_admit,aes(ymin =avgInc_attenders - avgIncse_attenders*1.96, ymax = avgInc_attenders+avgIncse_attenders*1.96),alpha=0.3) + ggtitle("Attenders by top admitted, no restrictions") 
    ggplot(plotdataEmploy_admit, aes(x=admit, y=avgInc_attenders, group =0)) + geom_line()+theme_bw()+xlab("Selectivity of top school admitted to")+ylab("Average post-college income over sample") +theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(), text = element_text(size=20))+geom_ribbon(data=plotdataEmploy_admit,aes(ymin =avgInc_attenders - avgIncse_attenders*1.96, ymax = avgInc_attenders+avgIncse_attenders*1.96),alpha=0.3) + ggtitle("Attenders by top admitted, employment restriction") 
    ggplot(plotdataEmploy10K_admit, aes(x=admit, y=avgInc_attenders, group =0)) + geom_line()+theme_bw()+xlab("Selectivity of top school admitted to")+ylab("Average post-college income over sample") +theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(), text = element_text(size=20))+geom_ribbon(data=plotdataEmploy10K_admit,aes(ymin =avgInc_attenders - avgIncse_attenders*1.96, ymax = avgInc_attenders+avgIncse_attenders*1.96),alpha=0.3) + ggtitle("Attenders by top admitted, employment and 10K") 
  #10K restriction makes very little difference, so we drop this approach for now
  #perform analysis by type and store
    #by attendance
      for (i in 1:5){
        inc_dat_temp = inc_dat[inc_dat$admit == i,]
        inc_dat_employ_temp = inc_dat_employ[inc_dat_employ$admit == i,]
        levels = levels(as.factor(inc_dat_temp$attend))
        levels_employ = levels(as.factor(inc_dat_employ_temp$attend))
        means_temp = data.frame(attend = levels,
          avgInc = aggregate(inc_dat_temp$average, list(gp=inc_dat_temp$attend), mean)$x,
          avgIncse = aggregate(inc_dat_temp$average, list(gp=inc_dat_temp$attend), se)$x)
        means_temp_employ = data.frame(attend = levels,
          avgInc = aggregate(inc_dat_employ_temp$averageEmploy, list(gp=inc_dat_employ_temp$attend), mean)$x,
          avgIncse = aggregate(inc_dat_employ_temp$averageEmploy, list(gp=inc_dat_employ_temp$attend), se)$x)
        curPlot = data.frame(attend = levels, avgInc = means_temp$avgInc, avgIncse = means_temp$avgIncse, type = i)
        curPlotEmploy = data.frame(attend = levels_employ, avgInc = means_temp_employ$avgInc, avgIncse = means_temp_employ$avgIncse, type = i)
        plotdata = rbind(plotdata, curPlot)
        plotdataEmploy = rbind(plotdataEmploy, curPlotEmploy)
      }
    #by admission
      for (i in c(-10,1:5)){
        inc_dat_temp = inc_dat[inc_dat$attend == i,]
        inc_dat_employ_temp = inc_dat_employ[inc_dat_employ$attend == i,]
        levels = levels(as.factor(inc_dat_temp$admit))
        levels_employ = levels(as.factor(inc_dat_employ_temp$admit))
        means_temp = data.frame(admit = levels,
          avgInc = aggregate(inc_dat_temp$average, list(gp=inc_dat_temp$admit), mean)$x,
          avgIncse = aggregate(inc_dat_temp$average, list(gp=inc_dat_temp$admit), se)$x)
        means_temp_employ = data.frame(admit = levels_employ,
          avgInc = aggregate(inc_dat_employ_temp$averageEmploy, list(gp=inc_dat_employ_temp$admit), mean)$x,
          avgIncse = aggregate(inc_dat_employ_temp$averageEmploy, list(gp=inc_dat_employ_temp$admit), se)$x)
        curPlot = data.frame(admit = levels, avgInc = means_temp$avgInc, avgIncse = means_temp$avgIncse, avgInc_attenders = NA, avgIncse_attenders = NA,type = i)
        curPlotEmploy = data.frame(admit = levels_employ, avgInc = means_temp_employ$avgInc, avgIncse = means_temp_employ$avgIncse, avgInc_attenders = NA, avgIncse_attenders = NA, type = i)
        plotdata_admit = rbind(plotdata_admit, curPlot)
        plotdataEmploy_admit = rbind(plotdataEmploy_admit, curPlotEmploy)
      }
    #clean up category labels
      plotdata[plotdata$attend==5,]$attend="5/6"
      plotdata[plotdata$attend==-10,]$attend="none"
      plotdataEmploy[plotdataEmploy$attend==-10,]$attend="none"
      plotdataEmploy[plotdataEmploy$attend==5,]$attend="5/6"
      plotdata_admit[plotdata_admit$admit==5,]$admit="5/6"
      plotdataEmploy_admit[plotdataEmploy_admit$admit==5,]$admit="5/6"
    #combined plots
      #plotting by attend--no restrictions only
        for (i in c(1:5)){ #this for loop does not work
          ggplot(plotdata, aes(x=attend, y = avgInc)) + 
            geom_bar(data = plotdata[plotdata$type==i,],stat='identity', colour = 'black',fill = NA) + 
            geom_line(data = plotdata[plotdata$type==0,], aes(group= 1), colour = 'blue')+
            geom_line(data = plotdata[plotdata$type==0,], aes(y = avgInc-avgIncse*1.96,group= 1),colour = 'grey')+
            geom_line(data = plotdata[plotdata$type==0,], aes(y = avgInc+avgIncse*1.96,group= 1), colour = 'grey')+
            theme_bw()+
            xlab("Selectivity of school attended") +
            ylab("Average income over ages 22-25") +
            theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(), text = element_text(size=20))+
            ggtitle(paste("Admitted=",i))+
            geom_errorbar(data= plotdata[plotdata$type==i,],aes(ymin=avgInc-avgIncse*1.96, ymax = avgInc+avgIncse*1.96))
        }
      #plotting by admit--no restrictions only
        for (i in c(-10,1:5)){
          ggplot(plotdata_admit, aes(x=admit, y = avgInc)) + 
          geom_bar(data = plotdata_admit[plotdata_admit$type==i,],stat='identity', colour = 'black',fill = NA) + 
          geom_line(data = plotdata_admit[plotdata_admit$type==0,], aes(group= 1), colour = 'blue')+
          geom_line(data = plotdata_admit[plotdata_admit$type==0,], aes(y = avgInc-avgIncse*1.96,group= 1),colour = 'grey')+
          geom_line(data = plotdata_admit[plotdata_admit$type==0,], aes(y = avgInc+avgIncse*1.96,group= 1), colour = 'grey')+
          theme_bw()+
          xlab("Selectivity of top school admitted to") +
          ylab("Average income over ages 22-25") +
          theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(), text = element_text(size=20))+
          ggtitle(paste("Attended=",i))+
          geom_errorbar(data= plotdata_admit[plotdata_admit$type==i,],aes(ymin=avgInc-avgIncse*1.96, ymax = avgInc+avgIncse*1.96))
        }
    #plot all-in-one
      #by admission--other lines still go down
        ggplot(plotdata_admit, aes(x=admit, y = avgInc, color = as.factor(type))) + 
          scale_color_brewer(palette="Spectral")+
          geom_line(aes(group = as.factor(type)))+
          theme_bw()+
          xlab("Selectivity of top school admitted to") +
          ylab("Average income over ages 22-25")+
          theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(), text = element_text(size=20))
      #by attendance--still seem to see downward trend
        ggplot(plotdata, aes(x=attend, y = avgInc, color = as.factor(type))) + 
          scale_color_brewer(palette="Spectral")+
          geom_line(aes(group =as.factor(type)))+
          theme_bw()+
          xlab("Selectivity of top school attended") +
          ylab("Average income over ages 22-25")+
          theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(), text = element_text(size=20))

#regressions and other analysis regarding income mean differences============================================================================
  #Table 3: Check slope differences
    #ignore non-attenders
      plotdata_attenders= plotdata[plotdata$attend != "none",]
    #by attendance
      plotdata_attenders$conditional=1
      plotdata_attenders[plotdata_attenders$type == 0,]$conditional = 0
      plotdata_attenders$attend = as.numeric(plotdata_attenders$attend)
      byattend_reg=lm(avgInc~attend+as.factor(type)+I(attend*conditional), data= plotdata_attenders)
    #by admission
      plotdata_admit_attenders= plotdata_admit[plotdata_admit$type != -10,]
      plotdata_admit_attenders$conditional=1
      plotdata_admit_attenders[plotdata_admit_attenders$type == 0,]$conditional = 0
      plotdata_admit_attenders$admit = as.numeric(plotdata_admit_attenders$admit)
      byadmit_reg=lm(avgInc~admit+as.factor(type)+I(admit*conditional), data= plotdata_admit_attenders)
  #Table 4: which matters individually
    admitModel = lm(average~factor(admit), data = na.exclude(inc_dat))
    attendModel = lm(average~factor(attend), data = na.exclude(inc_dat))
  #Table 5: attendance and admissions error
    admit_cats <- c(1,2, 3, 4, 5)
    attend_cats <- c(-10,1,2, 3, 4, 5)
    inc_dat_attenders$attend2 = NA
    inc_dat_attenders$admit2 = NA
    lookup = data.frame(id = c(1,2,3,4,5), percent = c(33, 60, 75, 85, 100))
    for (i in 1:nrow(inc_dat_attenders)){
      inc_dat_attenders$attend2[i] = lookup[(1:dim(lookup)[1])[lookup[,1]==inc_dat_attenders$attend[i]],2]
      inc_dat_attenders$admit2[i] = lookup[(1:dim(lookup)[1])[lookup[,1]==inc_dat_attenders$admit[i]],2]
    }
    test1 = lm(admit2~attend2, data = inc_dat_attenders)
    inc_dat_attenders$admitError = test1$residuals
    test2 = lm(attend2~admit2, data = inc_dat_attenders)
    inc_dat_attenders$attendError = test2$residuals
    err1 = lm(average~attend2+admitError, data = inc_dat_attenders)
    err2 = lm(average~admit2+attendError, data = inc_dat_attenders)
    #table 5 removing those admitted to tier1
      inc_dat_subset = inc_dat_attenders[inc_dat_attenders$admit != 1,]
      test1 = lm(admit2~attend2, data = inc_dat_subset)
      inc_dat_subset$admitError = test1$residuals
      test2 = lm(attend2~admit2, data = inc_dat_subset)
      inc_dat_subset$attendError = test2$residuals
      err1 = lm(average~attend2+admitError, data = inc_dat_subset)
      err2 = lm(average~admit2+attendError, data = inc_dat_subset)
    #continuous test and ANOVA by admitted category
      for (i in 1:length(admit_cats)){
        curData = inc_dat_attenders[inc_dat_attenders$admit==admit_cats[i],]
        curCount = nrow(curData)
        if (curCount >0){
          curData<-curData[c("average", "attend2", "attend")]
          curCount = nrow(na.exclude(curData))
          if (curCount > 1){
            myMod = lm(average~attend2, data = curData)
            print(paste("Admit=",admit_cats[i]))
            #print(summary(myMod))
            if(length(levels(as.factor(curData$attend)))>1){
              print(summary(aov(average ~ as.factor(attend), data=curData))) 
            }
          }
        }
      }
  #Table 11: categorical table and pairwise t-tests
    admit_cats <- c(1,2, 3, 4, 5)
    attend_cats <- c(-10,1,2, 3, 4, 5)
    #by admission
      byAdmitCoeffVect = data.frame(matrix(ncol = 15, nrow = length(admit_cats)))
      colnames(byAdmitCoeffVect)=c("intercept","1", "2", "3", "4", "6", "intsig", "1sig", "2sig", "3sig", "4sig", "6sig","R2", "NumObservations", "levels")
      for (i in 1:length(admit_cats)){
        curData = inc_dat[inc_dat$admit==admit_cats[i],]
        curCount = nrow(curData)
        if (curCount >0){
          curData<-curData[c("average", "attend")]
          curCount = nrow(na.exclude(curData))
          numCoeffs <- length(levels(factor(curData$attend)))
          if (numCoeffs >1 & curCount > numCoeffs){
            curModel = lm(average~factor(attend), data = na.exclude(curData))
            numCoeffs <- length(curModel$coefficients)
            #pairwise t-test
              print(paste("admit =",i))
              test = pairwise.t.test(curData$average, factor(curData$attend), p.adj = "none")
              print(test$p.value)
            for (j in 1:numCoeffs){
              byAdmitCoeffVect[i,j]= curModel$coefficients[j]
              byAdmitCoeffVect[i,j+6]= summary(curModel)$coefficients[j,4]
            }
            byAdmitCoeffVect[i,13]= summary(curModel)$r.squared
            byAdmitCoeffVect[i,15]= toString(levels(factor(curData$attend)))
          } 
        }
      byAdmitCoeffVect[i,14]= curCount
    }
    write.csv(byAdmitCoeffVect, "byAdmit2.csv")
    #by attendance
      byAttendCoeffVect = data.frame(matrix(ncol = 13, nrow = length(admit_cats)))
      colnames(byAttendCoeffVect)=c("intercept", "2", "3", "4", "6", "intsig", "2sig", "3sig", "4sig", "6sig","R2", "NumObservations", "levels")
      for (i in 1:length(attend_cats)){
        curData = inc_dat[inc_dat$attend==attend_cats[i],]
        curCount = nrow(curData)
        if (curCount >0){
          curData<-curData[c("average", "admit")]
          curCount = nrow(na.exclude(curData))
          numCoeffs <- length(levels(factor(curData$admit)))
          if (numCoeffs >1 & curCount > numCoeffs){
            curModel = lm(average~factor(admit), data = na.exclude(curData))
            numCoeffs <- length(curModel$coefficients)
            #pairwise t-test
              print(paste("attend =",i))
              test = pairwise.t.test(curData$average, factor(curData$admit), p.adj = "none")
              print(test$p.value)
            for (j in 1:numCoeffs){
              byAttendCoeffVect[i,j]= curModel$coefficients[j]
              byAttendCoeffVect[i,j+5]= summary(curModel)$coefficients[j,4]
            }
            byAttendCoeffVect[i,11]= summary(curModel)$r.squared
            byAttendCoeffVect[i,13]= toString(levels(factor(curData$admit)))
          } 
        }
        byAttendCoeffVect[i,12]= curCount
      }
      write.csv(byAttendCoeffVect, "byAttend2.csv")
  #Table 13: Attending college at all
    inc_dat$attendInd = NA
    for (i in 1:nrow(inc_dat)){
      if(inc_dat$attend[i]==-10){
        inc_dat$attendInd[i] = 0
      } else{
        inc_dat$attendInd[i] = 1
      }
    }
    #run checkPredictionAbility using this indicator
      source("fun_checkPredictionAbilityAttendOnly2.R")
      coeffs = checkPredictionAbilityAttendOnly(inc_dat)
#difference plots for college choice section, Figure 3
  #make sure there is a difference in salary by admissions category
    inc_dat_nonattenders = inc_dat[inc_dat$attend==-10,]
    anova(lm(average~factor(admit), data = inc_dat_attenders))
    anova(lm(average~factor(admit), data = inc_dat_nonattenders)) 
  #pairwise t-tests for good measure
    pairwise.t.test(inc_dat_attenders$average, factor(inc_dat_attenders$admit), p.adj = "none")
    pairwise.t.test(inc_dat_nonattenders$average, factor(inc_dat_nonattenders$admit), p.adj = "none")
  #create plot
    aggregate(inc_dat_attenders$average, list(gp=inc_dat_attenders$admit), mean)
    meanscheck = data.frame(admit = c("1","2","3","4","5/6"),
      attend = aggregate(inc_dat_attenders$average, list(gp=inc_dat_attenders$admit), mean)$x,
      attendse = aggregate(inc_dat_attenders$average, list(gp=inc_dat_attenders$admit), se)$x,
      nonattend = aggregate(inc_dat_nonattenders$average, list(gp=inc_dat_nonattenders$admit), mean)$x,
      nonattendse = aggregate(inc_dat_nonattenders$average, list(gp=inc_dat_nonattenders$admit), se)$x)
    meanscheck = within(meanscheck, {
      diff <- attend - nonattend
      diffse <- sqrt(attendse^2 + nonattendse^2)
    })
    #plot
      ggplot(meanscheck, aes(x=admit, y=diff)) + geom_bar(stat='identity', fill = 'grey') +
        geom_errorbar(aes(ymin=diff - diffse*1.96, ymax = diff+diffse*1.96))+theme_bw()+xlab("Selectivity of the top school admitted to") +ylab("Income difference, (attenders)-(non-attenders)") +theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(), text = element_text(size=20))+scale_y_continuous(labels = comma)
      ggplot(meanscheck, aes(x=admit, y=attend)) + geom_bar(stat='identity', fill = 'grey') +
        geom_errorbar(aes(ymin=attend - attendse*1.96, ymax = attend+attendse*1.96))+theme_bw()+xlab("Selectivity of the top school admitted to") +ylab("Average income for attenders")+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(), text = element_text(size=20))+scale_y_continuous(labels = comma) 
  save.image("regress-averages.RData")

#regressions and other analysis regarding variance differences===========================================================
  #pull mean-based standard errors by attend/admit group. This is not really useful.
    outData = data.frame(matrix(ncol = 5, nrow = 0))
    colnames(outData)= c("attend","mean","sd","count","admit" )
    dataList = list()
    for (i in 1:length(admit_cats)){
      curData = inc_dat[inc_dat$admit==admit_cats[i],]
      curCount = nrow(curData)
      if (curCount > 1){
        curOutData = ddply(curData,~attend,summarise,mean=mean(average),sd=sd(average), count = length(average))
        curOutData$se =1.96*((curOutData$sd)/(sqrt(curOutData$count)))
        curOutData$attend2 = curOutData$attend
        curOutData$lb = curOutData$mean-curOutData$se
        curOutData$ub = curOutData$mean+curOutData$se
      }
    curOutData$admit = admit_cats[i]
    dataList[[i]]= curOutData #this is totally wrong
    outData = rbind(outData, curOutData)
    }
  #calculate each person's MSE in regards to their top admitted group
    #mse based only on top admitted group
      inc_dat$mse = NA
      for (i in 1:nrow(inc_dat)){
        curAdmit = inc_dat$admit[i]
        if(curAdmit == 5){
          curAdmit = "5/6"
        }
        curMean = means_byadmit[means_byadmit$admit == curAdmit,]$avgInc
        inc_dat$mse[i]= (curMean - inc_dat$average[i])^2
      }
    #mse based on top admitted and attended group (more correct)
      inc_dat$mse = NA
      for (i in 1:nrow(inc_dat)){
        curAdmit = inc_dat$admit[i]
        curMean = outData[outData$admit == inc_dat$admit[i] & outData$attend == inc_dat$attend[i],]$mean
        inc_dat$mse[i]= (curMean - inc_dat$average[i])^2
      }
  #check if attendance predicts this standard error
    seCheck = lm(mse~as.factor(attend), data = inc_dat)
    test = pairwise.t.test(inc_dat$mse, factor(inc_dat$attend), p.adj = "none")
    print(test$p.value)
  #check it attendance predicts this standard error WITHIN AN ADMISSIONS GROUP (more correct)
    for (i in 1:length(admit_cats)){
      curData = inc_dat[inc_dat$admit==admit_cats[i],]
      curCount = nrow(curData)
      if (curCount >0){
        print(paste("admit =",admit_cats[i]))
        test = pairwise.t.test(curData$mse, factor(curData$attend), p.adj = "none")
        print(test$p.value)
      }
    }
  #new table get relevant averages
    #type 5 admitted, all individuals
      mean(inc_dat[inc_dat$admit == 5,]$average)
    #attenders only for 2, 3, 4
      aggregate(inc_dat_attenders$average, list(gp=inc_dat_attenders$admit), mean)
    #attenders of type 1 who did not attend 1
      mean(inc_dat_attenders[inc_dat_attenders$admit ==1 & inc_dat_attenders$attend != 1,]$average)
        
#other drivers of income============================================================
  #get variables
    #inc_dat_store = inc_dat
    incomePreds = read.csv("inputs/incomepredictorslong.csv")
    inc_dat = merge(x = inc_dat, y = incomePreds, by = "pubid", all.x = TRUE)
    #COLLEGE_NUM<- read.csv("inputs/collegenumber.csv")
    #inc_dat<- merge(x = inc_dat, y = COLLEGE_NUM, by.x="pubid",by.y = "PUBID_1997", all.x = TRUE)
    LOC_DATA <- read.csv("inputs/desensitizedloc.csv")
    inc_dat<- merge(x = inc_dat, y = LOC_DATA, by.x="pubid",by.y = "PUBID_1997", all.x = TRUE)
    SIC_DATA <- read.csv("inputs/sic.csv")
    inc_dat<- merge(x = inc_dat, y = SIC_DATA, by.x="pubid",by.y = "PUBID_1997", all.x = TRUE)
    CVC_DATA <- read.csv("inputs/cvcvars.csv")
    inc_dat<- merge(x = inc_dat, y = CVC_DATA, by.x="pubid",by.y = "PUBID_1997", all.x = TRUE)
    REMAINING_DATA <- read.csv("inputs/frommerged_data_reduced.csv")
    inc_dat<- merge(x = inc_dat, y = REMAINING_DATA, by.x="pubid",by.y = "PUBID_1997", all.x = TRUE)
    MERGE_DATA = read.csv("merginginfo.csv")
    inc_dat<- merge(x = inc_dat, y = MERGE_DATA, by.x="pubid",by.y = "PUBID_1997", all.x = TRUE)
    RES_DATA = read.csv("inputs/StateOfRes.csv")
    #inc_dat<- merge(x = inc_dat, y = RES_DATA, by.x="pubid",by.y = "PUBID_1997", all.x = TRUE)
    BLS_DATA = read.csv("blsvarsforchoice.csv")
    inc_dat<- merge(x = inc_dat, y = BLS_DATA, by.x="pubid",by.y = "PUBID_1997", all.x = TRUE)
    keepCols = c("pubid","average","admit","attend","mse","gender","MAJOR","MAJOR2","GEO","GRADES","COLLEGECOMPLETED", "sat_math","sat_verbal")
    source("fun_fillIncomePredictor2.R")
    inc_dat2 = fillIncomePredictors(inc_dat)
  #populate useful variables
    #religious preference
      inc_dat2$STUDENT_RELIGION = -3
      inc_dat2[inc_dat2$WHAT_CURR_RELIGIOUS_PREFERENCE_1997 >=0,]$STUDENT_RELIGION = inc_dat2[inc_dat2$WHAT_CURR_RELIGIOUS_PREFERENCE_1997 >=0,]$WHAT_CURR_RELIGIOUS_PREFERENCE_1997
      inc_dat2[inc_dat2$WHAT_CURR_RELIGIOUS_PREFERENCE_1997 < 0,]$STUDENT_RELIGION = inc_dat2[inc_dat2$WHAT_CURR_RELIGIOUS_PREFERENCE_1997 < 0,]$R_CURR_REL_PREF_2005
      keepCols = c(keepCols,"STUDENT_RELIGION")
    #parent's highest education
      inc_dat2$DAD_ED = inc_dat2$CV_HGC_RES_DAD_1997
      inc_dat2$MOM_ED = inc_dat2$CV_HGC_RES_MOM_1997
      keepCols = c(keepCols, "MOM_ED", "DAD_ED")
    #yearly variables
      inc_dat2$HH_SIZE = -3
      inc_dat2$HH_INCOME = -3
      inc_dat2$URBAN_RURAL = -3
      inc_dat2$SCHOOL_TYPE = -3
      inc_dat2$RES_STATE = -3
      source("fun_fillMiss2.R")
      for (i in 1:nrow(inc_dat2)){
        #household size
          hhSizeVar = paste("CV_HH_SIZE_", toString(inc_dat2$CHOICE_YEAR[i]), sep = "")
          if(hhSizeVar %in% colnames(inc_dat2)){
            if(inc_dat2[i, hhSizeVar]>0){
              inc_dat2$HH_SIZE[i]= inc_dat2[i, hhSizeVar]
            } else{
              inc_dat2$HH_SIZE[i]= fillMiss(hhSizeVar, i, TRUE, "CHOICE")
            }
          }
        #household income
          hhIncomeVar = paste("CV_INCOME_GROSS_YR_", toString(inc_dat2$CHOICE_YEAR[i]), sep = "")
          if(hhIncomeVar %in% colnames(inc_dat2)){
            if(inc_dat2[i, hhIncomeVar]>0){
              inc_dat2$HH_INCOME[i]= inc_dat2[i, hhIncomeVar]
            } else{
              inc_dat2$HH_INCOME[i]= fillMiss(hhIncomeVar, i, TRUE, "CHOICE")
            }
          }
        #urban rural
          urbanRuralVar = paste("CV_URBAN_RURAL_", toString(inc_dat2$CHOICE_YEAR[i]), sep = "")
          if(urbanRuralVar %in% colnames(inc_dat2)){
            if(inc_dat2[i, urbanRuralVar]>0){
              inc_dat2$URBAN_RURAL[i]= inc_dat2[i, urbanRuralVar]
            } else{
              inc_dat2$URBAN_RURAL[i]= fillMiss(urbanRuralVar, i, TRUE, "CHOICE")
            }
          }
        #high school type
          schoolTypeVar = paste("NEWSCH_TYPE_.ROS_ITEM._L1_", toString(inc_dat2$CHOICE_YEAR[i]), sep = "")
          if(inc_dat2[i,schoolTypeVar]>0){
            inc_dat2$SCHOOL_TYPE[i]= inc_dat2[i, schoolTypeVar]
          }else{
            inc_dat2$SCHOOL_TYPE[i]= fillMiss(schoolTypeVar, i, FALSE, "CHOICE")
          }
        #state of residence in SCHOOL ATTENDANCE YEAR!
          resStateVar = paste("GEO02_", toString(inc_dat2$COLLEGEID_YEAR2[i]), sep = "")
          if(resStateVar %in% colnames(RES_DATA)){
            if(RES_DATA[i,resStateVar]>0){
              inc_dat2$RES_STATE[i]= RES_DATA[i, resStateVar]
            }else{
              inc_dat2$RES_STATE[i]= fillMiss(resStateVar, i, TRUE, "START")
            }
          }
        }
        keepCols = c(keepCols, "HH_SIZE", "HH_INCOME", "URBAN_RURAL","SCHOOL_TYPE","RES_STATE")
  #clean up
    inc_dat = inc_dat2[,keepCols]
    inc_dat[inc_dat==-3]=NA
    inc_dat[inc_dat==-4]=NA
    inc_dat[inc_dat==-5]=NA
  #make sure inc_dat is correctly set up
    inc_dat$attend2 = NA
    inc_dat$admit2 = NA
    lookup = data.frame(id = c(-10,1,2,3,4,5), percent = c(NA,33, 60, 75, 85, 100))
    for (i in 1:nrow(inc_dat)){
      inc_dat$attend2[i] = lookup[(1:dim(lookup)[1])[lookup[,1]==inc_dat$attend[i]],2]
      inc_dat$admit2[i] = lookup[(1:dim(lookup)[1])[lookup[,1]==inc_dat$admit[i]],2]
    }

#regression with other drivers of income=================================================================
  reg.dat = inc_dat[,c("pubid", "admit2","attend2", "average", "MAJOR", "sat_math", "HH_INCOME")]
  reg.dat = na.exclude(reg.dat)
  engineering <- c(9,13)
  bus <- c(7, 8)
  reg.dat$EngInd= 0
  reg.dat[reg.dat$MAJOR %in% engineering,]$EngInd = 1
  reg.dat$BusInd= 0
  reg.dat[reg.dat$MAJOR %in% bus,]$BusInd = 1
  reg.dat$LowInd = 0
  reg.dat[reg.dat$MAJOR %in% c(3,11,12,16,26,31,33,32),]$LowInd = 1
  reg.dat$HighInd = 0
  reg.dat[reg.dat$MAJOR %in% c(engineering,bus),]$HighInd =1
  #checkPred = lm(average~sat_math+HH_INCOME+factor(EngInd)+factor(BusInd), data = reg.dat)
  checkPred = lm(average~sat_math+HH_INCOME+factor(LowInd)+factor(HighInd), data = reg.dat)
  #major effect plot
    bus.dat = reg.dat[reg.dat$BusInd==1,]
    eng.dat = reg.dat[reg.dat$EngInd==1,]
    high.dat = reg.dat[reg.dat$HighInd==1,]
    low.dat = reg.dat[reg.dat$LowInd==1,]
    sal_byadmit_bus = data.frame(admit = c("1","2","3","4","5/6"),
      avgInc = aggregate(bus.dat$average, list(gp=bus.dat$admit2), mean)$x,
      avgIncse = aggregate(bus.dat$average, list(gp=bus.dat$admit2), se)$x)
    sal_byadmit_eng = data.frame(admit = c("1","2","3","4","5/6"),
       avgInc = aggregate(eng.dat$average, list(gp=eng.dat$admit2), mean)$x,
       avgIncse = aggregate(eng.dat$average, list(gp=eng.dat$admit2), se)$x)
    sal_byadmit_avg = data.frame(admit = c("1","2","3","4","5/6"),
      avgInc = aggregate(reg.dat$average, list(gp=reg.dat$admit2), mean)$x,
      avgIncse = aggregate(reg.dat$average, list(gp=reg.dat$admit2), se)$x)
    sal_byadmit_high = data.frame(admit = c("1","2","3","4","5/6"),
      avgInc = aggregate(high.dat$average, list(gp=high.dat$admit2), mean)$x,
      avgIncse = aggregate(high.dat$average, list(gp=high.dat$admit2), se)$x)
    sal_byadmit_low = data.frame(admit = c("1","2","3","4","5/6"),
      avgInc = aggregate(low.dat$average, list(gp=low.dat$admit2), mean)$x,
      avgIncse = aggregate(low.dat$average, list(gp=low.dat$admit2), se)$x)
  #test--does major still matter for top people
    topDat = inc_dat[inc_dat$admit ==1,]
      topTest =aov(average ~ as.factor(MAJOR), data=topDat)
      topReg = lm(average~factor(attend), data = topDat) #attending 1 makes a difference
      topDat$EngInd= 0
      topDat$BusInd= 0
      topDat$HighInd = 0
      topDat$LowInd = 0
      topDat[topDat$MAJOR %in% engineering,]$EngInd = 1
      topDat[topDat$MAJOR %in% bus,]$BusInd = 1
      topDat[topDat$MAJOR %in% c(bus,engineering),]$HighInd = 1
      topDat[topDat$MAJOR %in% c(3,11,12,16,26,31,33,32),]$LowInd = 1
      topReg = lm(average~factor(attend)+factor(HighInd)+factor(LowInd), data = topDat)
    topDat = inc_dat[inc_dat$attend ==1,]
      topTest =aov(average ~ as.factor(MAJOR), data=topDat)
      topDat$EngInd= 0
      topDat$BusInd= 0
      topDat$HighInd = 0
      topDat$LowInd= 0
      topDat[topDat$MAJOR %in% engineering,]$EngInd = 1
      topDat[topDat$MAJOR %in% bus,]$BusInd = 1
      topDat[topDat$MAJOR %in% c(bus,engineering),]$HighInd = 1
      topDat[topDat$MAJOR %in% c(3,11,12,16,26,31,33,32),]$LowInd = 1
      topReg = lm(average~factor(HighInd)+factor(LowInd), data = topDat)    
  save.image("regression-income.RData")

#get data for choice models=========================================================
  choice.dat = read.table("inputs/studentadmitdata.txt", header = TRUE)
  incNames = inc_dat$pubid
  LONG_DATA = merge(x = choice.dat, y = inc_dat, by.y = "pubid",by.x = "PUBID_1997", all.x = TRUE)
  LONG_DATA = LONG_DATA[LONG_DATA$PUBID_1997 %in% incNames,] #keep only the people who were in the income analysis
  #note that some people do not appear in the choice data list--reduced from 1053 to 1028 people
  #read financial data
    FIN_DATA = read.csv("finaid.csv")
    REL_IDS = data.frame(PUBID_1997 = choice.dat$PUBID_1997)
    FIN_DATA = merge(x = REL_IDS, y = FIN_DATA, by = "PUBID_1997", all.x = TRUE)
    SECRET_DATA = read.csv("MERGED_DATA_REDUCED.csv")
    strPREVCOL = "PREV_COL_APP_ID"
    strYCOC = "YCOC_050P"
    strGEO = "GEO69"
    varListPREVCOL= colnames(SECRET_DATA)[grep(strPREVCOL, colnames(SECRET_DATA))] 
    varListYCOC= colnames(SECRET_DATA)[grep(strYCOC, colnames(SECRET_DATA))]
    varListGEO= colnames(SECRET_DATA)[grep(strGEO, colnames(SECRET_DATA))] 
    varList = c(varListPREVCOL, varListYCOC, varListGEO)
    SECRET_DATA = SECRET_DATA[,c("PUBID_1997",varList)]
    FIN_DATA = merge(x=FIN_DATA, y = SECRET_DATA, by = "PUBID_1997", all.x = TRUE)
    FIN_IND = read.csv("finaidind.csv")
    FIN_DATA = merge(x=FIN_DATA, y = FIN_IND, by = "PUBID_1997", all.x = TRUE)
    YCOC_DLI = read.csv("ycocdli.csv")
    FIN_DATA = merge(x=FIN_DATA, y = YCOC_DLI, by = "PUBID_1997", all.x = TRUE)
    #loop through long data and fill financial aid information for each school
      source("fun_checkDLI.R")
      source("fun_getAid.R")
      source("fun_getAidDLI.R")
      source("fun_getAidYSCH.R")
      #get aid from YCOC-055 variables (main)
        aidList055 = list()
        for (i in 1: nrow(LONG_DATA)){
          curData = FIN_DATA[FIN_DATA$PUBID_1997 == LONG_DATA$PUBID_1997[i],]
          curSchool = LONG_DATA$AdmittedSchool[i]
          varListMATCH = c()
          for (j in 1:length(varListYCOC)){ #look for current school index in all YCOC variables, beginning with earliest
            if (curData[1,varListYCOC[j]]==curSchool | curData[1,varListYCOC[j]]*100==curSchool| curData[1,varListYCOC[j]]==curSchool*100){
              varListMATCH[length(varListMATCH)+1]= varListYCOC[j]
            }
          }
          if (length(varListMATCH>0)){
            aidList055[[i]] = getAid(varListMATCH)
          } else{
            aidList055[[i]]= -3
          }
        }
      #get aid from DLI variables (date of last interview), this only yields one result for i = 973, pubid = 6412
        #build varListDLI
          varListDLI= colnames(SECRET_DATA)[grep("PREV_COL_APP", colnames(SECRET_DATA))]
          aidListDLI = list()
          matchList = rep(NA, nrow(LONG_DATA))
          for (i in 1: nrow(LONG_DATA)){
            curData = FIN_DATA[FIN_DATA$PUBID_1997 == LONG_DATA$PUBID_1997[i],]
            curSchool = LONG_DATA$AdmittedSchool[i]
            varListMATCH = c()
            for (j in 1:length(varListDLI)){ 
              if (curData[1,varListDLI[j]]==curSchool| curData[1,varListDLI[j]]*100==curSchool | curData[1,varListDLI[j]]==curSchool* 100){
                varListMATCH[length(varListMATCH)+1]= varListDLI[j]
              }
            }
            if (length(varListMATCH>0)){
              aidListDLI[[i]] = getAidDLI(varListMATCH)
              matchList[i] = 1
            } else{
              aidListDLI[[i]]= -3
              matchList[i] = 0
            }
          }
        #get school-independent aid
          otherAidStr = paste("YCOC_022_01_",sep = "")
          varList022= colnames(FIN_DATA)[grep(otherAidStr, colnames(FIN_DATA))]
          aidListALLSCHOOL = list()
          for (i in 1:nrow(LONG_DATA)){
            aidListALLSCHOOL[[i]]= rep(NA, length(varList022))
            curData = FIN_DATA[FIN_DATA$PUBID_1997 == LONG_DATA$PUBID_1997[i],]
            for (k in 1:length(varList022)){
              aidListALLSCHOOL[[i]][k] = curData[1,varList022[k]]
            }
          }
        #get aid data from YSCH for attended schools
          aidListYSCH = list()
          for (i in 1:nrow(LONG_DATA)){
            curData = FIN_DATA[FIN_DATA$PUBID_1997 == LONG_DATA$PUBID_1997[i],]
            curSchool = LONG_DATA$AdmittedSchool[i]
            varListMATCH = c()
            for (j in 1:length(varListGEO)){
              if (curData[1,varListGEO[j]]==curSchool | curData[1,varListGEO[j]]*100==curSchool | curData[1,varListGEO[j]]==curSchool*100){
                varListMATCH[length(varListMATCH)+1]= varListGEO[j]
              }
            }
            if (length(varListMATCH>0)){
              aidListYSCH[[i]] = getAidYSCH(varListMATCH)
            } else{
              aidListYSCH[[i]]= -3
            }
          }
    #combine financial aid estimates
      YCOCEst = rep(-3, nrow(LONG_DATA))
      TOTEst = rep(-3, nrow(LONG_DATA))
    #aggregate YCOC data
      for (i in 1:nrow(LONG_DATA)){
        YCOCEst[i] = max(aidList055[[i]])
        TOTEst[i] = YCOCEst[i]
        if (YCOCEst[i]<0){
          #check DLI data and replace if better
            DLIEst = max(aidListDLI[[i]])
            TOTEst[i]=max(YCOCEst[i], DLIEst) #less negative missing values are more informative
        }
      }
    #get best estimate of YSCH data
      YSCHEst = rep(-3, nrow(LONG_DATA))
      #aggregate YSCH data
        for (i in 1:nrow(LONG_DATA)){
          YSCHEst[i] = max(aidListYSCH[[i]])
        }
    #estimate all school aid
      ALLSCHOOLEst = rep(-3, nrow(LONG_DATA))
      for (i in 1:nrow(LONG_DATA)){
        ALLSCHOOLEst[i] = max(aidListALLSCHOOL[[i]])
      }
    #set -4's to zero since they mean not eligible for all school aid
      ALLSCHOOLEst[ALLSCHOOLEst==-4]=0
    #set -2's to zero since htey don't know their aid amount, so does not factor into decision
      ALLSCHOOLEst[ALLSCHOOLEst==-2]=0
    #set attendAid to zero if it is not know
      YSCHEst[YSCHEst <0]= 0
      LONG_DATA$attendAid = YSCHEst
      LONG_DATA$nonAttendAid = TOTEst  #note YTOT still has missing values, coded as -1 and -3 (missing) and -2 and -4 (don't know)
      LONG_DATA$allSchoolAid = ALLSCHOOLEst
    save.image("finAid.RData")
  #get school data--note that these generally have -3 missingness indicators
    SCHOOLDATA = read.table(file = "inputs/collegedataoutput2.txt", header = TRUE, sep = "\t")
    LONG_DATA = merge(x=LONG_DATA, y = SCHOOLDATA,  by.x = "AdmittedSchool", by.y= "collegeID", all.x = TRUE)
    LONG_DATA$totstudents = LONG_DATA$male_enrolled+LONG_DATA$female_enrolled
    LONG_DATA$expperstudent = LONG_DATA$totalexp/LONG_DATA$totstudents
    LONG_DATA$instperstudent = LONG_DATA$instspend/LONG_DATA$totstudents
    LONG_DATA$facperstudent = LONG_DATA$numfaculty/LONG_DATA$totstudents
    LONG_DATA$genderratio= LONG_DATA$male_enrolled/LONG_DATA$female_enrolled
    urbanInputs = c(11,12,13,21,22,23,31,32,33,41,42,43,-3,1,2,3,4,5,6,7,9,-3)
    urbanOutputs = c(1,1,1,1,2,2,2,2,2,3,3,3,-3,1,1,1,1,2,2,3,-3,-3)
    LONG_DATA$urbanrural = -3
    for (i in 1:nrow(LONG_DATA)){
      LONG_DATA$urbanrural[i]= urbanOutputs[match(LONG_DATA$locale[i],urbanInputs)]
    }
    #calculate interaction terms
      #instate
        LONG_DATA$instate = -3
        for (i in 1:nrow(LONG_DATA)){
          if(is.na(LONG_DATA$RES_STATE[i])){ #if we don't know residence state, they are out of state
            LONG_DATA$instate[i] = 0
          } else if(LONG_DATA$RES_STATE[i]>0 & LONG_DATA$state[i]>0){
            if(LONG_DATA$RES_STATE[i] == LONG_DATA$state[i]){
              LONG_DATA$instate[i] = 1
            } else{
              LONG_DATA$instate[i] = 0
            }
          }
        }
      #get true financial aid and tuition
        LONG_DATA$finaidest = -3
        LONG_DATA$realtui = -3
        for (i in 1:nrow(LONG_DATA)){
          if (LONG_DATA$nonAttendAid[i]<0 & LONG_DATA$attendAid[i]>0){ #replace nonattend aid with modeled non-attendaid for those who dont report it
            LONG_DATA$nonAttendAid[i]= LONG_DATA$attendAid[i]*(0.44536)+963.86273
          } 
          if (LONG_DATA$nonAttendAid[i]<0){ #if it is still less than zero, set to zero
            LONG_DATA$nonAttendAid[i]= 0
          }
          LONG_DATA$finaidest[i]= LONG_DATA$allSchoolAid[i] + LONG_DATA$nonAttendAid[i]
          if(LONG_DATA$instate[i]==1){
            LONG_DATA$realtui[i] = LONG_DATA$tuiinlist[i]+ LONG_DATA$feein[i]- LONG_DATA$finaidest[i]
          }else if (LONG_DATA$instate[i] ==0){
            LONG_DATA$realtui[i] = LONG_DATA$tuioutlist[i]+ LONG_DATA$feeout[i]- LONG_DATA$finaidest[i]
          }
        }
        #note thie yields some tuition less than 0
      #sporting division
        confInputs = c(103,102,104,112,113,114,362,117,127,204,306,198,133,111,200,126,155,191,359,318,119,356,213,323,141,144,148,153,161,163,195,192,308,159,130,366,354,201,205,121,320,321,182,175,115,167,170,172,176,180,183,185,197,202,134,171,128,193,351,137,181,352,302,311,337,309,315,353)
        confOutputs =c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4)
        LONG_DATA$division = -3
        for (i in 1:nrow(LONG_DATA)){
          matchInd = match(LONG_DATA$conference_no[i], confInputs)
          if (!is.na(matchInd)){
            LONG_DATA$division[i]= confOutputs[matchInd]  
          }
        }
      #ability match
        LONG_DATA$SATDiff = -3
        satInput = c(1,2,3,4,5,6,0, -3)
        satOutput = c(250,350,450,550,670,750,-3, -3)
        LONG_DATA$sat_math2 = satOutput[match(LONG_DATA$sat_math, satInput)]
        LONG_DATA$sat_verbal2 = satOutput[match(LONG_DATA$sat_verbal, satInput)]
        for (i in 1:nrow(LONG_DATA)){
          if(!is.na(LONG_DATA$sat_math2[i]) & !is.na(LONG_DATA$sat_verbal2[i]) & !is.na(LONG_DATA$sat75[i])){
            LONG_DATA$SATDiff[i] = LONG_DATA$sat75[i]-sum(LONG_DATA$sat_math[i], LONG_DATA$sat_verbal[i])
          }
        }
      #urban/rural match
        LONG_DATA$urbanruralmatch = -3
        for (i in 1:nrow(LONG_DATA)){
          if (!is.na(LONG_DATA$urbanrural[i]) & !is.na(LONG_DATA$URBAN_RURAL[i])){
            if(LONG_DATA$urbanrural[i] ==3 & LONG_DATA$URBAN_RURAL[i]==0){
              LONG_DATA$urbanruralmatch[i]=1
            } else if (LONG_DATA$urbanrural[i] %in% c(1,2) & LONG_DATA$URBAN_RURAL[i]==1){
              LONG_DATA$urbanruralmatch[i]=1
            }
            else if (LONG_DATA$urbanrural[i]>= 0 & LONG_DATA$URBAN_RURAL[i]>= 0){
              LONG_DATA$urbanruralmatch[i]=0
            }
          }
        }
      #distance from college to home
        #get students' lat and longitude
          LONG_DATA$latstudent = -3
          LONG_DATA$longstudent = -3
        #run in console
          fakeCBSA = c(2,3,4,5,6,7,8,9,10,12,13,15,16,17,20,21,22,23,25,26,27,28,30,31,32,33,34,35,36,37,38,39,40,42,43,44,45,46,48,49,50,51,52,53,54,55,56,57,58,59,60,61,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,256,257,258,259,260,261,262,263,264,265,266,267,268,269,270,271,272,273,274,275,276,277,278,279,280,281,282,283,284,285,286,287,288,289,290,291,292,293,294,295,296,297,298,299,300,301,302,303,304,305,306,307,308,309,310,311,312,313,314,315,316,317,318,319,320,321,322,323,324,325,326,327,328,329,330,331,332,333,334,335,336,337,338,339,340,341,342,343,344,345,346,347,348,349,350,351,352,353,354,355,356,357,358,359,360,361,362,363,364,365,366,367,368,369,370,371,372,373,374,375,376,377,378,379,380,381,382,383,384,385,386,387,388,389,390,391,392,393,394,395,396,397,398,399,400,401,402,403,404,405,407,408,409,410,411,412,413,414,415,416,417,418,419,420,421,422,423,424,425,426,427,428,429,430,431,432,433,434,435,436,437,438,439,440,441,442,443,444,445,446,447,448,449,450,451,452,453,454,455,456,457,459,460,461,462,463,464,465,466,467,468,469,470,471,472,473,474,475,476,477,478,479,480,481,482,483,484,485,486,487,488,489,490,491,492,493,494,495,496,497,498,499,500,501,502,503,504,505,506,507,508,509,510,511,512,513,514,515,516,517,518,519,520,521,522,523,524,525,526,527,528,529,530,531,532,533,534,535,536,537,538,539,540,541,542,543,544,545,546,547,548,549,550,551,552,553,554,555,556,557,558,559,560,561,562,563,564,565,566,567,568,569,570,571,572,573,574,575,576,577,578,579,580,581,582, 24,19,41,47,1,11,96,29,18,62,406, 14)
          latout = c(32.93,29.19,42.73,41.33,28.58,33.76,35.20,26.96,39.27,26.57,43.11,41.22,41.39,41.56,40.74,61.32,38.97,43.06,38.26,32.48,40.57,35.77,40.27,36.98,26.08,40.05,41.52,35.13,28.03,30.18,35.06,39.68,34.13,37.67,38.69,33.38,29.45,35.34,34.18,41.12,36.30,36.10,34.34,35.03,40.75,41.18,42.47,39.74,30.12,37.24,44.24,32.96,37.03,32.27,40.22,40.13,32.57,43.09,40.49,40.66,41.72,40.49,41.11,29.81,35.99,36.18,41.42,42.06,38.19,37.53,32.10,41.53,34.55,40.51,39.77,38.23,29.77,42.26,37.96,30.22,35.32,38.42,36.23,35.42,38.46,45.03,39.34,35.67,40.34,35.98,29.98,42.98,42.28,36.12,42.31,42.73,34.78,44.90,37.33,43.02,26.70,42.25,30.31,39.00,30.52,32.66,31.56,31.13,38.25,37.78,43.81,38.89,35.18,35.81,38.39,39.03,41.89,33.34,40.16,33.64,39.81,30.19,32.12,34.76,43.71,41.74,41.73,42.31,32.82,33.96,30.68,44.93,45.51,40.47,32.97,32.47,32.40,28.00,32.66,44.02,31.74,28.80,38.68,40.32,39.06,30.43,40.07,32.42,27.81,45.58,32.29,37.24,35.61,48.14,31.50,30.21,34.77,38.27,32.33,40.36,41.12,47.63,46.97,30.89,44.74,44.63,44.28,43.30,36.62,39.13,41.79,30.56,42.89,39.46,40.76,33.86,41.59,27.25,34.14,40.41,36.04,41.06,39.91,34.20,31.39,31.23,39.91,39.96,40.74,38.63,40.95,33.49,43.80,47.08,42.70,43.12,45.43,37.98,33.50,29.05,41.91,42.74,42.57,38.98,37.30,43.28,39.53,46.47,41.01,31.21,41.42,31.28,40.80,41.34,41.62,35.40,40.12,36.64,39.19,46.41,42.23,35.11,37.94,36.44,34.73,34.61,43.04,47.08,39.18,44.78,44.55,19.69,37.35,43.39,43.68,35.54,43.68,43.64,43.70,37.81,35.27,39.47,38.66,41.24,44.08,41.52,43.51,36.74,37.29,34.49,44.39,30.48,44.44,45.63,47.66,35.15,35.25,41.01,36.73,36.23,44.07,27.38,34.06,39.31,28.24,34.89,36.63,35.29,35.65,34.66,36.05,36.23,36.13,36.79,37.17,43.19,42.33,40.77,36.70,41.26,38.40,24.89,35.82,32.95,48.28,36.30,35.61,38.25,30.74,37.11,35.89,36.12,36.36,35.45,36.09,38.39,39.09,35.19,34.63,35.28,44.29,30.67,30.88,45.12,33.38,47.37,28.75,35.20,27.75,23.15,31.85,27.32,38.15,33.51,36.64,40.45,43.00,41.49,45.92,39.09,48.81,48.44,40.61,44.01,46.85,37.64,31.80,46.89,37.31,36.46,43.62,33.58,37.24,37.97,40.76,41.46,41.83,43.35,40.06,44.69,42.26,44.75,44.53,31.78,40.00,42.92,42.88,44.15,41.72,41.81,42.32,42.07,41.44,39.51,40.93,38.85,38.97,40.16,39.32,35.53,39.84,39.90,46.67,42.62,46.91,32.43,39.21,40.40,44.15,38.65,37.10,47.66,39.78,34.37,30.12,47.86,46.29,58.34,42.49,42.30,38.28,46.85,32.32,48.42,41.37,41.43,41.16,41.13,35.42,35.64,38.98,44.93,35.52,43.41,44.46,42.59,32.45,33.09,33.74,30.48,34.38,37.51,36.96,38.06,30.62,31.27,30.29,31.52,31.26,30.62,33.77,32.42,35.77,36.23,40.85,38.49,35.89,35.59,35.69,33.48,37.67,36.14,36.21,39.09,38.10,31.58,33.14,34.92,32.42,33.45,35.28,41.24,41.92,39.60,45.71,46.97,33.90,44.38,47.43,42.53,44.75,39.54,26.27,44.51,45.67,45.62,44.05,43.26,42.99,41.07,40.45,38.13,30.83,31.83,32.62,34.08,33.21,32.47,34.03,42.43,36.00,28.87,32.51,39.86,29.93,33.50,32.82,33.27,38.39,35.52,38.81,34.94,38.75,29.29,26.11,35.76,28.71,38.51,43.60,34.32,35.47,36.73,41.72,33.82,41.80,37.76,41.89,37.77,37.84,44.35,39.62,35.89,39.27,39.91,41.32,44.45,35.35,40.79,44.37,43.06,35.36,43.06,31.62,46.33,38.75,64.80,41.02,22.03,38.92,37.26,37.61,33.59,30.59,34.09,37.36,30.03,37.21,31.58,36.41,35.03,34.26,30.30,32.90,35.03,47.11,41.89,37.51,42.19,33.22,27.26,32.23,40.06,42.64,44.98,38.84,29.68,47.72,36.39,35.48,40.35,35.20,34.58,32.82,40.83,36.28,30.73,28.44,44.04,38.60,42.53,46.16,30.59,48.30,39.70,41.56,44.46,43.78,34.21,34.32,42.07,45.31,43.86,31.73,44.63,44.00,40.95,26.45,39.93,47.25,33.77,37.8,40.71,38.88,25.79,32.78,42.36,42.37,44.38, 41.79)
          longout = c(-116.96,-82.11,-73.92,-75.84,-81.41,-84.39,-80.86,-82.11,-76.58,-81.87,-76.01,-73.38,-72.94,-83.60,-75.42,-149.73,-95.38,-77.56,-75.73,-84.92,-105.23,-81.41,-74.71,-76.36,-81.59,-82.89,-74.12,-106.57,-82.53,-91.91,-85.29,-104.90,-116.74,-97.28,-121.24,-112.01,-98.50,-118.82,-119.10,-111.94,-85.48,-115.06,-85.21,-78.91,-89.55,-80.63,-96.39,-119.71,-94.19,-121.80,-103.00,-115.42,-122.01,-90.15,-88.26,-89.41,-92.26,-88.07,-90.20,-111.95,-86.13,-88.82,-87.91,-95.42,-83.94,-119.74,-81.71,-91.64,-88.85,-77.45,-81.19,-86.64,-87.15,-86.87,-86.18,-122.05,-91.42,-85.05,-121.26,-81.67,-120.62,-122.39,-119.08,-78.02,-122.88,-93.26,-75.11,-75.65,-76.93,-79.03,-90.06,-86.08,-83.76,-86.68,-72.59,-84.50,-82.33,-91.30,-79.27,-83.70,-81.29,-84.43,-97.72,-94.54,-91.11,-114.36,-97.14,-93.28,-76.58,-89.36,-72.26,-104.75,-77.60,-78.53,-108.16,-95.78,-111.88,-86.21,-111.79,-95.53,-84.22,-96.46,-111.22,-86.65,-70.46,-72.64,-71.38,-71.86,-83.64,-83.31,-88.15,-122.85,-122.64,-85.63,-80.05,-83.65,-95.33,-81.72,-100.88,-75.92,-106.35,-82.06,-90.32,-92.56,-92.42,-84.31,-105.28,-80.80,-97.43,-94.48,-106.80,-107.91,-111.62,-122.55,-85.88,-85.65,-92.33,-75.35,-97.86,-79.85,-81.41,-122.63,-122.82,-81.71,-88.06,-73.04,-72.55,-71.63,-82.40,-84.47,-73.21,-87.22,-78.80,-74.70,-81.86,-78.93,-85.88,-80.34,-79.83,-75.91,-79.87,-75.32,-77.18,-78.09,-100.50,-85.51,-76.69,-77.68,-76.22,-75.31,-78.46,-88.93,-91.25,-92.38,-89.06,-89.57,-89.66,-87.55,-86.84,-81.16,-84.07,-87.96,-92.48,-119.84,-89.70,-86.30,-77.89,-87.66,-85.19,-97.61,-85.48,-81.62,-86.26,-85.07,-85.08,-78.84,-85.71,-95.90,-85.83,-116.79,-85.78,-89.89,-78.60,-84.16,-77.42,-103.39,-85.50,-88.54,-86.76,-85.67,-122.86,-155.43,-93.24,-83.97,-83.98,-86.69,-84.42,-85.32,-84.87,-92.17,-79.49,-92.49,-93.77,-104.63,-92.39,-105.72,-96.90,-119.69,-79.95,-119.98,-96.80,-89.23,-103.76,-111.15,-117.41,-84.56,-94.42,-92.43,-93.23,-94.20,-88.69,-81.38,-80.98,-80.36,-80.67,-80.83,-87.52,-81.57,-80.55,-81.02,-79.44,-81.74,-80.27,-79.37,-80.57,-75.30,-123.48,-84.08,-79.97,-95.99,-97.61,-81.06,-83.56,-80.74,-101.20,-82.33,-87.13,-85.70,-86.66,-86.35,-84.38,-96.02,-97.79,-97.49,-96.90,-96.26,-75.56,-96.90,-98.44,-99.68,-88.36,-96.43,-83.22,-68.74,-94.13,-119.36,-97.02,-101.77,-98.04,-163.00,-102.50,-97.83,-104.53,-105.45,-108.24,-104.56,-106.54,-91.63,-112.71,-108.53,-122.35,-122.20,-123.95,-123.13,-116.75,-120.95,-81.56,-102.83,-120.67,-121.49,-116.36,-92.68,-113.43,-120.25,-122.10,-72.09,-71.97,-72.17,-76.22,-74.36,-73.64,-73.65,-75.14,-95.66,-78.98,-72.27,-71.64,-70.26,-89.38,-89.99,-89.08,-89.27,-90.48,-89.31,-88.59,-86.50,-85.64,-87.72,-96.58,-88.65,-94.82,-85.01,-94.21,-85.86,-96.99,-99.75,-121.46,-109.72,-94.10,-93.28,-94.31,-116.73,-89.64,-86.29,-93.25,-96.93,-96.83,-134.38,-90.87,-77.38,-81.62,-100.82,-86.36,-103.37,-83.22,-82.57,-82.65,-83.15,-79.22,-105.96,-97.66,-89.77,-82.56,-88.67,-89.39,-88.50,-88.70,-87.64,-85.85,-88.60,-85.40,-84.26,-85.79,-84.42,-92.00,-92.55,-92.43,-90.43,-89.22,-90.41,-90.85,-90.81,-80.86,-81.14,-82.32,-78.86,-77.82,-77.43,-77.89,-81.93,-85.93,-83.35,-82.87,-74.80,-84.96,-94.64,-95.53,-95.70,-94.84,-80.80,-114.11,-110.53,-86.50,-106.08,-108.57,-114.02,-80.34,-105.46,-111.38,-114.59,-106.94,-106.62,-98.18,-123.40,-119.04,-121.64,-121.22,-123.39,-84.15,-98.43,-86.10,-79.13,-95.55,-85.34,-85.30,-91.90,-92.63,-93.78,-93.16,-122.83,-101.91,-82.47,-82.83,-88.98,-91.02,-90.10,-90.34,-90.97,-105.39,-105.19)
          longout = c(longout,-83.01,-81.97,-87.33,-96.16,-97.58,-90.66,-100.47,-77.94,-73.05,-83.80,-84.59,-90.39,-70.28,-98.49,-76.50,-113.22,-83.51,-88.99,-90.49,-93.34,-110.71,-83.11,-120.83,-85.40,-89.23,-100.32,-81.86,-80.73,-92.56,-95.14,-80.30,-88.71,-84.08,-119.31,-85.80,-147.49,-76.45,-159.51,-99.23,-81.41,-92.60,-101.81,-87.73,-85.22,-85.28,-92.16,-96.80,-81.99,-80.68,-90.79,-88.73,-92.88,-103.37,-81.77,-123.84,-74.20,-84.75,-76.01,-95.01,-82.41,-95.84,-91.30,-76.08,-92.00,-85.45,-82.38,-120.21,-99.31,-81.24,-80.68,-82.79,-79.10,-105.83,-96.81,-76.33,-98.21,-97.78,-111.59,-92.15,-121.67,-118.40,-86.58,-114.27,-90.23,-93.81,-89.98,-88.49,-97.33,-89.51,-80.10,-121.13,-117.36,-109.95,-71.53,-91.74,-77.03,-97.67, -75.12,-122.44,-118.2,-122.27,-74.01,-77.11,-80.23,-96.8,-71.06,-83.35,-89.82, -88.15)
        #average lat and long across zip codes within this CBSA
          for (i in 1:nrow(LONG_DATA)){
            LONG_DATA$latstudent[i] = latout[match(LONG_DATA$GEO[i], fakeCBSA)]
            LONG_DATA$longstudent[i] = longout[match(LONG_DATA$GEO[i], fakeCBSA)]
          }
          LONG_DATA$distance = -3
          for (i in 1:nrow(LONG_DATA)){
            if (LONG_DATA$latitude[i] != -3 & !is.na(LONG_DATA$latstudent[i])){
              LONG_DATA$distance[i] = sqrt((LONG_DATA$latitude[i] -LONG_DATA$latstudent[i])^2+ (LONG_DATA$longitude[i] - LONG_DATA$longstudent[i])^2)
            }
          }
  #write and recode NAs
    keepVect = c("finaidest","realtui","AdmittedSchool","admit", "attend","PUBID_1997","average", "gender","GRADES","sat_math", "sat_verbal", "MAJOR2", "DAD_ED","MOM_ED","HH_SIZE", "HH_INCOME", "URBAN_RURAL", "SCHOOL_TYPE", "AttendedIndicator", "instate","SATDiff","urbanruralmatch","sat25", "sat75", "avgsal", "control", "selectivity", "fedgrantp", "gradrate", "loanp", "admitperc", "expperstudent", "instperstudent", "facperstudent", "genderratio", "division", "urbanrural", "distance", "totstudents", "attend2", "admit2", "carnegie")
    LONG_OUT = LONG_DATA[,keepVect]
    LONG_OUT[LONG_OUT$totstudents<0,]$totstudents= NA
    LONG_OUT[LONG_OUT$expperstudent<0,]$expperstudent= NA
    LONG_OUT[LONG_OUT$instperstudent<0,]$instperstudent= NA
    LONG_OUT[LONG_OUT$facperstudent<0,]$facperstudent= NA
    LONG_OUT[LONG_OUT$genderratio<0,]$genderratio= NA
    LONG_OUT[LONG_OUT$SATDiff<0,]$SATDiff= NA
    LONG_OUT[LONG_OUT$realtui==-6,]$realtui= NA
    LONG_OUT[LONG_OUT==-3]= NA
    write.csv(LONG_OUT, "longdataout.csv")
  #simplify datapoints
    #selectivity
      haveSelect = LONG_OUT[!is.na(LONG_OUT$selectivity),]
      noSelect =  LONG_OUT[is.na(LONG_OUT$selectivity),]
      selectPred.mod = lm(selectivity ~ avgsal+carnegie+admitperc + sat25+sat75, data = haveSelect)
      preAll = noSelect[!is.na(noSelect$avgsal)&!is.na(noSelect$carnegie)&!is.na(noSelect$admitperc)&!is.na(noSelect$sat25)& !is.na(noSelect$sat75),]
      select2Pred = round(predict(selectPred.mod,preAll))
      LONG_OUT[is.na(LONG_OUT$selectivity)&!is.na(LONG_OUT$avgsal)&!is.na(LONG_OUT$carnegie)&!is.na(LONG_OUT$admitperc)&!is.na(LONG_OUT$sat25)& !is.na(LONG_OUT$sat75),]$selectivity = select2Pred
      noSelect =  LONG_OUT[is.na(LONG_OUT$selectivity),]
      preAll = noSelect[!is.na(noSelect$avgsal)&!is.na(noSelect$carnegie),]
      selectPred.mod = lm(selectivity ~ avgsal+carnegie, data = haveSelect)
      select2Pred = round(predict(selectPred.mod,preAll))
      LONG_OUT[is.na(LONG_OUT$selectivity)&!is.na(LONG_OUT$avgsal)&!is.na(LONG_OUT$carnegie),]$selectivity = select2Pred
      #delete 19 with remaining no selectivity
        LONG_OUT = LONG_OUT[!is.na(LONG_OUT$selectivity),]
      #delete those with selectivity = 7
        LONG_OUT = LONG_OUT[LONG_OUT$selectivity!=7,]
      #fill selectivity difference
        LONG_OUT$selectdiff = LONG_OUT$selectivity-LONG_OUT$admit
        LONG_OUT[LONG_OUT$selectdiff<0,]$selectivity =LONG_OUT[LONG_OUT$selectdiff<0,]$admit #fix selectivity errors
        LONG_OUT[LONG_OUT$selectdiff<0,]$selectdiff =0 
      #interval selectivity difference
        LONG_OUT$selectdiffInt = NA
        LONG_OUT$selectInt = NA
        lookup = data.frame(id = c(1,2,3,4,5,6), percent = c(33, 60, 75, 85, 100, 100))
        for (i in 1:nrow(LONG_OUT)){
          LONG_OUT$selectInt[i] = lookup[(1:dim(lookup)[1])[lookup[,1]==LONG_OUT$selectivity[i]],2]
          LONG_OUT$selectdiffInt[i]=LONG_OUT$selectInt[i]-LONG_OUT$admit2[i]
        } 
    #simplified sports variable
      LONG_OUT$division2 = LONG_OUT$division
      LONG_OUT[is.na(LONG_OUT$division2),]$division2 = 0
      LONG_OUT[LONG_OUT$division2>1,]$division2 = 0
    #fix gradrates (manual fills--there may be more TBD)
      LONG_OUT[LONG_OUT$AdmittedSchool == 186399,]$gradrate = .51
      LONG_OUT[LONG_OUT$AdmittedSchool == 228644,]$gradrate = 1
    #salary premium/indicator for top tier school
      LONG_OUT$eliteInd = 0
      LONG_OUT[LONG_OUT$selectivity==1,]$eliteInd = 1
    #adjust control entry
      LONG_OUT[LONG_OUT$control==3,]$control = 2
    #output
      keyVars = c("PUBID_1997", "AdmittedSchool", "realtui","finaidest","admit","attend","average" ,"gender","GRADES", "sat_math","sat_verbal","MAJOR2","DAD_ED", "MOM_ED","HH_SIZE","HH_INCOME",  "SCHOOL_TYPE", "AttendedIndicator", "instate", "SATDiff", "urbanruralmatch", "sat25", "sat75", "avgsal", "control", "selectivity", "fedgrantp", "gradrate","loanp", "admitperc", "expperstudent", "instperstudent", "facperstudent", "genderratio", "division", "urbanrural","distance", "totstudents", "attend2", "admit2", "selectdiff", "selectdiffInt", "selectInt", "division2", "eliteInd")
      out.df = LONG_OUT[,keyVars]
      write.csv(out.df, "test.csv")
    save.image("dataset.RData")
#check number of schools significance==============================
  frequency.dat = data.frame(freq(ordered(LONG_OUT$PUBID_1997), plot=FALSE))
  frequency.dat$pubid= rownames(frequency.dat)
  withfreq.dat = merge(x = inc_dat, y = frequency.dat, by = "pubid", all.x = TRUE)
  regNum.dat = withfreq.dat[,c("pubid", "admit2","attend2", "average", "MAJOR", "sat_math", "HH_INCOME", "Frequency")]
  regNum.dat = na.exclude(regNum.dat)
  engineering <- c(9,13)
  bus <- c(7, 8)
  regNum.dat$EngInd= 0
  regNum.dat[regNum.dat$MAJOR %in% engineering,]$EngInd = 1
  regNum.dat$BusInd= 0
  regNum.dat[regNum.dat$MAJOR %in% bus,]$BusInd = 1
  regNum.dat$LowInd = 0
  regNum.dat[regNum.dat$MAJOR %in% c(3,11,12,16,26,31,33,32),]$LowInd = 1
  regNum.dat$HighInd = 0
  regNum.dat[regNum.dat$MAJOR %in% c(engineering,bus),]$HighInd =1
  #checkPred = lm(average~sat_math+HH_INCOME+factor(EngInd)+factor(BusInd), data = reg.dat)
  checkPred = lm(average~Frequency+sat_math+HH_INCOME+factor(LowInd)+factor(HighInd), data = regNum.dat)
#run first stage models==========================================================
  #get attenders
    attenders.dat = out.df[out.df$attend != -10,]
  #delete those with only 1 appearance
    frequency.dat = data.frame(freq(ordered(attenders.dat$PUBID_1997), plot=FALSE))
    frequency.dat$PUBID_1997= rownames(frequency.dat)
    attenders.dat = merge(x = attenders.dat, y = frequency.dat, by = "PUBID_1997", all.x = TRUE)
    multi.dat = attenders.dat[attenders.dat$Frequency>1,] 
  #set categorical variables
    multi.dat$instate = as.factor(multi.dat$instate)
    multi.dat$urbanruralmatch = as.factor(multi.dat$urbanruralmatch)
    multi.dat$control = as.factor(multi.dat$control)
    multi.dat$attend = as.factor(multi.dat$attend)
  #set up LHS
    #keep most relevant variables
      relVarsNoPCA.dat = multi.dat[,c("PUBID_1997","AttendedIndicator","realtui","instate","selectivity","gradrate","selectdiffInt","eliteInd","instperstudent","avgsal","expperstudent", "selectdiff", "selectInt")]
      relVarsNoPCA.dat = na.exclude(relVarsNoPCA.dat)
      lhsNoPCA = matrix(c(relVarsNoPCA.dat$AttendedIndicator, relVarsNoPCA.dat$PUBID_1997), nrow(relVarsNoPCA.dat), 2)  
      mclogit.mod = mclogit(lhsNoPCA~realtui+gradrate+instperstudent+selectdiffInt+eliteInd,data=relVarsNoPCA.dat, model = TRUE, )
      clogit.mod = clogit(AttendedIndicator~realtui+gradrate+selectdiff+strata(PUBID_1997),relVarsNoPCA.dat)
      ll = clogit.mod$loglik #first entry is intercept-only model, second is full model
      McFR2= 1-ll[2]/ll[1]
    #reduce model to the final variables
      relVarsNoPCA.dat = multi.dat[,c("PUBID_1997","AttendedIndicator","realtui","gradrate")]
      relVarsNoPCA.dat = na.exclude(relVarsNoPCA.dat)
      lhsNoPCA = matrix(c(relVarsNoPCA.dat$AttendedIndicator, relVarsNoPCA.dat$PUBID_1997), nrow(relVarsNoPCA.dat), 2)  
      mclogit.mod = mclogit(lhsNoPCA~I(realtui^2)+realtui+gradrate,data=relVarsNoPCA.dat, model = TRUE, )
      clogit.mod = clogit(AttendedIndicator~I(realtui^2)+realtui+gradrate+strata(PUBID_1997),relVarsNoPCA.dat)
        ll = clogit.mod$loglik #first entry is intercept-only model, second is full model
        McFR2= 1-ll[2]/ll[1]
        save(mclogit.mod, file = "mclogit.saved2")
        save(clogit.mod, file = "clogit.saved2")
        save(relVarsNoPCA.dat, file = "stage1input.saved2")
        save.image("finalmodels2.RData")
    #BTL model
      BTLdat =  multi.dat[,c("PUBID_1997","AttendedIndicator","realtui","distance","instperstudent","avgsal","selectdiffInt")]
      BTLdat= na.exclude(BTLdat)
      lhsBTL = matrix(c(BTLdat$AttendedIndicator, BTLdat$PUBID_1997), nrow(BTLdat), 2)  
      mclogit.btl.mod = mclogit(lhsBTL~realtui+I(realtui^2)+distance+I(distance^2)+instperstudent+I(instperstudent^2)+avgsal+selectdiffInt, model = TRUE,data= BTLdat )
      clogit.btl.mod = clogit(AttendedIndicator~realtui+I(realtui^2)+distance+I(distance^2)+instperstudent+I(instperstudent^2)+avgsal+selectdiffInt+ strata(PUBID_1997), BTLdat)
      pR2(clogit.btl.mod)
#run second stage models==========================================================
  #create dataset
    studentList = as.numeric(levels(as.factor(out.df$PUBID_1997)))
    model.dat = data.frame(PUBID_1997 = studentList)
    #select attended where possible
      attendSchools.dat = attenders.dat[attenders.dat$AttendedIndicator==1,]
      attenderList = as.numeric(levels(as.factor(attendSchools.dat$PUBID_1997)))
      model.dat = merge(x = model.dat, y = attendSchools.dat, by = "PUBID_1997", all.x = TRUE)
    #project attended school for non-attenders
      nonattenders.dat = out.df[!(out.df$PUBID_1997 %in% attenderList),]
      frequency.dat = data.frame(freq(ordered(nonattenders.dat$PUBID_1997), plot=FALSE))
      frequency.dat$PUBID_1997= rownames(frequency.dat)
      nonattenders.dat = merge(x = nonattenders.dat, y = frequency.dat, by = "PUBID_1997", all.x = TRUE)
    #those with only one school
      nonattendone.dat = nonattenders.dat[nonattenders.dat$Frequency==1,]
      #replace the corresponding rows in model.dat
      for (i in 1:nrow(nonattendone.dat)){
        curID = nonattendone.dat$PUBID_1997[i]
        for (j in 1:length(nonattendone.dat[i,])){
          model.dat[model.dat$PUBID_1997==curID,j]= nonattendone.dat[i,j]
        }
      }
    #those with multiple schools
      nonattendmulti.dat = nonattenders.dat[nonattenders.dat$Frequency>1,]
      predict.dat = nonattendmulti.dat
      #predict.dat = predict.dat[predict.dat$PUBID_1997 != 43204,] #drop bad observation
      #predict
        #remove two problematic observations
        predict.dat = predict.dat[!is.na(predict.dat$gradrate),]
        predProbs= predict(mclogit.mod, newdata = predict.dat, type = "link") #gives linear predictor as expected
        predict.dat$predProbs= exp(predProbs)
        sums.dat = ddply(predict.dat,~PUBID_1997,summarise,probSum=sum(predProbs))
        predict.dat = merge(x = predict.dat, y = sums.dat, by = "PUBID_1997", all.x = TRUE)
        predict.dat$prob = predict.dat$predProbs/predict.dat$probSum
      #select max probability row
        bestSchool.dat = ddply(predict.dat, .(PUBID_1997), function(subdf) {subdf[which.max(subdf$prob),]})
        bestSchool.dat = bestSchool.dat[,c(1:48)]
      #replace the corresponding rows in model.dat
        for (i in 1:nrow(bestSchool.dat)){
          curID = bestSchool.dat$PUBID_1997[i]
          for (j in 1:length(bestSchool.dat[i,])){
            model.dat[model.dat$PUBID_1997==curID,j]= bestSchool.dat[i,j]
          }
        }
    #set factors (there may be more relevant ones)
      model.dat$instate = as.factor(model.dat$instate)
      model.dat$urbanruralmatch = as.factor(model.dat$urbanruralmatch)
      model.dat$control = as.factor(model.dat$control)
      model.dat$attend = as.factor(model.dat$attend)
      model.dat$admit = as.factor(model.dat$admit)
      model.dat$gender = as.factor(model.dat$gender)
  #estimate model
      reducedDat = model.dat[,c( "AttendedIndicator", "gradrate", "admit2")]
      reducedDat = na.exclude(reducedDat)
      reduced.mod =glm(AttendedIndicator ~admit2+ gradrate, data = reducedDat, family = "binomial")
      pR2(reduced.mod)
  #BTL model    
      input.dat = model.dat[,c("AttendedIndicator","realtui", "distance","instperstudent")]
      input.dat = na.exclude(input.dat)
      btl.mod =glm(AttendedIndicator ~ realtui+I(realtui^2)+distance+I(distance^2)+instperstudent+I(instperstudent^2), data = input.dat, family = "binomial")
      pR2(btl.mod)