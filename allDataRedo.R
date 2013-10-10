#NOTES:======================================================================
  #this file redoes the analysis without throwing out the people discarded by the old projection method

#libraries===================================================================
  library(ggplot2)
  library(plyr)
  library(scales)

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
    BLS_DATA = read.csv("blsvarsforchoice.csv")
    inc_dat<- merge(x = inc_dat, y = BLS_DATA, by.x="pubid",by.y = "PUBID_1997", all.x = TRUE)
    keepCols = c("pubid","average","admit","attend","mse","attend2","admit2","gender","MAJOR","MAJOR2","GEO","GRADES","COLLEGECOMPLETED", "sat_math","sat_verbal")
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
        }
        keepCols = c(keepCols, "HH_SIZE", "HH_INCOME", "URBAN_RURAL","SCHOOL_TYPE","RES_STATE")
  #state or residence: TBD
    #state of residence in SCHOOL ATTENDANCE YEAR!
    #resStateVar = paste("GEO02_", toString(CHOICE_DATA$COLLEGEID_YEAR2[i]), sep = "")
    #if(resStateVar %in% colnames(RES_DATA)){
    #  if(RES_DATA[i,resStateVar]>0){
    #    CHOICE_DATA$RES_STATE[i]= RES_DATA[i, resStateVar]
    #  }else{
    #    CHOICE_DATA$RES_STATE[i]= fillMiss(resStateVar, i, TRUE, "START")
    #  }
    #} 
  #clean up
    inc_dat = inc_dat2[,keepCols]
    inc_dat[inc_dat==-3]=NA
    inc_dat[inc_dat==-4]=NA
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
  checkPred = lm(average~sat_math+HH_INCOME+factor(EngInd) +factor(BusInd), data = reg.dat)
  #major effect plot
    bus.dat = reg.dat[reg.dat$BusInd==1,]
    eng.dat = reg.dat[reg.dat$EngInd==1,]
    sal_byadmit_bus = data.frame(admit = c("1","2","3","4","5/6"),
      avgInc = aggregate(bus.dat$average, list(gp=bus.dat$admit2), mean)$x,
      avgIncse = aggregate(bus.dat$average, list(gp=bus.dat$admit2), se)$x)
    sal_byadmit_eng = data.frame(admit = c("1","2","3","4","5/6"),
       avgInc = aggregate(eng.dat$average, list(gp=eng.dat$admit2), mean)$x,
       avgIncse = aggregate(eng.dat$average, list(gp=eng.dat$admit2), se)$x)
    sal_byadmit_avg = data.frame(admit = c("1","2","3","4","5/6"),
      avgInc = aggregate(reg.dat$average, list(gp=reg.dat$admit2), mean)$x,
      avgIncse = aggregate(reg.dat$average, list(gp=reg.dat$admit2), se)$x)

#get data for choice models=========================================================
  choice.dat = read.table("inputs/studentadmitdata.txt", header = TRUE)
  LONG_DATA = merge(x = choice.dat, y = inc_dat, by.y = "pubid",by.x = "PUBID_1997", all.x = TRUE)
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
    #fill in all possible data using YSCH (attended school) data, since this is the best estimate we have
      count = 0
      for (i in 1:nrow(LONG_DATA)){
        if (TOTEst[i]<0 & YSCHEst[i] >= 0){ #we may want to do this only where YCOCEst == -3 implying that school was never included in applied list
          count = count+1
          TOTEst[i] = YSCHEst[i]
        }
      }
    #estimate all school aid
      ALLSCHOOLEst = rep(-3, nrow(LONG_DATA))
      for (i in 1:nrow(LONG_DATA)){
        ALLSCHOOLEst[i] = max(aidListALLSCHOOL[[i]])
      }
    #set -4's to zero since they mean not eligible for all school aid
      #-2 indicates don't know
      ALLSCHOOLEst[ALLSCHOOLEst==-4]=0
    #update people in weird categories
      for (i in 1:nrow(LONG_DATA)){
        if (TOTEst[i]==-4 & YCOCEst[i]==-200){ #these are 2003 people who said they did not know if they got aid in initial interview, then were ineligible for followup, coded to dont know
          TOTEst[i] = -200 #don't know about their aid when asked IF THEY GOT ANY
        }
        if (TOTEst[i]==-3 & YCOCEst[i]==-200){ #these are 2003 people who said they did not know if they got aid in initial interview, then their school did not appear in the DLI variables
          TOTEst[i] = -200 #don't know about their aid when asked IF THEY GOT ANY
        }
        if (TOTEst[i]== -3 & YCOCEst[i]==-200){#one person who was not eligible for aid
          TOTEst[i]= 0
        }
      }
      LONG_DATA$FINAIDEST = TOTEst
      LONG_DATA$AIDALLSCHOOL = ALLSCHOOLEst

#TBD START HERE


#get info on number applied to
anon.dat = read.csv("inputs/anon_data2.csv", stringsAsFactors=FALSE)
library(descr)
frequency.dat = data.frame(freq(ordered(anon.dat$pubid_anon), plot=FALSE))
frequency.dat$pubid_anon= rownames(frequency.dat)
withfreq.dat = merge(x = merge.dat, y = frequency.dat, by = "pubid_anon", all.x = TRUE)
reg.dat = withfreq.dat[,c("pubid_anon", "admit2", "avgb", "major", "SAT_MATH", "HH_INCOME", "Frequency")]
reg.dat = na.exclude(reg.dat)
reg.dat$EngInd= 0
reg.dat[reg.dat$major %in% engineering,]$EngInd = 1
reg.dat$HumInd= 0
reg.dat[reg.dat$major %in% hum,]$HumInd = 1
checkPred = lm(avgb~admit2+ Frequency+factor(EngInd)+ factor(HumInd)+ SAT_MATH, data = reg.dat)
