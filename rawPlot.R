library(ggplot2)

#create raw plot for introduction===========================================================
  #read data
    test= read.csv("inputs/rawinputs.csv")
    inc_dat = read.csv("inputs/rawinputs.csv")
    #inc_dat = read.csv("inputs/longsample.csv")
    #keep only 1 column for each person
    inc_dat = inc_dat[!duplicated(inc_dat[,c('pubid_anon')]),]
    
  #se function
    se <- function(data) {
      sqrt(var(data)/length(data))
    }

  #find each person's average salary
    #note that those with a b include 0 income, while those without a b treat it as missing
    inc_dat[inc_dat$income22==-3,]$income22=NA
    inc_dat[inc_dat$income23==-3,]$income23=NA
    inc_dat[inc_dat$income24==-3,]$income24=NA
    inc_dat[inc_dat$income25==-3,]$income25=NA
    inc_dat[inc_dat$income22b==-3,]$income22b=NA
    inc_dat[inc_dat$income23b==-3,]$income23b=NA
    inc_dat[inc_dat$income24b==-3,]$income24b=NA
    inc_dat[inc_dat$income25b==-3,]$income25b=NA
    inc_dat$avg = NA
    inc_dat$avgb = NA
    for (i in 1:nrow(inc_dat)){
      curVect = c(inc_dat$income22[i], inc_dat$income23[i], inc_dat$income24[i], inc_dat$income25[i])
      curVectb = c(inc_dat$income22b[i], inc_dat$income23b[i], inc_dat$income24b[i], inc_dat$income25b[i])
      if(length(na.exclude(curVect)>2)){
        inc_dat$avg[i] = mean(na.exclude(curVect))  
      }
      else{
        inc_dat$avg[i] = NA
        print("happens1")
      }
      if(length(na.exclude(curVectb)>2)){
        inc_dat$avgb[i] = mean(na.exclude(curVectb))  
      }
      else{
        inc_dat$avgb[i]= NA
        print("happens2")
      }
    }

    #remove rows that are missing averages
      inc_datb = inc_dat[!(is.na(inc_dat$avgb)),]
      inc_dat = inc_dat[!(is.na(inc_dat$avg)),]

    #find variance of average salary
      means = data.frame(attend = c("none","1","2","3","4","5/6"),
        avgInc = aggregate(inc_dat$avg, list(gp=inc_dat$attend), mean)$x,
        avgIncse = aggregate(inc_dat$avg, list(gp=inc_dat$attend), se)$x)
      meansb = data.frame(attend = c("none","1","2","3","4","5/6"),
        avgIncb = aggregate(inc_datb$avgb, list(gp=inc_datb$attend), mean)$x,
        avgIncbse = aggregate(inc_datb$avgb, list(gp=inc_datb$attend), se)$x)
      meansb_byadmit = data.frame(admit = c("1","2","3","4","5/6"),
        avgIncb = aggregate(inc_datb$avgb, list(gp=inc_datb$admit), mean)$x,
        avgIncbse = aggregate(inc_datb$avgb, list(gp=inc_datb$admit), se)$x)
    
    #plots
      #ggplot(means, aes(x=attend, y=avgInc, group =1)) + geom_line()+theme_bw()+xlab("Selectivity of school attended")+ylab("Average post-college income over sample") +theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(), text = element_text(size=20))+geom_ribbon(data=means,aes(ymin =avgInc - avgIncse*1.96, ymax = avgInc+avgIncse*1.96),alpha=0.3)
      #ggplot(meansb, aes(x=attend, y=avgIncb, group = 1)) + geom_line() +
        #geom_ribbon(data=meansb,aes(ymin =avgIncb - avgIncbse*1.96, ymax = avgIncb+avgIncbse*1.96),alpha=0.3)+xlab("Selectivity of school attended") +ylab("Average post-college income over sample") +theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(), text = element_text(size=20))

    #plotdata 
      plotdata= data.frame(attend = meansb$attend, avgIncb= meansb$avgIncb, avgIncbse = meansb$avgIncbse, type = 0)
      plotdata_admit = data.frame(admit = meansb_byadmit$admit, avgIncb= meansb_byadmit$avgIncb, avgIncbse = meansb_byadmit$avgIncbse, type = 0)
    
    #repeat analysis by type and store--by attendance
      #1
      inc_dat_temp = inc_dat[inc_dat$admit == 1,]
      inc_datb_temp = inc_datb[inc_datb$admit ==1,]
      means_temp = data.frame(attend = c("none","1", "2","3","4"),
         avgInc = aggregate(inc_dat_temp$avg, list(gp=inc_dat_temp$attend), mean)$x,
         avgIncse = aggregate(inc_dat_temp$avg, list(gp=inc_dat_temp$attend), se)$x)
      meansb_temp = data.frame(attend = c("none","1", "2","3","4"),
          avgIncb = aggregate(inc_datb_temp$avgb, list(gp=inc_datb_temp$attend), mean)$x,
          avgIncbse = aggregate(inc_datb_temp$avgb, list(gp=inc_datb_temp$attend), se)$x)
      curPlot = data.frame(attend = c("none","1", "2","3","4"), avgIncb = meansb_temp$avgIncb, avgIncbse = meansb_temp$avgIncbse, type = 1)
      plotdata = rbind(plotdata, curPlot)
    
      #2
      inc_dat_temp = inc_dat[inc_dat$admit == 2,]
      inc_datb_temp = inc_datb[inc_datb$admit ==2,]
      means_temp = data.frame(attend = c("none", "2","3","4","5/6"),
                              avgInc = aggregate(inc_dat_temp$avg, list(gp=inc_dat_temp$attend), mean)$x,
                              avgIncse = aggregate(inc_dat_temp$avg, list(gp=inc_dat_temp$attend), se)$x)
      meansb_temp = data.frame(attend = c("none", "2","3","4","5/6"),
                               avgIncb = aggregate(inc_datb_temp$avgb, list(gp=inc_datb_temp$attend), mean)$x,
                               avgIncbse = aggregate(inc_datb_temp$avgb, list(gp=inc_datb_temp$attend), se)$x)
      curPlot = data.frame(attend = c("none", "2","3","4","5/6"), avgIncb = meansb_temp$avgIncb, avgIncbse = meansb_temp$avgIncbse, type = 2)
      plotdata = rbind(plotdata, curPlot)

      #3
      inc_dat_temp = inc_dat[inc_dat$admit == 3,]
      inc_datb_temp = inc_datb[inc_datb$admit ==3,]
      means_temp = data.frame(attend = c("none","3","4","5/6"),
                              avgInc = aggregate(inc_dat_temp$avg, list(gp=inc_dat_temp$attend), mean)$x,
                              avgIncse = aggregate(inc_dat_temp$avg, list(gp=inc_dat_temp$attend), se)$x)
      meansb_temp = data.frame(attend = c("none","3","4","5/6"),
                               avgIncb = aggregate(inc_datb_temp$avgb, list(gp=inc_datb_temp$attend), mean)$x,
                               avgIncbse = aggregate(inc_datb_temp$avgb, list(gp=inc_datb_temp$attend), se)$x)
      curPlot = data.frame(attend = c("none","3","4","5/6"), avgIncb = meansb_temp$avgIncb, avgIncbse = meansb_temp$avgIncbse, type = 3)
      plotdata = rbind(plotdata, curPlot)

      #4
      inc_dat_temp = inc_dat[inc_dat$admit == 4,]
      inc_datb_temp = inc_datb[inc_datb$admit ==4,]
      means_temp = data.frame(attend = c("none","4","5/6"),
                              avgInc = aggregate(inc_dat_temp$avg, list(gp=inc_dat_temp$attend), mean)$x,
                              avgIncse = aggregate(inc_dat_temp$avg, list(gp=inc_dat_temp$attend), se)$x)
      meansb_temp = data.frame(attend = c("none","4","5/6"),
                               avgIncb = aggregate(inc_datb_temp$avgb, list(gp=inc_datb_temp$attend), mean)$x,
                               avgIncbse = aggregate(inc_datb_temp$avgb, list(gp=inc_datb_temp$attend), se)$x)
      curPlot = data.frame(attend = c("none","4","5/6"), avgIncb = meansb_temp$avgIncb, avgIncbse = meansb_temp$avgIncbse, type = 4)
      plotdata = rbind(plotdata, curPlot)

      #5
      inc_dat_temp = inc_dat[inc_dat$admit == 5,]
      inc_datb_temp = inc_datb[inc_datb$admit ==5,]
      means_temp = data.frame(attend = c("none","5/6"),
                              avgInc = aggregate(inc_dat_temp$avg, list(gp=inc_dat_temp$attend), mean)$x,
                              avgIncse = aggregate(inc_dat_temp$avg, list(gp=inc_dat_temp$attend), se)$x)
      meansb_temp = data.frame(attend = c("none","5/6"),
                               avgIncb = aggregate(inc_datb_temp$avgb, list(gp=inc_datb_temp$attend), mean)$x,
                               avgIncbse = aggregate(inc_datb_temp$avgb, list(gp=inc_datb_temp$attend), se)$x)
      curPlot = data.frame(attend = c("none","5/6"), avgIncb = meansb_temp$avgIncb, avgIncbse = meansb_temp$avgIncbse, type = 5)
      plotdata = rbind(plotdata, curPlot)

    #repeat analysis by type and store--by admission
    #-3
    inc_dat_temp = inc_dat[inc_dat$attend == -10,]
    inc_datb_temp = inc_datb[inc_datb$attend ==-10,]
    meansb_byadmit_temp = data.frame(admit = c("1","2","3","4","5/6"),
                            avgIncb = aggregate(inc_datb_temp$avgb, list(gp=inc_datb_temp$admit), mean)$x,
                            avgIncbse = aggregate(inc_datb_temp$avgb, list(gp=inc_datb_temp$admit), se)$x)
    means_byadmit_temp = data.frame(admit = c("1","2","3","4","5/6"),
                            avgInc = aggregate(inc_dat_temp$avg, list(gp=inc_dat_temp$admit), mean)$x,
                            avgIncse = aggregate(inc_dat_temp$avg, list(gp=inc_dat_temp$admit), se)$x)
    curPlot = data.frame(admit = c("1", "2","3","4","5/6"), avgIncb = meansb_byadmit_temp$avgIncb, avgIncbse = meansb_byadmit_temp$avgIncbse, type = -10)
    plotdata_admit = rbind(plotdata_admit, curPlot)

  #1
  inc_dat_temp = inc_dat[inc_dat$attend == 1,]
  inc_datb_temp = inc_datb[inc_datb$attend ==1,]
  meansb_byadmit_temp = data.frame(admit = c("1"),
                                   avgIncb = aggregate(inc_datb_temp$avgb, list(gp=inc_datb_temp$admit), mean)$x,
                                   avgIncbse = aggregate(inc_datb_temp$avgb, list(gp=inc_datb_temp$admit), se)$x)
  means_byadmit_temp = data.frame(admit = c("1"),
                                  avgInc = aggregate(inc_dat_temp$avg, list(gp=inc_dat_temp$admit), mean)$x,
                                  avgIncse = aggregate(inc_dat_temp$avg, list(gp=inc_dat_temp$admit), se)$x)
  curPlot = data.frame(admit = c("1"), avgIncb = meansb_byadmit_temp$avgIncb, avgIncbse = meansb_byadmit_temp$avgIncbse, type = 1)
  plotdata_admit = rbind(plotdata_admit, curPlot)

  #2
  inc_dat_temp = inc_dat[inc_dat$attend == 2,]
  inc_datb_temp = inc_datb[inc_datb$attend ==2,]
  meansb_byadmit_temp = data.frame(admit = c("1","2"),
                                   avgIncb = aggregate(inc_datb_temp$avgb, list(gp=inc_datb_temp$admit), mean)$x,
                                   avgIncbse = aggregate(inc_datb_temp$avgb, list(gp=inc_datb_temp$admit), se)$x)
  means_byadmit_temp = data.frame(admit = c("1","2"),
                                  avgInc = aggregate(inc_dat_temp$avg, list(gp=inc_dat_temp$admit), mean)$x,
                                  avgIncse = aggregate(inc_dat_temp$avg, list(gp=inc_dat_temp$admit), se)$x)
  curPlot = data.frame(admit = c("1", "2"), avgIncb = meansb_byadmit_temp$avgIncb, avgIncbse = meansb_byadmit_temp$avgIncbse, type = 2)
  plotdata_admit = rbind(plotdata_admit, curPlot)

  #3
  inc_dat_temp = inc_dat[inc_dat$attend == 3,]
  inc_datb_temp = inc_datb[inc_datb$attend ==3,]
  meansb_byadmit_temp = data.frame(admit = c("1","2","3"),
                                   avgIncb = aggregate(inc_datb_temp$avgb, list(gp=inc_datb_temp$admit), mean)$x,
                                   avgIncbse = aggregate(inc_datb_temp$avgb, list(gp=inc_datb_temp$admit), se)$x)
  means_byadmit_temp = data.frame(admit = c("1","2","3"),
                                  avgInc = aggregate(inc_dat_temp$avg, list(gp=inc_dat_temp$admit), mean)$x,
                                  avgIncse = aggregate(inc_dat_temp$avg, list(gp=inc_dat_temp$admit), se)$x)
  curPlot = data.frame(admit = c("1", "2","3"), avgIncb = meansb_byadmit_temp$avgIncb, avgIncbse = meansb_byadmit_temp$avgIncbse, type = 3)
  plotdata_admit = rbind(plotdata_admit, curPlot)

  #4
  inc_dat_temp = inc_dat[inc_dat$attend ==4,]
  inc_datb_temp = inc_datb[inc_datb$attend ==4,]
  meansb_byadmit_temp = data.frame(admit = c("1","2","3","4"),
                                   avgIncb = aggregate(inc_datb_temp$avgb, list(gp=inc_datb_temp$admit), mean)$x,
                                   avgIncbse = aggregate(inc_datb_temp$avgb, list(gp=inc_datb_temp$admit), se)$x)
  means_byadmit_temp = data.frame(admit = c("1","2","3","4"),
                                  avgInc = aggregate(inc_dat_temp$avg, list(gp=inc_dat_temp$admit), mean)$x,
                                  avgIncse = aggregate(inc_dat_temp$avg, list(gp=inc_dat_temp$admit), se)$x)
  curPlot = data.frame(admit = c("1", "2","3","4"), avgIncb = meansb_byadmit_temp$avgIncb, avgIncbse = meansb_byadmit_temp$avgIncbse, type = 4)
  plotdata_admit = rbind(plotdata_admit, curPlot)

  #5
  inc_dat_temp = inc_dat[inc_dat$attend ==5,]
  inc_datb_temp = inc_datb[inc_datb$attend ==5,]
  meansb_byadmit_temp = data.frame(admit = c("2","3","4","5"),
                                   avgIncb = aggregate(inc_datb_temp$avgb, list(gp=inc_datb_temp$admit), mean)$x,
                                   avgIncbse = aggregate(inc_datb_temp$avgb, list(gp=inc_datb_temp$admit), se)$x)
  means_byadmit_temp = data.frame(admit = c("2","3","4","5"),
                                  avgInc = aggregate(inc_dat_temp$avg, list(gp=inc_dat_temp$admit), mean)$x,
                                  avgIncse = aggregate(inc_dat_temp$avg, list(gp=inc_dat_temp$admit), se)$x)
  curPlot = data.frame(admit = c("2","3","4","5"), avgIncb = meansb_byadmit_temp$avgIncb, avgIncbse = meansb_byadmit_temp$avgIncbse, type = 5)
  plotdata_admit = rbind(plotdata_admit, curPlot)


      #plotting by attend
      ggplot(plotdata, aes(x=attend, y = avgIncb)) + 
          geom_bar(data = plotdata[plotdata$type==3,],stat='identity', colour = 'black',fill = NA) + 
          geom_line(data = plotdata[plotdata$type==0,], aes(group= 1), colour = 'blue')+
          geom_line(data = plotdata[plotdata$type==0,], aes(y = avgIncb-avgIncbse*1.96,group= 1),colour = 'grey')+
          geom_line(data = plotdata[plotdata$type==0,], aes(y = avgIncb+avgIncbse*1.96,group= 1), colour = 'grey')+
          #geom_ribbon(data = plotdata[plotdata$type==0,],aes(ymin=avgIncb-avgIncbse*1.96, ymax = avgIncb+avgIncbse*1.96),alpha=0.3)+
          #geom_errorbar(data = plotdata[plotdata$type==0,],aes(ymin=avgIncb-avgIncbse*1.96, ymax = avgIncb+avgIncbse*1.96))+
          theme_bw()+
          xlab("Selectivity of school attended") +
          ylab("Average income over ages 22-25") +
          theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(), text = element_text(size=20))+
          geom_errorbar(data= plotdata[plotdata$type==3,],aes(ymin=avgIncb-avgIncbse*1.96, ymax = avgIncb+avgIncbse*1.96))
          

        #plotting by admit
        ggplot(plotdata_admit, aes(x=admit, y = avgIncb)) + 
          geom_bar(data = plotdata_admit[plotdata_admit$type==5,],stat='identity', colour = 'black',fill = NA) + 
          geom_line(data = plotdata_admit[plotdata_admit$type==0,], aes(group= 1), colour = 'blue')+
          geom_line(data = plotdata_admit[plotdata_admit$type==0,], aes(y = avgIncb-avgIncbse*1.96,group= 1),colour = 'grey')+
          geom_line(data = plotdata_admit[plotdata_admit$type==0,], aes(y = avgIncb+avgIncbse*1.96,group= 1), colour = 'grey')+
          #geom_ribbon(data = plotdata[plotdata$type==0,],aes(ymin=avgIncb-avgIncbse*1.96, ymax = avgIncb+avgIncbse*1.96),alpha=0.3)+
          #geom_errorbar(data = plotdata[plotdata$type==0,],aes(ymin=avgIncb-avgIncbse*1.96, ymax = avgIncb+avgIncbse*1.96))+
          theme_bw()+
          xlab("Selectivity of top school admitted to") +
          ylab("Average income over ages 22-25") +
          theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(), text = element_text(size=20))+
          geom_errorbar(data= plotdata_admit[plotdata_admit$type==5,],aes(ymin=avgIncb-avgIncbse*1.96, ymax = avgIncb+avgIncbse*1.96))

      #single plots, no longer in use
      #ggplot(means_temp, aes(x=attend, y=avgInc)) + geom_bar(stat='identity', fill = 'grey') +
        #geom_errorbar(aes(ymin=avgInc - avgIncse*1.96, ymax = avgInc+avgIncse*1.96))+theme_bw()+xlab("Selectivity of school attended") +ylab("Average income over ages 22-25") +theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(), text = element_text(size=20)) +  geom_line(meansb, aes(x=attend, y=avgIncb, group = 1))
      #ggplot(meansb_temp, aes(x=attend, y=avgIncb)) + geom_bar(stat='identity', fill = 'grey') +
        #geom_errorbar(aes(ymin=avgIncb - avgIncbse*1.96, ymax = avgIncb+avgIncbse*1.96))+theme_bw()+xlab("Selectivity of school attended") +ylab("Average post-college income") +theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(), text = element_text(size=20))

      #regression
        #ignore non-attenders
          plotdata_reg= plotdata[plotdata$attend != "none",]
        #add "conditional" tag
          plotdata_reg$conditional=1
          plotdata_reg[plotdata_reg$type == 0,]$conditional = 0
        #regressions
          #plotdata_reg= plotdata_reg[plotdata_admit$type != -10,]
          plotdata_reg$attend = as.numeric(plotdata_reg$attend)
          byattend_reg=lm(avgIncb~attend+as.factor(type)+I(attend*conditional), data= plotdata_reg)
          lapply(plotdata_reg, class)
          byattend_noncon_reg = lm(avgIncb~attend, data = plotdata_reg[plotdata_reg$conditional == 0,])
          byattend_con_reg = lm(avgIncb~attend, data = plotdata_reg[plotdata_reg$conditional == 1,])
        #add conditional tag and regressions for by admit
          plotdata_admit= plotdata_admit[plotdata_admit$type != -10,]
          plotdata_admit$conditional=1
          plotdata_admit[plotdata_admit$type == 0,]$conditional = 0
          plotdata_admit$admit = as.numeric(plotdata_admit$admit)
          byadmit_reg=lm(avgIncb~admit+I(admit*conditional), data= plotdata_admit)
          byadmit_reg=lm(avgIncb~admit+as.factor(type)+I(admit*conditional), data= plotdata_admit)
          lapply(plotdata_reg, class)
          byadmit_noncon_reg = lm(avgIncb~admit, data= plotdata_admit[plotdata_admit$conditional == 0,])
          byadmit_con_reg = lm(avgIncb~admit, data= plotdata_admit[plotdata_admit$conditional == 1,])

  #plot all-in-one==========================================================
    ggplot(plotdata_admit, aes(x=admit, y = avgIncb, color = as.factor(type))) + 
      scale_color_brewer(palette="Spectral")+
      geom_line()+
      #scale_fill_brewer()
      #geom_line(data = plotdata_admit[plotdata_admit$type==0,], aes(group= 1), colour = 'blue')+
      theme_bw()+
  xlab("Selectivity of top school admitted to") +
  ylab("Average income over ages 22-25")+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(), text = element_text(size=20))


ggplot(plotdata_reg, aes(x=attend, y = avgIncb, color = as.factor(type))) + 
  scale_color_brewer(palette="Spectral")+
  geom_line()+
  #scale_fill_brewer()
  #geom_line(data = plotdata_admit[plotdata_admit$type==0,], aes(group= 1), colour = 'blue')+
  theme_bw()+
  xlab("Selectivity of top school attended") +
  ylab("Average income over ages 22-25")+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(), text = element_text(size=20))

  #interval data error regressions=============================================
    #check the importance of admission versus attendance
      inc_attend = inc_datb[inc_datb$attend != -10,]
      inc_attend$attend2 = NA
      inc_attend$admit2 = NA
      lookup = data.frame(id = c(1,2,3,4,5), percent = c(33, 60, 75, 85, 100))
      for (i in 1:nrow(inc_attend)){
        inc_attend$attend2[i] = lookup[(1:dim(lookup)[1])[lookup[,1]==inc_attend$attend[i]],2]
        inc_attend$admit2[i] = lookup[(1:dim(lookup)[1])[lookup[,1]==inc_attend$admit[i]],2]
      }
    test1 = lm(admit2~attend2, data = inc_attend)
    inc_attend$admitError = test1$residuals
    test2 = lm(attend2~admit2, data = inc_attend)
    inc_attend$attendError = test2$residuals
    err1 = lm(avgb~attend2+admitError, data = inc_attend)
    err2 = lm(avgb~admit2+attendError, data = inc_attend)

    predInc = lm(avgb~admit2+attend2, data = inc_attend)

##check for differences in salary by admission/attendance using interval/erros===============================
  #source("fun_checkPredictionAbilityInterval.R")
  #source("fun_checkPredictionAbilityIntervalErrors.R")
  #checkPredictionAbilityInterval(intDataEmploy10K, "employ10K")
  #AllFactor =lm(b0~factor(attend2 +factor(collgrad) + factor(major2)+ factor(collgrad) + satm +satv , data=na.exclude(intDataEmploy10K))
   
#recreate old table 4=============================================================================
    admit_cats <- c(1,2, 3, 4, 5)
    attend_cats <- c(-10,1,2, 3, 4, 5)
    #get by admission
    byAdmitCoeffVect = data.frame(matrix(ncol = 15, nrow = length(admit_cats)))
    colnames(byAdmitCoeffVect)=c("intercept","1", "2", "3", "4", "6", "intsig", "1sig", "2sig", "3sig", "4sig", "6sig","R2", "NumObservations", "levels")
    #inc_dat[is.na(inc_dat)] <- -3
    for (i in 1:length(admit_cats)){
      curData = inc_dat[inc_dat$admit==admit_cats[i],]
      curCount = nrow(curData)
      if (curCount >0){
        #curData[curData$attend == -10,]$attend <- -4 #change attends to -4 so they dont get deleted
        #curData[curData == -3] <- NA
        curData<-curData[c("avgb", "attend")]
        curCount = nrow(na.exclude(curData))
        numCoeffs <- length(levels(factor(curData$attend)))
        if (numCoeffs >1 & curCount > numCoeffs){
          #curData<-curData[c("avgb", "attend")]
          curModel = lm(avgb~factor(attend), data = na.exclude(curData))
          numCoeffs <- length(curModel$coefficients)
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
    
    #get by attendance
    byAttendCoeffVect = data.frame(matrix(ncol = 13, nrow = length(admit_cats)))
    colnames(byAttendCoeffVect)=c("intercept", "2", "3", "4", "6", "intsig", "2sig", "3sig", "4sig", "6sig","R2", "NumObservations", "levels")
    for (i in 1:length(attend_cats)){
      curData = inc_dat[inc_dat$attend==attend_cats[i],]
      curCount = nrow(curData)
      if (curCount >0){
        #if (i == 1){
        #  curData[curData$attend == -3,]$attend <- -4 #change attends to -4 so they dont get deleted
        #}
        #curData[curData == -3] <- NA
        curData<-curData[c("avgb", "admit")]
        curCount = nrow(na.exclude(curData))
        numCoeffs <- length(levels(factor(curData$admit)))
        if (numCoeffs >1 & curCount > numCoeffs){
          curModel = lm(avgb~factor(admit), data = na.exclude(curData))
          numCoeffs <- length(curModel$coefficients)
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
                
    #check significance of attending at all=======================
      inc_dat$attendInd = NA
      for (i in 1:nrow(inc_dat)){
        if(inc_dat$attend[i]==-10){
          inc_dat$attendInd[i] = 0
        } else{
          inc_dat$attendInd[i] = 1
        }
      }
      
      #run checkPredictionAbility using this indicator
      source("fun_checkPredictionAbilityAttendOnly.R")
      coeffs = checkPredictionAbilityAttendOnly(inc_dat)
      
    #make the differences plot for attendance choice section-==========
      #make sure there is a difference in salary by admissions category
      attenders = inc_dat[inc_dat$attendInd == 1,]
      nonattenders = inc_dat[inc_dat$attendInd == 0,]
      anova(lm(avgb~factor(admit), data = attenders))
      anova(lm(avgb~factor(admit), data = nonattenders)) #seems there is no difference in nonattenders
      pairwise.t.test(attenders$avgb, factor(attenders$admit), p.adj = "none")
      pairwise.t.test(nonattenders$avgb, factor(nonattenders$admit), p.adj = "none")
      #qplot(factor(attenders), avgb, data = na.exclude(attenders), notch= TRUE, geom = "boxplot", position = "dodge")+theme_bw()
      aggregate(attenders$avgb, list(gp=attenders$admit), mean)
      means = data.frame(admit = c("1","2","3","4","5/6"),
         attend = aggregate(attenders$avgb, list(gp=attenders$admit), mean)$x,
         attendse = aggregate(attenders$avgb, list(gp=attenders$admit), se)$x,
         nonattend = aggregate(nonattenders$avgb, list(gp=nonattenders$admit), mean)$x,
         nonattendse = aggregate(nonattenders$avgb, list(gp=nonattenders$admit), se)$x)
      means = within(means, {
        diff <- attend - nonattend
        diffse <- sqrt(attendse^2 + nonattendse^2)
      })
      ggplot(means, aes(x=admit, y=diff)) + geom_bar(stat='identity', fill = 'grey') +
        geom_errorbar(aes(ymin=diff - diffse*1.96, ymax = diff+diffse*1.96))+theme_bw()+xlab("Selectivity of the top school admitted to") +ylab("Income difference, (attenders)-(non-attenders)") +theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(), text = element_text(size=20)) +scale_y_continuous(labels = comma)
      
      ggplot(means, aes(x=admit, y=attend)) + geom_bar(stat='identity', fill = 'grey') +
        geom_errorbar(aes(ymin=attend - attendse*1.96, ymax = attend+attendse*1.96))+theme_bw()+xlab("Selectivity of the top school admitted to") +ylab("Average income for attenders")+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(), text = element_text(size=20))+scale_y_continuous(labels = comma) 
      
    #do some pairwise t-tests just to check no strong differences===========================================
      admit1 = inc_dat[inc_dat$admit == 1,]
        pairwise.t.test(admit1$avgb, factor(admit1$attend), p.adj = "none")
                
    #redo old table 3==================================================
      admitModel = lm(avg~factor(admit), data = na.exclude(inc_dat))
      attendModel = lm(avg~factor(attend), data = na.exclude(inc_dat))
                
    #variance===========================================================
          #get mean income across admissions group
    
    #merge in choice data============================================
        anon.dat = read.csv("personaldata.csv", stringsAsFactors=FALSE)
        #test = anon.dat[c("pubid_anon","KEYSEX_1997"),]
        anon.dat = anon.dat[,c("pubid_anon", "KEYSEX_1997", "KEYRACE_ETHNICITY_1997", "MOM_ED", "HH_SIZE", "HH_INCOME", "URBAN_RURAL", "SAT_MATH")]
        merge.dat = merge(x = inc_dat, y = anon.dat, by = "pubid_anon", all.x = TRUE)
          merge.dat$attend2 = NA
          merge.dat$admit2 = NA
          merge.dat= merge.dat[merge.dat$attend != -10,]
          lookup = data.frame(id = c(1,2,3,4,5), percent = c(33, 60, 75, 85, 100))
          for (i in 1:nrow(merge.dat)){
            merge.dat$attend2[i] = lookup[(1:dim(lookup)[1])[lookup[,1]==merge.dat$attend[i]],2]
            merge.dat$admit2[i] = lookup[(1:dim(lookup)[1])[lookup[,1]==merge.dat$admit[i]],2]
          }
        reg.dat = merge.dat[,c("pubid_anon", "HH_INCOME", "MOM_ED", "admit2", "avgb")]
        reg.dat = na.exclude(reg.dat)
        checkPred = lm(avgb~admit2+HH_INCOME+MOM_ED, data = reg.dat)
        checkPred = lm(avgb~admit2+factor(KEYSEX_1997)+ factor(KEYRACE_ETHNICITY_1997)+ MOM_ED + HH_INCOME, data = merge.dat)
  
  #check on majors====================================
    anon.dat = read.csv("personaldata.csv", stringsAsFactors=FALSE)
    anon.dat = anon.dat[,c("pubid_anon", "MOM_ED", "HH_INCOME", "major2", "major", "SAT_MATH")]
    merge.dat = merge(x = inc_dat, y = anon.dat, by = "pubid_anon", all.x = TRUE)
    merge.dat$attend2 = NA
    merge.dat$admit2 = NA
    merge.dat= merge.dat[merge.dat$attend != -10,]
    lookup = data.frame(id = c(1,2,3,4,5), percent = c(33, 60, 75, 85, 100))
    for (i in 1:nrow(merge.dat)){
      merge.dat$attend2[i] = lookup[(1:dim(lookup)[1])[lookup[,1]==merge.dat$attend[i]],2]
      merge.dat$admit2[i] = lookup[(1:dim(lookup)[1])[lookup[,1]==merge.dat$admit[i]],2]
    }
    #adjust major variable to be binary--1 is technical, 2 is not
      merge.dat$major3 = NA
      merge.dat[merge.dat$major2 %in% c(5),]$major3 = 1
      merge.dat[merge.dat$major2 %in% c(4,1,2,3),]$major3 = 0
    reg.dat = merge.dat[,c("pubid_anon", "HH_INCOME", "MOM_ED", "admit2", "avgb", "major3", "major2", "SAT_MATH")]
    reg.dat = na.exclude(reg.dat)
    checkPred = lm(avgb~admit2+HH_INCOME+MOM_ED+ factor(major3)+SAT_MATH, data = reg.dat)
    checkPred = lm(avgb~HH_INCOME+MOM_ED+ factor(major3), data = reg.dat)
    summary(checkPred)
    #get original major variable--that is "major"
      #check prediction ability of each one
        reg.dat = merge.dat[,c("pubid_anon", "admit2", "avgb", "major", "HH_INCOME","MOM_ED", "SAT_MATH")]
        reg.dat[merge.dat$major==0,]$major = NA
        reg.dat[merge.dat$major==99,]$major = NA
        reg.dat = na.exclude(reg.dat)
        majList = levels(as.factor(reg.dat$major))
        lessList = c()
        moreList = c()
        for (i in majList){
          reg.dat$major3 = 0
          reg.dat[reg.dat$major == i,]$major3 =1
          checkPred = lm(avgb~admit2+ factor(major3), data = reg.dat)
          getVal = summary(checkPred)$coefficients[3,1]
          getP = summary(checkPred)$coefficients[3,4]
          if(getP <.05){
            if(getVal > 0){
              moreList = c(moreList, i)
            }
            else{
              lessList = c(lessList,i)
            }
          }
        }
    #convert to categorical
      hardSci <- c(6, 21, 25)
      softSci <- c(3, 10, 11, 31, 32)
      bus <- c(7, 8)
      engineering <- c(9,13)
      health <- c(22, 23, 27, 29, 30, 28)
      hum <- c(1,2,5,12,14,15,17,19,18,20,24,26,33,4,16)
      reg.dat$major2 = NA
      for (i in 1:nrow(reg.dat)){
        if (reg.dat$major[i] %in% hardSci){
          reg.dat$major2[i]= 1
        }
        else if (reg.dat$major[i] %in% softSci){
          reg.dat$major2[i]= 2
        }
        else if (reg.dat$major[i] %in% bus){
          reg.dat$major2[i]= 3
        }
        else if (reg.dat$major[i] %in% engineering){
          reg.dat$major2[i]= 4
        }
        else if (reg.dat$major[i] %in% health){
          reg.dat$major2[i]= 5
        }
        else if (reg.dat$major[i] %in% hum){
          reg.dat$major2[i]= 6
        }
      }
      majList = levels(as.factor(reg.dat$major2))
      lessList = c()
      moreList = c()
      for (i in majList){
        reg.dat$major3 = 0
        reg.dat[reg.dat$major2 == i,]$major3 =1
        checkPred = lm(avgb~admit2+ factor(major3), data = reg.dat)
        getVal = summary(checkPred)$coefficients[3,1]
        getP = summary(checkPred)$coefficients[3,4]
        if(getP <.05){
          if(getVal > 0){
            moreList = c(moreList, i)
          }
          else{
            lessList = c(lessList,i)
          }
        }
      }
    #get final regression
      reg.dat = merge.dat[,c("pubid_anon", "admit2", "avgb", "major", "SAT_MATH", "HH_INCOME")]
reg.dat = merge.dat[,c("pubid_anon", "avgb","admit2","attend2", "major", "SAT_MATH")]
      reg.dat = na.exclude(reg.dat)
      reg.dat$EngInd= 0
      reg.dat[reg.dat$major %in% engineering,]$EngInd = 1
      reg.dat$HumInd= 0
      reg.dat[reg.dat$major %in% hum,]$HumInd = 1
      checkPred = lm(avgb~SAT_MATH+attend2+SAT_MATH, data = reg.dat)
    #plot
      hum.dat = reg.dat[reg.dat$HumInd==1,]
      eng.dat = reg.dat[reg.dat$EngInd==1,]
      sal_byadmit_hum = data.frame(admit = c("1","2","3","4","5/6"),
        avgIncb = aggregate(hum.dat$avgb, list(gp=hum.dat$admit2), mean)$x,
        avgIncbse = aggregate(hum.dat$avgb, list(gp=hum.dat$admit2), se)$x)
      sal_byadmit_eng = data.frame(admit = c("1","2","3","4","5/6"),
        avgIncb = aggregate(eng.dat$avgb, list(gp=eng.dat$admit2), mean)$x,
        avgIncbse = aggregate(eng.dat$avgb, list(gp=eng.dat$admit2), se)$x)
      sal_byadmit_avg = data.frame(admit = c("1","2","3","4","5/6"),
        avgIncb = aggregate(reg.dat$avgb, list(gp=reg.dat$admit2), mean)$x,
        avgIncbse = aggregate(reg.dat$avgb, list(gp=reg.dat$admit2), se)$x)

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

#plots based SAT instead of top admitted
  #merge SAT into inc_datb
    satplot.dat = read.csv("satplotin.csv")
    satplot.dat = satplot.dat[,c("pubid_anon", "attend", "admit", "avg", "avgb", "SAT_MATH")]
    satplot.dat = na.exclude(satplot.dat)
  #find variance of average salary
  meansb = data.frame(attend = c("none","1","2","3","4","5/6"),
    avgIncb = aggregate(satplot.dat$avgb, list(gp=satplot.dat$attend), mean)$x,
    avgIncbse = aggregate(satplot.dat$avgb, list(gp=satplot.dat$attend), se)$x)
  meansb_bysat = data.frame(sat = c("1","2","3","4","5","6"),
    avgIncb = aggregate(satplot.dat$avgb, list(gp=satplot.dat$SAT_MATH), mean)$x,
    avgIncbse = aggregate(satplot.dat$avgb, list(gp=satplot.dat$SAT_MATH), se)$x)
  
  #plotdata 
    plotdata= data.frame(attend = meansb$attend, avgIncb= meansb$avgIncb, avgIncbse = meansb$avgIncbse, type = 0)
    plotdata_sat = data.frame(sat = meansb_bysat$sat, avgIncb= meansb_bysat$avgIncb, avgIncbse = meansb_bysat$avgIncbse, type = 0)

  #repeat analysis by type and store--by attendance
    #1
      satplot.dat_temp = satplot.dat[satplot.dat$SAT_MATH ==1,]
      myvect = levels(as.factor(satplot.dat_temp$attend))
      meansb_temp = data.frame(attend = myvect,
        avgIncb = aggregate(satplot.dat_temp$avgb, list(gp=satplot.dat_temp$attend), mean)$x,
        avgIncbse = aggregate(satplot.dat_temp$avgb, list(gp=satplot.dat_temp$attend), se)$x)
      curPlot = data.frame(attend = myvect, avgIncb = meansb_temp$avgIncb, avgIncbse = meansb_temp$avgIncbse, type = 1)
      plotdata = rbind(plotdata, curPlot)

    #2
      satplot.dat_temp = satplot.dat[satplot.dat$SAT_MATH ==2,]
      myvect = levels(as.factor(satplot.dat_temp$attend))
      meansb_temp = data.frame(attend = myvect,
       avgIncb = aggregate(satplot.dat_temp$avgb, list(gp=satplot.dat_temp$attend), mean)$x,
       avgIncbse = aggregate(satplot.dat_temp$avgb, list(gp=satplot.dat_temp$attend), se)$x)
      curPlot = data.frame(attend = myvect, avgIncb = meansb_temp$avgIncb, avgIncbse = meansb_temp$avgIncbse, type = 2)
      plotdata = rbind(plotdata, curPlot)

    #3
      satplot.dat_temp = satplot.dat[satplot.dat$SAT_MATH ==3,]
      myvect = levels(as.factor(satplot.dat_temp$attend))
      meansb_temp = data.frame(attend = myvect,
       avgIncb = aggregate(satplot.dat_temp$avgb, list(gp=satplot.dat_temp$attend), mean)$x,
       avgIncbse = aggregate(satplot.dat_temp$avgb, list(gp=satplot.dat_temp$attend), se)$x)
      curPlot = data.frame(attend = myvect, avgIncb = meansb_temp$avgIncb, avgIncbse = meansb_temp$avgIncbse, type = 3)
      plotdata = rbind(plotdata, curPlot)

    #4
      satplot.dat_temp = satplot.dat[satplot.dat$SAT_MATH ==4,]
      myvect = levels(as.factor(satplot.dat_temp$attend))
      meansb_temp = data.frame(attend = myvect,
       avgIncb = aggregate(satplot.dat_temp$avgb, list(gp=satplot.dat_temp$attend), mean)$x,
       avgIncbse = aggregate(satplot.dat_temp$avgb, list(gp=satplot.dat_temp$attend), se)$x)
      curPlot = data.frame(attend = myvect, avgIncb = meansb_temp$avgIncb, avgIncbse = meansb_temp$avgIncbse, type = 4)
      plotdata = rbind(plotdata, curPlot)

    #5
      satplot.dat_temp = satplot.dat[satplot.dat$SAT_MATH ==5,]
      myvect = levels(as.factor(satplot.dat_temp$attend))
      meansb_temp = data.frame(attend = myvect,
       avgIncb = aggregate(satplot.dat_temp$avgb, list(gp=satplot.dat_temp$attend), mean)$x,
       avgIncbse = aggregate(satplot.dat_temp$avgb, list(gp=satplot.dat_temp$attend), se)$x)
      curPlot = data.frame(attend = myvect, avgIncb = meansb_temp$avgIncb, avgIncbse = meansb_temp$avgIncbse, type = 5)
      plotdata = rbind(plotdata, curPlot)

    #6
      satplot.dat_temp = satplot.dat[satplot.dat$SAT_MATH ==6,]
      myvect = levels(as.factor(satplot.dat_temp$attend))
      meansb_temp = data.frame(attend = myvect,
       avgIncb = aggregate(satplot.dat_temp$avgb, list(gp=satplot.dat_temp$attend), mean)$x,
       avgIncbse = aggregate(satplot.dat_temp$avgb, list(gp=satplot.dat_temp$attend), se)$x)
      curPlot = data.frame(attend = myvect, avgIncb = meansb_temp$avgIncb, avgIncbse = meansb_temp$avgIncbse, type = 6)
      plotdata = rbind(plotdata, curPlot)

    #repeat analysis by type and store--by sat
    #-3
      inc_datb_temp = satplot.dat[satplot.dat$attend == -10,]
      myvect = levels(as.factor(inc_datb_temp$SAT_MATH))
      meansb_bysat_temp = data.frame(sat = myvect,
         avgIncb = aggregate(inc_datb_temp$avgb, list(gp=inc_datb_temp$SAT_MATH), mean)$x,
         avgIncbse = aggregate(inc_datb_temp$avgb, list(gp=inc_datb_temp$SAT_MATH), se)$x)
      curPlot = data.frame(sat = myvect, avgIncb = meansb_bysat_temp$avgIncb, avgIncbse = meansb_bysat_temp$avgIncbse, type = -10)
      plotdata_sat = rbind(plotdata_sat, curPlot)
    #1
      inc_datb_temp = satplot.dat[satplot.dat$attend == -1,]
      myvect = levels(as.factor(inc_datb_temp$SAT_MATH))
      meansb_bysat_temp = data.frame(sat = myvect,
       avgIncb = aggregate(inc_datb_temp$avgb, list(gp=inc_datb_temp$SAT_MATH), mean)$x,
       avgIncbse = aggregate(inc_datb_temp$avgb, list(gp=inc_datb_temp$SAT_MATH), se)$x)
      curPlot = data.frame(sat = myvect, avgIncb = meansb_bysat_temp$avgIncb, avgIncbse = meansb_bysat_temp$avgIncbse, type = 1)
      plotdata_sat = rbind(plotdata_sat, curPlot)
    #2
      inc_datb_temp = satplot.dat[satplot.dat$attend == 2,]
      myvect = levels(as.factor(inc_datb_temp$SAT_MATH))
      meansb_bysat_temp = data.frame(sat = myvect,
       avgIncb = aggregate(inc_datb_temp$avgb, list(gp=inc_datb_temp$SAT_MATH), mean)$x,
       avgIncbse = aggregate(inc_datb_temp$avgb, list(gp=inc_datb_temp$SAT_MATH), se)$x)
      curPlot = data.frame(sat = myvect, avgIncb = meansb_bysat_temp$avgIncb, avgIncbse = meansb_bysat_temp$avgIncbse, type = 2)
      plotdata_sat = rbind(plotdata_sat, curPlot)
    #3
      inc_datb_temp = satplot.dat[satplot.dat$attend == 3,]
      myvect = levels(as.factor(inc_datb_temp$SAT_MATH))
      meansb_bysat_temp = data.frame(sat = myvect,
       avgIncb = aggregate(inc_datb_temp$avgb, list(gp=inc_datb_temp$SAT_MATH), mean)$x,
       avgIncbse = aggregate(inc_datb_temp$avgb, list(gp=inc_datb_temp$SAT_MATH), se)$x)
      curPlot = data.frame(sat = myvect, avgIncb = meansb_bysat_temp$avgIncb, avgIncbse = meansb_bysat_temp$avgIncbse, type = 3)
      plotdata_sat = rbind(plotdata_sat, curPlot)
    #4
      inc_datb_temp = satplot.dat[satplot.dat$attend == 4,]
      myvect = levels(as.factor(inc_datb_temp$SAT_MATH))
      meansb_bysat_temp = data.frame(sat = myvect,
       avgIncb = aggregate(inc_datb_temp$avgb, list(gp=inc_datb_temp$SAT_MATH), mean)$x,
       avgIncbse = aggregate(inc_datb_temp$avgb, list(gp=inc_datb_temp$SAT_MATH), se)$x)
      curPlot = data.frame(sat = myvect, avgIncb = meansb_bysat_temp$avgIncb, avgIncbse = meansb_bysat_temp$avgIncbse, type = 4)
      plotdata_sat = rbind(plotdata_sat, curPlot)
    #5
      inc_datb_temp = satplot.dat[satplot.dat$attend == 5,]
      myvect = levels(as.factor(inc_datb_temp$SAT_MATH))
      meansb_bysat_temp = data.frame(sat = myvect,
       avgIncb = aggregate(inc_datb_temp$avgb, list(gp=inc_datb_temp$SAT_MATH), mean)$x,
       avgIncbse = aggregate(inc_datb_temp$avgb, list(gp=inc_datb_temp$SAT_MATH), se)$x)
      curPlot = data.frame(sat = myvect, avgIncb = meansb_bysat_temp$avgIncb, avgIncbse = meansb_bysat_temp$avgIncbse, type = 5)
      plotdata_sat = rbind(plotdata_sat, curPlot)
