library(ggplot2)

#create raw plot for introduction===========================================================
  #read data
    inc_dat = read.csv("inputs/rawinputs.csv")
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
      inc_dat$avg[i] = mean(na.exclude(curVect))
      inc_dat$avgb[i] = mean(na.exclude(curVectb))
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
          geom_bar(data = plotdata[plotdata$type==5,],stat='identity', colour = 'black',fill = NA) + 
          geom_line(data = plotdata[plotdata$type==0,], aes(group= 1), colour = 'blue')+
          geom_line(data = plotdata[plotdata$type==0,], aes(y = avgIncb-avgIncbse*1.96,group= 1),colour = 'grey')+
          geom_line(data = plotdata[plotdata$type==0,], aes(y = avgIncb+avgIncbse*1.96,group= 1), colour = 'grey')+
          #geom_ribbon(data = plotdata[plotdata$type==0,],aes(ymin=avgIncb-avgIncbse*1.96, ymax = avgIncb+avgIncbse*1.96),alpha=0.3)+
          #geom_errorbar(data = plotdata[plotdata$type==0,],aes(ymin=avgIncb-avgIncbse*1.96, ymax = avgIncb+avgIncbse*1.96))+
          theme_bw()+
          xlab("Selectivity of school attended") +
          ylab("Average income over ages 22-25") +
          theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(), text = element_text(size=20))+
          geom_errorbar(data= plotdata[plotdata$type==5,],aes(ymin=avgIncb-avgIncbse*1.96, ymax = avgIncb+avgIncbse*1.96))
          

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

