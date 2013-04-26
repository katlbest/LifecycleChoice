#NOTES:======================================================================

#libraries ====================================================================
  library(reshape)
  library(ggplot2)

#data i/o=======================================================================
  INCOME_DATA = read.csv("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Stock market correlation/79income_revisednames.csv")

#clean data=======================================================================
  #income is in Q13_5_YEAR variables (previous calendar year)
  #enrollment is in ENROLLMTREVYY_YYYY
  #highest degree ever received is q3-10b
  #job industry is in QES_55E_CODE_##_2000
  #HRSWK_PCY_YYYY gives hours worked (previous calendar year)
  
  #set up vector lists for storing data needed for projection
    ageVectList <- list()
    incomeVectList <- list()

  #populate income vector
    yearVect = c("1979", "1980", "1981", "1982", "1983", "1984", "1985", "1986", "1987", "1988", "1989", "1990", "1991", "1992", "1994", "1996", "1998", "2000", "2002", "2004", "2006", "2008", "2010")
      for (i in 1:nrow(INCOME_DATA)){
      incVect = rep(NA, length(yearVect))
      for (j in 1: length(yearVect)){
        curStr = paste("Q13_5_", yearVect[j], sep = "")
        curInc = INCOME_DATA[i,curStr]
        if (yearVect[j]== "1989"){ #we are missing data for 1989 for some reason
          curStr2 = paste("HRSWK_PCY_", yearVect[j-1], sep = "") 
        }
        else{
          curStr2 = paste("HRSWK_PCY_", yearVect[j], sep = "")
        }
        curHrsWkd = INCOME_DATA[i,curStr2]
        if (curInc >= 10000 & curHrsWkd >= 1200){ #10K and enrollment restriction, as imposed in other data
          incVect[j]=curInc
        }
      }
      startYear = 78-INCOME_DATA$BIRTH_YEAR[i] #1979 variables gives information about 1978
      incomeVectList[[i]]= incVect
      ageVectList[[i]]= c(startYear:(startYear+22))
    }

#Project income and calculate errors
  #project without fixing any variables
    tau = 27.8818
    newIncsList = list()
    R2List = rep(NA, nrow(INCOME_DATA))
    for (i in 1:nrow(INCOME_DATA)){
      curData = data.frame(age = ageVectList[[i]], income = incomeVectList[[i]])
      curData = na.exclude(curData)
      if (nrow(curData)>5){
        numObs = length(ageVectList[[i]])
        input1 = (1-exp(-curData$age/tau))/(curData$age/tau)
        input2 = input1 - exp(-curData$age/tau)
        quadMod = lm(curData$income~input1 + input2)
        new =  ageVectList[[i]] #we want to predict over the space where we have actual data
        new1 =(1-exp(-new/tau))/(new/tau)
        new2 = new1 - exp(-new/tau)
        new = data.frame(input1 = new1, input2 = new2)
        newIncs =predict(quadMod,new)
        newIncsList[[i]] = newIncs
        R2List[i] = summary(quadMod)$r.squared
      }
      else{
        newIncsList[[i]] = rep(NA, length(ageVectList[[i]]))
      }
    }
  

#calculate errors
  #make sure to track which year you are in
    yearVectNum = c(1979, 1980, 1981, 1982, 1983, 1984, 1985, 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1994, 1996, 1998, 2000, 2002, 2004, 2006, 2008, 2010)
  #calculate errors
  errList = list()
  yearList = list()
  for (i in 1:nrow(INCOME_DATA)){
    newIncVect = newIncsList[[i]]
    oldIncVect = incomeVectList[[i]]
    newIncVect[is.na(newIncVect)]= -3
    oldIncVect[is.na(oldIncVect)]= -3
    errVect = c()
    yearVect = c()
    if (is.na(R2List[i])){
      errList[[i]]= NA
      yearList[[i]] = NA
    } else{
      if (R2List[i]> .5){#good fit, this excludes1200 out of 12.5K people
        for (j in 1:length(newIncVect)){
          if (newIncVect[j]>0 & oldIncVect[j]>0){
            errVect[length(errVect)+1]= oldIncVect[j]- newIncVect[j]
            yearVect[length(yearVect)+1] = yearVectNum[j]
          }
        }
        errList[[i]]= errVect
        yearList[[i]] = yearVect
      } else{
        errList[[i]]= NA
        yearList[[i]] = NA
      }
    }
  }

#check overall error correlations
  INCOME_DATA$corr = NA
  longYear = c()
  longIncError = c()
  longStockError = c()
  STOCKERR_DATA = read.csv("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Stock market correlation/stockreturnin.csv")
  for (i in 1:nrow(INCOME_DATA)){
  #for (i in 1:20){
    if ((is.na(errList[[i]])==FALSE)[1]){
      #create dataset
        curData = data.frame(year = yearList[[i]], incError = errList[[i]])
        curData <- merge(x = curData, y = STOCKERR_DATA, by = "year", all.x = TRUE)
        INCOME_DATA$corr[i]= cor(curData)[2,5]
        longYear = c(longYear, curData$year)
        longIncError = c(longIncError, curData$incError)
        longStockError = c(longStockError, curData$stockError)
    } #else do nothing
  }
 
  #histogram
    ggplot(INCOME_DATA, aes(x=corr))+ geom_histogram()
    ggsave(file="C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Stock market correlation/corrhist.pdf")
  #save workspace
    save.image(file="corrsAdded.RData")
  
#calculate correlation by stacking
  LONG_DATA = data.frame(year= longYear, incError= longIncError, stockError=longStockError)
  totalCor = cor(LONG_DATA)[2,3]
  print(totalCor)
