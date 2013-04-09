ENROLL_DATA<-read.csv("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/compileddata-allways.csv")

#read in relevant predictors=================================================
#NOTE: SHOULDNT HAVE TO RUN THIS IF READING IN FROM ABOVE
#read in other relevant predictor information
INCOME_PREDS<- read.csv("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/incomepredictors.csv")
ENROLL_DATA2<- merge(x = ENROLL_DATA, y = INCOME_PREDS, by = "PUBID_1997", all.x = TRUE)
COLLEGE_NUM<- read.csv("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/collegenumber.csv")
ENROLL_DATA2<- merge(x = ENROLL_DATA2, y = COLLEGE_NUM, by = "PUBID_1997", all.x = TRUE)
LOC_DATA <- read.csv("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/desensitizedloc.csv")
ENROLL_DATA2<- merge(x = ENROLL_DATA2, y = LOC_DATA, by = "PUBID_1997", all.x = TRUE)
ENROLL_DATA<-ENROLL_DATA2

#populate major
#use latest major at "real school"
ENROLL_DATA$MAJOR <- -3
for (i in 1:nrow(ENROLL_DATA)){
  colNumString = paste("YSCH_21300", ENROLL_DATA$COLLEGEID_ROSTERNUM2[i], "_", sep ="")
  varVector = c()
  curYear = ENROLL_DATA$COLLEGEID_YEAR2[i]
  yearString = paste("_", ENROLL_DATA$COLLEGEID_YEAR2[i], sep ="")
  termVect = c("01","02", "03", "04","05", "06", "07","08","09","10","11","12", "13")
  for (j in 1:length(termVect)){
    curstr = paste(colNumString, termVect[j], yearString, sep = "")
    if (curstr %in% colnames(ENROLL_DATA)){
      varVector[length(varVector)+1] = curstr
    }
  }
  #now have vector of variables to check, use most recent
  if (length(varVector)>0){
    for (j in length(varVector):1){
      curStr = paste("ENROLL_DATA$", varVector[j], sep = "")
      curMajor = eval(parse(text =curStr))[i]
      if (ENROLL_DATA$MAJOR[i] == -3 & curMajor >= 0){
        ENROLL_DATA$MAJOR[i]= curMajor
      }
    }
  }
  else {
    ENROLL_DATA$MAJOR[i]= -3
  }
}

#populate area of residence
#geo variables are pretty straight forward, GEO03--use desensitized
#get mode, and if all are different use the latest avaialble
ENROLL_DATA$GEO = -3
for (i in 1:nrow(ENROLL_DATA)){
  geoVector = c()
  if (ENROLL_DATA$GEO03_2010[i]>=0){
    geoVector[length(geoVector)+1]= ENROLL_DATA$GEO03_2010[i]
  }
  if (ENROLL_DATA$GEO03_2009[i]>=0){
    geoVector[length(geoVector)+1]= ENROLL_DATA$GEO03_2009[i]
  }
  if (ENROLL_DATA$GEO03_2008[i]>=0){
    geoVector[length(geoVector)+1]= ENROLL_DATA$GEO03_2008[i]
  }
  if (ENROLL_DATA$GEO03_2009[i]>=0){
    geoVector[length(geoVector)+1]= ENROLL_DATA$GEO03_2009[i]
  }
  if (length(geoVector)>0){
    uniques <- unique(geoVector)
    ENROLL_DATA$GEO[i]<- uniques[which.max(tabulate(match(geoVector, uniques)))]
  }
}

#populate GPA
#use the one GPA variable (), #YSCH-7300 (dont worry about recode, etc)
#value of 1 is badm 8 is good, above 8 should be discarded
yearVect  = c("2007", "2006","2005","2004","2003","2002","2001","2000","1999","1998","1997")
ENROLL_DATA$GRADES = -3
for (i in 1:nrow(ENROLL_DATA)){
  for (j in 1:length(yearVect)){
    curStr = paste("ENROLL_DATA$YSCH_7300_", yearVect[j], sep = "")
    curValue  = eval(parse(text =curStr))[i]
    if (ENROLL_DATA$GRADES[i] == -3 & curValue >= 0 & curValue <9){
      ENROLL_DATA$GRADES[i] = curValue
    }
  }
}

#populate data on schooling completion (CVC_HIGHEST_DEGREE_EVER_XRND)
ENROLL_DATA$COLLEGECOMPLETED = -3
for (i in 1:nrow(ENROLL_DATA)){
  if (ENROLL_DATA$stillInCollege[i]==0){
    if (ENROLL_DATA$CVC_HIGHEST_DEGREE_EVER_XRND[i]>3){ #4 is bachelors
      ENROLL_DATA$COLLEGECOMPLETED[i] = 1
    }
    else{
      ENROLL_DATA$COLLEGECOMPLETED[i] = 0
    }
  }
}

#create second major variable (categorical)===========================================
hardSci <- c(6, 21, 25)
softSci <- c(3, 10, 11, 31, 32)
bus <- c(7, 8, 9, 13)
health <- c(22, 23, 27, 29, 30, 28)
hum <- c(1,2,5,12,14,15,17,19,18,20,24,26,33,4,16)
ENROLL_DATA$MAJOR2
#reduce categories
for (i in 1:nrow(ENROLL_DATA)){
  if (ENROLL_DATA$MAJOR[i] %in% hardSci){
    ENROLL_DATA$MAJOR2[i]= 1
  }
  else if (ENROLL_DATA$MAJOR[i] %in% softSci){
    ENROLL_DATA$MAJOR2[i]= 2
  }
  else if (ENROLL_DATA$MAJOR[i] %in% bus){
    ENROLL_DATA$MAJOR2[i]= 3
  }
  else if (ENROLL_DATA$MAJOR[i] %in% health){
    ENROLL_DATA$MAJOR2[i]= 4
  }
  else if (ENROLL_DATA$MAJOR[i] %in% hum){
    ENROLL_DATA$MAJOR2[i]= 5
  }
  else {
    ENROLL_DATA$MAJOR2[i]= -3
  }
}

#create full datasets========================================================
#no enrollment restriction
  b0ProjectData <- data.frame(b0 = ENROLL_DATA$b0, cat = ENROLL_DATA$cat, admit = ENROLL_DATA$BestAd5, attend = ENROLL_DATA$BestAtt5, major = ENROLL_DATA$MAJOR, major2 = ENROLL_DATA$MAJOR2, gpa = ENROLL_DATA$GRADES, geo = ENROLL_DATA$GEO, collgrad = ENROLL_DATA$COLLEGECOMPLETE)
  #7 values for admission/attendance should not be included, and associated cateogires should be discarded
  b0ProjectData[b0ProjectData$admit == 7,]$cat <- -3
  b0ProjectData[b0ProjectData$attend == 7,]$cat <- -3
  b0ProjectData[b0ProjectData$admit == 7,]$admit <- -3
  b0ProjectData[b0ProjectData$attend == 7,]$attend <- -3
  b0ProjectData[b0ProjectData == -3] <- NA 

#with enrollment restriction
  b02ProjectData <- b0ProjectData
  b02ProjectData$b0 <- ENROLL_DATA$b02
  b02ProjectData[b02ProjectData == -3] <- NA
 
#project=========================================================================
#model selection with no enrollment restriction
  #we exclude location because it has too many categories
  AllFactor <- lm(b0~factor(cat)+ factor(gpa) + factor(major2)+ factor(collgrad), data=na.exclude(b0ProjectData))
  GPAMod <- lm(b0~ factor(gpa), data=na.exclude(b0ProjectData))
  MajorMod <- lm(b0~ factor(major2), data=na.exclude(b0ProjectData))
  GradMod <- lm(b0~ factor(collgrad), data=na.exclude(b0ProjectData))
  CatMod <- lm(b0~ factor(cat), data=na.exclude(b0ProjectData))
  AdmitMod <- lm(b0~ factor(admit), data=na.exclude(b0ProjectData))
  AttendMod <- lm(b0~ factor(attend), data=na.exclude(b0ProjectData))

#model selection with enrollment restriction
#we exclude location because it has too many categories
AllFactor <- lm(b0~factor(cat)+ factor(gpa) + factor(major2)+ factor(collgrad), data=na.exclude(b02ProjectData))
summary(AllFactor)
GPAMod <- lm(b0~ factor(gpa), data=na.exclude(b02ProjectData))
summary(GPAMod)
MajorMod <- lm(b0~ factor(major2), data=na.exclude(b02ProjectData))
summary(MajorMod)
GradMod <- lm(b0~ factor(collgrad), data=na.exclude(b02ProjectData))
summary(GradMod)
CatMod <- lm(b0~ factor(cat), data=na.exclude(b02ProjectData))
summary(CatMod)
AdmitMod <- lm(b0~ factor(admit), data=na.exclude(b02ProjectData))
summary(AdmitMod)
AttendMod <- lm(b0~ factor(attend), data=na.exclude(b02ProjectData))
summary(AttendMod)