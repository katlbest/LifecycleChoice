checkPredictionAbilityStErr<- function(seName, nameString){
  #THIS FUNCTION IS NOT USED
  
  #create prediction datasets
  catVect = levels(as.factor(ENROLL_DATA$cat))
  admitVect = sapply(catVect, substring, first =1, last = 1)
  attendVect = sapply(catVect, substring, first =2, last = 4)
  inputDataset <- data.frame(stErr = seName, cat = catVect, admit = admitVect, attend = attendVect)
  
  #7 values for admission/attendance should not be included, and associated cateogires should be discarded
  levels(inputDataset$cat) <- c(levels(inputDataset$cat),-3)
  levels(inputDataset$admit) <- c(levels(inputDataset$admit),-3)
  inputDataset[inputDataset$attend == 7,]$cat <- -3
  inputDataset[inputDataset$admit == 7,]$cat <- -3
  inputDataset[inputDataset$admit == 7,]$admit <- -3
  inputDataset[inputDataset$attend == 7,]$attend <- -3
  inputDataset[inputDataset == -3] <- NA
  inputDataset[inputDataset == -4] <- NA
  
  #plot
  myDir = "C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Income/Curve fitting/Plots/"
  fileName = paste(myDir, nameString, "CatSE.pdf",sep ="")
  qplot(factor(cat), stErr, data = na.exclude(inputDataset), notch= TRUE, geom = "boxplot", position = "dodge")+theme_bw() + labs(title = paste("By Category, ", nameString, sep =""))
  ggsave(file = fileName)
  fileName = paste(myDir, nameString, "AdmitSE.pdf",sep ="")
  qplot(factor(admit), stErr, data = na.exclude(inputDataset), notch= TRUE, geom = "boxplot", position = "dodge")+theme_bw()+ labs(title = paste("By best admitted, ", nameString, sep= ""))
  ggsave(file = fileName)
  fileName = paste(myDir, nameString, "AttendSE.pdf",sep ="")
  qplot(factor(attend), stErr, data = na.exclude(inputDataset), notch= TRUE, geom = "boxplot", position = "dodge")+theme_bw()+ labs(title =paste("By best applied, ", nameString, sep=""))
  ggsave(file = fileName)
  
  #regress
  AdmitMod <- lm(stErr~ factor(admit), data=na.exclude(inputDataset))
  print(summary(AdmitMod))
  AttendMod <- lm(stErr~ factor(attend), data=na.exclude(inputDataset))
  print(summary(AttendMod))
}