library(sqldf)
library(plyr)

#file should be runnable as is except the CONSOLE portion
#notes=======================================================================
#to clear data: rm(list = ls(all = TRUE))
#to replace missing values: MERGED_DATA[MERGED_DATA < 0] <- NA
#sapply(PYTHON_OUT, class)

#data i/o=======================================================================
  MERGED_DATA <- read.csv("D:/MERGED_DATA_REDUCED.csv")
  #MERGED_DATA_STORE<-MERGED_DATA
  #get smaller test set or go back to full data
  #MERGED_DATA<-MERGED_DATA[1:100,]
  #MERGED_DATA<-MERGED_DATA_STORE
                                                  
#attendance====================================================================   
#get first school you attended for at least a bachelor's degree and full time
#TBD-it may be worth getting more data out of this if IPEDS lookups dont yield all we need
MERGED_DATA$COLLEGES_ATTEND_VECTOR<- "" #vector of attended schools  
MERGED_DATA$COLLEGEID_YEAR2_VECTOR <- ""  #vector of years of current school
MERGED_DATA$COLLEGEID_ROSTERNUM2_VECTOR <- "" #rosternum of schools vector
MERGED_DATA$COLLEGEID_DEGREE2_VECTOR <- "" #degrees at schools vector
year_vect = c('_1998', '_1999', '_2000', '_2001', '_2002', '_2003','_2004','_2005','_2006','_2007','_2008','_2009','_2010')
school_vect = c('_01','_02','_03','_04','_05','_06','_07','_08')
month_vect = c('_01','_02','_03','_04','_05','_06')
for (i in 1:length(school_vect)){
  for (j in 1:length(year_vect)){
    curstr = paste('GEO69',school_vect[i],year_vect[j], sep = "") #school ID
    curstr3 = paste('MERGED_DATA$YSCH_27337',school_vect[i],year_vect[j], sep = "") #type of degree
    if (curstr %in% colnames(MERGED_DATA)){
      curstr = paste('MERGED_DATA$', curstr,sep = '')
      for (k in 1:nrow(MERGED_DATA)){
        minYear = MERGED_DATA$CHOICE_YEAR[k] - 2
        maxYear = MERGED_DATA$CHOICE_YEAR[k] + 3
        if (eval(parse(text =curstr))[k] > 0){    #check for existence of school ID
          curID = eval(parse(text =curstr))[k]
          curYearStr = year_vect[j]
          curYearNum = j + 1997
          curSchoolStr = school_vect[i]
          #if (curYearNum > minYear){
          if (curYearNum > minYear & curYearNum < maxYear){
            fullFlag = 0
            for (l in 1:length(month_vect)){
              curstr2 = paste('MERGED_DATA$YSCH_21800',school_vect[i],month_vect[l],year_vect[j], sep = "")
              fullFlag = fullFlag + max(eval(parse(text =curstr2))[k], 0)
            }
            if (fullFlag > 0) {
              if (eval(parse(text =curstr3))[k] > 2){
                MERGED_DATA$COLLEGES_ATTEND_VECTOR[k]= paste(MERGED_DATA$COLLEGES_ATTEND_VECTOR[k], eval(parse(text =curstr))[k], ",", sep = "")
                MERGED_DATA$COLLEGEID_YEAR2_VECTOR[k]= paste(MERGED_DATA$COLLEGEID_YEAR2_VECTOR[k], toString(curYearNum), ",", sep = "")
                MERGED_DATA$COLLEGEID_DEGREE2_VECTOR[k] = paste(MERGED_DATA$COLLEGEID_DEGREE2_VECTOR[k], eval(parse(text =curstr3))[k], ",", sep = "")
                MERGED_DATA$COLLEGEID_ROSTERNUM2_VECTOR[k] = paste(MERGED_DATA$COLLEGEID_ROSTERNUM2_VECTOR[k], school_vect[i], ",", sep = "")
              }
            }
          }
        }
      }
    }
  }
}

MERGED_DATA$COLLEGES_SCHOOLID2 <- -3 #number of first attended school     
MERGED_DATA$COLLEGEID_YEAR2 <- -3 #year of first attended school
MERGED_DATA$COLLEGEID_ROSTERNUM2 <- "" #rosternum of schools vector
MERGED_DATA$COLLEGEID_DEGREE2 <- -3 #degrees at schools vector
for (k in 1:nrow(MERGED_DATA)){
  if (MERGED_DATA$COLLEGES_ATTEND_VECTOR[k] != ""){
    MERGED_DATA$COLLEGES_SCHOOLID2[k]= as.integer(strsplit(MERGED_DATA$COLLEGES_ATTEND_VECTOR[k], ",", fixed = FALSE, perl = FALSE, useBytes = FALSE)[[1]][1])
    MERGED_DATA$COLLEGEID_YEAR2[k]= as.integer(strsplit(MERGED_DATA$COLLEGEID_YEAR2_VECTOR[k], ",", fixed = FALSE, perl = FALSE, useBytes = FALSE)[[1]][1])
    MERGED_DATA$COLLEGEID_ROSTERNUM2[k]= strsplit(MERGED_DATA$COLLEGEID_ROSTERNUM2_VECTOR[k], ",", fixed = FALSE, perl = FALSE, useBytes = FALSE)[[1]][1]
    MERGED_DATA$COLLEGEID_DEGREE2[k] = as.integer(strsplit(MERGED_DATA$COLLEGEID_DEGREE2_VECTOR[k], ",", fixed = FALSE, perl = FALSE, useBytes = FALSE)[[1]][1])
  }
}

#college attendance indicator====================================================================
#whole sample is college eligible
#those with school ID within 2 years, full-time, bachelors are college goers
MERGED_DATA$COLLEGE_GOER_FLAG = -3
for (k in 1:nrow(MERGED_DATA)){
  if (MERGED_DATA$COLLEGES_SCHOOLID2[k] > 0) {
    MERGED_DATA$COLLEGE_GOER_FLAG[k] = 1
  }
} 

write.csv(MERGED_DATA, file = "D:/MERGED_DATA_REDUCED_ATTENDANCE.csv") #write this to reduced dataset so we do not have to run again

#application/admission==========================================================
  #empty vectors for storage
  MERGED_DATA$COLLEGES_APPLY_VECTOR <- "" #vector of applied schools
  MERGED_DATA$COLLEGES_APPLYALL_VECTOR <- "" #vector of applied schools including outside of term limit
  MERGED_DATA$COLLEGES_ADMIT_VECTOR <- "" #vector of admitted schools
  MERGED_DATA$COLLEGES_TERM_VECTOR <- "" #vector of term admitted
  MERGED_DATA$COLLEGES_APPLYYEAR_VECTOR <- "" #stores years of every applied school
  MERGED_DATA$COLLEGES_APPLYROSTER_VECTOR <- "" #stores roster number of every applied school
  #create comma-separated list of applied, applied within term limits, and admitted
  year_vect = c('_2004','_2005','_2006','_2007','_2008','_2009','_2010')
  school_vect = c('_01','_02','_03','_04','_05','_06','_07','_08','_09','_10')
  for (i in 1:length(school_vect)){
    for (j in 1:length(year_vect)){
      curstr = paste('PREV_COL_APP_ID',school_vect[i],year_vect[j], sep = "")
      curstr2 = paste("PREV_COL_APP_TERMNUM",school_vect[i],year_vect[j], sep = "")
      curstr3 = paste("PREV_COL_APP_ADMIT",school_vect[i],year_vect[j], sep = "")
      if (curstr %in% colnames(MERGED_DATA)){
        curstr = paste('MERGED_DATA$', curstr,sep = '')
        curstr2 = paste('MERGED_DATA$', curstr2,sep = '')                              
        curstr3 = paste('MERGED_DATA$', curstr3,sep = '')
        for (k in 1:nrow(MERGED_DATA)){
          curMin = MERGED_DATA$CHOICE_YEAR[k] - 2
          if (MERGED_DATA$COLLEGEID_YEAR2[k] > 0){
            curMax = MERGED_DATA$COLLEGEID_YEAR2[k]+3
          }
          else {curMax = 2013}
          if (eval(parse(text =curstr))[k] > 0){    #check for existence of application ID
            curTerm = ((eval(parse(text =curstr2))[k] +1) %/% 4) +1997
            MERGED_DATA$COLLEGES_APPLYALL_VECTOR[k]= paste(MERGED_DATA$COLLEGES_APPLYALL_VECTOR[k], eval(parse(text =curstr))[k], ",", sep = "")
            MERGED_DATA$COLLEGES_ADMITYEAR_VECTOR <- paste(MERGED_DATA$COLLEGES_ADMITYEAR_VECTOR[k], year_vect[j], ",", sep = "")
            MERGED_DATA$COLLEGES_ADMITROSTER_VECTOR <- paste(MERGED_DATA$COLLEGES_ADMITROSTER_VECTOR[k], year_vect[i], ",", sep = "")
            MERGED_DATA$COLLEGES_TERM_VECTOR[k]= paste(MERGED_DATA$COLLEGES_TERM_VECTOR[k], toString(curTerm), ",", sep = "")
            if (eval(parse(text =curstr3))[k] == 1){ #admitted
              # print("yes")
              MERGED_DATA$COLLEGES_ADMIT_VECTOR[k]= paste(MERGED_DATA$COLLEGES_ADMIT_VECTOR[k], "1", ",", sep = "")
            }
            else if (eval(parse(text =curstr3))[k] == 0){ #not admitted
              MERGED_DATA$COLLEGES_ADMIT_VECTOR[k]= paste(MERGED_DATA$COLLEGES_ADMIT_VECTOR[k], "0", ",", sep = "")
            }
            else{ #decision pending
              MERGED_DATA$COLLEGES_ADMIT_VECTOR[k]= paste(MERGED_DATA$COLLEGES_ADMIT_VECTOR[k], "-3", ",", sep = "")
            }
            if (curTerm > curMin & curTerm < curMax){
              MERGED_DATA$COLLEGES_APPLY_VECTOR[k]= paste(MERGED_DATA$COLLEGES_APPLY_VECTOR[k], eval(parse(text =curstr))[k], ",", sep = "")            
            }
          }  
        }
      }
    }
  }
  write.csv(MERGED_DATA, file = "D:/MERGED_DATA_REDUCED_PREVVECTORS.csv") #write this to reduced dataset so we do not have to run again

#application from YCOC-050P==========================================================
  #empty vectors for storage
  MERGED_DATA$COLLEGES_APPLY_VECTOR2 <- "" #vector of applied schools
  MERGED_DATA$COLLEGES_APPLYALL_VECTOR2 <- "" #vector of applied schools including outside of term limit
  MERGED_DATA$COLLEGES_ADMIT_VECTOR2 <- "" #vector of admitted schools
  MERGED_DATA$COLLEGES_TERM_VECTOR2 <- "" #vector of admitted schools
  MERGED_DATA$COLLEGES_ADMITYEAR_VECTOR2 <-""
  MERGED_DATA$COLLEGES_ADMITSCHOOL_VECTOR2 <- ""
  MERGED_DATA$COLLEGES_ADMITLOOP_VECTOR2 <- ""
  #create comma-separated list of applied, applied within term limits, and admitted--remove term restriction, correct vectors are APPLYALL2 and ADMIT2
  year_vect = c('_2004','_2005','_2006','_2007','_2008','_2009','_2010')
  school_vect = c('_01','_02','_03','_04','_05','_06','_07','_08','_09','_10')
  loop_vect = c('01','02','03','04','05','06','07','08','09','10')
  for (i in 1:length(school_vect)){ #this loop is only for 2003 since it looks different
    for (k in 1:length(loop_vect)){
    curstr = paste('YCOC_050P_',loop_vect[k],school_vect[i],"_000001_2003", sep = "")
    curstr2 = paste("YCOC_003C_",loop_vect[k],"_2003", sep = "")
    curstr3 = paste("YCOC_054_",loop_vect[k],school_vect[i],"_2003", sep = "") #YCOC_050P_01_01_000001_2003
    if (curstr %in% colnames(MERGED_DATA)){
      curstr = paste('MERGED_DATA$', curstr,sep = '')
      curstr2 = paste('MERGED_DATA$', curstr2,sep = '')
      curstr3 = paste('MERGED_DATA$', curstr3,sep = '')
      for (m in 1:nrow(MERGED_DATA)){
        curMin = MERGED_DATA$CHOICE_YEAR[m] - 2
        if (MERGED_DATA$COLLEGEID_YEAR2[m] > 0){
          curMax = MERGED_DATA$COLLEGEID_YEAR2[m]+3
        } else {curMax = 2013}
        if (eval(parse(text =curstr))[m] > 0){    #check for existence of application ID
          curTerm = ((eval(parse(text =curstr2))[m] +1) %/% 4) +1997
          MERGED_DATA$COLLEGES_APPLYALL_VECTOR2[m]= paste(MERGED_DATA$COLLEGES_APPLYALL_VECTOR2[m], eval(parse(text =curstr))[m], ",", sep = "")
          MERGED_DATA$COLLEGES_ADMITSCHOOL_VECTOR2 <- paste(MERGED_DATA$COLLEGES_ADMITSCHOOL_VECTOR2[m], school_vect[i], ",", sep = "")
          MERGED_DATA$COLLEGES_ADMITLOOP_VECTOR2 <- paste(MERGED_DATA$COLLEGES_ADMITLOOP_VECTOR2[m], loop_vect[k], ",", sep = "")
          MERGED_DATA$COLLEGES_ADMITYEAR_VECTOR2 <- paste(MERGED_DATA$COLLEGES_ADMITYEAR_VECTOR2[m],"_2003", ",", sep = "")
          MERGED_DATA$COLLEGES_TERM_VECTOR2[m]= paste(MERGED_DATA$COLLEGES_TERM_VECTOR2[m], toString(curTerm), ",", sep = "")
          if (eval(parse(text =curstr3))[m] == 1){ #admitted
            MERGED_DATA$COLLEGES_ADMIT_VECTOR2[m]= paste(MERGED_DATA$COLLEGES_ADMIT_VECTOR2[m], "1", ",", sep = "")
          }
          else if (eval(parse(text =curstr3))[m] == 0){ #not admitted
            MERGED_DATA$COLLEGES_ADMIT_VECTOR2[m]= paste(MERGED_DATA$COLLEGES_ADMIT_VECTOR2[m], "0", ",", sep = "")
          }
          else{ #decision pending
            MERGED_DATA$COLLEGES_ADMIT_VECTOR2[m]= paste(MERGED_DATA$COLLEGES_ADMIT_VECTOR2[m], "-3", ",", sep = "")
          }
          if (curTerm > curMin & curTerm < curMax){
            MERGED_DATA$COLLEGES_APPLY_VECTOR2[m]= paste(MERGED_DATA$COLLEGES_APPLY_VECTOR2[m], eval(parse(text =curstr))[m], ",", sep = "")            
          } 
        }
      }
    }
  }
  }

  for (i in 1:length(school_vect)){ #this loops over the rest of the years
    for (j in 1:length(year_vect)){
      for (k in 1:length(loop_vect)){
        curstr = paste('YCOC_050P_000001_',loop_vect[k],school_vect[i],year_vect[j], sep = "")
        curstr2 = paste("YCOC_003C_",loop_vect[k],year_vect[j], sep = "")
        curstr3 = paste("YCOC_054_",loop_vect[k],school_vect[i],year_vect[j], sep = "") #"YCOC_050P_01_01_2004" "YCOC_050P_000001.01.03_2005" 
        if (curstr %in% colnames(MERGED_DATA)){
          curstr = paste('MERGED_DATA$', curstr,sep = '')
          curstr2 = paste('MERGED_DATA$', curstr2,sep = '')
          curstr3 = paste('MERGED_DATA$', curstr3,sep = '')
          for (m in 1:nrow(MERGED_DATA)){
            curMin = MERGED_DATA$CHOICE_YEAR[m] - 2
            if (MERGED_DATA$COLLEGEID_YEAR2[m] > 0){
              curMax = MERGED_DATA$COLLEGEID_YEAR2[m]+3
            } else {curMax = 2013}
            if (eval(parse(text =curstr))[m] > 0){    #check for existence of application ID
              curTerm = ((eval(parse(text =curstr2))[m] +1) %/% 4) +1997
              MERGED_DATA$COLLEGES_APPLYALL_VECTOR2[m]= paste(MERGED_DATA$COLLEGES_APPLYALL_VECTOR2[m], eval(parse(text =curstr))[m], ",", sep = "")
              MERGED_DATA$COLLEGES_ADMITSCHOOL_VECTOR2 <- paste(MERGED_DATA$COLLEGES_ADMITSCHOOL_VECTOR2[m], school_vect[i], ",", sep = "")
              MERGED_DATA$COLLEGES_ADMITLOOP_VECTOR2 <- paste(MERGED_DATA$COLLEGES_ADMITLOOP_VECTOR2[m], loop_vect[k], ",", sep = "")
              MERGED_DATA$COLLEGES_ADMITYEAR_VECTOR2 <- paste(MERGED_DATA$COLLEGES_ADMITYEAR_VECTOR2[m],year_vect[j], ",", sep = "")
              MERGED_DATA$COLLEGES_TERM_VECTOR2[m]= paste(MERGED_DATA$COLLEGES_TERM_VECTOR2[m], toString(curTerm), ",", sep = "")
              if (eval(parse(text =curstr3))[m] == 1){ #admitted
                MERGED_DATA$COLLEGES_ADMIT_VECTOR2[m]= paste(MERGED_DATA$COLLEGES_ADMIT_VECTOR2[m], "1", ",", sep = "")
              }
              else if (eval(parse(text =curstr3))[m] == 0){ #not admitted
                MERGED_DATA$COLLEGES_ADMIT_VECTOR2[m]= paste(MERGED_DATA$COLLEGES_ADMIT_VECTOR2[m], "0", ",", sep = "")
              }
              else{ #decision pending
                MERGED_DATA$COLLEGES_ADMIT_VECTOR2[m]= paste(MERGED_DATA$COLLEGES_ADMIT_VECTOR2[m], "-3", ",", sep = "")
              }
              if (curTerm > curMin & curTerm < curMax){
                MERGED_DATA$COLLEGES_APPLY_VECTOR2[m]= paste(MERGED_DATA$COLLEGES_APPLY_VECTOR2[m], eval(parse(text =curstr))[m], ",", sep = "")            
              } 
            }
          }
        }
      }
    }
  }
  
 write.csv(MERGED_DATA,"D:/MERGED_DATA_REDUCED_YCOCVECTORS.csv")

#cleaned up vectors==========================================================
#create dataset of only the youngest individuals whom we want to match to IPEDS
  #read in compiled data (so we can start here when desired)
 #COMPILED_DATA <- read.csv("D:/MERGED_DATA_HASYCOCVECTORS.csv")
COMPILED_DATA <-MERGED_DATA
  #concatenate lists
  COMPILED_DATA$COMPILED_APPLY <- "" 
  COMPILED_DATA$COMPILED_ADMIT <- ""
  COMPILED_DATA$COMPILED_APPLYALL <- ""
  for (i in 1:nrow(COMPILED_DATA)){
    COMPILED_DATA$COMPILED_APPLY[i] <- paste(COMPILED_DATA$COLLEGES_APPLY_VECTOR[i],COMPILED_DATA$COLLEGES_APPLY_VECTOR2[i], sep = "")
    COMPILED_DATA$COMPILED_ADMIT[i] <- paste(COMPILED_DATA$COLLEGES_ADMIT_VECTOR[i],COMPILED_DATA$COLLEGES_ADMIT_VECTOR2[i], sep = "")
    COMPILED_DATA$COMPILED_APPLYALL[i] <- paste(COMPILED_DATA$COLLEGES_APPLYALL_VECTOR[i],COMPILED_DATA$COLLEGES_APPLYALL_VECTOR2[i], sep = "")
  }
    
  write.csv(COMPILED_DATA, file ="D:/COMPILED_DATA.csv")

#I GUESS THIS PART FAILS IF YOU READ FROM FILE!
#count total appliers===============================================================================================================
COMPILED_DATA$APPLYER <- 0 #whether you applied
for (i in 1:nrow(COMPILED_DATA)){
    numApplyAllVect <- strsplit(COMPILED_DATA$COMPILED_APPLYALL[i], ",", fixed = FALSE, perl = FALSE, useBytes = FALSE)[[1]]
    if (length(numApplyAllVect) > 0){
      COMPILED_DATA$APPLYER[i] = 1
    }
}

#automate identification of missings=================================================================================================
COMPILED_DATA$MISSING_ALL <- 0 #missing all indicator
COMPILED_DATA$MISSING_ALL_UNRESTRICTED <- 0 #missing all indicator when using unrestricted times
COMPILED_DATA$MISSING_ATTENDED <- 0
COMPILED_DATA$MISSING_ATTENDED_UNRESTRICTED <- 0
for (i in 1:nrow(COMPILED_DATA)){
  if (COMPILED_DATA$COLLEGES_SCHOOLID2[i]>0){
    numApplyVect <- strsplit(COMPILED_DATA$COMPILED_APPLY[i], ",", fixed = FALSE, perl = FALSE, useBytes = FALSE)[[1]]
    numApplyAllVect <- strsplit(COMPILED_DATA$COMPILED_APPLYALL[i], ",", fixed = FALSE, perl = FALSE, useBytes = FALSE)[[1]]
    curID <- COMPILED_DATA$COLLEGES_SCHOOLID2[i]
    if (length(numApplyVect) < 1){
      COMPILED_DATA$MISSING_ALL[i] = 1
    } else if (!(curID %in% numApplyVect)) {
      COMPILED_DATA$MISSING_ATTENDED[i] = 1
    }
    if (length(numApplyAllVect) < 1){
      COMPILED_DATA$MISSING_ALL_UNRESTRICTED[i] = 1
    } else if (!(curID %in% numApplyAllVect)) {
      COMPILED_DATA$MISSING_ATTENDED_UNRESTRICTED[i] = 1
    }
    }
}

write.csv(COMPILED_DATA, file = "D:/compiled_data.csv")

MISSING_ALL_DATA <- COMPILED_DATA[COMPILED_DATA$MISSING_ALL ==1,] #56 infividuals are missing all data
dim(MISSING_ALL_DATA)
MISSING_ALL_UNRESTRICTED_DATA <- COMPILED_DATA[COMPILED_DATA$MISSING_ALL_UNRESTRICTED ==1,] #19 individuals are missing all when we are unrestricted?
dim(MISSING_ALL_UNRESTRICTED_DATA)
MISSING_ATTENDED_DATA <- COMPILED_DATA[COMPILED_DATA$MISSING_ATTENDED ==1,] #145 additional people missing the school they attended
dim(MISSING_ATTENDED_DATA)
MISSING_ATTENDED_UNRESTRICTED_DATA <- COMPILED_DATA[COMPILED_DATA$MISSING_ATTENDED_UNRESTRICTED ==1,] #151 are missing the school they attended if we lift the restriction
dim(MISSING_ATTENDED_UNRESTRICTED_DATA)

#create output file for python college lookup==========================================================
for (i in 1:nrow(COMPILED_DATA)){
  COMPILED_DATA$COMPILED_APPLY[i] <-toString(COMPILED_DATA$COMPILED_APPLY[i])
  COMPILED_DATA$COMPILED_ADMIT[i] <-toString(COMPILED_DATA$COMPILED_ADMIT[i])
}
PYTHON_OUT <- COMPILED_DATA[,c('PUBID_1997','COLLEGE_GOER_FLAG','COLLEGES_SCHOOLID2','COMPILED_APPLY','COMPILED_APPLYALL','COMPILED_ADMIT')]
write.table(PYTHON_OUT, file = "D:/compiledcollegelist.txt", sep = "\t") #this is the input file for python

#variable existence checks===============================================================================
write.csv(colnames(MERGED_DATA),"D:/colnames.csv")
relvars = c()
#ATTENDANCE
year_vect = c('_1998', '_1999', '_2000', '_2001', '_2002', '_2003','_2004','_2005','_2006','_2007','_2008','_2009','_2010')
school_vect = c('_01','_02','_03','_04','_05','_06','_07','_08')
month_vect = c('_01','_02','_03','_04','_05','_06')
for (i in 1:length(school_vect)){
  for (j in 1:length(year_vect)){
    for (l in 1:length(month_vect)){
      curstr = paste('GEO69',school_vect[i],year_vect[j], sep = "")
      curstr2 = paste('YSCH_21800',school_vect[i],month_vect[l],year_vect[j], sep = "")
      curstr3 = paste('YSCH_27337',school_vect[i],year_vect[j], sep = "")
      if (curstr %in% colnames(MERGED_DATA)){
        print(curstr)
        #relvars = c(relvars,curstr)
      }
      if (curstr2 %in% colnames(MERGED_DATA)){
        print(curstr2)
      }
      if (curstr3 %in% colnames(MERGED_DATA)){
        print(curstr3)
      }
    }
  }
}

#ADMIT BY PREV
year_vect = c('_2004','_2005','_2006','_2007','_2008','_2009','_2010')
school_vect = c('_01','_02','_03','_04','_05','_06','_07','_08','_09','_10')
for (i in 1:length(school_vect)){
  for (j in 1:length(year_vect)){
    curstr = paste('PREV_COL_APP_ID',school_vect[i],year_vect[j], sep = "")
    curstr2 = paste("PREV_COL_APP_TERMNUM",school_vect[i],year_vect[j], sep = "")
    curstr3 = paste("PREV_COL_APP_ADMIT",school_vect[i],year_vect[j], sep = "")
    if (curstr %in% colnames(MERGED_DATA)){
      print(curstr)
      relvars = c(relvars,curstr)
    }
    if (curstr2 %in% colnames(MERGED_DATA)){
        print(curstr2)
    }
    if (curstr3 %in% colnames(MERGED_DATA)){
      print(curstr3)
    }
  }
}

#ADMIT BY YCOC
year_vect = c('_2004','_2005','_2006','_2007','_2008','_2009','_2010')
school_vect = c('_01','_02','_03','_04','_05','_06','_07','_08','_09','_10')
loop_vect = c('01','02','03','04','05','06','07','08','09','10')
for (i in 1:length(school_vect)){ #this loop is only for 2003 since it looks different
  for (k in 1:length(loop_vect)){
    curstr = paste('YCOC_050P_',loop_vect[k],school_vect[i],"_000001_2003", sep = "")
    curstr2 = paste("YCOC_003C_",loop_vect[k],"_2003", sep = "")
    curstr3 = paste("YCOC_054_",loop_vect[k], school_vect[i],"_2003", sep = "")
    for (j in 1:length(year_vect)){  
      curstr4 = paste('YCOC_050P_000001_',loop_vect[k],school_vect[i],year_vect[j], sep = "")
      curstr5 = paste("YCOC_054_",loop_vect[k],school_vect[i],year_vect[j], sep = "") 
      if (curstr4 %in% colnames(MERGED_DATA)){
        print(curstr4)
        relvars = c(relvars,curstr4)
      }
      if (curstr5 %in% colnames(MERGED_DATA)){
        print(curstr5)
      }
    }
    if (curstr %in% colnames(MERGED_DATA)){
      print(curstr)
      relvars = c(relvars,curstr)
    }
    if (curstr2 %in% colnames(MERGED_DATA)){
      print(curstr2)
    }
    if (curstr3 %in% colnames(MERGED_DATA)){
      print(curstr3)
    }
  }
}

relvars = c('PUBID_1997','COLLEGES_SCHOOLID2', 'COLLEGEID_DEGREE2', 'CHOICE_YEAR','COLLEGEID_YEAR2','YCOC_002_2003','YCOC_002_2004','YCOC_002_2005','YCOC_002_2006','YCOC_002_2007','YCOC_002_2008','YCOC_002_2009','YCOC_002_2010','YCOC_003A_2003','YCOC_003A_2004','YCOC_003A_2005','YCOC_003A_2006','YCOC_003A_2007','YCOC_003A_2008','YCOC_003A_2009','YCOC_003A_2010',relvars)
MISSING_ALL_CHECK <- MISSING_ALL_DATA[,relvars]
MISSING_ALL_UNRESTRICTED_CHECK <- MISSING_ALL_UNRESTRICTED_DATA[,relvars]
MISSING_ATTENDED_CHECK <- MISSING_ATTENDED_DATA[,relvars]
MISSING_ATTENDED_UNRESTRICTED_CHECK <- MISSING_ATTENDED_UNRESTRICTED_DATA[,relvars]

write.csv(MISSING_ALL_CHECK, file = "D:/missing_all_check.csv")
write.csv(MISSING_ALL_UNRESTRICTED_CHECK, file = "D:/missing_all_unrestricted_check.csv")
write.csv(MISSING_ATTENDED_CHECK, file = "D:/missing_attended_check.csv")
write.csv(MISSING_ATTENDED_UNRESTRICTED_CHECK, file = "D:/missing_attended_unrestricted_check.csv")

#ycoc-002: eligibility for YCOC section, want 1
#ycoc-003A: ever applied to college or technical school

#delete those with no application data===============================================================================
COMPILED_DATA_APPSONLY <- COMPILED_DATA[COMPILED_DATA$APPLYER ==1,]
dim(COMPILED_DATA_APPSONLY)
write.csv(COMPILED_DATA_APPSONLY, file = "D:/COMPILED_DATA_APPLYER.csv")

#create applier-only output file for python college lookup==========================================================
  COMPILED_DATA_APPSONLY$COMPILED_APPLY[i] <-toString(COMPILED_DATA_APPSONLY$COMPILED_APPLY[i])
for (i in 1:nrow(COMPILED_DATA_APPSONLY)){
  COMPILED_DATA_APPSONLY$COMPILED_ADMIT[i] <-toString(COMPILED_DATA_APPSONLY$COMPILED_ADMIT[i])
}
PYTHON_OUT <- COMPILED_DATA_APPSONLY[,c('PUBID_1997','COLLEGE_GOER_FLAG','COLLEGES_SCHOOLID2','COMPILED_APPLY','COMPILED_APPLYALL','COMPILED_ADMIT')]
write.table(PYTHON_OUT, file = "D:/compiledcollegelist-app.txt", sep = "\t") #this is the input file for python

#getting the application info for the missings to check for trailing zero issue
#myList = c(15,35,40,158,180,221,245,286,300,305,396,459,472,548,555,561,562,632,633,638,650,827,853,865,878,911,916,917,929,936,948,950,957,969,988,989,1007,1013,1086,1114,1119,1243,1478,1483,1486,1590,1617,1656,1781,1812,1883,1885,1904,1969,2025,2055,2090,2109,2298,2338,2344,2467,2472,2537,2557,2563,2683,2756,2818,2877,2898,2951,3068,3107,3210,3211,3229,3245,3341,3378,3408,3456,3469,3683,3684,3703,3712,3771,3843,3847,3866,3898,3960,4013,4017,4047,4217,4239,4278,4300,4372,4375,4407,4437,4448,4458,4525,4599,4642,4702,4763,4885,4983,4994,5030,5151,5238,5268,5309,5381,5442,5446,5474,5478,5646,5702,5714,5733,5794,5855,5898,6048,6075,6133,6135,6164,6262,6263,6279,6375,6385,6429,6453,6458,6461,6493,6555,8796,8870,8909,8919)
#COMPILED_DATA[COMPILED_DATA$PUBID_1997 %in% myList,]$COMPILED_APPLYALL
#COMPILED_DATA[COMPILED_DATA$PUBID_1997 %in% myList,]$COLLEGES_ATTEND_VECTOR
#write.csv(COMPILED_DATA[COMPILED_DATA$PUBID_1997 %in% myList,]$COLLEGES_ATTEND_VECTOR, file = "D:/test.csv")

#fill in missing selectivity regression===============================================================================
SELECT_DATA <- read.csv("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Ycoc/Rinputmissingselect.csv")
testall <- lm(Selectivity ~ SAT_25+SAT_75+ADMIT_PER+Carnegie.class, data = SELECT_DATA)
summary(testall) #.47
testadmitper <- lm(Selectivity ~ADMIT_PER, data = SELECT_DATA)
summary(testadmitper)#.27
testcarnegie <- lm(Selectivity ~Carnegie.class, data = SELECT_DATA)
summary(testcarnegie)#.001
testsat <- lm(Selectivity ~SAT_25+SAT_75, data = SELECT_DATA)
summary(testsat)#.39
testsatandadmit <- lm(Selectivity ~SAT_25+SAT_75+ADMIT_PER, data = SELECT_DATA) 
summary(testsatandadmit)#.47

#print out data we need for determining choice year
TEST <- read.csv("D:/compiled_data.csv")
write.csv(TEST[,c('PUBID_1997', 'COLLEGEID_YEAR2','CHOICE_YEAR')], "D:/dates.csv")

