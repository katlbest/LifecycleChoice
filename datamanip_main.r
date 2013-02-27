library(sqldf)

#library(epicalc)     #why do i need epicalc?
#notes=======================================================================
#to clear data: rm(list = ls(all = TRUE))
#to replace missing values: MERGED_DATA[MERGED_DATA < 0] <- NA

#MERGED_DATA<-read.csv("C:/Users/katley/Documents/DataCollection/MERGED_DATA.csv")
MERGED_DATA <- read.csv("D:/MERGED_DATA.csv")
drops <- c("GEO70.01_1999","GEO70.01_2000","GEO70.01_2001","GEO70.01_2002","GEO70.01_2003","GEO70.01_2004","GEO70.01_2005","GEO70.01_2006","GEO70.01_2007","GEO70.01_2008","GEO70.01_2009","GEO70.01_2010","GEO70.02_1998","GEO70.02_1999","GEO70.02_2000","GEO70.02_2001","GEO70.02_2002","GEO70.02_2003","GEO70.02_2004","GEO70.02_2005","GEO70.02_2006","GEO70.02_2007","GEO70.02_2008","GEO70.02_2009","GEO70.02_2010","GEO70.03_1998","GEO70.03_1999","GEO70.03_2000","GEO70.03_2001","GEO70.03_2002","GEO70.03_2003","GEO70.03_2004","GEO70.03_2005","GEO70.03_2006","GEO70.03_2007","GEO70.03_2008","GEO70.03_2009","GEO70.03_2010","GEO70.04_1998","GEO70.04_1999","GEO70.04_2000","GEO70.04_2001","GEO70.04_2002","GEO70.04_2003","GEO70.04_2004","GEO70.04_2005","GEO70.04_2006","GEO70.04_2008","GEO70.04_2010","GEO70.05_1998","GEO70.05_1999","GEO70.05_2001","GEO70.05_2003","GEO70.05_2004","GEO70.05_2006","GEO70.05_2007","GEO70.05_2008","GEO70.05_2010","GEO70.06_2001","GEO70.06_2008","GEO70.07_2008","GEO70.08_2008","GEO70_1997","TRANS_GRADREQ_ENGL_1999","TRANS_GRADREQ_MATH_1999","TRANS_GRADREQ_SCI_1999","TRANS_GRADREQ_SOC_1999","TRANS_GRADREQ_SOURCE_1999","TRANS_GRADREQ_TOTAL_1999","TRANS_GRADREQ_TYPE_1999","GEO69_1997","GEO69.01_1998","GEO69.01_1999","GEO69.01_2000","GEO69.01_2001","GEO69.01_2002","GEO69.01_2003","GEO69.01_2004","GEO69.01_2005","GEO69.01_2006","GEO69.01_2007","GEO69.01_2008","GEO69.01_2009","GEO69.01_2010","GEO69.02_1998","GEO69.02_1999","GEO69.02_2000","GEO69.02_2001","GEO69.02_2002","GEO69.02_2003","GEO69.02_2004","GEO69.02_2005","GEO69.02_2006","GEO69.02_2007","GEO69.02_2008","GEO69.02_2009","GEO69.02_2010","GEO69.03_1998","GEO69.03_1999","GEO69.03_2000","GEO69.03_2001","GEO69.03_2002","GEO69.03_2003","GEO69.03_2004","GEO69.03_2005","GEO69.03_2006","GEO69.03_2007","GEO69.03_2008","GEO69.03_2009","GEO69.03_2010","GEO69.04_1998","GEO69.04_1999","GEO69.04_2000","GEO69.04_2001","GEO69.04_2002","GEO69.04_2003","GEO69.04_2004","GEO69.04_2005","GEO69.04_2006","GEO69.04_2007","GEO69.04_2008","GEO69.04_2010","GEO69.05_1998","GEO69.05_1999","GEO69.05_2001","GEO69.05_2003","GEO69.05_2004","GEO69.05_2006","GEO69.05_2007","GEO69.05_2008","GEO69.05_2010","GEO69.06_2001","GEO69.06_2008","GEO69.07_2008","GEO69.08_2008")
MERGED_DATA <- MERGED_DATA[,!(names(MERGED_DATA) %in% drops)]
MERGED_DATA_STORE<-MERGED_DATA

#get smaller test set
MERGED_DATA<-MERGED_DATA[1:100,]

#application/admission==========================================================
#empty vectors for storage
MERGED_DATA["COLLEGES_APPLY_VECTOR"] <- "" #vector of applied schools
MERGED_DATA["COLLEGES_APPLYALL_VECTOR"] <- "" #vector of applied schools including outside of term limit
MERGED_DATA["COLLEGES_ADMIT_VECTOR"] <- "" #vector of admitted schools
MERGED_DATA["COLLEGES_TERM_VECTOR"] <- "" #vector of admitted schools
MERGED_DATA["ATTENDED_IN_APPLIED"] <- 0 #indicator of whether you applied where you went

#create comma-separated list of applied, applied within term limits, and admitted
i = 1 #school
j = 1 #year
year_vect = c('_2004','_2005','_2006','_2007','_2008','_2009','_2010')
school_vect = c('.01','.02','.03','.04','.05','.06','.07','.08','.09','.10')
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
        curMin = MERGED_DATA$CHOICE_YEAR[k]
        curMax = MERGED_DATA$SCHOOLID_YEAR[k]+1
        if (eval(parse(text =curstr))[k] > 0){    #check for existence of application ID
          curTerm = ((eval(parse(text =curstr2))[k] +1) %/% 4) +1997
          MERGED_DATA$COLLEGES_APPLYALL_VECTOR[k]= paste(MERGED_DATA$COLLEGES_APPLYALL_VECTOR[k], eval(parse(text =curstr))[k], ",", sep = "")
          MERGED_DATA$COLLEGES_TERM_VECTOR[k]= paste(MERGED_DATA$COLLEGES_TERM_VECTOR[k], toString(curTerm), ",", sep = "")
          if (curTerm > curMin & curTerm < curMax){
            MERGED_DATA$COLLEGES_APPLY_VECTOR[k]= paste(MERGED_DATA$COLLEGES_APPLY_VECTOR[k], eval(parse(text =curstr))[k], ",", sep = "")            
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
          }
        }  
      }
    }
  }
}

#write.csv(MERGED_DATA, file = "D:/MERGED_DATA2.csv")
#write.csv(MERGED_DATA, file = "C:/Users/katley/Documents/DataCollection/MERGED_DATA2.csv")

#count number of schools each person applied to and check whether attended school is in vector
MERGED_DATA <- read.csv("D:/MERGED_DATA2.csv")
curmax = 0
for (i in 1:nrow(MERGED_DATA)){
  curstr = toString(MERGED_DATA$COLLEGES_APPLY_VECTOR[i])
  curstr= substr(curstr, 1, nchar(curstr)-1)
  curmax = max(curmax,length(eval(parse(text = paste("c(", curstr, ")", sep = "")))))
  MERGED_DATA["ATTENDED_IN_APPLIED"][i]= (toString(MERGED_DATA$SCHOOL_ID[i]))
}

#find term of application to school one went to and check that its in the time frame we want


#=======================================================================================
#old stuff
#for testing, find a person that has an entry in termnum and see why it doesnt work
#MERGED_DATA$PUBID <- MERGED_DATA$PUBID_1997
#MERGED_DATA$TERM <- MERGED_DATA$PREV_COL_APP_TERMNUM.01_2004
#MERGED_DAT <- df <- data.frame(matrix(ncol = 0, nrow = 6357))
#MERGED_DAT$PUBID <- MERGED_DATA$PUBID
#MERGED_DAT$TERM <- MERGED_DATA$TERM
#sapply(MERGED_DAT, typeof)
#sqlStr = 'select PUBID,TERM from MERGED_DAT where TERM > 10'
#test = sqldf(sqlStr)
#print test
