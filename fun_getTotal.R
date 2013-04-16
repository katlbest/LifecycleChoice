#NOTES: function returns a vector of incomes in a particular category
  #data is the dataset to use
  #indicator is na indicator that determines whether data should be available (did person answer ?)
  #main variable is the primary variable
  #secondvar is the secondary question. if someone did not answer main question, a clarifying question is often asked, and the answers are recorded in this variable
  #clarify is an indicator of whether clarifying variable exists
  #lookuptype chooses which scale to use in order to change categorical variables to values

getTotal <- function(data, indicator, mainvar, secondvar, clarify, lookupType) { 
  source("C:/Users/Katharina/Documents/Umich/Lifecycle Choice/Data/Data manipulation/fun_lookupCategory.R")
  totalVect <- rep(0, nrow(data))
  for (j in 1:nrow(data)){
    #if indicator is positive
    if (clarify == "None"){#this means we dont have a clarifying question
      realIndicator = data[j, indicator]
    } else {
      realIndicator = max(data[j, indicator],data[j, clarify])
    }
    if (realIndicator == 1){#some income received
      if (data[j, mainvar] > 0){
        totalVect[j]= data[j, mainvar]
      }
      else if(data[j, secondvar] > 0){
        totalVect[j]= lookupCategory(lookupType, data[j, secondvar])
      }
    }
  }
  invisible(return(totalVect)) #output vector of incomes
}