#NOTE: Remove income entries where person has not worked at least 1200 hours this year

removeNotFT <- function(IncomeVectorList, AgeVectorList, EnrollmentVectorList, EmploymentVectorList){ #this one does not change the intermediate values
  outInc = list()
  outAge = list()
  outEnroll = list()
  outEmploy= list()
  for (i in 1:length(IncomeVectorList)){
    outInc[[i]] = IncomeVectorList[[i]]
    outAge[[i]] = AgeVectorList[[i]]
    outEnroll[[i]] = EnrollmentVectorList[[i]]
    outEmploy[[i]]= EmploymentVectorList[[i]]
    if (length(IncomeVectorList[[i]]) >1){ #we dont have a -3
      removeVect = c()
      for (j in 1:length(IncomeVectorList[[i]])){
        if (EmploymentVectorList[[i]][j]<1200){# did not work enough hours
          outInc[[i]][j] = -3
          #outAge[[i]][j] = -3 # we dont change age so that we can index properly at end
          outEnroll[[i]][j] = -3
          outEmploy[[i]][j] = -3
        }
      }
    }
  }
  outList = list(outInc, outAge, outEnroll, outEmploy)
  return(outList)
}