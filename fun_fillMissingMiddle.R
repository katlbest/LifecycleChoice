#NOTE: remove any income values below $10,000 and fills in values that are in the middle of a string of filled ones

fillMissingMiddle <- function(IncomeVectorList, AgeVectorList, EnrollmentVectorList, EmploymentVectorList){ #this one does not change the intermediate values
  outInc = list()
  outAge = list()
  outEnroll = list()
  outEmploy= list()
  for (i in 1:length(IncomeVectorList)){
    firstBigIndex = 0
    outInc[[i]] = IncomeVectorList[[i]]
    outAge[[i]] = AgeVectorList[[i]]
    outEnroll[[i]] = EnrollmentVectorList[[i]]
    outEmploy[[i]]= EmploymentVectorList[[i]]
    if (length(IncomeVectorList[[i]]) >1){ #we dont have a -3
      if (IncomeVectorList[[i]][1]>=10000){ #changed this to 10000, start with first 10000 value
        firstBigIndex =1
      }
      for (j in 2:length(IncomeVectorList[[i]])){
        if (IncomeVectorList[[i]][j]<IncomeVectorList[[i]][j-1]){
          IncomeVectorList[[i]][j]=IncomeVectorList[[i]][j-1]
        }
      }
      for (j in 2:length(IncomeVectorList[[i]])){
        if (firstBigIndex == 0){
          if (IncomeVectorList[[i]][j] >= 10000){
            firstBigIndex = j
          }
        }
      }
      if (firstBigIndex ==0){
        outInc[[i]]= c(-3)
        outAge[[i]]= c(-3)
        outEnroll[[i]]= c(-3)
        outEmploy[[i]]= c(-3)
      }
      else{
        outInc[[i]]= IncomeVectorList[[i]][firstBigIndex:length(IncomeVectorList[[i]])]
        outAge[[i]]= AgeVectorList[[i]][firstBigIndex:length(AgeVectorList[[i]])]
        outEnroll[[i]]= EnrollmentVectorList[[i]][firstBigIndex:length(EnrollmentVectorList[[i]])]
        outEmploy[[i]]= EmploymentVectorList[[i]][firstBigIndex:length(EmploymentVectorList[[i]])]
      }
    }
    else{
      outInc[[i]]= IncomeVectorList[[i]][firstBigIndex:length(IncomeVectorList[[i]])]
      outAge[[i]]= AgeVectorList[[i]][firstBigIndex:length(AgeVectorList[[i]])]
      outEnroll[[i]]= EnrollmentVectorList[[i]][firstBigIndex:length(EnrollmentVectorList[[i]])]
      outEmploy[[i]]= EmploymentVectorList[[i]][firstBigIndex:length(EmploymentVectorList[[i]])]
    }
  }
  outList = list(outInc, outAge, outEnroll, outEmploy)
  return(outList)
}