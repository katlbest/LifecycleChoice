
from21 <- function(IncomeVectorList, AgeVectorList, EnrollmentVectorList, EmploymentVectorList){ #this one does not change the intermediate values
  outInc = list()
  outAge = list()
  outEnroll = list()
  outEmploy= list()
  for (i in 1:length(IncomeVectorList)){
    outInc[[i]] = IncomeVectorList[[i]]
    outAge[[i]] = AgeVectorList[[i]]
    outEnroll[[i]] = EnrollmentVectorList[[i]]
    outEmploy[[i]]= EmploymentVectorList[[i]]
    for (j in 1:length(IncomeVectorList[[i]])){
      if (AgeVectorList[[i]][j]<22){
        outInc[[i]][j] = -3
      }
    }
  }
  outList = list(outInc, outAge, outEnroll, outEmploy)
  return(outList)
}