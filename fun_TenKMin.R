#NOTE: remove any income values below $10,000

TenKMin <- function(IncomeVectorList, AgeVectorList, EnrollmentVectorList, EmploymentVectorList){ #this one does not change the intermediate values
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
      if (IncomeVectorList[[i]][j]<10000){
        outInc[[i]][j] = -3
      }
    }
  }
  outList = list(outInc, outAge, outEnroll, outEmploy)
  return(outList)
}