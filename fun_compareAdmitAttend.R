compareAdmitAttend<- function(inData){

  #check the importance of admission versus attendance
    #get only attenders
      inData = inData[inData$attend != -10,]
    #replace attendance with percentage
      inData$attend2 = NA
      inData$admit2 = NA
      lookup = data.frame(id = c(1,2,3,4,5), percent = c(33, 60, 75, 85, 100))
      for (i in 1:nrow(inData)){
        inData$attend2[i] = lookup[(1:dim(lookup)[1])[lookup[,1]==inData$attend[i]],2]
        inData$admit2[i] = lookup[(1:dim(lookup)[1])[lookup[,1]==inData$admit[i]],2]
      }
    #model
      myModStraight = lm(b0~admit+attend, data = inData)
      print(summary(myModStraight))
      myModInterval = lm(b0~admit2+attend2, data = inData)
      print(summary(myModInterval))
      
    #other diagnostics
      #correlation
        testDat = data.frame(admit = inData$admit, attend = inData$attend, admit2 = inData$admit2, attend2 = inData$attend2)
        cor(testDat)
      #predict attendance from admission
        attMod = lm(attend~admit, data =inData) #this clearly isn't linear
        attModInterval = lm(attend2~admit2, data = inData)
        inData$attendError = attModInterval$residuals
        myModAttendError = lm(b0~admit2+attendError, data = inData)
        print(summary(myModAttendError))
        testDat = data.frame(admit2 = inData$admit2, attendError = inData$attendError)
        cor(testDat)
      #predict admission from attendance
        admitModInterval = lm(admit2~attend2, data = inData)
        inData$admitError = admitModInterval$residuals
        myModAdmitError = lm(b0~attend+admitError, data = inData)
        print(summary(myModAdmitError))
        testDat = data.frame(admitError = inData$admitError, attend2 = inData$attend2)
        cor(testDat)
      
      return(inData)
}