import sys
#from collections import Counter

#setup global variables
collegeList = [] #list of colleges
missedCollegeList = [] #list of colleges without info
crosswalkLookup = {} #returns an IPEDS ID for a FICE number
ipedsSet = set() #set of IPEDS numbers that are in the FICE dataset
ipedsSetfull = set() #complete set of IPEDS numbers
OPEIDcrosswalkLookup = {} #returns an IPEDS ID for an OPEID number
collegeDataLookup = {} #lookup table storing college info based on IPEDS number

class CollegeData:
	def __init__(self, bachFlag, control, selectivity):
		self.bachFlag, self.control, self.selectivity = bachFlag, control, selectivity

	def println(self):
		print str(self.bachFlag) + "\t" + str(self.control) +  "\t" + str(self.selectivity)

def main():
	
	#set up lookup tables for IPEDS existence
	crosswalkSetup()
	ipedsCheckSetup()

	#get list of colleges for which to extract information, and list missing colleges
	collegeListSetup()

	#try to fill in missing colleges using other years of IPEDS files and OPEIDs
	OPEIDScheck()
	otherIPEDScheck(2006)
	otherIPEDScheck(2005)
	otherIPEDScheck(2003)
	otherIPEDScheck(2002)
	otherIPEDScheck(2001)
	otherIPEDScheck(2011)
	printCollegeList()
	getSchoolCharacteristics()

	#merge with matched information--remove non-4-year schools and investigate public/private distinction
	#get info at http://nces.ed.gov/ipeds/datacenter/InstitutionByName.aspx?stepId=1

	#get Barron's numbers

	#populate accepted set for each individual (must be correct school type)

	#check that attended school was accepted

	#set highest accepted flag

	#set level of attended flag

def crosswalkSetup():
	global crosswalkLookup
	global ipedsSet
	crosswalkFile = open('C:/Users/Katharina/Documents/UMICH/Lifecycle choice/Data/ycoc//crosswalkfile.txt', 'r')
	for line in crosswalkFile.readlines():
		varList = line.split('\t')
		unitID = varList[0]
		name = varList[1]
		ficeID = varList[2]
		ficeID = str.replace(ficeID, '\n', '')
		if (len(ficeID) > 0 and ficeID not in crosswalkLookup):
			crosswalkLookup[ficeID]= unitID
		if unitID not in ipedsSet:
			ipedsSet.add(unitID)
	crosswalkFile.close()
	#print crosswalkLookup
	#print ipedsSet

def ipedsCheckSetup():
	global ipedsSetfull
	global OPEIDcrosswalkLookup
	ipedsFile = open('C:/Users/Katharina/Documents/UMICH/Lifecycle choice/Data/ycoc//hd2004.txt', 'r')
	for line in ipedsFile.readlines():
		varList = line.split('\t')
		unitID = varList[0]
		name = varList[1]
		opeid = varList[15] #this changes depending on file we are
		#print opeid
		if unitID not in ipedsSetfull:
			ipedsSetfull.add(unitID)
		if (opeid not in OPEIDcrosswalkLookup):
			OPEIDcrosswalkLookup[opeid]= unitID
	ipedsFile.close()
	#print OPEIDcrosswalkLookup

def collegeListSetup():
	global collegeList
	global missedCollegeList
	global ipedsSet
	global ipedsSetfull
	global crosswalkLookup
	global crosswalkLookupfull
	#collegeList = [] #list of colleges
	#missedCollegeList = [] #list of college we cannot find an ID for
	vectorList = open('C:/Users/Katharina/Documents/UMICH/Lifecycle choice/Data/ycoc/compiledcollegelist.txt', 'r')
	for line in vectorList.readlines():
		#read in variables
		varList = line.split('\t')
		PUBID_1997 = varList[1]
		COLLEGEGOER_FLAG = varList[2]
		COLLEGE_SCHOOLID = varList[3]
		COMPILED_APPLY = varList[4]
		COMPILED_ADMIT = varList[5]
		applyVector = COMPILED_APPLY.split(';')
		admitVector = COMPILED_ADMIT.split(';')
		for i in range(len(applyVector)):
			collegeList.append(applyVector[i])
		for i in range(len(admitVector)):
			collegeList.append(admitVector[i])
	vectorList.close()
	#clean up college list
	collegeList = list(set(collegeList)) #de-dupe
	print "Total number of unique IDs: " + str(len(collegeList))
	#try to replace missing ones with FICE; for future reference we could do it like this: map((lambda entry: other[entry]), x.intersection(y))
	for i in range(len(collegeList)):
		#collegeList[i]= str.replace(collegeList[i], '\"', '')
		if collegeList[i] not in ipedsSetfull:
			if (collegeList[i]) in crosswalkLookup:
				print "double foundit" #this doesn't happen -> we don't have any FICE codes
				collegeList[i] = crosswalkLookup[collegeList[i]]
			missedCollegeList.append(collegeList[i])
			collegeList[i] = -3
	collegeList = list(set(collegeList)) #de-dupe
	print "Total number of unique IDs that were in ipedsSetfull: " + str((len(collegeList)-1))
	print "Total number of entries in missed colleges list: " + str((len(missedCollegeList)))

def OPEIDScheck():
	global collegeList
	global missedCollegeList
	global crosswalkLookupfull
	global OPEIDcrosswalkLookup
	for i in range(len(missedCollegeList)):
		#print missedCollegeList[i]
		#missedCollegeList[i]= str.replace(missedCollegeList[i], '\"', '') #shouldnt need htis
		if (missedCollegeList[i]) in OPEIDcrosswalkLookup:
				print "triple foundit"  #this doesnt happen
				#missedCollegeList[i] = crosswalkLookup[collegeList[i]]

def otherIPEDScheck(myYear):
	global collegeList
	global missedCollegeList
	global crosswalkLookupfull
	global OPEIDcrosswalkLookup
	curIPEDS = open('C:/Users/Katharina/Documents/UMICH/Lifecycle choice/Data/ycoc/hd' + str(myYear) + '.txt', 'rb')
	for line in curIPEDS.readlines():
		varList = line.split('\t')
		unitID = varList[0]
		opeid = varList[15]
		if unitID not in ipedsSet:
			ipedsSet.add(unitID)
		if (opeid not in OPEIDcrosswalkLookup):
			OPEIDcrosswalkLookup[opeid]= unitID
	curIPEDS.close()
	missedCollegeListCopy = list(missedCollegeList)
	collegeListCopy = list(collegeList)
	for i in range(len(missedCollegeList)):
		if (missedCollegeList[i]) in ipedsSet:
			#print "quadruple found it" + str(myYear) #this happens
			collegeListCopy.append(missedCollegeList[i])
			missedCollegeListCopy.pop(i)
		elif (missedCollegeList[i]) in OPEIDcrosswalkLookup:
			#print "quintuple found it" + str(myYear) #this happens
			collegeListCopy.append(OPEIDcrosswalkLookup[missedCollegeList[i]])
			missedCollegeListCopy.pop(i)
	missedCollegeList = missedCollegeListCopy
	collegeList = collegeListCopy

def printCollegeList():	
	global collegeList
	global missingCollegeList
	print "Total number of unique IDs that are found after using all means: " + str((len(collegeList)-1))
	collegeListStr = str(collegeList)
	collegeListStr= str.replace(collegeListStr, ' ', '')
	collegeListStr= str.replace(collegeListStr, ',-3', '')
	collegeListStr= str.replace(collegeListStr, '[', '')
	collegeListStr= str.replace(collegeListStr, ']', '')
	collegeListStr= str.replace(collegeListStr, '\'', '')
	#collegeListStr= str.replace(collegeListStr, '\"', '')
	missedCollegeListStr = str(missedCollegeList)
	missedCollegeListStr= str.replace(missedCollegeListStr, ' ', '')
	missedCollegeListStr= str.replace(missedCollegeListStr, '[', '')
	missedCollegeListStr= str.replace(missedCollegeListStr, ']', '')
	missedCollegeListStr= str.replace(missedCollegeListStr, '\'', '')
	#missedCollegeListStr= str.replace(missedCollegeListStr, '\"', '')
	outFile = open('C:/Users/Katharina/Documents/UMICH/Lifecycle choice/Data/ycoc/uniqueColleges.txt', 'w')
	outFile.write(collegeListStr)
	outFile.close()
	outFile2 = open('C:/Users/Katharina/Documents/UMICH/Lifecycle choice/Data/ycoc/missedColleges.txt', 'w')
	outFile2.write(missedCollegeListStr)
	outFile2.close()

def getSchoolCharacteristics(): #TBD create data structure for storing info on relevant schools
	global collegeList
	global collegeDataLookup
	#populate 
	for i in range(len(collegeList)):
		if (collegeList[i]) not in collegeDataLookup:
			curBachFlag = 1 #we havent done this yet
			curControl = 1 #we haven't done this yet
			curSelectivity = 1  #we haven't done this yet
			curColData = CollegeData(curBachFlag, curControl, curSelectivity)
			collegeDataLookup[collegeList[i]]= curColData


if __name__ == '__main__':
	main()