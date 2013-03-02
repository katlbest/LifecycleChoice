import sys
#from collections import Counter

#setup global variables
collegeList = [] #list of colleges
missedCollegeList = []
crosswalkLookup = {} #returns an IPEDS ID for a FICE number
ipedsLookup = set() #set of IPEDS numbers that are in the FICE dataset
ipedsLookupfull = set() #complete set of IPEDS numbers
OPEIDcrosswalkLookup = {} #returns an IPEDS ID for an OPEID number

def main():
	
	#set up lookup tables for IPEDS existence
	crosswalkSetup()
	ipedsCheckSetup()

	#get list of colleges for which to extract information, and list missing colleges
	collegeListSetup()

	#try to fill in missing colleges using other years of IPEDS files and OPEIDs
	OPEIDScheck()
	otherIPEDScheck(2006)

	#merge with matched information--remove non-4-year schools and investigate public/private distinction
	#get info at http://nces.ed.gov/ipeds/datacenter/InstitutionByName.aspx?stepId=1

	#get Barron's numbers

	#populate accepted set for each individual (must be correct school type)

	#check that attended school was accepted

	#set highest accepted flag

	#set level of attended flag


def crosswalkSetup():
	global crosswalkLookup
	global ipedsLookup
	crosswalkFile = open('C:/Users/Katharina/Documents/UMICH/Lifecycle choice/Data/ycoc//crosswalkfile.txt', 'r')
	for line in crosswalkFile.readlines():
		varList = line.split('\t')
		unitID = varList[0]
		name = varList[1]
		ficeID = varList[2]
		ficeID = str.replace(ficeID, '\n', '')
		if (len(ficeID) > 0 and ficeID not in crosswalkLookup):
			crosswalkLookup[ficeID]= unitID
		if unitID not in ipedsLookup:
			ipedsLookup.add(unitID)
	crosswalkFile.close()
	#print crosswalkLookup
	#print ipedsLookup

def ipedsCheckSetup():
	global ipedsLookupfull
	global OPEIDcrosswalkLookup
	ipedsFile = open('C:/Users/Katharina/Documents/UMICH/Lifecycle choice/Data/ycoc//HD2004.csv', 'r')
	for line in ipedsFile.readlines():
		varList = line.split(',')
		unitID = varList[0]
		name = varList[1]
		opeid = varList[15] #this changes depending on file we are
		#print opeid
		if unitID not in ipedsLookupfull:
			ipedsLookupfull.add(unitID)
		if (opeid not in OPEIDcrosswalkLookup):
			OPEIDcrosswalkLookup[opeid]= unitID
	ipedsFile.close()
	#print OPEIDcrosswalkLookup

def collegeListSetup():
	global collegeList
	global missedCollegeList
	global ipedsLookup
	global crosswalkLookup
	global crosswalkLookupfull
	#collegeList = [] #list of colleges
	#missedCollegeList = [] #list of college we cannot find an ID for
	vectorList = open('C:/Users/Katharina/Documents/UMICH/Lifecycle choice/Data/ycoc/compiledcollegelist.csv', 'r')
	for line in vectorList.readlines():
		#read in variables
		varList = line.split(',')
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
	print len(collegeList)
	#try to replace missing ones with FICE
	for i in range(len(collegeList)):
		collegeList[i]= str.replace(collegeList[i], '\"', '')
		if collegeList[i] not in ipedsLookup:
			if (collegeList[i]) in crosswalkLookup:
				print "double foundit" #this doesn't happen -> we don't have any FICE codes
				collegeList[i] = crosswalkLookup[collegeList[i]]
	for i in range(len(collegeList)):
		if collegeList[i] not in ipedsLookupfull:
			missedCollegeList.append(collegeList[i])
			collegeList[i] = -3
	collegeList = list(set(collegeList)) #de-dupe
	print (len(collegeList)-1)
	collegeListStr = str(collegeList)
	collegeListStr= str.replace(collegeListStr, ' ', '')
	collegeListStr= str.replace(collegeListStr, ',-3', '')
	collegeListStr= str.replace(collegeListStr, '[', '')
	collegeListStr= str.replace(collegeListStr, ']', '')
	collegeListStr= str.replace(collegeListStr, '\'', '')
	collegeListStr= str.replace(collegeListStr, '\"', '')
	missedCollegeListStr = str(missedCollegeList)
	missedCollegeListStr= str.replace(missedCollegeListStr, ' ', '')
	missedCollegeListStr= str.replace(missedCollegeListStr, '[', '')
	missedCollegeListStr= str.replace(missedCollegeListStr, ']', '')
	missedCollegeListStr= str.replace(missedCollegeListStr, '\'', '')
	missedCollegeListStr= str.replace(missedCollegeListStr, '\"', '')
	outFile = open('C:/Users/Katharina/Documents/UMICH/Lifecycle choice/Data/ycoc/uniqueColleges.txt', 'w')
	outFile.write(collegeListStr)
	outFile.close()
	outFile2 = open('C:/Users/Katharina/Documents/UMICH/Lifecycle choice/Data/ycoc/missedColleges.txt', 'w')
	outFile2.write(missedCollegeListStr)
	outFile2.close()

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
	global ipedsLookup
	curIPEDS = open('C:/Users/Katharina/Documents/UMICH/Lifecycle choice/Data/ycoc/hd' + str(myYear) + '.csv', 'rb')
	for line in curIPEDS.readlines():
		varList = line.split(',')
		unitID = varList[0]
		if unitID not in ipedsLookup:
			ipedsLookup.add(unitID)
	curIPEDS.close()
	for i in range(len(missedCollegeList)):
		if (missedCollegeList[i]) in ipedsLookup:
			print "quadruple found it" + str(myYear)

	

if __name__ == '__main__':
	main()