import sys
#from collections import Counter

def crosswalkSetup():
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

def collegeListSetup():
	collegeList = [] #list of colleges
	missedCollegeList = [] #list of college we cannot find an ID for
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
		if collegeList[i] not in ipedsLookup:
			if collegeList[i] in crosswalkLookup:
				print len(crosswalkLookup[collegeList[i]]) #none of the mising ones are valid fice IDs according to my crosswalk
				print crosswalkLookup[collegeList[i]]
			else:
				missedCollegeList.append(collegeList[i])
				collegeList[i] = -3
	collegeList = list(set(collegeList)) #de-dupe again
	print (len(collegeList)-1) #minus one because we still have on -3 in here
	collegeList = str(collegeList)
	collegeList= str.replace(collegeList, ' ', '')
	collegeList= str.replace(collegeList, ',-3', '') #get rid of the fake negative 3
	collegeList= str.replace(collegeList, '[', '')
	collegeList= str.replace(collegeList, ']', '')
	collegeList= str.replace(collegeList, '\'', '')
	collegeList= str.replace(collegeList, '\"', '')
	outFile = open('C:/Users/Katharina/Documents/UMICH/Lifecycle choice/Data/ycoc/uniqueColleges.txt', 'w')
	outFile.write(collegeList)
	outFile.close()
	outFile2 = open('C:/Users/Katharina/Documents/UMICH/Lifecycle choice/Data/ycoc/missedColleges.txt', 'w')
	outFile2.write(collegeList)
	outFile2.close()

if __name__ == '__main__':
	#setup global variables
	#collegeList = list() #list of colleges
	crosswalkLookup = {} #returns an IPEDS ID for a FICE number
	ipedsLookup = set()
	
	crosswalkSetup()
	collegeListSetup()

