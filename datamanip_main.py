#import modules========================================================================================
import sys

#set up global variables===============================================================================
collegeList = [] #list of colleges
missedCollegeList = [] #list of colleges without info
crosswalkLookup = {} #returns an IPEDS ID for a FICE number
ipedsSetFull = set() #complete set of IPEDS IDs for which we have data
OPEIDcrosswalkLookup = {} #returns an IPEDS ID for an OPEID number
collegeDataLookup = {} #lookup table storing college info based on IPEDS number

#define classes========================================================================================
class CollegeData: #class storing college data
	def __init__(self, bachFlag, control, selectivity):
		self.bachFlag, self.control, self.selectivity = bachFlag, control, selectivity

	def println(self):
		print str(self.bachFlag) + "\t" + str(self.control) +  "\t" + str(self.selectivity)

#main===================================================================================================
def main():
	#get list of colleges for which to extract information
	collegeListSetup()
	#try to fill in missing colleges using multiple years of IPEDS files and the OPEIDS crosswalk
	IPEDScheck(2004)
	IPEDScheck(2006)
	IPEDScheck(2005)
	IPEDScheck(2003)
	IPEDScheck(2002)
	IPEDScheck(2001)
	IPEDScheck(2011)
	#try to replace schools using FICE codes where possible
	FICEcrosswalkSetup() #set up lookup that returns an IPEDS ID for a FICE number
	FICEcheck() #do replacement where applicable
	#print college lists to filed fof future use
	printCollegeList()

	#getSchoolCharacteristics()

	#merge with matched information--remove non-4-year schools and investigate public/private distinction
	#get info at http://nces.ed.gov/ipeds/datacenter/InstitutionByName.aspx?stepId=1

	#get Barron's numbers

	#populate accepted set for each individual (must be correct school type)

	#check that attended school was accepted

	#set highest accepted flag

	#set level of attended flag

#function definitions==============================================================================================
def collegeListSetup(): #extract list of colleges people have attended
	global missedCollegeList
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
			missedCollegeList.append(applyVector[i])
		for i in range(len(admitVector)):
			missedCollegeList.append(admitVector[i])
	vectorList.close()
	#clean up college list
	missedCollegeList = list(set(missedCollegeList)) #de-dupe
	print "Total number of unique IDs: " + str(len(missedCollegeList))

def IPEDScheck(myYear): #look up colleges in the list in myYear's ipeds list and this year's OPEID list; create OPEID lookup table based on this year's data
	global collegeList
	global missedCollegeList
	global OPEIDcrosswalkLookup
	curIPEDS = open('C:/Users/Katharina/Documents/UMICH/Lifecycle choice/Data/ycoc/hd' + str(myYear) + '.txt', 'rb')
	#update set of "known" IDs and OPEID lookup table
	for line in curIPEDS.readlines():
		varList = line.split('\t')
		unitID = varList[0]
		opeid = varList[15]
		if unitID not in ipedsSetFull: #update current known IPED IDs
			ipedsSetFull.add(unitID)
			#also set up the data structure here, TBD
		if (opeid not in OPEIDcrosswalkLookup): #update OPEID crosswalk
			OPEIDcrosswalkLookup[opeid]= unitID
	curIPEDS.close()
	missedCollegeListCopy = list(missedCollegeList)
	collegeListCopy = list(collegeList)
	for i in range(len(missedCollegeList)): #try to ID missed colleges by IPEDS
		if (missedCollegeList[i]) in ipedsSetFull:
			collegeListCopy.append(missedCollegeList[i])
			missedCollegeListCopy.remove(missedCollegeList[i])
		elif (missedCollegeList[i]) in OPEIDcrosswalkLookup: #try to ID missed colleges by OPEID
			print "code 1: found college by OPEID lookup in year " + str(myYear) #this happens once
			collegeListCopy.append(OPEIDcrosswalkLookup[missedCollegeList[i]])
			missedCollegeListCopy.remove(missedCollegeList[i])
	missedCollegeList = missedCollegeListCopy
	collegeList = collegeListCopy
	collegeList = list(set(collegeList)) #de-dupe
	missedCollegeList = list(set(missedCollegeList)) #de-dupe
	#print "Total number of found colleges after searching " + str(myYear) + ": " + str((len(collegeList)))

def FICEcrosswalkSetup(): #set up crosswalk for FICE number
	global crosswalkLookup
	crosswalkFile = open('C:/Users/Katharina/Documents/UMICH/Lifecycle choice/Data/ycoc//crosswalkfile.txt', 'r')
	for line in crosswalkFile.readlines():
		varList = line.split('\t')
		unitID = varList[0]
		name = varList[1]
		ficeID = varList[2]
		ficeID = str.replace(ficeID, '\n', '')
		if (len(ficeID) > 0 and ficeID not in crosswalkLookup):
			crosswalkLookup[ficeID]= unitID
	crosswalkFile.close()

def FICEcheck(): #use FICE crosswalk to try to fill in missing information
	global missedCollegeList
	global collegeList
	global crosswalkLookup
	missedCollegeListCopy = list(missedCollegeList)
	collegeListCopy = list(collegeList)
	for i in range(len(missedCollegeList)):
		if (missedCollegeList[i]) in crosswalkLookup:
				print "code 2: found college by FICE check" #this doesn't happen
				collegeListCopy.append(crosswalkLookup[missedCollegeList[i]])
				missedCollegeListCopy.remove(missedCollegeList[i])
	missedCollegeList = missedCollegeListCopy
	collegeList = collegeListCopy
	collegeList = list(set(collegeList)) #de-dupe

def printCollegeList():	
	global collegeList
	global missingCollegeList
	print "Total number of unique IDs that are found after using all means: " + str((len(collegeList)))
	print "Total number of entries in missed colleges list: " + str((len(missedCollegeList)))
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



