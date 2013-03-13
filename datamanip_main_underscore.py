#import modules========================================================================================
import sys

#set up global variables===============================================================================
collegeList = [] #list of colleges
missedCollegeList = [] #list of colleges without info
missedSelectivityList = [] #list of schools among 4-year schools for which selectivity is missing
crosswalkLookup = {} #returns an IPEDS ID for a FICE number
collegeDataLookup = {} #returns collegeData object for an IPEDS number
OPEIDcrosswalkLookup = {} #returns an IPEDS ID for an OPEID number
collegeDataLookup = {} #lookup table storing college info based on IPEDS number

#define classes========================================================================================
class CollegeData: #class storing college data
	def __init__(self, colName, bachFlag, control, selectivity):
		self.colName, self.bachFlag, self.control, self.selectivity = colName, bachFlag, control, selectivity

	def __str__(self):
		return str(self.colName) + "\t" + str(self.bachFlag) + "\t" + str(self.control) +  "\t" + str(self.selectivity)

#main===================================================================================================
def main():
	#get list of colleges for which to extract information
	collegeListSetup()

	#try to fill in missing colleges using multiple years of IPEDS files and the OPEIDS crosswalk; populatecolelgedatalookup with data that comes from this file
	IPEDScheck(2004)
	IPEDScheck(2006)
	IPEDScheck(2005)
	IPEDScheck(2003)
	IPEDScheck(2002)
	IPEDScheck(2001)
	IPEDScheck(2011)
	BarronsSetup() #add barron's selectivity to infor we have about schools
	
	#try to replace schools using FICE codes where possible
	FICEcrosswalkSetup() #set up lookup that returns an IPEDS ID for a FICE number
	FICEcheck() #do replacement where applicable

	#print college lists for future use
	printCollegeList()

	#delete colleges from found list that do not indicate having 4-year data
	deleteNonCollege()

	#check whether there is anything different about the schools for which people have missing data
	checkMissings()

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
	vectorList = open('D:/compiledcollegelist.txt', 'r')
	lines = vectorList.readlines()
	for line in lines[1:]:
	#for line in vectorList:
		#read in variables
		varList = line.split('\t')
		PUBID_1997 = int(varList[1])
		COLLEGEGOER_FLAG = int(varList[2])
		COLLEGE_SCHOOLID = int(varList[3])
		missedCollegeList.append(str(COLLEGE_SCHOOLID))
		COMPILED_APPLY = varList[4]
		COMPILED_ADMIT = varList[5]
		applyVector = COMPILED_APPLY.split(',')
		admitVector = COMPILED_ADMIT.split(',')
		for i in range(len(applyVector)):
			applyVector[i] = applyVector[i].replace('"','')
			missedCollegeList.append(applyVector[i])
		for i in range(len(admitVector)):
			admitVector[i] = admitVector[i].replace('"','')
			missedCollegeList.append(admitVector[i])
	vectorList.close()
	#clean up college list
	#print "Total number of unique IDs: " + str(len(missedCollegeList))
	#outFiletest = open('C:/Users/Katharina/Documents/UMICH/Lifecycle choice/Data/ycoc/beforededupe.txt', 'w')
	#outFiletest.write(str(missedCollegeList))
	#outFiletest.close()
	#print "test" + str(len(missedCollegeList))
	missedCollegeList = list(set(missedCollegeList)) #de-dupe
	missedCollegeList.pop(0)
	print "Total number of unique IDs: " + str(len(missedCollegeList))

def IPEDScheck(myYear): #look up colleges in the list in myYear's ipeds list and this year's OPEID list; create OPEID lookup table based on this year's data
	global collegeList
	global missedCollegeList
	global OPEIDcrosswalkLookup
	global collegeDataLookup
	curIPEDS = open('C:/Users/Katharina/Documents/UMICH/Lifecycle choice/Data/ycoc/hd' + str(myYear) + '.txt', 'rb')
	#update set of "known" IDs and OPEID lookup table
	for line in curIPEDS.readlines():
		varList = line.split('\t')
		unitID = varList[0]
		colName = varList[1]
		opeid = varList[15]
		curBachFlag = varList[19]
		curControl = varList[20]
		if unitID not in collegeDataLookup: #update current known IPED IDs#
			curSelectivity = -3  #we haven't done this yet
			curColData = CollegeData(colName, curBachFlag, curControl, curSelectivity)
			collegeDataLookup[unitID] = curColData
			#print collegeDataLookup[unitID]
		if (opeid not in OPEIDcrosswalkLookup): #update OPEID crosswalk
			OPEIDcrosswalkLookup[opeid]= unitID
	curIPEDS.close()
	missedCollegeListCopy = list(missedCollegeList)
	collegeListCopy = list(collegeList)
	for i in range(len(missedCollegeList)): #try to ID missed colleges by IPEDS
		if (missedCollegeList[i]) in collegeDataLookup:
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

def BarronsSetup(): #lookup for Barron's selectivity
	barronsFile = open('C:/Users/Katharina/Documents/UMICH/Lifecycle choice/Data/ycoc//barrons04.txt', 'r')
	for line in barronsFile.readlines():
		varList = line.split('\t')
		unitID = varList[1]
		curSelectivity = varList[8]
		if unitID in collegeDataLookup:
			collegeDataLookup[unitID].selectivity = curSelectivity
	barronsFile.close()
	barronsFile2 = open('C:/Users/Katharina/Documents/UMICH/Lifecycle choice/Data/ycoc//barrons08.txt', 'r')
	for line in barronsFile2.readlines(): #there are no cases where 04 has no data and 08 does, but just checking
		varList = line.split('\t')
		unitID = varList[1]
		curSelectivity = varList[8]
		if unitID in collegeDataLookup and collegeDataLookup[unitID].selectivity == -3:
			collegeDataLookup[unitID].selectivity = curSelectivity
	barronsFile2.close()

def printCollegeList():	
	global collegeList
	global missedCollegeList
	global missedSelectivityList
	for i in collegeList:
		if collegeDataLookup[i].selectivity == -3:
			missedSelectivityList.append(i)
	print "Total number of unique IDs that are found after using all means: " + str((len(collegeList)))
	print "Total number of entries in missed colleges list: " + str((len(missedCollegeList)))
	print "Total number of found schools with no selectivity data: " + str((len(missedSelectivityList)))
	collegeListStr = str(collegeList)
	collegeListStr= str.replace(collegeListStr, '[', '')
	collegeListStr= str.replace(collegeListStr, ']', '')
	collegeListStr= str.replace(collegeListStr, '\'', '')
	missedCollegeListStr = str(missedCollegeList)
	missedCollegeListStr= str.replace(missedCollegeListStr, '[', '')
	missedCollegeListStr= str.replace(missedCollegeListStr, ']', '')
	missedCollegeListStr= str.replace(missedCollegeListStr, '\'', '')
	missedSelectivityListStr = str(missedSelectivityList)
	missedSelectivityListStr= str.replace(missedSelectivityListStr, '[', '')
	missedSelectivityListStr= str.replace(missedSelectivityListStr, ']', '')
	missedSelectivityListStr= str.replace(missedSelectivityListStr, '\'', '')
	outFile = open('C:/Users/Katharina/Documents/UMICH/Lifecycle choice/Data/ycoc/uniqueColleges.txt', 'w')
	outFile.write(collegeListStr)
	outFile.close()
	outFile2 = open('C:/Users/Katharina/Documents/UMICH/Lifecycle choice/Data/ycoc/missedColleges.txt', 'w')
	outFile2.write(missedCollegeListStr)
	outFile2.close()
	outFile3 = open('C:/Users/Katharina/Documents/UMICH/Lifecycle choice/Data/ycoc/missingSelectivity.txt', 'w')
	outFile3.write(missedSelectivityListStr)
	outFile3.close()

def deleteNonCollege(): #note that colleges are still in the lookup table, just not in the college lists
	global collegeList
	global collegeDataLookup
	collegeListCopy = list(collegeList)
	for i in range(len(collegeList)):
		if collegeDataLookup[collegeList[i]].bachFlag != "1":
			collegeListCopy.remove(collegeList[i])
			#print("im here")
	collegeList = collegeListCopy
	print "Total number of unique 4-year IDs that are found after using all means: " + str(len(collegeList))
	#print(collegeList)
	collegeListCopy = list(collegeList)
	for i in range(len(collegeList)):
		if collegeDataLookup[collegeList[i]].selectivity == -3:
			collegeListCopy.remove(collegeList[i])
			#print("im here 2")
	collegeList = collegeListCopy
	print "Total number of unique 4-year IDs with selctivity that are found after using all means: " + str(len(collegeList))

def checkMissings():
	#all those that are only missing the school they attended are attending invalid schools (schools not in IPEDS list)
	#missingList = []
	missingList = ['193308','190594','190655','191649','214768','162706','190664','190594','190770','3022','115001','113856','117788','121619','110680','110510','121901','119137','110422','151351','144892','152248','112190','210401','110635','113634','113634','123341','110662','112190','122791','122791','122791','122755','115047','122728','110644','235097','212054','365268','184791','170532','198507','232186','199139','227182','129020','139959','153162','178396','381413','126614','174783','200217','121901','162706','3006','216825','196088','196088','191074','195474','139959','161864','193900','213349','150066','243780','142522','764','204635','3024','169521','178022','178420','178396','247296','163259','133508','132903','199139','199218','198154','133951','3044','3043','234155','163259','221768','421045','176318','101480','157951','206941','179946','207209','222992','159939','228723','4000','128106','128106','420574','127741','127741','381787','115409','234687','237011','118976','118718','196246','202541','127653','145813','155399','174792','174792','207263','200253','201432','152530','206011','206011','141334','441','157951','157951','175573','176372','198987','126614','226204','227401','227401','229179','199193','170976','105330','200280','210429','200059','420556','209746','209533','209825','102553','4000','195960','105330','217907']
	missingList = list(set(missingList))
	for i in missingList:
		if i in collegeDataLookup:
			if i in collegeList:
				print "1" + "\t"+ str(i) + "\t" + str(collegeDataLookup[i])
			else:
				print "0" + "\t"+ str(i) + "\t" + str(collegeDataLookup[i])
		else:
			print "Not found"

if __name__ == '__main__':
	main()


