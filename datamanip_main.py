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

	#check into missing entries
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
	vectorList = open('C:/Users/Katharina/Documents/UMICH/Lifecycle choice/Data/ycoc/compiledcollegelist-new.txt', 'r')
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
	#print "Total number of unique IDs: " + str(len(missedCollegeList))
	#outFiletest = open('C:/Users/Katharina/Documents/UMICH/Lifecycle choice/Data/ycoc/beforededupe.txt', 'w')
	#outFiletest.write(str(missedCollegeList))
	#outFiletest.close()
	missedCollegeList = list(set(missedCollegeList)) #de-dupe
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
	collegeList = collegeListCopy
	print "Total number of unique 4-year IDs that are found after using all means: " + str((len(collegeList)))
	collegeListCopy = list(collegeList)
	for i in range(len(collegeList)):
		if collegeDataLookup[collegeList[i]].bachFlag == -3:
			collegeListCopy.remove(collegeList[i])
	collegeList = collegeListCopy
	print "Total number of unique 4-year IDs with seelctivity that are found after using all means: " + str((len(collegeList)))

def checkMissings():
	#all those that are only missing the school they attended are attending invalid schools (schools not in IPEDS list)
	missingAttendedList = ['191649','214768','190664','194091','190770','110680','123341','110644','123013','186380','170240','170532','232186','199139','139959','178396','200217','217059','216825','196088','191074','193900','243780','169521','176071','178420','221759','187985','234155','163259','421045','176318','157951','179946','222992','228723','127741','127741','115409','127653','155399','200253','157951','175573','126614','227401','229179','180179','200059','209746']
	missingAttendedList = list(set(missingAttendedList))
	missingAllList = ['129242','190594','199306','193016','240444','190558','213826','190558','186201','163286','168005','213543','186283','190594','196097','129215','129242','112385','110671','113856','110671','243744','151351','145725','145637','148627','149727','145813','210401','110635','122931','122597','110714','123554','110422','104179','117946','123572','130943','152080','196413','152390','171100','169798','171571','170806','172644','171571','228778','227216','228875','139755','249247','142522','134130','228778','229027','130590','228778','174491','174066','105154','121901','163851','162706','236948','196088','203517','215105','240329','151102','151102','151111','150136','189097','136455','203517','204909','198516','172699','171100','169248','169248','171100','171100','217864','169248','6000','179566','207403','137351','199193','199218','151351','201104','157085','222053','207388','207500','206941','207865','206941','223232','228459','159939','133951','159939','175980','228778','224350','228705','126614','126818','381787','100706','168148','127741','162654','3031','237011','236948','118976','118976','118976','118976','113193','152080','174792','173300','174792','179566','130934','141264','139755','139940','223816','157216','160658','159009','199087','218061','226204','227225','228778','105330','105330','209533','161253','141486','207388','195960','206941','105330','217907']
	missingAllList = list(set(missingAllList))
	print("Missing Attended: \n")
	for i in missingAttendedList:
		if i in collegeDataLookup:
			if i in collegeList:
				print "1" + "\t"+ str(i) + "\t" + str(collegeDataLookup[i])
			else:
				print "0" + "\t"+ str(i) + "\t" + str(collegeDataLookup[i])
		else:
			print "Not found"
	print("\n Missing All: \n")
	for i in missingAllList:
		if i in collegeDataLookup:
			if i in collegeList:
				print "1" + "\t"+ str(i) + "\t" + str(collegeDataLookup[i])
			else:
				print "0" + "\t"+ str(i) + "\t" + str(collegeDataLookup[i])
		else:
			print "Not found"

if __name__ == '__main__':
	main()



