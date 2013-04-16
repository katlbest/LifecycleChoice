#input file has appliers only, all assumptions made that are required to get full data
#tag: current latest file

#import modules========================================================================================
import sys

#set up global variables===============================================================================
collegeList = [] #list of colleges
missedCollegeList = [] #list of colleges without info
missedSelectivityList = [] #list of schools among 4-year schools for which selectivity is missing
crosswalkLookup = {} #returns an IPEDS ID for a FICE number
collegeDataLookup = {} #returns collegeData object for an IPEDS number
OPEIDcrosswalkLookup = {} #returns an IPEDS ID for an OPEID number
selectivityEstLookup = {} #returns selectivity estimate for IPEDS number
#collegeDataLookup = {} #lookup table storing college info based on IPEDS number
studentLookup = {} #Lookup table for personal information by PUBID_1997

#define classes========================================================================================
class CollegeData: #class storing college data
	def __init__(self, colName, bachFlag, control, selectivity, sat25, sat75, admitperc, carnegie):
		self.colName, self.bachFlag, self.control, self.selectivity, self.sat25, self.sat75, self.admitperc, self.carnegie = colName, bachFlag, control, selectivity, sat25, sat75, admitperc, carnegie

	def __str__(self):
		return str(self.colName) + "\t" + str(self.bachFlag) + "\t" + str(self.control) +  "\t" + str(self.selectivity)

class StudentData: #class storing student data
	#we may want to add major and other info here.
	def __init__(self, pubID, maxAttend, maxAdmit, controlAttend):
		self.pubID, self.maxAttend, self.maxAdmit, self.controlAttend = pubID, maxAttend, maxAdmit, controlAttend

	def __str__(self):
		if self.maxAdmit == 100:
				printAdmit = -3
		else:
			printAdmit = self.maxAdmit
		return str(self.pubID) + "\t" + str(self.maxAttend) + "\t" + str(printAdmit) +  "\t" + str(self.controlAttend)

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

	#pull data needed to fill int missing selectivity (and possibly other data in the future)
	#we only need this for the selectivity regression, so do not run every time
	#populateCollegeData(2004)
	#populateCollegeData(2006)
	#populateCollegeData(2005)
	#populateCollegeData(2003)
	#populateCollegeData(2002)
	#populateCollegeData(2001)
	#populateCollegeData(2011)
	#writeMissingSelect()

	#check whether there is anything different about the schools for which people have missing data
	#we do not need to run this every time
	#checkMissings()

	#set up lookup for estimated selectivities
	setupSelectivityEst()

	#populate accepted and admitted set for each individual (must be correct school type)
	setupIndividualData()

#function definitions==============================================================================================
def collegeListSetup(): #extract list of colleges people have attended
	global missedCollegeList
	vectorList = open('D:/compiledcollegelist-app.txt', 'r')
	lines = vectorList.readlines()
	for line in lines[1:]:
	#for line in vectorList:
		#read in variables
		varList = line.split('\t')
		PUBID_1997 = int(varList[1])
		COLLEGEGOER_FLAG = int(varList[2])
		COLLEGE_SCHOOLID = int(varList[3])
		if COLLEGE_SCHOOLID > 999999:
			COLLEGE_SCHOOLID = COLLEGE_SCHOOLID/100 #adjust for unncessary trailing zeros in some cases
		missedCollegeList.append(str(COLLEGE_SCHOOLID))
		COMPILED_APPLY = varList[5]
		COMPILED_ADMIT = varList[6]
		applyVector = COMPILED_APPLY.split(',')
		admitVector = COMPILED_ADMIT.split(',')
		for i in range(0,len(applyVector)-1):
			applyVector[i] = applyVector[i].replace('"','')
			applyVector[i] = int(applyVector[i])
			if applyVector[i] > 999999:
				applyVector[i] = applyVector[i]/100
			missedCollegeList.append(str(applyVector[i]))
	vectorList.close()
	#clean up college list
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
		#TBD setup read-in of these relevant variables, probabaly as separate function
		curSat25 = -3
		curSat75 = -3
		curAdmitperc = -3
		if myYear < 2005:
			curCarnegie = varList[36]
		elif myYear <2007:
			curCarnegie = varList[49]
		else:
			curCarnegie = -3
		if unitID not in collegeDataLookup: #update current known IPED IDs#
			curSelectivity = -3  #we haven't done this yet
			curColData = CollegeData(colName, curBachFlag, curControl, curSelectivity, curSat25, curSat75, curAdmitperc, curCarnegie)
			collegeDataLookup[unitID] = curColData
		if (opeid not in OPEIDcrosswalkLookup): #update OPEID crosswalk
			OPEIDcrosswalkLookup[opeid]= unitID
	curIPEDS.close()
	missedCollegeListCopy = list(missedCollegeList)
	collegeListCopy = list(collegeList)
	for i in range(len(missedCollegeList)): #try to ID missed colleges by OPEID
		if (missedCollegeList[i]) in collegeDataLookup:
			collegeListCopy.append(missedCollegeList[i])
			missedCollegeListCopy.remove(missedCollegeList[i])
		elif (missedCollegeList[i]) in OPEIDcrosswalkLookup: #try to ID missed colleges by OPEID
			print "code 1: found college by OPEID lookup in year " + str(myYear) + ", " + missedCollegeList[i] #this happens once
			collegeListCopy.append(OPEIDcrosswalkLookup[missedCollegeList[i]])
			missedCollegeListCopy.remove(missedCollegeList[i])
			collegeDataLookup[missedCollegeList[i]] = collegeDataLookup[OPEIDcrosswalkLookup[missedCollegeList[i]]]
			#but the selectivity data probably gets lost
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
				print "code 2: found college by FICE check, " + missedCollegeList[i] #this doesn't happen
				missedCollegeListCopy.remove(missedCollegeList[i])
				collegeDataLookup[missedCollegeList[i]] = collegeDataLookup[crosswalkLookup[missedCollegeList[i]]]
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

def populateCollegeData(myYear):
	global collegeList
	global missedCollegeList
	global OPEIDcrosswalkLookup
	global collegeDataLookup
	curIPEDS = open('C:/Users/Katharina/Documents/UMICH/Lifecycle choice/Data/ycoc/ic' + str(myYear) + '.txt', 'rb')
	linecount = 1
	if myYear == 2011:
		stringList = ['SATMT25', 'SATMT75', 'SATVR25', 'SATVR75', 'ACTCM25', 'ACTCM75', 'ACTEN25', 'ACTEN75', 'APPLCN', 'ADMSSN']
	else:
		stringList = ['satmt25', 'satmt75', 'satvr25', 'satvr75', 'actcm25', 'actcm75', 'acten25', 'acten75', 'applcn', 'admssn']
	indexVector = [0]*len(stringList)
	for line in curIPEDS.readlines():
		#find index for each item we want
		if linecount ==1:
			linecount = linecount+1
			varNameList = line.split('\t')
			for i in range(0,len(varNameList)):
				j = 0
				while j < len(stringList):
					if varNameList[i] == stringList[j]:
						#print "found "+ str(j) + "," + str(myYear)
						indexVector[j] = i
						j = len(stringList)
					else: 
						j = j+1
			#print indexVector
		#get info on each school
		else:
			varList = line.split('\t')
			unitID = varList[0]
			varVector = [0]*len(stringList)
			for i in range(0,len(varVector)):
				if varList[indexVector[i]] == "" or varList[indexVector[i]] == ".":
					varVector[i]= 0
				else:
					varVector[i] = int(varList[indexVector[i]])
			curCollege = collegeDataLookup[unitID]
			if curCollege.sat25 == -3:
				if varVector[0] >0: #have SAT scores
					curCollege.sat25 = varVector[0]+ varVector[2]
				elif varVector[4] >0: #have ACT scores
					curCollege.sat25 = 41.084*(varVector[4]+varVector[6]) +116.45
			if curCollege.sat75 == -3:
				if varVector[1] >0: #have SAT scores
					curCollege.sat75 = varVector[1]+ varVector[3]
				elif varVector[5] >0: #have ACT scores
					curCollege.sat75 = 41.084*(varVector[5]+varVector[7]) +116.45
				#print str(unitID)+ ","  + str(collegeDataLookup[unitID].sat75)
			if curCollege.admitperc == -3:
				if varVector[8]>0 and varVector[9]>0:
					curCollege.admitperc = float(float(varVector[9])/float(varVector[8]))
		curIPEDS.close()

def writeMissingSelect():
#write to output file
	outstr= "SCHOOL_ID" + "\t" + "SAT 25 Perc."+ "\t" +"SAT 75 Perc."+ "\t" + "Admit percentage" + "\t" +"Selectivity"+ "\t" +"Carnegie class" + "\t"+ "Needs Data Flag" + "\n"
	open("C:/Users/Katharina/Documents/UMICH/Lifecycle choice/Data/ycoc/findSelect.txt","wb").write(outstr)
	for i in collegeList:
		if i in collegeDataLookup:
			curCollege = collegeDataLookup[i]
			if str(i) in missedSelectivityList:
				needsSelect = 1
			else:
				needsSelect = 0
			outstr = str(i)+ "\t" + str(curCollege.sat25)+ "\t" +str(curCollege.sat75)+ "\t" +str(curCollege.admitperc)+ "\t" +str(curCollege.selectivity) + "\t" + str(curCollege.carnegie) + "\t" + str(needsSelect) + "\n"
			open("C:/Users/Katharina/Documents/UMICH/Lifecycle choice/Data/ycoc/findSelect.txt","a").write(outstr)
		else:
			print "no"+str(i)

def printCollegeList():	
	global collegeList
	global missedCollegeList
	global missedSelectivityList
	global collegeDataLookup
	for i in collegeList:
		if collegeDataLookup[i].selectivity == -3: #update selectivity list so that only the 4-year schools are counted
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
	print "Total number of unique 4-year IDs that are found after using all means: " + str(len(collegeList))
	#print(collegeList)
	collegeListCopy = list(collegeList)
	#for i in range(len(collegeList)): #this code removes schools with no selectivity, but we don't wnat to do that
	#	if collegeDataLookup[collegeList[i]].selectivity == -3:
#			collegeListCopy.remove(collegeList[i])
	#collegeList = collegeListCopy
	#print "Total number of unique 4-year IDs with selectivity that are found after using all means: " + str(len(collegeList))

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

def setupSelectivityEst():
	global selectivityEstLookup
	myFile = open('C:/Users/Katharina/Documents/UMICH/Lifecycle choice/Data/ycoc/selectest.txt', 'rb')
	for line in myFile.readlines():
		varList = line.split('\t')
		unitID = int(varList[0])
		selectEst = int(varList[1])
		selectivityEstLookup[unitID]= selectEst
	myFile.close()

def setupIndividualData():
	attenderCount = 0
	applierCount = 0
	applierWithAdmitCount = 0
	missingAllAdmitCount = 0
	missingAdmitCount = 0
	noSelectCounter = 0
	noSelectCounter2 = 0
	global studentLookup
	global collegeList
	global collegeDataLookup
	global selectivityEstLookup
	vectorList = open('D:/compiledcollegelist.txt', 'r')
	lines = vectorList.readlines()
	outstr= "PUBID_1997" + "\t" + "Best Attended"+ "\t" +"Best Admitted"+ "\t" + "Attended control" + "\t" + "New Collegegoer Flag" + "\t" + "Old Collegegoer Flag" + "\n"
	open("C:/Users/Katharina/Documents/UMICH/Lifecycle choice/Data/ycoc/studentdata2.txt","wb").write(outstr)
	for line in lines[1:]:
	#once we do the cleanup everyone in this file should be an applier, so we wont have to do checks for length zero
	#for line in vectorList:
		#read in variables and set up relevant tables
		varList = line.split('\t')
		admitLookup = {} #stores admission by school ID for this individual
		PUBID_1997 = int(varList[1])
		COLLEGEGOER_FLAG = int(varList[2])
		COLLEGE_SCHOOLID = int(varList[3])
		if COLLEGE_SCHOOLID > 999999:
			COLLEGE_SCHOOLID = COLLEGE_SCHOOLID/100 #adjust for unncessary trailing zeros in some cases
		COMPILED_APPLY = varList[5]
		COMPILED_ADMIT = varList[6]
		applyVector = COMPILED_APPLY.split(',')
		admitVector = COMPILED_ADMIT.split(',')
		attenderFlag = -3
		applierFlag = -3
		applierWithAdmitFlag = -3
		for i in range(0,len(applyVector)-1):
			applyVector[i] = applyVector[i].replace('"','')
			applyVector[i] = int(applyVector[i])
			if applyVector[i] > 999999:
				applyVector[i] = applyVector[i]/100
		for i in range(0,len(admitVector)-1):
			admitVector[i] = admitVector[i].replace('"','')
			admitVector[i] = int(admitVector[i])
		for i in range(0,len(admitVector)-1): #this now loops through both apply and admit and creates a lookup
			if str(applyVector[i]) in collegeList: #this is a school that is relevant
				if applyVector[i] not in admitLookup:
					admitLookup[applyVector[i]] = -3
				if admitVector[i] > admitLookup[applyVector[i]]:
					admitLookup[applyVector[i]] = admitVector[i]
				#if you attended you were definitely admitted
				if applyVector[i] == COLLEGE_SCHOOLID:
					admitLookup[applyVector[i]] = 1
		#even if you did not apply, you were admitted where you attended
		if ((COLLEGE_SCHOOLID > 0) & (str(COLLEGE_SCHOOLID) in collegeList) & (COLLEGE_SCHOOLID not in admitLookup)):
			admitLookup[COLLEGE_SCHOOLID] = 1

		#check for missing admissions information
		#count number of peopl emissing all information
		minim = 0 #holder for lower admission info level
		if len(admitLookup) > 0: #you applied to some schools (or attended a school)
			applierFlag = 1 #set applier flag accordingly
			applierWithAdmitFlag = 1
			applierCount = applierCount+1
			minim = -3 #set minimum holder to -3
			for i in admitLookup:
				minim = max(admitLookup[i], minim) #if current values is a positive number, use that
			if minim <0: #if we still haven't found a positive number, you are missing all admit info
				missingAllAdmitCount = missingAllAdmitCount +1
				applierWithAdmitFlag = 0 #this means you applied places, but we got no info out of it; you clearly did not attend bc otherwise you would have at least one 1 entry
			else:
				applierWithAdmitCount = applierWithAdmitCount +1
		else:
			applierFlag = 0
			applierWithAdmitFlag = 0
		#count number of people missing some admissions information
		if len(admitLookup) > 0:
			missingAdmitCheck = 0
			for i in admitLookup:
				missingAdmitCheck = min(admitLookup[i], missingAdmitCheck)
			if missingAdmitCheck < 0:
				#print str(PUBID_1997) + "," + str(admitLookup) + "," + str(admitVector) + "," + str(applyVector)
				missingAdmitCount = missingAdmitCount + 1

		#create output re: attended school and status
		if str(COLLEGE_SCHOOLID) in collegeList:
			attenderCount = attenderCount +1
			attenderFlag = 1
			curMaxAttend = collegeDataLookup[str(COLLEGE_SCHOOLID)].selectivity
			if curMaxAttend <0:
				if COLLEGE_SCHOOLID in selectivityEstLookup: #use estimate if you dont have a selectivity for this person
					curMaxAttend = selectivityEstLookup[COLLEGE_SCHOOLID]
			curControlAttend = collegeDataLookup[str(COLLEGE_SCHOOLID)].control
		else:
			curMaxAttend = -3
			curControlAttend = -3
			attenderFlag = 0

		#create output re: applied schools and status
		curMaxAdmit = 100
		for i in admitLookup:
			if int(collegeDataLookup[str(i)].selectivity) > 0: #since 7 is the highest possible number, you get "specialty" value only if that is the only place you applied
				curMaxAdmit = min(int(collegeDataLookup[str(i)].selectivity), curMaxAdmit)
		if curMaxAdmit == 100:
			if COLLEGE_SCHOOLID in selectivityEstLookup:
				for i in admitLookup:
					curMaxAdmit = min(selectivityEstLookup[i], curMaxAdmit)
		if curMaxAdmit == 100:
			curMaxAdmit = curMaxAttend
		#populate curStudentData
		curStudentData = StudentData(PUBID_1997, curMaxAttend, curMaxAdmit, curControlAttend)
		studentLookup[PUBID_1997] = curStudentData
		#print if you are an applicant
		#if applierFlag ==1: #this implies you applied to a school; we only care about those who are admitted to at least one college
		if applierWithAdmitFlag ==1:
			#if curMaxAdmit == -3 and curMaxAttend == -3:
			if attenderFlag == 1  and curMaxAttend == -3:
				noSelectCounter = noSelectCounter + 1
			elif attenderFlag == 0  and curMaxAdmit== -3:
				noSelectCounter2 = noSelectCounter2 + 1
			else:
				outstr= str(curStudentData)  +  "\t" + str(attenderFlag) + "\t" + str(COLLEGEGOER_FLAG)+ "\n"
				open("C:/Users/Katharina/Documents/UMICH/Lifecycle choice/Data/ycoc/studentdata2.txt","a").write(outstr)
	vectorList.close()
	print "Total number of applicants to 4-year IPEDS universities: " + str(applierCount)
	print "Total number of applicants for whom all admission data is missing: " + str(missingAllAdmitCount)
	print "Total number of applicants for whom some admission data is missing: " + str(missingAdmitCount)
	print "Total number of applicants who have some admission information: " + str(applierWithAdmitCount)
	print "Total number of attenders at 4-year IPEDS universities: " + str(attenderCount)
	print "Total number of attenders without a max selectivity: " + str(noSelectCounter)
	print "Total number of non-attendee applicants without a max selectivity: " + str(noSelectCounter2)
if __name__ == '__main__':
	main()


