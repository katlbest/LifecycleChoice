import sys
#from collections import Counter

collegeTypeLookup = {} #school type by IPEDS
collegeControlLookip = {} #public/private by IPEDS
collegeHash = {} #hash table indicating which colleges to pull data for

vectorList = open('C:/Users/Katharina/Documents/UMICH/Lifecycle choice/Data/ycoc/compiledcollegelist.csv', 'r')
for line in vectorList.readlines():
	varList = line.split(',')
	PUBID_1997 = varList[1]
	COLLEGEGOER_FLAG = varList[2]
	COLLEGE_SCHOOLID = varList[3]
	COMPILED_APPLY = varList[4]
	COMPILED_ADMIT = varList[5]


#udemo = list(set(splitdemo)) #de-dupe

