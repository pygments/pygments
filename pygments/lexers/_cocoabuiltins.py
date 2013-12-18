import os
import re

FRAMEWORKS_PATH = '/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS7.0.sdk/System/Library/Frameworks/'
frameworks = os.listdir(FRAMEWORKS_PATH)

all_interfaces = set()
all_protocols  = set()
for framework in frameworks:
	frameworkHeadersDir = FRAMEWORKS_PATH + framework + '/Headers/'
	if (not os.path.exists(frameworkHeadersDir)): continue

	headerFilenames = os.listdir(frameworkHeadersDir)

	interfaces = set()

	for f in headerFilenames:
		if (not f.endswith('.h')): continue

		headerFilePath = frameworkHeadersDir + f
		content = open(headerFilePath).read()
		int_res = re.search('(?<=@interface )\w+', content)
		if (int_res):
			interfaces.add(int_res.group(0))
			all_interfaces.add(int_res.group(0))
		
		pro_res = re.search('(?<=@protocol )\w+', content)
		if (pro_res):
			interfaces.add(pro_res.group(0))
			all_protocols.add(pro_res.group(0))

	print framework + "\n"
	# print types


print "ALL interfaces: \n"
print all_interfaces

print "\nALL protocols: \n"
print all_protocols