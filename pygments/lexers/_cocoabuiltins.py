import os
import re

FRAMEWORKS_PATH = '/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS7.0.sdk/System/Library/Frameworks/'
frameworks = os.listdir(FRAMEWORKS_PATH)

all_interfaces = set()
all_protocols  = set()
all_primitives = set()
for framework in frameworks:
	frameworkHeadersDir = FRAMEWORKS_PATH + framework + '/Headers/'
	if (not os.path.exists(frameworkHeadersDir)): continue

	headerFilenames = os.listdir(frameworkHeadersDir)

	for f in headerFilenames:
		if (not f.endswith('.h')): continue

		headerFilePath = frameworkHeadersDir + f
		content = open(headerFilePath).read()
		res = re.findall('(?<=@interface )\w+', content)
		for r in res:
			all_interfaces.add(r)
		
		res = re.findall('(?<=@protocol )\w+', content)
		for r in res:
			all_protocols.add(r)

		res = re.findall('(?<=typedef enum )\w+', content)
		for r in res:
			all_primitives.add(r)

		res = re.findall('(?<=typedef struct )\w+', content)
		for r in res:
 			all_primitives.add(r)

		res = re.findall('(?<=typedef const struct )\w+', content)
		for r in res:
 			all_primitives.add(r)


print "ALL interfaces: \n"
print all_interfaces

print "\nALL protocols: \n"
print all_protocols

print "\nALL primitives2: \n"
print all_primitives