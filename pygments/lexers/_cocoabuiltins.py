import os
import re

FRAMEWORKS_PATH = '/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS7.0.sdk/System/Library/Frameworks/'
frameworks = os.listdir(FRAMEWORKS_PATH)

all_types = set()
for framework in frameworks:
	frameworkHeadersDir = FRAMEWORKS_PATH + framework + '/Headers/'
	if (not os.path.exists(frameworkHeadersDir)): continue

	headerFilenames = os.listdir(frameworkHeadersDir)

	types = set()

	for f in headerFilenames:
		if (not f.endswith('.h')): continue

		headerFilePath = frameworkHeadersDir + f
		content = open(headerFilePath).read()
		res = re.search('(?<=@interface )\w+', content)
		if (res):
			types.add(res.group(0))
			all_types.add(res.group(0))
		

	print framework + "\n"
	# print types


print "ALL: \n"
print all_types