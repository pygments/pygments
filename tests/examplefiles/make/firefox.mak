#
# ***** BEGIN LICENSE BLOCK *****
# Version: MPL 1.1/GPL 2.0/LGPL 2.1
#
# The contents of this file are subject to the Mozilla Public License Version
# 1.1 (the "License"); you may not use this file except in compliance with
# the License. You may obtain a copy of the License at
# http://www.mozilla.org/MPL/
#
# Software distributed under the License is distributed on an "AS IS" basis,
# WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
# for the specific language governing rights and limitations under the
# License.
#
# The Original Code is mozilla.org code.
#
# The Initial Developer of the Original Code is
# Netscape Communications Corporation.
# Portions created by the Initial Developer are Copyright (C) 1998
# the Initial Developer. All Rights Reserved.
#
# Contributor(s):
#
# Alternatively, the contents of this file may be used under the terms of
# either the GNU General Public License Version 2 or later (the "GPL"), or
# the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
# in which case the provisions of the GPL or the LGPL are applicable instead
# of those above. If you wish to allow use of your version of this file only
# under the terms of either the GPL or the LGPL, and not to allow others to
# use your version of this file under the terms of the MPL, indicate your
# decision by deleting the provisions above and replace them with the notice
# and other provisions required by the GPL or the LGPL. If you do not delete
# the provisions above, a recipient may use your version of this file under
# the terms of any one of the MPL, the GPL or the LGPL.
#
# ***** END LICENSE BLOCK *****

DEPTH		= .
topsrcdir	= @top_srcdir@
srcdir		= @srcdir@
VPATH		= @srcdir@

include $(DEPTH)/config/autoconf.mk

include $(topsrcdir)/build/unix/modules.mk

ifeq ($(BUILD_MODULES),all)
#
# And now for something completely different...
# Divide the default build into tiers. 
# Tiers must be defined on module boundaries 
#
SUPPRESS_DEFAULT_RULES = 1

default alldep all:: $(SUBMAKEFILES)
	$(RM) -rf $(DIST)/sdk
	$(RM) -rf $(DIST)/include
	$(MAKE) -C config export
	$(MAKE) nspr
	$(MAKE) ldap
	$(MAKE) tier_0
	$(MAKE) tier_1
	$(MAKE) tier_2
	$(MAKE) tier_9
	$(MAKE) tier_50
	$(MAKE) tier_99

# Make sure that the existing rulesets work
DIRS = \
	$(tier_0_dirs) \
	$(tier_1_dirs) \
	$(tier_2_dirs) \
	$(tier_9_dirs) \
	$(tier_50_dirs) \
	$(NULL)

ifdef GC_LEAK_DETECTOR
DIRS += gc/boehm
endif

DIRS	+= $(tier_99_dirs)

#
# tier 0 - base build config dirs
# 
tier_0_dirs = \
	config \
	build \
	$(NULL)

#
# tier 1 -  3rd party individual libraries
#
tier_1_dirs	+= dbm

ifndef MOZ_NATIVE_JPEG
tier_1_dirs	+= jpeg
endif

ifndef MOZ_NATIVE_ZLIB
tier_1_dirs	+= modules/zlib
endif

# Installer needs standalone libjar, hence standalone zlib
ifdef MOZ_INSTALLER
tier_1_dirs	+= modules/zlib/standalone
endif

ifdef MOZ_UPDATER
tier_1_dirs += modules/libbz2
tier_1_dirs += modules/libmar
endif

ifdef MOZ_SVG_RENDERER_LIBART
tier_1_dirs	+= other-licenses/libart_lgpl
endif

#
# tier 2 - base libraries
# 
tier_2_dirs	= \
		js \
		xpcom \
		$(NULL)

ifndef MOZ_NO_XPCOM_OBSOLETE
tier_2_dirs += modules/libreg xpcom/obsolete
endif

ifdef NS_TRACE_MALLOC
tier_2_dirs	+= tools/trace-malloc/lib
endif

#
# tier 9 - core components (necko,gecko)
#

tier_9_dirs += \
		js/src/xpconnect \
		intl \
		db \
		$(NULL)

ifdef MOZ_STORAGE
tier_9_dirs += storage
endif

ifdef MOZ_ENABLE_XLIB
tier_9_dirs	+= gfx/src/xlibrgb widget/src/xlibxtbin
endif

ifdef MOZ_ENABLE_GTK
tier_9_dirs	+= widget/src/gtksuperwin widget/src/gtkxtbin
endif

ifdef MOZ_ENABLE_GTK2
tier_9_dirs     += widget/src/gtkxtbin
endif

ifdef MOZ_IPCD
tier_9_dirs += ipc/ipcd
endif

ifdef MOZ_JSDEBUGGER
tier_9_dirs += js/jsd
endif

tier_9_dirs	+= \
		modules/libutil \
		netwerk \
		modules/libjar \
		uriloader \
		modules/libpref \
		modules/libimg \
		caps \
		rdf \
		parser/expat \
		parser/xml \
		parser/htmlparser \
		gfx \
		modules/libpr0n \
		sun-java \
		modules/plugin \
		dom \
		view \
		widget \
		content \
		layout \
		xpfe/components/shistory \
		docshell \
		webshell \
		embedding \
		editor \
		xpfe/appshell \
		$(NULL)

ifdef MOZ_OJI
tier_9_dirs	+= \
		js/src/liveconnect \
		modules/oji \
		$(NULL)
endif

ifdef ACCESSIBILITY
tier_9_dirs    += accessible
endif

# 
# tier 50 - xpfe & toolkit
#

ifdef MOZ_XUL
ifdef MOZ_XUL_APP
tier_50_dirs += chrome
else
tier_50_dirs += rdf/chrome
endif
else
tier_50_dirs += embedding/minimo/chromelite
endif

tier_50_dirs += profile

# This must preceed xpfe
ifdef MOZ_JPROF
tier_50_dirs        += tools/jprof
endif

ifneq (,$(filter mac cocoa,$(MOZ_WIDGET_TOOLKIT)))
tier_50_dirs	+= xpfe/bootstrap/appleevents
endif

tier_50_dirs	+= \
	xpfe \
	toolkit/components \
	$(NULL)

ifndef MOZ_XUL_APP
tier_50_dirs += themes
endif

ifdef MOZ_ENABLE_XREMOTE
tier_50_dirs += widget/src/xremoteclient
endif

ifdef MOZ_XUL_APP
tier_50_dirs	+= toolkit
endif

ifdef MOZ_PHOENIX
#XXXBlake this shell path is a temp hack; toolkit shouldn't depend on browser
tier_50_dirs    += browser/components/shell/public
endif

ifdef MOZ_XPINSTALL
tier_50_dirs     +=  xpinstall
endif

# JavaXPCOM JNI code is compiled into libXUL
ifdef MOZ_JAVAXPCOM
tier_50_dirs	+= extensions/java/xpcom/src
endif

ifdef MOZ_ENABLE_LIBXUL
tier_50_dirs += \
		toolkit/library \
		xpcom/stub \
		$(NULL)
endif

ifdef NS_TRACE_MALLOC
tier_50_dirs += tools/trace-malloc
endif

ifdef MOZ_PSM
tier_50_dirs	+= security/manager
else
tier_50_dirs	+= security/manager/boot/public security/manager/ssl/public
endif

ifdef MOZ_LDAP_XPCOM
tier_50_dirs	+= directory/xpcom
endif

ifndef MINIMO
ifdef MOZ_XUL_APP
ifdef MOZ_ENABLE_GTK2
tier_50_dirs    += toolkit/components/gnome
endif
endif
endif

ifdef MOZ_LEAKY
tier_50_dirs        += tools/leaky
endif

ifdef MOZ_MAPINFO
tier_50_dirs	+= tools/codesighs
endif

#
# tier 99 - application features
#

ifdef MOZ_MAIL_NEWS
tier_99_dirs	+= mailnews
endif

ifdef MOZ_CALENDAR
tier_99_dirs	+= calendar
endif

ifdef MOZ_EXTENSIONS
tier_99_dirs	+= extensions
endif

ifdef MOZ_JAVAXPCOM
tier_99_dirs	+= extensions/java
endif

# axcontrol
ifeq ($(OS_ARCH),WINNT)
ifndef MOZ_NO_ACTIVEX_SUPPORT
tier_99_dirs += \
		embedding/browser/activex/src/control \
		embedding/browser/activex/src/control_kicker \
		$(NULL)
endif
endif

# Java Embedding Plugin
ifneq (,$(filter mac cocoa,$(MOZ_WIDGET_TOOLKIT)))
tier_99_dirs += plugin/oji/JEP
endif

ifneq (,$(filter browser suite,$(MOZ_BUILD_APP)))
tier_99_dirs += xpfe/components/search
endif

ifdef MOZ_BRANDING_DIRECTORY
tier_99_dirs += $(MOZ_BRANDING_DIRECTORY)
endif

ifdef MOZ_PHOENIX
tier_99_dirs	+= browser xpfe/bootstrap/init.d
endif

ifdef MOZ_XULRUNNER
tier_99_dirs	+= xulrunner
endif

ifdef MOZ_COMPOSER
tier_99_dirs	+= editor/ui
endif

ifdef MOZ_THUNDERBIRD
tier_99_dirs	+= mail xpfe/bootstrap/init.d
endif

ifdef MOZ_STANDALONE_COMPOSER
tier_99_dirs	+= composer
endif

ifdef MOZ_SUNBIRD
tier_99_dirs	+= calendar/sunbird
endif

ifdef MOZ_SUITE
tier_99_dirs	+= suite
endif

ifdef MINIMO
tier_99_dirs	+= minimo
endif

ifdef MOZ_XUL_APP
ifdef MOZ_INSTALLER
tier_99_dirs     +=  toolkit/mozapps/installer
endif
else
ifneq (,$(MOZ_XPFE_COMPONENTS)$(MOZ_XUL))
ifndef MINIMO
tier_99_dirs	+= xpfe/bootstrap
endif
endif
endif

ifneq (,$(MOZ_ENABLE_GTK)$(MOZ_ENABLE_GTK2))
tier_99_dirs	+= embedding/browser/gtk
endif

# viewer
ifneq (,$(ENABLE_TESTS))
ifndef MOZ_ENABLE_LIBXUL
tier_99_dirs += webshell/tests
endif
endif

# winembed, mfcembed
ifeq ($(OS_ARCH),WINNT)
ifneq (,$(ENABLE_TESTS)$(MOZILLA_OFFICIAL))
tier_99_dirs += embedding/tests
endif
endif

# os2embed
ifeq ($(OS_ARCH),OS2)
ifneq (,$(ENABLE_TESTS)$(MOZILLA_OFFICIAL))
tier_99_dirs += embedding/tests
endif
endif

ifeq ($(MOZ_BUILD_APP),macbrowser)
tier_99_dirs += \
	embedding/config \
	camino \
	$(NULL)
endif

# test harnesses 
ifdef ENABLE_TESTS
tier_99_dirs += tools/test-harness
endif

else

# Standalone build

DIRS		= $(BUILD_MODULE_DIRS)

# Hack to generate xpidl Makefile
ifneq ($(BUILD_MODULES),all)
ifneq (,$(findstring xpcom, $(BUILD_MODULE_DIRS)))
DIRS		:= xpcom/typelib $(DIRS)
SUBMAKEFILES	:= xpcom/typelib/Makefile
endif
endif

default:: $(SUBMAKEFILES)
	$(MAKE) export
	$(MAKE) libs

endif # BUILD_MODULES == all

STATIC_MAKEFILES := nsprpub directory/c-sdk security/nss

GARBAGE_DIRS += dist
DIST_GARBAGE = config.cache config.log config.status config-defs.h \
   dependencies.beos config/autoconf.mk config/myrules.mk config/myconfig.mk \
   unallmakefiles mozilla-config.h \
   $(topsrcdir)/.mozconfig.mk $(topsrcdir)/.mozconfig.out 

# Build pseudo-external modules first when export is explicitly called
export::
	$(RM) -rf $(DIST)/sdk
	$(MAKE) -C config export
	$(MAKE) nspr
	$(MAKE) ldap
ifneq ($(BUILD_MODULES),all)
ifneq (,$(findstring xpcom, $(BUILD_MODULE_DIRS)))
	$(MAKE) -C xpcom/typelib
	$(MAKE) export-idl
endif
endif

install::
ifndef MOZ_NATIVE_NSPR
	$(MAKE) -C nsprpub real_install DESTDIR=$(DESTDIR) libdir=$(mozappdir) includedir=$(includedir)/nspr
	$(RM) -f $(addprefix $(DESTDIR)$(mozappdir)/$(LIB_PREFIX), $(addsuffix .$(LIB_SUFFIX), nspr4 plds4 plc4))
	$(RM) -f $(addprefix $(DESTDIR)$(bindir)/,nspr-config compile-et.pl prerr.properties)
endif
ifdef MOZ_LDAP_XPCOM
	$(MAKE) -C directory/c-sdk real_install DESTDIR=$(DESTDIR) libdir=$(mozappdir) includedir=$(includedir)/ldap
endif

include $(topsrcdir)/config/rules.mk

# Clean up after pseudo-external modules
clean clobber realclean clobber_all distclean::
ifndef MOZ_NATIVE_NSPR
	$(MAKE) -C nsprpub $@
endif
ifdef MOZ_LDAP_XPCOM
	$(MAKE) -C directory/c-sdk $@
endif

# Map mozilla targets to standard automake target
ifdef MOZ_ENABLE_LIBXUL
tier_50: $(addsuffix /Makefile, $(filter-out $(STATIC_MAKEFILES), $($@_dirs)))
	@echo "tier_50: $(tier_50_dirs)"
	@$(EXIT_ON_ERROR) \
	for d in $(tier_50_dirs); do \
	    $(UPDATE_TITLE) \
	    if test ! -f $$d/Makefile; then \
	      $(PERL) $(AUTOCONF_TOOLS)/make-makefile -t $(topsrcdir) -d $(DEPTH) $(CYGWIN_TOPSRCDIR) $$d/Makefile; \
	    fi; \
	    $(MAKE) -C $$d export; \
	done ; \
	for d in $(tier_50_dirs); do \
	    $(UPDATE_TITLE) \
	    $(MAKE) -C $$d libs; \
	done
	@echo "Building tools from tier 2/9/50"
	@$(EXIT_ON_ERROR) \
	for d in $(tier_2_dirs) $(tier_9_dirs) $(tier_50_dirs); do \
	    $(UPDATE_TITLE) \
	    $(MAKE) -C $$d tools; \
	done;
endif

tier_%:
	@echo "$@: $($@_dirs)"
	@$(EXIT_ON_ERROR) \
	  for d in $($@_dirs); do \
	    $(UPDATE_TITLE) \
	    if test ! -f $$d/Makefile; then \
	      $(PERL) $(AUTOCONF_TOOLS)/make-makefile -t $(topsrcdir) -d $(DEPTH) $(CYGWIN_TOPSRCDIR) $$d/Makefile; \
	    fi; \
	    $(MAKE) -C $$d export; \
	  done ; \
	  for d in $($@_dirs); do $(UPDATE_TITLE) \
	    $(MAKE) -C $$d libs; \
	  done

#
# Individual modules
#
boehm:
ifdef GC_LEAK_DETECTOR
	$(MAKE) -C gc/boehm
endif

nspr: boehm
ifndef MOZ_NATIVE_NSPR
	$(MAKE) -C nsprpub
endif

ldap:
ifdef MOZ_LDAP_XPCOM
	$(MAKE) -C directory/c-sdk
endif

distclean::
	cat unallmakefiles | $(XARGS) rm -f
	rm -f unallmakefiles $(DIST_GARBAGE)

ifeq ($(OS_ARCH),WINNT)
rebase:
ifdef MOZILLA_OFFICIAL
	echo rebasing $(DIST)
	/bin/find $(DIST) -name "*.dll" > rebase.lst
	rebase -b 60000000 -R . -G rebase.lst
	rm rebase.lst
endif

splitsymbols:
ifdef MOZILLA_OFFICIAL
ifdef MOZ_DEBUG_SYMBOLS
	echo finding pdb files
	mkdir -p $(DIST)/$(BUILDID)
	-cp `/bin/find . -path "./dist" -prune -o -name "*.dll" | sed "s/\.dll$$/\.pdb/" | xargs` $(DIST)/$(BUILDID)
	-cp `/bin/find . -path "./dist" -prune -o -name "*.exe" | sed "s/\.exe$$/\.pdb/" | xargs` $(DIST)/$(BUILDID)
	-cp `/bin/find . -path "./dist" -prune -o -name "*.EXE" | sed "s/\.EXE$$/\.pdb/" | xargs` $(DIST)/$(BUILDID)
endif # MOZ_DEBUG_SYMBOLS
ifdef MOZ_PROFILE
	echo splitting symbols out of binaries
	/bin/find $(DIST) -name "*.dll" -exec splitsym {} \;
	/bin/find $(DIST) -name "*.exe" -exec splitsym {} \;
	/bin/find $(DIST) -name "*.EXE" -exec splitsym {} \;
	mkdir -p $(DIST)/$(BUILDID)
	/bin/find $(DIST) -name "*.dbg" -exec mv {} $(DIST)/$(BUILDID) \;
endif # MOZ_PROFILE
endif # MOZILLA_OFFICIAL

signnss:
ifdef MOZILLA_OFFICIAL
	echo signing NSS libs
	cd $(DIST)/bin; ./shlibsign.exe -v -i softokn3.dll
	cd $(DIST)/bin; ./shlibsign.exe -v -i freebl3.dll
endif # MOZILLA_OFFICIAL

BUILDID = $(shell cat $(DEPTH)/config/build_number)
deliver: splitsymbols rebase signnss

endif # WINNT

