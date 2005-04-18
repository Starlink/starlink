
# To install a new workstation in the WDT, you should add the name of
# its WDT file to the variable WDTS below. Check that you have the
# right range of workstation handlers being made in the drivers
# makefile.

WDTS = wdt0003 wdt0010 wdt0050 wdt0012 wdt0052 wdt2700 wdt2701 wdt2702	\
wdt2703 wdt2720 wdt2721 wdt2722 wdt2723 wdt0201 wdt0203 wdt0800		\
wdt0801 wdt0820 wdt0821 wdt0825 wdt0845 wdt3800 wdt3801 wdt3802		\
wdt3803 wdt3805 wdt3806 wdt3807 wdt3808 wdt2600 wdt2601 wdt2610		\
wdt2611 wdt1720 wdt1721

# Need simple version number
RLSE = $(PACKAGE_VERSION_MAJOR).$(PACKAGE_VERSION_MINOR)

## Makefile simply needs to build the font database file
noinst_PROGRAMS = gkswdt

# The create_wdt script could be removed if we simply edited gkswdt
# such that it didn't ask any questions.
dist_noinst_SCRIPTS = create_wdt

staretc_DATA = gks.wdt-$(RLSE)

gkswdt_SOURCES = gkswdt.f
gkswdt_LDADD = ../../base/libgks_base.la

# Create the font database from the ascii font files
gks.wdt-$(RLSE): gkswdt create_wdt
	rm -f $@ gkswdt.lst gkswdt.dat gks.wdt cdrive.inc
	cat $(WDTS) > gkswdt.dat
	./create_wdt
	mv gks.wdt gks.wdt-$(RLSE)

## gks.wdt-$(RLSE): gkswdt create_wdt
## 	if test -f gks.wdt-$(RLSE); then \
## 	   rm gks.wdt-$(RLSE) ; \
## 	else :; fi;
## 	if test -f gkswdt.lst; then \
## 	   rm gkswdt.lst ; \
## 	else :; fi;
## 	if test -f gks.wdt; then \
## 	   rm gks.wdt ; \
## 	else :; fi;
## 	if test -f cdrive.inc; then \
## 	   rm cdrive.inc ; \
## 	else :; fi;
## 	cat $(WDTS) > gkswdt.dat
## 	./create_wdt
## 	mv gks.wdt gks.wdt-$(RLSE)

EXTRA_DIST = $(WDTS) wdtbuf.cmn wdtbuf.par wdtenm.par wdtwkd.cmn \
             wdterr.par wdtwrk.cmn wdtfls.cmn wdterr.cmn

CLEANFILES = gks.wdt-$(RLSE) gkswdt.lst gkswdt.dat cdrive.inc



