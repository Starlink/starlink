
bin_PROGRAMS = vot2tab

vot2tab_SOURCES = vot2tab.C $(VOTABLE_ROUTINES)

#  Include files.
AM_CXXFLAGS = -I. -I$(STARLINK)/include/tclutil -I$(STARLINK)/include/astrotcl

#  Common source code.
VOTABLE_ROUTINES =  VOTable.C VOTable1.1.C VOTable1.1_dns.C \
   VOTableStream.C GaiaBase64.C GaiaGzip.C GaiaUtils.C

#  Libraries we link against.
VOTABLE_LIBS = -lastrotcl2.0 -ltclutil2.0 -lxerces-c
vot2tab_LDADD = $(VOTABLE_LIBS)

#  Test programs.

TESTS = readtable_template
check_PROGRAMS = readtable_template

readtable_template_SOURCES = readtable_template.C $(VOTABLE_ROUTINES)
readtable_template_LDADD = $(VOTABLE_LIBS)
