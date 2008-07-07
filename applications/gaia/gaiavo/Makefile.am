
bin_PROGRAMS = vot2tab tab2vot

vot2tab_SOURCES = vot2tab.C $(VOTABLE_ROUTINES)
tab2vot_SOURCES = tab2vot.C $(VOTABLE_ROUTINES)

#  Include files.
AM_CXXFLAGS = -I. -I$(STARLINK)/include/tclutil \
   -I$(STARLINK)/include/astrotcl -I$(STARLINK)/include/cat

#  Common source code.
VOTABLE_ROUTINES =  VOTable.C VOTableWriteFunctions.C VOTableReadFunctions.C \
   VOTable1.1.C VOTable1.1_dns.C VOTableStream.C GaiaBase64.C GaiaGzip.C \
   GaiaUtils.C

#  Explicit dependencies.
VOTableFunctions.C: VOTableWriteFunctions.icc

#  Libraries we link against.
VOTABLE_LIBS = -lastrotcl2.0 -ltclutil2.0 -lxerces-c -lcat4.0
vot2tab_LDADD = $(VOTABLE_LIBS)
tab2vot_LDADD = $(VOTABLE_LIBS)

#  Test programs.

TESTS = readtable_template
check_PROGRAMS = readtable_template

readtable_template_SOURCES = readtable_template.C $(VOTABLE_ROUTINES)
readtable_template_LDADD = $(VOTABLE_LIBS)

#  Generate parsing and serialization code from VOTable schema. Requires
#  XSD from codesynthesis to be installed. Not part of distribution.
VOTable1.1.C: VOTable1.1.xsd
	xsd cxx-tree --root-element VOTABLE \
	    --generate-serialization \
	    --namespace-map http://www.ivoa.net/xml/VOTable/v1.1=votable_11 \
	    --parser-regex '/VOTABLE/VOTABLE_read/' \
	    --serializer-regex '/VOTABLE/VOTABLE_write/' \
	    VOTable1.1.xsd
	rm -f VOTable1.1.C
	mv VOTable1.1.cxx VOTable1.1.C

#  Note no serialization support for this. Do not want to write these.
VOTable1.1_dns.C: VOTable1.1_dns.xsd
	xsd cxx-tree --root-element VOTABLE \
	    --namespace-map '=votable_11_dns' \
	    --parser-regex '/VOTABLE/VOTABLE_read/' \
	    VOTable1.1_dns.xsd
	rm -f VOTable1.1_dns.C
	mv VOTable1.1_dns.cxx VOTable1.1_dns.C
