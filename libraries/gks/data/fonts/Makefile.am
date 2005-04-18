##

#  Font files that should be processed.
#  This may be system specific since not all the font files that 
#  are in this directory are actually read.

#  To install a new font, you should add the name
#  of its font description file to the variable FONTS below, set the
#  variable NFONTS to the number of fonts in FONTS.
#  The compulsory font 1 is automatically included and must be omitted
#  from both FONTS and NFONTS.

FONTS = font101 font102 font103 font104 font105 font106 font107 \
    font110 font115
NFONTS = 9

# Need simple version number
RLSE = $(PACKAGE_VERSION_MAJOR).$(PACKAGE_VERSION_MINOR)

## Makefile simply needs to build the font database file
noinst_PROGRAMS = gksdbs
noinst_SCRIPTS = create_dbs

gksdbs_SOURCES = gksdbs.f
gksdbs_LDADD = ../../base/libgks_base.la

staretc_DATA = gks.dbs-$(RLSE)

# In order to create the font file we need to create a wrapper
# script that runs the db creation program with the correct arguments
create_dbs:  $(FONTS)
	if test -f create_dbs; then \
	   rm create_dbs; \
	else :; fi; \
	echo '#! /bin/sh' > create_dbs
	echo '# AUTOMATICALLY GENERATED. DO NOT EDIT' >> create_dbs
	echo './gksdbs <<!' >> create_dbs
	echo `expr $(NFONTS) + 1` >> create_dbs
	echo 'C' >> create_dbs
	echo 'font001' >> create_dbs
	echo '1' >> create_dbs
	for i in $(FONTS); do \
	    echo $$i >> create_dbs; \
	    echo `echo $$i | sed 's/font/\-/'` >> create_dbs; \
	done
	echo 'EXIT' >> create_dbs
	echo '!' >> create_dbs
	chmod 755 create_dbs


# Create the font database from the ascii font files

gks.dbs-$(RLSE): gksdbs create_dbs hershey.dat
	rm -f gks.dbs $@
	./create_dbs
	mv gks.dbs $@

CLEANFILES = gks.dbs-$(RLSE) fort.10

EXTRA_DIST = hershey.dat $(FONTS) params.cmn font001




