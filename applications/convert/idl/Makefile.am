## Process this file with automake to produce Makefile.in

## List the scripts which are to be installed in the `bin' directory.
bin_SCRIPTS = read_ndf.pro write_ndf.pro idl2hds.pro addnames.pro \
 convert_idl.dlm

## List libraries which are to be built, these are not installed by this
## package, convert does that.
lib_LTLIBRARIES = convert_idl.la

## Declare the sources for the library.
convert_idl_la_SOURCES = \
 $(PRIVATE_INCLUDES) \
 $(C_ROUTINES)

convert_idl_la_LIBADD = $(LIBADD) `ndf_link` @FCLIBS@

## We want to dlopen this library.
convert_idl_la_LDFLAGS = -module

PRIVATE_INCLUDES = export.h hds.h hds2idl.h

C_ROUTINES = read_ndf.c write_ndf.c probe_ndf.c crehds.c hds2idl.c \
 idlstructdef.c idlstructarrdef.c idlstructfill.c idlprimfill.c \
 hdsstructwrite.c hdsprimwrite.c gethdstype.c getidltype.c \
 getidlstringsize.c getobjectdetails.c getstringarray.c tagstrip.c \
 getcomp.c checkarr.c IDL_Load.c dat.c

##  Dummy Fortran routine to make sure we can handle the Fortran in the
##  dependency libraries. Removed as got duplicate main reports in OS X.
##  Should revisit this question sometime.
F_ROUTINES = dummy.f

## In order to get the correct name for the dlopened library we need to let
## libtool do it's work and then query the convert_idl.la file for the dlname
## variable. Once we have this name we can subsitute it into some of the
## scripts. Do this at install-time. ENTRY_PREFIX is some prefix used to access
## the function, see the IDL documentation if needs to be set.

ENTRY_PREFIX=

getdlname = grep 'dlname=' convert_idl.la | sed -e 's:dlname=::' -e s:\'::g
getshortdlname = grep '^library_names=' convert_idl.la | sed -e s:\'::g | tr " " "\n" | tail -1

edit = sed \
        -e 's,@libdir\@,$(libdir),g' \
        -e "s,@DLNAME\@,$$DLNAME,g" \
	-e 's,@ENTRY_PREFIX\@,$(ENTRY_PREFIX),g'

read_ndf.pro: convert_idl.la read_ndf.pro.in
	DLNAME="`$(getdlname)`";\
	rm -f read_ndf.pro.tmp read_ndf.pro;\
	$(edit) \
	   $(srcdir)/read_ndf.pro.in >read_ndf.pro.tmp;\
	mv read_ndf.pro.tmp read_ndf.pro

write_ndf.pro: convert_idl.la write_ndf.pro.in
	DLNAME="`$(getdlname)`";\
	rm -f write_ndf.pro.tmp write_ndf.pro;\
	$(edit) \
	   $(srcdir)/write_ndf.pro.in >write_ndf.pro.tmp;\
	mv write_ndf.pro.tmp write_ndf.pro

EXTRA_DIST = read_ndf.pro.in  write_ndf.pro.in idl2hds.pro addnames.pro \
             convert_idl.dlm 

MISC_BUILT_FILES = version.dat read_ndf.pro write_ndf.pro

CLEANFILES = $(MISC_BUILT_FILES)

##
## Post install hooks. Need to make a link to the shareable library into
## bindir for compatibility with old behaviour and DLM loaded code. Note
## DLM does like versioning of the sharable library, so we avoid the 
## dlname and use the last of library_names.
## 
install-exec-hook: convert_idl.la
	DLNAME="`$(getshortdlname)`" ;\
	cd $(DESTDIR)/$(bindir) ;\
	if test -f "$$DLNAME"; then \
	   rm -f $$DLNAME ;\
	fi ;\
	$(LN_S) `$(PERL) -MFile::Spec -e "print File::Spec->abs2rel('$(libdir)/$$DLNAME')"` $$DLNAME
