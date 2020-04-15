## Process this file with automake to produce Makefile.in

SUBDIRS = polsub ccdpack

dist_bin_SCRIPTS = $(MISC_FILES) $(DATA_FILES) $(PUBLICSCRIPTS) $(DYNSCRIPTS) version.dat

LOCAL_LINK = polsub/libpolsub.la ccdpack/libccdpack.la `$(srcdir)/polpack_link_adam`

bin_MONOLITHS = polpack_mon
polpack_mon_SOURCES = $(POLPACK_MONO_SOURCE) $(POLPACK_F_ROUTINES)
polpack_mon_LDADD = $(LOCAL_LINK)
polpack_mon_TASKS = $(POLPACK_TASKS)


#  version.dat is just the package verion in a file for polversion command
#  This is for backwards compatibility since, of course, we could make
#  polversion have the value compiled in

version.dat:
	-rm -f version.dat
	echo $(PACKAGE_VERSION) > version.dat

# Help source file
starhelp_DATA = polpack.shl

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@

.hlp.shl:
	@HLIB@ $<

polpack.hlp: $(POLPACK_F1_ROUTINES) polpack_master.hlp
	cat $(POLPACK_F1_ROUTINES) > all.f
	PATH="$(PATH)":$(STARLINK)/bin/sst; \
	export PATH ; \
	@PROHLP@ in=all.f out=all.hlp single=no
	cat polpack_master.hlp all.hlp > polpack.hlp
	-rm -f all.hlp all.f

#  List of Fortran routines required for building the package.  This is
#  just a list of all the Fortran source files (excluding BLOCK DATA
#  routines and the main routine, which are treated separately).

F_ROUTINES = $(CCDPACK_F_ROUTINES) \
             $(POLPACK_F_ROUTINES) $(POLSUB_F_ROUTINES) \
             $(POLPACK_MONO_SOURCE)

#  All other polpack .f files.
POLPACK_F1_ROUTINES = polbin.f polcal.f polcent.f polexp.f polexpx.f \
polext.f polhelp.f polimage.f polimp.f polimpx.f polka.f polplot.f \
polprep.f polrdtcl.f polsim.f polstack.f polvec.f polversion.f \
polwrtcl.f polzconv.f polrotref.f polwcscopy.f polconcat.f polselect.f \
poledit.f

#  polpack routines for which the atask action name can be derived from the
#  the f file name by remving the .f file suffix.
POLPACK_F_ROUTINES = $(POLPACK_F1_ROUTINES)


MONO_SOURCE = $(POLPACK_MONO_SOURCE)
POLPACK_MONO_SOURCE = polpack_mon.f

#  List of ancillary data files (all types).

DATA_FILES = test_data.sdf

#  Other miscellaneous files in polpack-source.tar
MISC_FILES = blank.bit feature.bit vertex.bit rfeature.bit rvertex.bit \
left_arrow.bit right_arrow.bit tick.bit

#  Define the tasks for each monolith.

POLPACK_TASKS = $(POLPACK_F_ROUTINES:.f=)


#  List of public script files.  These are scripts which form part of
#  the package and will be required by users of it.  They will be
#  installed in the $(INSTALL_BIN) directory with execute permission
#  set.

PUBLICSCRIPTS = Polka.tcl Polka_procs.tcl \
CCDShowHelp.tcl dialog.tcl polimp.csh \
polexp.csh makecube.csh

#  Define the set of all interface files.

IFL_FILES = $(POLPACK_IFL_FILES)

#  Define the interface files required for compilation.

POLPACK_IFL_FILES = $(POLPACK_F_ROUTINES:.f=.ifl)
MONO_IFL = $(MONO_SOURCE:.f=.ifl)

DYNSCRIPTS = polpack.csh polpack.sh polpack.icl

$(IFL_FILES) $(DYNSCRIPTS): polpack.ifd
	mv -f polpack.ifd polpack.ifd.tmp
	sed -e 's,INSTALL_HELP,$(prefix)/help/polpack,g' \
	    -e 's,INSTALL_BIN,$(prefix)/bin/polpack,g' \
	    polpack.ifd.tmp > polpack.ifd
	$(IFD2STAR) polpack
	mv -f polpack.ifd.tmp polpack.ifd

# Fixup scripts with bin location
# The scripts needs to have installation locations edited into them
edit = sed \
	-e 's,@bindir\@,$(bindir),g' \
	-e 's,@VERSION\@,$(VERSION),g'


@PREDIST@polka.htx_tar: polka.tex
@PREDIST@	star2html -splitlevel 3 polka.tex
@PREDIST@	test -d polka.htx
@PREDIST@	if test -f polka.htx_tar.extras; then \
@PREDIST@	   for f in `cat polka.htx_tar.extras`; do \
@PREDIST@	      test -f "$f" && cp "$f" polka.htx || true; \
@PREDIST@	   done; else :; fi
@PREDIST@	tar cf polka.htx_tar polka.htx


# Polka needs to be installed as other .htx_tar files but into starhelp
# starhelp_DATA is used earlier
polkahelpdir = $(starhelpdir)
dist_polkahelp_DATA = polka.htx_tar

dist_starnews_DATA = polpack.news

MISC_BUILT_FILES = polpack.hlp polpack.shl polka.htx_tar \
version.dat polpack_mon polpack.csh polpack.icl polpack.sh

BUILT_IFC_FILES = polbin.ifc polcal.ifc polcent.ifc polexp.ifc \
polexpx.ifc polext.ifc polhelp.ifc polimage.ifc \
polimp.ifc polimpx.ifc polka.ifc polplot.ifc \
polprep.ifc polrdtcl.ifc polsim.ifc polstack.ifc \
polvec.ifc polversion.ifc polwrtcl.ifc polzconv.ifc \
polpack_mon.ifc polrotref.ifc polwcscopy.ifc polconcat.ifc \
polselect.ifc poledit.ifc

BUILT_IFL_FILES = polbin.ifl polcal.ifl polcent.ifl polexp.ifl \
polexpx.ifl polext.ifl polhelp.ifl polimage.ifl \
polimp.ifl polimpx.ifl polka.ifl polpack_mon.ifl \
polplot.ifl polprep.ifl polrdtcl.ifl polsim.ifl \
polstack.ifl polvec.ifl polversion.ifl polwrtcl.ifl \
polzconv.ifl polrotref.ifl polwcscopy.ifl polconcat.ifl \
polselect.ifl poledit.ifl

EXTRA_DIST = polpack_master.hlp polka.htx_tar

CLEANFILES = $(MISC_BUILT_FILES) $(BUILT_IFC_FILES) $(BUILT_IFL_FILES)

#  After all other installation tasks have been done, untar the polka
#  on-line help, remove the tar file and HTX link it.
install-data-hook:
	f="$(prefix)/help/polpack/polka.htx_tar";\
	d="$(prefix)/help/polpack";\
	if test -n "$(PAX)"; then \
	  if $(MANIFEST); then \
	    $(PAX) -f $$f | \
	      sed 's+^+MANIFEST:$$d/+'; \
	  fi; \
	  cat $$f | (cd $$d; $(PAX) -r); \
	  rm -f $$f; \
	elif test -n "$(TAR)"; then \
	  if $(MANIFEST); then \
	    cat $$f | (cd $$d; $(TAR) xBpvf -) | sed 's+^+MANIFEST:$$d/+'; \
	  else \
	    cat $$f | (cd $$d; $(TAR) xBpf -); \
	  fi; \
	  rm -f $$f; \
	else \
	   echo "Neither tar nor pax!!!" >&2; \
	   exit 1; \
	fi
	if test -f $(prefix)/bin/hlink; then \
	    $(prefix)/bin/hlink -v $(prefix)/help/polpack; \
	fi

