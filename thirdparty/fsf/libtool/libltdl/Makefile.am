## Process this file with automake to produce Makefile.in

AUTOMAKE_OPTIONS = no-dependencies foreign

if INSTALL_LTDL
include_HEADERS = ltdl.h
lib_LTLIBRARIES = libltdl.la
else
noinst_HEADERS = ltdl.h
endif

if CONVENIENCE_LTDL
noinst_LTLIBRARIES = libltdlc.la
endif

## Make sure these will be cleaned even when they're not built by
## default.
CLEANFILES = libltdl.la libltdlc.la

libltdl_la_SOURCES = ltdl.c
libltdl_la_LDFLAGS = -no-undefined -version-info 4:1:1
libltdl_la_LIBADD = $(LIBADD_DL)

libltdlc_la_SOURCES = ltdl.c
libltdlc_la_LIBADD = $(LIBADD_DL)

## Because we do not have automatic dependency tracking:
ltdl.lo: ltdl.h config.h

$(libltdl_la_OBJECTS) $(libltdlc_la_OBJECTS): libtool
libtool: $(LIBTOOL_DEPS)
	$(SHELL) ./config.status --recheck

## ## This allows us to install libltdl without using ln and without creating
## ## a world writeable directory.
## ## FIXME:  Remove this rule once automake can do this properly by itself.
## ## Starlink: add a trace of the installed files to MANIFEST.
## local-install-files: $(DISTFILES)
## 	-rm -rf $(DESTDIR)$(datadir)/libtool/libltdl
## 	$(mkinstalldirs) $(DESTDIR)$(datadir)/libtool/libltdl
## 	@for file in $(DISTFILES); do \
## 	  case $$file in \
## 	    $(srcdir)/*) file=`echo "$$file" | sed "s|^$(srcdir)/||"`;; \
## 	  esac; \
## 	  d=$(srcdir); \
## 	  if test -d $$d/$$file; then \
## 	    cp -r $$d/$$file $(DESTDIR)$(datadir)/libtool/libltdl/$$file; \
## 	    if $(MANIFEST); then \
## 	      (cd $$d/$$file; /bin/ls -R . | awk '/^ *$$/{next};BEGIN{p="'$(DESTDIR)$(datadir)/libtool/libltdl/$$file'"};{if (sub(/:$$/,"")) p="'$(DESTDIR)$(datadir)/libtool/libltdl/$$file'/"$$0; else printf "MANIFEST:%s/%s\n", p, $$0;}'); \
## 	    fi; \
## 	  else \
## 	    test -f $(DESTDIR)$(datadir)/libtool/libltdl/$$file \
## 	    || cp $$d/$$file $(DESTDIR)$(datadir)/libtool/libltdl/$$file || :; \
## 	    $(MANIFEST) && echo MANIFEST:$(DESTDIR)$(datadir)/libtool/libltdl/$$file || :; \
## 	  fi; \
## 	done


## These are installed as a subdirectory of pkgdatadir so that
## libtoolize --ltdl can find them later:
ltdldatadir = $(datadir)/libtool/libltdl
ltdldatafiles = COPYING.LIB README acinclude.m4 aclocal.m4 \
		Makefile.am Makefile.in configure.ac configure \
		config-h.in config.guess config.sub \
		install-sh missing ltmain.sh \
		ltdl.c ltdl.h

## To avoid spurious reconfiguration when the user installs these files
## with libtoolize, we have to preserve their timestamps carefully:
## Starlink: add a trace of the installed files to MANIFEST
local-install-files: $(ltdldatafiles)
	-rm -rf $(DESTDIR)$(ltdldatadir)
	$(mkinstalldirs) $(DESTDIR)$(ltdldatadir)
	if $(MANIFEST); then \
	  ( cd $(srcdir) && $(AMTAR) chf - $(ltdldatafiles); ) \
	    | ( umask 0 && cd $(DESTDIR)$(ltdldatadir) && $(AMTAR) xvf -; ) | \
	  sed 's,^,MANIFEST:$(DESTDIR)/$(ltdldatadir)/,'; \
	else \
	  ( cd $(srcdir) && $(AMTAR) chf - $(ltdldatafiles); ) \
	    | ( umask 0 && cd $(DESTDIR)$(ltdldatadir) && $(AMTAR) xf -; ) \
	fi
