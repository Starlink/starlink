## Process this file with automake to produce Makefile.in

bin_SCRIPTS = starmem_link
cincludedir = $(includedir)/star
cinclude_HEADERS = $(PUBLIC_CINCLUDES)
noinst_HEADERS = $(PRIVATE_INCLUDES)
lib_LTLIBRARIES = libstarmem.la

libstarmem_la_SOURCES = $(PRIVATE_C_FILES) $(PUBLIC_C_FILES)

# We need to install starMemInit.c as a starMemInit.o for Fortran interface
starMemInit.o: starMemInit.c
install-exec-local: starMemInit.o
	$(mkdir_p) $(DESTDIR)$(libdir)
	$(INSTALL_PROGRAM) starMemInit.o $(DESTDIR)$(libdir)
	$(MANIFEST) && echo "MANIFEST:$(DESTDIR)$(libdir)/starMemInit.o" || :

PUBLIC_C_FILES = starMalloc.c starMallocAtomic.c starMemInitPrivate.c \
starFree.c starFreeForce.c starRealloc.c starCalloc.c \
starMemIsInitialised.c

PRIVATE_C_FILES = mem1_globals.c

PUBLIC_CINCLUDES = mem.h
PRIVATE_INCLUDES = mem1.h

TESTS = memtest

check_PROGRAMS = memtest
memtest_SOURCES = memtest.c
memtest_LDADD = libstarmem.la


# Need to modify some files during make
edit = sed \
	-e 's,@libdir\@,$(libdir),g' \
	-e 's,@LIBS\@,$(LIBS),g'

starmem_link: starmem_link.in
	rm -f starmem_link starmem_link.tmp
	$(edit) \
	   -e 's,@edited_input\@,starmem_link: Generated from starmem_link.in by Makefile.am,' \
	   $(srcdir)/starmem_link.in > starmem_link.tmp
	chmod +x starmem_link.tmp
	mv starmem_link.tmp starmem_link

