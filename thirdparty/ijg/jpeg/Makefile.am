## Process this file with automake to produce Makefile.in

@SET_MAKE@

# We can't override target `all', but `all-local' is the approved hook.
all-local: $(MAIN_TARGET)

# Build jpeglib
jpeglib:
	cd src; $(MAKE) all

# Don't build jpeglib.  Just create a stamp file.
jpeglib-stamp:
	rm -f $@
	{ date; \
          echo "jpeglib.h found in system -- no need to build our own"; } >$@

# Recursive targets, use local versions so that we get proper cleans too.
# This means we must expand into explicit lines so that automake see them
# (previously these three were a macro, plus target)
clean-local:
	- cd src; $(MAKE) clean

distclean-local:
	- cd src; $(MAKE) distclean

check:
	- cd src; $(MAKE) check

#We need to distribute the source.

EXTRA_DIST = src/ansi2knr.1 src/jchuff.c src/jdapistd.c src/jidctint.c src/makeapps.ds \
src/rdswitch.c src/ansi2knr.c src/jchuff.h src/jdatadst.c src/jidctred.c \
src/makefile.ansi src/rdtarga.c src/cderror.h src/jcinit.c src/jdatasrc.c \
src/jinclude.h src/makefile.bcc src/README src/cdjpeg.c src/jcmainct.c \
src/jdcoefct.c src/jmemansi.c src/makefile.cfg src/structure.doc src/cdjpeg.h \
src/jcmarker.c src/jdcolor.c src/jmemdosa.asm src/makefile.dj src/testimg.bmp \
src/change.log src/jcmaster.c src/jdct.h src/jmemdos.c src/makefile.manx \
src/testimg.jpg src/cjpeg.1 src/jcomapi.c src/jddctmgr.c src/jmemmac.c \
src/makefile.mc6 src/testimgp.jpg src/cjpeg.c src/jconfig.bcc src/jdhuff.c \
src/jmemmgr.c src/makefile.mms src/testimg.ppm src/ckconfig.c src/jconfig.cfg \
src/jdhuff.h src/jmemname.c src/makefile.sas src/testorig.jpg src/coderules.doc \
src/jconfig.dj src/jdmainct.c src/jmemsys.h src/makefile.vc src/transupp.c \
src/config.sub src/jconfig.mac src/jdinput.c src/jmemsys.h src/makefile.vc \
src/transupp.c src/config.sub src/jconfig.mac src/jdmarker.c src/jmemnobs.c \
src/makefile.unix src/testprog.jpg src/config.guess src/jconfig.doc src/jdmainct.c \
src/jmemsys.h src/makefile.vc src/transupp.c src/config.sub src/jconfig.mac \
src/jdmarker.c src/jmorecfg.h src/makefile.vms src/transupp.h src/configure \
src/jconfig.manx src/jdmaster.c src/jpegint.h src/makefile.wat src/usage.doc \
src/jconfig.mc6 src/jdmerge.c src/jpeglib.h src/makelib.ds src/wizard.doc \
src/djpeg.1 src/jconfig.sas src/jdphuff.c src/jpegtran.1 src/makeproj.mac \
src/wrbmp.c src/djpeg.c src/jconfig.st src/jdpostct.c src/jpegtran.c \
src/makljpeg.st src/wrgif.c src/example.c src/jconfig.vc src/jdsample.c \
src/jquant1.c src/maktjpeg.st src/wrjpgcom.1 src/filelist.doc src/jconfig.vms \
src/jdtrans.c src/jquant2.c src/makvms.opt src/wrjpgcom.c src/install.doc \
src/jconfig.wat src/jerror.c src/jutils.c src/rdbmp.c src/wrppm.c src/install-sh \
src/jcparam.c src/jerror.h src/jversion.h src/rdcolmap.c src/wrrle.c \
src/jcapimin.c src/jcphuff.c src/jfdctflt.c src/libjpeg.doc src/rdgif.c \
src/wrtarga.c src/jcapistd.c src/jcprepct.c src/jfdctfst.c src/ltconfig \
src/rdjpgcom.1 src/jccoefct.c src/jcsample.c src/jfdctint.c src/ltmain.sh \
src/rdjpgcom.c src/jccolor.c src/jctrans.c src/jidctflt.c src/makcjpeg.st \
src/rdppm.c src/jcdctmgr.c src/jdapimin.c src/jidctfst.c src/makdjpeg.st \
src/rdrle.c

CLEANFILES = jpeglib-stamp

