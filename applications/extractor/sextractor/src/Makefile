# SExtractor V2.0
#
# general makefile
#
# E.BERTIN 28/08/1998
# P.W.Draper 24/11/1998: Remove copy to home directory.
#
include Macrodefs.$(SEXMACHINE)
DEST  = ${HOME}/bin
LIBS  = -L. -lwcs_c -lm
ALL   = analyse.o assoc.o astrom.o back.o bpro.o catout.o check.o clean.o \
	extract.o field.o filter.o fitscat.o fitsconv.o fitshead.o fitskey.o \
	fitsmisc.o fitsread.o fitstab.o fitsutil.o fitswrite.o flag.o graph.o \
	growth.o image.o interpolate.o main.o makeit.o manobjlist.o misc.o \
	neurro.o pc.o photom.o plist.o poly.o prefs.o psf.o readimage.o \
	refine.o retina.o scan.o som.o weight.o libwcs_c.a
STDH  = define.h globals.h types.h
STDO  = 

sex: $(ALL)
	$(CC) $(ALL) -o sex $(LIBS) $(COPTS)

clean:
	$(RM) $(ALL) sex
	($(CD) wcs && $(MAKE) clean)

analyse.o: $(STDH) $(STDO) analyse.c assoc.h back.h check.h growth.h image.h \
		photom.h plist.h growth.h psf.h
	$(CC) $(COPTS) -c analyse.c

assoc.o: $(STDH) $(STDO) assoc.c assoc.h
	$(CC) $(COPTS) -c assoc.c

astrom.o: $(STDH) $(STDO) astrom.c astrom.h
	$(CC) $(COPTS) -c astrom.c

back.o: $(STDH) $(STDO) back.c back.h field.h
	$(CC) $(COPTS) -c back.c

bpro.o: $(STDH) $(STDO) bpro.c bpro.h fitscat.h
	$(CC) $(COPTS) -c bpro.c

catout.o: $(STDH) $(STDO) catout.c param.h sexhead.h sexhead1.h sexheadsc.h
	$(CC) $(COPTS) -c catout.c

check.o: $(STDH) $(STDO) check.c check.h
	$(CC) $(COPTS) -c check.c

clean.o: $(STDH) $(STDO) clean.c clean.h check.h
	$(CC) $(COPTS) -c clean.c

extract.o: $(STDH) $(STDO) extract.c extract.h plist.h
	$(CC) $(COPTS) -c extract.c

field.o: $(STDH) $(STDO) field.c field.h interpolate.h
	$(CC) $(COPTS) -c field.c

filter.o: $(STDH) $(STDO) filter.c bpro.h filter.h image.h
	$(CC) $(COPTS) -c filter.c

fitscat.o: $(STDH) $(STDO) fitscat.c fitscat_defs.h fitscat.h
	$(CC) $(COPTS) -c fitscat.c

fitsconv.o: $(STDH) $(STDO) fitsconv.c fitscat_defs.h fitscat.h
	$(CC) $(COPTS) -c fitsconv.c

fitshead.o: $(STDH) $(STDO) fitshead.c fitscat_defs.h fitscat.h
	$(CC) $(COPTS) -c fitshead.c

fitskey.o: $(STDH) $(STDO) fitskey.c fitscat_defs.h fitscat.h
	$(CC) $(COPTS) -c fitskey.c

fitsmisc.o: $(STDH) $(STDO) fitsmisc.c fitscat_defs.h fitscat.h
	$(CC) $(COPTS) -c fitsmisc.c

fitsread.o: $(STDH) $(STDO) fitsread.c fitscat_defs.h fitscat.h
	$(CC) $(COPTS) -c fitsread.c

fitstab.o: $(STDH) $(STDO) fitstab.c fitscat_defs.h fitscat.h
	$(CC) $(COPTS) -c fitstab.c

fitsutil.o: $(STDH) $(STDO) fitsutil.c fitscat_defs.h fitscat.h
	$(CC) $(COPTS) -c fitsutil.c

fitswrite.o: $(STDH) $(STDO) fitswrite.c fitscat_defs.h fitscat.h
	$(CC) $(COPTS) -c fitswrite.c

flag.o: $(STDH) $(STDO) flag.c flag.h plist.h
	$(CC) $(COPTS) -c flag.c

graph.o: $(STDH) $(STDO) graph.c
	$(CC) $(COPTS) -c graph.c

growth.o: $(STDH) $(STDO) growth.c growth.h
	$(CC) $(COPTS) -c growth.c

image.o: $(STDH) $(STDO) image.c
	$(CC) $(COPTS) -c image.c

interpolate.o: $(STDH) $(STDO) interpolate.c interpolate.h
	$(CC) $(COPTS) -c interpolate.c

main.o: $(STDH) $(STDO) main.c
	$(CC) $(COPTS) -c main.c

makeit.o: $(STDH) $(STDO) makeit.c check.h growth.h interpolate.h psf.h \
	weight.h
	$(CC) $(COPTS) -c makeit.c

manobjlist.o: $(STDH) $(STDO) manobjlist.c plist.h
	$(CC) $(COPTS) -c manobjlist.c

misc.o: $(STDH) $(STDO) misc.c
	$(CC) $(COPTS) -c misc.c

neurro.o: $(STDH) $(STDO) neurro.c neurro.h
	$(CC) $(COPTS) -c neurro.c         

pc.o: $(STDH) $(STDO) pc.c image.h psf.h
	$(CC) $(COPTS) -c pc.c

photom.o: $(STDH) $(STDO) photom.c photom.h
	$(CC) $(COPTS) -c photom.c

plist.o: $(STDH) $(STDO) plist.c plist.h
	$(CC) $(COPTS) -c plist.c

poly.o: $(STDH) $(STDO) poly.c poly.h
	$(CC) $(COPTS) -c poly.c

prefs.o: $(STDH) $(STDO) prefs.c prefs.h
	$(CC) $(COPTS) -c prefs.c

psf.o: $(STDH) $(STDO) psf.c image.h psf.h
	$(CC) $(COPTS) -c psf.c

readimage.o: $(STDH) $(STDO) readimage.c check.h interpolate.h weight.h
	$(CC) $(COPTS) -c readimage.c

refine.o: $(STDH) $(STDO) refine.c extract.h plist.h
	$(CC) $(COPTS) -c refine.c

retina.o: $(STDH) $(STDO) retina.c image.h retina.h fitscat.h
	$(CC) $(COPTS) -c retina.c

scan.o: $(STDH) $(STDO) scan.c check.h clean.h extract.h plist.h
	$(CC) $(COPTS) -c scan.c

som.o: $(STDH) $(STDO) som.c som.h
	$(CC) $(COPTS) -c som.c

weight.o: $(STDH) $(STDO) weight.c weight.h field.h
	$(CC) $(COPTS) -c weight.c

libwcs_c.a:
	($(CD) wcs && $(MAKE))

