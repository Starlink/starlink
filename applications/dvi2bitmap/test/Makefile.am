## Process this file with automake to produce Makefile.in

# Makefile for the test directory
#
# - The test of hello.dvi is the basic test of dvi2bitmap
# - magtest.tex is a test of whether dvi2bitmap correctly processes DVI
#   and font magnification.
# - modetest.tex is more of a test of the Metafont modes than one of
#   dvi2bitmap, but it does exercise a range of fonts, and so can show
#   some weaknesses.

TESTS = t1.test t2.test t3.test t4.test t5.test t6.test t7.test t8.test

CHECK_programs = $(TESTS)

EXTRA_DIST = t5.dvi modetest.tex modetest-maths.tex

# All the tests depend on the library
$(TESTS): ../libdvi2bitmap.la

SUFFIXES = .dvi .dtl

# The dvi2bitmap program we'll use.  This is usually the one just built in
# the parent, but allow this to be overridden.
DVI2BITMAP = ../dvi2bitmap

.dtl.dvi:
	dt2dv $< $@ >$@.log 2>&1

check_PROGRAMS = $(TESTS)
# t6.test forks and execs itself, so the libtool trick of creating a shell
# script with the same name, which resolves the shared library dependencies,
# won't work.  Instead link it statically.  Note that this fails on Tru64
# because of some as-yet unintelligible linking problem [FIXME: why doesn't
# it work on Tru64 -- I can't see the problem in the ld messages]
t6_test_LDFLAGS = -all-static
t5_test_LDFLAGS = -lm
t7_test_LDFLAGS = -lm
LDADD = ../libdvi2bitmap.la
CPPFLAGS = -I..
t1_test_SOURCES = t1.cc
t2_test_SOURCES = t2.cc
t3_test_SOURCES = t3.cc
t4_test_SOURCES = t4.cc
t5_test_SOURCES = t5.cc
t6_test_SOURCES = t6.cc
t7_test_SOURCES = t7.cc
t8_test_SOURCES = t8.pl
# t8.test is a perl script -- see below

# t8.test is just a link to t8.pl.
t8.test: t8.pl
	if test -w t8.test; then rm -f t8.test;fi
	$(LN_S) t8.pl t8.test

# Test 5 depends on its DVI file; test 7 uses it, too
t5.test$(EXEEXT) t7.test$(EXEEXT): t5.dvi
# Test 6 generates its own test data
t6.data: t6.test$(EXEEXT)
	./t6.test -g 500 >$@

pathtest: hello.dvi
	sh ./test-font-gen.sh $(DVI2BITMAP) hello.dvi temp-hello-output

magtest: magtest.dvi
	sh ./test-font-gen.sh $(DVI2BITMAP) magtest.dvi temp-magtest-output

modetest: modetest.dvi
	sh ./test-font-gen.sh $(DVI2BITMAP) modetest.dvi temp-modetest-output

hello.dvi: hello.tex
	tex hello

magtest.dvi: magtest.tex
	tex magtest

modetest.dvi: modetest.tex
	latex modetest

modetest-maths.dvi: modetest-maths.tex
	latex modetest-maths

modetest-maths-concmath.dvi: modetest-maths.tex
	ln -s modetest-maths.tex modetest-maths-concmath.tex
	latex '\def\optionswitch{1}\input modetest-maths-concmath.tex '
	rm modetest-maths-concmath.tex

%-sampler.html: %.dvi
	./make-sampler.scsh -x$(DVI2BITMAP) -o$(@:.html=) -d$<

sampler: sampler.html modetest-sampler.html modetest-maths-sampler.html modetest-maths-concmath-sampler.html
	test -d sampler || mkdir sampler
	mv modetest-sampler.html modetest-maths-sampler.html modetest-maths-concmath-sampler.html sampler
	mv modetest*.png sampler
	cp sampler.html sampler



MOSTLYCLEANFILES = \
	t6.data \
	*~ *.o *.lo *.test *.stderr \
	*.log *.aux *.pdf \
	test-output* temp* \
	modetest*sampler*



## # Things to do, and dependencies to satisfy, before distribution.
## # Parent Makefile makes `prepare-dist' just before packaging.
## prepare-dist: t5.dvi
## 	echo "Test directory prepared for distribution"

