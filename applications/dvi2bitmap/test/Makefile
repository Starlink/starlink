# Makefile for the test directory
#
# - The test of hello.dvi is the basic test of dvi2bitmap
# - magtest.tex is a test of whether dvi2bitmap correctly processes DVI
#   and font magnification.
# - modetest.tex is more of a test of the Metafont modes than one of
#   dvi2bitmap, but it does exercise a range of fonts, and so can show
#   some weaknesses.
#
# $Id$

CXX=c++

all: test

test: dvi2bitmap.tested

dvi2bitmap.tested: ../dvi2bitmap hello.dvi
	sh ./test-font-gen.sh ../dvi2bitmap hello.dvi
	touch dvi2bitmap.tested

magtest: ../dvi2bitmap magtest.dvi
	sh ./test-font-gen.sh ../dvi2bitmap magtest.dvi magtest-output

modetest: ../dvi2bitmap modetest.dvi
	sh ./test-font-gen.sh ../dvi2bitmap modetest.dvi modetest-output

hello.dvi: hello.tex
	tex hello

magtest.dvi: magtest.tex
	tex magtest

modetest.dvi: modetest.tex
	latex modetest

dvi2bitmap.diff: ../dvi2bitmap dvi2bitmap.correct
	../dvi2bitmap -X >dvi2bitmap.test
	diff dvi2bitmap.correct dvi2bitmap.test >dvi2bitmap.diff; \
		if test $$? -gt 0; then echo Fail; else echo OK; fi

# The order of the files in the for statement below must match the
# order of the calls in dvi2bitmap::doRegression.
dvi2bitmap.correct: ../dvi2bitmap
	rm -f $@
	for f in ../PkFont.cc ../Util.cc; do \
		FN=`echo $$f|sed -e 's+^.*/\([^.]*\).*$$+\1+' `; \
		sed -n "/REGRESSIONTEST/s+^.*REGRESSIONTEST:+$$FN:+p" $$f >> $@; \
	done