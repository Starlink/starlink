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
