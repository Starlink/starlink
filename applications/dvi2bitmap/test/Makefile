# Makefile for the test directory
#
# $Id$

test: dvi2bitmap.tested

dvi2bitmap.tested: hello.dvi
	sh ./test-font-gen.sh ../dvi2bitmap hello.dvi
	touch dvi2bitmap.tested

hello.dvi: hello.tex
	tex hello
