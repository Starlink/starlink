## Process this file with automake to produce Makefile.in

# grfont.dat and rgb.txt are not actually programs but they still need to
# go into bin/

noinst_PROGRAMS = pgpack
pgpack_SOURCES = pgpack.f

# How else to install into bin?
bin_SCRIPTS = grfont.dat

grfont.dat: grfont.txt pgpack.f
	-rm -f grfont.dat
	./pgpack <grfont.txt
