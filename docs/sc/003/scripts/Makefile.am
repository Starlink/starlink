## Process this file with automake to produce Makefile.in

examplesdocsdir = $(starexamplesdir)/sc3

## These IRAF scripts did not have execute access, hence they're in DATA.
examplesdocs_DATA = echimdivide.cl echmkv.cl echtrim.cl 

examplesdocs_SCRIPTS = arcpoly echrdarc echrduce echtract_echarc \
   echtract_echomop meanarc preparcs prepbias prepflat prepobjs \
   preprun1 preprun2 stackgen_echarc stackgen_echomop tracepoly

EXTRA_DIST = $(examplesdocs_DATA) $(examplesdocs_SCRIPTS)