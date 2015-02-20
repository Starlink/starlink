# Makefile for SUN/152
#
# $Id$


# Following might potentially be overridden on the command line, by
# the parent makefile.  However we have to hope that it isn't, since
# would cause problems -- this Makefile isn't clever enough to make
# anything else.
PKG_NAME=echomop
LATEX_DOCS=sun152.latex_tar
HYPERTEXT_DOCS=sun152.htx_tar

# Bundle up the LaTeX documentation.  The filename must match that required by
# the makefile in the parent directory.
# This dependency on sun152.ind generates and incorporates the index,
# but we don't actually distribute that file.
$(LATEX_DOCS): sun152.tex sun152.ind
	tar cf $@ sun152.tex sun152_*.pdf

# Generate the index
sun152.ind: sun152.tex
	pdflatex 'sun152'
	makeindex sun152

$(HYPERTEXT_DOCS): sun152.htx/sun152.html
	tar cf $@ sun152.htx
