# Makefile for SUN/152
#
# $Id$


# Following might be overridden on the command line (by the parent makefile)
PKG_NAME=echomop
LATEX_DOCS=sun152.latex_tar
HYPERTEXT_DOCS=sun152.htx_tar

# Bundle up the LaTeX documentation.  The filename must match that required by
# the makefile in the parent directory.
# This dependency on sun152.ind generates and incorporates the index,
# but we don't actually distribute that file.
$(LATEX_DOCS): sun152.tex sun152.ind
	tar cf $@ sun152.tex sun152*eps

# Generate the index and paste it in place, replacing the previous version.
sun152.ind: sun152.tex
	latex '\def\generateindex{1}\input sun152'
	makeindex sun152
	sed -e '/\\begin{theindex}/r sun152.ind' -e '/\\begin{theindex}/,/\\end{theindex}/d' sun152.tex >t
	mv t sun152.tex

$(HYPERTEXT_DOCS): sun152.htx/sun152.html
	tar cf $@ sun152.htx

# The LaTeX2html commands needs some preprocessing.  The .tex file has
# the magic within it.
sun152.htx/sun152.html: sun152.tex
	sed -n 's/^%%S2HPOST//p' sun152.tex | csh -f
#	sed -n -e '1s+.*+cd '`pwd`+p -e 's/^%%S2HPOST//p' sun152.tex | csh -f


