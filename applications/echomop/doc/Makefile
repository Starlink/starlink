# Makefile for the various forms of ECHOMOP documentation.
#
# This is largely a slave makefile for the makefile in the parent directory.
#
# Parts of this (specifically the generation of the defects list) use
# software installed only on Norman's machine, and so those parts are
#
#      NOT BUILDABLE AFTER DISTRIBUTION
#
# $Id$


# Following might be overridden on the command line (by the parent makefile)
PKG_NAME=echomop
LATEX_DOCS=sun152.latex_tar
HYPERTEXT_DOCS=sun152.htx_tar

# Bundle up the LaTeX documentation.  The filename must match that required by
# the makefile in the parent directory.
$(LATEX_DOCS): sun152/sun152.tex
	cd sun152; tar cf ../$@ sun152*

# This isn't buildable at present -- it should simply be present.
# Following fails with an error if the bundle isn't already there.
$(HYPERTEXT_DOCS):

# Bundle up the HTML version of the HELP.  Filename must match that
# required by the makefile in the parent directory.
#
# The command here will simply hoover up everything in the directory,
# including any CVS directory present.  We could avoid this using
# `file', but at the risk of biting ourselves with excess cleverness.
# It's not really a problem, since the distributions will typically be
# built from exported copies of the repository rather than checked out ones.
$(PKG_NAME)_hyper.tar: htmlhelp/intro.html
	cd htmlhelp; tar cf ../$@ *


