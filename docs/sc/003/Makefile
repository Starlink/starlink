# Makefile for SUN/152
#
# $Id$


# Following might potentially be overridden on the command line, by
# the parent makefile.  However we have to hope that it isn't, since
# would cause problems -- this Makefile isn't clever enough to make
# anything else.
LATEX_DOCS=sc3.latex_tar
HYPERTEXT_DOCS=sc3.htx_tar
## %%VERSION%%
VERSION=3.2-0

# Bundle up the LaTeX documentation.  The filename must match that required by
# the makefile in the parent directory.
# This dependency on sun152.ind generates and incorporates the index,
# but we don't actually distribute that file.
$(LATEX_DOCS): sc3.tex
	tar cf $@ sc3.tex sc3*eps

$(HYPERTEXT_DOCS): sc$(VERSION).htx_tar
	if test -e $@; then rm -f $@; fi
	ln -s $< $@

sc$(VERSION).htx_tar: sc3.htx/sc3.html
	tar cf $@ sc3.htx

sc3.htx/sc3.html: sc3.tex sc3-scripts.tex
	if test -e sc3.htx; then rm -Rf sc3.htx; fi
	star2html sc3
	mkdir sc3.htx/scripts
	for f in scripts/*; do \
	    if test -f $$f; then cp $$f sc3.htx/scripts; fi; \
	done

sc3-scripts.tex: scripts
	cd scripts; \
	for f in *; do if test -f $$f; then \
	    echo $$f | sed 's/\([a-z0-9_]*\).*/\\subsection{\\label{se_\1_source}\\xlabel{\1}\1}/'; \
	    echo "\htmladdnormallink{Download $$f}{scripts/$$f}"; \
	    echo ''; \
	    echo '\begin{verbatim}'; \
	    cat $$f; \
	    echo '\end{verbatim}'; \
	fi; done > ../$@

sc$(VERSION)-scripts.tar:
	DN=sc$(VERSION)-scripts; \
	    mkdir $$DN; \
	    for f in scripts/*; do if test -f $$f; then cp $$f $$DN; fi; done;\
	    tar cf $@ $$DN; \
	    rm -Rf $$DN

Makefile sc3.tex: VERSION
	./propagate-version Makefile sc3.tex
