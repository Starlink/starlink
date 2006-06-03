## Process this file with automake to produce Makefile.in

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@

sc3.htx_tar: sc3-scripts.tex

# sc3-scripts.tex is an appendix to the HTML version of the document,
# comprising the scripts contained in the scripts/ directory.  These are
# also bundled in the htx tarball by virtue of being listed in the
# sc3.htx_tar.extras file.
sc3-scripts.tex: scripts
	cd scripts; \
	for f in *; do if test -f $$f; then \
	    echo $$f | sed 's/\([a-z0-9_]*\).*/\\subsection{\\label{se_\1_source}\\xlabel{\1}\1}/'; \
	    echo "\htmladdnormallink{Download $$f}{$$f}"; \
	    echo ''; \
	    echo '\begin{verbatim}'; \
	    cat $$f; \
	    echo '\end{verbatim}'; \
	fi; done > ../$@

SUBDIRS = scripts
