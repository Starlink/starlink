## Process this file with automake to produce Makefile.in

ACLOCAL_AMFLAGS = -I build-aux

bin_SCRIPTS = figaro.csh figaro.icl figaro.sh

dist_bin_SCRIPTS = specdre.csh specdre.icl twodspec.csh twodspec.icl \
                   demo_specdre.icl demo_twodspec.icl demo.icl \
                   specdre.sh twodspec.sh

## Should really call this FIG_DYNAMIC_MEMORY
include_HEADERS = include/DYNAMIC_MEMORY 

dist_noinst_HEADERS = include/DSA_TYPES include/SPD_EPAR  include/arc_dims  include/bytesdef  include/fit_coding_inc  include/gr_inc  \
	include/opt_cmn  include/status_inc

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@ \
    sun86a.tex sun86_2df.pdf sun86_ech1.pdf sun86_ech2.pdf sun86_ech3.pdf \
    sun86_ech4.pdf sun86_ech5.pdf sun86_hard.pdf sun86_icont.png \
    sun86_igrey.png sun86_imag2.png sun86_imag3.png sun86_imag.png \
    sun86_spec2.png sun86_spec.png

# Define the help source file.
HELP_SOURCES = figaro.hlp
starhelp_DATA = figaro.shl

.hlp.shl:
	@HLIB@ $<

## News files
dist_starnews_DATA = figaro.news

SUBDIRS = \
    appcb appsub dsa dta fda fig fibre fit fitres fla gen gkd graph ich jty \
    main opt par spext sploop tio twoutil unix work util \
    figaro1 figaro2 figaro3 figaro4 figaro5 fighelp data

EXTRA_DIST = $(HELP_SOURCES)


CLEANFILES = figaro.shl version.dat
