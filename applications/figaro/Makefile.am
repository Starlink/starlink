## Process this file with automake to produce Makefile.in

bin_SCRIPTS = specdre.csh specdre.icl figaro.csh figaro.icl demo.icl \
demo_specdre.icl demo_twodspec.icl twodspec.icl twodspec.csh

## Should really call this FIG_DYNAMIC_MEMORY
include_HEADERS = include/DYNAMIC_MEMORY 

dist_noinst_HEADERS = include/DSA_TYPES include/SPD_EPAR  include/arc_dims  include/bytesdef  include/fit_coding_inc  include/gr_inc  \
	include/opt_cmn  include/status_inc

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@ \
    sun86a.tex sun86_2df.eps sun86_ech1.eps sun86_ech2.eps sun86_ech3.eps \
    sun86_ech4.eps sun86_ech5.eps sun86_hard.eps sun86_icont.eps \
    sun86_igrey.eps sun86_imag2.eps sun86_imag3.eps sun86_imag.eps \
    sun86_spec2.eps sun86_spec.eps

# Define the help source file.
starhelp_DATA = figaro.shl

.hlp.shl:
	@HLIB@ $<

## News files
dist_pkgdata_DATA = figaro.news

SUBDIRS = \
    appcb appsub dsa dta fda fig fibre fit fitres fla gen gkd graph ich jty \
    main opt par spext sploop tio twoutil unix work util \
    figaro1 figaro2 figaro3 figaro4 figaro5 
