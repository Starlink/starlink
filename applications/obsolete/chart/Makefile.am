## Process this file with automake to produce Makefile.in

SUBDIRS = @subdirs@

bin_SCRIPTS = $(DYNSCRIPTS)
bin_MONOLITHS = chart_mon
chart_mon_SOURCES = chart_mon.f
chart_mon_LDADD = applic/libapplic.la libchart/libchart.la \
`sgs_link_adam` `chr_link_adam` `gks_link_adam` `pda_link_adam`

chart_mon_TASKS = $(TASKS)

BUILT_SOURCES = chart.csh chart.icl $(IFL_FILES)

TASKS = astro bsearch display ecreate fcreate listo oset ploto \
	qcreate rgogs search setone sset typeo cht_disp

IFL_FILES = $(TASKS:=.ifl) chart.ifl

DYNSCRIPTS = chart.csh chart.icl chart.sh

PRIVATE_INCLUDES = MAIN CHT_ERR

#  Generate ifl and startup scripts from the ifd file and then do any 
#  necessary post processing of the generated files to overcome 
#  deficiencies in ifd2star
$(IFL_FILES) $(DYNSCRIPTS): chart.ifd
	$(IFD2STAR) chart


# Helper programs
noinst_PROGRAMS = crepar
crepar_SOURCES = crepar.f
crepar_LDADD = libchart/libchart.la `err_link`
