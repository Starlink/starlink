## Process this file with automake to produce Makefile.in

SUBDIRS = @subdirs@

bin_PROGRAMS = ast2a ast2b csi2a csi2b nso2a nso2b
bin_SCRIPTS = $(DYNSCRIPTS) chart_params
bin_MONOLITHS = chart_mon
chart_mon_SOURCES = chart_mon.f
chart_mon_LDADD = applic/libapplic.la libchart/libchart.la \
`sgs_link_adam` `chr_link_adam` `gks_link_adam` `pda_link_adam`

chart_mon_TASKS = $(TASKS)

ast2a_SOURCES = ast2a.f
ast2b_SOURCES = ast2b.f
csi2a_SOURCES = csi2a.f
csi2b_SOURCES = csi2b.f
nso2a_SOURCES = nso2a.f
nso2b_SOURCES = nso2b.f

BUILT_SOURCES = chart.csh chart.icl $(IFL_FILES)

TASKS = astro bsearch display ecreate fcreate listo oset ploto \
	qcreate rgogs search setone sset typeo disp

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

#  Install data files into etc
staretc_DATA = chartpar.dat nsoref.txt starnames.lis


#  Create the direct access data file.
#
chartpar.dat: chartpar.ascii crepar
	-rm -f fort.1
	-rm -f chartpar.dat
	$(LN_S) chartpar.ascii fort.1
	./crepar
	mv fort.2 chartpar.dat
	rm fort.1

#  File for testing
chartpar_test.dat: chartpar_test.ascii crepar
	-rm -f fort.1
	-rm -f chartpar_test.dat
	$(LN_S) chartpar_test.ascii fort.1
	./crepar
	mv fort.2 chartpar_test.dat
	rm fort.1



# The chart_params script needs to have installation locations edited into it
# Can not be done during configure since @bindir@ gets converted
# to ${exec_prefix}/bin

edit = sed \
	-e 's,@staretcdir\@,$(staretcdir),g' \
	-e 's,@CSH\@,$(CSH),g' \
	-e 's,@LN_S\@,$(LN_S),g'

chart_params: chart_params.in
	rm -f chart_params.tmp chart_params
	$(edit) \
	 -e 's,@edited_input\@,chart_params: produced from chart_params.in by Makefile.am,' \
	 $(srcdir)/chart_params.in > chart_params.tmp
	mv chart_params.tmp chart_params
	chmod +x chart_params

chartpar.ascii: chartpar.ascii.in
	rm -f chartpar.tmp chartpar.ascii
	$(edit) \
	 $(srcdir)/chartpar.ascii.in > chartpar.tmp
	mv chartpar.tmp chartpar.ascii
