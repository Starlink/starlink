## Process this file with automake to produce Makefile.in

SUBDIRS = subs applic docs

bin_SCRIPTS = $(DYNSCRIPTS)

bin_PROGRAMS = jcmt_help
jcmt_help_SOURCES = helpm.c
jcmt_help_LDADD = `shl_link` @FCLIBS@

bin_MONOLITHS = jcmtdr

jcmtdr_SOURCES = jcmtdr.f

pkgdata_DATA = jcmtdr.news JCMTDR_CONDITIONS

# libira comes from kaplibs but we do not want all of kaplibs
# Figaro libraries are required: -lfig -ldsa -ldta -lgen -lpar -lich

FIG_LIBS = -ldsa -ldta -lfigpar -lfigich -lfiggen -lfig

jcmtdr_LDADD = applic/libjcmtapplic.la subs/libjcmtsubs.la -lira \
$(FIG_LIBS) \
`sla_link_adam` `hlp_link_adam` `ndf_link_adam` `prm_link_adam`	 \
`fio_link_adam` `gsd_link` `pda_link_adam`

jcmtdr_TASKS = $(TASKS)

BUILT_SOURCES = jcmtdr.sh jcmtdr.csh jcmtdr.icl $(IFL_FILES)

TASKS = ae2rd1 ae2rd2 fake iras_tag jcmtextc makemap map2mem map2ts \
	restore ts2map

IFL_FILES = $(TASKS:=.ifl) jcmtdr.ifl

DYNSCRIPTS = jcmtdr.csh jcmtdr.icl jcmtdr.sh

#  Generate ifl and startup scripts from the ifd file and then do any 
#  necessary post processing of the generated files to overcome 
#  deficiencies in ifd2star
$(IFL_FILES) $(DYNSCRIPTS): jcmtdr.ifd
	$(IFD2STAR) jcmtdr

# Help source file
starhelp_DATA = jcmtdr.shl

.hlp.shl:
	@HLIB@ $<

