## Process this file with automake to produce Makefile.in

SUBDIRS = @subdirs@

bin_SCRIPTS = $(DYNSCRIPTS)

bin_PROGRAMS = jcmthelp
jcmthelp_SOURCES = helpc.c helpm.c
jcmthelp_LDADD = `hlp_link` `fio_link` `cnf_link` @FCLIBS@

bin_MONOLITHS = jcmtdr

jcmtdr_SOURCES = jcmtdr.f

# libira comes from kaplibs but we do not want all of kaplibs
# Figaro libraries are required: -lfig -ldsa -ldta -lgen -lpar -lich

jcmtdr_LDADD = subs/libjcmtsubs.la applic/libjcmtapplic.la -lira \
`sla_link_adam` `hlp_link_adam` `ndf_link_adam` `prm_link_adam`	 \
`fio_link_adam` `gsd_link` `pda_link_adam`

jcmtdr_TASKS = $(TASKS)

BUILT_SOURCES = jcmtdr.csh jcmtdr.ifc jcmtdr.icl $(IFL_FILES)

TASKS = ae2rd1 ae2rd2 fake iras_tag jcmtextc makemap map2mem map2ts \
	restore ts2map

IFL_FILES = $(TASKS:=.ifl) jcmtdr.ifl

DYNSCRIPTS = jcmtdr.csh jcmtdr.icl

#  Generate ifl and startup scripts from the ifd file and then do any 
#  necessary post processing of the generated files to overcome 
#  deficiencies in ifd2star
$(IFL_FILES) $(DYNSCRIPTS): jcmtdr.ifd
	$(IFD2STAR) jcmtdr

# Help source file
starhelp_DATA = jcmtdr.shl

.hlp.shl:
	@HLIB@ $<

