## Process this file with automake to produce Makefile.in

dist_bin_SCRIPTS = hdstools.csh hdstools.icl

dist_noinst_SCRIPTS = hdstools_link_adam

bin_MONOLITHS = hdstools_mon

hdstools_mon_SOURCES = \
        hdstools_mon.f \
	$(hdstools_mon_TASKS:=.f) \
	$(BLOCK_DATA_FILES) \
	$(FSRC) \
	$(CSRC) \
	$(PRIVATE_INCLUDES)

hdstools_mon_LDADD = $(LDADD) `$(srcdir)/hdstools_link_adam`

hdstools_mon_TASKS = hcopy hcreate hdelete hdir hdisplay hfill hget hhelp \
hmodify hread hrename hreset hreshape hretype htab hwrite

FSRC = aio_assoco.f aio_blnk.f aio_cancl.f aio_close.f aio_ctoc.f \
         aio_fopen.f aio_iwrite.f aio_open.f aio_readf.f \
         aio_setdext.f aio_setpath.f aio_setwrap.f aio_title.f \
         aio_write.f \
         ast_rexit.f \
         ar7_pad.f \
         arr_cop1.f arr_cop1c.f arr_init1.f arr_init1c.f \
         arr_prang1.f arr_sumdim.f \
         hdx_clear.f hdx_copy.f hdx_find.f hdx_numeric.f \
         hdx_sameshape.f hdx_sametype.f hdx_typint.f \
         prs_getslice.f \
         str_abbrev.f str_ctodim.f str_dimtoc.f str_item.f str_obdesc.f \
         str_obval.f str_parse.f

CSRC = aio_appfco_c.c aio_crefco_c.c aio_frefco_c.c \
         util_rename_c.c util_spool_c.c


BLOCK_DATA_FILES = aio_blk.f

PUBLIC_INCLUDES = ADI_PAR AIO_PAR aio.h

PRIVATE_INCLUDES = AIO_CMN


##  The on-line help.
HELP_SOURCES = hdstools.hlp hdstools_master.hlp
starhelp_DATA = $(HELP_SOURCES:.hlp=.shl)

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@

prohlp=	@PROHLP@

hdstools.hlp: $(hdstools_mon_TASKS:=.f) hdstools_master.hlp
	PATH="$(PATH):$(STARLINK)/bin/sst"; \
	export PATH ; \
	for f in $(hdstools_mon_TASKS:=.f); do \
	   $(prohlp) in=$${f} out=$${f}.help ;\
	done
	cat hdstools_master.hlp *.help > hdstools.hlp
	rm $(hdstools_mon_TASKS:=.f.help)

#  Rule for building the help library from the help source.
.hlp.shl:
	@HLIB@ $<

# Rule for creating various components from the ifd file.
hdstools.csh hdstools.icl $(hdstools_mon_TASKS:=.ifl): hdstools.ifd
	@IFD2STAR@ hdstools

##  Things for distribution that are not picked up otherwise.
EXTRA_DIST = $(HELP_SOURCES) $(PRIVATE_INCLUDES) $(PUBLIC_INCLUDES) htest.in

##  Need to add generated files as BUILT_SOURCES.
BUILT_SOURCES = hdstools.csh hdstools.icl hdstools_mon.ifl $(hdstools_mon_TASKS:=.ifl)

##  Simple check.
edit = sed \
       -e 's,@bindir\@,$(bindir),g'

TESTS = htest

htest: htest.in
	-rm -f htest.tmp htest
	$(edit) \
	$(srcdir)/htest.in >htest.tmp
	mv htest.tmp htest
	chmod 0755 htest

CLEANFILES = hdstools.hlp htest hdstools.shl hdelete.ifc hdir.ifc \
             hdisplay.ifc hfill.ifc hget.ifc hhelp.ifc hmodify.ifc \
             hread.ifc hrename.ifc hreset.ifc hreshape.ifc hretype.ifc \
             htab.ifc hwrite.ifc version.dat hcopy.ifc hcreate.ifc \
             hdstools_mon dtask_applic.lo hdstools_mon.ifc hcopy.ifl \
             hcreate.ifl hdelete.ifl hdir.ifl hdisplay.ifl \
             hdstools.csh hdstools.icl hdstools_mon.ifl hdstools.sh \
             hfill.ifl hget.ifl hhelp.ifl hmodify.ifl hread.ifl \
             hrename.ifl hreset.ifl hreshape.ifl hretype.ifl htab.ifl \
             hwrite.ifl hdstools_master.shl

dist_starnews_DATA = hdstools.news
