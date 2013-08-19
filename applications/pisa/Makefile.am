## Process this file with automake to produce Makefile.in

dist_bin_SCRIPTS = pisa_demo pisa_demo.icl pisa_proc.icl \
 pisahelp.ifc pisawww $(DYNSCRIPTS)

dist_noinst_SCRIPTS = pisa_link_adam

bin_MONOLITHS = pisa_mon pisa_mon4 pisahelp

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@ sun109_cover.eps

DYNSCRIPTS = pisa.sh pisa.csh pisa.icl

IFL_FILES = $(pisa_mon_TASKS:=.ifl) pisa_mon.ifl \
	$(pisa_mon4_TASKS:=.ifl) pisa_mon4.ifl \
	pisahelp.ifl

BUILT_SOURCES = $(DYNSCRIPTS) $(IFL_FILES)

pisa_mon_SOURCES = \
        pisa_mon.f \
	$(pisa_mon_TASKS:=.f) \
	$(SUBSRC) \
	$(PRIVATE_INCLUDES)

pisa_mon_LDADD = $(LDADD) `$(srcdir)/pisa_link_adam`

pisa_mon_TASKS = addnoise pisa2ard pisacut pisafind pisafit pisagen \
 pisagrey pisaknn pisamatch pisapeak pisaplot pisa2cat

pisa_mon4_SOURCES = \
        pisa_mon4.f \
	$(pisa_mon4_TASKS:=.f) \
	$(SUBSRC) \
	$(PRIVATE_INCLUDES)

pisa_mon4_LDADD = $(LDADD) `$(srcdir)/pisa_link_adam`

pisa_mon4_TASKS = pisafind4 pisafit4

##  Link the pisahelp program as an atask. 
pisahelp_SOURCES = pisahelp.f
pisahelp_LDADD = `shl_link_adam`
# no variable pisahelp_TASKS -- pisahelp is an ATASK


SUBSRC = addmod.f func.f analys.f anoise.f checkp.f dchole.f \
         ellpse.f extend.f featur.f fitplo.f gtmlim.f \
         histat.f median.f overlp.f parbol.f peakn.f pgextr.f \
         pgintr.f pgvclr.f phopt.f phopt2.f polynm.f pqsort.f psa1_clwo.f \
         psa1_exss.f psa1_mkhis.f psa1_mnmax.f psa1_ndfin.f pxyget.f \
         pxygt2.f ranorm.f raprof.f rdpifd.f rdpiin.f \
         rdpipd.f rdpipe.f rdpisd.f scllim.f solve.f sorti3.f sortin.f \
         sortm.f super.f update.f psa1_asfio.f \
         psa1_rminv.f \
         analys4.f overlp4.f psa1_mkhii.f sortm4.f extend4.f \
         phopt24.f psa1_clwo4.f psa1_ndfin4.f super4.f featur4.f \
         phopt4.f  psa1_exss4.f sorti34.f median4.f psa1_iminv.f \
         sortin4.f pqsort4.f psa1_ccatf.f psa1_ccats.f psa1_ccatp.f \
         dchole4.f psa1_fitfn.f

PRIVATE_INCLUDES = PSA1_PAR hlpcmd.inc

##  The on-line help.
HELP_SOURCES = pisahelp.hlp
starhelp_DATA = $(HELP_SOURCES:.hlp=.shl)

#  Rule for building the help library from the help source.
.hlp.shl:
	@HLIB@ $<

##  Additional files. These have traditionally gone into bindir. 
##  Not having frame.sdf there would be an issue. Use a trick to get these
##  installed. 

pisasupportdir = $(bindir)
pisasupport_DATA = frame.sdf frame_stars.acc clustan.dat findinfo.dat \
 sizeinfo.dat peakinfo.dat

##  PISA news file.
dist_starnews_DATA = pisa.news

##  Things for distribution that are not picked up otherwise.
EXTRA_DIST = $(pisasupport_DATA) $(HELP_SOURCES) sun109_cover.eps

CLEANFILES = addnoise.ifc pisa2ard.ifc pisa2cat.ifc pisacut.ifc \
             pisafind4.ifc pisafind.ifc pisafit4.ifc pisafit.ifc \
             pisagen.ifc pisagrey.ifc pisahelp pisahelp.shl \
             pisaknn.ifc pisamatch.ifc pisapeak.ifc pisaplot.ifc \
             version.dat pisa_mon4 pisa_mon4.ifc pisa_mon4.ifl \
             pisa_mon pisa_mon.ifc pisa_mon.ifl

# Generate IFL files
$(IFL_FILES) $(DYNSCRIPTS): pisa.ifd
	$(IFD2STAR) pisa