## Process this file with automake to produce Makefile.in

bin_SCRIPTS = parsecon_link_adam
bin_PROGRAMS = compifl
lib_LTLIBRARIES = libparsecon_adam.la

libparsecon_adam_la_SOURCES = \
	$(F_ROUTINES) \
	$(FPP_ROUTINES) \
	$(PUBLIC_INCLUDES) \
	$(PRIVATE_INCLUDES)

compifl_SOURCES = \
	compifl.f

# This must be compifl_LDFLAGS or AM_LDFLAGS, but _not_ plain LDFLAGS,
# since that would override a configure variable.
compifl_LDFLAGS = `$(bindir)/chr_link_adam` \
	`$(bindir)/ems_link_adam` \
	`$(bindir)/subpar_link_adam` \
	`./parsecon_link_adam`
##LIBS = `$(bindir)/chr_link_adam` `$(bindir)/ems_link_adam` `$(bindir)/subpar_link_adam`
##LDFLAGS = `./parsecon_link_adam`

## List headers which are to be installed -- these are built first if necessary
include_HEADERS = $(PUBLIC_INCLUDES)

PUBLIC_INCLUDES = PARSECON_ERR PARSECON_PAR

PRIVATE_INCLUDES = PARSECON_CMN PARSECON2_CMN PARSECON3_CMN PARSECON4_CMN 

FPP_ROUTINES = \
	pcn_openifl.fpp

F_ROUTINES = pcn_actend.f \
	pcn_actlist.f \
	pcn_actres.f \
	pcn_arrchar.f \
	pcn_convert.f \
	pcn_creatifc.f \
	pcn_creq.f \
	pcn_decval.f \
	pcn_dumpifc.f \
	pcn_erfl.f \
	pcn_error.f \
	pcn_facend.f \
	pcn_findact.f \
	pcn_findpar.f \
	pcn_gettok.f \
	pcn_gpbtok.f \
	pcn_interp.f \
	pcn_mestext.f \
	pcn_newacrds.f \
	pcn_newact.f \
	pcn_newpar.f \
	pcn_newpcrds.f \
	pcn_oreq.f \
	pcn_packc.f \
	pcn_packi.f \
	pcn_packl.f \
	pcn_paknb.f \
	pcn_pakni.f \
	pcn_parend.f \
	pcn_parlist.f \
	pcn_ptrinit.f \
	pcn_readifl.f \
	pcn_reset.f \
	pcn_runact.f \
	pcn_setacc.f \
	pcn_setacrds.f \
	pcn_setahel.f \
	pcn_setakey.f \
	pcn_setamenu.f \
	pcn_setass.f \
	pcn_setcan.f \
	pcn_setdef.f \
	pcn_setepath.f \
	pcn_setface.f \
	pcn_sethel.f \
	pcn_sethkey.f \
	pcn_sethlib.f \
	pcn_setkey.f \
	pcn_setmon.f \
	pcn_setob.f \
	pcn_setpcrds.f \
	pcn_setpmenu.f \
	pcn_setpos.f \
	pcn_setpp.f \
	pcn_setprog.f \
	pcn_setprom.f \
	pcn_setpty.f \
	pcn_setres.f \
	pcn_settyp.f \
	pcn_setvp.f \
	pcn_stface.f \
	pcn_tabent.f \
	pcn_tabinit.f \
	pcn_tabset.f \
	pcn_tabstart.f \
	pcn_toktyp.f \
	pcn_upkc.f \
	pcn_upki.f \
	pcn_upkl.f \
	pcn_upknb.f \
	pcn_upkni.f

# For some reason, the error file that is included and installed is
# not the PARSE_ERR file that is generated, but PARSECON_ERR.  So
# create the latter from the former:
PARSECON_ERR: PARSE_ERR
	cp PARSE_ERR PARSECON_ERR

BUILT_SOURCES = PARSECON_ERR
