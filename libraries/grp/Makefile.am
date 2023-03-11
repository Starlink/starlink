## Process this file with automake to produce Makefile.in

cincludedir = $(includedir)/star

lib_LTLIBRARIES = libgrp.la libgrp_adam.la
dist_bin_SCRIPTS = grp_link grp_link_adam

libgrp_la_SOURCES = \
        $(PUBLIC_INCLUDES) \
	$(include_MESSAGES) \
        $(BASIC_C_ROUTINES) \
        $(BASIC_F_ROUTINES) $(STANDALONE_F_ROUTINES) $(BLOCKDATA)

libgrp_adam_la_SOURCES = \
        $(PUBLIC_INCLUDES) \
	$(include_MESSAGES) \
        $(BASIC_C_ROUTINES) \
        $(BASIC_F_ROUTINES) $(ADAM_F_ROUTINES) $(ADAM_C_ROUTINES) $(BLOCKDATA)

include_HEADERS = $(PUBLIC_F_INCLUDES)
cinclude_HEADERS = $(PUBLIC_C_INCLUDES)
include_MESSAGES = GRP_ERR grp_err.h

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@

BUILT_SOURCES = GRP_ERR grp_err.h

#  Define test programs.
TESTS = test1
check_PROGRAMS = test1
test1_SOURCES = grp_test.f
test1_LDADD = libgrp.la `$(srcdir)/grp_link`

PUBLIC_F_INCLUDES = GRP_PAR
PUBLIC_C_INCLUDES = grp.h
PUBLIC_INCLUDES = $(PUBLIC_F_INCLUDES) $(PUBLIC_C_INCLUDES)
PRIVATE_INCLUDES = GRP_CONST GRP_COM grp1.h

BASIC_C_ROUTINES = grp.c

#  List of Fortran routines required by both ADAM and standalone libraries.
BASIC_F_ROUTINES = grp1_conc.f grp1_eledt.f \
  grp1_elems.f grp1_expan.f grp1_expid.f grp1_fchk.f grp1_find.f \
  grp1_fkern.f grp1_getc.f grp1_grapp.f grp1_gsupp.f grp1_gtelm.f \
  grp1_gtind.f grp1_gtslt.f grp1_ihead.f grp1_idele.f grp1_id2sl.f \
  grp1_ilist.f grp1_impid.f grp1_inced.f grp1_ipurg.f grp1_iput.f \
  grp1_kwild.f grp1_listu.f grp1_lunit.f grp1_modif.f grp1_modnm.f \
  grp1_parel.f grp1_ptelm.f grp1_ptind.f grp1_rdind.f grp1_readf.f \
  grp1_setc.f grp1_seti.f grp1_simin.f grp1_slave.f grp1_subst.f \
  grp1_trunc.f grp_copy.f grp_delet.f grp_get.f grp_getcc.f grp_getcs.f \
  grp_grpex.f grp_grpsz.f grp_gtype.f grp_head.f grp_index.f grp_infoc.f \
  grp_infoi.f grp_listf.f grp_msg.f grp_new.f grp_own.f grp_ptype.f \
  grp_purge.f grp_put.f grp_put1.f grp_remov.f grp_setcc.f grp_setcs.f \
  grp_setsz.f grp_sown.f grp_valid.f grp1_index.f grp1_chkcc.f grp1_rmesc.f \
  grp1_vrbtm.f grp_slave.f grp_show.f grp_same.f grp_watch.f grp_alarm.f

#  Additional Fortran routines required for the standalone library.
STANDALONE_F_ROUTINES =

#  Additional Fortran routines required for the ADAM library.
ADAM_F_ROUTINES = grp_group.f grp_list.f grp1_gtfil.f

#  Additional C routines required for the ADAM library.
ADAM_C_ROUTINES = grp_adam.c

#  List of Fortran BLOCK DATA routines.
BLOCKDATA = grp1_init.f

dist_starnews_DATA = grp.news
dist_pkgdata_DATA = LICENCE
dist_noinst_HEADERS = $(PRIVATE_INCLUDES)

EXTRA_DIST = grp_test.dat
