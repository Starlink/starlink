
SUBDIRS = @subdirs@

#  Things to build.

ccdsupportdir = $(bindir)

ccdsupport_DATA = $(DATA_FILES) ccdpack.shl

bin_SCRIPTS = $(PUBLIC_SCR)

bin_PROGRAMS = ccdwish

ccdwish_SOURCES = ccdwish.c grf_ccdpack.c grf.h

ccdwish_LDADD = $(LDADD) $(CCDPACK_LDADD)

bin_MONOLITHS = ccdpack_red ccdpack_reg ccdpack_res ccdpack_scr \
                ccdhelp

starhelp_DATA = ccdpack.shl

dist_pkgdata_DATA = ccdpack.news

ccdpack_red_SOURCES = ccdpack_red.f
ccdpack_reg_SOURCES = ccdpack_reg.f
ccdpack_res_SOURCES = ccdpack_res.f
ccdpack_scr_SOURCES = ccdpack_scr.f
ccdhelp_SOURCES = ccdhelp.f

ccdpack_red_TASKS = $(REDTASKS)
ccdpack_reg_TASKS = $(REGTASKS)
ccdpack_res_TASKS = $(RESTASKS)
ccdpack_scr_TASKS = $(SCRTASKS)
ccdhelp_TASKS = ccdhelp

#  Link flags.
CCDPACK_LIBS = tasks/libccdpack_tasks.la \
               gen/libccdpack_gen.la \
               main/libccdpack.la

CCDPACK_LDADD = $(CCDPACK_LIBS) `$(srcdir)/ccdpack_link_adam`
CCDPACK_LDADD_PGP = $(CCDPACK_LIBS) `$(srcdir)/ccdpack_link_adam-pgp`

ccdpack_red_LDADD = $(LDADD) $(CCDPACK_LDADD)
ccdpack_reg_LDADD = $(LDADD) $(CCDPACK_LDADD_PGP)
ccdpack_res_LDADD = $(LDADD) $(CCDPACK_LDADD_PGP)
ccdpack_scr_LDADD = $(LDADD) $(CCDPACK_LDADD)
ccdhelp_LDADD = $(LDADD) help/libccdpack_help.la

#  List of applications/tasks in monoliths.

REDTASKS = makebias debias makecal calcor makeflat flatcor
REGTASKS = findcent idicurs findobj register tranlist plotlist findoff \
 pairndf ccdedit makemos tranndf drizzle astimp astexp wcsreg wcsedit drawndf
RESTASKS = ccdsetup ccdclear ccdnote ccdshow ccdndfac ccdgenerate \
 picinfo import present schedule ccdimp ccdexp makeset showset
SCRTASKS = reduce ccdalign xreduce

#  Startup scripts.  These are the files that must be executed (sourced)
#  by a user of this package in order to define appropriate aliases
#  and environment variables.  They are listed separately from the public
#  scripts as they are edited by the installation procedure.

STARTUP_SCRIPT = ccdpack.csh
IRAF_STARTUP_SCRIPT = ccdpack.cl
ICL_STARTUP_SCRIPT = ccdpack.icl



#  List of public script files.  These are scripts which form part of
#  the package and will be required by users of it.  They will be
#  installed in the $(INSTALL_BIN) directory with execute permission
#  set. 

PUBLIC_SCR = $(SHELL_SCR) $(ICL_SCR) $(CONFIG_SCR)

SHELL_SCR = ccdfork ccdexercise wcsexercise setexercise \
 ccdpack_back xreduce \
 ccdexecute ccdwww geometry ccdpack_ex1.csh ccdpack_ex2.csh \
 ccdpack_ex3.csh ccdpack_ex4.csh ccdpack_ex5.csh ccdpack_ex6.csh \
 ccdpack_ex7.csh ccdpack_ex8.csh glitch2ard filemonitor irg_wild \
 ccdimp.csh ccdexp.csh 

ICL_SCR = ccdexercise.icl ccdfork.icl

CONFIG_SCR = DATASEC.DAT EEV3QUICK.DAT EEV3STANDARD.DAT EEV5QUICK.DAT \
 EEV5STANDARD.DAT EEV6QUICK.DAT EEV6STANDARD.DAT EEV7QUICK.DAT \
 EEV7STANDARD.DAT GEC7QUICK.DAT GEC7STANDARD.DAT TEK1QUICK.DAT \
 TEK1STANDARD.DAT TEK2QUICK.DAT TEK2STANDARD.DAT TEK3QUICK.DAT \
 TEK3STANDARD.DAT TEK4QUICK.DAT TEK4STANDARD.DAT TEK5QUICK.DAT \
 TEK5STANDARD.DAT WHTFLAT.DAT WHTSKY.DAT \
 INTWIDEFLAT.DAT  INTWIDESKY.DAT  WHT2000FLAT.DAT  WHT2000SKY.DAT \
 INT-WFC.ast ccdpack_style.def

DATA_FILES = ccdtest.ard ccdtest_obj.dat ccdtest2_obj.dat ccdtest.ast \
 c1.xbm c2.xbm c3.xbm c4.xbm c5.xbm c6.xbm c7.xbm c8.xbm \
 arrow_left.xbm arrow_right.xbm \
 ccdbitmap ccdbitmap64 import.table export.table

IFD_FILES = ccdpack.ifd $(REDTASKS:=.ifd) $(REGTASKS:=.ifd) \
            $(RESTASKS:=.ifd) $(SCRTASKS:=.ifd) \
            ccdhelp.ifd


#  Rules.

ccdpack.shl: ccdpack.hlp
	$(HLIB) ccdpack.hlp

#  Rule for generating the ADAM interface files and startup scripts from
#  the IFD files.

$(IFL_FILES) $(STARTUP_SCRIPT) $(ICL_STARTUP_SCRIPT): $(IFD_FILES)
	$(IFD2STAR) ccdpack

#  List of task interface files.

IFL_FILES = $(REDTASKS:=.ifl) $(REGTASKS:=.ifl) $(RESTASKS:=.ifl) \
            $(SCRTASKS:=.ifl)




