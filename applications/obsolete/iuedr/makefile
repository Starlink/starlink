#+
#  Name:
#     makefile
#
#  Version:
#     Application makefile Mk V
#
#  Purpose:
#     Build and install the IUEDR package.
#
#  Type of Module:
#     Description file for the make utility.
#
#  Description:
#     This description file is used by the make utility to build the
#     IUEDR package from the distributed source files, to install the
#     resulting system for use, and to perform other housekeeping
#     tasks.
#
#  Invocation:
#     This makefile is not intended to be used by make when invoked
#     directly (although this is possible), but instead to be used via
#     the accompanying mk script.  This script sets up a number of
#     environment variables which are used as macros within the
#     makefile and which accommodate differences between machines and
#     operating systems (it invokes make with the -e option).  Please
#     consult the mk script prologue for full details.
#
#  Targets:
#     The following make targets are defined in this script for
#     external use:
#
#        [help]
#           This is the default target.  It outputs a message describing
#           the mk script and lists the targets provided.
#
#        check
#           Performs a simple check that all necessary source files are
#           present, and displays the version number and current state
#           of the package (built/installed/tested, etc.).
#
#        build
#           Compiles the source files and creates all files needed
#           prior to installing the package for use.
#
#        install
#           Installs the package for use by putting the necessary files
#           into sub-directories of the $INSTALL directory (the $HOME
#           directory is used if the environment variable INSTALL is
#           not defined).  Links to the installed files are left in the
#           source directory.
#
#        deinstall
#           Reverses the action of the install target, removing files
#           from sub-directories of the $INSTALL directory and
#           restoring them to the source directory (the $HOME directory
#           is used by default if the environment variable INSTALL is
#           not defined).
#
#        test
#           Builds and runs a simple test program to check for correct
#           installation of the package.
#
#        export
#           Produces an export copy of the built package suitable for
#           passing to another user.  A compressed tar file is created
#           in the $EXPORT directory containing copies of the source
#           files and built files for the package (the current
#           directory is used by default if the environment variable
#           EXPORT is not defined).  The package should normally be
#           built, installed and tested (see above) before using this
#           target.  After unpacking the exported file on a similar
#           machine, the recipient may simply install it for use.
#
#        export_run
#           Produces an export copy of the built package suitable for
#           passing to another user.  A compressed tar file is created
#           in the $EXPORT directory containing copies of the built
#           files for the package (the current directory is used by
#           default if the environment variable EXPORT is not defined).
#           The package should normally be built, installed and tested
#           (see above) before using this target.  After unpacking the
#           exported file on a similar machine, the recipient may simply
#           install it for use.
#
#        export_source
#           Produces an export copy of the source for the package
#           suitable for passing to another user to build (possibly on
#           a different type of machine).  A compressed tar file is
#           created in the $EXPORT directory containing copies of just
#           the source files for the package (the current directory is
#           used by default if the environment variable EXPORT is not
#           defined).  After unpacking the exported file, the recipient
#           must build the package before installing it for use.
#
#        clean
#           Cleans up after building the package, removing all
#           intermediate files created during the building process, but
#           leaving the built files themselves.
#
#        unbuild
#           Reverses the building process, removing all intermediate
#           files along with all the built files.
#
#        strip
#           This target `strips' the executable files.  This can lead
#           to significant reduction in the sizes of binaries.

#  External Dependencies:
#     The IUEDR package depends on the following other Starlink
#     packages which must previously have been installed into the
#     appropriate sub-directories of the $STARLINK directory (/star is
#     used if the environment variable STARLINK is not defined).
#        chr
#           Character handling routines
#        ems
#           Error message service
#        hds
#           Hierarchical data system
#        ndf
#           N-d data system
#        hlp
#           Help system
#        fio
#           File I/O system
#        sae_par
#           Global include files

#  Notes:
#     This makefile uses the presence/absence of the hidden files
#     .BUILT, .INSTALLED_$(SYSTEM) and .TESTED_$(SYSTEM) to record the
#     current state of the system during housekeeping operations.

#  Implementation Deficiencies:
#
#  Copyright:
#     Copyright (C) 1995 Rutherford Appleton Laboratory
#
#  Authors:
#     RFWS: R.F. Warren-Smith (Starlink, RAL)
#     PMA: P.M. Allan (Starlink, RAL)
#     PTW: P.T. Wallace (Starlink, RAL)
#     MJC: M.J. Clayton (Starlink, UCL)
#     BLY: M.J. Bly (Starlink, RAL)
#     {enter_new_authors_here}

#  History:
#     4-JAN-1993 (RFWS):
#        Original version.
#     15-MAR-1993 (RFWS):
#        Adapted for use on sun4_Solaris.
#     11-JUN-1993 (PMA):
#        Added parts for handling platform specific files.
#     27-SEP-1993 (PMA):
#        Added the target "filler1" to prevent the list of platform
#        specific files being empty, thereby causing a make syntax error.
#     3-NOV-1993 (PMA):
#        Change the check target to print out for which system the
#        software has been built.
#     3-NOV-1993 (PMA):
#        Add a test to check that files really are extracted from the
#        tar file.
#     18-NOV-1993 (PTW):
#        Cosmetics.
#     26-NOV-1993 (PMA):
#        Remove the dummy file "filler1" and add a dummy target called
#        dummy_target1.
#        Add a file called " " to all of the for loops that install and
#        deinstall files to prevent syntax errors if any of the relevent
#        macros are null.
#        Add a macro STARTUP_SCRIPT that is the name of the startup
#        script.
#     4-JAN-1994 (PMA):
#        Add some missing semicolons to for loops. This seems not to
#        cause errors on our systems, but it is a syntax error.
#     30-MAY-1995 (MJC):
#        Various tidying up.
#     26-AUG-1997 (BLY):
#        Added CFLINK, CFLIBS macros for linking HELPTASK.
#        Version 3.2-1.
#     {enter_further_changes_here}
#
#  Bugs:
#     {note_any_bugs_here}
#
#-------------------------------------------------------------------------------

#  Help target.
#  ===========
#
#  This is the default target, so it appears first.

#  Display information about the mk script and the make targets.

help:
	@ echo \
   '   The makefile provided is intended to be used by the make utility when';\
        echo \
   '   invoked via the associated mk script.  This script defines environment';\
        echo \
   '   variables which are used by make to accommodate differing machine and';\
        echo \
   '   operating system characteristics.  Please see the mk script prologue';\
        echo \
   '   for full details.';\
        echo;\
        echo \
   '   The following targets are provided:';\
        echo;\
        echo \
   '      help          - Display this message';\
        echo \
   '      check         - Check source file presence and show current state';\
        echo \
   '      build         - Build the package from source';\
        echo \
   '      install       - Install the built package for use';\
        echo \
   '      deinstall     - Deinstall the package';\
        echo \
   '      test          - Perform a quick test of the installation';\
        echo \
   '      export        - Make a compressed tar file for exporting the'\
   'built package';\
	echo \
   '                      complete with source and documentation';\
        echo \
   '      export_run    - Make a compressed tar file for exporting the'\
   'built package';\
        echo \
   '                      with documentation but no source';\
        echo \
   '      export_source - Make a compressed tar file for exporting the'\
   'source files';\
        echo \
   '      clean         - Tidy up after building the package';\
        echo \
   '      unbuild       - Remove all the built files';\
        echo \
   '      strip         - Strip the binary files (executables)';\
        echo;\
        echo \
   '   To build and install the $(PACK_NAME) package on a supported system:';\
        echo;\
        echo \
   '      mk build; mk install; mk test; mk clean';\
        echo

#-------------------------------------------------------------------------------

#  Defaults.
#  ========
#
#  This section defines default macros and should rarely need changing.
#  The values given here should be overridden externally to adapt to
#  the local system setup (either use the mk script or use environment
#  variables and invoke "make" with the "-e" option).

#  Name of computer hardware/OS combination.

SYSTEM = unknown

#  Name used to distinguish platform-specific source files.

SOURCE_VARIANT = $(SYSTEM)

#  Pathname of the root directory beneath which other Starlink software
#  is currently installed.

STARLINK = /star

#  Pathnames of Starlink sub-directories that may be referenced when
#  building this package.

STAR_BIN = $(STARLINK)/bin
STAR_DATES = $(STARLINK)/dates
STAR_DOCS = $(STARLINK)/docs
STAR_ETC = $(STARLINK)/etc
STAR_HELP = $(STARLINK)/help
STAR_INC = $(STARLINK)/include
STAR_LIB = $(STARLINK)/lib
STAR_SHARE1 = $(STARLINK)/share

#  Pathname of the root directory beneath which the built files for
#  this package should be installed for use.  This defaults to the
#  user's home directory.

INSTALL = $(HOME)

#  Pathname of the directory into which exported tar files will be
#  placed.  This defaults to the current working directory.

EXPORT = .

#  Default macros for compiling C and Fortran source code.

CC = c89
CFLAGS = -O
FC = fort77
FFLAGS = -O

#  Command for forming a link to a file.

LINK = ln

#  Command for "randomizing" an object library.  The default acts as a
#  null command.

RANLIB = :

#  Commands for adding to and extracting from an archive file (.tar).

TAR_IN = pax -w -v -x ustar -f
TAR_OUT = pax -r -f

#  Command for adding a file to an object archive (.a).

AR_IN = ar -r

#  ADAM monolith linker command.

ALINK = $(STAR_BIN)/alink

#  Linker flags.  These are placed either side of the library list
#  and are used adding system-dependent linker flags to link commands.

LDFLAG1 =
LDFLAG2 =

#  Linker command to link C main programs with Fortran subroutines and
#  libraries (and C functions).
 
CFLINK = $(FC) -O
 
#  Extra link libraries for CFLINK.
 
CFLIBS =

#-------------------------------------------------------------------------------
################################################################################
#
#  Define package source files.
#  ===========================
#
#  This section defines the set of source files for the package.

#  Name of the package as specified in documentation
#  The value is used in messages from make to the user.

PACK_NAME = IUEDR

#  Prefix for the package in lower-case as used in filenames etc.

PKG_NAME = iuedr

#  Prefix for the package in upper-case as used in include file links.

PKG_LINK = IUEDR

#  Version number (as in the documentation - i.e. not the same thing
#  as the shared library version number).
#
#  The major component of the version number (before the dot) should
#  normally only be incremented following major changes to the package.
#  The minor version number (after the dot) is the number normally
#  incremented following development which introduces new documented
#  functionality.  Any revision number (appended after a dash) should
#  be incremented for other minor changes (bug fixes, etc.) which do
#  not merit documentation changes.

PKG_VERS = 3.2-4

#  List of files comprising the distributed source-only system.  This
#  defines the minimum set of files required to completely rebuild the
#  package from source (including this makefile, the associated mk
#  script and any documentation files).  It is advisable to use several
#  smaller source archives in place of one large one to speed up build
#  times.

SOURCE_FILES = $(PKG_NAME)_source.tar makefile mk $(DOCUMENTATION) \
$(ARCHIVES)

ARCHIVES = $(DEMO_ARCHIVE) $(DATA_ARCHIVE) $(USER_ARCHIVE) $(DOC_ARCHIVE) \
$(SG7_ARCHIVE) $(HYPERTEXT_ARCHIVE) $(PKG_NAME)_inc.tar

DEMO_ARCHIVE = $(PKG_NAME)_demo.tar
DATA_ARCHIVE = $(PKG_NAME)_data.tar
USER_ARCHIVE = $(PKG_NAME)_user.tar
DOC_ARCHIVE = $(PKG_NAME)_doc.tar
SG7_ARCHIVE = $(PKG_NAME)_sg7.tar
HYPERTEXT_ARCHIVE = $(PKG_NAME)_hyper.tar

#  List of public script files.  These are scripts which form part of
#  the package and will be required by users of it.  They will be
#  installed in the $(INSTALL_BIN) directory with execute permission
#  set.

PUBLIC_SCRIPTS = iuecnv.com iuecnv.exe iuecnv.for

#  Startup script.  This is the file that must be executed by a
#  programmer using this package in order to define links to include
#  files.  It is listed separately from the public scripts as it is
#  edited by the installation procedure.

STARTUP_SCRIPT = iuedr3.csh

#  List of public include files.  These are include files
#  which form part of the package and are required in order to build
#  it, but which are not required by users of it.
#  They are nevertheless commonly used in user-written applications and
#  should therefore remain in the built system and safe against
#  cleaning.

PUBLIC_INCLUDES =

#  List of private include files.  These are additional include files
#  which form part of the package and are required in order to build
#  it, but which are not required by users of it.

PRIVATE_INCLUDES = CMABS CMBIN CMBKG CMBKGS CMBUG CMCAL CMCEN CMCMD CMCOLR \
   CMCOMB CMCPV CMCUT CMDATA CMDEV CMDEVS CMDISH CMDISP CMDYN CMECOR CMERR \
   CMEXTP CMFACE CMFIDS CMFIDT CMFILE CMFLX CMGEOM CMGRAF CMHAL CMHEAD CMIDEV \
   CMISAF CMITFC CMLBLS CMLINE CMLINR CMMUST CMNDF CMNEED CMNET CMPAL CMPAN \
   CMPLOT CMPRT CMPSX CMRIP CMROTR CMSAVE CMSCAN CMSIST CMSPC CMSPEC CMTAPE \
   CMTEM CMTEMP CMTRUN CMUEZ1 CMVEL CMWAV CMWCOR CMDSB

#  List of external include files.  These are files which are required
#  in order to build the package but form part of other, externally
#  installed packages.  This list should contain the names used to
#  reference the files within the source code, not the actual names of
#  the files.

EXTERNAL_INCLUDES = CHR_ERR DAT_PAR MAG_ERR MSG_ERR MSG_PAR PAR_ERR PRM_PAR \
   SAE_PAR

#  List of Fortran routines required for building the package.  This is
#  just a list of all the Fortran source files (excluding BLOCK DATA
#  routines, which are treated separately).

F_ROUTINES = abscal.f aladr.f alcut.f alrip.f altem.f \
   brk_chk.f brk_drk.f brk_flxmean.f brk_kcorr.f brk_merc.f brk_ovlap.f \
   brk_ripval.f brk_rpc.f brk_rplc.f brk_sm.f brk_woof.f \
   cagrid.f cahi.f calbls.f calgen.f calo.f cawav.f chr_etom.f iue_clean.f \
   clfile.f closdr.f cmd_err.f cmd_hlp.f cncomb.f cndata.f cndsn.f \
   cnlbls.f cnord.f cnpar.f cnscan.f cnspec.f crdata.f crspec.f \
   dassoc.f defapr.f defext.f defpan.f defrip.f desubs.f dladr.f dlcut.f \
   dlrip.f dmore.f dq_and.f dq_copy.f dq_find.f dq_itou.f dq_pack.f \
   dq_rdpk.f dq_unpk.f dq_utoi.f dq_wrpk.f dq_zero.f \
   editim.f editlo.f errint.f errout.f errpar.f errset.f errstr.f exbkg.f \
   exlbls.f exobj.f expan.f exspec.f exsubs.f extrak.f \
   face.f fnaper.f fncut.f fnord.f fnrip.f fntem.f frdsn.f frqual.f frsubs.f \
   gen_ctos.f gen_ftoe.f gen_ftos.f gen_htoi.f \
   gen_itoh.f gen_itoo.f gen_itos.f gen_otoi.f gen_rev.f gen_stoc.f \
   gen_stof.f gen_stoi.f gencal.f geomf.f get_orders.f get_sptype.f \
   getrip.f gphot.f \
   gphotq.f grf_axes.f grf_clzone.f grf_curs.f grf_cuzone.f grf_deaxes.f \
   grf_drqual.f grf_fnline.f grf_grsymb.f grf_hicurv.f grf_lutcs.f \
   grf_lutg.f grf_opcurs.f grf_opidev.f grf_opzone.f grf_pocurv.f \
   grf_ppalet.f grf_qpalet.f grf_reline.f grf_rscolr.f grf_rsgraf.f \
   grf_rsline.f grf_rslut.f grf_seline.f grf_tzone.f grf_vqlim.f \
   grf_xind.f grf_xyind.f grf_xylim.f grf_zinit.f gtor.f gtow.f \
   halcal.f hicomb.f higrid.f hirad.f hislcu.f hislit.f hitem.f \
   hlp_nametr.f  \
   initdr.f invoke.f itfcor.f iulog.f iue_aprn.f iue_camn.f iue_device.f  \
   iue_flan.f iue_resn.f iue_tair.f iue_tvac.f iue_velo.f iuedr3.f \
   line_get.f line_wcont.f line_writf.f line_writi.f line_writl.f \
   line_writs.f locomb.f logrid.f lorad.f loset.f loslcu.f loslit.f \
   lotem.f lwritf.f \
   maspec.f mebkg.f mequal.f mkfids.f modcal.f modim.f modmap.f modued.f \
   modueq.f mrdata.f msc_chef.f msc_chep.f msc_chev.f msc_date.f \
   msc_dayn.f msc_evpoly.f msc_fit1d3.f msc_grpog.f msc_grsub.f \
   msc_iswap.f msc_lured.f msc_luslv.f msc_map.f msc_map1d4.f msc_mesub.f \
   msc_posub.f msc_reslv.f \
   msc_rswap.f msc_xmonot.f mt_fix.f mt_open.f \
   mvdisp.f mvfidt.f mwdata.f mwspec.f mwsubs.f \
   newapr.f \
   orset.f outpan.f outrk.f outspc.f \
   panhi.f panlo.f parfer.f pariue.f parspc.f photm.f photq.f pickhi.f \
   picklo.f pideax.f pideim.f pidri2.f pipar.f plist.f prabs.f prdisp.f \
   prext.f prextp.f prfid.f prgeom.f prgrs.f prhal.f primg.f prmap.f \
   prpan.f prpar.f prpix.f prqkey.f prrip.f \
   prspc.f prspec.f prtbuf.f \
   prtchr.f prteol.f prtlin.f prtset.f prxyq.f \
   raw.f rawp.f rdabs.f rdcal2.f rdpart2.f rdcal.f rdcomb.f rdcut.f \
   rddat.f rddisp.f rdface.f rdfids.f rdfile.f rdhi.f rdimg.f rdlo.f \
   rdmap.f rdmesp.f rdord.f rdparc.f rdparf.f rdpari.f rdparl.f rdpart.f \
   rdqual.f rdrip.f rdspar.f rdspc.f rdspec.f rdtem.f read_ndf.f rebkg.f \
   recal.f rfabs.f rfcut.f rfdisp.f rfface.f rffid.f rfgeom.f rfrip.f \
   rftem.f ripcal.f rtog.f rtou.f rtow.f \
   sacen.f scanhi.f scanlo.f sdbkg.f secen.f setdh.f setdr.f setuv.f \
   slculm.f slcurs.f str_add.f str_appnd.f str_clean.f \
   str_copy.f str_decf.f str_decp.f \
   str_desc.f str_desf.f str_desh.f str_desi.f str_deso.f \
   str_equal.f str_feat.f str_gtok.f str_index.f str_isalf.f str_isdig.f \
   str_just.f str_lcase.f str_len.f str_lower.f str_move.f str_plant.f \
   str_prf.f str_prh.f str_pri.f str_pro.f str_prq.f str_prs.f str_pwf.f \
   str_pwi.f str_readf.f str_readi.f str_readl.f str_reads.f str_rmblk.f \
   str_rpos.f str_simlr.f str_simlr2.f str_size.f str_teat.f str_term.f \
   str_type.f \
   str_ucase.f str_upper.f str_wcont.f str_wit1.f str_wpos.f str_writf.f \
   str_writi.f str_writl.f str_writs.f swpitf.f sztype.f \
   tape_file.f tape_find.f tape_shop.f trakhi.f traklo.f trifld.f \
   unbkg.f usr_aeshift.f usr_agshift.f usr_barker.f usr_cgshift.f \
   usr_clean.f usr_cmdread.f usr_culimits.f usr_cursor.f \
   usr_drimage.f usr_edimage.f usr_edmean.f usr_edspec.f usr_erase.f \
   usr_help.f usr_lbls.f usr_listiue.f usr_map.f \
   usr_modimage.f usr_mtmove.f usr_mtrew.f usr_mtshow.f usr_mtskipeov.f \
   usr_mtskipf.f usr_newabs.f usr_newcut.f usr_newdisp.f usr_newfid.f \
   usr_newrip.f usr_newtem.f usr_outem.f usr_outlbls.f usr_outmean.f \
   usr_outnet.f usr_outrak.f usr_outscan.f usr_outspec.f usr_plcen.f \
   usr_plflux.f usr_plgrs.f usr_plmean.f usr_plnet.f usr_plscan.f \
   usr_prgrs.f \
   usr_prlbls.f usr_prmean.f usr_prscan.f usr_prspec.f \
   usr_readiue.f usr_readsips.f usr_save.f usr_scan.f usr_seta.f \
   usr_setd.f usr_setm.f usr_sgs.f usr_show.f usr_spawn.f usr_trak.f \
   utor.f utow.f \
   vic_meor.f vic_mez1.f vic_mez2.f vic_mezr.f vic_rdat.f vic_trhd.f \
   vtou.f \
   wrcal2.f wrdq.f wrimg.f write_ndf.f wrmap.f wrord.f wrparc.f wrparf.f \
   wrpari.f wrparl.f wrpart.f wrpart2.f wrspar.f wrspc.f wrtem.f wtog.f \
   wtor.f wtou.f \
   xlget.f xyculm.f xycurs.f \
   ylget.f \
   getinp.f gtslot.f rdkybd.f reca.f icl_reada.f

#  List of Fortran BLOCK DATA routines.

BLOCK_DATA =

#  List of platform specific Fortran routines.  The source tar file will
#  contain a version of these files for each set of platforms.  The names
#  given here are the general version of the file, e.g. pkg_open.f.  The
#  names in the tar file will be pkg_open.f_sun4, pkg_open.f_mips, etc.

PLATFORM_F =

#  C routines required for building the package.  This is just a list of
#  all the C source files.

C_ROUTINES = rdkbd2.c

#  List of applications/tasks in monolith.

TASKS =

#  Monolith routine source name.

MONO_SOURCE = mono.f
HELPTASK_SOURCE = helpm.c helpc.c

#  Monolith binary name.

MONOLITH = $(PKG_NAME)3
HELPTASK = iuehelp

#  Monolith interface file name.

MONO_IFL = $(PKG_NAME)3.ifl

#  List of task interface files.

IFL_FILES =

#  List of help library sources.

HELP_SOURCES = $(PKG_NAME)3.hlp

#  List of ancilliary data files.

DATA_FILES = info.lis lwp.rip  lwpg.fid lwphi.abs lwphi.dsp lwplap.cut \
   lwplo.abs lwplo.dsp lwplo1.abs lwplo2.abs lwpr.fid lwprhi.fpt \
   lwprlo.fpt lwpsap.cut lwr.rip lwrg.fid lwrghi.fpt lwrglo.fpt \
   lwrhi.abs lwrhi.dsp lwrlap.cut lwrlo.abs lwrlo.dsp lwrlo1.abs \
   lwrlo1_bg.abs lwrlo2.abs lwrr.fid lwrrhi.fpt lwrrlo.fpt lwrsap.cut \
   swp.rip swpg.fid swpghi.fpt swpglo.fpt swphi.abs swphi.dsp swplap.cut \
   swplo.abs swplo.dsp swplo2.abs swplo3.abs swpr.fid swprhi.fpt \
   swprlo.fpt swpsap.cut

DEMO_FILES = swp14931.phot demo.cmd demo.sh demo.ref

DOCS_FILES = iuefile.hlp ver10.doc ver11.doc ver12.doc ver13.doc ver14.doc \
   ver20.doc ver20a.doc ver30.doc ver31-9.doc ver32.doc

SG7_FILES = sg7.info lwp_correction.dat lwr_correction.dat \
   swp_correction.dat LWR_CORRECTION.sdf SWP_CORRECTION.sdf \
   WAVECAL_LORES.sdf page.cmd

USER_FILES = build.sh rdlbls.f readlb.f single.csh readiue.csh extract.csh \
   template.cmd

#  Lists of Latex and hypertext documents.

LATEX_DOCS = sg3.tex sg7.tex mud45.tex sun37.tex

HYPERTEXT_DOCS = $(LATEX_DOCS:.tex=.htx)

#  List of documentation files.

DOCUMENTATION = $(LATEX_DOCS) $(HYPERTEXT_DOCS:.htx=.htx_tar) $(PKG_NAME).news

################################################################################
#-------------------------------------------------------------------------------

#  Define files required for building the package.
#  ==============================================
#
#  This section defines the set of files produced from the source files
#  when the package is built and installed.

#  Use only .o, .c, .f, .ifc, .ifl, .shl and .hlp suffix rules.

.SUFFIXES:
.SUFFIXES: .o .c .f .ifc .ifl .shl .hlp

#  List of files which must be built from the source files before the
#  package can be installed for use.  This should comprise all the files
#  that are required to use the package (but excluding the date stamp
#  file).

BUILT_FILES = $(PUBLIC_SCRIPTS) $(PUBLIC_INCLUDES) $(OBJECT_LIBRARIES) \
   $(MONOLITH) $(HELPTASK) $(IFC_FILES) $(HELP_FILES) $(STARTUP_SCRIPT)

#  List of links used to access include files during compilation.  This
#  should comprise all the external include files and any other include
#  files whose names do not exactly match the names used in the source
#  code.

INCLUDE_LINKS = $(EXTERNAL_INCLUDES)

#  Rules to set up links to locate each of the above include files.

SAE_PAR:               $(STAR_INC)/sae_par; $(LINK) $? $@
CHR_ERR:               $(STAR_INC)/chr_err; $(LINK) $? $@
DAT_PAR:               $(STAR_INC)/dat_par; $(LINK) $? $@
MAG_ERR:               $(STAR_INC)/mag_err; $(LINK) $? $@
MSG_ERR:               $(STAR_INC)/msg_err; $(LINK) $? $@
MSG_PAR:               $(STAR_INC)/msg_par; $(LINK) $? $@
PAR_ERR:               $(STAR_INC)/par_err; $(LINK) $? $@
PRM_PAR:               $(STAR_INC)/prm_par; $(LINK) $? $@

#  Rules for extracting source files from the source archive.

$(PUBLIC_SCRIPTS) $(PUBLIC_INCLUDES) $(MONO_SOURCE) $(HELPTASK_SOURCE) \
   $(HELP_SOURCES) $(STARTUP_SCRIPT) $(IFL_FILES) $(MONO_IFL) $(F_ROUTINES) \
   $(BLOCK_DATA) $(C_ROUTINES):
	$(TAR_OUT) $(PKG_NAME)_source.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi

#  Source Include files.

$(PRIVATE_INCLUDES): $(PKG_NAME)_inc.tar
	$(TAR_OUT) $(PKG_NAME)_inc.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi

#  Rules for extracting platform specific source files from the source
#  archive.
#  It is quite likely that there are no platform-specific source files
#  and that the macro PLATFORM_F is empty. To prevent a syntax error in
#  make, a dummy target is present.

$(PLATFORM_F) dummy_target1:
	$(TAR_OUT) $(PKG_NAME)_source.tar $@_$(SOURCE_VARIANT)
	@ if test -f $@_$(SOURCE_VARIANT); then :;\
           else echo $@_$(SOURCE_VARIANT) is not in the tar file; exit 1; fi
	mv $@_$(SOURCE_VARIANT) $@

#  List of object files produced by compiling the source code and rules
#  for performing the compilations.

OBJECT_FILES =  $(F_ROUTINES:.f=.o) $(C_ROUTINES:.c=.o)

MONO_OBJECT = $(MONO_SOURCE:.f=.o)

.c.o:
	$(CC) $(CFLAGS) -c $< -o $(<:.c=.o)
.f.o:
	$(FC) $(FFLAGS) -c $< -o $(<:.f=.o)

#  List of object library files to be built and rules for building
#  them.

OBJECT_LIBRARIES = lib$(PKG_NAME).a

lib$(PKG_NAME).a: $(OBJECT_FILES)
	$(AR_IN) $@ $?
	$(RANLIB) $@

#  Rule and dependencies for building the monolith.

$(MONOLITH): $(MONO_OBJECT) $(OBJECT_LIBRARIES)
	$(ALINK) mono icl_reada.o -L. -liuedr $(ADDLIBS) `mag_link_adam` `snx_link_adam` \
	   `hlp_link` `psx_link` `fio_link` `ndf_link`
	mv mono iuedr3

#  Rules for building the help browser.

$(HELPTASK): $(HELPTASK_SOURCE)
	$(CC) $(CFLAGS) -c helpc.c
	$(CC) $(CFLAGS) -c helpm.c
	$(CFLINK) helpm.o helpc.o -o $(HELPTASK) \
           $(LDFLAG1) $(ADDLIBS) -L$(STAR_LIB) \
           `hlp_link` `fio_link` `cnf_link` $(CFLIBS) $(LDFLAG2)
	- rm -f help[mc].o

#  Rules for building the help libraries.

HELP_FILES = $(HELP_SOURCES:.hlp=.shl)

.hlp.shl:
	$(STAR_BIN)/hlib $<

#  Rules for compiling the interface files.

IFC_FILES = $(IFL_FILES:.ifl=.ifc) $(MONO_IFL:.ifl=.ifc)

.ifl.ifc:
	$(STAR_BIN)/compifl $<

#  Developers targets.
#
#  "Manual" link of the monolith.

byhand:
	$(ALINK) mono -L. -liuedr $(ADDLIBS) `mag_link_adam` `snx_link_adam` \
	   `hlp_link` `psx_link` `fio_link` `ndf_link`
	mv mono iuedr3

startupcheck:
	if test -n "$(STARTUP_SCRIPT)"; then \
           sed -e s#INSTALL_BIN#$(INSTALL_BIN)# \
               -e s#INSTALL_HELP#$(INSTALL_HELP)# \
               -e s#PKG_VERS#$(PKG_VERS)# \
              $(STARTUP_SCRIPT) >$(STARTUP_SCRIPT)t;\
           chmod 755 $(STARTUP_SCRIPT)t;\
        else :; fi

#
#  Build compressed archive of IUEDR beta release.

beta: iuedr3 iuedr3.ifc iuedr3.shl iuedrsetup
	$(TAR_IN) $(EXPORT)/$(PKG_NAME)_$(SYSTEM)_beta.tar \
	iuedr3 iuedr3.ifc iuedr3.shl iuedrsetup
	compress -v $(EXPORT)/$(PKG_NAME)_$(SYSTEM)_beta.tar
	@ echo
	@ echo \
'*** Beta release copy of the $(PKG_NAME) package is in the compressed'
	@ echo \
'    tar file $(EXPORT)/$(PKG_NAME)_$(SYSTEM)_beta.tar.Z'
	@ echo

#  Name of the date stamp file.  This is used to record the time of the
#  most recent build for use in subsequent operations that require it.
#  There must be no rule for generating this file; it is only updated
#  as a side effect of building the package.

DATE_STAMP = $(PKG_NAME)_datestamp

#  Pathnames of directories into which files may be placed when the
#  package is installed.

INSTALL_BIN = $(INSTALL)/bin/$(PKG_NAME)
INSTALL_DATES = $(INSTALL)/dates
INSTALL_DOCS = $(INSTALL)/docs
INSTALL_HELP = $(INSTALL)/help/$(PKG_NAME)

INSTALL_DEMO = $(INSTALL_BIN)/demo
INSTALL_DOC = $(INSTALL_BIN)/doc
INSTALL_DATA = $(INSTALL_BIN)/data
INSTALL_HYPER = $(INSTALL_BIN)/hyper
INSTALL_SG7 = $(INSTALL_BIN)/sg7
INSTALL_USER = $(INSTALL_BIN)/user

#  List of directories actually used for installation (selected from
#  those above) and rules to create them.

INSTALL_DIRS = $(INSTALL_BIN) $(INSTALL_HELP) $(INSTALL_DATES) \
   $(INSTALL_DOCS) $(INSTALL_DEMO) $(INSTALL_DOC) $(INSTALL_DATA) \
   $(INSTALL_HYPER) $(INSTALL_SG7) $(INSTALL_USER)

DEINSTALL_DIRS = $(INSTALL_DEMO) $(INSTALL_DOC) $(INSTALL_DATA) \
   $(INSTALL_USER) $(INSTALL_SG7) $(INSTALL_HYPER)

$(INSTALL_DIRS):
	mkdir -p $@

#-------------------------------------------------------------------------------

#  Primary targets.
#  ===============
#
#  These are the targets intended for normal external use (apart from
#  help, which appears at the start of the file).

#  check: Check source file presence and show current state.
#  --------------------------------------------------------

#  The check target simply depends on all the source files being
#  present.

check:
	@ echo
	@ echo \
   '*** This is $(PACK_NAME) version V$(PKG_VERS) on system $(SYSTEM)'
	@ echo
	@ nosource='';\
          for f in $(SOURCE_FILES); do \
             if test ! -f $$f; then \
                nosource='1';\
                break;\
             else :; fi;\
          done;\
          if test -n "$$nosource"; then \
             echo '    Source files are NOT present';\
          else \
             echo '    All essential source files are present';\
          fi
	@ echo
#
#  Display the current state.
	@ if test -f .BUILT;\
          then echo '    The package is currently:  built for system'\
             `cat .BUILT`;\
          else echo '    The package is currently:  not built';fi
	@ if test -f .INSTALLED_$(SYSTEM);\
          then echo '                               installed in'\
             `cat .INSTALLED_$(SYSTEM)`;\
          else echo '                               not installed';fi
	@ if test -f .TESTED_$(SYSTEM);\
          then echo '                               tested';\
          else echo '                               not tested';fi
	@ echo
	@ if test -f .BUILT;\
          then if test "$(SYSTEM)" != "`cat .BUILT`";\
             then echo '***  WARNING  ***';\
                echo \
'    The package is built for a system other than the current one';\
                echo ;\
             else :;fi;\
          else :;fi

#  build: Build the system.
#  -----------------------
#
#  Compile the source and build the required files in the source
#  directory.

#  The build target first checks that the package is not installed.  If
#  not, it then causes the .BUILT target to be made which ensures that
#  the package has been built.

build:
	@ if test -f .INSTALLED_$(SYSTEM); then \
           echo;\
           echo \
   '*** The $(PACK_NAME) package is currently installed -- please use the';\
           echo '    "deinstall" target before re-building it';\
           echo;\
        elif $(MAKE) .BUILT; then \
           echo;\
           echo '*** The $(PACK_NAME) package has been built';\
           echo;\
        else \
           echo;\
           echo '*** "make" failed building the $(PACK_NAME) package';\
           echo;\
           exit 1;\
        fi

#  The .BUILT target records the time of the most recent build which
#  modified any of the built files.  It depends on all the built files
#  being up to date (which causes them to be built).

.BUILT: $(BUILT_FILES)
#
#  Enter information about the current machine and build environment
#  into the date stamp file.
	@ echo 'Package : $(PKG_NAME)'         >$(DATE_STAMP)
	@ echo 'Version : V$(PKG_VERS)'       >>$(DATE_STAMP)
	@ echo ''                             >>$(DATE_STAMP)
	@ echo "Built by: $(USER) on node `uname -n`" \
                                              >>$(DATE_STAMP)
	@ echo "On      : `date`"             >>$(DATE_STAMP)
	@ echo ''                             >>$(DATE_STAMP)
	@ echo \
  "Machine : `uname -m` running `uname -s` `uname -v` (release `uname -r`)" \
                                              >>$(DATE_STAMP)
	@ echo ''                             >>$(DATE_STAMP)
	@ echo 'make macros:'                 >>$(DATE_STAMP)
	@ echo ''                             >>$(DATE_STAMP)
	@ echo '   SYSTEM  : $(SYSTEM)'       >>$(DATE_STAMP)
	@ echo ''                             >>$(DATE_STAMP)
	@ echo '   EXPORT  : $(EXPORT)'       >>$(DATE_STAMP)
	@ echo '   INSTALL : $(INSTALL)'      >>$(DATE_STAMP)
	@ echo '   STARLINK: $(STARLINK)'     >>$(DATE_STAMP)
	@ echo ''                             >>$(DATE_STAMP)
	@ echo '   AR_IN   : $(AR_IN)'        >>$(DATE_STAMP)
	@ echo '   CC      : $(CC)'           >>$(DATE_STAMP)
	@ echo '   CFLAGS  : $(CFLAGS)'       >>$(DATE_STAMP)
	@ echo '   FC      : $(FC)'           >>$(DATE_STAMP)
	@ echo '   FFLAGS  : $(FFLAGS)'       >>$(DATE_STAMP)
	@ echo '   LINK    : $(LINK)'         >>$(DATE_STAMP)
	@ echo '   RANLIB  : $(RANLIB)'       >>$(DATE_STAMP)
	@ echo '   SOURCE_VARIANT: $(SOURCE_VARIANT)' \
                                              >>$(DATE_STAMP)
	@ echo '   TAR_IN  : $(TAR_IN)'       >>$(DATE_STAMP)
	@ echo '   TAR_OUT : $(TAR_OUT)'      >>$(DATE_STAMP)
	@ echo ''                             >>$(DATE_STAMP)
#
#  Record completion of the build.
	@ echo '$(SYSTEM)' > .BUILT

#  install: Install the package for use.
#  ------------------------------------
#
#  Copy the built files to their installation directories, from where
#  they may be accessed.

#  The install target first checks that no part of the package is
#  already installed.  If not, it causes the .INSTALLED_$(SYSTEM) target
#  to be made which performs the installation.

install:
	@ if test -f .INSTALLED_$(SYSTEM); then \
           echo;\
           echo \
   '*** The $(PACK_NAME) package has already been installed -- please use the';\
           echo \
   '    "deinstall" target first if you wish to reinstall it';\
           echo;\
        elif test -f .BUILT; then \
           if test "`cat .BUILT`" = "$(SYSTEM)"; then \
              if $(MAKE) .INSTALLED_$(SYSTEM); then \
                 echo;\
                 echo \
   '*** The $(PACK_NAME) package has been installed in directory $(INSTALL)';\
                 echo;\
              else \
                 echo;\
                 echo \
   '*** "make" failed installing the $(PACK_NAME) package in directory $(INSTALL)';\
                 echo;\
                 exit 1;\
              fi;\
           else \
              echo;\
              echo \
   "*** The $(PACK_NAME) package is built for system `cat .BUILT` -"\
   'so cannot be installed on system $(SYSTEM)';\
              echo;\
              exit 1;\
           fi;\
        else \
           echo;\
           echo \
   '*** The $(PACK_NAME) package is not built, so cannot be installed';\
           echo;\
           exit 1;\
        fi

#  The .INSTALLED_$(SYSTEM) target copies each file from the source
#  directory using "cp -p" to preserve its date, and replaces each
#  original file by a link to the installed copy.

.INSTALLED_$(SYSTEM): $(INSTALL_DIRS)
#
#  Create .INSTALLED_$(SYSTEM), containing $INSTALL, to record that the
#  package is installed (at least in part).
	@ echo $(INSTALL) > .INSTALLED_$(SYSTEM)
#
#  Install the public scripts, giving them world execute permission.
	for f in $(PUBLIC_SCRIPTS) ""; do \
           if test -n "$$f"; then \
              cp -p $$f $(INSTALL_BIN);\
              chmod 755 $(INSTALL_BIN)/$$f;\
              rm -f $$f;\
              $(LINK) $(INSTALL_BIN)/$$f $$f;\
           else :; fi;\
        done
#
#  Install the monolith, giving it world execute permission.
	for f in $(MONOLITH) $(HELPTASK) ""; do \
           if test -n "$$f"; then \
              cp -p $$f $(INSTALL_BIN);\
              chmod 755 $(INSTALL_BIN)/$$f;\
              rm -f $$f;\
              $(LINK) $(INSTALL_BIN)/$$f $$f;\
           else :; fi;\
        done
#
#  Install the interface files, giving them world read permission.
	for f in $(IFC_FILES) ""; do \
           if test -n "$$f"; then \
              cp -p $$f $(INSTALL_BIN);\
              chmod 644 $(INSTALL_BIN)/$$f;\
              rm -f $$f;\
              $(LINK) $(INSTALL_BIN)/$$f $$f;\
           else :; fi;\
        done
#
#  Install the data files, giving them world read permission.
	if test -n "$(DATA_ARCHIVE)"; then \
           pwd=`pwd`;\
	   (cd $(INSTALL_DATA);\
           for f in $(DATA_ARCHIVE) ""; do \
              if test -n "$$f"; then\
	         $(TAR_OUT) $$pwd/$${f};\
                 chmod 644 *;\
              else :; fi;\
           done);\
	fi
#
#  Install the "old" documentation files, giving them world read permission.
	if test -n "$(DOC_ARCHIVE)"; then \
           pwd=`pwd`;\
	   (cd $(INSTALL_DOC);\
           for f in $(DOC_ARCHIVE) ""; do \
              if test -n "$$f"; then\
	         $(TAR_OUT) $$pwd/$${f};\
                 chmod 644 *;\
              else :; fi;\
           done);\
	fi
#
#  Install the demo files.
	if test -n "$(DEMO_ARCHIVE)"; then \
           pwd=`pwd`;\
	   (cd $(INSTALL_DEMO);\
           for f in $(DEMO_ARCHIVE) ""; do \
              if test -n "$$f"; then\
	         $(TAR_OUT) $$pwd/$${f};\
                 chmod 644 *;\
              else :; fi;\
           done);\
	fi
#
#  Install the sg7 files.
	if test -n "$(SG7_ARCHIVE)"; then \
           pwd=`pwd`;\
	   (cd $(INSTALL_SG7);\
           for f in $(SG7_ARCHIVE) ""; do \
              if test -n "$$f"; then\
	         $(TAR_OUT) $$pwd/$${f};\
                 chmod 644 *;\
              else :; fi;\
           done);\
	fi
#
#  Install the user files.
	if test -n "$(USER_ARCHIVE)"; then \
           pwd=`pwd`;\
	   (cd $(INSTALL_USER);\
           for f in $(USER_ARCHIVE) ""; do \
              if test -n "$$f"; then\
	         $(TAR_OUT) $$pwd/$${f};\
                 chmod 644 *;\
              else :; fi;\
           done);\
	fi
#
#  Install the help files, giving them world read permission.
	for f in $(HELP_FILES) ""; do \
           if test -n "$$f"; then \
              cp -p $$f $(INSTALL_HELP);\
              chmod 644 $(INSTALL_HELP)/$$f;\
              rm -f $$f;\
              $(LINK) $(INSTALL_HELP)/$$f $$f;\
	      $(LINK) $(INSTALL_HELP)/$$f $(INSTALL_BIN)/$$f;\
           else :; fi;\
        done
#
#  Install the hypertext help.
	if test -n "$(HYPERTEXT_ARCHIVE)"; then \
           pwd=`pwd`;\
	   (cd $(INSTALL_HYPER);\
           for f in $(HYPERTEXT_ARCHIVE) ""; do \
              if test -n "$$f"; then\
	         $(TAR_OUT) $$pwd/$${f};\
                 chmod 644 *;\
              else :; fi;\
           done);\
	fi
#
#  Install the package startup script.  The name of the directory
#  containing the installed public include files must be edited into
#  this, and execute permission given.  Leave the original file in
#  place.
	if test -n "$(STARTUP_SCRIPT)"; then \
           sed -e s#INSTALL_BIN#$(INSTALL_BIN)# \
               -e s#INSTALL_HELP#$(INSTALL_HELP)# \
               -e s#PKG_VERS#$(PKG_VERS)# \
              $(STARTUP_SCRIPT) >$(INSTALL_BIN)/$(STARTUP_SCRIPT) ;\
           chmod 755 $(INSTALL_BIN)/$(STARTUP_SCRIPT) ;\
        else :; fi
#
#  Install the Latex documentation, giving it world read permission,
#  leaving the source copy in place.
	for f in $(LATEX_DOCS) ""; do \
           if test -n "$$f"; then \
              cp -p $$f $(INSTALL_DOCS);\
              chmod 644 $(INSTALL_DOCS)/$$f;\
           else :; fi;\
        done
#
#  Install any hypertext documents, giving world read access to all the files
#  they contain and linking with other documents.
	if test -n "$(HYPERTEXT_DOCS)"; then \
           pwd=`pwd`;\
           (cd $(INSTALL_DOCS);\
           for f in $(HYPERTEXT_DOCS) ""; do \
              if test -n "$$f"; then\
                 $(TAR_OUT) $$pwd/$${f}_tar;\
                 chmod 755 `find $$f -type d -print`;\
                 chmod 644 `find $$f ! -type d -print`;\
                 touch $$f;\
              else :; fi;\
           done);\
           HTX_PATH='$(STAR_DOCS):$(STAR_HELP)';\
           export HTX_PATH;\
           $(STAR_BIN)/hlink $(INSTALL_DOCS) $(INSTALL_HELP);\
        fi;
#
#  Install the date stamp file and make it read-only to prevent its
#  date being changed.
	cp -p $(DATE_STAMP) $(INSTALL_DATES)
	chmod 444 $(INSTALL_DATES)/$(DATE_STAMP)
	chmod 644 $(DATE_STAMP)
	rm -f $(DATE_STAMP)
	$(LINK) $(INSTALL_DATES)/$(DATE_STAMP) $(DATE_STAMP)

#  deinstall: Deinstall the package.
#  --------------------------------
#
#  Reverse the action of the install target, removing the installed
#  files and returning them to the source directory.

#  The deinstall target checks that the package is installed.  If so,
#  it causes the do_deinstall target to be made which performs the
#  deinstallation.

deinstall:
	@ if test ! -f .INSTALLED_$(SYSTEM); then \
           echo;\
           echo '*** The $(PACK_NAME) package is not currently installed';\
           echo;\
        else \
           if test "`cat .INSTALLED_$(SYSTEM)`" = "$(INSTALL)"; then \
              if $(MAKE) do_deinstall; then \
                 echo;\
                 echo \
'*** The $(PACK_NAME) package has been deinstalled from directory $(INSTALL)';\
                 echo;\
              else \
                 echo;\
                 echo \
'*** "make" failed deinstalling the $(PACK_NAME) package from directory $(INSTALL)';\
                 echo;\
                 exit 1;\
              fi;\
           else \
              echo;\
              echo \
"*** The $(PACK_NAME) package is installed in `cat .INSTALLED_$(SYSTEM)`";\
              echo \
"*** and not in your INSTALL directory ($(INSTALL))";\
              echo '*** Not deinstalled';\
              exit 1;\
           fi;\
        fi

#  The do_deinstall target (which should never exist) checks that an
#  installed version of each file exists (in case an install failed
#  part of the way through) and returns it to the source directory,
#  using "cp -p" to preserve file dates.  Links are removed from the
#  source directory before copying.

do_deinstall:
#
#  Note the package will need to be tested again.
	@- if test -f .TESTED_$(SYSTEM); then rm -f .TESTED_$(SYSTEM); else :; fi
#
#  Deinstall the public script files, if installed versions exist.
	- for f in $(PUBLIC_SCRIPTS) ""; do \
           if test -n "$$f" -a -f $(INSTALL_BIN)/$$f; then \
              rm -f $$f;\
              cp -p $(INSTALL_BIN)/$$f .;\
              rm -f $(INSTALL_BIN)/$$f;\
           else :; fi; \
        done
#
#  Deinstall the monolith, if an installed version exists.
	- for f in $(MONOLITH) $(HELPTASK) ""; do \
           if test -n "$$f" -a -f $(INSTALL_BIN)/$$f; then \
              rm -f $$f;\
              cp -p $(INSTALL_BIN)/$$f .;\
              rm -f $(INSTALL_BIN)/$$f;\
           else :; fi;\
        done
#
#  Deinstall the interface files, if installed versions exist.
	- for f in $(IFC_FILES) ""; do \
           if test -n "$$f" -a -f $(INSTALL_BIN)/$$f; then \
              rm -f $$f;\
              cp -p $(INSTALL_BIN)/$$f .;\
              rm -f $(INSTALL_BIN)/$$f;\
           else :; fi;\
        done
#
#  Deinstall the data files, if installed versions exist.
	- for f in $(INSTALL_DATA) ""; do \
           if test -n "$$f" -a -d "$$f"; then \
              rm -rf $$f;\
           else :; fi;\
        done
#
#  Deinstall the "old" documentation files, if installed versions exist.
	- for f in $(INSTALL_DOC) ""; do \
           if test -n "$$f" -a -d "$$f"; then \
              rm -rf $$f;\
           else :; fi;\
        done
#
#  Deinstall the demo files, if installed versions exist.
	- for f in $(INSTALL_DEMO) ""; do \
           if test -n "$$f" -a -d "$$f"; then \
              rm -rf $$f;\
           else :; fi;\
        done
#
#  Deinstall the sg7 files, if installed versions exist.
	- for f in $(INSTALL_SG7) ""; do \
           if test -n "$$f" -a -d "$$f"; then \
              rm -rf $$f;\
           else :; fi;\
        done
#
#  Deinstall the user files, if installed versions exist.
	- for f in $(INSTALL_USER) ""; do \
           if test -n "$$f" -a -d "$$f"; then \
              rm -rf $$f;\
           else :; fi;\
        done
#
#  Deinstall the help files, if installed versions exist.
	- for f in $(HELP_FILES) ""; do \
           if test -n "$$f" -a -f $(INSTALL_HELP)/$$f; then \
              rm -f $$f;\
              cp -p $(INSTALL_HELP)/$$f .;\
              rm -f $(INSTALL_BIN)/$$f;\
              rm -f $(INSTALL_HELP)/$$f;\
           else :; fi;\
        done
#
#  Deinstall the hypertext files, if installed versions exist.
	- for f in $(INSTALL_HYPER) ""; do \
           if test -n "$$f" -a -d "$$f"; then \
              rm -rf $$f;\
           else :; fi;\
        done
#
#  Deinstall the package startup file.  Since it will have been edited
#  during installation, we remove the installed copy, if present, and
#  then ensure that the original exists.
	- if test "$(STARTUP_SCRIPT)" != ""; then \
           if test -f $(INSTALL_BIN)/$(STARTUP_SCRIPT); then\
              rm $(INSTALL_BIN)/$(STARTUP_SCRIPT);\
           else :; fi ;\
           $(MAKE) $(STARTUP_SCRIPT);\
        else :; fi
#
#  Deinstall the Latex documentation, if installed versions exist.
	- for f in $(LATEX_DOCS) ""; do \
           if test -n "$$f" -a -f $(INSTALL_DOCS)/$$f; then \
              rm -f $(INSTALL_DOCS)/$$f;\
           else :; fi;\
        done
#
#  Deinstall any hypertext documents,  and relink the hypertext if required.
	- if test -n "$(HYPERTEXT_DOCS)"; then \
           for f in $(HYPERTEXT_DOCS) ""; do \
              if test -n "$$f" -a -d $(INSTALL_DOCS)/$$f; then \
                 rm -f -r $(INSTALL_DOCS)/$$f;\
              else :; fi;\
           done;\
           HTX_PATH='$(STAR_DOCS):$(STAR_HELP)';\
           export HTX_PATH;\
           $(STAR_BIN)/hlink $(INSTALL_DOCS) $(INSTALL_HELP);\
        fi
#
#  Remove iuedr-specific directories.
#	- for f in $(DEINSTALL_DIRS) ""; do \
#          if test -d "$$f"; then \
#             rmdir $$f;\
#          else :; fi;\
#        done
#
#  Deinstall the date stamp file after setting its protection so it may
#  be removed.
	- if test -f $(INSTALL_DATES)/$(DATE_STAMP); then \
           chmod 644 $(DATE_STAMP); rm -f $(DATE_STAMP);\
           chmod 644 $(INSTALL_DATES)/$(DATE_STAMP);\
           cp -p $(INSTALL_DATES)/$(DATE_STAMP) .;\
           rm -f $(INSTALL_DATES)/$(DATE_STAMP);\
        else :; fi
#
#  Note the system is no longer installed.  Re-create .BUILT, since we have
#  returned the built files to the source directory but an unbuild may have
#  been done.
	@- rm -f .INSTALLED_$(SYSTEM) 1>/dev/null 2>/dev/null
	@ echo '$(SYSTEM)' > .BUILT

#  test: Perform an installation test.
#  ----------------------------------
#
#  Check that installed files are in their correct places and that a
#  simple test program will run correctly.

#  The test target checks that the package is currently installed.  If
#  so, it causes the do_test target to be made, which performs the
#  installation test.

test:
	@ if test ! -f .INSTALLED_$(SYSTEM); then \
           echo;\
           echo '*** The $(PACK_NAME) package is not currently installed';\
           echo;\
        elif $(MAKE) do_test; then\
           echo;\
           echo \
           '*** Installation test for the $(PACK_NAME) package has been run';\
           echo;\
        else \
           echo;\
           echo \
           '*** Installation test for the $(PACK_NAME) package failed';\
           echo;\
           exit 1;\
        fi

#  The do_test target performs the installation test. A file named do_test
#  should never exist.

do_test:
#
#  Note the test has not yet succeeded.
	@- if test -f .TESTED_$(SYSTEM); then rm -f .TESTED_$(SYSTEM); else :; fi
#
	@ echo
	@ echo '$(PACK_NAME) does not have a make driven installation test'
	@ echo
	@ echo 'You can run a quick installation test using the following'
	@ echo 'commands:'
	@ echo ' '
	@ echo '     source $(INSTALL_BIN)/$(STARTUP_SCRIPT)'
	@ echo '     source $(INSTALL_BIN)/demo/demo.sh'
	@ echo ' '
	@ echo "The test compares it's results with a reference list."
	@ echo 'Some differences (e.g. file names) may be displayed.'
#
#  Note the test has been run.
	@ touch .TESTED_$(SYSTEM)

#  export: Export the installed system.
#  -----------------------------------
#
#  Export the source plus all the built files to a new user.

#  The export target depends on the resulting compressed tar file being
#  up to date.

export: $(EXPORT)/$(PKG_NAME)_$(SYSTEM).tar.Z
	@ echo
	@ echo \
'*** Export copy of the built $(PACK_NAME) package is in the compressed'
	@ echo \
'    tar file $(EXPORT)/$(PKG_NAME)_$(SYSTEM).tar.Z'
	@ echo

#  The compressed tar file is up to date if it exists and is more
#  recent than all the source files and the date stamp file (which
#  records the time of the last build which modified any files).

$(EXPORT)/$(PKG_NAME)_$(SYSTEM).tar.Z: $(SOURCE_FILES) $(DATE_STAMP)
#
#  Issue a warning if the package has not been tested.
	@ if test ! -f .TESTED_$(SYSTEM); then \
           echo;\
           echo '*** Warning: the $(PACK_NAME) package has not been tested';\
           echo;\
        else :; fi
#
#  Remove any pre-existing tar files before creating new ones.
	if test -f $(EXPORT)/$(PKG_NAME)_$(SYSTEM).tar.Z; then \
           rm -f $(EXPORT)/$(PKG_NAME)_$(SYSTEM).tar.Z; else :; fi
	$(TAR_IN) - $(SOURCE_FILES) $(BUILT_FILES) $(DATE_STAMP) .BUILT \
           | compress -v > $(EXPORT)/$(PKG_NAME)_$(SYSTEM).tar.Z

#  export_run: Export the built system (without source).
#  -----------------------------------------------------
#
#  Export all the built files to a new user.

#  The export_run target depends on the resulting compressed tar file being
#  up to date.

export_run: $(EXPORT)/$(PKG_NAME)_$(SYSTEM)_run.tar.Z
	@ echo
	@ echo \
'*** Export copy of the "runtime" $(PACK_NAME) package is in the compressed'
	@ echo \
'    tar file $(EXPORT)/$(PKG_NAME)_$(SYSTEM)_run.tar.Z'
	@ echo

#  The compressed tar file is up to date if it exists and is more
#  recent than all the source files and the date stamp file (which
#  records the time of the last build which modified any files).

$(EXPORT)/$(PKG_NAME)_$(SYSTEM)_run.tar.Z: $(SOURCE_FILES) $(DATE_STAMP)
#
#  Issue a warning if the package has not been tested.
	@ if test ! -f .TESTED_$(SYSTEM); then \
           echo;\
           echo '*** Warning: the $(PACK_NAME) package has not been tested';\
           echo;\
        else :; fi
#
#  Remove any pre-existing tar files before creating new ones.
	if test -f $(EXPORT)/$(PKG_NAME)_$(SYSTEM)_run.tar.Z; then \
           rm -f $(EXPORT)/$(PKG_NAME)_$(SYSTEM)_run.tar.Z; else :; fi
	$(TAR_IN) - mk makefile $(DOCUMENTATION) $(BUILT_FILES) \
           $(ARCHIVES) $(DATE_STAMP) .BUILT \
           | compress -v > $(EXPORT)/$(PKG_NAME)_$(SYSTEM)_run.tar.Z

#  export_source: Export the source.
#  --------------------------------
#
#  Export the source files only to a new user.

#  This target depends on the resulting compressed tar file being up to
#  date.

export_source: $(EXPORT)/$(PKG_NAME).tar.Z
	@ echo
	@ echo \
'*** Export copy of the $(PACK_NAME) package source is in the compressed'
	@ echo \
'    tar file $(EXPORT)/$(PKG_NAME).tar.Z'
	@ echo

#  The compressed tar file is up to date if it exists and is more
#  recent than all the source files.

$(EXPORT)/$(PKG_NAME).tar.Z: $(SOURCE_FILES)
#
#  Remove any pre-existing tar files before creating new ones.
	if test -f $(EXPORT)/$(PKG_NAME).tar.Z; then \
           rm -f $(EXPORT)/$(PKG_NAME).tar.Z; else :; fi
	$(TAR_IN) - $(SOURCE_FILES) \
           | compress -v > $(EXPORT)/$(PKG_NAME).tar.Z

#  export_dev: Build the source tar files.
#  ---------------------------------------
#
#  This is not a public target, and intended only for the software
#  developer.

#  This target constructs the tar files that constitute the main
#  SOURCE_FILES from their contents.

export_dev: $(PKG_NAME)_source.tar $(PKG_NAME)_data.tar $(PKG_NAME)_demo.tar \
   $(PKG_NAME)_doc.tar $(PKG_NAME)_sg7.tar \
   $(PKG_NAME)_user.tar $(PKG_NAME)_hyper.tar $(PKG_NAME)_inc.tar \
   $(HYPERTEXT_DOCS:.htx=.htx_tar)

$(PKG_NAME)_source.tar:
	$(TAR_IN) $@ $(PUBLIC_SCRIPTS) $(PUBLIC_INCLUDES) $(MONO_SOURCE) \
           $(HELPTASK_SOURCE) $(HELP_SOURCES) $(STARTUP_SCRIPT) $(IFL_FILES) \
           $(MONO_IFL) $(F_ROUTINES) $(BLOCK_DATA) $(C_ROUTINES)

$(HYPERTEXT_DOCS:.htx=.htx_tar):
	$(TAR_IN) $@ $(@:.htx_tar=.htx)

$(PKG_NAME)_data.tar:
	cd data; $(TAR_IN) ../$@ $(DATA_FILES)

$(PKG_NAME)_demo.tar:
	cd demo; $(TAR_IN) ../$@ $(DEMO_FILES)

$(PKG_NAME)_doc.tar:
	cd doc; $(TAR_IN) ../$@ $(DOCS_FILES)

$(PKG_NAME)_hyper.tar:
	cd hyper; $(TAR_IN) ../$@ .

$(PKG_NAME)_inc.tar:
	$(TAR_IN) $@ $(PRIVATE_INCLUDES)

$(PKG_NAME)_sg7.tar:
	cd sg7; $(TAR_IN) ../$@ $(SG7_FILES)

$(PKG_NAME)_user.tar:
	cd user; $(TAR_IN) ../$@ $(USER_FILES)

#  clean: Clean up the source directory.
#  ------------------------------------
#
#  Remove all intermediate files.  Do not remove built files.

clean:
	@- rm -f $(INCLUDE_LINKS) $(PRIVATE_INCLUDES) $(IFL_FILES) \
	      $(MONO_IFL) $(HELP_SOURCES) $(F_ROUTINES) $(PLATFORM_F) \
	      $(BLOCK_DATA) $(C_ROUTINES) $(OBJECT_FILES) $(HELPTASK_SOURCE) \
	      $(MONO_OBJECT) $(MONO_SOURCE) \
	      1>/dev/null 2>/dev/null
	@ echo
	@ echo '*** Intermediate files removed'
	@ echo

#  unbuild: Reverse the build process.
#  ----------------------------------

#  Remove all intermediate files and all built files, and note that the
#  package is no longer built or tested.

unbuild: clean
	@- rm -f $(BUILT_FILES) $(DATE_STAMP) .BUILT 1>/dev/null 2>/dev/null
	@ echo '*** Built files removed'
	@ echo

#  strip: strip the monoliths of debug information.
#  -----------------------------------------------

strip:
	strip $(MONOLITH)
	strip $(HELPTASK)
	@ echo '*** Monolith stripped'

#-------------------------------------------------------------------------------

#  Include file dependencies.
#  =========================

#  Object file dependencies on include files (or links to those include
#  files).  These are normally generated automatically from the source
#  files.

abscal.o: CMABS CMCAL CMFLX CMHEAD CMWAV SAE_PAR 
aladr.o: CMDYN SAE_PAR 
alcut.o: CMCUT 
alrip.o: CMRIP 
altem.o: CMTEM 
cagrid.o: CMBIN CMDATA CMEXTP CMWAV SAE_PAR 
cahi.o: CMECOR CMHEAD CMSPEC CMVEL CMWAV SAE_PAR 
calbls.o: CMECOR CMLBLS CMWCOR 
calgen.o: CMCAL CMFLX CMHEAD CMNET CMWAV SAE_PAR 
calo.o: CMSPEC CMVEL CMWAV CMWCOR SAE_PAR 
cawav.o: CMECOR CMVEL CMWCOR 
clfile.o: SAE_PAR 
closdr.o: CMDYN CMFILE CMGRAF SAE_PAR 
cmd_err.o: SAE_PAR 
cncomb.o: CMCOMB 
cndata.o: CMDATA CMDISP CMFACE CMFIDS CMFIDT CMGEOM CMHEAD CMITFC CMTEM 
cndsn.o: CMFILE 
cnlbls.o: CMLBLS 
cnord.o: CMBKG CMCAL CMCEN CMDEV CMEXTP CMFLX CMNET CMSPEC CMWAV 
cnpar.o: SAE_PAR 
cnscan.o: CMSCAN 
cnspec.o: CMABS CMCUT CMECOR CMHAL CMRIP CMSAVE CMVEL CMWCOR 
crdata.o: CMFILE CMHEAD SAE_PAR 
crspec.o: CMFILE CMHEAD SAE_PAR 
dassoc.o: CMCOMB CMDATA CMFILE CMMUST CMNEED CMSAVE SAE_PAR 
defapr.o: CMHEAD SAE_PAR 
defext.o: CMEXTP CMHEAD SAE_PAR 
defpan.o: CMEXTP CMHEAD PAR_ERR SAE_PAR 
defrip.o: CMHEAD CMRIP 
desubs.o: CMDATA CMEXTP CMWAV SAE_PAR 
dladr.o: CMDYN SAE_PAR 
dlcut.o: CMCUT 
dlrip.o: CMRIP 
dmore.o: CMCOMB CMDATA CMFILE CMMUST CMNEED CMSAVE SAE_PAR 
editim.o: CMDATA CMGRAF SAE_PAR 
editlo.o: CMGRAF CMNET CMWAV SAE_PAR 
errint.o: CMERR 
errout.o: CMERR SAE_PAR 
errset.o: CMERR 
errstr.o: CMERR 
exbkg.o: CMBKG CMBKGS CMDEV CMDEVS CMEXTP CMSIST CMWAV 
exlbls.o: CMPAN 
exobj.o: CMBIN CMBKG CMCEN CMDEV CMDISH CMEXTP CMNET CMSIST CMWAV 
expan.o: CMBIN CMCEN CMDISH CMEXTP CMHEAD CMLBLS CMWAV SAE_PAR 
exspec.o: CMDATA CMHEAD SAE_PAR 
exsubs.o: CMBUG CMEXTP SAE_PAR 
extrak.o: CMEXTP 
face.o: CMDATA CMFACE 
fnaper.o: CMHEAD 
fncut.o: CMCUT 
fnord.o: CMSAVE 
fnrip.o: CMRIP 
fntem.o: CMTEM 
frdsn.o: CMFILE SAE_PAR 
gencal.o: CMCAL CMFLX CMNET CMWAV 
geomf.o: CMFACE CMFIDS CMGEOM CMHEAD SAE_PAR 
get_orders.o: PAR_ERR SAE_PAR 
get_sptype.o: SAE_PAR 
getinp.o: CMDSB 
getrip.o: CMHEAD CMRIP 
gphot.o: SAE_PAR 
grf_axes.o: CMCOLR CMGRAF CMPAL SAE_PAR 
grf_clzone.o: CMGRAF SAE_PAR 
grf_cuzone.o: CMCOLR CMGRAF SAE_PAR 
grf_deaxes.o: CMGRAF CMPLOT SAE_PAR 
grf_fnline.o: CMLINR 
grf_hicurv.o: CMCOLR CMGRAF CMLINR CMPAL SAE_PAR 
grf_opcurs.o: CMGRAF 
grf_opidev.o: CMGRAF PAR_ERR SAE_PAR 
grf_opzone.o: CMGRAF PAR_ERR SAE_PAR 
grf_pocurv.o: CMCOLR CMGRAF CMLINR CMPAL 
grf_ppalet.o: CMGRAF CMPAL 
grf_qpalet.o: CMGRAF CMPAL 
grf_reline.o: CMCOLR CMGRAF CMLINR 
grf_rscolr.o: CMCOLR CMGRAF CMPAL 
grf_rsgraf.o: CMCOLR CMGRAF CMLINR 
grf_rsline.o: CMCOLR CMGRAF CMLINR SAE_PAR 
grf_rslut.o: CMGRAF 
grf_seline.o: CMCOLR CMGRAF CMLINR 
grf_tzone.o: SAE_PAR 
grf_zinit.o: SAE_PAR 
gtor.o: CMGEOM CMHEAD 
gtow.o: CMDISH 
gtslot.o: CMDSB 
halcal.o: CMCAL CMDISH CMDISP CMFLX CMHAL CMNET CMSAVE CMSPEC CMWAV 
hicomb.o: CMCOMB CMFLX CMHEAD CMSAVE CMTEMP CMWAV SAE_PAR 
higrid.o: CMCUT CMDISH CMEXTP CMFACE CMWAV SAE_PAR 
hirad.o: CMDISH CMDISP CMEXTP CMHEAD 
hislcu.o: CMDATA CMGRAF CMHEAD MSG_PAR SAE_PAR 
hislit.o: CMDISH CMEXTP CMHEAD SAE_PAR 
hitem.o: CMFACE CMHEAD SAE_PAR 
hlp_nametr.o: CMPSX 
initdr.o: CMGRAF CMPSX SAE_PAR 
invoke.o: SAE_PAR 
itfcor.o: CMEXTP CMHEAD CMITFC 
iue_aprn.o: SAE_PAR 
iue_camn.o: SAE_PAR 
iue_device.o: CMISAF CMPSX CMTAPE PAR_ERR SAE_PAR 
iue_flan.o: CMISAF 
iue_resn.o: SAE_PAR 
iuedr3.o: CMDYN SAE_PAR 
iulog.o: CMPSX 
line_get.o: CMLINE 
line_wcont.o: CMLINE 
line_writf.o: CMLINE 
line_writi.o: CMLINE 
line_writl.o: CMLINE 
line_writs.o: CMLINE 
locomb.o: CMCOMB CMFLX CMHEAD CMSAVE CMSPEC CMTEMP CMWAV SAE_PAR 
logrid.o: CMDISH CMEXTP CMFACE CMHEAD CMWAV SAE_PAR 
lorad.o: CMEXTP CMHEAD SAE_PAR 
loset.o: CMDISH 
loslcu.o: CMDATA CMGRAF CMHEAD SAE_PAR 
loslit.o: CMEXTP CMHEAD SAE_PAR 
lotem.o: CMFACE CMHEAD SAE_PAR 
maspec.o: CMCAL CMFLX CMWAV 
mebkg.o: CMBKG CMBUG CMCEN CMEXTP CMSIST CMWAV 
mequal.o: SAE_PAR 
mkfids.o: CMDATA CMFIDS 
modcal.o: CMFILE 
modim.o: CMDATA CMGRAF SAE_PAR 
modmap.o: CMCOMB CMFILE 
modued.o: CMFILE 
modueq.o: CMFILE 
mono.o: CHR_ERR CMDYN CMPRT CMPSX SAE_PAR 
mrdata.o: CMDATA CMFILE 
msc_chef.o: SAE_PAR 
msc_map.o: SAE_PAR 
msc_map1d4.o: SAE_PAR 
mt_fix.o: CMTAPE MAG_ERR SAE_PAR 
mt_open.o: CMTAPE MAG_ERR SAE_PAR 
mvdisp.o: CMDISH CMDISP CMHEAD SAE_PAR 
mvfidt.o: CMFIDS CMFIDT CMHEAD 
mwspec.o: SAE_PAR 
newapr.o: CMDISP CMECOR CMHEAD CMVEL CMWCOR SAE_PAR 
orset.o: CMDISH CMDISP CMROTR 
outpan.o: CMLBLS SAE_PAR 
outrk.o: CMHEAD CMSAVE SAE_PAR 
outspc.o: CHR_ERR SAE_PAR 
panhi.o: CMDATA CMHEAD CMSPEC SAE_PAR 
panlo.o: CMDATA CMHEAD CMSPEC SAE_PAR 
parfer.o: PAR_ERR SAE_PAR 
pariue.o: CMHEAD CMISAF CMTRUN SAE_PAR 
parspc.o: CMHEAD CMISAF CMSPC SAE_PAR 
pickhi.o: CMDISP CMGRAF CMHEAD CMSCAN SAE_PAR 
picklo.o: CMDISP CMGRAF CMSCAN SAE_PAR 
pideax.o: CMCOLR CMGRAF CMLINR CMPAL SAE_PAR 
pideim.o: CMGRAF CMIDEV SAE_PAR 
pipar.o: CMGRAF CMIDEV PAR_ERR SAE_PAR 
plist.o: CMDATA CMHEAD SAE_PAR 
prabs.o: CMABS CMHEAD SAE_PAR 
prdisp.o: CMDISP CMECOR CMHEAD CMVEL CMWCOR SAE_PAR 
prext.o: CMDISH CMEXTP CMHEAD CMWAV SAE_PAR 
prextp.o: CMEXTP CMHEAD CMSIST 
prfid.o: CMFIDS CMFIDT CMHEAD SAE_PAR 
prgeom.o: CMGEOM CMHEAD SAE_PAR 
prgrs.o: CMBKG CMEXTP CMFLX CMNET CMWAV 
prhal.o: CMHAL CMHEAD SAE_PAR 
primg.o: CMDATA CMFACE SAE_PAR 
prmap.o: CMCOMB SAE_PAR 
prpan.o: CMEXTP CMHEAD CMWAV SAE_PAR 
prpar.o: CMABS CMCOMB CMCUT CMDATA CMDISP CMFACE CMFIDS CMFIDT CMFILE CMGEOM \
   CMHAL CMHEAD CMITFC CMRIP CMSAVE CMSPEC CMTEM SAE_PAR 
prpix.o: CMEXTP CMSIST 
prrip.o: CMCUT CMHEAD CMRIP SAE_PAR 
prspc.o: CMFLX CMNET CMWAV 
prspec.o: CMHEAD CMSAVE SAE_PAR 
prtlin.o: CMPSX 
prtset.o: CMPRT 
raw.o: SAE_PAR 
rdabs.o: CMABS CMHEAD CMTEMP SAE_PAR 
rdcal.o: CMABS CMCUT CMDATA CMDISP CMECOR CMFACE CMFIDS CMFIDT CMGEOM CMHAL \
   CMHEAD CMITFC CMRIP CMTEM CMVEL CMWCOR SAE_PAR 
rdcal2.o: CMABS CMCUT CMDATA CMDISP CMECOR CMFACE CMFIDS CMFIDT CMGEOM CMHAL \
   CMHEAD CMITFC CMRIP CMTEM CMVEL CMWCOR SAE_PAR 
rdcomb.o: CMCOMB SAE_PAR 
rdcut.o: CMCUT 
rddat.o: CMDATA SAE_PAR 
rddisp.o: CMDISP CMECOR CMHEAD CMVEL CMWCOR SAE_PAR 
rdface.o: CMFACE 
rdfids.o: CMFIDS CMFIDT SAE_PAR 
rdfile.o: CMPSX SAE_PAR 
rdhi.o: SAE_PAR 
rdimg.o: CMCOMB CMDATA CMFILE CMHEAD CMMUST CMNEED CMPSX CMSAVE SAE_PAR 
rdkybd.o: CMDSB 
rdmap.o: CMCOMB SAE_PAR 
rdmesp.o: CMHEAD CMSAVE CMSPC CMSPEC SAE_PAR 
rdord.o: CMNET CMSAVE CMSPEC CMWAV SAE_PAR 
rdparc.o: CMPRT CMPSX PAR_ERR SAE_PAR 
rdparf.o: CMPRT CMPSX PAR_ERR SAE_PAR 
rdpari.o: CMPRT CMPSX PAR_ERR SAE_PAR 
rdparl.o: CMPRT CMPSX PAR_ERR SAE_PAR 
rdpart.o: SAE_PAR 
rdpart2.o: SAE_PAR 
rdqual.o: CMDATA 
rdrip.o: CMRIP 
rdspar.o: CMDATA 
rdspc.o: CMSAVE SAE_PAR 
rdspec.o: CMHEAD SAE_PAR 
rdtem.o: CMTEM SAE_PAR 
read_ndf.o: CHR_ERR CMCOMB CMNDF CMSAVE DAT_PAR SAE_PAR 
rebkg.o: CMBKG CMCEN CMDEV CMEXTP CMSIST CMWAV 
reca.o: CMDSB SAE_PAR 
recal.o: CMHEAD CMNET 
rfabs.o: CMABS CMHEAD PAR_ERR SAE_PAR 
rfcut.o: CMCUT CMHEAD SAE_PAR 
rfdisp.o: CMDISP CMHEAD SAE_PAR 
rfface.o: CMFACE CMHEAD SAE_PAR 
rffid.o: CMFIDS CMFIDT CMHEAD SAE_PAR 
rfgeom.o: CMGEOM CMHEAD SAE_PAR 
rfrip.o: CMHEAD CMRIP SAE_PAR 
rftem.o: CMHEAD CMTEM SAE_PAR 
ripcal.o: CMCAL CMCUT CMHEAD CMRIP CMSPEC CMWAV 
rtog.o: CMGEOM CMHEAD 
rtou.o: CMROTR 
sacen.o: CMCEN CMDISH CMEXTP CMTEM CMWAV 
scanhi.o: CMDATA CMFACE CMHEAD CMSCAN SAE_PAR 
scanlo.o: CMDATA CMFACE CMHEAD CMSCAN SAE_PAR 
sdbkg.o: CMBKG CMCEN CMDEV CMEXTP CMSIST CMWAV 
secen.o: CMCEN CMDISH CMEXTP CMTEM CMWAV 
setdh.o: CMHAL CMHEAD SAE_PAR 
setdr.o: CMHEAD CMRIP SAE_PAR 
setuv.o: CMROTR 
slculm.o: SAE_PAR 
slcurs.o: CMHEAD SAE_PAR 
sztype.o: SAE_PAR 
tape_file.o: PAR_ERR SAE_PAR 
tape_find.o: CMTAPE SAE_PAR 
tape_shop.o: CMTAPE SAE_PAR 
trakhi.o: CMDATA CMHEAD CMSPEC SAE_PAR 
traklo.o: CMDATA CMHEAD CMSPEC SAE_PAR 
unbkg.o: CMBKG CMCEN CMEXTP CMWAV 
usr_aeshift.o: CMDISP CMECOR CMHEAD CMNET CMSAVE CMWAV SAE_PAR 
usr_agshift.o: CMDISP CMHEAD CMSCAN SAE_PAR 
usr_barker.o: CMCUT CMHEAD CMRIP CMSAVE SAE_PAR 
usr_cgshift.o: CMHEAD SAE_PAR 
usr_clean.o: SAE_PAR 
usr_cmdread.o: CMPSX SAE_PAR 
usr_culimits.o: CMGRAF SAE_PAR 
usr_cursor.o: CMGRAF SAE_PAR 
usr_drimage.o: CMDATA CMGRAF CMHEAD CMIDEV CMPAL CMPSX SAE_PAR 
usr_edimage.o: CMHEAD SAE_PAR 
usr_edmean.o: CMCOMB CMGRAF CMTEMP SAE_PAR 
usr_edspec.o: CMHEAD SAE_PAR 
usr_erase.o: CMGRAF SAE_PAR 
usr_help.o: SAE_PAR 
usr_lbls.o: CMHEAD SAE_PAR 
usr_listiue.o: CMISAF CMTAPE MAG_ERR SAE_PAR 
usr_map.o: CMHEAD SAE_PAR 
usr_modimage.o: CMHEAD SAE_PAR 
usr_mtmove.o: CMTAPE SAE_PAR 
usr_mtrew.o: CMTAPE SAE_PAR 
usr_mtshow.o: SAE_PAR 
usr_mtskipeov.o: CMTAPE SAE_PAR 
usr_mtskipf.o: CMTAPE SAE_PAR 
usr_newabs.o: CMABS SAE_PAR 
usr_newcut.o: CMCUT CMHEAD SAE_PAR 
usr_newdisp.o: CMDISP SAE_PAR 
usr_newfid.o: CMDATA CMFIDS CMFIDT CMGEOM CMHEAD SAE_PAR 
usr_newrip.o: CMHEAD CMRIP SAE_PAR 
usr_newtem.o: CMHEAD CMTEM SAE_PAR 
usr_outem.o: CMHEAD CMTEM SAE_PAR 
usr_outlbls.o: CMHEAD CMLBLS CMPAN SAE_PAR 
usr_outmean.o: CMCOMB CMHEAD CMTEMP SAE_PAR 
usr_outnet.o: CMHEAD CMNET CMSPEC CMWAV SAE_PAR 
usr_outrak.o: CMHEAD CMSAVE SAE_PAR 
usr_outscan.o: CMHEAD CMSCAN SAE_PAR 
usr_outspec.o: CMFLX CMHEAD CMSPEC CMWAV SAE_PAR 
usr_plcen.o: CMCEN CMCOLR CMEXTP CMGRAF CMHEAD CMPLOT CMSIST CMSPEC CMWAV \
   PAR_ERR SAE_PAR 
usr_plflux.o: CMCOLR CMEXTP CMFLX CMGRAF CMHEAD CMPLOT CMSPEC CMWAV SAE_PAR 
usr_plgrs.o: CMBKG CMCOLR CMEXTP CMGRAF CMHEAD CMNET CMPLOT CMSPEC CMWAV \
   PAR_ERR SAE_PAR 
usr_plmean.o: CMCOLR CMCOMB CMGRAF CMHEAD CMPLOT CMTEMP PAR_ERR SAE_PAR 
usr_plnet.o: CMCOLR CMEXTP CMGRAF CMHEAD CMNET CMPLOT CMSPEC CMWAV PAR_ERR \
   SAE_PAR 
usr_plscan.o: CMCOLR CMGRAF CMHEAD CMPLOT CMSCAN CMWAV SAE_PAR 
usr_prgrs.o: CMBKG CMEXTP CMFLX CMHEAD CMNET CMSPEC CMWAV SAE_PAR 
usr_prlbls.o: CMHEAD CMLBLS CMPAN SAE_PAR 
usr_prmean.o: CMCOMB CMHEAD SAE_PAR 
usr_prscan.o: CMHEAD CMSCAN SAE_PAR 
usr_prspec.o: CMFLX CMHEAD CMNET CMSPEC CMWAV SAE_PAR 
usr_readiue.o: CMDATA CMFILE CMHEAD CMISAF CMITFC CMTRUN SAE_PAR 
usr_readsips.o: CMFILE CMHEAD CMISAF CMSPC SAE_PAR 
usr_save.o: SAE_PAR 
usr_scan.o: CMHEAD SAE_PAR 
usr_seta.o: CMDISP CMECOR CMHEAD CMVEL CMWCOR SAE_PAR 
usr_setd.o: CMABS CMDATA CMFIDS CMFIDT CMGEOM CMHEAD CMITFC SAE_PAR 
usr_setm.o: CMCUT CMHEAD CMRIP PAR_ERR SAE_PAR 
usr_sgs.o: SAE_PAR 
usr_show.o: CMFILE CMHEAD SAE_PAR 
usr_spawn.o: SAE_PAR 
usr_trak.o: CMHEAD SAE_PAR 
utor.o: CMROTR 
vic_meor.o: CMSAVE CMSPC CMUEZ1 
vic_mez1.o: CMHEAD CMSAVE CMUEZ1 
vic_mez2.o: CMHEAD 
vic_mezr.o: CMHEAD CMSPC 
vic_rdat.o: CMISAF CMTAPE SAE_PAR 
vic_trhd.o: CMISAF CMPSX SAE_PAR 
vtou.o: CMDISH 
wrcal2.o: CMABS CMCUT CMDATA CMDISP CMECOR CMFACE CMFIDS CMFIDT CMGEOM CMHAL \
   CMHEAD CMITFC CMRIP CMTEM CMVEL CMWCOR SAE_PAR 
wrdq.o: CMFILE CMNET CMSAVE CMSPEC CMWAV 
wrimg.o: CMCOMB CMDATA CMFILE CMHEAD CMPSX CMSAVE SAE_PAR 
write_ndf.o: CMCOMB CMDATA CMFILE CMNDF CMSAVE DAT_PAR PRM_PAR SAE_PAR 
wrmap.o: CMCOMB SAE_PAR 
wrord.o: CMFILE CMNET CMSAVE CMSPEC CMWAV SAE_PAR 
wrparc.o: SAE_PAR 
wrparf.o: SAE_PAR 
wrpari.o: SAE_PAR 
wrparl.o: SAE_PAR 
wrpart.o: SAE_PAR 
wrpart2.o: SAE_PAR 
wrspar.o: CMDATA 
wrspc.o: CHR_ERR CMSAVE SAE_PAR 
wrtem.o: CMHEAD CMTEM SAE_PAR 
wtog.o: CMDISH 
xlget.o: CMGRAF PAR_ERR SAE_PAR 
xyculm.o: SAE_PAR 
xycurs.o: CHR_ERR CMGRAF MSG_PAR SAE_PAR 
ylget.o: CMGRAF PAR_ERR SAE_PAR 
