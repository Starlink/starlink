#+
#  Name:
#     makefile
#
#  Purpose:
#     Build and install the IRAS90 package.
#
#  Type of Module:
#     Description file for the make utility.
#
#  Description:
#     This description file is used by the make utility to build the
#     IRAS90 package from the distributed source files, to install
#     the resulting system for use, and to perform other housekeeping
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
#
#  External Dependencies:
#     The IRAS90 package depends on the following other Starlink
#     packages which must previously have been installed into the
#     appropriate sub-directories of the $STARLINK directory (/star is
#     used if the environment variable STARLINK is not defined).
#        agi
#           Graphics database
#        ard
#           Ascii region definition
#        chr
#           Character handling routines
#        ems
#           Error message service
#        fio
#           File I/O 
#        gks
#           Graphics Kernal system
#        gns
#           Graphics device naming service
#        grp
#           Groups of objects manipulation
#        mers
#           Error reporting
#        ndf
#           Extensible n-dimensional data system
#        par
#           Adam parameter system
#        primdat
#           Primative data manipulation
#        psx
#           Posix interface foutines
#        sae_par
#           Global parameters and constants
#        sla
#           positional astrometry library
#        trn
#           data transformation
#
#  Other dependencies.
#     These are dependencies other that Starlink Software.
#
#        nag
#           NAG Fortran Library (Double Precision)
#
#  Notes:
#     This makefile uses the presence/absence of the hidden files
#     .BUILT, .INSTALLED_$(SYSTEM) and .TESTED_$(SYSTEM) to record the
#     current state of the system during housekeeping operations.
#
#  Implementation Deficiencies:
#
#  Copyright:
#     Copyright (C) 1994 Engineering and Physical Sciences Research Council
#
#  Authors:
#     RFWS: R.F.Warren-Smith (Starlink, RAL)
#     PMA: P.M.Allan (Starlink, RAL)
#     PTW: P.T.Wallace (Starlink, RAL)
#     BLY: M.J.Bly (Starlink, RAL)
#     DCP: D.C.Parsons (IPMAF, RAL)
#     {enter_new_authors_here}
#
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
#     7-FEB-1994 (PMA):
#        Move the block data files to the beginning of the definition of
#        OBJECT_FILES. This makes it possible to extract the block data
#        modules with the -u loader flag. This is needed when using f2c.
#     19-MAY-1994 (BLY/DCP):
#        Version for IRAS90 applications package.  This is a new single 
#        makefile for IRAS90 to simplfy the building stages. No install
#        or deinstall system yet.
#     20-OCT-1994 (BLY):
#        Minor revisions and completion of makefile for IRAS90 release.
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
   '      help          - Print out this message';\
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
   '      export_source - Make a compressed tar file for exporting the'\
   'source files';\
        echo \
   '      clean         - Tidy up after building the package';\
        echo \
   '      unbuild       - Remove all the built files';\
        echo;\
        echo \
   '   To build and install the $(PKG_NAME) package on a supported system:';\
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
STAR_ETC = $(STARLINK)/etc
STAR_HELP = $(STARLINK)/help
STAR_INC = $(STARLINK)/include
STAR_LIB = $(STARLINK)/lib

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

RANLIB = echo > /dev/null

#  Commands for adding to and extracting from an archive file (.tar).

TAR_IN = pax -w -v -x ustar -f
TAR_OUT = pax -r -f

#  Command for adding a file to an object archive (.a).

AR_IN = ar -r

#-------------------------------------------------------------------------------
################################################################################
#
#  Define package source files.
#  ===========================
#
#  This section defines the set of source files for the package.

#  Name of the package.

PKG_NAME = iras90

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

PKG_VERS = 1.1

#  List of files comprising the distributed source-only system.  This
#  defines the minimum set of files required to completely rebuild the
#  package from source (including this makefile, the associated mk
#  script and any documentation files).

SOURCE_FILES = $(PKG_NAME)_source.tar makefile mk $(DOCUMENTATION) \
     i90_source.tar ira_source.tar irc_source.tar iri_source.tar \
     irm_source.tar irq_source.tar ndg_source.tar apps_source.tar \
     data_source.tar ifl_source.tar

#  List of public script files.  These are scripts which form part of
#  the package and will be required by users of it.  They will be
#  installed in the $(INSTALL_BIN) directory with execute permission
#  set.

PUBLIC_SCRIPTS = pscont iras90_test iras90_slink_adam iras90_link iras90_dev

#  List of private script files.  These are scripts which are required
#  by developers who wish to use the subroutine libraries, but should not
#  be installed in the INSTALL_BIN directory. 

PRIVATE_SCRIPTS = $(IRA_S) $(IRC_S) $(IRI_S) $(IRM_S) $(IRQ_S) \
$(NDG_S) $(I90_S)

IRA_S = ira_link iraa_link

IRC_S = irc_link irca_link

IRI_S = iri_link iria_link

IRM_S = irm_link irma_link

IRQ_S = irq_link irqa_link

NDG_S = ndg_link ndga_link

I90_S = i90_link i90a_link

#  Package startup script.  This script is edited on installation to 
#  contain the correct location of the binaries etc, and to contain
#  the correct package version number.  It is thus dealt with separately
#  from the other scripts at installation.

STARTUP_SCRIPT =iras90

#  List of private include files.  These are additional include files
#  which form part of the package and are required in order to build
#  it.  They are also required for developers, but should not be installed
#  in the INSTALL_INC directory.

PRIVATE_INCLUDES = $(IRAS90_I) $(I90_I) $(IRA_I) $(IRC_I) $(IRI_I) \
$(IRM_I) $(IRQ_I) $(NDG_I)

IRAS90_I =  ficomn sky_par tra_com

I90_I = i90_dat i90_par i90_com

IRA_I = ira_com ira_err ira_par

IRC_I = irc_com irc_err irc_par

IRI_I = iri_err iri_par

IRM_I = irm_com

IRQ_I = irq_com irq_cmo irq_opc irq_pao irq_err irq_par

NDG_I = ndg_com ndg_const ndg_err ndf_const


#  List of external include files.  These are files which are required
#  in order to build the package but form part of other, externally
#  installed packages.  This list should contain the names used to
#  reference the files within the source code, not the actual names of
#  the files.

EXTERNAL_INCLUDES = AGI_ERR AGI_PAR CHR_ERR CMP_ERR DAT_ERR DAT_PAR \
    ERR_ERR ERR_PAR FIO_ERR FIO_PAR GNS_PAR GKS_PAR GRP_ERR GRP_PAR \
    MSG_ERR MSG_PAR NDF_ERR NDF_PAR NUM_DEC_CVT NUM_DEF_CVT PAR_ERR \
    PAR_PAR PRM_PAR PSX_ERR SUBPAR_PAR SUBPAR_ERR SAE_PAR TRN_ERR

#  List of Fortran routines required for building the package.  This is
#  just a list of all the Fortran source files (excluding BLOCK DATA
#  routines, which are treated separately).

F_ROUTINES = $(I90_F) $(IRA_F) $(IRA_ADAM_F) $(IRC_F) $(IRI_F) $(IRI_ADAM_F) \
$(IRM_F) $(IRM_ADAM_F) $(IRQ_F) $(NDG_F) $(NDG_ADAM_F) $(APPS_F)

I90_F = i90_test.f

IRA_F = ira1_aito.f ira1_aitot.f ira1_ascre.f ira1_asdef.f ira1_asndf.f \
ira1_asres.f ira1_asset.f ira1_axlm.f ira1_axlp.f ira1_bbox.f ira1_bgch.f \
ira1_check.f ira1_chprj.f ira1_chscs.f ira1_curve.f ira1_decod.f ira1_delob.f \
ira1_drms.f ira1_drps.f ira1_ecec.f ira1_eglb.f ira1_egtk.f ira1_eqecl.f \
ira1_eqeq.f ira1_eqgal.f ira1_fndb.f ira1_fngp3.f ira1_fpars.f ira1_ftol.f \
ira1_galec.f ira1_getid.f ira1_gnom.f ira1_gnomt.f ira1_iconv.f ira1_ictd1.f \
ira1_idrva.f ira1_idst2.f ira1_idtc1.f ira1_ipaco.f \
ira1_iprj.f ira1_iscnm.f ira1_ivald.f ira1_lacc.f ira1_lamb.f ira1_lambt.f \
ira1_limit.f ira1_llex.f ira1_map1.f ira1_map2.f ira1_map3.f \
ira1_nrval.f ira1_optid.f ira1_orth.f ira1_plot.f ira1_plst.f ira1_potl.f \
ira1_potu.f ira1_prec.f ira1_scac.f ira1_sfld.f ira1_shapp.f ira1_shcal.f \
ira1_sortd.f ira1_stokn.f ira1_tpot.f ira1_trab.f ira1_tref.f ira1_usym.f \
ira1_vaito.f ira1_vgnom.f ira1_vlamb.f ira1_vorth.f ira_activ.f ira_annul.f \
ira_close.f ira_convt.f ira_creat.f ira_ctod.f ira_ctod1.f \
ira_demo1.f ira_demo2.f ira_dist.f ira_dist2.f ira_drbnd.f ira_drbrk.f \
ira_drgrd.f ira_drgtc.f ira_drmer.f ira_drops.f ira_dropt.f ira_drpar.f \
ira_drval.f ira_drvpo.f ira_dtoc.f ira_dtoc1.f ira_exprt.f ira_find.f \
ira_geteq.f ira_imprt.f ira_init.f \
ira_iproj.f ira_iscs.f ira_locat.f ira_mag.f ira_move.f ira_norm.f \
ira_offst.f ira_pacon.f ira_pixsz.f ira_proj.f ira_read.f ira_rot.f \
ira_scnam.f ira_scsep.f ira_seteq.f ira_shift.f ira_test.f ira_trace.f \
ira_trans.f ira_valid.f ira_write.f ira_xylim.f msg_loc.f

IRA_ADAM_F = ira1_igtc1.f ira_gtscs.f ira_getco.f ira_gtco1.f

# drexam1.f drexam2.f drexam3.f drexam4.f drexam5.f fig3d.f 
# ira1_map.f ira_cretr.f 

IRC_F = irc1_bclpi.f irc1_bclsb.f irc1_bposi.f irc1_bpssb.f irc1_chkla.f \
irc1_chkun.f irc1_clpnt.f irc1_clsat.f irc1_dclpi.f irc1_dclsb.f irc1_distg.f \
irc1_disti.f irc1_dposi.f irc1_dpssb.f irc1_dstsb.f irc1_getid.f irc1_imcoi.f \
irc1_offsb.f irc1_offsi.f irc1_pmpsb.f irc1_pntsb.f irc1_satci.f irc1_satsb.f \
irc1_sbfit.f irc1_scosb.f irc1_tr2sb.f irc1_traci.f irc1_trasb.f irc_annul.f \
irc_bclap.f irc_bpos.f irc_close.f irc_dclap.f irc_detin.f irc_detno.f \
irc_dinds.f irc_dist.f irc_dpos.f irc_fpco1.f irc_fpco2.f irc_ilab.f \
irc_imco.f irc_imprt.f irc_info.f irc_init.f irc_iunit.f irc_limit.f \
irc_offst.f irc_posmp.f irc_satco.f irc_simul.f irc_skco1.f irc_skco2.f \
irc_supp.f irc_test.f irc_trace.f irc_trunc.f

IRI_F = iri_check.f iri_new.f iri_old.f iri_test.f iri_trace.f

IRI_ADAM_F = iri_getun.f

IRM_F = irm1_ghlpi.f irm1_lunit.f irm1_namtr.f irm1_rglmt.f irm1_scrsz.f \
irm1_setip.f irm1_split.f irm1_sread.f irm1_statr.f irm1_wrtbx.f \
irm_agfnd.f irm_agref.f irm_antso.f irm_averg.f irm_biln1.f irm_biln2.f \
irm_blocr.f irm_cdesc.f irm_cff2r.f irm_chkob.f irm_clean.f irm_comnt.f \
irm_cpoin.f irm_crtyp.f irm_cvdet.f irm_delob.f irm_ellip.f irm_filnm.f \
irm_fit2.f irm_gdtyp.f irm_gkcmt.f irm_gkeyc.f irm_gkeyr.f \
irm_gthlp.f irm_hmenu.f irm_hmfft.f irm_hmsg.f irm_hmult.f irm_iattr.f \
irm_linr.f irm_lintr.f irm_litrr.f irm_medn.f irm_mline.f irm_noise.f\
irm_page.f irm_profl.f irm_ptast.f irm_ptout.f irm_qcol.f irm_qndf.f \
irm_qntlr.f irm_rlfft.f irm_skcur.f irm_solin.f irm_sortd.f irm_spage.f \
irm_sqflt.f irm_stats.f irm_staxs.f irm_stgrd.f irm_stlmt.f irm_stlog.f \
irm_stmap.f irm_stnul.f irm_stpen.f irm_stpha.f irm_subst.f irm_table.f \
irm_td.f irm_test.f irm_trcon.f irm_trinv.f irm_untcv.f \
irm_untim.f irm_untiv.f irm_vmenu.f pwrtm.f

IRM_ADAM_F = irm_rdndf.f irm_wrndf.f irm_dimen.f irm_getqx.f irm_gropn.f \
irm_gtdet.f irm_hist.f irm_tmenu.f irm_asfio.f irm_gettd.f irm_listn.f \
irm_gtbnd.f irm_gtast.f irm_readf.f irm_gtcun.f

IRQ_F = irq1_add.f irq1_altrp.f irq1_antmp.f irq1_check.f irq1_cmqm.f \
irq1_count.f irq1_evstk.f irq1_get.f irq1_gtidq.f irq1_iannu.f irq1_indf.f \
irq1_init.f irq1_islot.f irq1_mod.f irq1_ndtov.f irq1_nulop.f irq1_opand.f \
irq1_qcnt.f irq1_qlst.f irq1_qlst2.f irq1_qmsk.f irq1_qset.f \
irq1_rbit.f irq1_reset.f irq1_rslot.f irq1_sbad1.f irq1_searc.f irq1_simpl.f \
irq1_sorti.f irq1_space.f irq1_temp.f irq1_vtofx.f irq_addqn.f irq_annul.f \
irq_chkqn.f irq_close.f irq_cntq.f irq_comp.f irq_find.f irq_getqn.f \
irq_new.f irq_numqn.f irq_nxtqn.f irq_remqn.f irq_resq.f irq_resql.f \
irq_resqm.f irq_rlse.f irq_sbad.f irq_setq.f irq_setql.f irq_setqm.f \
irq_syntx.f irq_test.f

NDG_F = ndf1_a2p.f ndf1_chxnm.f ndf1_fparx.f ndf1_indxp.f ndf1_p2a.f \
ndf1_pscpx.f ndf1_psndb.f ndf1_psnde.f ndf1_psndf.f ndf1_pxlst.f ndf1_simlr.f \
ndg1_axlim.f ndg1_ffind.f ndg1_fspec.f ndg1_gtloc.f ndg1_lasto.f ndg1_ncut.f \
ndg1_ndfch.f ndg1_slice.f ndg1_stloc.f ndg_annul.f ndg_asexp.f ndg_crexp.f \
ndg_delet.f ndg_ndfas.f ndg_ndfcr.f ndg_ndfpr.f ndg_test.f

NDG_ADAM_F = ndg_assoc.f ndg_creat.f

APPS_F = agchax.f agchcu.f agchil.f \
backa0.f backa1.f backa2.f backa3.f backcrdd.f \
ccora0.f ccora1.f ccora2.f ccorp0.f \
colcorr.f coltemp.f \
cdcra0.f cdcra1.f cdcra2.f cdcra3.f cdcra4.f cdcra5.f cdcra6.f cdcra7.f \
cdcra8.f cdcra9.f cdcrb0.f cdcrd0.f coaddcrdd.f \
ctemp0.f ctemz0.f ctemz1.f ctemz2.f ctemz3.f ctemz4.f \
deglcrdd.f dgcra0.f dgcra1.f dgcra2.f dgcra3.f dgcra4.f \
desta0.f desta1.f desta3.f destcrdd.f \
find01.f find02.f find03.f find04.f find05.f find06.f find07.f find08.f \
find09.f find10.f find11.f find12.f find13.f find14.f find15.f find16.f \
find17.f find18.f find19.f find20.f find21.f find22.f find23.f find24.f \
find25.f find26.f find27.f find28.f find29.f find30.f find31.f find32.f \
find33.f find34.f find35.f find36.f find37.f find38.f find39.f find40.f \
find41.f find42.f find43.f find44.f find45.f find46.f findcrdd.f \
hista0.f hista1.f hista2.f hista3.f histb0.f histb1.f histc0.f history.f \
i90help.f irastrace.f itraa0.f \
ifield.f ifldz0.f ifldz1.f ifldz2.f ifldz3.f \
makeprof.f makeresp.f \
mapca0.f mapca1.f mapca2.f mapca3.f mapca5.f mapca6.f mapca7.f mapcb0.f \
mapcb1.f mapcb2.f mapcb3.f mapcb4.f mapcb5.f mapcb6.f mapcb7.f mapcc0.f \
mapcc2.f mapcc3.f mapcc4.f mapcc5.f mapcc6.f mapcc7.f mapcc8.f mapcc9.f \
mapcd0.f mapcd1.f mapcd2.f mapcd3.f mapcf0.f mapcf1.f mapcf2.f mapcrdd.f \
mproa0.f mresa0.f newua0.f newua1.f newua2.f newunits.f \
poina0.f poina1.f poina2.f poina3.f poina4.f poina5.f poina6.f poina7.f \
poina8.f poina9.f poinb0.f poinb1.f poinb2.f poinb3.f poinb4.f poinc0.f \
poinc1.f poinc2.f poinc3.f poinc4.f poind0.f poind1.f pointcrdd.f \
posca0.f posca1.f posca2.f posca3.f posca4.f poscrdd.f \
prepa0.f prepa1.f prepa2.f prepa3.f prepa4.f prepa5.f prepa6.f prepa7.f \
prepa8.f prepare.f prepb0.f prepb1.f prepb2.f prepb3.f prepb4.f prepb5.f \
prepb6.f prepb7.f prepb8.f prepb9.f prepc0.f prepc1.f prepc2.f prepc3.f \
prepc4.f prepc6.f prepc7.f \
qualtobad.f remqual.f \
salia0.f salia1.f salia2.f salia3.f salia4.f salia5.f salia6.f \
sboxa0.f sboxa1.f sboxa2.f sboxb0.f \
setimage.f setqa0.f setqa1.f setqual.f showqual.f \
simca0.f simca1.f simca2.f simcb0.f simcb1.f simcc5.f simcc6.f simcf0.f \
simcf1.f simcrdd.f \
skyalign.f skybox.f skygrid.f skyline.f skymark.f skyphot.f skypos.f \
skywrite.f \
slina1.f slina2.f slina3.f slina4.f slinc0.f slinc1.f slinc2.f slinc3.f \
sline0.f sline1.f sline2.f sline3.f slinf0.f slinf1.f slinf2.f slinf3.f \
sling0.f sling1.f sling2.f sling3.f sling4.f \
smara0.f smara1.f smara2.f smara3.f smarb0.f \
sphoa0.f sphob0.f sphob1.f sphoz1.f sphoz2.f \
sposa0.f sposb0.f sposb1.f sposb2.f sposb3.f sposb4.f \
swria0.f swria1.f swria2.f swria3.f swria4.f swria5.f swrib0.f swrib1.f \
swric0.f swrie0.f swrif0.f swrif1.f \
traca1.f traca2.f traca4.f traca5.f traca6.f traca7.f traca8.f traca9.f \
tracb0.f tracb1.f tracb2.f tracb3.f tracb4.f tracb5.f tracb6.f tracb9.f \
tracc1.f tracc2.f tracc3.f tracc4.f tracc5.f tracecrdd.f

#  List of Fortran BLOCK DATA routines.

BLOCK_DATA = $(I90_B) $(IRA_B) $(IRM_B) $(IRQ_B) $(NDG_B)

I90_B = i90_init.f

IRA_B = ira1_init.f

IRM_B = irm1_bldat.f

IRQ_B = irq1_opcin.f

NDG_B = ndg1_init.f

#  List of platform specific Fortran routines.  The source tar file will
#  contain a version of these files for each set of platforms.  The names
#  given here are the general version of the file, e.g. pkg_open.f.  The
#  names in the tar file will be pkg_open.f_sun4, pkg_open.f_mips, etc.

PLATFORM_F =

#  C routines required for building the package.  This is just a list of
#  all the C source files.

C_ROUTINES = $(IRM_C) $(NDG_C)

IRM_C = irm1_trmsz.c

NDG_C = find_file.c

#  List of documentation files.

DOCUMENTATION = sun161.tex sun161_a_1_1.ps sun161_a_1_2.ps sun161_a_2_1.ps \
sun161_a_3_1_canon.dat sun161_a_4_1.ps sun161_a_4_2.ps sun161_a_4_3.ps \
sun161_a_4_4.ps sun161_a_4_5.ps sun163.tex sun165.tex iras90.news

#  Name of pseudo-Monolith.

MONO_SOURCE = iras90_pm.f

MONOLITH = iras90_pm

#  Non-adam programs.

PROG_SOURCE = ffield.f

PROG_EXE = ffield

#  List of interface file sources.

IFL_FILES = backcrdd.ifl coaddcrdd.ifl colcorr.ifl coltemp.ifl deglcrdd.ifl \
destcrdd.ifl findcrdd.ifl history.ifl i90help.ifl ifield.ifl irastrace.ifl \
makeprof.ifl makeresp.ifl mapcrdd.ifl newunits.ifl pointcrdd.ifl \
poscrdd.ifl prepare.ifl qualtobad.ifl remqual.ifl setimage.ifl \
setqual.ifl showqual.ifl simcrdd.ifl skyalign.ifl skybox.ifl skygrid.ifl \
skyline.ifl skymark.ifl skyphot.ifl skypos.ifl skywrite.ifl tracecrdd.ifl

#  List of application names.

APPLICATIONS = backcrdd coaddcrdd colcorr coltemp deglcrdd \
destcrdd findcrdd history i90help ifield irastrace \
makeprof makeresp mapcrdd newunits pointcrdd \
poscrdd prepare qualtobad remqual setimage \
setqual simcrdd skyalign skybox skygrid \
skyline skymark skyphot skypos skywrite showqual tracecrdd

#  List of data files.

#  Detector PSF images.

PSF_FILES = psf1.sdf psf2.sdf psf3.sdf psf4.sdf psf5.sdf psf6.sdf psf7.sdf\
psf8.sdf psf9.sdf psf10.sdf psf11.sdf psf12.sdf psf13.sdf psf14.sdf psf15.sdf \
psf16.sdf psf18.sdf psf19.sdf psf21.sdf psf22.sdf psf23.sdf psf24.sdf \
psf25.sdf psf26.sdf psf27.sdf psf28.sdf psf29.sdf psf30.sdf psf31.sdf \
psf32.sdf psf33.sdf psf34.sdf psf35.sdf psf37.sdf psf38.sdf psf39.sdf \
psf40.sdf psf41.sdf psf42.sdf psf43.sdf psf44.sdf psf45.sdf psf46.sdf \
psf47.sdf psf48.sdf psf49.sdf psf50.sdf psf51.sdf psf52.sdf psf53.sdf \
psf54.sdf psf55.sdf psf56.sdf psf57.sdf psf58.sdf psf59.sdf psf60.sdf \
psf61.sdf psf62.sdf newspfarch.sdf profiles.sdf 

#  Spectral response NDFs.

DATA_FILES = specresp12.sdf specresp25.sdf specresp60.sdf specresp100.sdf 

#  Help Library source.

HELP_SOURCE = iras90_help.hlp

################################################################################
#-------------------------------------------------------------------------------

#  Define files required for building the package.
#  ==============================================
#
#  This section defines the set of files produced from the source files
#  when the package is built and installed.

#  Use only .o, .c, .f, .ifc, .ifl, .shl, and .hlp rules.

.SUFFIXES:
.SUFFIXES: .o .c .f .ifc .ifl .shl .hlp

#  List of files which must be built from the source files before the
#  package can be installed for use.  This should comprise all the files
#  that are required to use the package (but excluding the date stamp
#  file).

BUILT_FILES = $(PUBLIC_SCRIPTS) $(OBJECT_LIBRARIES) $(STARTUP_SCRIPT) \
$(IFC_FILES) $(PROG_EXE) $(MONOLITH) $(PSF_FILES) \
$(DATA_FILES) $(HELP_LIBRARY) $(PRIVATE_SCRIPTS) $(PRIVATE_INCLUDES) 

#  List of links used to access include files during compilation.  This
#  should comprise all the external include files and any other include
#  files whose names do not exactly match the names used in the source
#  code.

INCLUDE_LINKS = $(EXTERNAL_INCLUDES) FICOMN SKY_PAR TRA_COM \
                I90_DAT I90_PAR I90_COM IRA_COM IRA_ERR IRA_PAR \
                IRC_COM IRC_ERR IRC_PAR IRI_ERR IRI_PAR IRM_COM \
                IRQ_COM IRQ_CMO IRQ_OPC IRQ_PAO IRQ_ERR IRQ_PAR \
                NDG_COM NDG_CONST NDG_ERR NDF_CONST

#  Rules to set up links to locate each of the above include files.
FICOMN:   ficomn;   $(LINK) $? $@
SKY_PAR:  sky_par;  $(LINK) $? $@
TRA_COM:  tra_com;  $(LINK) $? $@
I90_DAT:  i90_dat;  $(LINK) $? $@
I90_PAR:  i90_par;  $(LINK) $? $@
I90_COM:  i90_com;  $(LINK) $? $@
IRA_COM:  ira_com;  $(LINK) $? $@
IRA_ERR:  ira_err;  $(LINK) $? $@
IRA_PAR:  ira_par;  $(LINK) $? $@
IRC_COM:  irc_com;  $(LINK) $? $@
IRC_ERR:  irc_err;  $(LINK) $? $@
IRC_PAR:  irc_par;  $(LINK) $? $@
IRI_ERR:  iri_err;  $(LINK) $? $@
IRI_PAR:  iri_par;  $(LINK) $? $@
IRM_COM:  irm_com;  $(LINK) $? $@
IRQ_COM:  irq_com;  $(LINK) $? $@
IRQ_CMO:  irq_cmo;  $(LINK) $? $@
IRQ_OPC:  irq_opc;  $(LINK) $? $@
IRQ_PAO:  irq_pao;  $(LINK) $? $@
IRQ_ERR:  irq_err;  $(LINK) $? $@
IRQ_PAR:  irq_par;  $(LINK) $? $@
NDF_CONST:  ndf_const;  $(LINK) $? $@
NDG_COM:  ndg_com;  $(LINK) $? $@
NDG_CONST:  ndg_const;  $(LINK) $? $@
NDG_ERR:  ndg_err;  $(LINK) $? $@

#  List of external include files.  These are files which are required
AGI_ERR:       $(STAR_INC)/agi_err;     $(LINK) $? $@
AGI_PAR:       $(STAR_INC)/agi_par;     $(LINK) $? $@
CHR_ERR:       $(STAR_INC)/chr_err;     $(LINK) $? $@
CMP_ERR:       $(STAR_INC)/cmp_err;     $(LINK) $? $@
DAT_ERR:       $(STAR_INC)/dat_err;     $(LINK) $? $@
DAT_PAR:       $(STAR_INC)/dat_par;     $(LINK) $? $@
ERR_ERR:       $(STAR_INC)/err_err;     $(LINK) $? $@
ERR_PAR:       $(STAR_INC)/err_par;     $(LINK) $? $@
FIO_ERR:       $(STAR_INC)/fio_err;     $(LINK) $? $@
FIO_PAR:       $(STAR_INC)/fio_par;     $(LINK) $? $@
GNS_PAR:       $(STAR_INC)/gns_par;     $(LINK) $? $@
GKS_PAR:       $(STAR_INC)/gks_par;     $(LINK) $? $@
GRP_ERR:       $(STAR_INC)/grp_err;     $(LINK) $? $@
GRP_PAR:       $(STAR_INC)/grp_par;     $(LINK) $? $@
MSG_ERR:       $(STAR_INC)/msg_err;     $(LINK) $? $@
MSG_PAR:       $(STAR_INC)/msg_par;     $(LINK) $? $@
NDF_ERR:       $(STAR_INC)/ndf_err;     $(LINK) $? $@
NDF_PAR:       $(STAR_INC)/ndf_par;     $(LINK) $? $@
NUM_DEC_CVT:   $(STAR_INC)/num_dec_cvt; $(LINK) $? $@
NUM_DEF_CVT:   $(STAR_INC)/num_def_cvt; $(LINK) $? $@
PAR_PAR:       $(STAR_INC)/par_par;     $(LINK) $? $@
PAR_ERR:       $(STAR_INC)/par_err;     $(LINK) $? $@
PRM_PAR:       $(STAR_INC)/prm_par;     $(LINK) $? $@
PSX_ERR:       $(STAR_INC)/psx_err;     $(LINK) $? $@
SUBPAR_PAR:    $(STAR_INC)/subpar_par;  $(LINK) $? $@
SUBPAR_ERR:    $(STAR_INC)/subpar_err;  $(LINK) $? $@
SAE_PAR:       $(STAR_INC)/sae_par;     $(LINK) $? $@
TRN_ERR:       $(STAR_INC)/trn_err;     $(LINK) $? $@

#  Rules for extracting source files from the source archives.

#
#  Main archive
$(PUBLIC_SCRIPTS) $(IRAS90_I) $(PROG_SOURCE) $(MONO_SOURCE) \
$(STARTUP_SCRIPT) $(HELP_SOURCE): $(PKG_NAME)_source.tar
	$(TAR_OUT) $(PKG_NAME)_source.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi
#
#  i90 acrhive
$(I90_B) $(I90_F) $(I90_I) $(I90_S): i90_source.tar
	$(TAR_OUT) i90_source.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi
#
#  ira acrhive
$(IRA_B) $(IRA_F) $(IRA_ADAM_F) $(IRA_I) $(IRA_S): ira_source.tar
	$(TAR_OUT) ira_source.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi
#
#  irc acrhive
$(IRC_F) $(IRC_I) $(IRC_S): irc_source.tar
	$(TAR_OUT) irc_source.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi
#
#  iri acrhive
$(IRI_F) $(IRI_ADAM_F) $(IRI_I) $(IRI_S): iri_source.tar
	$(TAR_OUT) iri_source.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi
#
#  irm acrhive
$(IRM_B) $(IRM_C) $(IRM_F) $(IRM_ADAM_F) $(IRM_I) $(IRM_S): irm_source.tar
	$(TAR_OUT) irm_source.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi
#
#  irq acrhive
$(IRQ_B) $(IRQ_F) $(IRQ_I) $(IRQ_S): irq_source.tar
	$(TAR_OUT) irq_source.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi
#
#  ndg acrhive
$(NDG_B) $(NDG_C) $(NDG_F) $(NDG_ADAM_F) $(NDG_I) $(NDG_S): ndg_source.tar
	$(TAR_OUT) ndg_source.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi
#
#  apps acrhive
$(APPS_F): apps_source.tar
	$(TAR_OUT) apps_source.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi


#  ifl  acrhive
$(IFL_FILES): ifl_source.tar
	$(TAR_OUT) ifl_source.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi

#
#  PSF  acrhive
$(PSF_FILES): data_source.tar
	$(TAR_OUT) data_source.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi

#  Rules for extracting platform specific source files from the source
#  archive.
#  It is quite likely that there are no platform-specific source files
#  and that the macro PLATFORM_F is empty. To prevent a syntax error in
#  make, a dummy target is present.

$(PLATFORM_F) dummy_target1: $(PKG_NAME)_source.tar
	$(TAR_OUT) $(PKG_NAME)_source.tar $@_$(SOURCE_VARIANT)
	@ if test -f $@_$(SOURCE_VARIANT); then :;\
           else echo $@_$(SOURCE_VARIANT) is not in the tar file; exit 1; fi
	mv $@_$(SOURCE_VARIANT) $@

#  List of object files produced by compiling the source code and rules
#  for performing the compilations.

OBJECT_FILES = $(I90_O) $(IRA_O) $(IRC_O) $(IRI_O) $(IRM_O) $(IRQ_O) \
               $(NDG_O) $(APPS_O)

I90_O = $(I90_B:.f=.o) $(I90_F:.f=.o)

IRA_O = $(IRA_B:.f=.o) $(IRA_F:.f=.o) $(IRA_ADAM_F:.f=.o)

IRC_O = $(IRC_F:.f=.o)

IRI_O = $(IRI_F:.f=.o) $(IRI_ADAM_F:.f=.o)

IRM_O = $(IRM_B:.f=.o) $(IRM_C:.c=.o) $(IRM_F:.f=.o) $(IRM_ADAM_F:.f=.o)

IRQ_O = $(IRQ_B:.f=.o) $(IRQ_F:.f=.o)

NDG_O = $(NDG_B:.f=.o) $(NDG_C:.c=.o) $(NDG_F:.f=.o) $(NDG_ADAM_F:.f=.o)

APPS_O = $(APPS_F:.f=.o)

MONO_OBJECT = $(MONO_SOURCE:.f=.o)

.c.o:
	$(CC) $(CFLAGS) -c $<
.f.o:
	$(FC) $(FFLAGS) -c $<

#  List of object library files to be built and rules for building
#  them.

OBJECT_LIBRARIES = libapps.a libirm.a libiri.a libirc.a libirq.a \
                   libira.a libndg.a libi90.a 

libi90.a: $(I90_O)
	$(AR_IN) $@ $?
	$(RANLIB) $@

libira.a: $(IRA_O)
	$(AR_IN) $@ $?
	$(RANLIB) $@

libirc.a: $(IRC_O)
	$(AR_IN) $@ $?
	$(RANLIB) $@

libiri.a: $(IRI_O)
	$(AR_IN) $@ $?
	$(RANLIB) $@

libirm.a: $(IRM_O)
	$(AR_IN) $@ $?
	$(RANLIB) $@

libirq.a: $(IRQ_O)
	$(AR_IN) $@ $?
	$(RANLIB) $@

libndg.a: $(NDG_O)
	$(AR_IN) $@ $?
	$(RANLIB) $@

libapps.a: $(APPS_O)
	$(AR_IN) $@ $?
	$(RANLIB) $@


#  Rules and dependencies for compiling .ifl files to .ifc files.

IFC_FILES = $(IFL_FILES:.ifl=.ifc)

.ifl.ifc:
	compifl $<

#  Rule for building the non-ADAM binary.

$(PROG_EXE): $(PROG_SOURCE)
	$(FC) $(FFLAGS) $(PROG_SOURCE) -o $(PROG_EXE)

#  Rule for building the ADAM monolith.

$(MONOLITH): $(MONO_OBJECT) $(OBJECT_LIBRARIES) $(PUBLIC_SCRIPTS)
	alink $(MONO_OBJECT) -o $(MONOLITH) $(OBJECT_LIBRARIES) -L$(STAR_LIB) \
              `./iras90_slink_adam`

#  Rule for generating help library.

HELP_LIBRARY = $(HELP_SOURCE:.hlp=.shl)

.hlp.shl:
	hlib $<

#  Rules for building Spectral response NDFs.

$(DATA_FILES): $(APPLICATIONS) $(IFC_FILES)

specresp12.sdf: makeresp makeresp.ifc
	./makeresp specresp12 12

specresp25.sdf: makeresp makeresp.ifc
	./makeresp specresp25 25

specresp60.sdf: makeresp makeresp.ifc
	./makeresp specresp60 60

specresp100.sdf: makeresp makeresp.ifc
	./makeresp specresp100 100

#  Rule for makeing softlinks for applications is source directory.

$(APPLICATIONS): $(MONOLITH) 
	for  f in $(APPLICATIONS); do \
           $(LINK) iras90_pm $$f;\
        done

#  Name of the date stamp file.  This is used to record the time of the
#  most recent build for use in subsequent operations that require it.
#  There must be no rule for generating this file; it is only updated
#  as a side effect of building the package.

DATE_STAMP = $(PKG_NAME)_datestamp

#  Pathnames of directories into which files may be placed when the
#  package is installed.

INSTALL_BIN = $(INSTALL)/bin/$(PKG_NAME)
INSTALL_DATES = $(INSTALL)/dates
INSTALL_HELP = $(INSTALL)/help/$(PKG_NAME)

#  List of directories actually used for installation (selected from
#  those above) and rules to create them.

INSTALL_DIRS = $(INSTALL_BIN) $(INSTALL_DATES) $(INSTALL_HELP)

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

check: $(SOURCE_FILES)
	@ echo
	@ echo \
   '*** This is $(PKG_NAME) version V$(PKG_VERS) on system $(SYSTEM)'
	@ echo
	@ echo '    All essential source files are present'
	@ echo
#
#  Display the current state.
	@ if test -f .BUILT;\
          then echo '    The package is currently:  built for system'\
             `cat .BUILT`;\
          else echo '    The package is currently:  not built';fi
	@ if test -f .INSTALLED_$(SYSTEM);\
          then echo '                               installed';\
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
   '*** The $(PKG_NAME) package is currently installed -- please use the';\
           echo '    "deinstall" target before re-building it';\
           echo;\
        elif $(MAKE) .BUILT; then \
           echo;\
           echo '*** The $(PKG_NAME) package has been built';\
           echo;\
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
   '*** The $(PKG_NAME) package has already been installed -- please use the';\
           echo \
   '    "deinstall" target first if you wish to reinstall it';\
           echo;\
        elif $(MAKE) .INSTALLED_$(SYSTEM); then \
           echo;\
           echo \
   '*** The $(PKG_NAME) package has been installed in directory $(INSTALL)';\
           echo;\
        fi

#  The .INSTALLED_$(SYSTEM) target copies each file from the source
#  directory using "cp -p" to preserve its date, and replaces each
#  original file by a link to the installed copy.

.INSTALLED_$(SYSTEM): $(INSTALL_DIRS)
#
#  Touch .INSTALLED_$(SYSTEM) to record that the package is installed
#  (at least in part).
	@ touch .INSTALLED_$(SYSTEM)
#
#  Install the public scripts, giving them world execute permission.
	for f in $(PUBLIC_SCRIPTS) " "; do \
           if test -f "$$f"; then \
              cp -p $$f $(INSTALL_BIN);\
              chmod 755 $(INSTALL_BIN)/$$f;\
              rm $$f;\
              $(LINK) $(INSTALL_BIN)/$$f $$f;\
           else :; fi;\
        done
#
#  Install the binaries, giving them world execute permission.
	for f in $(MONOLITH) $(PROG_EXE) " "; do \
           if test -f "$$f"; then \
              cp -p $$f $(INSTALL_BIN);\
              chmod 755 $(INSTALL_BIN)/$$f;\
              rm $$f;\
              $(LINK) $(INSTALL_BIN)/$$f $$f;\
           else :; fi;\
        done
#
#  Install the Applications soft links.
	for f in $(APPLICATIONS); do \
           $(LINK) $(MONOLITH) $(INSTALL_BIN)/$$f ;\
        done
#
#  Install the compiled interface files, giving them world read permission.
	for f in $(IFC_FILES) " "; do \
           if test -f "$$f"; then \
              cp -p $$f $(INSTALL_BIN);\
              chmod 644 $(INSTALL_BIN)/$$f;\
              rm $$f;\
              $(LINK) $(INSTALL_BIN)/$$f $$f;\
           else :; fi;\
        done
#
#  Install the data & PSF files, giving them world read permission.
	for f in $(DATA_FILES) $(PSF_FILES) " "; do \
           if test -f "$$f"; then \
              cp -p $$f $(INSTALL_BIN);\
              chmod 644 $(INSTALL_BIN)/$$f;\
              rm $$f;\
              $(LINK) $(INSTALL_BIN)/$$f $$f;\
           else :; fi;\
        done
#
#  Install the help library, giving it world read permission.
	for f in $(HELP_LIBRARY) " "; do \
           if test -f "$$f"; then \
              cp -p $$f $(INSTALL_HELP);\
              chmod 644 $(INSTALL_HELP)/$$f;\
              rm $$f;\
              $(LINK) $(INSTALL_HELP)/$$f $$f;\
           else :; fi;\
        done
#
#  Install the package startup script.  The name of the directory
#  containing the installed binaries etc, and the package version number
#  must be edited into this, and execute permission given.  Leave the
#  original file in  place.
	if test "$(STARTUP_SCRIPT)" != ""; then \
           sed -e s#INSTALL_BIN#$(INSTALL_BIN)# \
               -e s#INSTALL_HELP#$(INSTALL_HELP)# \
               -e s#PKG_VERS#$(PKG_VERS)# \
              $(STARTUP_SCRIPT) >$(INSTALL_BIN)/$(STARTUP_SCRIPT) ;\
           chmod 755 $(INSTALL_BIN)/$(STARTUP_SCRIPT) ;\
         else :; fi
#
#  Install the date stamp file and make it read-only to prevent its
#  date being changed.
	cp -p $(DATE_STAMP) $(INSTALL_DATES)
	chmod 444 $(INSTALL_DATES)/$(DATE_STAMP)
	chmod 644 $(DATE_STAMP)
	rm $(DATE_STAMP)
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
           echo '*** The $(PKG_NAME) package is not currently installed';\
           echo;\
        else \
           $(MAKE) do_deinstall;\
           echo;\
           echo \
'*** The $(PKG_NAME) package has been deinstalled from directory $(INSTALL)';\
           echo;\
        fi

#  The do_deinstall target (which should never exist) checks that an
#  installed version of each file exists (in case an install failed
#  part of the way through) and returns it to the source directory,
#  using "cp -p" to preserve file dates.  Links are removed from the
#  source directory before copying.

do_deinstall:
#
#  Note the package will need to be tested again.
	@- if test -f .TESTED_$(SYSTEM); then rm .TESTED_$(SYSTEM); else :; fi
#
#  Deinstall the public script files, if installed versions exist.
	- for f in $(PUBLIC_SCRIPTS) " "; do \
           if test -f $(INSTALL_BIN)/$$f; then \
              rm $$f;\
              cp -p $(INSTALL_BIN)/$$f ./;\
              rm $(INSTALL_BIN)/$$f;\
           else :; fi; \
        done
#
#  Deinstall the binaries, if installed versions exist.
	- for f in $(MONOLITH) $(PROG_EXE) " "; do \
           if test -f $(INSTALL_BIN)/$$f; then \
              rm $$f;\
              cp -p $(INSTALL_BIN)/$$f ./;\
              rm $(INSTALL_BIN)/$$f;\
           else :; fi; \
        done
#
#  Deinstall the Applications soft links.
	- for f in $(APPLICATIONS); do \
           rm -f $(INSTALL_BIN)/$$f ;\
        done
#
#  Deinstall the Compiled Interface, if installed versions exist.
	- for f in $(IFC_FILES) " "; do \
           if test -f $(INSTALL_BIN)/$$f; then \
              rm $$f;\
              cp -p $(INSTALL_BIN)/$$f ./;\
              rm $(INSTALL_BIN)/$$f;\
           else :; fi; \
        done
#
#  Deinstall the public include files, if installed versions exist.
	- for f in $(DATA_FILES) $(PSF_FILES) " "; do \
           if test -f $(INSTALL_BIN)/$$f; then \
              rm $$f;\
              cp -p $(INSTALL_BIN)/$$f ./;\
              rm $(INSTALL_BIN)/$$f;\
           else :; fi; \
        done
#
#  Deinstall the Help library, if installed an version exists.
	- for f in $(HELP_LIBRARY) " "; do \
           if test -f $(INSTALL_HELP)/$$f; then \
              rm $$f;\
              cp -p $(INSTALL_HELP)/$$f ./;\
              rm $(INSTALL_HELP)/$$f;\
           else :; fi; \
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
#  Deinstall the date stamp file after setting its protection so it may
#  be removed.
	- if test -f $(INSTALL_DATES)/$(DATE_STAMP); then \
           chmod 644 $(DATE_STAMP); rm $(DATE_STAMP);\
           chmod 644 $(INSTALL_DATES)/$(DATE_STAMP);\
           cp -p $(INSTALL_DATES)/$(DATE_STAMP) ./;\
           rm $(INSTALL_DATES)/$(DATE_STAMP);\
        else :; fi
#
#  Note the system is no longer installed.  Touch .BUILT, since we have
#  returned the built files to the source directory.
	@- rm .INSTALLED_$(SYSTEM) 1>/dev/null 2>/dev/null
	@ touch .BUILT

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
           echo '*** The $(PKG_NAME) package is not currently installed';\
           echo;\
        elif $(MAKE) do_test; then\
           echo;\
           echo \
           '*** Installation test for the $(PKG_NAME) package has been run';\
           echo;\
        fi

#  The do_test target performs the installation test. A file named do_test
#  should never exist.

do_test: $(EXTERNAL_INCLUDES)
#
#  Note the test has not yet succeeded.
	@- if test -f .TESTED_$(SYSTEM); then rm .TESTED_$(SYSTEM); else :; fi
#
#  Extract the test program from the archive and set up new links for
#  the include files which point at the installed versions.  Remove any
#  pre-existing links first if necessary.
	$(TAR_OUT) $(PKG_NAME)_source.tar $(PKG_NAME)_test.f
	@ if test -f $(PKG_NAME)_test.f; then :;\
           else echo $(PKG_NAME)_test.f is not in the tar file; exit 1; fi
	-rm IRAS90_ERR IRAS90_PAR
	$(LINK) $(INSTALL_INC)/iras90_err IRAS90_ERR
	$(LINK) $(INSTALL_INC)/iras90_par IRAS90_PAR
#
#  Build the test program, ensuring that the installed version of the library
#  and link files are used.
	$(FC) $(FFLAGS) $(PKG_NAME)_test.f -L$(INSTALL_LIB) -L$(STAR_LIB) \
              `$(INSTALL_BIN)/$(PKG_NAME)_link` -o $(PKG_NAME)_test
#
#  Remove the test program source and the include file links used to
#  build it.
	rm $(PKG_NAME)_test.f $(EXTERNAL_INCLUDES) IRAS90_ERR IRAS90_PAR
#
#  Execute the test program and remove the binary file when done.  Note
#  that any external mechanism for locating shareable libraries (e.g. a
#  search path) must previously have been set up.
	./$(PKG_NAME)_test
	rm $(PKG_NAME)_test
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
'*** Export copy of the built $(PKG_NAME) package is in the compressed'
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
           echo '*** Warning: the $(PKG_NAME) package has not been tested';\
           echo;\
        else :; fi
#
#  Remove any pre-existing tar files before creating new ones.
	if test -f $(EXPORT)/$(PKG_NAME)_$(SYSTEM).tar; then \
           rm $(EXPORT)/$(PKG_NAME)_$(SYSTEM).tar; else :; fi
	if test -f $(EXPORT)/$(PKG_NAME)_$(SYSTEM).tar.Z; then \
           rm $(EXPORT)/$(PKG_NAME)_$(SYSTEM).tar.Z; else :; fi
	$(TAR_IN) $(EXPORT)/$(PKG_NAME)_$(SYSTEM).tar $(SOURCE_FILES) \
            $(BUILT_FILES) $(DATE_STAMP) .BUILT
	compress -v $(EXPORT)/$(PKG_NAME)_$(SYSTEM).tar

#  export_source: Export the source.
#  --------------------------------
#
#  Export the source files only to a new user.

#  This target depends on the resulting compressed tar file being up to
#  date.

export_source: $(EXPORT)/$(PKG_NAME).tar.Z
	@ echo
	@ echo \
'*** Export copy of the $(PKG_NAME) package source is in the compressed'
	@ echo \
'    tar file $(EXPORT)/$(PKG_NAME).tar.Z'
	@ echo

#  The compressed tar file is up to date if it exists and is more
#  recent than all the source files.

$(EXPORT)/$(PKG_NAME).tar.Z: $(SOURCE_FILES)
#
#  Remove any pre-existing tar files before creating new ones.
	if test -f $(EXPORT)/$(PKG_NAME).tar; then \
           rm $(EXPORT)/$(PKG_NAME).tar; else :; fi
	if test -f $(EXPORT)/$(PKG_NAME).tar.Z; then \
           rm $(EXPORT)/$(PKG_NAME).tar.Z; else :; fi
	$(TAR_IN) $(EXPORT)/$(PKG_NAME).tar $(SOURCE_FILES)
	compress -v $(EXPORT)/$(PKG_NAME).tar

#  clean: Clean up the source directory.
#  ------------------------------------
#
#  Remove all intermediate files.  Do not remove built files.

clean:
	@- rm $(BLOCK_DATA) $(IFL_FILES) $(OBJECT_FILES) \
              1>/dev/null 2>/dev/null
	@- rm $(INCLUDE_LINKS) $(APPLICATIONS) 1>/dev/null 2>/dev/null
	@- rm $(F_ROUTINES) $(MONO_SOURCE) $(MONO_OBJECT) $(C_ROUTINES) \
              $(HELP_SOURCE) $(PROG_SOURCE) 1>/dev/null 2>/dev/null
	@ echo
	@ echo '*** Intermediate files removed'
	@ echo

#  unbuild: Reverse the build process.
#  ----------------------------------

#  Remove all intermediate files and all built files, and note that the
#  package is no longer built or tested.

unbuild: clean
	@- rm $(BUILT_FILES) $(DATE_STAMP) 1>/dev/null 2>/dev/null
	@- rm .BUILT 1>/dev/null 2>/dev/null
	@ echo '*** Built files removed'
	@ echo

#-------------------------------------------------------------------------------

#  Include file dependencies.
#  =========================

#  Object file dependencies on include files (or links to those include
#  files).  These are normally generated automatically from the source
#  files.

#
#  i90 include file dependencies
i90_dat: I90_COM I90_PAR
#
#  i90 library include dependencies
i90_init.o: I90_COM I90_PAR
i90_test.o: I90_DAT SAE_PAR
#
#  ira library include dependencies
drexam1.o: DAT_PAR IRA_PAR NDF_PAR SAE_PAR
drexam2.o: DAT_PAR IRA_PAR NDF_PAR SAE_PAR
drexam3.o: DAT_PAR IRA_PAR NDF_PAR SAE_PAR
drexam4.o: DAT_PAR IRA_PAR NDF_PAR SAE_PAR
drexam5.o: DAT_PAR IRA_PAR NDF_PAR SAE_PAR
ira1_aito.o: DAT_PAR IRA_ERR IRA_PAR PRM_PAR SAE_PAR
ira1_aitot.o: DAT_PAR IRA_ERR IRA_PAR SAE_PAR
ira1_ascre.o: DAT_PAR IRA_ERR IRA_PAR SAE_PAR
ira1_asdef.o: IRA_PAR SAE_PAR
ira1_asndf.o: DAT_PAR IRA_COM IRA_ERR IRA_PAR SAE_PAR
ira1_asres.o: DAT_PAR SAE_PAR
ira1_asset.o: DAT_PAR SAE_PAR
ira1_axlm.o: DAT_PAR IRA_COM IRA_PAR PRM_PAR SAE_PAR
ira1_axlp.o: DAT_PAR IRA_COM IRA_PAR PRM_PAR SAE_PAR
ira1_bbox.o: DAT_PAR SAE_PAR
ira1_bgch.o: DAT_PAR SAE_PAR
ira1_check.o: DAT_PAR IRA_COM IRA_ERR IRA_PAR SAE_PAR
ira1_chprj.o: DAT_PAR IRA_ERR IRA_PAR SAE_PAR
ira1_chscs.o: DAT_PAR IRA_ERR IRA_PAR PRM_PAR SAE_PAR
ira1_curve.o: DAT_PAR PRM_PAR SAE_PAR
ira1_decod.o: DAT_PAR IRA_ERR SAE_PAR
ira1_delob.o: DAT_PAR SAE_PAR
ira1_drms.o: DAT_PAR IRA_PAR SAE_PAR
ira1_drps.o: DAT_PAR IRA_PAR SAE_PAR
ira1_ecec.o: DAT_PAR PRM_PAR SAE_PAR
ira1_eglb.o: DAT_PAR IRA_COM IRA_PAR SAE_PAR
ira1_egtk.o: DAT_PAR SAE_PAR
ira1_eqecl.o: DAT_PAR PRM_PAR SAE_PAR
ira1_eqeq.o: DAT_PAR PRM_PAR SAE_PAR
ira1_eqgal.o: DAT_PAR PRM_PAR SAE_PAR
ira1_fndb.o: DAT_PAR PRM_PAR SAE_PAR
ira1_fngp3.o: DAT_PAR IRA_PAR SAE_PAR
ira1_fpars.o: DAT_PAR IRA_ERR IRA_PAR SAE_PAR
ira1_ftol.o: DAT_PAR SAE_PAR
ira1_galec.o: DAT_PAR PRM_PAR SAE_PAR
ira1_getid.o: DAT_PAR IRA_COM IRA_ERR IRA_PAR SAE_PAR
ira1_gnom.o: DAT_PAR IRA_ERR IRA_PAR PRM_PAR SAE_PAR
ira1_gnomt.o: DAT_PAR IRA_ERR IRA_PAR SAE_PAR
ira1_iconv.o: DAT_PAR IRA_ERR IRA_PAR PRM_PAR SAE_PAR
ira1_ictd1.o: DAT_PAR IRA_ERR IRA_PAR PRM_PAR SAE_PAR
ira1_idrva.o: DAT_PAR IRA_COM IRA_PAR SAE_PAR
ira1_idst2.o: DAT_PAR IRA_PAR PRM_PAR SAE_PAR
ira1_idtc1.o: DAT_PAR IRA_ERR IRA_PAR PRM_PAR SAE_PAR
ira1_igtc1.o: DAT_PAR IRA_ERR IRA_PAR PAR_ERR PRM_PAR SAE_PAR
ira1_init.o: DAT_PAR IRA_COM IRA_PAR PRM_PAR SAE_PAR
ira1_ipaco.o: DAT_PAR IRA_ERR IRA_PAR PRM_PAR SAE_PAR
ira1_iprj.o: DAT_PAR IRA_ERR IRA_PAR PRM_PAR SAE_PAR
ira1_iscnm.o: DAT_PAR IRA_ERR IRA_PAR SAE_PAR
ira1_ivald.o: DAT_PAR IRA_ERR IRA_PAR PRM_PAR SAE_PAR
ira1_lacc.o: DAT_PAR IRA_ERR IRA_PAR SAE_PAR
ira1_lamb.o: DAT_PAR IRA_ERR IRA_PAR PRM_PAR SAE_PAR
ira1_lambt.o: DAT_PAR IRA_ERR IRA_PAR SAE_PAR
ira1_limit.o: IRA_ERR PRM_PAR SAE_PAR
ira1_llex.o: DAT_PAR IRA_PAR PRM_PAR SAE_PAR
ira1_map.o: DAT_PAR IRA_COM IRA_PAR PRM_PAR SAE_PAR TRN_ERR
ira1_map1.o: DAT_PAR IRA_COM IRA_PAR SAE_PAR
ira1_map2.o: DAT_PAR IRA_COM IRA_PAR SAE_PAR
ira1_map3.o: DAT_PAR IRA_COM IRA_PAR SAE_PAR
ira1_nrval.o: DAT_PAR PRM_PAR SAE_PAR
ira1_optid.o: DAT_PAR IRA_PAR SAE_PAR
ira1_orth.o: DAT_PAR IRA_ERR IRA_PAR PRM_PAR SAE_PAR
ira1_plot.o: DAT_PAR PRM_PAR SAE_PAR
ira1_plst.o: DAT_PAR PRM_PAR SAE_PAR
ira1_potl.o: DAT_PAR SAE_PAR
ira1_potu.o: DAT_PAR PRM_PAR SAE_PAR
ira1_prec.o: DAT_PAR PRM_PAR SAE_PAR
ira1_scac.o: DAT_PAR SAE_PAR
ira1_sfld.o: DAT_PAR IRA_ERR SAE_PAR
ira1_shapp.o: DAT_PAR IRA_ERR IRA_PAR PRM_PAR SAE_PAR
ira1_shcal.o: DAT_PAR IRA_ERR IRA_PAR PRM_PAR SAE_PAR
ira1_sortd.o: DAT_PAR PRM_PAR SAE_PAR
ira1_stokn.o: DAT_PAR SAE_PAR
ira1_tpot.o: DAT_PAR SAE_PAR
ira1_trab.o: DAT_PAR SAE_PAR
ira1_tref.o: DAT_PAR IRA_COM IRA_PAR SAE_PAR
ira1_usym.o: DAT_PAR SAE_PAR
ira1_vaito.o: DAT_PAR IRA_ERR IRA_PAR PRM_PAR SAE_PAR
ira1_vgnom.o: DAT_PAR IRA_ERR IRA_PAR PRM_PAR SAE_PAR
ira1_vlamb.o: DAT_PAR IRA_ERR IRA_PAR PRM_PAR SAE_PAR
ira1_vorth.o: DAT_PAR IRA_ERR IRA_PAR PRM_PAR SAE_PAR
ira_activ.o: DAT_PAR IRA_COM IRA_PAR SAE_PAR
ira_annul.o: DAT_PAR IRA_COM IRA_PAR SAE_PAR
ira_close.o: DAT_PAR IRA_COM IRA_PAR SAE_PAR
ira_convt.o: DAT_PAR IRA_PAR SAE_PAR
ira_creat.o: DAT_PAR IRA_COM IRA_ERR IRA_PAR PRM_PAR SAE_PAR
ira_cretr.o: DAT_PAR IRA_COM IRA_ERR IRA_PAR PRM_PAR SAE_PAR
ira_ctod.o: DAT_PAR IRA_ERR IRA_PAR SAE_PAR
ira_ctod1.o: DAT_PAR IRA_ERR IRA_PAR PRM_PAR SAE_PAR
ira_demo1.o: DAT_PAR IRA_PAR NDF_PAR PRM_PAR SAE_PAR
ira_demo2.o: DAT_PAR IRA_PAR SAE_PAR
ira_dist.o: DAT_PAR IRA_PAR PRM_PAR SAE_PAR
ira_dist2.o: DAT_PAR IRA_PAR PRM_PAR SAE_PAR
ira_drbnd.o: DAT_PAR IRA_COM IRA_ERR IRA_PAR SAE_PAR
ira_drbrk.o: DAT_PAR IRA_COM IRA_PAR PRM_PAR SAE_PAR
ira_drgrd.o: DAT_PAR IRA_COM IRA_ERR IRA_PAR SAE_PAR
ira_drgtc.o: DAT_PAR IRA_COM IRA_ERR IRA_PAR PRM_PAR SAE_PAR
ira_drmer.o: DAT_PAR IRA_COM IRA_ERR IRA_PAR PRM_PAR SAE_PAR
ira_drops.o: DAT_PAR IRA_COM IRA_PAR SAE_PAR
ira_dropt.o: DAT_PAR IRA_COM IRA_PAR SAE_PAR
ira_drpar.o: DAT_PAR IRA_COM IRA_ERR IRA_PAR PRM_PAR SAE_PAR
ira_drval.o: DAT_PAR IRA_ERR IRA_PAR PRM_PAR SAE_PAR
ira_drvpo.o: DAT_PAR IRA_COM IRA_PAR PRM_PAR SAE_PAR
ira_dtoc.o: DAT_PAR IRA_ERR IRA_PAR SAE_PAR
ira_dtoc1.o: DAT_PAR IRA_ERR IRA_PAR PRM_PAR SAE_PAR
ira_exprt.o: DAT_PAR SAE_PAR
ira_find.o: DAT_PAR IRA_ERR IRA_PAR SAE_PAR
ira_getco.o: DAT_PAR IRA_PAR SAE_PAR
ira_geteq.o: DAT_PAR IRA_ERR IRA_PAR PRM_PAR SAE_PAR
ira_gtco1.o: DAT_PAR IRA_ERR IRA_PAR SAE_PAR
ira_gtscs.o: DAT_PAR IRA_ERR IRA_PAR SAE_PAR
ira_imprt.o: DAT_PAR IRA_COM IRA_ERR IRA_PAR SAE_PAR
ira_init.o: DAT_PAR IRA_COM IRA_PAR SAE_PAR
ira_iproj.o: DAT_PAR IRA_PAR SAE_PAR
ira_iscs.o: DAT_PAR IRA_ERR IRA_PAR SAE_PAR
ira_locat.o: DAT_PAR IRA_COM IRA_PAR SAE_PAR
ira_mag.o: DAT_PAR IRA_COM IRA_ERR IRA_PAR PRM_PAR SAE_PAR
ira_move.o: DAT_PAR IRA_COM IRA_ERR IRA_PAR PRM_PAR SAE_PAR
ira_norm.o: DAT_PAR IRA_PAR PRM_PAR SAE_PAR
ira_offst.o: DAT_PAR IRA_ERR IRA_PAR PRM_PAR SAE_PAR
ira_pacon.o: DAT_PAR IRA_PAR SAE_PAR
ira_pixsz.o: DAT_PAR IRA_COM IRA_ERR IRA_PAR SAE_PAR
ira_proj.o: DAT_PAR IRA_ERR IRA_PAR PRM_PAR SAE_PAR
ira_read.o: DAT_PAR IRA_COM IRA_ERR IRA_PAR SAE_PAR
ira_rot.o: DAT_PAR IRA_COM IRA_ERR IRA_PAR PRM_PAR SAE_PAR
ira_scnam.o: DAT_PAR IRA_ERR SAE_PAR
ira_scsep.o: DAT_PAR IRA_COM IRA_ERR IRA_PAR SAE_PAR
ira_seteq.o: DAT_PAR IRA_ERR IRA_PAR PRM_PAR SAE_PAR
ira_shift.o: DAT_PAR IRA_ERR IRA_PAR PRM_PAR SAE_PAR
ira_test.o: DAT_PAR IRA_PAR NDF_PAR SAE_PAR
ira_trace.o: DAT_PAR IRA_COM IRA_PAR SAE_PAR MSG_PAR
ira_trans.o: DAT_PAR IRA_COM IRA_PAR SAE_PAR
ira_valid.o: DAT_PAR IRA_COM IRA_PAR PRM_PAR SAE_PAR
ira_write.o: DAT_PAR IRA_COM IRA_ERR IRA_PAR SAE_PAR
ira_xylim.o: DAT_PAR IRA_COM IRA_ERR IRA_PAR PRM_PAR SAE_PAR
msg_loc.o: DAT_ERR DAT_PAR SAE_PAR
#
#  irc library include dependencies
irc1_bclpi.o: DAT_PAR I90_DAT IRC_COM IRC_ERR IRC_PAR SAE_PAR
irc1_bclsb.o: DAT_PAR I90_DAT IRA_PAR IRC_COM IRC_ERR IRC_PAR SAE_PAR
irc1_bposi.o: DAT_PAR I90_DAT IRA_PAR IRC_COM IRC_ERR IRC_PAR SAE_PAR
irc1_bpssb.o: DAT_PAR I90_DAT IRA_PAR IRC_COM IRC_PAR PRM_PAR SAE_PAR
irc1_chkla.o: I90_DAT IRC_ERR IRC_PAR SAE_PAR
irc1_chkun.o: I90_DAT IRC_ERR IRC_PAR SAE_PAR
irc1_clpnt.o: DAT_PAR I90_DAT IRC_COM IRC_ERR IRC_PAR SAE_PAR
irc1_clsat.o: DAT_PAR I90_DAT IRC_COM IRC_PAR SAE_PAR
irc1_dclpi.o: DAT_PAR I90_DAT IRC_COM IRC_ERR IRC_PAR SAE_PAR
irc1_dclsb.o: DAT_PAR I90_DAT IRA_PAR IRC_COM IRC_ERR IRC_PAR SAE_PAR
irc1_distg.o: DAT_PAR I90_DAT IRA_PAR IRC_COM IRC_PAR SAE_PAR
irc1_disti.o: DAT_PAR I90_DAT IRC_COM IRC_ERR IRC_PAR SAE_PAR
irc1_dposi.o: DAT_PAR I90_DAT IRA_PAR IRC_COM IRC_ERR IRC_PAR SAE_PAR
irc1_dpssb.o: DAT_PAR I90_DAT IRA_PAR IRC_COM IRC_PAR PRM_PAR SAE_PAR
irc1_dstsb.o: DAT_PAR I90_DAT IRA_PAR IRC_COM IRC_PAR PRM_PAR SAE_PAR
irc1_getid.o: DAT_PAR I90_DAT IRC_COM IRC_ERR IRC_PAR SAE_PAR
irc1_imcoi.o: DAT_PAR I90_DAT IRA_PAR IRC_COM IRC_ERR IRC_PAR PRM_PAR SAE_PAR
irc1_offsb.o: DAT_PAR I90_DAT IRA_PAR IRC_COM IRC_PAR PRM_PAR SAE_PAR
irc1_offsi.o: DAT_PAR I90_DAT IRC_COM IRC_ERR IRC_PAR SAE_PAR
irc1_pmpsb.o: DAT_PAR I90_DAT IRA_PAR IRC_COM IRC_PAR SAE_PAR
irc1_pntsb.o: DAT_PAR I90_DAT IRA_PAR IRC_COM IRC_ERR IRC_PAR SAE_PAR
irc1_satci.o: DAT_PAR I90_DAT IRA_PAR IRC_COM IRC_ERR IRC_PAR SAE_PAR
irc1_satsb.o: SAE_PAR
irc1_sbfit.o: DAT_PAR I90_DAT IRA_PAR IRC_COM IRC_ERR IRC_PAR PRM_PAR SAE_PAR
irc1_scosb.o: DAT_PAR I90_DAT IRA_PAR IRC_COM IRC_PAR PRM_PAR SAE_PAR
irc1_tr2sb.o: DAT_PAR I90_DAT IRA_PAR IRC_ERR IRC_PAR PRM_PAR SAE_PAR MSG_PAR
irc1_traci.o: DAT_PAR I90_DAT IRC_COM IRC_ERR IRC_PAR SAE_PAR
irc1_trasb.o: DAT_PAR I90_DAT IRC_COM IRC_PAR SAE_PAR
irc_annul.o: DAT_PAR I90_DAT IRC_COM IRC_ERR IRC_PAR NDF_PAR SAE_PAR
irc_bclap.o: DAT_PAR I90_DAT IRC_COM IRC_ERR IRC_PAR PRM_PAR SAE_PAR
irc_bpos.o: DAT_PAR I90_DAT IRA_PAR IRC_COM IRC_ERR IRC_PAR SAE_PAR
irc_close.o: DAT_ERR DAT_PAR I90_DAT IRC_COM IRC_PAR SAE_PAR
irc_dclap.o: DAT_PAR I90_DAT IRC_COM IRC_ERR IRC_PAR PRM_PAR SAE_PAR
irc_detin.o: DAT_PAR I90_DAT IRC_COM IRC_ERR IRC_PAR PRM_PAR SAE_PAR
irc_detno.o: DAT_PAR I90_DAT IRC_COM IRC_ERR IRC_PAR PRM_PAR SAE_PAR
irc_dinds.o: DAT_PAR I90_DAT IRC_COM IRC_ERR IRC_PAR PRM_PAR SAE_PAR
irc_dist.o: DAT_PAR I90_DAT IRC_COM IRC_ERR IRC_PAR PRM_PAR SAE_PAR
irc_dpos.o: DAT_PAR I90_DAT IRA_PAR IRC_COM IRC_ERR IRC_PAR SAE_PAR
irc_fpco1.o: DAT_PAR I90_DAT IRA_PAR IRC_COM IRC_ERR IRC_PAR PRM_PAR SAE_PAR
irc_fpco2.o: DAT_PAR I90_DAT IRA_PAR IRC_COM IRC_ERR IRC_PAR PRM_PAR SAE_PAR
irc_ilab.o: I90_DAT IRC_ERR IRC_PAR SAE_PAR
irc_imco.o: DAT_PAR I90_DAT IRC_COM IRC_ERR IRC_PAR SAE_PAR
irc_imprt.o: DAT_PAR I90_DAT IRC_COM IRC_ERR IRC_PAR NDF_PAR PRM_PAR SAE_PAR
irc_info.o: DAT_PAR I90_DAT IRC_COM IRC_ERR IRC_PAR SAE_PAR
irc_init.o: DAT_PAR I90_DAT IRC_COM IRC_PAR SAE_PAR
irc_iunit.o: I90_DAT IRC_ERR IRC_PAR SAE_PAR
irc_limit.o: DAT_PAR I90_DAT IRA_PAR IRC_COM IRC_ERR IRC_PAR PRM_PAR SAE_PAR
irc_offst.o: DAT_PAR I90_DAT IRC_COM IRC_ERR IRC_PAR PRM_PAR SAE_PAR
irc_posmp.o: DAT_PAR I90_DAT IRC_COM IRC_ERR IRC_PAR SAE_PAR
irc_satco.o: DAT_PAR I90_DAT IRA_PAR IRC_COM IRC_ERR IRC_PAR SAE_PAR
irc_simul.o: DAT_PAR I90_DAT IRC_COM IRC_ERR IRC_PAR SAE_PAR
irc_skco1.o: DAT_PAR I90_DAT IRA_PAR IRC_COM IRC_ERR IRC_PAR PRM_PAR SAE_PAR
irc_skco2.o: DAT_PAR I90_DAT IRA_PAR IRC_COM IRC_ERR IRC_PAR PRM_PAR SAE_PAR
irc_supp.o: DAT_PAR I90_DAT IRC_COM IRC_ERR IRC_PAR SAE_PAR
irc_test.o: I90_DAT IRC_ERR IRC_PAR SAE_PAR
irc_trace.o: DAT_PAR I90_DAT IRA_PAR IRC_COM IRC_ERR IRC_PAR SAE_PAR MSG_PAR
irc_trunc.o: DAT_PAR I90_DAT IRC_COM IRC_ERR IRC_PAR SAE_PAR
#
#  iri library include dependencies
iri_check.o: IRI_PAR SAE_PAR
iri_getun.o: IRI_PAR SAE_PAR
iri_new.o: DAT_PAR IRI_ERR IRI_PAR NDF_PAR SAE_PAR
iri_old.o: DAT_PAR IRI_ERR IRI_PAR NDF_PAR SAE_PAR
iri_test.o: DAT_PAR IRA_PAR IRI_PAR SAE_PAR
iri_trace.o: DAT_PAR I90_DAT IRA_PAR IRI_ERR IRI_PAR MSG_PAR SAE_PAR
#
#  irm library include dependencies
irm1_bldat.o: IRM_COM
irm1_ghlpi.o: IRM_COM SAE_PAR
irm1_lunit.o: SAE_PAR
irm1_namtr.o: SAE_PAR
irm1_rglmt.o: SAE_PAR
irm1_scrsz.o: SAE_PAR
irm1_setip.o: PRM_PAR SAE_PAR
irm1_split.o: SAE_PAR
irm1_statr.o: NUM_DEC_CVT NUM_DEF_CVT PRM_PAR SAE_PAR
irm1_wrtbx.o: SAE_PAR
irm_agfnd.o: DAT_PAR SAE_PAR
irm_agref.o: DAT_PAR SAE_PAR
irm_antso.o: SAE_PAR
irm_asfio.o: PAR_ERR SAE_PAR
irm_averg.o: PRM_PAR SAE_PAR
irm_biln1.o: PRM_PAR SAE_PAR
irm_biln2.o: PRM_PAR SAE_PAR
irm_blocr.o: PRM_PAR SAE_PAR
irm_cff2r.o: MSG_PAR NUM_DEC_CVT NUM_DEF_CVT PRM_PAR SAE_PAR
irm_chkob.o: DAT_PAR SAE_PAR
irm_clean.o: PRM_PAR SAE_PAR
irm_comnt.o: SAE_PAR
irm_crtyp.o: SAE_PAR
irm_cvdet.o: I90_DAT SAE_PAR
irm_delob.o: DAT_PAR SAE_PAR
irm_dimen.o: DAT_PAR IRA_PAR SAE_PAR
irm_ellip.o: IRA_PAR PRM_PAR SAE_PAR
irm_filnm.o: SAE_PAR
irm_fit2.o: PRM_PAR SAE_PAR
irm_gdtyp.o: GNS_PAR SAE_PAR
irm_getqx.o: IRQ_ERR IRQ_PAR SAE_PAR
irm_gettd.o: SAE_PAR
irm_gkcmt.o: SAE_PAR
irm_gkeyc.o: SAE_PAR
irm_gkeyr.o: SAE_PAR
irm_gropn.o: SAE_PAR
irm_gtast.o: DAT_PAR IRA_PAR MSG_PAR PAR_ERR PRM_PAR SAE_PAR
irm_gtbnd.o: I90_DAT PAR_ERR SAE_PAR
irm_gtcun.o: I90_DAT IRC_PAR SAE_PAR
irm_gtdet.o: GRP_PAR I90_DAT MSG_PAR PAR_ERR PRM_PAR SAE_PAR
irm_gthlp.o: IRM_COM SAE_PAR
irm_hist.o: DAT_PAR SAE_PAR
irm_hmenu.o: SAE_PAR
irm_hmfft.o: SAE_PAR
irm_hmsg.o: SAE_PAR
irm_hmult.o: SAE_PAR
irm_iattr.o: IRM_COM SAE_PAR
irm_linr.o: PRM_PAR SAE_PAR
irm_lintr.o: PRM_PAR SAE_PAR
irm_listn.o: GRP_PAR SAE_PAR
irm_litrr.o: PRM_PAR SAE_PAR
irm_medn.o: PRM_PAR SAE_PAR
irm_mline.o: PRM_PAR SAE_PAR
irm_noise.o: PRM_PAR SAE_PAR
irm_page.o: IRM_COM
irm_profl.o: SAE_PAR
irm_ptast.o: DAT_PAR IRA_PAR SAE_PAR
irm_ptout.o: IRM_COM SAE_PAR
irm_qcol.o: SAE_PAR
irm_qndf.o: DAT_PAR IRQ_ERR IRQ_PAR MSG_PAR SAE_PAR
irm_qntlr.o: SAE_PAR
irm_rdndf.o: GRP_PAR MSG_PAR NDG_ERR PAR_ERR SAE_PAR
irm_readf.o: GRP_ERR GRP_PAR SAE_PAR
irm_rlfft.o: SAE_PAR
irm_skcur.o: PRM_PAR SAE_PAR
irm_solin.o: SAE_PAR
irm_sortd.o: PRM_PAR SAE_PAR
irm_spage.o: IRM_COM
irm_sqflt.o: SAE_PAR
irm_stats.o: PRM_PAR SAE_PAR
irm_staxs.o: SAE_PAR
irm_stgrd.o: SAE_PAR
irm_stlmt.o: SAE_PAR
irm_stlog.o: SAE_PAR
irm_stmap.o: SAE_PAR
irm_stnul.o: PRM_PAR SAE_PAR
irm_stpen.o: GKS_PAR IRM_COM SAE_PAR
irm_stpha.o: SAE_PAR
irm_subst.o: SAE_PAR
irm_table.o: SAE_PAR
irm_td.o: SAE_PAR
irm_test.o: DAT_PAR I90_DAT PRM_PAR SAE_PAR
irm_tmenu.o: SAE_PAR
irm_trcon.o: SAE_PAR
irm_trinv.o: SAE_PAR
irm_untcv.o: I90_DAT IRC_PAR PRM_PAR SAE_PAR
irm_untim.o: DAT_PAR I90_DAT IRI_PAR SAE_PAR
irm_untiv.o: I90_DAT IRI_PAR SAE_PAR
irm_vmenu.o: SAE_PAR
irm_wrndf.o: GRP_PAR MSG_PAR NDG_ERR PAR_ERR SAE_PAR
#
#  irq include file dependencies
irq_opc: IRQ_PAO IRQ_CMO
#
#  irq library include dependencies
irq1_add.o: DAT_PAR IRQ_ERR IRQ_PAR SAE_PAR
irq1_altrp.o: IRQ_ERR IRQ_OPC IRQ_PAR PRM_PAR SAE_PAR
irq1_antmp.o: DAT_PAR SAE_PAR
irq1_check.o: DAT_PAR IRQ_ERR IRQ_PAR SAE_PAR
irq1_cmqm.o: IRQ_OPC IRQ_PAR SAE_PAR
irq1_count.o: PRM_PAR SAE_PAR
irq1_evstk.o: IRQ_OPC IRQ_PAR SAE_PAR
irq1_get.o: DAT_PAR IRQ_ERR IRQ_PAR SAE_PAR
irq1_gtidq.o: DAT_PAR IRQ_COM IRQ_ERR IRQ_PAR SAE_PAR
irq1_iannu.o: DAT_PAR IRQ_COM IRQ_PAR SAE_PAR
irq1_indf.o: IRQ_ERR SAE_PAR
irq1_init.o: DAT_PAR IRQ_COM IRQ_PAR SAE_PAR
irq1_islot.o: DAT_PAR IRQ_ERR IRQ_PAR SAE_PAR
irq1_mod.o: DAT_PAR IRQ_ERR IRQ_PAR SAE_PAR
irq1_ndtov.o: SAE_PAR
irq1_nulop.o: IRQ_OPC IRQ_PAR SAE_PAR
irq1_opand.o: IRQ_ERR IRQ_OPC IRQ_PAR SAE_PAR
irq1_opcin.o: IRQ_CMO IRQ_PAO IRQ_PAR
irq1_qcnt.o: NUM_DEC_CVT NUM_DEF_CVT SAE_PAR
irq1_qlst.o: DAT_PAR SAE_PAR
irq1_qlst2.o: NUM_DEC_CVT NUM_DEF_CVT SAE_PAR
irq1_qmsk.o: NUM_DEC_CVT NUM_DEF_CVT PRM_PAR SAE_PAR
irq1_qset.o: NUM_DEC_CVT NUM_DEF_CVT SAE_PAR
irq1_rbit.o: DAT_PAR IRQ_ERR IRQ_PAR SAE_PAR
irq1_reset.o: DAT_PAR IRQ_ERR IRQ_PAR SAE_PAR
irq1_rslot.o: DAT_PAR IRQ_PAR SAE_PAR
irq1_sbad1.o: IRQ_ERR IRQ_PAR PRM_PAR SAE_PAR
irq1_searc.o: IRQ_ERR IRQ_PAR SAE_PAR
irq1_simpl.o: IRQ_OPC IRQ_PAR SAE_PAR
irq1_sorti.o: SAE_PAR
irq1_space.o: IRQ_ERR IRQ_OPC IRQ_PAR SAE_PAR
irq1_temp.o: DAT_PAR SAE_PAR
irq1_vtofx.o: IRQ_OPC IRQ_PAR SAE_PAR
irq_addqn.o: DAT_PAR IRQ_ERR IRQ_PAR SAE_PAR
irq_annul.o: DAT_PAR IRQ_COM IRQ_ERR IRQ_PAR SAE_PAR
irq_chkqn.o: DAT_PAR IRQ_PAR SAE_PAR
irq_close.o: DAT_PAR IRQ_COM IRQ_ERR IRQ_PAR SAE_PAR
irq_cntq.o: IRQ_ERR IRQ_PAR SAE_PAR
irq_comp.o: DAT_PAR IRQ_COM IRQ_ERR IRQ_PAR SAE_PAR
irq_find.o: DAT_PAR IRQ_ERR IRQ_PAR SAE_PAR
irq_getqn.o: DAT_PAR IRQ_PAR SAE_PAR
irq_new.o: DAT_PAR IRQ_ERR IRQ_PAR SAE_PAR
irq_numqn.o: SAE_PAR
irq_nxtqn.o: IRQ_ERR IRQ_PAR SAE_PAR
irq_remqn.o: DAT_PAR IRQ_ERR IRQ_PAR SAE_PAR
irq_resq.o: IRQ_ERR IRQ_PAR SAE_PAR
irq_resql.o: IRQ_ERR IRQ_PAR NDF_PAR SAE_PAR
irq_resqm.o: IRQ_ERR IRQ_PAR SAE_PAR
irq_rlse.o: DAT_PAR IRQ_PAR SAE_PAR
irq_sbad.o: DAT_PAR IRQ_COM IRQ_ERR IRQ_PAR SAE_PAR
irq_setq.o: IRQ_ERR IRQ_PAR SAE_PAR
irq_setql.o: IRQ_ERR IRQ_PAR NDF_PAR SAE_PAR
irq_setqm.o: IRQ_ERR IRQ_PAR SAE_PAR
irq_syntx.o: DAT_PAR IRQ_PAR SAE_PAR
irq_test.o: DAT_PAR IRQ_ERR IRQ_PAR PRM_PAR SAE_PAR
#
#  ndg library include dependencies
ndf1_a2p.o: DAT_PAR NDF_ERR SAE_PAR
ndf1_chxnm.o: NDF_ERR NDF_PAR SAE_PAR
ndf1_p2a.o: DAT_PAR NDF_ERR SAE_PAR
ndf1_pscpx.o: DAT_PAR NDF_CONST NDF_ERR SAE_PAR
ndf1_psndb.o: DAT_PAR NDF_ERR SAE_PAR
ndf1_psnde.o: DAT_PAR NDF_ERR NDF_PAR SAE_PAR
ndf1_psndf.o: DAT_PAR NDF_ERR SAE_PAR
ndf1_pxlst.o: DAT_PAR NDF_ERR SAE_PAR
ndg1_axlim.o: NDF_ERR NDF_PAR SAE_PAR
ndg1_ffind.o: GRP_PAR NDG_CONST NDG_ERR SAE_PAR
ndg1_fspec.o: GRP_PAR SAE_PAR
ndg1_gtloc.o: DAT_PAR NDF_PAR NDG_COM NDG_CONST SAE_PAR
ndg1_init.o: DAT_PAR NDF_PAR NDG_COM NDG_CONST SAE_PAR
ndg1_lasto.o: SAE_PAR
ndg1_ncut.o: DAT_PAR NDF_ERR NDF_PAR SAE_PAR
ndg1_ndfch.o: DAT_PAR GRP_PAR NDG_CONST NDG_ERR SAE_PAR
ndg1_slice.o: GRP_PAR SAE_PAR
ndg1_stloc.o: DAT_PAR NDF_PAR NDG_COM NDG_CONST SAE_PAR
ndg_annul.o: DAT_PAR SAE_PAR
ndg_asexp.o: GRP_PAR NDG_CONST NDG_ERR SAE_PAR
ndg_assoc.o: GRP_ERR GRP_PAR NDG_ERR PAR_ERR SAE_PAR
ndg_creat.o: GRP_PAR PAR_ERR SAE_PAR
ndg_crexp.o: GRP_PAR NDG_CONST SAE_PAR
ndg_delet.o: DAT_PAR SAE_PAR
ndg_ndfas.o: DAT_PAR GRP_PAR NDF_PAR NDG_CONST SAE_PAR
ndg_ndfcr.o: DAT_PAR GRP_PAR NDF_PAR NDG_CONST SAE_PAR
ndg_ndfpr.o: DAT_PAR GRP_PAR NDF_CONST NDF_PAR NDG_CONST SAE_PAR
ndg_test.o: GRP_PAR SAE_PAR
#
#  apps library include dependencies
agchax.o: SAE_PAR
agchcu.o: SAE_PAR
agchil.o: SAE_PAR
backa0.o: I90_DAT IRC_PAR MSG_PAR PRM_PAR SAE_PAR
backa1.o: DAT_PAR I90_DAT IRA_PAR IRC_PAR MSG_PAR PRM_PAR SAE_PAR
backa2.o: DAT_PAR I90_DAT IRA_PAR IRC_PAR SAE_PAR
backa3.o: DAT_PAR IRQ_ERR MSG_PAR SAE_PAR
backcrdd.o: DAT_PAR GRP_PAR I90_DAT IRC_PAR IRQ_PAR MSG_PAR PAR_ERR \
            PRM_PAR SAE_PAR
ccora0.o: MSG_PAR PRM_PAR SAE_PAR
ccora1.o: MSG_PAR PRM_PAR SAE_PAR
ccora2.o: I90_DAT SAE_PAR
colcorr.o: DAT_PAR I90_DAT IRI_PAR MSG_PAR PAR_ERR SAE_PAR
coltemp.o: DAT_PAR I90_DAT IRI_PAR MSG_PAR PAR_ERR SAE_PAR
cdcra0.o: IRA_PAR PRM_PAR SAE_PAR
cdcra1.o: IRA_PAR PAR_ERR SAE_PAR
cdcra2.o: I90_DAT SAE_PAR
cdcra3.o: SAE_PAR
cdcra4.o: I90_DAT IRA_PAR SAE_PAR
cdcra5.o: SAE_PAR
cdcra6.o: I90_DAT PRM_PAR SAE_PAR
cdcra7.o: I90_PAR PRM_PAR SAE_PAR
cdcra8.o: SAE_PAR
cdcra9.o: DAT_ERR DAT_PAR PAR_PAR PAR_ERR SAE_PAR
cdcrb0.o: PRM_PAR SAE_PAR
cdcrd0.o: PRM_PAR SAE_PAR
coaddcrdd.o: DAT_PAR I90_DAT IRA_PAR IRC_PAR SAE_PAR
ctemz0.o: I90_DAT MSG_PAR PRM_PAR SAE_PAR
ctemz1.o: I90_DAT SAE_PAR
ctemz2.o: MSG_PAR PRM_PAR SAE_PAR
ctemz3.o: I90_DAT SAE_PAR
ctemz4.o: MSG_PAR PRM_PAR SAE_PAR
desta0.o: I90_DAT IRC_PAR MSG_PAR PRM_PAR SAE_PAR
desta1.o: I90_DAT PRM_PAR SAE_PAR
desta3.o: DAT_PAR IRQ_ERR MSG_PAR SAE_PAR
destcrdd.o: DAT_PAR GRP_PAR I90_DAT IRC_PAR IRQ_PAR MSG_PAR PAR_ERR \
            PRM_PAR SAE_PAR
deglcrdd.o: DAT_PAR I90_DAT IRA_PAR IRQ_PAR MSG_PAR PRM_PAR SAE_PAR
dgcra0.o: PRM_PAR SAE_PAR
dgcra1.o: PRM_PAR SAE_PAR
dgcra2.o: PRM_PAR SAE_PAR
dgcra3.o: DAT_PAR IRQ_PAR MSG_PAR SAE_PAR
dgcra4.o: PRM_PAR SAE_PAR
find01.o: SAE_PAR
find02.o: ERR_ERR ERR_PAR FICOMN I90_PAR IRA_ERR IRA_PAR MSG_ERR MSG_PAR \
            PAR_ERR SAE_PAR
find03.o: CHR_ERR ERR_ERR ERR_PAR FICOMN I90_PAR IRA_ERR IRA_PAR MSG_ERR \
            MSG_PAR PAR_ERR SAE_PAR
find04.o: ERR_ERR ERR_PAR FICOMN I90_PAR IRA_ERR IRA_PAR MSG_ERR MSG_PAR \
            PAR_ERR SAE_PAR
find05.o: ERR_ERR ERR_PAR FICOMN I90_PAR IRA_ERR IRA_PAR MSG_ERR MSG_PAR \
            PAR_ERR SAE_PAR
find06.o: ERR_ERR ERR_PAR FICOMN I90_PAR IRA_ERR IRA_PAR MSG_ERR MSG_PAR \
            PAR_ERR SAE_PAR
find07.o: ERR_ERR ERR_PAR FICOMN I90_PAR IRA_ERR IRA_PAR MSG_ERR MSG_PAR \
            PAR_ERR SAE_PAR
find08.o: FICOMN I90_PAR IRA_ERR IRA_PAR SAE_PAR
find09.o: FICOMN I90_PAR IRA_ERR IRA_PAR SAE_PAR
find10.o: ERR_ERR ERR_PAR FICOMN I90_PAR IRA_ERR IRA_PAR PAR_ERR SAE_PAR
find11.o: CHR_ERR ERR_ERR ERR_PAR FICOMN I90_PAR IRA_ERR IRA_PAR MSG_ERR \
            MSG_PAR PAR_ERR SAE_PAR
find12.o: I90_PAR IRA_ERR IRA_PAR SAE_PAR
find13.o: CHR_ERR ERR_ERR ERR_PAR FICOMN I90_PAR IRA_ERR IRA_PAR MSG_ERR \
            MSG_PAR PAR_ERR SAE_PAR
find14.o: ERR_ERR ERR_PAR I90_PAR IRA_ERR IRA_PAR MSG_ERR MSG_PAR PAR_ERR \
            SAE_PAR
find15.o: I90_PAR IRA_ERR IRA_PAR SAE_PAR
find16.o: ERR_ERR ERR_PAR FICOMN I90_PAR IRA_ERR IRA_PAR MSG_ERR MSG_PAR \
            PAR_ERR SAE_PAR
find17.o: SAE_PAR
find18.o: SAE_PAR
find19.o: SAE_PAR
find20.o: DAT_ERR DAT_PAR ERR_ERR ERR_PAR FICOMN FIO_ERR FIO_PAR I90_PAR \
            IRA_ERR IRA_PAR MSG_ERR MSG_PAR PAR_ERR SAE_PAR
find21.o: FICOMN I90_PAR IRA_ERR IRA_PAR SAE_PAR
find22.o: ERR_ERR ERR_PAR I90_PAR IRA_ERR IRA_PAR MSG_ERR MSG_PAR PAR_ERR \
            SAE_PAR
find23.o: CMP_ERR DAT_ERR DAT_PAR ERR_ERR ERR_PAR FICOMN FIO_ERR FIO_PAR \
            I90_PAR IRA_ERR IRA_PAR MSG_ERR MSG_PAR PAR_ERR SAE_PAR
find24.o: ERR_ERR ERR_PAR I90_PAR IRA_ERR IRA_PAR MSG_ERR MSG_PAR PAR_ERR \
            SAE_PAR
find25.o: ERR_ERR ERR_PAR FICOMN I90_PAR IRA_ERR IRA_PAR MSG_ERR MSG_PAR \
            PAR_ERR SAE_PAR
find26.o: ERR_ERR ERR_PAR FICOMN I90_PAR IRA_ERR IRA_PAR MSG_ERR MSG_PAR \
            PAR_ERR SAE_PAR
find27.o: CHR_ERR ERR_ERR ERR_PAR FICOMN I90_PAR IRA_ERR IRA_PAR MSG_ERR \
            MSG_PAR PAR_ERR SAE_PAR
find28.o: ERR_ERR ERR_PAR FICOMN FIO_ERR FIO_PAR I90_PAR IRA_ERR IRA_PAR \
            MSG_ERR MSG_PAR PAR_ERR SAE_PAR
find29.o: ERR_ERR ERR_PAR FICOMN FIO_ERR FIO_PAR I90_PAR IRA_ERR IRA_PAR \
            MSG_ERR MSG_PAR PAR_ERR PSX_ERR SAE_PAR
find30.o: ERR_ERR ERR_PAR I90_PAR IRA_ERR IRA_PAR MSG_ERR MSG_PAR PAR_ERR \
            SAE_PAR
find32.o: SAE_PAR
find33.o: SAE_PAR
find34.o: SAE_PAR
find35.o: SAE_PAR
find36.o: SAE_PAR
find37.o: SAE_PAR
find38.o: DAT_ERR DAT_PAR ERR_ERR ERR_PAR FICOMN FIO_ERR FIO_PAR I90_PAR \
            IRA_ERR IRA_PAR MSG_ERR MSG_PAR PAR_ERR SAE_PAR
find39.o: ERR_ERR ERR_PAR FICOMN I90_PAR IRA_ERR IRA_PAR MSG_ERR MSG_PAR \
            PAR_ERR SAE_PAR
find40.o: CHR_ERR DAT_ERR DAT_PAR ERR_ERR ERR_PAR FICOMN FIO_ERR FIO_PAR \
            I90_PAR IRA_ERR IRA_PAR MSG_ERR MSG_PAR PAR_ERR SAE_PAR
find41.o: SAE_PAR
find42.o: SAE_PAR
find43.o: SAE_PAR
find44.o: ERR_ERR ERR_PAR FICOMN I90_PAR IRA_ERR IRA_PAR MSG_ERR MSG_PAR \
            PAR_ERR SAE_PAR
find45.o: ERR_ERR ERR_PAR FICOMN FIO_ERR FIO_PAR I90_PAR IRA_ERR IRA_PAR \
            MSG_ERR MSG_PAR PAR_ERR PSX_ERR SAE_PAR
find46.o: SAE_PAR
findcrdd.o: ERR_ERR ERR_PAR FICOMN I90_PAR IRA_ERR IRA_PAR MSG_ERR MSG_PAR \
            PAR_ERR SAE_PAR
hista0.o: SAE_PAR
hista1.o: DAT_PAR SAE_PAR
hista2.o: DAT_PAR SAE_PAR
hista3.o: SAE_PAR
histb0.o: SAE_PAR
histb1.o: DAT_PAR SAE_PAR
histc0.o: MSG_PAR SAE_PAR
history.o: DAT_PAR GRP_PAR MSG_PAR PAR_ERR PRM_PAR SAE_PAR
i90help.o: PAR_ERR SAE_PAR
ifield.o: IRA_PAR SAE_PAR SUBPAR_PAR
ifldz0.o: IRA_ERR IRA_PAR SAE_PAR
ifldz1.o: IRA_ERR IRA_PAR SAE_PAR
ifldz2.o: IRA_PAR SAE_PAR
ifldz3.o: IRA_PAR SAE_PAR
irastrace.o: DAT_PAR IRI_PAR MSG_PAR PAR_ERR SAE_PAR TRA_COM
itraa0.o: MSG_PAR SAE_PAR TRA_COM
makeprof.o: I90_DAT SAE_PAR
makeresp.o: I90_DAT SAE_PAR
mapca0.o: DAT_PAR IRA_PAR SAE_PAR
mapca1.o: GRP_PAR I90_DAT SAE_PAR
mapca2.o: I90_DAT MSG_PAR PRM_PAR SAE_PAR
mapca3.o: SAE_PAR
mapca5.o: DAT_PAR GRP_PAR I90_DAT IRA_PAR MSG_PAR SAE_PAR
mapca6.o: DAT_PAR I90_DAT IRA_PAR SAE_PAR
mapca7.o: I90_DAT IRC_PAR IRI_PAR SAE_PAR
mapcb0.o: MSG_PAR PRM_PAR SAE_PAR
mapcb1.o: MSG_PAR PRM_PAR SAE_PAR
mapcb2.o: MSG_PAR PRM_PAR SAE_PAR
mapcb3.o: MSG_PAR PRM_PAR SAE_PAR
mapcb4.o: MSG_PAR SAE_PAR
mapcb5.o: MSG_PAR SAE_PAR
mapcb6.o: MSG_PAR SAE_PAR
mapcb7.o: MSG_PAR SAE_PAR
mapcc0.o: I90_DAT SAE_PAR
mapcc2.o: PRM_PAR SAE_PAR
mapcc3.o: PRM_PAR SAE_PAR
mapcc4.o: PRM_PAR SAE_PAR
mapcc5.o: PRM_PAR SAE_PAR
mapcc6.o: SAE_PAR
mapcc7.o: SAE_PAR
mapcc8.o: SAE_PAR
mapcc9.o: SAE_PAR
mapcd0.o: MSG_PAR PRM_PAR SAE_PAR
mapcd1.o: MSG_PAR PRM_PAR SAE_PAR
mapcd2.o: MSG_PAR SAE_PAR
mapcd3.o: MSG_PAR SAE_PAR
mapcf0.o: SAE_PAR
mapcf1.o: SAE_PAR
mapcf2.o: DAT_PAR IRA_PAR IRI_PAR SAE_PAR
mapcrdd.o: DAT_PAR I90_DAT IRA_PAR IRI_PAR MSG_PAR NDF_PAR PAR_ERR SAE_PAR
mproa0.o: SAE_PAR
mresa0.o: SAE_PAR
newua0.o: I90_DAT IRC_PAR IRI_PAR SAE_PAR
newua1.o: PRM_PAR SAE_PAR
newua2.o: PRM_PAR SAE_PAR
newunits.o: DAT_PAR GRP_PAR I90_DAT IRC_PAR IRI_PAR MSG_PAR PAR_ERR SAE_PAR
poina0.o: MSG_PAR SAE_PAR
poina1.o: MSG_PAR PAR_ERR PAR_PAR SAE_PAR
poina2.o: IRA_ERR IRA_PAR MSG_PAR PAR_ERR PAR_PAR PRM_PAR SAE_PAR
poina3.o: I90_DAT IRA_ERR IRA_PAR MSG_PAR PAR_ERR PAR_PAR PRM_PAR SAE_PAR
poina4.o: I90_DAT PRM_PAR SAE_PAR
poina5.o: PRM_PAR SAE_PAR
poina6.o: I90_DAT IRA_ERR IRA_PAR MSG_PAR PAR_ERR PAR_PAR PRM_PAR SAE_PAR
poina7.o: MSG_PAR SAE_PAR
poina8.o: PRM_PAR SAE_PAR
poina9.o: PRM_PAR SAE_PAR
poinb0.o: I90_DAT SAE_PAR
poinb1.o: IRA_PAR SAE_PAR
poinb2.o: IRA_PAR SAE_PAR
poinb3.o: MSG_PAR SAE_PAR
poinb4.o: MSG_PAR SAE_PAR
poinc0.o: I90_DAT SAE_PAR
poinc1.o: SAE_PAR
poinc2.o: PRM_PAR SAE_PAR
poinc3.o: PRM_PAR SAE_PAR
poinc4.o: SAE_PAR
poind0.o: PRM_PAR SAE_PAR
poind1.o: I90_DAT PRM_PAR SAE_PAR
pointcrdd.o: I90_DAT IRA_PAR MSG_PAR PAR_ERR PRM_PAR SAE_PAR
posca0.o: DAT_PAR I90_DAT IRA_PAR PRM_PAR SAE_PAR
posca1.o: PRM_PAR SAE_PAR
posca2.o: DAT_PAR GRP_PAR IRA_PAR PRM_PAR SAE_PAR
posca3.o: MSG_PAR SAE_PAR
posca4.o: DAT_PAR IRA_PAR SAE_PAR
poscrdd.o: DAT_PAR IRA_PAR MSG_PAR PAR_ERR SAE_PAR
prepa0.o: GRP_PAR MSG_PAR PAR_ERR SAE_PAR
prepa1.o: DAT_PAR GRP_PAR IRA_PAR SAE_PAR
prepa2.o: GRP_PAR SAE_PAR
prepa3.o: DAT_PAR GRP_PAR IRI_PAR SAE_PAR
prepa4.o: IRI_PAR MSG_PAR SAE_PAR
prepa5.o: IRI_PAR SAE_PAR
prepa6.o: SAE_PAR
prepa7.o: DAT_PAR IRI_ERR IRI_PAR MSG_PAR NDF_PAR SAE_PAR
prepa8.o: I90_DAT SAE_PAR
prepare.o: DAT_PAR GRP_PAR IRA_PAR IRI_PAR MSG_PAR NDF_PAR PAR_ERR SAE_PAR
prepb0.o: IRI_PAR NDF_PAR SAE_PAR
prepb1.o: DAT_PAR IRA_PAR IRI_PAR MSG_PAR SAE_PAR
prepb2.o: SAE_PAR
prepb3.o: SAE_PAR
prepb4.o: SAE_PAR
prepb5.o: SAE_PAR
prepb6.o: IRI_PAR SAE_PAR
prepb7.o: SAE_PAR
prepb8.o: SAE_PAR
prepb9.o: SAE_PAR
prepc0.o: I90_DAT IRI_PAR MSG_PAR PAR_ERR SAE_PAR
prepc1.o: PRM_PAR SAE_PAR
prepc2.o: GRP_PAR MSG_PAR SAE_PAR
prepc3.o: DAT_PAR IRI_PAR SAE_PAR
prepc4.o: DAT_PAR IRI_PAR SAE_PAR
prepc6.o: IRI_PAR SAE_PAR
prepc7.o: SAE_PAR
qualtobad.o: DAT_PAR GRP_PAR IRQ_ERR IRQ_PAR MSG_PAR PAR_ERR SAE_PAR
remqual.o: DAT_PAR GRP_PAR IRQ_ERR IRQ_PAR MSG_PAR PAR_ERR SAE_PAR
salia0.o: DAT_PAR MSG_PAR PRM_PAR SAE_PAR
salia1.o: IRA_PAR PRM_PAR SAE_PAR
salia2.o: PRM_PAR SAE_PAR
salia3.o: PRM_PAR SAE_PAR
salia4.o: PRM_PAR SAE_PAR
salia5.o: PRM_PAR SAE_PAR
salia6.o: PRM_PAR SAE_PAR
sboxa0.o: MSG_PAR PRM_PAR SAE_PAR
sboxa1.o: MSG_PAR PRM_PAR SAE_PAR
sboxa2.o: IRA_PAR MSG_PAR PRM_PAR SAE_PAR
sboxb0.o: MSG_PAR PRM_PAR SAE_PAR
setimage.o: DAT_PAR GRP_PAR I90_DAT IRI_PAR MSG_PAR PAR_ERR SAE_PAR
setqa0.o: GRP_PAR SAE_PAR
setqa1.o: PRM_PAR SAE_PAR
setqual.o: DAT_PAR GRP_PAR IRQ_ERR IRQ_PAR MSG_PAR NDF_PAR PAR_ERR \
           PRM_PAR SAE_PAR
showqual.o: DAT_PAR IRQ_ERR IRQ_PAR MSG_PAR PAR_ERR SAE_PAR
simca0.o: I90_DAT SAE_PAR
simca1.o: DAT_PAR I90_DAT IRA_PAR IRC_PAR MSG_PAR SAE_PAR
simca2.o: GRP_PAR I90_DAT SAE_PAR
simcb0.o: DAT_PAR I90_DAT MSG_PAR PRM_PAR SAE_PAR
simcb1.o: DAT_PAR I90_DAT SAE_PAR
simcc5.o: PRM_PAR SAE_PAR
simcc6.o: PRM_PAR SAE_PAR
simcf0.o: PRM_PAR SAE_PAR
simcf1.o: SAE_PAR
simcrdd.o: DAT_PAR GRP_PAR I90_DAT IRI_PAR MSG_PAR PAR_ERR SAE_PAR
skyalign.o: GRP_PAR IRA_PAR MSG_PAR NDF_PAR PAR_ERR PRM_PAR SAE_PAR
skybox.o: AGI_ERR AGI_PAR IRA_PAR MSG_PAR PAR_ERR SAE_PAR
skygrid.o: AGI_ERR DAT_PAR GNS_PAR IRA_PAR MSG_PAR PAR_ERR SAE_PAR
skyline.o: AGI_ERR AGI_PAR DAT_PAR GNS_PAR IRA_PAR MSG_PAR PAR_ERR SAE_PAR
skymark.o: AGI_ERR AGI_PAR DAT_PAR IRA_PAR MSG_PAR PAR_ERR SAE_PAR
skyphot.o: DAT_PAR GRP_PAR IRA_PAR IRI_PAR MSG_PAR PAR_ERR PRM_PAR SAE_PAR
skypos.o: DAT_PAR GRP_PAR IRA_PAR MSG_PAR PAR_ERR PRM_PAR SAE_PAR
skywrite.o: AGI_ERR AGI_PAR GNS_PAR IRA_PAR MSG_PAR PAR_ERR SAE_PAR
slina1.o: PAR_ERR SAE_PAR
slina2.o: GRP_ERR GRP_PAR IRA_ERR IRA_PAR SAE_PAR
slina3.o: SAE_PAR
slina4.o: MSG_PAR SAE_PAR
slinc0.o: DAT_PAR IRA_PAR MSG_PAR PAR_ERR SAE_PAR
slinc1.o: DAT_PAR IRA_PAR MSG_PAR PAR_ERR SAE_PAR
slinc2.o: DAT_PAR IRA_PAR MSG_PAR PAR_ERR SAE_PAR
slinc3.o: DAT_PAR IRA_PAR MSG_PAR PAR_ERR PRM_PAR SAE_PAR
sline0.o: DAT_PAR IRA_PAR SAE_PAR
sline1.o: DAT_PAR IRA_PAR SAE_PAR
sline2.o: DAT_PAR IRA_PAR SAE_PAR
sline3.o: DAT_PAR IRA_PAR SAE_PAR
slinf0.o: DAT_PAR IRA_PAR SAE_PAR
slinf1.o: DAT_PAR IRA_PAR SAE_PAR
slinf2.o: DAT_PAR IRA_PAR SAE_PAR
slinf3.o: SAE_PAR
sling0.o: GRP_PAR SAE_PAR
sling1.o: DAT_PAR GRP_PAR IRA_PAR SAE_PAR
sling2.o: DAT_PAR GRP_PAR IRA_PAR SAE_PAR
sling3.o: DAT_PAR GRP_PAR IRA_PAR SAE_PAR
sling4.o: GRP_PAR SAE_PAR
smara0.o: MSG_PAR PRM_PAR SAE_PAR
smara1.o: DAT_PAR GRP_ERR GRP_PAR IRA_ERR IRA_PAR SAE_PAR
smara2.o: SAE_PAR
smara3.o: DAT_PAR IRA_PAR PRM_PAR SAE_PAR
smarb0.o: PRM_PAR SAE_PAR
sphoa0.o: DAT_PAR GRP_ERR GRP_PAR IRA_ERR IRA_PAR SAE_PAR
sphob0.o: PAR_ERR SAE_PAR
sphob1.o: AGI_ERR MSG_PAR SAE_PAR
sphoz1.o: IRA_PAR MSG_PAR PRM_PAR SAE_PAR
sphoz2.o: IRA_PAR MSG_PAR PRM_PAR SAE_PAR
sposa0.o: DAT_PAR GRP_ERR GRP_PAR IRA_ERR IRA_PAR SAE_PAR
sposb0.o: PAR_ERR SAE_PAR
sposb1.o: AGI_ERR MSG_PAR SAE_PAR
sposb2.o: PAR_ERR SAE_PAR
sposb3.o: DAT_PAR IRA_PAR MSG_PAR PRM_PAR SAE_PAR
sposb4.o: IRA_PAR MSG_PAR PRM_PAR SAE_PAR
swria0.o: MSG_PAR PAR_ERR SAE_PAR SKY_PAR
swria1.o: SAE_PAR
swria2.o: PAR_ERR SAE_PAR
swria3.o: SAE_PAR
swria4.o: SAE_PAR
swria5.o: IRA_PAR MSG_PAR PRM_PAR SAE_PAR
swrib0.o: IRA_PAR MSG_PAR PAR_ERR PRM_PAR SAE_PAR
swrib1.o: IRA_PAR MSG_PAR PAR_ERR PRM_PAR SAE_PAR
swric0.o: GRP_PAR IRA_ERR IRA_PAR SAE_PAR SKY_PAR
swrie0.o: IRA_PAR SAE_PAR SKY_PAR
swrif0.o: SAE_PAR
swrif1.o: SAE_PAR SKY_PAR
traca1.o: DAT_PAR I90_DAT IRA_PAR SAE_PAR
traca2.o: PRM_PAR SAE_PAR
traca4.o: SAE_PAR
traca5.o: PRM_PAR SAE_PAR
traca6.o: DAT_PAR IRA_PAR PRM_PAR SAE_PAR
traca7.o: DAT_PAR I90_DAT IRA_PAR SAE_PAR
traca8.o: I90_DAT IRC_PAR IRQ_PAR MSG_PAR PAR_ERR SAE_PAR
traca9.o: DAT_PAR IRA_PAR MSG_PAR SAE_PAR
tracb0.o: DAT_PAR IRA_PAR PRM_PAR SAE_PAR
tracb1.o: PRM_PAR SAE_PAR
tracb2.o: PRM_PAR SAE_PAR
tracb3.o: DAT_PAR IRA_PAR MSG_PAR PRM_PAR SAE_PAR
tracb4.o: PRM_PAR SAE_PAR
tracb5.o: PRM_PAR SAE_PAR
tracb6.o: SAE_PAR
tracb9.o: PRM_PAR SAE_PAR
tracc1.o: DAT_PAR IRA_PAR SAE_PAR
tracc2.o: GRP_PAR MSG_PAR PAR_ERR SAE_PAR
tracc3.o: DAT_PAR IRQ_ERR IRQ_PAR MSG_PAR PAR_ERR SAE_PAR
tracc4.o: DAT_PAR IRA_PAR PRM_PAR SAE_PAR
tracc5.o: DAT_PAR IRQ_PAR PAR_ERR SAE_PAR
tracecrdd.o: DAT_PAR GRP_PAR I90_DAT IRA_PAR IRC_PAR IRQ_PAR MSG_PAR \
            PAR_ERR SAE_PAR
