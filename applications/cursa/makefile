#+
#  Name:
#     makefile
#
#  Version:
#     Application makefile Mk V
#
#  Purpose:
#     Build and install the CURSA package.
#
#  Type of Module:
#     Description file for the make utility.
#
#  Description:
#     This description file is used by the make utility to build the
#     CURSA package from the distributed source files, to install
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
#
#  External Dependencies:
#     The CURSA package depends on the following other Starlink
#     packages which must previously have been installed into the
#     appropriate sub-directories of the $STARLINK directory (/star is
#     used if the environment variable STARLINK is not defined).
#
#        cat
#           Catalogue handling system.
#        par
#           ADAM parameter system.
#        mers
#           ADAM message and error system.
#
#        Note that FITSIO, CHI and various ADAM libraries are accessed
#        indirectly, via CAT.  However, FITSIO and CHI are explicitly
#        included in the CURSA link command.
#
#  Notes:
#     This makefile uses the presence/absence of the hidden files
#     .BUILT, .INSTALLED_$(SYSTEM) and .TESTED_$(SYSTEM) to record the
#     current state of the system during housekeeping operations.
#
#  Implementation Deficiencies:
#
#  Copyright:
#     Copyright (C) 1995 Rutherford Appleton Laboratory
#
#  Authors:
#     RFWS: R.F.Warren-Smith (Starlink, RAL)
#     PMA: P.M.Allan (Starlink, RAL)
#     PTW: P.T.Wallace (Starlink, RAL)
#     BLY: M.J.Bly (Starlink, RAL)
#     AJC: A.J.Chipperfield (Starlink, RAL)
#     ACD: A.C.Davenhall (Starlink, ROE)
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
#     8-AUG-1994 (BLY):
#        Add dependency on source archive to extraction targets.
#     30-NOV-1994 (AJC):
#        Add facility error files
#        Add export_run target
#        Add where installed
#        Prevent installation if not built for this SYSTEM
#        Ensure deinstall is from where it is installed
#        Revise use of package name variables, adding PACK_NAME for
#         use in messages and PKG_LINK for use in INCLUDE FILE links.
#         Use variables in all references.
#        Use installed $(PKG_NAME)_dev to handle INCLUDE files for test
#     24-APR-1995 (AJC):
#        -p on install mkdir
#        Add else clause to catch failue of "elif $(MAKE)"
#        Add exit 1 to detected illegal install and deinstall cases.
#        Remove dependency of files on source archive.
#     2-MAY-1995 (BLY):
#        Shared libraries now in INSTALL_SHARE, also chmod 755.
#        INSTALL_SHARE added to INSTALL_DIRS.
#        RANLIB default changed to : (colon).
#        BLD_SHR default changed to : (colon).
#     5-JUN-1995 (RFWS):
#        Added -f flag to rm commands in clean and unbuild targets.
#     15-JUN-1995 (AJC):
#        Revised check target to cope with export_run versions.
#     16-JUN-1995 (BLY):
#        Designated Mk IVb.
#     26-JUN-1995 (BLY):
#        Modified export, export_run and export_source targets.
#        Cosmetics.
#        Designated Mk IVc.
#     29-JUN-1995 (BLY):
#        Added sections for manipulating Latex and Hypertest documentation.
#        Revised install and deinstall sequences.
#        Changed export* targets to make compressed tar files in one step.
#        Replacing of `rm' with `rm -f' in most cases.
#        Designated Mk V.
#     03-JUL-1995 (BLY):
#        Template makefile now generated automatically.
#     17-JUL-1995 (AJC):
#        Modified hypertext installation/deinstallation to avoid linking
#        if no hypertext documents defined.
#     23-OCT-1995 (BLY):
#        Adapted for CURSA, based on ACD original.
#     25-OCT-1995 (BLY):
#        Added xcatview TCL scripts.
#     20-DEC-1996 (ACD):
#        Modified for CURSA version 2.1.
#     04-FEB-1997 (ACD):
#        Added ADDLIBS for accessing new release of CAT library.
#     {enter_further_changes_here}
#     13-JUN-1997 (ACD):
#        Modified for CURSA version 3.1.
#     29-NOV-1998 (ACD):
#        Modified for CURSA version 5.1.  In particular, added functions
#        for Perl scripts.
#     07-DEC-1998 (BLY):
#        Added deinstall section for PERL _SCRIPTS.  Version 5.1-1.
#     13-DEC-1998 (ACD):
#        Modified for CURSA version 5.1-2.  Plotting scatter-plots
#        was removed from xcatview.
#     29-NOV-1999 (ACD):
#        Modified for CURSA version 6.1.  Added catgrid.  Replaced
#        expectk with STARTCL.
#     30-JUL-2000 (ACD):
#        Modified for CURSA version 6.2.
#     21-SEP-2000 (ACD):
#        Modified for CURSA version 6.2-1.
#     06-FEB-2001 (ACD):
#        Modified for CURSA version 6.2-2.  Applications other than
#        catview were converted to a monolith.
#     31-MAY-2001 (ACD):
#        Modified for CURSA version 6.3.  catremote is now a Perl script
#        (with its own SSN) rather than a C++ program.  Various other
#        minor changes.
#     05-NOV-2001 (ACD):
#        Modified for CURSA version 6.4.  Only minor changes required: there
#        were a couple of new subroutines in the CAP library.
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
STAR_SHARE = $(STARLINK)/share

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

#  Link script for geturl.  Note that it is only required for Sun/Solaris.

QLINK = 

#  Default location and name of tclsh and expectk.  Note that expectk
#  has been replaced with awish (for STARTCL).

TCLSH = $(STAR_BIN)/tclsh8.2
EXPECTK = $(STAR_BIN)/awish
PERL = $(STARLINK)/Perl/bin/perl

#
#  Flag indicating whether CURSA is being installed on a networked or
#  stand-alone computer.

NETWORKED = yes

#-------------------------------------------------------------------------------
################################################################################
#
#  Define package source files.
#  ===========================
#
#  This section defines the set of source files for the package.

#  Name of the package as specified in documentation
#  The value is used in messages from make to the user.

PACK_NAME = CURSA

#  Prefix for the package in lower-case as used in filenames etc.

PKG_NAME = cursa

#  Prefix for the package in upper-case as used in include file links.

PKG_LINK = CURSA

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

PKG_VERS = 6.4-1

#  List of files comprising the distributed source-only system.  This
#  defines the minimum set of files required to rebuild completely the
#  package from source (including this makefile, the associated mk
#  script and any documentation files).  It is advisable to use several
#  smaller source archives in place of one large one to speed up build
#  times.

SOURCE_FILES = $(PKG_NAME)_source.tar makefile mk $(DOCUMENTATION)

#  List of public script files.  These are scripts which form part of
#  the package and will be required by users of it.  They will be
#  installed in the $(INSTALL_BIN) directory with execute permission
#  set.

PUBLIC_SCRIPTS = cursa_test.csh

#  Startup script.  This is the file that must be executed (sourced)
#  by a user of this package in order to define appropriate aliases 
#  and environment variables.  It is listed separately from the public 
#  scripts as it is edited by the installation procedure.

STARTUP_SCRIPT = $(PKG_NAME).csh 

#  Link script.  This script does not form part of the package but is 
#  required for building it.  It will not be installed.
 
LINK_SCRIPT = $(PKG_NAME)_link_adam
CLINK_SCRIPT = 

#  Startup ICL script.  This is the file that must be loaded by ICL
#  for a user of this package,  in order to define the package command
#  set within ICL.  It is listed separately from the public scripts as 
#  it is edited by the installation procedure.

ICL_SCRIPT =

#  List of private include files.  These are include files
#  which form part of the package and are required in order to build
#  it, but which are not required by users of it.

PRIVATE_INCLUDES = $(I_SUB1) $(I_SUB2)

I_SUB1 = CAP_PAR SGZ_PAR SGZ_CMN CHART_PAR CHART_CMN \
CIO_PAR CIO_CMN SPLOT_CMN

I_SUB2 =

#  List of external include files.  These are files which are required
#  in order to build the package but form part of other, externally
#  installed packages.  This list should contain the names used to
#  reference the files within the source code, not the actual names of
#  the files.

EXTERNAL_INCLUDES = SAE_PAR CAT_ERR CAT_PAR FIO_ERR FIO_PAR PAR_ERR

#  List of Fortran routines required for building the package.  This is
#  just a list of all the Fortran source files (excluding BLOCK DATA
#  routines and the main routine, which are treated separately).  

F_ROUTINES = $(F_SUB1) $(F_SUB2) $(TASK_SOURCE)

F_SUB1 = \
cap_adpar.f   cap_crtar.f   cap_ghssh.f   cap_gtstr.f   cap_prgrt.f  \
cap_airms.f   cap_crtcp.f   cap_gids.f    cap_gwtfl.f   cap_pritm.f  \
cap_angdc.f   cap_cutob.f   cap_ginit.f   cap_hsdet.f   cap_prlin.f  \
cap_arcdc.f   cap_dceqp.f   cap_gisan.f   cap_info.f    cap_prng.f  \
cap_caxlb.f   cap_ellips.f  cap_glcol.f   cap_jtwrt.f   cap_pscat.f  \
cap_cctmm.f   cap_envsm.f   cap_glfmt.f   cap_lstas.f   cap_qsrtd.f  \
cap_cctmo.f   cap_expcl.f   cap_glist.f   cap_lstcl.f   cap_r2sgf.f  \
cap_cflph.f   cap_fitph.f   cap_gnhst.f   cap_lstnm.f   cap_range.f  \
cap_cfpar.f   cap_frear.f   cap_gnsgt.f   cap_lstpr.f   cap_rascl.f  \
cap_circ.f    cap_ganct.f   cap_gpcls.f   cap_lstsm.f   cap_rdclr.f  \
cap_circl.f   cap_gcdet.f   cap_gprev.f   cap_lsttx.f   cap_rdply.f  \
cap_clcnt.f   cap_gcdsp.f   cap_grows.f   cap_modet.f   cap_rdsct.f  \
cap_clcrd.f   cap_gcexp.f   cap_gscat.f   cap_nsamp.f   cap_rdtph.f  \
cap_clint.f   cap_gcids.f   cap_gsccl.f   cap_opgtt.f   cap_rect.f  \
cap_clval.f   cap_gcini.f   cap_gscmp.f   cap_out.f     cap_resph.f  \
cap_cmpmo.f   cap_gclse.f   cap_gscnf.f   cap_outf.f    cap_rflph.f  \
cap_cpcat.f   cap_gcols.f   cap_gscol.f   cap_pacol.f   cap_rjlst.f  \
cap_cpccl.f   cap_gcpax.f   cap_gscop.f   cap_pair.f    cap_rpfmt.f  \
cap_cpcol.f   cap_gcpid.f   cap_gscpl.f   cap_pairt.f   cap_rpscl.f  \
cap_cpcph.f   cap_gcprs.f   cap_gscrn.f   cap_pgett.f   cap_saxes.f  \
cap_cpcrd.f   cap_gcrng.f   cap_gscsh.f   cap_phist.f   cap_sclbr.f  \
cap_cpfld.f   cap_gcsel.f   cap_gsctb.f   cap_pltdt.f   cap_sepeq.f  \
cap_cpnul.f   cap_gdcol.f   cap_gsfil.f   cap_pltga.f   cap_spoly.f  \
cap_cppar.f   cap_gdcpl.f   cap_gspar.f   cap_pltgr.f   cap_staet.f  \
cap_cpprs.f   cap_gdpar.f   cap_gsrng.f   cap_pltgs.f   cap_stsym.f  \
cap_cpscl.f   cap_gfcol.f   cap_gssel.f   cap_pltlg.f   cap_tblph.f  \
cap_cpsco.f   cap_gfile.f   cap_gstar.f   cap_pltst.f   cap_tpcol.f  \
cap_cptab.f   cap_gfout.f   cap_gstat.f   cap_plysl.f   cap_tpsym.f  \
cap_cptgc.f   cap_gfpar.f   cap_gstax.f   cap_popg1.f   cap_txtph.f  \
cap_cptrg.f   cap_ghelp.f   cap_gstrd.f   cap_popg2.f   cap_warn.f  \
cap_cptxt.f   cap_ghsop.f   cap_gstrw.f   cap_popg3.f  \
cap_crnsp.f   cap_ghspl.f   cap_gstxt.f   cap_prdet.f  \
cap_crtab.f   cap_ghsrn.f   cap_gtgdc.f   cap_prerr.f

F_SUB2 = catchart.f  catchartrn.f  catcoord.f  catcopy.f  catgrid.f  \
catgscin.f  catheader.f   catpair.f  catphotomfit.f  catphotomlst.f  \
catphotomtrn.f  catselect.f  catsort.f

#  List of Fortran BLOCK DATA routines.

BLOCK_DATA = $(B_SUB1) $(B_SUB2)

B_SUB1 =
 
B_SUB2 =

#  List of platform specific Fortran routines.  The source tar file will
#  contain a version of these files for each set of platforms.  The names
#  given here are the general version of the file, e.g. pkg_open.f.  The
#  names in the tar file will be pkg_open.f_sun4, pkg_open.f_mips, etc.

PLATFORM_F = $(PF_SUB1) $(PF_SUB2)

PF_SUB1 =
PF_SUB2 =

#  C routines required for building the package.  This is just a list of
#  all the C source files.

C_ROUTINES = $(C_SUB1) $(C_SUB2)

C_SUB1 = 
C_SUB2 =

TASK_SOURCE =    $(TASK1_SOURCE)  $(TASK2_SOURCE)

TASK1_SOURCE  = catview.f
TASK2_SOURCE  = cursa.f

CTASK_SOURCE = geturl.c
 
#  Task binary name.
 
TASKS =  $(TASK1) $(TASK2)

TASK1  = catview
TASK2  = cursa

CTASK = geturl

#  List of individual applications in the TASK2 monolith.

INDAPPS = catchart  catchartrn  catcoord  catcopy  catgrid  \
catgscin  catheader   catpair  catphotomfit  catphotomlst  \
catphotomtrn  catselect  catsort

#  List of task interface files.

IFL_FILES = catchart.ifl  catchartrn.ifl  catcoord.ifl  catcopy.ifl  \
catgrid.ifl  catgscin.ifl  catheader.ifl   catpair.ifl  catphotomfit.ifl  \
catphotomlst.ifl  catphotomtrn.ifl  catselect.ifl  catsort.ifl  \
catview.ifl cursa.ifl
 
#  List of help library sources.

HELP_SOURCES =

#  List of ancilliary data files.

DATA_FILES = cursa_output.lis catselect.script catpair.script \
catview.script 5828.gsc targets.TXT

#  Example catalogues and other data files.

EXAMPLE_FILES = 0README.LIS  iras_psc.grt     rc3.grt  \
UrlReader.java         kappa.TXT              resolvename.tcl  \
angles.TXT             listavaildb.tcl        sao.grt  \
bd.grt                 photoprog.TXT          scale.TXT  \
catpair_cart.script    photostandards.TXT     scosmosbw.grt  \
catpair_sphplr.script  pisaresults.TXT        scosmoscol.grt  \
catremote-setup.csh    pisasizes.TXT          simbad.grt  \
complex.TXT            polygon.TXT            simple.TXT  \
complex.dat            ppm.grt                simple.grt  \
complex.grt            propmotn.TXT           simple_radians.TXT  \
gsc.grt                querycat.tcl           usno.grt

#  Lists of Latex and hypertext documents.

LATEX_DOCS = sun190.tex  ssn76.tex
HYPERTEXT_DOCS = sun190.htx  ssn76.htx

#  List of documentation files.

DOCUMENTATION = $(LATEX_DOCS) $(HYPERTEXT_DOCS:.htx=.htx_tar) $(PKG_NAME).news

#  List of TCL scripts.  These will be installed in a subdirectory of the 
#  INSTALL_BIN directory, and edited at install time.

TCL_SCRIPTS = \
Action.tcl          GetFile.tcl         GetScatterOver.tcl  \
Error.tcl           GetFormat.tcl       GetSelection.tcl  \
GetCat.tcl          GetHist.tcl         GetStatsCols.tcl  \
GetCatalogue.tcl    GetHistOver.tcl     HelpText.tcl  \
GetColumns.tcl      GetParam.tcl        OpenCat.tcl   \
GetConfig.tcl       GetRange.tcl        OpenRemote.tcl  \
GetDirCat.tcl       GetRemoteQuery.tcl  RemoteList.tcl  \
GetDispConfig.tcl   GetRow.tcl          RemoteName.tcl  \
GetExprn.tcl        GetScatter.tcl      RemoteQuery.tcl  \
RemoteUrl.tcl       adamtask.tcl  

TCL_INDEX = tclIndex

#  List of EXPECT procedures.  These will be installed in INSTALL_BIN,
#  and will be edited at install time.  Note that EXPECT_SCRIPTS is
#  now actually a list of STARTCL / awish scripts.

EXPECT_SCRIPTS = xcatview

#  ADAM message relay.

ADAM_RELAY = adamMessageRelay

#  List of Perl scripts.  These will be installed in INSTALL_BIN,
#  and will be edited at install time.

PERL_SCRIPTS = catcdsin catremote

#  List of Java class files.  Note that though a Java class file is
#  copied into the CURSA binary directory, CURSA does not require Java
#  to be present on the host system.  The class file is part of an
#  optional example (see SSN/76 for details).

JAVA_CLASSES = UrlReader.class

################################################################################
#-------------------------------------------------------------------------------

#  Define files required for building the package.
#  ==============================================
#
#  This section defines the set of files produced from the source files
#  when the package is built and installed.

#  Use only .o, .c .C .f, .ifc, .ifl, .shl and .hlp suffix rules.

.SUFFIXES:
.SUFFIXES: .o .c .C .f .ifc .ifl .shl .hlp

#  List of files which must be built from the source files before the
#  package can be installed for use.  This should comprise all the files
#  that are required to use the package (but excluding the date stamp
#  file).

BUILT_FILES = $(PUBLIC_SCRIPTS) $(OBJECT_LIBRARIES) $(TASKS) $(IFC_FILES) \
$(CTASK) $(DATA_FILES) $(STARTUP_SCRIPT) $(TCL_SCRIPTS) $(TCL_INDEX) \
$(EXPECT_SCRIPTS) $(ADAM_RELAY) $(PERL_SCRIPTS) $(EXAMPLE_FILES) \
$(JAVA_CLASSES)

#  List of links used to access include files during compilation.  This
#  should comprise all the external include files and any other include
#  files whose names do not exactly match the names used in the source
#  code.

INCLUDE_LINKS = $(EXTERNAL_INCLUDES) $(PKG_LINK)_ERR $(PKG_LINK)_PAR

#  Rules to set up links to locate each of the above include files.

CAT_ERR:   $(STAR_INC)/cat_err;   $(LINK) $? $@
CAT_PAR:   $(STAR_INC)/cat_par;   $(LINK) $? $@
SAE_PAR:   $(STAR_INC)/sae_par;   $(LINK) $? $@
FIO_ERR:   $(STAR_INC)/fio_err;   $(LINK) $? $@
FIO_PAR:   $(STAR_INC)/fio_par;   $(LINK) $? $@
PAR_ERR:   $(STAR_INC)/par_err;   $(LINK) $? $@

#  Rules for extracting source files from the source archive.

$(PUBLIC_SCRIPTS) $(LINK_SCRIPT) $(CLINK_SCRIPT) \
$(TASK_SOURCE) $(CTASK_SOURCE) $(STARTUP_SCRIPT) $(DATA_FILES) \
$(IFL_FILES) $(TCL_SCRIPTS) $(EXPECT_SCRIPTS) $(ADAM_RELAY) \
$(PERL_SCRIPTS) $(EXAMPLE_FILES) $(JAVA_CLASSES):
	$(TAR_OUT) $(PKG_NAME)_source.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi

$(F_SUB1) $(F_SUB2) $(I_SUB1) $(B_SUB1) $(C_SUB1) $(CP_SUB1):
	$(TAR_OUT) $(PKG_NAME)_source.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi

#  Rules for extracting platform specific source files from the source
#  archives.

#  It is quite likely that there are no platform-specific source files
#  and that the macro PF_SUBn is empty. To prevent a syntax error in
#  make, a dummy target is present.

$(PF_SUB1) dummy_target1:
	$(TAR_OUT) $(PKG_NAME)_source.tar $@_$(SOURCE_VARIANT)
	@ if test -f $@_$(SOURCE_VARIANT); then :;\
           else echo $@_$(SOURCE_VARIANT) is not in the tar file; exit 1; fi
	mv $@_$(SOURCE_VARIANT) $@

#  List of object files produced by compiling the source code and rules
#  for performing the compilations.

OBJECT_FILES = $(O_SUB1) $(O_SUB2) $(TASK_OBJECT) $(CTASK_OBJECT)

O_SUB1 = $(B_SUB1:.f=.o) $(F_SUB1:.f=.o) $(PF_SUB1:.f=.o) \
$(C_SUB1:.c=.o) $(CP_SUB1:.C=.o)

O_SUB2 = $(B_SUB2:.f=.o) $(F_SUB2:.f=.o) $(PF_SUB2:.f=.o) \
$(C_SUB2:.c=.o) $(CP_SUB2:.C=.o)

TASK_OBJECT  = $(TASK_SOURCE:.f=.o)
CTASK_OBJECT = $(CTASK_SOURCE:.C=.o)

.c.o:
	$(CC) $(CFLAGS) -c $<
.C.o:
	$(CP) $(PFLAGS) -c $<
.f.o:
	$(FC) $(FFLAGS) -c $<

#  List of object library files to be built and rules for building
#  them.

OBJECT_LIBRARIES = libcap.a

libcap.a: $(O_SUB1) $(O_SUB2)
	$(AR_IN) $@ $?
	$(RANLIB) $@

#  Rules and dependencies for building the tasks.

catview: catview.o $(OBJECT_LIBRARIES) $(LINK_SCRIPT)
	$(ALINK) catview.o -o $@ $(OBJECT_LIBRARIES) \
            $(ADDLIBS) `./$(LINK_SCRIPT)`
 
cursa: cursa.o $(OBJECT_LIBRARIES) $(LINK_SCRIPT)
	$(ALINK) cursa.o -o $@ $(OBJECT_LIBRARIES) \
            $(ADDLIBS) `./$(LINK_SCRIPT)`

#  Note that geturl is different because it is a C program, not
#  an ADAM A-task.

geturl: geturl.c 
	$(CC) -o geturl geturl.c $(QLINK)

#  Rules for building the help libraries.

HELP_FILES = $(HELP_SOURCES:.hlp=.shl)

.hlp.shl:
	$(STAR_BIN)/hlib $<

#  Rules for compiling the interface files.

IFC_FILES = $(IFL_FILES:.ifl=.ifc)

.ifl.ifc:
	$(STAR_BIN)/compifl $<
#
#  Rule for building the tcl index.

tclIndex: $(TCLSH) $(TCL_SCRIPTS)
	echo "auto_mkindex . *.tcl" | $(TCLSH)

#  Name of the date stamp file.  This is used to record the time of the
#  most recent build for use in subsequent operations that require it.
#  There must be no rule for generating this file; it is updated only
#  as a side effect of building the package.

DATE_STAMP = $(PKG_NAME)_datestamp

#  Pathnames of directories into which files may be placed when the
#  package is installed.

INSTALL_BIN = $(INSTALL)/bin/$(PKG_NAME)
INSTALL_DATES = $(INSTALL)/dates
INSTALL_DOCS = $(INSTALL)/docs
#INSTALL_HELP = $(INSTALL)/help/$(PKG_NAME)
INSTALL_TCL = $(INSTALL)/bin/$(PKG_NAME)/xcatviewLib
INSTALL_EXAMPLES = $(INSTALL)/examples/cursa

#  List of directories actually used for installation (selected from
#  those above) and rules to create them.

INSTALL_DIRS = $(INSTALL_BIN) $(INSTALL_DATES) $(INSTALL_DOCS) \
$(INSTALL_TCL) $(INSTALL_EXAMPLES)

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
	@ echo 'Package : $(PACK_NAME)'        >$(DATE_STAMP)
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

#  The install target first checks if any part of the package is
#  already installed.  If not, it checks that the system is built for this
#  SYSTEM and, if it is, causes the .INSTALLED_$(SYSTEM) target to be made
#  which performs the installation.

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
              sed -e s#INSTALL_BIN#$(INSTALL_BIN)# \
              $$f >$(INSTALL_BIN)/$$f ;\
              chmod 755 $(INSTALL_BIN)/$$f;\
           else :; fi;\
        done
#
#  Install the monolith, giving it world execute permission.
	for f in $(TASKS) ""; do \
           if test -n "$$f"; then \
              cp -p $$f $(INSTALL_BIN);\
              chmod 755 $(INSTALL_BIN)/$$f;\
              rm -f $$f;\
              $(LINK) $(INSTALL_BIN)/$$f $$f;\
           else :; fi;\
        done
#
#  Create the application/task name soft links for the individual
#  applications in monolith TASK2.
	for f in $(INDAPPS) ""; do \
           if test -n "$$f"; then \
              $(LINK) ./$(TASK2) $(INSTALL_BIN)/$$f ;\
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
#  Install the C program giving it world execute permission.
	for f in $(CTASK) ""; do \
           if test -n "$$f"; then \
              cp -p $$f $(INSTALL_BIN);\
              chmod 755 $(INSTALL_BIN)/$$f;\
              rm -f $$f;\
              $(LINK) $(INSTALL_BIN)/$$f $$f;\
           else :; fi;\
        done
#
#  Install the data files, giving them world read permission.
	for f in $(DATA_FILES) ""; do \
           if test -n "$$f"; then \
              cp -p $$f $(INSTALL_BIN);\
              chmod 644 $(INSTALL_BIN)/$$f;\
              rm -f $$f;\
              $(LINK) $(INSTALL_BIN)/$$f $$f;\
           else :; fi;\
        done
#
#  Install the help files, giving them world read permission.
#for f in $(HELP_FILES) ""; do \
#          if test -n "$$f"; then \
#             cp -p $$f $(INSTALL_HELP);\
#             chmod 644 $(INSTALL_HELP)/$$f;\
#             rm -f $$f;\
#             $(LINK) $(INSTALL_HELP)/$$f $$f;\
#          else :; fi;\
#       done
#
#  Install the package TCL scripts.  The install location for the scripts
#  must be edited into the scripts, and execute permission given.  Leave
#  the original files in place.
	for f in $(TCL_SCRIPTS) ""; do \
           if test -n "$$f"; then \
              sed -e s#INSTALL_TCL#$(INSTALL_TCL)# \
              $$f >$(INSTALL_TCL)/$$f ;\
              chmod 755 $(INSTALL_TCL)/$$f ;\
           else :; fi;\
        done
#  Install the package TCL index.  The install location for the scripts
#  must be edited into the scripts, and execute permission given.  Leave
#  the original files in place.
	for f in $(TCL_INDEX) ""; do \
           if test -n "$$f"; then \
              sed -e s#INSTALL_TCL#$(INSTALL_TCL)# \
              $$f >$(INSTALL_TCL)/$$f ;\
              chmod 755 $(INSTALL_TCL)/$$f ;\
           else :; fi;\
        done
#
#  Install the package EXPECT scripts.  The install location for the TCL
#  scripts, the install location for the package binaries,  and the full 
#  path of the expectk procedure must be edited into the scripts, and 
#  execute permission given.  Leave the original files in place.
	for f in $(EXPECT_SCRIPTS) ""; do \
           if test -n "$$f"; then \
              sed -e s#INSTALL_BIN#$(INSTALL_BIN)# \
                  -e s#INSTALL_TCL#$(INSTALL_TCL)# \
                  -e s#EXPECTK#$(EXPECTK)# \
                  -e s#NETWORKED#$(NETWORKED)# \
              $$f >$(INSTALL_BIN)/$$f ;\
              chmod 755 $(INSTALL_BIN)/$$f ;\
           else :; fi;\
        done
#
#  Install the package ADAM_RELAY scripts.  The installed location of the
#  interperter (atclsh) must be edited in to the script, the script copied
#  to its installed location, and execute permission given.  The original
#  script is left in place.
	for f in $(ADAM_RELAY) ""; do \
           if test -n "$$f"; then \
              sed -e s#STAR_BIN#$(STAR_BIN)# \
              $$f >$(INSTALL_TCL)/$$f ;\
              chmod 755 $(INSTALL_TCL)/$$f ;\
           else :; fi;\
        done
#
#  Install the package Perl scripts.  The full path of the Perl
#  interpreter must be edited into the first line of the script, and 
#  execute permission given.  Leave the original files in place.
	for f in $(PERL_SCRIPTS) ""; do \
           if test -n "$$f"; then \
              sed -e s#PERL#$(PERL)# \
              $$f >$(INSTALL_BIN)/$$f ;\
              chmod 755 $(INSTALL_BIN)/$$f ;\
           else :; fi;\
        done
#
#  Install the Java class files, giving them world execute permission.
	for f in $(JAVA_CLASSES) ""; do \
           if test -n "$$f"; then \
              cp -p $$f $(INSTALL_BIN);\
              chmod 755 $(INSTALL_BIN)/$$f;\
              rm -f $$f;\
              $(LINK) $(INSTALL_BIN)/$$f $$f;\
           else :; fi;\
        done
#
#  Install the example files, giving them world read permission.
	for f in $(EXAMPLE_FILES) ""; do \
           if test -n "$$f"; then \
              cp -p $$f $(INSTALL_EXAMPLES);\
              chmod 644 $(INSTALL_EXAMPLES)/$$f;\
              rm -f $$f;\
              $(LINK) $(INSTALL_EXAMPLES)/$$f $$f;\
           else :; fi;\
        done
#
#
#  Install the package startup script.  The package version number,
#  the name of the directory containing the installed binaries and 
#  interface files, and the name of the directory containing the help
#  libraries must be edited into this, and execute permission given.
#  Leave the original file in place.
	if test -n "$(STARTUP_SCRIPT)"; then \
           sed -e s#INSTALL_BIN#$(INSTALL_BIN)# \
               -e s#INSTALL_HELP#$(INSTALL_HELP)# \
               -e s#PKG_VERS#$(PKG_VERS)# \
              $(STARTUP_SCRIPT) >$(INSTALL_BIN)/$(STARTUP_SCRIPT) ;\
           chmod 755 $(INSTALL_BIN)/$(STARTUP_SCRIPT) ;\
        else :; fi
#
#  Install the package ICL script.  The package version number must be 
#  edited into this, and read permission given.  Leave the original file
#  in place.
	if test -n "$(ICL_SCRIPT)"; then \
           sed -e s#PKG_VERS#$(PKG_VERS)# \
              $(ICL_SCRIPT) >$(INSTALL_BIN)/$(ICL_SCRIPT) ;\
           chmod 755 $(INSTALL_BIN)/$(ICL_SCRIPT) ;\
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
	rm $(DATE_STAMP)
	$(LINK) $(INSTALL_DATES)/$(DATE_STAMP) $(DATE_STAMP)

#  deinstall: Deinstall the package.
#  --------------------------------
#
#  Reverse the action of the install target, removing the installed
#  files and returning them to the source directory.

#  The deinstall target checks that the package is installed in the INSTALL
#  directory.  If so, it causes the do_deinstall target to be made which 
#  performs the deinstallation.

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
           if test -n "$$f"; then \
              if test -f $(INSTALL_BIN)/$$f; then\
                 rm -f $(INSTALL_BIN)/$$f;\
              else :; fi;\
              $(MAKE) $$f;\
           else :; fi;\
        done
#
#  Deinstall the monolith, if an installed version exists.
	- for f in $(TASKS) ""; do \
           if test -n "$$f" -a -f $(INSTALL_BIN)/$$f; then \
              rm -f $$f;\
              cp -p $(INSTALL_BIN)/$$f .;\
              rm -f $(INSTALL_BIN)/$$f;\
           else :; fi;\
        done
#
#  Remove the application/task name soft links for the individual
#  applications in monolith TASK2.
	- for f in $(INDAPPS) ""; do \
             if test -n "$$f"; then \
                rm -f $(INSTALL_BIN)/$$f ;\
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
#  Deinstall the C program if an installed version exists.
	- for f in $(CTASK) ""; do \
           if test -n "$$f" -a -f $(INSTALL_BIN)/$$f; then \
              rm -f $$f;\
              cp -p $(INSTALL_BIN)/$$f .;\
              rm -f $(INSTALL_BIN)/$$f;\
           else :; fi;\
        done
#
#  Deinstall the data files, if installed versions exist.
	- for f in $(DATA_FILES) ""; do \
           if test -n "$$f" -a -f $(INSTALL_BIN)/$$f; then \
              rm -f $$f;\
              cp -p $(INSTALL_BIN)/$$f .;\
              rm -f $(INSTALL_BIN)/$$f;\
           else :; fi;\
        done
#
#  Deinstall the help files, if installed versions exist.
#- for f in $(HELP_FILES) ""; do \
#          if test -n "$$f" -a -f $(INSTALL_HELP)/$$f; then \
#             rm -f $$f;\
#             cp -p $(INSTALL_HELP)/$$f .;\
#             rm -f $(INSTALL_HELP)/$$f;\
#          else :; fi;\
#       done
#
#  Remove the application/task soft links.
	- for f in $(TASKS) ""; do \
             if test -n "$$f"; then \
                rm -f $(INSTALL_BIN)/$$f ;\
           else :; fi;\
        done
#
#  Deinstall the package TCL scripts.  Since they will have been edited
#  during the installation, we remove the installed copies, if present,
#  and then ensure the originals exist.
	- for f in $(TCL_SCRIPTS) ""; do \
           if test -n "$$f"; then \
              if test -f $(INSTALL_TCL)/$$f; then\
                 rm -f $(INSTALL_TCL)/$$f;\
              else :; fi;\
              $(MAKE) $$f;\
           else :; fi;\
        done
#
#  Deinstall the package TCL index.  Since this file is generated 'on
#  the fly' simply delete it.
	- for f in $(TCL_INDEX) ""; do \
           if test -f $(INSTALL_TCL)/$$f; then\
              rm -f $(INSTALL_TCL)/$$f;\
           else :; fi;\
        done
#
#  Deinstall the package EXPECT scripts.  Since they will have been edited
#  during the installation, we remove the installed copies, if present,
#  and then ensure the originals exist.
	- for f in $(EXPECT_SCRIPTS) ""; do \
           if test -n "$$f"; then \
              if test -f $(INSTALL_BIN)/$$f; then\
                 rm -f $(INSTALL_BIN)/$$f;\
              else :; fi;\
              $(MAKE) $$f;\
           else :; fi;\
        done
#
#  Deinstall the package ADAM_RELAY scripts.  Since they will have been edited
#  during the installation, we remove the installed copies, if present,
#  and then ensure the originals exist.
	- for f in $(ADAM_RELAY) ""; do \
           if test -n "$$f"; then \
              if test -f $(INSTALL_TCL)/$$f; then\
                 rm -f $(INSTALL_TCL)/$$f;\
              else :; fi;\
              $(MAKE) $$f;\
           else :; fi;\
        done
#
#  Deinstall the package PERL scripts.  Since they will have been edited
#  during the installation, we remove the installed copies, if present,
#  and then ensure the originals exist.
	- for f in $(PERL_SCRIPTS) ""; do \
           if test -n "$$f"; then \
              if test -f $(INSTALL_BIN)/$$f; then\
                 rm -f $(INSTALL_BIN)/$$f;\
              else :; fi;\
              $(MAKE) $$f;\
           else :; fi;\
        done
#
#  Deinstall the Java class files, if installed versions exist.
	- for f in $(JAVA_CLASSES) ""; do \
           if test -n "$$f" -a -f $(INSTALL_BIN)/$$f; then \
              rm -f $$f;\
              cp -p $(INSTALL_BIN)/$$f .;\
              rm -f $(INSTALL_BIN)/$$f;\
           else :; fi;\
        done
#
#  Deinstall the example files, if installed versions exist.
	- for f in $(EXAMPLE_FILES) ""; do \
           if test -n "$$f" -a -f $(INSTALL_EXAMPLES)/$$f; then \
              rm -f $$f;\
              cp -p $(INSTALL_EXAMPLES)/$$f .;\
              rm -f $(INSTALL_EXAMPLES)/$$f;\
           else :; fi;\
        done
#
#  Deinstall the package startup file.  Since it will have been edited
#  during installation, we remove the installed copy, if present, and
#  then ensure that the original exists.
	- if test -n "$(STARTUP_SCRIPT)"; then \
           if test -f $(INSTALL_BIN)/$(STARTUP_SCRIPT); then\
              rm -f $(INSTALL_BIN)/$(STARTUP_SCRIPT);\
           else :; fi ;\
           $(MAKE) $(STARTUP_SCRIPT);\
        else :; fi
#
#  Deinstall the package ICL file.  Since it will have been edited
#  during installation, we remove the installed copy, if present, and
#  then ensure that the original exists.
	- if test -n "$(ICL_SCRIPT)"; then \
           if test -f $(INSTALL_BIN)/$(ICL_SCRIPT); then\
              rm -f $(INSTALL_BIN)/$(ICL_SCRIPT);\
           else :; fi ;\
           $(MAKE) $(ICL_SCRIPT);\
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
#  Deinstall the date stamp file after setting its protection so it may
#  be removed.
	- if test -f $(INSTALL_DATES)/$(DATE_STAMP); then \
           chmod 644 $(DATE_STAMP); rm $(DATE_STAMP);\
           chmod 644 $(INSTALL_DATES)/$(DATE_STAMP);\
           cp -p $(INSTALL_DATES)/$(DATE_STAMP) .;\
           rm $(INSTALL_DATES)/$(DATE_STAMP);\
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
#  Create a directory to run the test in, move to it, run the test and
#  tidy up.
	@ $(INSTALL_BIN)/cursa_test.csh
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
           $(DATE_STAMP) .BUILT \
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

#  clean: Clean up the source directory.
#  ------------------------------------
#
#  Remove all intermediate files.  Do not remove built files.

clean:
	@- rm -f $(INCLUDE_LINKS) $(PRIVATE_INCLUDES) $(IFL_FILES) \
              $(LINK_SCRIPT) 1>/dev/null 2>/dev/null
	@- rm -f $(F_ROUTINES) 1>/dev/null 2>/dev/null
	@- rm -f $(C_ROUTINES) 1>/dev/null 2>/dev/null
	@- rm -f $(CP_ROUTINES) 1>/dev/null 2>/dev/null
	@- rm -f $(CLINK_SCRIPT) 1>/dev/null 2>/dev/null
	@- rm -f $(CTASK_SOURCE) 1>/dev/null 2>/dev/null
	@- rm -f $(OBJECT_FILES) 1>/dev/null 2>/dev/null
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
	strip $(TASKS)
	@ echo '*** Tasks stripped'

#-------------------------------------------------------------------------------

#  Include file dependencies.
#  =========================

#  Object file dependencies on include files (or links to those include
#  files).  These are normally generated automatically from the source
#  files.

cap_adpar.o: CAT_ERR CAT_PAR SAE_PAR
cap_airms.o: SAE_PAR
cap_angdc.o: SAE_PAR
cap_arcdc.o: CAP_PAR SAE_PAR
cap_caxlb.o: CAT_PAR SAE_PAR
cap_cctmm.o: CAP_PAR SAE_PAR
cap_cctmo.o: CAP_PAR SAE_PAR
cap_cflph.o: SAE_PAR
cap_cfpar.o: CAT_PAR SAE_PAR
cap_circ.o: CAP_PAR CAT_PAR SAE_PAR
cap_circl.o: CAP_PAR SAE_PAR
cap_clcnt.o: SAE_PAR
cap_clcrd.o: CAP_PAR CAT_PAR SAE_PAR
cap_clint.o: SAE_PAR
cap_clval.o: SAE_PAR
cap_cmpmo.o: CAP_PAR PAR_ERR SAE_PAR
cap_cpcat.o: CAT_PAR SAE_PAR
cap_cpccl.o: CAP_PAR CAT_PAR SAE_PAR
cap_cpcol.o: CAT_PAR SAE_PAR
cap_cpcph.o: CAT_PAR SAE_PAR
cap_cpcrd.o: CAT_PAR SAE_PAR
cap_cpfld.o: CAT_ERR CAT_PAR SAE_PAR
cap_cpnul.o: CAT_ERR CAT_PAR SAE_PAR
cap_cppar.o: CAT_PAR SAE_PAR
cap_cpprs.o: CAT_PAR SAE_PAR
cap_cpscl.o: CAT_PAR SAE_PAR SGZ_CMN SGZ_PAR
cap_cpsco.o: CAT_PAR SAE_PAR
cap_cptab.o: CAT_ERR CAT_PAR SAE_PAR
cap_cptgc.o: CAT_PAR SAE_PAR
cap_cptrg.o: CAT_PAR SAE_PAR
cap_cptxt.o: CAT_PAR SAE_PAR
cap_crnsp.o: SAE_PAR
cap_crtab.o: CAT_ERR CAT_PAR CIO_CMN CIO_PAR SAE_PAR
cap_crtar.o: SAE_PAR
cap_crtcp.o: CAT_PAR CIO_CMN CIO_PAR SAE_PAR
cap_cutob.o: CAP_PAR SAE_PAR
cap_dceqp.o: SAE_PAR
cap_ellips.o: CAP_PAR SAE_PAR
cap_envsm.o: CAT_PAR SAE_PAR
cap_expcl.o: CAT_ERR CAT_PAR SAE_PAR
cap_fitph.o: SAE_PAR
cap_frear.o: SAE_PAR
cap_ganct.o: CAT_PAR SAE_PAR SGZ_CMN SGZ_PAR
cap_gcdet.o: CAT_PAR SAE_PAR SGZ_CMN SGZ_PAR
cap_gcdsp.o: CAT_PAR SAE_PAR SGZ_CMN SGZ_PAR
cap_gcexp.o: CAT_ERR CAT_PAR SAE_PAR SGZ_CMN SGZ_PAR
cap_gcids.o: CAT_PAR SAE_PAR SGZ_PAR
cap_gcini.o: CAT_PAR SAE_PAR SGZ_CMN SGZ_PAR
cap_gclse.o: CAT_PAR SAE_PAR SGZ_CMN SGZ_PAR
cap_gcols.o: CAT_PAR SAE_PAR
cap_gcpax.o: SAE_PAR
cap_gcpid.o: CAT_PAR SAE_PAR
cap_gcprs.o: SAE_PAR SGZ_PAR
cap_gcrng.o: CAT_ERR CAT_PAR SAE_PAR SGZ_CMN SGZ_PAR
cap_gcsel.o: CAT_PAR SAE_PAR SGZ_CMN SGZ_PAR
cap_gdcol.o: CAT_PAR SAE_PAR SGZ_CMN SGZ_PAR
cap_gdcpl.o: CAT_PAR SAE_PAR SGZ_CMN SGZ_PAR
cap_gdpar.o: CAT_PAR SAE_PAR SGZ_CMN SGZ_PAR
cap_gfcol.o: CAT_PAR SAE_PAR SGZ_CMN SGZ_PAR
cap_gfile.o: CAT_PAR SAE_PAR SGZ_CMN SGZ_PAR
cap_gfout.o: CAT_PAR SAE_PAR SGZ_CMN SGZ_PAR
cap_gfpar.o: CAT_PAR SAE_PAR SGZ_CMN SGZ_PAR
cap_ghelp.o: SAE_PAR
cap_ghsop.o: CAT_PAR SAE_PAR SGZ_CMN SGZ_PAR SPLOT_CMN
cap_ghspl.o: CAT_PAR SAE_PAR SGZ_CMN SGZ_PAR SPLOT_CMN
cap_ghsrn.o: CAT_PAR SAE_PAR SGZ_CMN SGZ_PAR SPLOT_CMN
cap_ghssh.o: CAT_PAR SAE_PAR SGZ_CMN SGZ_PAR SPLOT_CMN
cap_gids.o: CAT_PAR SAE_PAR
cap_ginit.o: CAT_PAR SAE_PAR SGZ_CMN SGZ_PAR SPLOT_CMN
cap_gisan.o: CAT_PAR SAE_PAR
cap_glcol.o: CAT_PAR SAE_PAR SGZ_CMN SGZ_PAR
cap_glfmt.o: CAT_PAR SAE_PAR
cap_glist.o: CAT_PAR SAE_PAR SGZ_CMN SGZ_PAR
cap_gnhst.o: SAE_PAR
cap_gnsgt.o: SAE_PAR
cap_gpcls.o: CAT_PAR SAE_PAR SPLOT_CMN
cap_gprev.o: CAT_PAR SAE_PAR SGZ_CMN SGZ_PAR
cap_grows.o: CAT_PAR SAE_PAR SGZ_CMN SGZ_PAR
cap_gscat.o: CAT_PAR SAE_PAR SGZ_CMN SGZ_PAR
cap_gsccl.o: CAT_PAR SAE_PAR
cap_gscmp.o: CAT_ERR CAT_PAR SAE_PAR SGZ_CMN SGZ_PAR
cap_gscnf.o: CAT_PAR SAE_PAR SGZ_CMN SGZ_PAR
cap_gscol.o: CAT_PAR SAE_PAR SGZ_CMN SGZ_PAR
cap_gscop.o: CAT_PAR SAE_PAR SGZ_CMN SGZ_PAR SPLOT_CMN
cap_gscpl.o: CAT_PAR SAE_PAR SGZ_CMN SGZ_PAR SPLOT_CMN
cap_gscrn.o: CAT_PAR SAE_PAR SGZ_CMN SGZ_PAR SPLOT_CMN
cap_gscsh.o: CAT_PAR SAE_PAR SGZ_CMN SGZ_PAR SPLOT_CMN
cap_gsctb.o: CAT_ERR CAT_PAR SAE_PAR
cap_gsfil.o: CAT_PAR SAE_PAR SGZ_CMN SGZ_PAR
cap_gspar.o: CAT_PAR SAE_PAR SGZ_CMN SGZ_PAR
cap_gsrng.o: CAT_PAR SAE_PAR SGZ_CMN SGZ_PAR
cap_gssel.o: CAT_PAR SAE_PAR SGZ_CMN SGZ_PAR
cap_gstar.o: CAT_PAR SAE_PAR
cap_gstat.o: CAT_PAR SAE_PAR SGZ_CMN SGZ_PAR
cap_gstax.o: SAE_PAR
cap_gstrd.o: CAT_PAR SAE_PAR
cap_gstrw.o: CAT_PAR SAE_PAR SGZ_CMN SGZ_PAR
cap_gstxt.o: CAT_PAR SAE_PAR SGZ_CMN SGZ_PAR
cap_gtgdc.o: CAT_PAR SAE_PAR
cap_gtstr.o: SAE_PAR
cap_gwtfl.o: CAT_PAR SAE_PAR SGZ_CMN SGZ_PAR
cap_hsdet.o: CAT_PAR SAE_PAR SGZ_CMN SGZ_PAR
cap_info.o: SAE_PAR SGZ_PAR
cap_jtwrt.o: CAT_PAR SAE_PAR
cap_lstas.o: SAE_PAR SGZ_PAR
cap_lstcl.o: CAT_PAR SAE_PAR
cap_lstnm.o: CAT_PAR SAE_PAR
cap_lstpr.o: CAT_PAR SAE_PAR
cap_lstsm.o: CAT_PAR SAE_PAR
cap_lsttx.o: SAE_PAR SGZ_PAR
cap_modet.o: CAP_PAR PAR_ERR SAE_PAR
cap_nsamp.o: CAT_PAR SAE_PAR
cap_opgtt.o: CAT_PAR SAE_PAR
cap_out.o: SAE_PAR SGZ_PAR
cap_outf.o: SAE_PAR
cap_pacol.o: CAT_PAR SAE_PAR
cap_pair.o: CAT_PAR SAE_PAR
cap_pairt.o: CAT_PAR SAE_PAR
cap_pgett.o: CAT_PAR SAE_PAR
cap_phist.o: SAE_PAR
cap_pltdt.o: CAP_PAR CAT_ERR CAT_PAR SAE_PAR
cap_pltga.o: CAT_PAR CHART_CMN CHART_PAR SAE_PAR
cap_pltgr.o: CAT_ERR CAT_PAR CHART_CMN CHART_PAR CIO_PAR SAE_PAR
cap_pltgs.o: CAT_PAR CHART_CMN CHART_PAR CIO_PAR SAE_PAR
cap_pltlg.o: CAT_PAR CHART_CMN CHART_PAR CIO_PAR SAE_PAR
cap_pltst.o: CHART_CMN CHART_PAR CIO_PAR SAE_PAR
cap_plysl.o: CAT_PAR SAE_PAR
cap_popg1.o: SAE_PAR
cap_popg2.o: SAE_PAR
cap_popg3.o: SAE_PAR
cap_prdet.o: CAT_PAR SAE_PAR
cap_prerr.o: SAE_PAR
cap_prgrt.o: CAT_PAR CIO_CMN CIO_PAR FIO_ERR SAE_PAR
cap_pritm.o: CAT_PAR CIO_CMN CIO_PAR SAE_PAR
cap_prlin.o: CAT_PAR CIO_CMN CIO_PAR SAE_PAR
cap_prng.o: SAE_PAR
cap_pscat.o: SAE_PAR
cap_qsrtd.o: CAT_PAR
cap_r2sgf.o: SAE_PAR
cap_range.o: CAT_ERR CAT_PAR SAE_PAR
cap_rascl.o: CAT_ERR CAT_PAR CIO_CMN CIO_PAR SAE_PAR
cap_rdclr.o: SAE_PAR
cap_rdply.o: SAE_PAR
cap_rdsct.o: SAE_PAR
cap_rdtph.o: CAT_PAR SAE_PAR
cap_rect.o: CAT_PAR SAE_PAR
cap_resph.o: CAT_PAR SAE_PAR
cap_rflph.o: SAE_PAR
cap_rjlst.o: SAE_PAR
cap_rpfmt.o: CAT_PAR SAE_PAR SGZ_CMN SGZ_PAR
cap_rpscl.o: CAT_PAR SAE_PAR
cap_saxes.o: SAE_PAR
cap_sclbr.o: CAT_PAR CHART_CMN CIO_PAR SAE_PAR
cap_sepeq.o: CAT_ERR SAE_PAR
cap_spoly.o: SAE_PAR
cap_staet.o: SAE_PAR
cap_stsym.o: CAP_PAR CAT_PAR CHART_CMN CIO_PAR SAE_PAR
cap_tblph.o: CAT_PAR SAE_PAR
cap_tpcol.o: CAT_PAR SAE_PAR SGZ_CMN SGZ_PAR
cap_tpsym.o: CAT_PAR SAE_PAR SGZ_CMN SGZ_PAR
cap_txtph.o: SAE_PAR
cap_warn.o: SAE_PAR SGZ_PAR
catchart.o: SAE_PAR
catchartrn.o: CAT_PAR SAE_PAR
catcoord.o: CAP_PAR CAT_PAR SAE_PAR
catcopy.o: CAT_PAR SAE_PAR
catgrid.o: CAT_PAR SAE_PAR
catgscin.o: CAT_PAR SAE_PAR
catheader.o: CAT_PAR SAE_PAR
catpair.o: CAT_ERR CAT_PAR SAE_PAR
catphotomfit.o: CAT_PAR SAE_PAR
catphotomlst.o: SAE_PAR
catphotomtrn.o: CAT_PAR SAE_PAR
catselect.o: CAT_ERR CAT_PAR SAE_PAR
catsort.o: CAT_PAR SAE_PAR
catview.o: CAT_PAR SAE_PAR
cursa.o: SAE_PAR

#-------------------------------------------------------------------------------
#
#  End of makefile.
#.
