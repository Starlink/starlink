#+
#  Name:
#     makefile
#
#  Version:
#     Package makefile Mk V
#
#  Purpose:
#     Build and install the DIPSO package.
#
#  Type of Module:
#     Description file for the make utility.
#
#  Description:
#     This description file is used by the make utility to build the
#     DIPSO package from the distributed source files, to install
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
#     The DIPSO package depends on the following other Starlink
#     packages which must previously have been installed into the
#     appropriate sub-directories of the $STARLINK directory (/star is
#     used if the environment variable STARLINK is not defined).
#        err/msg
#           Error and message reporting system
#        snx                        
#           Starlink extensions to the NCAR graphics system
#        ndf
#           The NDF data access library
#        chr
#           Character string handling library
#        pda
#           The Starlink library of public domain numerical algorithms
#        sae_par                    
#           Global include files    
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
#     11-JUN-1996 (DSB):
#        Added DIPSOLIB to use in place of should be INSTALL_BIN in the 
#        my_dipso target.
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

#  Pathname of the root directory beneath which the built files for
#  this package should be installed for use.  This defaults to the
#  user's home directory.

INSTALL = $(HOME)

#  Pathname of the directory containing libraries (etc) which are needed to
#  create a local version of dipso using the my_dipso target. This defaults 
#  to the Starlink dipso directory but will usually be set by running the 
#  installed DIPSOSETUP script before executing make.

DIPSOLIB = $(STAR_BIN)/$(PKG_NAME)


#  Pathname of the directory into which exported tar files will be
#  placed.  This defaults to the current working directory.

EXPORT = .

#  Default macros for compiling C and Fortran source code.

CC = c89
CFLAGS = -O
FC = fort77
FFLAGS = -O
DIPSO_FFLAGS = 

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

#  Linker command.  Used to compile/link the final executables.

ELINK = $(FC) -O

#  Linker flags.  These are placed either side of the library list
#  and are used adding system-dependent linker flags to link commands.

LDFLAG1 =
LDFLAG2 =

#-------------------------------------------------------------------------------
################################################################################
#
#  Define package source files.
#  ===========================
#
#  This section defines the set of source files for the package.

#  Name of the package as specified in documentation
#  The value is used in messages from make to the user.

PACK_NAME = DIPSO

#  Prefix for the package in lower-case as used in filenames etc.
PKG_NAME = dipso

#  Prefix for the package in upper-case as used in include file links.
PKG_LINK = DIPSO

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

PKG_VERS = 3.5-6

#  List of files comprising the distributed source-only system.  This
#  defines the minimum set of files required to rebuild completely the
#  package from source (including this makefile, the associated mk
#  script and any documentation files).  It is advisable to use several
#  smaller source archives in place of one large one to speed up build
#  times.

SOURCE_FILES = $(PKG_NAME)_source.tar makefile mk $(DOCUMENTATION) \
   $(PKG_NAME)_sub1.tar dipso_specdat.tar

#  List of public script files.  These are scripts which form part of
#  the package and will be required by users of it.  They will be
#  installed in the $(INSTALL_BIN) directory with execute permission
#  set.

PUBLIC_SCRIPTS = dipsotest $(LINK_SCRIPT)

#  Startup script.  This is the file that must be executed (sourced)
#  by a user of this package in order to define appropriate aliases 
#  and environment variables.  It is listed separately from the public 
#  scripts as it is edited by the installation procedure.

STARTUP_SCRIPT = dipsosetup

#  Link script.  This script forms part of the package and is 
#  required for building it and for building local copies.  It is 
#  installed as a PUBLIC_SCRIPT.
 
LINK_SCRIPT = $(PKG_NAME)_link

#  List of public include files.  These are include files
#  which form part of the package and are required by users of it.

PUBLIC_INCLUDES = $(IP_SUB1) $(IP_SUB2)

IP_SUB1 = declare_stks.f declare_lbls.f declare_plts.f declare_data.f
IP_SUB2 =

#  List of private include files.  These are include files
#  which form part of the package and are required in order to build
#  it, but which are not required by users of it.

PRIVATE_INCLUDES = $(I_SUB1) $(I_SUB2)

I_SUB1 = dsb_clr.f kuse_com kars_com
I_SUB2 =

#  List of platform dependent private include files.  These are include 
#  files  which form part of the package and are required in order to build
#  it, but which are not required by users of it.

PLATFORM_PRIVATE_INCLUDES = $(PI_SUB1) $(PI_SUB2)

PI_SUB1 =
PI_SUB2 =

#  List of external include files.  These are files which are required
#  in order to build the package but form part of other, externally
#  installed packages.  This list should contain the names used to
#  reference the files within the source code, not the actual names of
#  the files.

EXTERNAL_INCLUDES = DAT_PAR DAT_ERR SAE_PAR PAR_ERR MSG_PAR NDF_PAR PRM_PAR \
CAT_PAR CAT_ERR

#  List of Fortran routines required for building the package.  This is
#  just a list of all the Fortran source files (excluding BLOCK DATA
#  routines and the main routine, which are treated separately).  

F_ROUTINES = $(F_SUB1) $(F_SUB2) $(EXE_SOURCES)

F_SUB1 = abload.f abscal.f acheck.f arb.f arith.f arrow.f arysrt.f \
asterix.f atfit.f atlasrd.f axmat.f badchk.f bbint.f bbody.f calcfx.f \
cdraw.f cheatsm.f cmatch.f comnds.f convol.f cpair.f ctrlc_ast.f dacfit.f \
dashit.f dbound.f decimate.f decode.f decrng.f deftyp.f dela.f dered.f \
detev.f dipso.f dparse.f dtoupp.f elfbps.f elfclr.f elfdep.f \
elfg.f elfinv.f elfit.f elfkey.f elfkj.f elfparea.f elfpflx.f elfpl.f \
elfpnt.f elfset.f elfsfun.f elfstor.f elfvar.f elfview.f endmsk.f \
ermess.f even.f ewmeas.f expand.f expar.f explabel.f fcaller.f fcdel.f \
fcpop.f fcrest.f fcsave.f fcsl.f filter.f findit.f flfix.f fneb.f fnum.f \
fouri.f fourier.f fpget.f fpush.f fword.f gammln.f gammode.f gauss.f \
gcfmode.f get0c.f get0i.f get0l.f get0r.f getcho.f getcom.f getcom2.f \
getdir.f gtenv.f getinp.f getndf.f getrng.f getstk.f gsermode.f gtslot.f \
handler.f heapsrt.f help1.f ihmmnt.f ihmrg.f ihrot.f ihrstr.f ihsmth.f \
ihsval.f ihubv.f incxy.f integrate.f intep.f ipset.f iruser.f \
isatm.f isbach.f itochr.f iwrite.f jsymbol.f kdcode.f lagint.f lagint4.f \
laguerre.f lalpha.f lblrun.f lenstr.f locdat.f lwrcor.f models.f modest.f \
mongowr.f move_real.f msgout.f ndfnam.f nebcont.f nebset.f nextk.f \
ocdraw.f options.f osp0rd.f osp0wr.f parse_env.f pdgfit.f perigram.f \
pltarr.f pointsym.f poly.f ppalet.f prpage.f putext.f pwritit.f range.f \
rdkybd.f rdndf.f rdstr.f read.f readvmsrec.f reca.f record.f remap.f \
repfrm.f reporting.f rerep.f resint.f resstk.f restore.f reverx.f \
rinteg.f rlint.f save.f savstk.f sdocont.f setcolours.f sgscurse.f \
slen.f smatin.f smooth.f snip.f sofar.f sort1i.f sp0rd.f sp0wr.f sp1rd.f \
sp2rd.f sp2wr.f spload.f srtbrk.f ssdraw.f sstrip.f star.f strrd.f \
swcor2.f swpcor.f sysexe.f tau.f toreal.f update.f upush.f va04a.f \
velsort.f voigt.f vpeak.f vsp0rd.f window.f wrcopy.f write.f write_ndf.f \
wrndf.f wrprop.f wtstruser.f xcorre.f xdcode.f xhelp.f xsort.f xsprmpt.f \
yendmsk.f yxn.f zanstra.f croper.f rdcat.f getcat.f gtctcl.f rdctd.f

F_SUB2 = 

#  List of Fortran BLOCK DATA routines.

BLOCK_DATA = $(B_SUB1) $(B_SUB2)

B_SUB1 = is_blk.f
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

C_SUB1 = handlev.c rdkbd2.c
C_SUB2 = 

#  Main routine source name.

EXE_SOURCES = $(EXE_1_SOURCE) $(EXE_2_SOURCE)

EXE_1_SOURCE = run_dipso.c
EXE_2_SOURCE =

#  Main binary names.

EXECUTABLES = $(EXE_1) $(EXE_2)

EXE_1 = dipso
EXE_2 =

#  List of help library sources.

HELP_SOURCES = 

#  List of ancilliary data files.

DATA_FILES = demo1.cmd demo2.cmd DEMO1_STK.sdf DEMO2.sdf exercise.cmd \
testspec2.cmd ATLIST.DAT ATOMIC.DAT NEBCONT.DAT SWPCOR.DAT LWPLO.ABS \
LWRLO.ABS SWPLO.ABS command.hlp help.lis dipso.hlp updates.lis

#  Lists of Latex and hypertext documents.

LATEX_DOCS = sun50.tex
HYPERTEXT_DOCS = sun50.htx

#  List of documentation files.

DOCUMENTATION = $(LATEX_DOCS) $(HYPERTEXT_DOCS:.htx=.htx_tar) $(PKG_NAME).news

################################################################################
#-------------------------------------------------------------------------------

#  Define files required for building the package.
#  ==============================================
#
#  This section defines the set of files produced from the source files
#  when the package is built and installed.

#  Use only .o, .c, .f, .shl and .hlp suffix rules.

.SUFFIXES:
.SUFFIXES: .o .c .f .shl .hlp

#  List of files which must be built from the source files before the
#  package can be installed for use.  This should comprise all the files
#  that are required to use the package (but excluding the date stamp
#  file).

BUILT_FILES = $(PUBLIC_SCRIPTS) $(OBJECT_LIBRARIES) $(EXECUTABLES) \
$(HELP_FILES) $(DATA_FILES) user.f $(STARTUP_SCRIPT) $(PUBLIC_INCLUDES) \
$(EXE_1_OBJECT) 

#  List of links used to access include files during compilation.  This
#  should comprise all the external include files and any other include
#  files whose names do not exactly match the names used in the source
#  code.

INCLUDE_LINKS = $(EXTERNAL_INCLUDES) $(L_SUB1) $(L_SUB2) 

L_SUB1 = DECLARE_STKS DECLARE_DATA DECLARE_LBLS DECLARE_PLTS DSB_CLR \
         KUSE_COM KARS_COM
L_SUB2 =

#  Rules to set up links to locate each of the above include files.

SAE_PAR:   $(STAR_INC)/sae_par;   $(LINK) $? $@
DAT_PAR:   $(STAR_INC)/dat_par;   $(LINK) $? $@
DAT_ERR:   $(STAR_INC)/dat_err;   $(LINK) $? $@
MSG_PAR:   $(STAR_INC)/msg_par;   $(LINK) $? $@
PAR_ERR:   $(STAR_INC)/par_err;   $(LINK) $? $@
NDF_PAR:   $(STAR_INC)/ndf_par;   $(LINK) $? $@
PRM_PAR:   $(STAR_INC)/prm_par;   $(LINK) $? $@
CAT_PAR:   $(STAR_INC)/cat_par;   $(LINK) $? $@
CAT_ERR:   $(STAR_INC)/cat_err;   $(LINK) $? $@

KUSE_COM:       kuse_com;         $(LINK) $? $@
KARS_COM:       kars_com;         $(LINK) $? $@
DSB_CLR:        dsb_clr.f;        $(LINK) $? $@
DECLARE_STKS:   declare_stks.f;   $(LINK) $? $@
DECLARE_DATA:   declare_data.f;   $(LINK) $? $@
DECLARE_LBLS:   declare_lbls.f;   $(LINK) $? $@
DECLARE_PLTS:   declare_plts.f;   $(LINK) $? $@


#  Rules for extracting source files from the source archive.

$(PUBLIC_SCRIPTS) $(DATA_FILES) $(EXE_SOURCES) \
$(HELP_SOURCES) $(STARTUP_SCRIPT):
	$(TAR_OUT) $(PKG_NAME)_source.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi

$(F_SUB1) $(I_SUB1) $(IP_SUB1) $(B_SUB1) $(C_SUB1) user.f:
	$(TAR_OUT) $(PKG_NAME)_sub1.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi

$(F_SUB2) $(I_SUB2) $(IP_SUB2) $(B_SUB2) $(C_SUB2):
	$(TAR_OUT) $(PKG_NAME)_sub2.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi

#  Rules for extracting platform specific source files from the source
#  archives.

#  It is quite likely that there are no platform-specific source files
#  and that the macro PF_SUBn is empty. To prevent a syntax error in
#  make, a dummy target is present.

$(PF_SUB1) dummy_target1:
	$(TAR_OUT) $(PKG_NAME)_sub1.tar $@_$(SOURCE_VARIANT)
	@ if test -f $@_$(SOURCE_VARIANT); then :;\
           else echo $@_$(SOURCE_VARIANT) is not in the tar file; exit 1; fi
	mv $@_$(SOURCE_VARIANT) $@

$(PF_SUB2) dummy_target2:
	$(TAR_OUT) $(PKG_NAME)_sub2.tar $@_$(SOURCE_VARIANT)
	@ if test -f $@_$(SOURCE_VARIANT); then :;\
           else echo $@_$(SOURCE_VARIANT) is not in the tar file; exit 1; fi
	mv $@_$(SOURCE_VARIANT) $@

#  List of object files produced by compiling the source code and rules
#  for performing the compilations.

OBJECT_FILES = $(O_SUB1) $(O_SUB2) $(EXE_2_OBJECT)

O_SUB1 = $(B_SUB1:.f=.o) $(F_SUB1:.f=.o) user.o $(C_SUB1:.c=.o) $(PF_SUB1:.f=.o)

O_SUB2 = $(B_SUB2:.f=.o) $(F_SUB2:.f=.o) $(C_SUB2:.c=.o) $(PF_SUB2:.f=.o)

EXE_1_OBJECT = $(EXE_1_SOURCE:.c=.o)

EXE_2_OBJECT = $(EXE_2_SOURCE:.f=.o)

.c.o:
	$(CC) $(CFLAGS) -c $<
.f.o:
	$(FC) $(FFLAGS) -c $<

#  List of object library files to be built and rules for building
#  them.

OBJECT_LIBRARIES = libdipso.a 

libdipso.a: $(O_SUB1)
	$(AR_IN) $@ $?
	$(RANLIB) $@

#  List of executables and rules for building them.

$(EXE_1): $(EXE_1_OBJECT) $(OBJECT_LIBRARIES) $(LINK_SCRIPT)
	$(ELINK) $(EXE_1_OBJECT) -o $(EXE_1) $(OBJECT_LIBRARIES) \
           $(LDFLAG1) -L. $(ADDLIBS) -L$(STAR_LIB) `./$(LINK_SCRIPT)` $(LDFLAG2)

my_dipso: ./user.o $(MY_OBJECTS)
	$(ELINK) $(DIPSOLIB)/$(EXE_1_OBJECT) -o $(EXE_1) ./user.o \
           $(MY_OBJECTS) $(LDFLAG1) $(ADDLIBS) -L$(DIPSOLIB) \
           -L$(STAR_LIB) `$(DIPSOLIB)/$(LINK_SCRIPT)` $(LDFLAG2)
	chmod 755 $(EXE_1)
	strip $(EXE_1)
	@ echo " " 
	@ echo "Linked a custom version of DIPSO"
	@ echo "Please re-alias the dipso command to use the new version"
	@ echo "i.e., alias dipso `pwd`/dipso"
	@ echo " " 


#  Rules for building the help libraries.

HELP_FILES = $(HELP_SOURCES:.hlp=.shl)

.hlp.shl:
	$(STAR_BIN)/hlib $<

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
INSTALL_ETC = $(INSTALL)/etc/$(PKG_NAME)
INSTALL_HELP = $(INSTALL)/help/$(PKG_NAME)
INSTALL_INC = $(INSTALL)/include
INSTALL_LIB = $(INSTALL)/lib

#  List of directories actually used for installation (selected from
#  those above) and rules to create them.

INSTALL_DIRS = $(INSTALL_BIN) $(INSTALL_DATES) $(INSTALL_DOCS) \
               $(INSTALL_HELP) $(INSTALL_ETC) $(INSTALL_INC)


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
              cp -p $$f $(INSTALL_BIN);\
              chmod 755 $(INSTALL_BIN)/$$f;\
              rm -f $$f;\
              $(LINK) $(INSTALL_BIN)/$$f $$f;\
           else :; fi;\
        done
#
#  Install the public include files, giving them world read permission.
	for f in $(PUBLIC_INCLUDES) ""; do \
           if test -n "$$f"; then \
              cp -p $$f $(INSTALL_INC);\
              chmod 644 $(INSTALL_INC)/$$f;\
              rm -f $$f;\
              $(LINK) $(INSTALL_INC)/$$f $$f;\
           else :; fi;\
        done
#
#  Install the executables, giving them world execute permission.
	for f in $(EXECUTABLES) ""; do \
           if test -n "$$f"; then \
              cp -p $$f $(INSTALL_BIN);\
              chmod 755 $(INSTALL_BIN)/$$f;\
              rm -f $$f;\
              $(LINK) $(INSTALL_BIN)/$$f $$f;\
           else :; fi;\
        done
#
#  Install the data files, etc, giving them world read permission.
	for f in $(DATA_FILES) $(OBJECT_LIBRARIES) $(EXE_1_OBJECT) user.f ""; do \
           if test -n "$$f"; then \
              cp -p $$f $(INSTALL_BIN);\
              chmod 644 $(INSTALL_BIN)/$$f;\
              rm -f $$f;\
              $(LINK) $(INSTALL_BIN)/$$f $$f;\
           else :; fi;\
        done
#
#  Install the help files, giving them world read permission.
	for f in $(HELP_FILES) ""; do \
           if test -n "$$f"; then \
              cp -p $$f $(INSTALL_HELP);\
              chmod 644 $(INSTALL_HELP)/$$f;\
              rm -f $$f;\
              $(LINK) $(INSTALL_HELP)/$$f $$f;\
           else :; fi;\
        done
#
# Install the SPECDAT files
	if test -n dipso_specdat.tar; then \
           pwd=`pwd`;\
           (cd $(INSTALL_ETC);\
           for f in dipso_specdat.tar ""; do \
              if test -n "$$f"; then \
                 $(TAR_OUT) $$pwd/$${f};\
                 chmod 644 *;\
              else :; fi;\
           done);\
        fi
#
#  Install the mk script and makefile (needed so that users can make my_dipso).
	for f in mk makefile ""; do \
           if test -n "$$f"; then \
              cp -p $$f $(INSTALL_BIN);\
           else :; fi;\
        done
#
#  Install the package startup script.  The package version number,
#  the name of the directory containing the installed binaries and 
#  interface files, the starlink executable directory, and the name 
#  of the directory containing the help libraries must be edited into 
#  this, and execute permission given. Leave the original file in place.
	if test -n "$(STARTUP_SCRIPT)"; then \
           sed -e s#INSTALL_BIN#$(INSTALL_BIN)#g \
               -e s#INSTALL_HELP#$(INSTALL_HELP)#g \
               -e s#INSTALL_ETC#$(INSTALL_ETC)#g \
               -e s#STAR_BIN#$(STAR_BIN)#g \
               -e s#PKG_VERS#$(PKG_VERS)#g \
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
	   if test -n "$$f" -a -f $(INSTALL_BIN)/$$f; then \
	      rm -f $$f;\
	      cp -p $(INSTALL_BIN)/$$f .;\
	      rm -f $(INSTALL_BIN)/$$f;\
	   else :; fi;\
	done
#
#  Deinstall the public include files, if installed versions exist.
	- for f in $(PUBLIC_INCLUDES) ""; do \
	   if test -n "$$f" -a -f $(INSTALL_INC)/$$f; then \
	      rm -f $$f;\
	      cp -p $(INSTALL_INC)/$$f .;\
	      rm -f $(INSTALL_INC)/$$f;\
	   else :; fi; \
	done
#
#  Deinstall the executables, if installed versions exist.
	- for f in $(EXECUTABLES) ""; do \
           if test -n "$$f" -a -f $(INSTALL_BIN)/$$f; then \
              rm -f $$f;\
              cp -p $(INSTALL_BIN)/$$f .;\
              rm -f $(INSTALL_BIN)/$$f;\
           else :; fi;\
        done
#
#  Deinstall the SPECDAT files, if installed versions exist.
	- for f in $(INSTALL_ETC) ""; do \
           if test -n "$$f" -a -d "$$f"; then \
              rm -rf $$f;\
           else :; fi;\
        done
#
#  Deinstall the data files, etc, if installed versions exist.
	- for f in $(DATA_FILES) $(OBJECT_LIBRARIES) $(EXE_1_OBJECT) user.f ""; do \
           if test -n "$$f" -a -f $(INSTALL_BIN)/$$f; then \
              rm -f $$f;\
              cp -p $(INSTALL_BIN)/$$f .;\
              rm -f $(INSTALL_BIN)/$$f;\
           else :; fi;\
        done
#
#  Deinstall the help files, if installed versions exist.
	- for f in $(HELP_FILES) ""; do \
           if test -n "$$f" -a -f $(INSTALL_HELP)/$$f; then \
              rm -f $$f;\
              cp -p $(INSTALL_HELP)/$$f .;\
              rm -f $(INSTALL_HELP)/$$f;\
           else :; fi;\
        done
#
#  Deinstall the mk script and makefile.
	- for f in makefile mk; do \
           if test -f $(INSTALL_BIN)/$$f; then \
              rm -f $(INSTALL_BIN)/$$f;\
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
	@ echo 
	@ echo 'DIPSO has now been installed. In order to test DIPSO, you should'
	@ echo 'first source the script $(INSTALL_BIN)/dipsosetup'
	@ echo 'to initialize the environment. You may then type the command'
	@ echo 
	@ echo '     dipso < $(INSTALL_BIN)/exercise.cmd'
	@ echo 
	@ echo 'which will exercise most of the DIPSO commands.'
	@ echo 
	@ echo 'In addition, you can verify the SPECDAT database files'
	@ echo 'using the command'
	@ echo 
	@ echo '    dipso < $(INSTALL_BIN)/testspec2.cmd'
	@ echo 
	@ echo 'Both of these tests require an xwindows device to plot on'
	@ echo 
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
	$(TAR_IN) - dipso_specdat.tar mk makefile $(DOCUMENTATION) \
           $(BUILT_FILES) $(DATE_STAMP) .BUILT \
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
	@- rm -f $(INCLUDE_LINKS) $(PRIVATE_INCLUDES) \
              $(HELP_SOURCES) $(PLATFORM_F) $(BLOCK_DATA) \
              $(C_ROUTINES) 1>/dev/null 2>/dev/null
	@- rm -f $(F_ROUTINES) 1>/dev/null 2>/dev/null
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

#  strip: strip the executables of debug information.
#  -------------------------------------------------

strip:
	strip $(EXECUTABLES)
	@ echo '*** Executables stripped'

#-------------------------------------------------------------------------------

#  Include file dependencies.
#  =========================

#  Object file dependencies on include files (or links to those include
#  files).  These are normally generated automatically from the source
#  files.

abload.o: KUSE_COM
acheck.o: KUSE_COM
atfit.o: declare_stks.f
atlasrd.o: DAT_ERR DAT_PAR DECLARE_DATA DECLARE_STKS MSG_PAR SAE_PAR
axmat.o: DECLARE_DATA DECLARE_STKS PRM_PAR SAE_PAR
badchk.o: SAE_PAR
cdraw.o: SAE_PAR
ctrlc_ast.o: SAE_PAR
decode.o: SAE_PAR
decrng.o: SAE_PAR
dipso.o: declare_data.f declare_lbls.f declare_plts.f declare_stks.f MSG_PAR PAR_ERR PRM_PAR SAE_PAR KUSE_COM KARS_COM
elfkey.o: SAE_PAR
elfkj.o: KARS_COM
elfvar.o: KARS_COM
elfview.o: KUSE_COM
expand.o: DECLARE_PLTS SAE_PAR
expar.o: SAE_PAR
fnum.o: KARS_COM
get0c.o: PAR_ERR SAE_PAR
get0i.o: PRM_PAR SAE_PAR
get0l.o: SAE_PAR
get0r.o: PRM_PAR SAE_PAR
getcat.o: SAE_PAR CAT_PAR MSG_PAR PAR_ERR     
getcho.o: SAE_PAR
getdir.o: SAE_PAR
getinp.o: DSB_CLR
getndf.o: DAT_PAR MSG_PAR NDF_PAR PAR_ERR SAE_PAR
getrng.o: PAR_ERR SAE_PAR
getstk.o: declare_stks.f
gtctcl.o: SAE_PAR CAT_PAR CAT_ERR PAR_ERR     
gtenv.o: SAE_PAR
gtslot.o: DSB_CLR
ihsval.o: declare_stks.f
ihsval_ndf.o: declare_stks.f
isatm.o: SAE_PAR
isbach.o: SAE_PAR
models.o: SAE_PAR
msgout.o: SAE_PAR
ndfnam.o: SAE_PAR
nebset.o: SAE_PAR
options.o: SAE_PAR
osp0wr.o: SAE_PAR
parse_env.o: SAE_PAR
pltarr.o: declare_lbls.f declare_plts.f
pwritit.o: SAE_PAR
rdcat.o:  SAE_PAR MSG_PAR CAT_PAR     
rdctd.o:  SAE_PAR CAT_PAR PRM_PAR DECLARE_STKS DECLARE_DATA
rdkybd.o: DSB_CLR
rdndf.o: DAT_PAR DECLARE_DATA DECLARE_STKS PRM_PAR SAE_PAR
rdstr.o: PAR_ERR SAE_PAR
read.o: DAT_PAR MSG_PAR SAE_PAR
reca.o: DSB_CLR SAE_PAR
repfrm.o: SAE_PAR
reporting.o: MSG_PAR SAE_PAR
rerep.o: MSG_PAR SAE_PAR
resstk.o: DAT_PAR SAE_PAR
restore.o: DAT_PAR DECLARE_STKS MSG_PAR SAE_PAR
save.o: DECLARE_STKS MSG_PAR SAE_PAR
savstk.o: DAT_PAR MSG_PAR SAE_PAR
sofar.o: KUSE_COM
sp0rd.o: DAT_PAR DECLARE_DATA DECLARE_STKS MSG_PAR SAE_PAR
sp0wr.o: DECLARE_DATA DECLARE_STKS MSG_PAR SAE_PAR
sp0wr_ndf.o: SAE_PAR
sp2wr.o: SAE_PAR
spload.o: SAE_PAR
sysexe.o: SAE_PAR
update.o: SAE_PAR
upush.o: declare_stks.f
wrcopy.o: SAE_PAR
write.o: DAT_PAR DECLARE_DATA DECLARE_STKS MSG_PAR PAR_ERR SAE_PAR
write_ndf.o: DAT_PAR SAE_PAR
wrndf.o: DAT_PAR DECLARE_DATA DECLARE_LBLS DECLARE_STKS SAE_PAR
wrprop.o: DAT_PAR SAE_PAR
xhelp.o: SAE_PAR
#-------------------------------------------------------------------------------
#
#  End of makefile.
#.
