
#  This is a Bourne shell script.  In order to be POSIX-compliant, the
#  first line should be blank.

#+
#  Name:
#     mk
#
#  Version:
#     Version for Mk V library makefile.
#
#  Purpose:
#     Invoke make to build and install the AST package.
#
#  Type of Module:
#     Shell script.
#
#  Description:
#     This script should normally be used to invoke the make utility
#     to build and install the AST package and to perform other
#     housekeeping tasks.  It invokes the make utility after first
#     defining appropriate environment variables and macros for the
#     computer system in use.  This file also serves to document the
#     UNIX systems on which AST is implemented.
#
#  Invocation:
#     The user of this script should normally first define the SYSTEM
#     environment variable to identify the host computer system (see
#     the "Supported Systems" section).  This script should then be used
#     in the same way as the make utility would be used.  For instance,
#     to build, install and test AST, you might use the following
#     commands:
#
#        ./mk build
#        ./mk install
#        ./mk test
#        ./mk clean
#
#  Supported Systems:
#     The following systems are currently supported and may be
#     identified by defining the SYSTEM environment variable
#     appropriately before invoking this script:
#
#        alpha_OSF1
#           DEC Alpha machines running OSF1
#        sun4_Solaris
#           SUN Sparcstations running SunOS 5.x (Solaris)
#        ix86_Linux
#           Intel PC running Linux
#
#     This script will exit without action if the SYSTEM environment
#     variable is not defined.  A warning will be issued if it is
#     defined but is not set to one of the values above.  In this case,
#     no additional environment variables will be defined by this
#     script (any that are pre-defined will be passed on to make
#     unaltered).
#
#  Targets:
#     For details of what targets are provided, see the associated
#     makefile.  The latter will normally provide a default target
#     called "help", which outputs a message describing this script
#     and lists the targets provided.
#
#  External Dependencies:
#     The AST package depends on the following other libraries:
#
#        SLALIB
#           Positional astronomy library, as described in Starlink User
#           Note 67 (http://star-www.rl.ac.uk/cgi-bin/htxserver/sun67.htx/).
#           Note that either the FORTRAN or C version may be used.
#
#  Notes on Porting:
#     If your machine or system setup does not appear in this script,
#     then it should be possible to build and install AST simply by
#     adding a new case to this script with appropriate definitions
#     (probably based on one of the existing implementations).
#
#     The C code of AST is ANSI standard and should not contain machine
#     dependencies. However, if you are using FORTRAN with AST (either
#     writing programs in FORTRAN or linking with FORTRAN implementations
#     of other libraries, such as SLALIB or PGPLOT), then some code
#     changes may be needed to cater for the unavoidable platform
#     dependence of the C/FORTRAN interface. The only files affected
#     should be "f77.h" and "c2f77.c" (both are contained in the
#     "ast_source.tar" archive). The existing files should work unchanged
#     on most UNIX systems, however.
#
#  make Macros:
#     The following "global" make macros are used in the associated
#     makefile and may be changed by means of appropriate environment
#     variable definitions (in each case the default is shown in
#     parentheses).  Note that these macros are provided to allow
#     external control over the directories in which software is
#     installed, etc., so they should not normally be re-defined within
#     this script.
#
#        STARLINK (/star)
#           Pathname of the root directory beneath which Starlink
#           software is currently installed.  This indicates to
#           AST where to find other Starlink software (include
#           files, libraries, etc.) which it uses.
#
#           The only other item of Starlink software which is essential
#           when using AST is SLALIB. Either the FORTRAN or C version will
#           do.
#
#        INSTALL ($HOME)
#           Pathname of the root directory beneath which AST will
#           be installed for use.  Your home directory will be used by
#           default.  This macro is provided to allow AST to be
#           installed locally for personal use (e.g. during development
#           or testing).  It should be set to the $STARLINK directory if
#           you want to add AST into an already installed set of
#           Starlink software.  You should ensure that the appropriate
#           sub-directories appear on any relevant search paths which
#           your system uses for locating software (e.g. binaries and
#           libraries).
#
#           For example, you should put $INSTALL/bin on your PATH in order
#           to locate the "ast_link" script used for linking.
#
#        EXPORT (.)
#           Pathname of the directory into which compressed tar files
#           will be written if the "export" or "export_source" make
#           targets are used to produce an exportable copy of AST
#           or its source files.  The current working directory (i.e.
#           the AST source directory) will be used by default.
#
#     The following "local" make macros are used in the associated
#     makefile and should normally be overridden by environment variable
#     definitions within this script.  All the local macros that are
#     used in building a package should be overridden, even when the value
#     is the same as the default.  This documents which macros are used
#     and ensures that the package will continue to build correctly even
#     if the default values are changed.  Macros that are not used on a
#     particular machine (e.g. BLD_SHR) should not be overridden.  In each
#     case the default is shown in parentheses.
#
#        AR_IN (ar -r)
#           The command to use to insert an object (.o) file into an
#           archive (.a) library.  On some systems the variation 'ar r'
#           may be required instead.
#
#        BLD_SHR (:)
#           Command to build a shareable library when given three
#           arguments specifying (1) the name of the library file to be
#           built (2) a list of the object files to be used in the
#           library and (3) a list of any additional libraries against
#           which to link.  By default, it is assumed that shareable
#           libraries are not available, and the default acts as a null
#           command.
#
#        CC (c89)
#           The C compiler command to use.
#
#        CFLAGS (-O)
#           The C compiler options to use.
#
#        FLIBS ('')
#           A sequence of additional loader options to be supplied on the C
#           compiler command line in order to link a C program which makes
#           internal use of FORTRAN libraries. Typically, this will be a
#           list of the FORTRAN system libraries which are included as
#           standard by a FORTRAN compiler, but not by a C compiler. The
#           default is not to use any additional loader options.
#
#           This value is only required if you plan to write C programs but
#           are using FORTRAN versions of other libraries (such as SLALIB or
#           PGPLOT). If you do not supply this value, only the installation
#           test (the "test" target in the makefile) will be affected. AST
#           should still build and install correctly. Howver, you must ensure
#           that you supply the correct loader options when building your
#           own programs.
#
#        LINK (ln)
#           The command required to establish a link to a file.  The
#           default assumes POSIX.2 behavior, which only provides a
#           "hard" link operating within a single file system.  If the
#           host operating system allows "symbolic" links, then this
#           macro might be re-defined as 'ln -s'.  Alternatively, if the
#           use of multiple file systems is essential but not supported
#           by any form of link, then a copy command could be
#           substituted (e.g. 'cp -p'), at some cost in file space.
#
#        RANLIB (:)
#           The command required to "randomise" an object library.  By
#           default, this operation is not performed (the default acts
#           as a null command).  On systems which require it, this
#           should typically be set to 'ranlib'.
#
#        SHARE (.so)
#           The file type suffix to be applied to produce the name of a
#           shareable library file.  By default, the ".so" suffix is
#           applied without a library version number.  For systems which
#           support version numbers on shareable libraries, the macro
#           LIB_VERS is defined within the associated makefile and may
#           be used as part of a definition such as '.so.$(LIB_VERS)'.
#
#        TAR_IN (pax -w -v -x ustar -f)
#           Command to use to insert a file into a .tar archive file.
#           The default uses the POSIX.2 pax command, which is not
#           available on traditional UNIX systems.  These typically use
#           a tar command such as 'tar -cvhf' instead (if symbolic
#           links are supported, then an option to follow these must be
#           included in this command).
#
#        TAR_OUT (pax -r -f)
#           Command to use to extract a file from a .tar archive file.
#           The default uses the POSIX.2 pax command, which is not
#           available on traditional UNIX systems.  These typically use
#           a tar command such as 'tar -xf' instead.
#
#  Copyright:
#     <COPYRIGHT_STATEMENT>
#
#  Authors:
#     RFWS: R.F.Warren-Smith (Starlink)
#
#  History:
#     11-JUN-1996 (RFWS):
#        Original version, based on a standard template.
#     15-NOV-1996 (RFWS):
#        Added Linux support.
#-

#  Export "local" definitions to the environment for use by make.
      export AR_IN
      export BLD_SHR
      export CC
      export CFLAGS
      export FLIBS
      export LINK
      export RANLIB
      export SHARE
      export TAR_IN
      export TAR_OUT

#  Check that the SYSTEM environment variable is defined.
      if test "$SYSTEM" = ""; then
         echo "mk: Please define the environment variable SYSTEM to identify"
         echo "    your computer system (the prologue in the mk script file"
         echo "    contains more information if you require it)."

#  If OK, test for each recognised system.
      else
         case "$SYSTEM" in

#  DEC Alpha:
#  =========
#  DEC Alpha machines running OSF1.
#  -------------------------------
            alpha_OSF1)
               AR_IN='ar -r'
#               BLD_SHR=\
#'f() ld -shared -update_registry $(INSTALL)/share/so_locations -o $$1 $$2 $$3 \
#-lfor -lFutil -lUfor -lm -lots -lc; f'
               CC='cc'
               CFLAGS='-std1 -O -Olimit 600'
               FLIBS='-lfor -lFutil -lUfor -lm -lots -lc'
               LINK='ln -s'
               RANLIB=':'
#               SHARE='.so'
               TAR_IN='pax -w -v -x ustar -f'
               TAR_OUT='pax -r -f'
               echo "mk: Environment variables defined for $SYSTEM system"
               ;;

#  SUN4 systems:
#  ============
#  SUN Sparcstations running SunOS 5.x (Solaris).
#  ---------------------------------------------
            sun4_Solaris)
               AR_IN='ar -r'
#               BLD_SHR='f() ld -G -z text -o $$1 $$2; f'
               CC='cc'
               CFLAGS='-Xc -v -O -KPIC'
               FLIBS='-lM77 -lF77 -lsunmath -lm'
               LINK='ln -s'
#               SHARE='.so'
               TAR_IN='tar -cvhf'
               TAR_OUT='tar -xf'
               echo "mk: Environment variables defined for $SYSTEM system"
               ;;

#  PC systems:
#  ==========
#  Intel PC running Linux.
#  ----------------------
            ix86_Linux)
               AR_IN='ar r'
               BLD_SHR='f() { ld -shared -soname $$1 -o $$1 $$2;}; f'
	       CC='gcc'
               CFLAGS='-ansi -O2 -fPIC'
               FC='g77'
	       FFLAGS='-fno-second-underscore -O2 -fPIC'
               FLIBS='-lf2c'
               LINK='ln -s'
               RANLIB='ranlib'
               SHARE='.so'
               TAR_IN='tar -cvhf'
               TAR_OUT='tar -xf'
               echo "mk: Environment variables defined for $SYSTEM system"
               ;;

#  Issue a warning if SYSTEM is not recognised.
            *)
               SOURCE_VARIANT='unknown'
               echo "mk: WARNING: value of SYSTEM = $SYSTEM not recognised..."
               echo "             ...assuming default system characteristics"
               ;;
         esac

#  Invoke make with the appropriate environment variables set to override
#  default macros defined in the makefile.
         echo make -e $*
         make -e $*
      fi

#  End of script.
