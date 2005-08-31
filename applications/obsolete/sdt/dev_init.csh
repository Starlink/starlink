#+
#  Name:
#     dev_init.csh

#  Purpose:
#     Initialise platform-specific environment variables.

#  Type of Module:
#     C shell commands (to be sourced from a parent shell).

#  Description:
#     This file contains C shell commands which define platform-specific
#     environment variables commonly used as macros in standard makefiles to
#     eliminate platform dependencies. The values assigned are intended to be
#     appropriate for software development work (as opposed to software
#     releases) and are determined by the value of the SYSTEM environment
#     variable.

#  Invocation:
#     source $SDT_DIR/dev_init.csh

#  Parameters:
#     None. The command examines the SYSTEM environment variable.

#  Examples:
#     setenv SYSTEM alpha_OSF1
#     source $SDT_DIR/dev_init.csh
#        Defines platform-specific environment variables suitable for use in
#        makefiles during software development on alpha_OSF1 systems.

#  Supported Systems:
#     The following systems are currently supported and may be identified by
#     defining the SYSTEM environment variable appropriately before sourcing
#     this file:
#        alpha_OSF1
#	    DEC Alpha machines running OSF1
#        mips
#           DECstations running Ultrix
#        sun4
#           SUN Sparcstations running SunOS 4.x
#        sun4_Solaris
#           SUN Sparcstations running SunOS 5.x (Solaris)
#        ix86_Linux
#           PCs running Linux

#  Environment Variables:
#     The following environment variables are defined by the commands in this
#     file:
#
#        AR_IN
#	    The command to use to insert an object (.o) file into an
#	    archive (.a) library.
#        BLD_SHR
#	    Command to build a shareable library when given three
#	    arguments specifying (1) the name of the library file to be
#	    built (2) a list of the object files to be used in the
#	    library and (3) a list of any additional libraries against
#	    which to link.
#        CC
#	    The C compiler command to use.
#        CFLAGS
#	    The C compiler options to use.
#        FC
#	    The Fortran compiler command to use.
#        FFLAGS (-O)
#	    The Fortan compiler options to be used.
#        LINK
#	    The command required to establish a link to a file.
#        RANLIB
#	    The command required to "randomise" an object library.
#        SHARE
#	    The file type suffix to be applied to produce the name of a
#	    shareable library file.
#	 TAR_IN
#	    Command to use to insert a file into a .tar archive file.
#	 TAR_OUT (pax -r -f)
#	    Command to use to extract a file from a .tar archive file.

#  Copyright:
#     Copyright (C) 1993 Science & Engineering Research Council

#  Authors:
#     RFWS: R.F. Warren-Smith (STARLINK, RAL)
#     PWD: Peter W. Draper (Starlink, Durham University)
#     {enter_new_authors_here}

#  History:
#     22-JUN-1994 (RFWS):
#        Original version.
#     25-MAY-1997 (PWD):
#        Added Linux.
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Check that the SYSTEM environment variable is defined.
      if ( ! $?SYSTEM ) then
         echo 'dev: Please define the environment variable SYSTEM to identify'
         echo '     your computer system. See the file $SDT_DIR/dev_init.csh'
         echo '     for details of supported systems.'

#  If OK, test for each recognised system.
      else
         if ( ! $?STARLINK ) then
	    setenv STARLINK /star
	 endif

#  DEC Alpha:
#  =========
#  DEC Alpha machines running OSF1:
#  -------------------------------
         switch ( $SYSTEM )
            case alpha_OSF1:
               setenv AR_IN 'ar rls'
               setenv BLD_SHR 'echo >/dev/null'
               setenv CC 'cc'
               setenv CFLAGS "-std1 -g -I${STARLINK}/include"
               setenv FC 'f77'
               setenv FFLAGS '-g -check bounds -check overflow'
               setenv LINK 'ln -s'
               setenv RANLIB 'echo >/dev/null'
               setenv SHARE '.so'
               setenv TAR_IN 'tar -cvhf'
               #setenv TAR_OUT 'f() { touch $$2 };f'
               setenv STRIP 'echo >/dev/null'
               echo \
"+ Environment variables initialised for development on $SYSTEM"
               breaksw

#  DECstations:
#  ===========
#  DECstations running Ultrix.
#  --------------------------
            case mips:
               setenv AR_IN 'ar -rls'
               setenv BLD_SHR 'echo >/dev/null'
               setenv CC 'cc'
               setenv CFLAGS "-std -g -I${STARLINK}/include"
               setenv FC 'f77'
               setenv FFLAGS '-g'
               setenv LINK 'ln -f -s'
               setenv RANLIB 'echo >/dev/null'
               setenv SHARE '.so'
               setenv TAR_IN 'tar -cvhf'
               #setenv TAR_OUT 'f() { touch $$2 };f'
               setenv STRIP 'echo >/dev/null'
               echo \
"+ Environment variables initialised for development on $SYSTEM"
               breaksw

#  SUN4 systems:
#  ============
#  SUN Sparcstations running SunOS 4.x.
#  -----------------------------------
            case sun4:
               setenv AR_IN 'ar r'
               setenv BLD_SHR 'echo >/dev/null'
               setenv CC 'gcc'
               setenv CFLAGS "-ansi -g -fPIC -I${STARLINK}/include"
               setenv FC 'f77'
               setenv FFLAGS '-g -PIC -C'
               setenv LINK 'ln -s'
               setenv RANLIB 'ranlib'
               setenv SHARE '.so.$(LIB_VERS)'
               setenv TAR_IN 'tar -cvhf'
               #setenv TAR_OUT 'f() { touch $$2 };f'
               setenv STRIP 'echo >/dev/null'
               echo \
"+ Environment variables initialised for development on $SYSTEM"
               breaksw

#  SUN Sparcstations running SunOS 5.x (Solaris).
#  ---------------------------------------------
            case sun4_Solaris:
               setenv AR_IN 'ar -r'
               setenv BLD_SHR 'echo >/dev/null'
               setenv CC 'cc'
               setenv CFLAGS "-Xa -v -g -xsb -I${STARLINK}/include"
               setenv FC 'f77'
               setenv FFLAGS '-g -sb -C'
               setenv LINK 'ln -s'
               setenv RANLIB 'echo >/dev/null'
               setenv SHARE '.so.$(LIB_VERS)'
               setenv TAR_IN 'tar -cvhf'
               #setenv TAR_OUT 'f() { touch $$2 };f'
               setenv STRIP  'echo /dev/null'
               echo \
"+ Environment variables initialised for development on $SYSTEM"
               breaksw

            case ix86_Linux:
               setenv AR_IN 'ar rls'
               setenv BLD_SHR 'echo >/dev/null'
               setenv CC 'gcc'
               setenv CFLAGS "-g -Wall -I${STARLINK}/include"
               setenv FC 'g77'
               setenv FFLAGS '-g -Wall -fno-second-underscore'
               setenv LINK 'ln -s -f'
               setenv RANLIB 'echo >/dev/null'
               setenv SHARE '.so'
               setenv TAR_IN 'tar -cvhf'
               setenv TAR_OUT 'tar -xvf'
               setenv STRIP 'echo >/dev/null'
               echo \
"+ Environment variables initialised for development on $SYSTEM"
               breaksw

#  Issue a warning if SYSTEM is not recognised.
            default:
               echo "dev: WARNING: value of SYSTEM = $SYSTEM not recognised..."
               echo '              ...assuming default system characteristics'
         endsw
      endif

#  End of C shell commands.
