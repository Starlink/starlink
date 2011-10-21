changequote([[,]])dnl
changecom[[]]dnl   no comments
#! /bin/sh -
# @edited_input@
dnl __GENMSG and __SCRIPTNAME are substituted by m4 within Makefile.am
# __GENMSG
# $Id$

#+
# Name:
ifelse(__SCRIPTNAME, alink,
[[#     alink]],
[[#     ilink]])
#
# Purpose:
ifelse(__SCRIPTNAME, alink,
[[#     link an ADAM A-task or A-task monolith for Unix]],
[[#     link an ADAM I-task for Unix]])
#
# Invocation:
#       % __SCRIPTNAME [-xdbx] [__SCRIPTNAME[[]]_opts] [linker_opts] prog_module [other_arguments]
#
# Arguments:
#       __SCRIPTNAME[[]]_opts are:
#          -xdbx or --xdbx: Must be the first argument if it is used.
#            Its effect is to add a -g option to the compile/link command,
#            create a dummy source file for dtask_main, the top-level routine 
#            of an ADAM task and to prevent the deletion of the otherwise 
#            temporary dtask_applic files. This overcomes some problems using
#            xdbx and ups on Suns and may be helpful in other cases where
#            debuggers are used.
#
#          --verbose: echoes the generated compiler line at the end
#
#          --static: link the monolith against static libraries only
#
#       linker_opts: option arguments preceding prog_module are 
#         permitted, and interpreted as if they formed part of the
#         [other_arguments]
#
#       prog_module specifies a file containing the task's main subroutine.
#         It may take the form prog.f, prog.c, prog.o or prog. If no extension
#         is given, .o will be assumed. prog must be the name of the task's 
#         main subroutine, and will be the name of the executable produced.
#         The main subroutine must have one argument, INTEGER for Fortran
#         and (int *) for C.  If the main subroutine is a .c file, a wrapper
#         subroutine is created to interface between it and the ADAM task
#         fixed part.  If the main subroutine is a prog.o, then it is treated
#         as a Fortran or C file depending on which one of prog.f or prog.c
#         exists.
#       
#       other_arguments is a space-separated list list of modules, and 
#         compiler or linker options. Fortran and C source files may be
#         mixed, they will be presented to the appropriate compiler.
#         Flags other than -I required for the C compiler must be specified
#         in the CFLAGS environment variable.
#
# Copyright:
#       Copyright (C) 1991-1995 Science & Engineering Research Council.
#       Copyright (C) 1996-1999, 2004 Central Laboratory of the Research Council.
#       Copyright (C) 2006 Particle Physics & Engineering Research Council.
#       Copyright (C) 2008 Science and Technology Facilities Council.
#       All Rights Reserved.
#
#  Licence:
#     This program is free software; you can redistribute it and/or
#     modify it under the terms of the GNU General Public License as
#     published by the Free Software Foundation; either version 2 of
#     the License, or (at your option) any later version.
#     
#     This program is distributed in the hope that it will be
#     useful,but WITHOUT ANY WARRANTY; without even the implied
#     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
#     PURPOSE. See the GNU General Public License for more details.
#     
#     You should have received a copy of the GNU General Public License
#     along with this program; if not, write to the Free Software
#     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
#     02111-1307, USA
#
# Authors:
#	CAAG: C A A Goswell (RLVAD::CAAG)
#       AJC:  A J Chipperfield (Starlink)
#       NG:   Norman Gray (Starlink)
#       PWD:  Peter W. Draper (JAC, Durham University)
#
# History:
#	24.07.1991 (CAAG):
#	   Original version (RLVAD::CAAG)
#       01.08.1991 (AJC):
#          Add ADAM bits (RLVAD::AJC)
#       23.06.1992 (AJC):
#          Add HLP and PSX and terminal size libraries
#        6.11.1992 (AJC):
#          Add REQUEST to PUT and GET_CURRINFO in DTASK_APPLIC
#          Set REQUEST to ACT__END
#          Private libs
#          Add MESSYS and MSC
#          Add ADAM and MSP
#       27.04.1993 (AJC):
#          Improve comments
#          Remove FFLAGS
#          Add -B for static and dynamic link options
#          Add directory of libraries (to be edited during installation)
#          Use library link scripts
#          Don't pre-compile .f files
#       29.06.1993 (AJC):
#          Put -L's before $(ARGS)
#          Include both new and Starlink libraries
#       08.11.1993 (AJC):
#          Don't assume .o for arguments other than the first.
#          Simplify link files for efficiency.
#          Fortran compiler name to be edited
#          Allow pathname on first argument
#       10.12.1993 (AJC):
#          Switch -Ls to after $ARGS
#       10.02.1994 (AJC):
#          Add -xdbx option to aid debugging
#       26.05.1995 (AJC):
#          Add ADAM_TASK_TYPE switch to prevent SUBPAR_DEACT
#       30.01.1996 (AJC):
#          Add TTYPE as argument of SUBPAR_DEACT
#       23.09.1996 (AJC):
#          Don't report error on rm dtask_applic.[fo]
#       21.11.1996 (AJC):
#          Allow for shareable linking (currently for Linux)
#       24-MAR-1998 (AJC):
#          Allow C source files.
#       14-OCT-1998 (AJC):
#          Retain directory spec for C files
#          Add -D's to CARGS
#          Put CARGS before standard -I's
#          Let installation remove repeat -I if INSTALL==STARLINK
#       10-MAY-1999 (AJC):
#          Don't search shared lib directories
#          Include ALINK_FLAGS1/2 to alter this if required.
#       10-MAY-1999 (AJC):
#          ilink produced as a mod of alink, removing parameter system
#          de-activation. 
#       16-FEB-2004 (NG):
#          (1) Permit [linker_options] before prog_module -- makes __SCRIPTNAME easier
#          to use with Makefiles generated by automake.  Also changed spelling
#          of `inquiry' to `enquiry'.
#          (2) Various values (FC, FCFLAGS, CC, CFLAGS) replaced by
#          autoconf-style @variable@ substitutions, mimicing the
#          install-time editing that used to be performed by the
#          makefile.  This meant that the ALINK_FLAGS1/2 could be
#          removed.  Also removed the paths from two INCLUDES, and
#          changed the dtask_applic.f include file names to uppercase,
#          since the FCFLAGS substitution includes a -I option, and since the
#          files in question are now installed in uppercase forms.
#          (3) Force the monolith to be linked statically.  This might
#          need further work, as the linker flags might be compiler or
#          loader specific.
#        26-APR-2004 (NG):
#          Generate both alink.in and ilink.in from ailink.in.m4.
#          Do linking using dtask_libtool.
#        1-AUG-2004 (NG): 
#          For subsequent modifications, see the CVS commit notes for 
#          successive revisions.
#          Check for -dylib_file as a linker option (OS X). Has an argument which
#          should also be gathered and not used for the program name.
#-

# Strip off `our' options from the beginning of the list of arguments
includedebug=false
verbose=false
staticlink=false
found_c_files=false
while [ $# != 0 ]
do 
    case $1 in
        -xdbx|--xdbx)
            includedebug=:
            ;;
        --verbose)
            verbose=:
            ;;
        --static)
            staticlink=:
            ;;
        -?|--help)
            cat >&2 <<FOO
usage: `basename $0` [__SCRIPTNAME-opts] [linker-opts] <name of program> [ other files, libraries and options ]
       `basename $0` [--help | --version]
	
e.g.
\$ f77 -c fred-aux.o
\$ $0 fred.f fred-aux.o -lgks

[__SCRIPTNAME-opts] are:
    --xdbx:     include debugging support (or -xdbx)
    --verbose:  be verbose
    --static:   link statically

This script was configured with
    CC            @CC@
    CFLAGS        @CFLAGS@
    STAR_CPPFLAGS @STAR_CPPFLAGS@
    FC            @FC@
    FCFLAGS       @FCFLAGS@
    STAR_FCFLAGS  @STAR_FCFLAGS@
    STAR_LDFLAGS  @STAR_LDFLAGS@
    bindir        @bindir@
    libdir        @libdir@
    staretcdir    @staretcdir@

If you need to override the libtool which is used to do the link
(@bindir@/dtask_libtool), 
do so by defining the environment variable DTASK_LIBTOOL to the full
path of the configured libtool you wish to use.
FOO
            exit 1
            ;;
        --version)
            echo "@PACKAGE_NAME@ version @PACKAGE_VERSION@" >&2
            exit 1
            ;;
        *)
            # not one of `our' options -- stop looking
            break
            ;;
    esac
dnl shift is an m4 macro
    [[shift]]
done

if test ! -z "$STARLINK_DIR";
then
  relocate=1
  star_libdir=${STARLINK_DIR}/lib
  star_bindir=${STARLINK_DIR}/bin
  star_incdir=${STARLINK_DIR}/include
  star_etcdir=${STARLINK_DIR}/etc
else
  relocate=0
  star_libdir=@libdir@
  star_bindir=@bindir@
  star_incdir=@includedir@
  star_etcdir=@staretcdir@
fi

star_cppflags="@STAR_CPPFLAGS@"
star_fcflags="@STAR_FCFLAGS@"
star_ldflags="@STAR_LDFLAGS@"

if [ $relocate -ne 0 ]
then
  star_cppflags=`echo $star_cppflags | sed -e "s,@includedir@,$star_incdir,"`
  star_fcflags=`echo $star_fcflags | sed -e "s,@includedir@,$star_incdir,"`
  star_ldflags=`echo $star_ldflags | sed -e "s,@libdir@,$star_libdir,"`
fi

# Get name of program by enquiry if necessary.
# Shift any initial option arguments into INITOPTS.
# Stop as soon as a non-option argument appears (and assign it to PROGNAME),
# or when we run out of arguments.
PROGNAME=
INITOPTS=
while [ $# != 0 -a -z "$PROGNAME" ]
do
    case $1 in
        -dylib_file)
            INITOPTS="$INITOPTS $1 $2"
            shift
            ;;
        -*)
            INITOPTS="$INITOPTS $1"
            ;;
        *)
            PROGNAME=$1
            ;;
    esac
    [[shift]]
done

# No PROGNAME specified yet, so prompt for it.
if [ -z "$PROGNAME" ]
then
    echo >&2 -n `basename $0`: 'name of execution module: '
    read PROGNAME
fi

# Next, determine the name of the program, allowing a certain
# amount of flexibility.  First get any path component.
DIR=`dirname $PROGNAME`

# Set PROGNAME to the name to be called by DTASK_APPLIC
#     EXENAME  to the name to call the executable
#     ARGS     to the object file to be linked (first of several)

found_object_file=:
case $PROGNAME in
    *.o)
        EXENAME=`basename $PROGNAME .o`
        ;;
    *.f)
        EXENAME=`basename $PROGNAME .f`
        found_object_file=false
        ;;
    *.c)
        found_c_files=:
        EXENAME=`basename $PROGNAME .c`
        found_object_file=false
        ;;
    *)
        # Assume a .o extension
        EXENAME=`basename $PROGNAME`
        ;;
esac

if $found_object_file
then
    # We don't know what language the .o file came from
    if [ -f $EXENAME.c ]
    then
        # seems to be C, but...
        if [ -f $EXENAME.f ]
        then
            # ...ooops, we don't know if this .o file came from a .f 
            # or a .c source, since both are present.
            echo "$0: subroutine is $PROGNAME, but both $EXENAME.c and $EXENAME.f exist.  I'm confused!" >&2
            exit 1
        fi
        found_c_files=:
    fi
fi

# Is the subroutine a C file or a compiled C file?
if $found_c_files; then
    # It is, so add a C subroutine which can be called from Fortran,
    # which calls the subroutine in the argument.
    if $found_object_file
    then
        # we were given the object file, so don't compile it again
        CARGS=dtask_wrap.c
    else
        # need to compile $PROGNAME
        CARGS="${DIR}/${EXENAME}.c dtask_wrap.c"
    fi
    PROGNAME=dtask_wrap
    ARGS="${EXENAME}.o dtask_wrap.o"
    cat >dtask_wrap.c <<FOO
#include <f77.h>
void $EXENAME(int *status);
F77_SUBROUTINE(dtask_wrap)(INTEGER(fstatus));

F77_SUBROUTINE(dtask_wrap)(INTEGER(fstatus)) {
int status;
F77_IMPORT_INTEGER(*fstatus,status);
$EXENAME(&status);
F77_EXPORT_INTEGER(status,*fstatus);
return;
}
FOO
else
    # A Fortran file, or a compiled Fortran file.
    PROGNAME=$EXENAME
    if $found_object_file
    then
        # don't compile it again
        ARGS=${DIR}/${PROGNAME}.o
    else
        ARGS=${DIR}/${PROGNAME}.f
    fi
fi



# Add remaining arguments to ARGS, starting with any INITOPTS.
if [ -n "$INITOPTS" ]
then
    ARGS="$ARGS $INITOPTS"
fi

while [ $# != 0 ]
do
    case $1 in
        *.c) 
          found_c_files=:
          NAME=`basename $1 .c`
          CARGS="$CARGS $1"
          ARGS="$ARGS ${NAME}.o"
          ;;
        -I*)
          CARGS="$CARGS $1"
          ARGS="$ARGS $1"
          ;;
        -D*)
          CARGS="$CARGS $1"
          ARGS="$ARGS $1"
          ;;
        *)
          ARGS="$ARGS $1"
          ;;
    esac
    [[shift]]
done


# Compile C files if any.  This compiles all of the files listed in CARGS.
# Is this portable?
if $found_c_files
then
    $verbose && echo "Compiling C files"
    $verbose && echo @CC@ -c $star_cppflags @CFLAGS@ $CARGS
    @CC@ -c $star_cppflags @CFLAGS@ $CARGS
fi

cat >dtask_applic.f <<EOD
*+  DTASK_APPLIC_DEACT - routine to call an A-task routine
      SUBROUTINE DTASK_APPLIC ( CONTEXT, ACTCODE, ANAME, ACTPTR, SEQ,
     :  VALUE, SCHEDTIME, REQUEST, STATUS ) 
*    Description :
*     This routine is edited automatically to call the main application
*     routine.ifelse(__SCRIPTNAME, alink, [[ It is a modification of the DTASK_APPLIC_NEW routine to
*     include a parameter system deactivation sequence and handle the
*     normal A-task OK status.]])
*    Invocation :
*     CALL name[(argument_list)]
*    Parameters :
*     parameter[(dimensions)]=type(access)
*           <description of parameter>
*    Method :
*     The routine first copies details of this entry to the application
*     routine into the TASK library then it calls the application. 
ifelse(__SCRIPTNAME, alink,
[[*     On return from the application, it calls the parameter system 
*     deactivation routine.
*     Finally, it retrieves values which may have been altered by the 
*     application from the TASK library.]],
[[*     On return from the application it retrieves values which may have
*     been altered by the application from the TASK library.]])
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     W.F.Lupton (AAOEPP::WFL)
*    History :
*     20.05.1991:  Original mod of DTASK_APPLIC_NEW (RLVAD::AJC)
*     04.06.1991:  Update/correct comments (ROE::BMC)
*     07.06.1991:  remove PATH and MESSID (REVAD::BDK)
*     07.06.1991:  change NAMECODE to ACTCODE (REVAD::BDK)
*     22.08.1991:  add REQUEST (REVAD::BDK)
*     14.05.1993:  UNIX version for substituting PROGNAME (RLVAD::AJC)
*     29.07.2009:  Set message filter level (JAC::TIMJ)
*    endhistory
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'ACT_ERR'
*    Import :
      INTEGER CONTEXT
      INTEGER ACTCODE
      CHARACTER*(*) ANAME
      INTEGER ACTPTR
*    Import-Export :
      INTEGER SEQ
      CHARACTER*(*) VALUE
*    Export :
      INTEGER SCHEDTIME
      INTEGER REQUEST
*    Status :
      INTEGER STATUS
*    Local variables :
      INTEGER LSTAT
ifelse(__SCRIPTNAME, alink,
[[      CHARACTER*1 TTYPE
]])dnl
*-
      IF ( STATUS .NE. SAI__OK ) RETURN      
*
*   Copy information about this action into the TASK library
*
      CALL TASK_PUT_CURRINFO ( ACTPTR, CONTEXT, ACTCODE, ANAME, SEQ,
     :  VALUE, ACT__END, STATUS )
*
*   Set the message filtering level
*
      CALL MSG_IFGET( STATUS )
*
*  Permit users to set MSG tuning via environment variables.
*
      CALL MSG_TUNE( 'ENVIRONMENT', 0, STATUS )
*
*   Call the application routine.
*
      CALL $PROGNAME ( STATUS )
*
ifelse(__SCRIPTNAME, alink,
[[*   Run-down the parameter system - this writes any global associations 
*   and 'frees' associated files. If environment variable ADAM_TASK_TYPE
*   is not set to I, all parameters are reset to the 'ground' state.
*
      CALL ERR_MARK
      LSTAT = SAI__OK
      CALL PSX_GETENV( 'ADAM_TASK_TYPE', TTYPE, LSTAT )
      IF ( LSTAT .NE. SAI__OK ) THEN
         CALL ERR_ANNUL( LSTAT )      
         TTYPE = ' '
      ENDIF
      CALL ERR_RLSE

      CALL SUBPAR_DEACT( TTYPE, STATUS )      
*
]])dnl
*  Retrieve values that may have been set by the application routine.
*
      LSTAT = SAI__OK
      CALL TASK_GET_CURRINFO ( SEQ, VALUE, SCHEDTIME, REQUEST, LSTAT ) 

      END
EOD

LIBTOOL=${DTASK_LIBTOOL-$star_bindir/dtask_libtool}
echo LIBTOOL=$LIBTOOL

extra_mode_args=
if $staticlink
then
    extra_mode_args='-all-static'
fi

linkextraflags=
if $includedebug
then
    $verbose && echo "Including debugging support in dtask_main.f"
    linkextraflags="$linkextraflags -g"
    sed -e s#PROGNAME#$PROGNAME# $staretcdir/dtask_main.txt \
           >dtask_main.f
fi

# Compile dtask_applic.f
## We substitute in the values of @STAR_FCFLAGS@ and @STAR_LDFLAGS@ here.
## Don't include AM_FCFLAGS, since the dtask/Makefile.am changes this
## in ways which are not appropriate for these compile/link commands,
## since they are working in a different directory, building some completely
## different application or library.
cmpdtask="$LIBTOOL --mode=compile @FC@ @FCFLAGS@ $extra_mode_args $star_fcflags \
        -c dtask_applic.f"
$verbose && echo $cmpdtask
eval $cmpdtask

linkcmd="$LIBTOOL --mode=link @FC@ @FCFLAGS@ $star_fcflags $extra_mode_args $star_ldflags \
        -o $EXENAME \
        $linkextraflags \
        ${star_libdir}/dtask_main.o \
        dtask_applic.lo \
        ${star_libdir}/starMemInit.o \
        $ARGS \
        -lhdspar_adam \
        -lpar_adam \
        @DTASK_LINK_ADAM@ \
        @C_FC_FCLINK_MAGIC@"

# Substitute any -lX options which refer to a libtool library
# @libdir@/libX.la,
# with an explicit reference to that library.  We
# could be more sophisticated and recognise them also in directories
# indicated in -L options, but that's more than we need right now.
xlinkcmd=
for x in $linkcmd
do
    l=`expr x$x : 'x-l\(.*\)'`
    if [ -n "$l" -a -r $star_libdir/lib$l.la ]
    then
        xlinkcmd="$xlinkcmd $star_libdir/lib$l.la"
    else
        xlinkcmd="$xlinkcmd $x"
    fi
done

# Finally, do the link
$verbose && echo $xlinkcmd
eval $xlinkcmd

# preserve the exit status of the link command
linkstatus=$?

if $includedebug
then
    : nothing
else
    # Don't remove dtask_applic.{o,lo} since this will prevent running
    # 'make check' in this directory.
    $verbose && echo "rm -f dtask_applic.f dtask_wrap.c dtask_wrap.o"
    rm -f dtask_applic.f dtask_wrap.c dtask_wrap.o
fi

# pass on the exit status of the link command
exit $linkstatus
