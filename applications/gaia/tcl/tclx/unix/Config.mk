#
# Config.mk --
#
#   Master configuration file for Extended Tcl.  This should be the only
# file you have to modify to get Extended Tcl to work.  It is used to
# set attributes that configure can't figure out and to override certain 
# attributes set by configure.
# 
#   All the values in this directory are set to reasonable defaults.  You might
# want to tune them to your taste.  You may set the value of "CC" and "CFLAGS"
# in the file or on the make command line or set them.  For example:
#
#       make -k CC=gcc CFLAGS=-O
#
#------------------------------------------------------------------------------
# Copyright 1992-1997 Karl Lehenbauer and Mark Diekhans.
#
# Permission to use, copy, modify, and distribute this software and its
# documentation for any purpose and without fee is hereby granted, provided
# that the above copyright notice appear in all copies.  Karl Lehenbauer and
# Mark Diekhans make no representations about the suitability of this
# software for any purpose.  It is provided "as is" without express or
# implied warranty.
#------------------------------------------------------------------------------
# $Id: Config.mk,v 8.10.2.3 1998/08/09 01:10:05 markd Exp $
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
#              READ THIS FIRST: FILE PATH SPECIFICATION RULES.
#------------------------------------------------------------------------------
# All paths to files outside of the distribution MUST follow these rules.
# The rules make it easy to specify locations of files either relative to
# the source or build directories or or as absolute directories.  If these
# rules are not followed, it will not build.  All values are defaulted to
# reasonable locations.  If Tcl and Tk are in directories that are siblings
# of the TclX source directory, things will probably work just fine.
#
# File paths MUST be one of:
#   o Absolute paths (starting with /), e.g.  /usr/local/lib/libtcl.a
#   o Paths relative to the source directory make macro ${srcbasedir}, e.g.
#     -I$(srcbasedir)/../tk4.0
#   o Paths relative to the build directory make macro ${bldbasedir}, e.g.
#     ${bldbasedir}/../tk4.0/libtk.a
#
# Other macros used in file paths:
#   o TCLX_FULL_VERSION is the full TclX version.
#   o TKX_FULL_VERSION is the full TkX version.
#   o prefix and exec_prefix.
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
# The Tcl source distribution directory, the path to tclConfig.sh, the Tcl
# library (libtcl8.0.a) and the flags neccessary to link with the Tcl shared
# library (libtcl8.0.so).  Note, access is required to tclInt.h which is not
# installed by Tcl.  If you want to use the installed Tcl library, set TCL_LIB
# to ${TCL_LIB_SPEC}.
# 

TCL_SRC=${srcbasedir}/../tcl8.0.3
TCL_BUILD=${bldbasedir}/../tcl8.0.3
TCL_CONFIG=${TCL_BUILD}/unix/tclConfig.sh
TCL_LIB=${TCL_BUILD_LIB_SPEC}

#------------------------------------------------------------------------------
# Unless configure is going to be run with --with-tk=NO, these defines must be
# set.  They define the directory containing the Tk source distribution, the
# path to tkConfig.sh, the path to the Tk library (libtk8.0.a) and the flags
# neccessary to link with the Tk shared library (libtk8.0.so).  If you want to
# use the installed Tk library, set TK_LIB to ${TK_LIB_SPEC}.

TK_SRC=${srcbasedir}/../tk8.0.3
TK_BUILD=${bldbasedir}/../tk8.0.3
TK_CONFIG=${TK_BUILD}/unix/tkConfig.sh
TK_LIB=${TK_BUILD_LIB_SPEC}

#------------------------------------------------------------------------------
# C compiler and debug/optimization/profiling flag to use.  Set by configure,
# and are normally overridden on the make command line (make CFLAGS=-g).  The
# can also be overridden here.

#CC=cc
#CFLAGS=-O

#------------------------------------------------------------------------------
# Definition of programs you wish to use. RANLIB is set by configure in the
# Makefiles, but they can be overridden here.
#

#RANLIB=ranlib

AR=ar
STRIP=strip

#------------------------------------------------------------------------------
# Location of optional tcl2c program.  Used if compiling with a Tcl that has
# the PlusPatch applied.

TCL2C = ${TCL_BUILD}/unix/tcl2c


#------------------------------------------------------------------------------
# X is often in strange places, override what configure figured out if
# its wrong.

#XINCLUDES=-I/usr/somewhere/include
#XLIBSW=-L/usr/somewhere/lib -lX11

#------------------------------------------------------------------------------
# EXtra flags:
#   o XCFLAGS - Extra compiler flags on all compiles and links
#   o XLDFLAGS - Extra compiler flags to specify at link time.
#   o XLDLIBS - Extra libraries to use at link time.

XCFLAGS=
XLDFLAGS=
XLDLIBS=

#------------------------------------------------------------------------------
# The following definition can be set to non-null for special systems
# like AFS with replication.  It allows the pathnames used for installation
# to be different than those used for actually reference files at
# run-time.  INSTALL_ROOT is prepended to $prefix and $exec_prefix
# when installing files.

INSTALL_ROOT =

#------------------------------------------------------------------------------
# Allow seperate prefixes for TclX and TkX, but default to standard prefix.

TCLX_PREFIX="${prefix}"
TCLX_EXEC_PREFIX="${exec_prefix}"
TKX_PREFIX="${prefix}"
TKX_EXEC_PREFIX="${exec_prefix}"

#------------------------------------------------------------------------------
# The TclX and TkX runtime directories.  This is where the shared runtime and
# help files are installed.

TCLX_INST_RUNTIME=${TCLX_PREFIX}/lib/tclX${TCLX_VERSION}
TKX_INST_RUNTIME=${TKX_PREFIX}/lib/tkX${TKX_VERSION}

#------------------------------------------------------------------------------
# The TclX and TkX exec runtime directories.  This is where the pkgIndex.tcl
# files are installed.

TCLX_EXEC_RUNTIME=${TCLX_EXEC_PREFIX}/lib/tclX${TCLX_VERSION}
TKX_EXEC_RUNTIME=${TKX_EXEC_PREFIX}/lib/tkX${TKX_VERSION}

#------------------------------------------------------------------------------
# The directories to install the executables in.

TCLX_INST_BIN=${TCLX_EXEC_PREFIX}/bin
TKX_INST_BIN=${TKX_EXEC_PREFIX}/bin

#------------------------------------------------------------------------------
# The directories to install the libraries in.

TCLX_INST_LIB=${TCLX_EXEC_PREFIX}/lib
TKX_INST_LIB=${TKX_EXEC_PREFIX}/lib

#------------------------------------------------------------------------------
# The directories the TclX include files are installed.

TCLX_INST_INCL=${TCLX_PREFIX}/include

#------------------------------------------------------------------------------
# Base manual directory where all of the man* and cat* directories live.

TCLX_INST_MAN=${TCLX_PREFIX}/man

#------------------------------------------------------------------------------
# Sections for Tcl commands and C function manual pages.

TCLX_MAN_CMD_SECTION=n
TCLX_MAN_FUNC_SECTION=3

#------------------------------------------------------------------------------
# The separator character used in the directory name of the cat* and man*
# manual directories.  This is usually empty or a period. i.e "/usr/man/man1"
# or "/usr/man/man.1".  Autoconf attempts to determine it but it can be
# overridden here.

#MAN_DIR_SEPARATOR=.


