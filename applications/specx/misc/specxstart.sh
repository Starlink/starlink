
#+
#  Name:
#     specxstart.sh
#
#  Purpose:
#     Start the Specx package from the C shell.
#
#  Type of Module:
#     Shell command list
#
#  Invocation:
#     , specxstart
#
#  Description:
#     This procedure starts the Specx package for use from the C shell by
#     setting a couple of environment variables and an alias to run the
#     executable programme.
#
#  Prior Requirements (edited for STANDALONE by Remo Tilanus)
#
#        SYS_SPECX: The directory with the executable system, including
#           standard command files. Normally .../.../specx
#
   if test -z $SYS_SPECX; then
     echo ' '
     echo '   You have to define SYS_SPECX to point to the SPECX directory,'
     echo '   i.e. export SYS_SPECX=.../.../specx.'
     echo ' '
     echo '   Then type: source ${SYS_SPECX}/specxstart to initialize the'
     echo '   SPECX environment. Then type specx to start the programme.'
     echo ' '
     echo '   You may want to edit specx.csh and put it somewhere in your'
     echo '   path as specx to do this for you or put the initialization'
     echo '   in your .profile file'
     echo ' '
     exit
   fi
#
#  Authors:
#     hme: Horst Meyerdierks (UoE, Starlink)
#     rpt: Remo Tilanus (JAC, Hilo)
#     rp:  Rachael Padman (MRAO, Cambridge)
#     timj: Tim Jenness (JAC, Hilo)
#     {enter_new_authors_here}
#
#  History:
#     11 Feb 1994 (hme):
#        Original Version.
#     16 Feb 1994 (rpt):
#        STAND-ALONE VERSION
#     30 Sep 1995 (rp):
#        Include SXG_GRAPHCAP
#     22 Aug 2005 (timj):
#        Clone from csh version
#     {enter_changes_here}
#
#-
#.
#
#  There is a file used by a READ/NAMELIST statement to set initial
#  values for a number of parameters. It is known as $SPECX_INIT.
#
   export SPECX_INIT=$SYS_SPECX/specx_init.dat
#
#  There is also the user's file which is @-ed at startup. If the user
#  has not yet set $SPXINIT to point to her own such file, then we will
#  use a default one.
#
   if test -z $SPXINIT; then
      export SPXINIT=$SYS_SPECX/init.spx
   fi
#
#  Point to specx.shl (.shl default) as help file.
#
if test -f $SYS_SPECX/specx.shl; then
   export SPECX_HELP=$SYS_SPECX/specx
else
   # Kluge this for now. This is caused by .shl files being installed
   # by default into $prefix/help not $prefix/share/package
   # Need to decide whether to force the install into share
   # or to dynamically edit this file at install time
   export SPECX_HELP=$SYS_SPECX/../../help/specx
fi
#
#  The standard command definitons use $SPECX_COMMAND for the directory
#  to find .spx files. This is the same as $SYS_SPECX.
#
   export SPECX_COMMAND=$SYS_SPECX
#
#  The current environment is dumped as commanded to the file SPECX_DUMP.
#  Initialize the value of this to specx.dmp
#
   export SPECX_DUMP=specx.dmp
#
#  Finally, the user wants to run the programme via the alias specx.
#  [this is no longer true since SPECX is now installed into the path
#  as a binary]
#
#   alias specx $SYS_SPECX/specx
#
#  PGPLOT X displayer and font file
#
#  setenv PGPLOT_DIR   $SYS_SPECX
#  setenv PGPLOT_FONT  $PGPLOT_DIR/grfont.dat
   export PGPLOT_XW_WIDTH=0.67
#
#  Point to graphics definition file for SXG routines
#
   export SXG_GRAPHCAP=$SYS_SPECX/graphcap.txt

#
#  Announce the availibility of the SPECX environment and how to start it.
#
   echo The SPECX environment variables have been initialised
   echo Please type \"specx\" to run up SPECX

#.
