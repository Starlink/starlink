#!/bin/csh -f
#+
#  Name:
#     cursa.csh
#
#  Purpose:
#     Start the CURSA system from Unix shell.
#
#  Type of Module:
#     C shell script.
#
#  Invocation:
#     source cursa.csh
#
#  Description:
#     This procedure defining the links needed to execute 
#     each application from the current directory.
#
#  Notes:
#     The installation target is set outside of this script. 
#     A test is made to see if the environment variable INSTALL
#     has been set.
#
#  Authors:
#     BLY: M.J.Bly (Starlink, RAL)
#     {enter_new_authors_here}
#
#  History:
#     26-MAY-1995 (BLY):
#       Version for CURSA v1.0
#     14-OCT-1995 (ACD):
#       Added aliases for application catgscin (which is new in
#       release 1.1).
#     20-DEC-1996 (ACD):
#       Added aliases for application catselect (which is new in
#       release 2.1).
#     13-JUN-1997 (ACD):
#       Added aliases for applications catcoord, catchart, catchartrn
#       and catremote (which are new in release 3.1).  Also set
#       environment variable SKYCAT_CONFIG so that the default set
#       of remote catalogues is the copy of the HEASARC at Leicester.
#     13-NOV-1997 (ACD):
#       Added aliases for applications catphotomfit, catphotomtrn
#       and catphotomlst (which are new in release 4.1).
#     29-NOV-1998 (ACD):
#       Added aliases for application (Perl script) catremote.
#     {enter_changes_here}
#     29-NOV-1999 (ACD):
#       Added aliases for application catgrid.  Corrected the URL for
#       the default configuration file.
#     31-MAY-2001 (ACD):
#       Modified to correspond to the new catremote which is a Perl script
#       rather than a C++ program.
#-
#

#  Prepare to run ADAM applications if this has not been done already.
#  ===================================================================
#
#  Here look to see if there is an ADAM_USER directory.  If there is not
#  check whether or not there is an adam file that is not a directory.
#  If there is, issue a warning and exit.  Otherwise create the required
#  directory.
#
if (-d ${HOME}/adam) then
   echo -n
else
   if (-f ${HOME}/adam) then
      echo "You have a file called adam in your home directory.  Please rename "
      echo "since adam must be a directory for ADAM files."
      exit
   else
      mkdir ${HOME}/adam
   endif
endif

#
#  Define aliases for the applications.
#  There should be a plain alias, and a package specific alias
#  so thatapplications that have conflicting command names are
#  still available.

alias catcopy         INSTALL_BIN/catcopy
alias cursa_catcopy   INSTALL_BIN/catcopy

alias catheader       INSTALL_BIN/catheader
alias cursa_catheader INSTALL_BIN/catheader

alias catpair         INSTALL_BIN/catpair
alias cursa_catpair   INSTALL_BIN/catpair

alias catsort         INSTALL_BIN/catsort
alias cursa_catsort   INSTALL_BIN/catsort

alias catview         INSTALL_BIN/catview
alias cursa_catview   INSTALL_BIN/catview

alias catgscin        INSTALL_BIN/catgscin
alias cursa_catgscin  INSTALL_BIN/catgscin

alias catselect       INSTALL_BIN/catselect
alias cursa_catselect INSTALL_BIN/catselect

alias catcoord        INSTALL_BIN/catcoord
alias cursa_catcoord  INSTALL_BIN/catcoord

alias catchart        INSTALL_BIN/catchart
alias cursa_catchart  INSTALL_BIN/catchart

alias catchartrn          INSTALL_BIN/catchartrn
alias cursa_catchartrn    INSTALL_BIN/catchartrn

alias catphotomfit        INSTALL_BIN/catphotomfit
alias cursa_catphotomfit  INSTALL_BIN/catphotomfit

alias catphotomtrn        INSTALL_BIN/catphotomtrn
alias cursa_catphotomtrn  INSTALL_BIN/catphotomtrn

alias catphotomlst        INSTALL_BIN/catphotomlst
alias cursa_catphotomlst  INSTALL_BIN/catphotomlst

alias catgrid         INSTALL_BIN/catphotomlst
alias cursa_catgrid   INSTALL_BIN/catphotomlst

# Perl scripts.

alias catcdsin        INSTALL_BIN/catcdsin
alias cursa_catcdsin  INSTALL_BIN/catcdsin

alias catremote       INSTALL_BIN/catremote
alias cursa_catremote INSTALL_BIN/catremote

# tcl/tk script.

alias xcatview        INSTALL_BIN/xcatview
alias cursa_xcatview  INSTALL_BIN/xcatview

#
#  Environment variables for catremote.
#  ===================================
#
#  Define the utility program to be used to submit the query.  The options
#  are:
#    "/star/bin/cursa/geturl" -- geturl utility, 
#    "lynx -source"           -- lynx command line browser,
#    "java  UrlReader"        -- Java utility.
#    "wget -q -O -"           -- wget utility (probably RedHat Linux only).

setenv  CATREM_URLREADER "INSTALL_BIN/geturl"

#
#  Specify the configuration file to be used.  The URL given here is the
#  the CURSA default.

setenv  CATREM_CONFIG  http://www.roe.ac.uk/acdwww/catremote/cursa.cfg

#
#  Define the maximum number of objects which may be included in the
#  returned table.

setenv  CATREM_MAXOBJ  200

#
#  Specify whether the URL constituting the query is echoed to the
#  command line.  The options re:
#    no   -  do not echo the URL (default),
#    yes  -  echo the URL.

setenv  CATREM_ECHOURL no

#
#  Set the Java CLASSPATH environment variable to pick up the URLreader
#  (note that CLASSPATH has to be set rather than setting the corresponding
#  command line option because the latter does not work on alphas).

setenv CLASSPATH INSTALL_BIN:/usr/lib/netscape/java/classes


#
#  Announce the availability of the CURSA commands.

echo ""
echo "   CURSA commands are now available -- (Version PKG_VERS)"
echo ""

#
# end
#
