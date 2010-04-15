#!/bin/csh -f
#+
#  Name:
#     restart
#
#  Purpose:
#     Start the REPACK system from Unix shell.
#
#  Type of Module:
#     C shell script.
#
#  Invocation:
#     start
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
#     JKA: J.K.Ashley (University of Leicester)
#     PAM: P. McGale (University of Leicester)
#
#  History:
#     21-JAN-1996 (JKA):
#       Original, based on PAM version of APR-1995.
#
#
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
if (-d $HOME/adam) then
   echo -n
else
   if (-f $HOME/adam) then
      echo "You have a file called adam in your home directory.  Please rename "
      echo "since adam must be a directory for ADAM files."
      exit
   else
      mkdir $HOME/adam
   endif
endif

#  Define the locations of the REPACK monolith and shell task soft links,
#  plus the location of the help library, if they are not alreday defined.
#  These are modified at install time by the makefile.

if !($?REBIN) then
   echo ""
   echo "   Environment Variable REBIN is not set.  Setting"
   echo "   REBIN to be INSTALL_BIN"
   echo ""
   setenv REBIN INSTALL_BIN
endif
if !($?RECAL) then
   echo ""
   echo "   Environment Variable RECAL is not set.  Setting"
   echo "   RECAL to be INSTALL_DATA"
   echo ""
   setenv RECAL INSTALL_DATA
endif
if !($?REDOCS) then
   echo ""
   echo "   Environment Variable REDOCS is not set.  Setting"
   echo "   REDOCS to be INSTALL_BIN"
   echo ""
   setenv REDOCS INSTALL_BIN
endif
if !($?REHELP) then
   echo ""
   echo "   Environment Variable REHELP is not set.  Setting"
   echo "   REHELP to be INSTALL_HELP"
   echo ""
   setenv REHELP INSTALL_HELP
endif

#  Set some preliminaries.

setenv START_RE yes
setenv REMESSAGE ${REDOCS}/repack.news

#
#  Define aliases for the applications.
#  There should be a plain alias, and a package specific alias
#  so thatapplications that have conflicting command names are
#  still available.  (REPACK doesn't need dual aliases).

alias re_menu   ${REBIN}/re_menu
alias re_imsrch ${REBIN}/re_imsrch
alias re_evsrch  ${REBIN}/re_evsrch
alias re_evget  ${REBIN}/re_evget
alias re_evmrg  ${REBIN}/re_evmrg
alias re_fitmrg ${REBIN}/re_fitmrg
alias re_fitmrgsh ${REBIN}/re_fitmrgsh
alias re_expos  ${REBIN}/re_expos
alias re_imexp  ${REBIN}/re_imexp
alias re_sort   ${REBIN}/re_sort
alias re_eoffset ${REBIN}/re_eoffset
alias re_exprof ${REBIN}/re_exprof
alias re_light  ${REBIN}/re_light
alias re_timexp ${REBIN}/re_timexp
alias re_spec   ${REBIN}/re_spec
alias re_pss  ${REBIN}/re_pss
alias re_bsub ${REBIN}/re_bsub
alias re_exp  ${REBIN}/re_exp
alias re_science  ${REBIN}/re_science
alias re_sci_par  source ${REBIN}/re_sci_par
#
#  Start ASTERIX if is has not previously been started.
#

if (! $?AST_ROOT) then
   aststart
endif

#
#  Display the message text.
#

if ( -f ${REMESSAGE} ) then
  cat ${REMESSAGE}
endif

#
#  Announce the availability of the REPACK commands.
#

echo ""
echo "   REPACK commands are now available -- (Version PKG_VERS)"
echo ""
#echo "   Type rehelp for help on REPACK commands"
echo ""

#
#  End
exit
