#!/bin/csh

#+
#  Name:
#     iuedrsetup

#  Purpose:
#     Prepares for IUEDR use from the UNIX shell.

#  Type of Module:
#     C-shell script

#  Invocation:
#     iuedrsetup

#  Description:
#     This procedure starts IUEDR for use from UNIX by defining
#     the aliases needed to execute each application or command.
#     It also initialises ADAM, if this has not already been
#     done.

#  Authors:
#     DMILLS: Dave Mills (STARLINK)
#     MJC: Martin Clayton (Starlink, UCL)
#     {enter_new_authors_here}

#  History:
#     1993 March 01 (DMILLS):
#        Original version.
#     1995 December 01 (MJC):
#        New version - edited by mk install procedure.
#     {enter_further_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Prepare to run ADAM applications if this has not been done already.
#  ===================================================================
#
#  Here look to see if there is ADAM_USER directory.  If there is not
#  check whether or not there is an adam file that is not a directory.
#  If there is, issue a warning and exit.  Otherwise create the required
#  directory.
#
echo " "
echo " ------------ Initialising for IUEDR -------------"
if ( -d $HOME/adam ) then
   echo -n;

else
   if ( -f $HOME/adam ) then
      echo "You have a file called adam in your home directory.  Please rename it";
      echo "since adam must be a directory for ADAM files.";
      exit;

   else
      mkdir $HOME/adam;
   endif
endif

#
#  Define environment for the applications.
#  ====================================
#
setenv IUEDR_BASE INSTALL_BIN
setenv IUEDR_DIR INSTALL_BIN
setenv IUEDR_DATA ${IUEDR_BASE}/data
setenv IUEDR_DEMO ${IUEDR_BASE}/demo
setenv IUEDR_DOC ${IUEDR_BASE}/doc
setenv IUEDR_HYPER ${IUEDR_BASE}/hyper
setenv IUEDR_SG7 ${IUEDR_BASE}/sg7
setenv IUEDR_TEST ${IUEDR_BASE}/test
setenv IUEDR_USER ${IUEDR_BASE}/user
setenv IUEDR_HELP INSTALL_HELP/iuedr3
setenv iuedr_help INSTALL_HELP/iuedr3

#
#  Define symbols for the applications.
#  ====================================
#
alias iuehelp "${IUEDR_DIR}/iuehelp"
alias iuedr   "${IUEDR_DIR}/iuedr3"
alias iuedr3  "${IUEDR_DIR}/iuedr3"
alias iuewww  "Mosaic ${IUEDR_HYPER}/iuedr3.html &"
#
echo "                IUE data reduction"
echo "          Version PKG_VERS 24 January 1996"
echo " "
echo "          Type" \"iuehelp iuedr\" for help
echo "    or" \"iuehelp news\" for news on changes
echo " "
echo " Type" \"iuewww\" to start Mosaic documentation browser.
echo " "
echo " Type" \"iuedr\" to start the monolith.
echo " "
#
#  Exit the procedure.
#
