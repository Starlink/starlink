
#  N.B. the previous line should be blank.

#+
#  Name:
#     mk

#  Purpose:
#     Control the installation and deinstall of LaTeX2HTML

#  Type of Module:
#     Bourne shell script.

#  Description:
#     This script installs and deinstalls the Starlink modified
#     version of LaTeX2HTML. It assumes that the current directory
#     contains an unpacked release and that the "latex2html" command
#     is to be installed in "/usr/local/bin".

#  Invocation:
#     mk test
#     mk install
#     mk deinstall

#  Parameters:
#     test:
#        This runs the local installation test procedure making sure
#        that the "latex2html" command can run from the current
#        directory.
#     install:
#        This makes the script run the installation test and installs
#        the main script into "/usr/local/bin".
#     deinstall:
#        This removes the latex2html command from the "/usr/local/bin"
#        directory.

#  Copyright:
#     Copyright (C) 1998 Central Laboratory of the Research Councils

#  Authors:
#     PWD: Peter W. Draper (STARLINK, Durham University)
#     {enter_new_authors_here}

#  History:
#     25-MAY-1997 (PWD):
#        Original version.
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Installation directory.
INSTALLDIR="/usr/local/bin"

#.

#  Check the argument.
action="${*}"
if test "$action" = ""; then 
   echo "usage: mk test | install| deinstall" >&2
   exit 1
fi

#  And perform the required action.
if test "$action" = "test" -o "$action" = "install"; then

   #  Need to perform the installation test. Use GIFS and stop 
   #  prompt requests and messy output.
   echo ""
   echo "Performing the installation test."
   echo "---------------------------------"
   echo ""
   if test -e "install-test.log"; then
      rm -f install-test.log
   fi
   ./install-test -gif < /dev/null > install-test.log 2>&1

   # Look for "Well done!". This should mean that test succeeded.
   complete=`grep 'Well done!' install-test.log`
   if test "$complete" = ""; then 
      echo "Failed installation test; check install-test.log file and README."
      exit 1
   else 
      echo "Test completed."
   fi
   if test "$action" = "install" ; then
      if test -d "$INSTALLDIR"; then
         echo "Copying main script to: $INSTALLDIR"
         cp -p latex2html $INSTALLDIR/latex2html
      else
         echo "Sorry cannot install into: $INSTALLDIR; directory doesn't exist."
      fi
   fi
elif test "$action" = "deinstall"; then
   echo "Removing main script from: $INSTALLDIR"
   rm $INSTALLDIR/latex2html
else
   echo "usage: mk test | install| deinstall" >&2
   exit 1
fi

#  end of script
exit
