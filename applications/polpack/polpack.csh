#+          
#  Name:
#     polpack.csh

#  Purpose:
#     Set up aliases for the POLPACK package.

#  Type of Module:
#     C shell script.

#  Invocation:
#     source polpack.csh

#  Description:
#     This procedure defines an alias for each POLPACK command. The string
#     INSTALL_BIN is replaced by the path of the directory containing the 
#     package executable files when the package is installed. The string
#     HELP_DIR is likewise replaced by the path to the directory containing 
#     help libraries.

#  Authors:
#     DSB: D.S. Berry (STARLINK)
#     {enter_new_authors_here}

#  History:
#     29-JUN-1997 (DSB):
#       Original Version (based on ircampack.csh).
#     {enter_changes_here}

#-

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
#
#
#  Set up an environment variable pointing to the help library. This is refered
#  to within the appliation interface files.
setenv POLPACK_HELP INSTALL_HELP/polpack
#
#  Define symbols for the applications and scripts.
#  ===============================================
#
alias polhelp   INSTALL_BIN/polhelp 
alias polmap    INSTALL_BIN/polmap
alias polreg    INSTALL_BIN/polreg
#
#  Now do the same with alternative names.
#
alias pol_polhelp   INSTALL_BIN/polhelp 
alias pol_polmap    INSTALL_BIN/pol_polmap
alias pol_polreg    INSTALL_BIN/pol_polreg
#
#
# Tell the user that POLPACK commands are now available.
#
echo ""
echo "   POLPACK commands are now available -- (Version PKG_VERS)"
echo " "
echo "   Type polhelp for help on POLPACK commands"
echo "   Type 'showme sun???' to browse the hypertext documentation"
echo " "
#
# end
