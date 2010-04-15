#+
#  Name:
#     ircampack.csh

#  Purpose:
#     Set up aliases for the IRCAMPACK package.

#  Type of Module:
#     C shell script.

#  Invocation:
#     source ircampack.csh

#  Description:
#     This procedure defines an alias for each IRCAMPACK command. The string
#     INSTALL_BIN is replaced by the path of the directory containing the
#     package executable files when the package is installed. The string
#     HELP_DIR is likewise replaced by the path to the directory containing
#     help libraries.

#  Authors:
#     DSB: D.S. Berry (STARLINK)
#     BLY: M.J. Bly (Starlink, RAL)
#     {enter_new_authors_here}

#  History:
#     1-DEC-1993 (DSB):
#       Original Version.
#     31-May-1995 (BLY):
#       Modifications to help file name, INSTALL location, HELP location
#       and version number.  Name changed to ircampack.csh
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
setenv IRCAMPACK_HELP INSTALL_HELP/ircampack
#
#  Define symbols for the applications and scripts.
#  ===============================================
#
alias irchelp   INSTALL_BIN/irchelp library=${IRCAMPACK_HELP}
alias tnorm     INSTALL_BIN/tnorm
alias ircamset  INSTALL_BIN/ircamset
alias check_ndfname INSTALL_BIN/check_ndfname
alias vecplot   INSTALL_BIN/vecplot
alias calpol    INSTALL_BIN/calpol
alias errclip   INSTALL_BIN/errclip
alias segment   INSTALL_BIN/segment
#
alias polzap    INSTALL_BIN/polzap
alias polsky    INSTALL_BIN/polsky
alias polsmooth INSTALL_BIN/polsmooth
alias polcal    INSTALL_BIN/polcal
alias polmapc   INSTALL_BIN/polmapc
alias polmapd   INSTALL_BIN/polmapd
alias ircamndf  source INSTALL_BIN/ircamndf
alias ircamstr  source INSTALL_BIN/ircamstr
alias awkseg    INSTALL_BIN/awkseg
alias awkvec    INSTALL_BIN/awkvec
#
#  Now do the same with alternative names.
#
alias irc_irchelp   INSTALL_BIN/irchelp library=${IRCAMPACK_HELP}
alias irc_tnorm     INSTALL_BIN/tnorm
alias irc_check_ndfname INSTALL_BIN/check_ndfname
alias irc_ircamset  INSTALL_BIN/ircamset
alias irc_vecplot   INSTALL_BIN/vecplot
alias irc_calpol    INSTALL_BIN/calpol
alias irc_errclip   INSTALL_BIN/errclip
alias irc_segment   INSTALL_BIN/segment
#
alias irc_polzap    INSTALL_BIN/polzap
alias irc_polsky    INSTALL_BIN/polsky
alias irc_polsmooth INSTALL_BIN/polsmooth
alias irc_polcal    INSTALL_BIN/polcal
alias irc_polmapc   INSTALL_BIN/polmapc
alias irc_polmapd   INSTALL_BIN/polmapd
alias irc_ircamndf  source INSTALL_BIN/ircamndf
alias irc_ircamstr  source INSTALL_BIN/ircamstr
#
# Tell the user that IRCAMPACK commands are now available.
#
echo ""
echo "   IRCAMPACK commands are now available -- (Version PKG_VERS)"
echo " "
echo "   Type irchelp for help on IRCAMPACK commands"
echo " "
#
# end
