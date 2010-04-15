#!/bin/csh -f
#+
#  Name:
#     esp.csh
#
#  Purpose:
#     Start the ESP system from Unix shell.
#
#  Type of Module:
#     C shell script.
#
#  Invocation:
#     esp
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
#     GJP: G.J.Privett (STARLINK)
#     BLY: M.J.Bly (Starlink, RAL)
#     NG:  Norman Gray (Starlink, Glasgow)
#     {enter_new_authors_here}
#
#  History:
#     29-SEP-1993 (GJP):
#       Original Version.
#     22-AUG-1994 (GJP):
#       Modified to add monolith.
#     6-OCT-1994 (BLY):
#       Remodeled to Starlink requirements.
#     21-NOV-1994 (BLY):
#       Added help system to startup.
#     24-FEB-1997 (GJP):
#       Added GAUFIT to startup.
#     17-FEB-1998 (NG):
#       Use ADAM_USER env.var. (as documented)
#     {enter_changes_here}
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
if ($?ADAM_USER) then
   set adamdir=$ADAM_USER
else
   set adamdir=$HOME/adam
endif

if (-d $adamdir) then
   echo -n
else
   if (-f $adamdir) then
      echo "You have a file called $adamdir"
      echo "Please rename this, or define the environment variable ADAM_USER"
      echo "to point to a directory for ADAM files."
      exit
   else
      mkdir $adamdir
   endif
endif
unset adamdir

#  Define aliases for the applications.
#  There should be a plain alias, and a package specific alias
#  so that applications that have conflicting command names are
#  still available.

alias corr ${ESP_DIR}/corr
alias esp_corr ${ESP_DIR}/esp_corr

alias ellfou ${ESP_DIR}/ellfou
alias esp_ellfou ${ESP_DIR}/esp_ellfou

alias ellpro ${ESP_DIR}/ellpro
alias esp_ellpro ${ESP_DIR}/esp_ellpro

alias esphelp ${ESP_DIR}/esphelp
alias esp_esphelp ${ESP_DIR}/esp_esphelp

alias fastmed ${ESP_DIR}/fastmed
alias esp_fastmed ${ESP_DIR}/esp_fastmed

alias gaufit ${ESP_DIR}/gaufit
alias esp_gaufit ${ESP_DIR}/esp_gaufit

alias graphs ${ESP_DIR}/graphs
alias esp_graphs ${ESP_DIR}/esp_graphs

alias histpeak ${ESP_DIR}/histpeak
alias esp_histpeak ${ESP_DIR}/esp_histpeak

alias hsub ${ESP_DIR}/hsub
alias esp_hsub ${ESP_DIR}/esp_hsub

alias loback ${ESP_DIR}/loback
alias esp_loback ${ESP_DIR}/esp_loback

alias mask ${ESP_DIR}/mask
alias esp_mask ${ESP_DIR}/esp_mask

alias mixup ${ESP_DIR}/mixup
alias esp_mixup ${ESP_DIR}/esp_mixup

alias sector ${ESP_DIR}/sector
alias esp_sector ${ESP_DIR}/esp_sector

alias selfc ${ESP_DIR}/selfc
alias esp_selfc ${ESP_DIR}/esp_selfc

alias selfcw ${ESP_DIR}/selfcw
alias esp_selfcw ${ESP_DIR}/esp_selfcw

alias skew ${ESP_DIR}/skew
alias esp_skew ${ESP_DIR}/esp_skew

alias topped ${ESP_DIR}/topped
alias esp_topped ${ESP_DIR}/esp_topped

#
#  Announce the availability of the ESP commands.
#

echo ""
echo "   ESP commands are now available -- (Version 0.11-4)"
echo ""
echo "   Type  esphelp  for help on ESP commands"
echo "   Type  showme sun180  to see the hypertext document."
echo ""

#
# end
#
