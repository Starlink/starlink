#+
#  Name:
#     ndprogs.csh

#  Purpose:
#     Set up aliases for the NDPROGS package.

#  Type of Module:
#     C shell script.

#  Invocation:
#     source  ndprogs.csh

#  Description:
#     This procedure defines an alias for each NDPROGS command. If the
#     environment variable NDPROGS_DIR does not exist, it is defined
#     with the value /star/bin/ndprogs.

#  Authors:
#     GJP: G.J. Privett (Starlink, Cardiff)
#     BLY: M.J. Bly (Starlink, RAL)
#     {enter_new_authors_here}

#  History:
#     15-NOV-1994 (BLY/GJP)
#       Original Version:  uses IRAS90 version as guide.

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
#  If there is currently no definition of the environment variable
#  NDPROGS_DIR, set one up pointing to /star/bin/ndprogs.
if !($?NDPROGS_DIR) then
   setenv NDPROGS_DIR INSTALL_BIN
endif
#
#  If there is currently no definition of the environment variable
#  NDPROGS_HELP, set one up pointing to /star/help/ndprogs.
if !($?NDPROGS_HELP) then
   setenv NDPROGS_HELP INSTALL_HELP/ndprogshelp
endif
#
#
#  Define symbols for the applications.
#  ====================================
#
alias addnd         ${NDPROGS_DIR}/addnd
alias ndp_addnd     ${NDPROGS_DIR}/addnd

alias arith1        ${NDPROGS_DIR}/arith1
alias ndp_arith1    ${NDPROGS_DIR}/arith1

alias arith2        ${NDPROGS_DIR}/arith2
alias ndp_arith2    ${NDPROGS_DIR}/arith2

alias axflip        ${NDPROGS_DIR}/axflip
alias ndp_axflip    ${NDPROGS_DIR}/axflip

alias collapse      ${NDPROGS_DIR}/collapse
alias ndp_collapse  ${NDPROGS_DIR}/collapse

alias degamma       ${NDPROGS_DIR}/degamma
alias ndp_degamm    ${NDPROGS_DIR}/degamma

alias depict        ${NDPROGS_DIR}/depict
alias ndp_depict    ${NDPROGS_DIR}/depict

alias dummy         ${NDPROGS_DIR}/dummy
alias ndp_dummy     ${NDPROGS_DIR}/dummy

alias hilite        ${NDPROGS_DIR}/hilite
alias ndp_hilite    ${NDPROGS_DIR}/hilite

alias logic1        ${NDPROGS_DIR}/logic1
alias ndp_logic1    ${NDPROGS_DIR}/logic1

alias logic2        ${NDPROGS_DIR}/logic2
alias ndp_logic2    ${NDPROGS_DIR}/logic2

alias looknd        ${NDPROGS_DIR}/look
alias ndp_looknd    ${NDPROGS_DIR}/look

alias magic         ${NDPROGS_DIR}/magic
alias ndp_magic     ${NDPROGS_DIR}/magic

alias mask1         ${NDPROGS_DIR}/mask1
alias ndp_mask1     ${NDPROGS_DIR}/mask1

alias mask2         ${NDPROGS_DIR}/mask2
alias ndp_mask2     ${NDPROGS_DIR}/mask2

alias moments       ${NDPROGS_DIR}/moments
alias ndp_moments   ${NDPROGS_DIR}/moments

alias movie         ${NDPROGS_DIR}/movie
alias ndp_movie     ${NDPROGS_DIR}/movie

alias ndprogshelp   ${NDPROGS_DIR}/ndprogshelp
alias ndp_help      ${NDPROGS_DIR}/ndprogshelp

alias peek          ${NDPROGS_DIR}/peek
alias ndp_peek      ${NDPROGS_DIR}/peek

alias plots         ${NDPROGS_DIR}/plots
alias ndp_plots     ${NDPROGS_DIR}/plots

alias setaxes       ${NDPROGS_DIR}/setaxes
alias ndp_setaxes   ${NDPROGS_DIR}/setaxes

alias slice3d       ${NDPROGS_DIR}/slice3d
alias ndp_slice3d   ${NDPROGS_DIR}/slice3d

alias spectrum      ${NDPROGS_DIR}/spectrum
alias ndp_spectrum  ${NDPROGS_DIR}/spectrum

alias squint        ${NDPROGS_DIR}/squint
alias ndp_squint    ${NDPROGS_DIR}/squint

alias smooth        ${NDPROGS_DIR}/smooth
alias ndp_smooth    ${NDPROGS_DIR}/smooth

alias stack         ${NDPROGS_DIR}/stack
alias ndp_stack     ${NDPROGS_DIR}/stack

alias stats         ${NDPROGS_DIR}/stats
alias ndp_stats     ${NDPROGS_DIR}/stats

alias stretch       ${NDPROGS_DIR}/stretch
alias ndp_stretch   ${NDPROGS_DIR}/stretch

alias subset        ${NDPROGS_DIR}/subset
alias ndp_subset    ${NDPROGS_DIR}/subset

alias tau2fig       echo "tau2fig is not available on Unix"
alias ndp_tau2fig   echo "tau2fig is not available on Unix"

alias testnd        ${NDPROGS_DIR}/test
alias ndp_testnd    ${NDPROGS_DIR}/test

alias transform     ${NDPROGS_DIR}/transform
alias ndp_transform ${NDPROGS_DIR}/transform

alias transpose     ${NDPROGS_DIR}/transpose
alias ndp_transpose ${NDPROGS_DIR}/transpose

alias typecon       ${NDPROGS_DIR}/typecon
alias ndp_typecon   ${NDPROGS_DIR}/typecon

alias unmagic       ${NDPROGS_DIR}/unmagic
alias ndp_unmagic   ${NDPROGS_DIR}/unmagic

#
# Tell the user that NDPROGS commands are now available.
#
echo ""
echo "   NDPROGS commands are now available -- (Version PKG_VERS)"
echo " "
echo "   Type ndp_help for help on NDPROGS commands"
echo " "
#
# end
