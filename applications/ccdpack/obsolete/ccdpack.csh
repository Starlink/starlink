#+
#  Name:
#     ccdpack.csh
#
#  Purpose:
#     Start the CCDPACK system from Unix shell.
#
#  Type of Module:
#     C shell script.
#
#  Invocation:
#     source ccdpack.csh
#
#  Description:
#     This procedure starts the CCDPACK system for use from Unix by
#     defining the aliases needed to execute each application.
#
#  Notes:
#     The installation target is not known, this relies on an
#     environment variable $CCDPACK_DIR which should be set before
#     sourcing this script.
#
#  Authors:
#     PDRAPER: P.W. Draper (STARLINK)
#     {enter_new_authors_here}
#
#  History:
#     3-APR-1992 (PDRAPER):
#        Original Version
#     5-JAN-1993 (PDRAPER):
#        Added help system stuff.
#     1-DEC-1993 (PDRAPER):
#        Added automated tasks.
#     19-JAN-1994 (PDRAPER):
#        Added import, renamed other automated tasks
#     19-JUL-1995 (PDRAPER):
#        Remove INSTALL as installation target and replaced with
#       '$CCDPACK_DIR
#     13-MAR-1997 (PDRAPER):
#        Added changes to support foreign data access.
#     16-JUN-1997 (PDRAPER):
#        Replace reduce and ccdalign scripts by super-script equivalents.
#     {enter_changes_here}
#
#-
#
#   Define aliases for each application.
#
      alias astimp '$CCDPACK_DIR/astimp'
      alias ccd_astimp '$CCDPACK_DIR/astimp'
#
      alias calcor '$CCDPACK_DIR/calcor'
      alias ccd_calcor '$CCDPACK_DIR/calcor'
#
      alias ccdalign '$CCDPACK_DIR/ccdalign'
      alias ccd_ccdalign '$CCDPACK_DIR/ccdalign'
#
      alias ccdclear '$CCDPACK_DIR/ccdclear'
      alias ccd_ccdclear '$CCDPACK_DIR/ccdclear'
#
      alias ccdedit '$CCDPACK_DIR/ccdedit'
      alias ccd_ccdedit '$CCDPACK_DIR/ccdedit'
#
      alias ccdexercise '$CCDPACK_DIR/ccdexercise'
      alias ccd_ccdexercise '$CCDPACK_DIR/ccdexercise'
#
      alias ccdfork '$CCDPACK_DIR/ccdfork'
      alias ccd_ccdfork '$CCDPACK_DIR/ccdfork'
#
      alias ccdgenerate '$CCDPACK_DIR/ccdgenerate'
      alias ccd_ccdgenerate '$CCDPACK_DIR/ccdgenerate'
#
      alias ccdhelp '$CCDPACK_DIR/ccdhelp'
      alias ccd_ccdhelp '$CCDPACK_DIR/ccdhelp'
#
      alias ccdndfac '$CCDPACK_DIR/ccdndfac'
      alias ccd_ccdndfac '$CCDPACK_DIR/ccdndfac'
#
      alias ccdnote '$CCDPACK_DIR/ccdnote'
      alias ccd_ccdnote '$CCDPACK_DIR/ccdnote'
#
      alias ccdsetup '$CCDPACK_DIR/ccdsetup'
      alias ccd_ccdsetup '$CCDPACK_DIR/ccdsetup'
#
      alias ccdshow '$CCDPACK_DIR/ccdshow'
      alias ccd_ccdshow '$CCDPACK_DIR/ccdshow'
#
      alias debias '$CCDPACK_DIR/debias'
      alias ccd_debias '$CCDPACK_DIR/debias'
#
      alias findcent '$CCDPACK_DIR/findcent'
      alias ccd_findcent '$CCDPACK_DIR/findcent'
#
      alias findobj '$CCDPACK_DIR/findobj'
      alias ccd_findobj '$CCDPACK_DIR/findobj'
#
      alias findoff '$CCDPACK_DIR/findoff'
      alias ccd_findoff '$CCDPACK_DIR/findoff'
#
      alias flatcor '$CCDPACK_DIR/flatcor'
      alias ccd_flatcor '$CCDPACK_DIR/flatcor'
#
      alias idicurs '$CCDPACK_DIR/idicurs'
      alias ccd_idicurs '$CCDPACK_DIR/idicurs'
#
      alias import '$CCDPACK_DIR/import'
      alias ccd_import '$CCDPACK_DIR/import'
#
      alias makebias '$CCDPACK_DIR/makebias'
      alias ccd_makebias '$CCDPACK_DIR/makebias'
#
      alias makecal '$CCDPACK_DIR/makecal'
      alias ccd_makecal '$CCDPACK_DIR/makecal'
#
      alias makeflat '$CCDPACK_DIR/makeflat'
      alias ccd_makeflat '$CCDPACK_DIR/makeflat'
#
      alias makemos '$CCDPACK_DIR/makemos'
      alias ccd_makemos '$CCDPACK_DIR/makemos'
#
      alias pairndf '$CCDPACK_DIR/pairndf'
      alias ccd_pairndf '$CCDPACK_DIR/pairndf'
#
      alias plotlist '$CCDPACK_DIR/plotlist'
      alias ccd_plotlist '$CCDPACK_DIR/plotlist'
#
      alias present '$CCDPACK_DIR/present'
      alias ccd_present '$CCDPACK_DIR/present'
#
      alias register '$CCDPACK_DIR/register'
      alias ccd_register '$CCDPACK_DIR/register'
#
      alias schedule '$CCDPACK_DIR/schedule'
      alias ccd_schedule '$CCDPACK_DIR/schedule'
#
      alias reduce '$CCDPACK_DIR/reduce'
      alias ccd_reduce '$CCDPACK_DIR/reduce'
#
      alias tranlist '$CCDPACK_DIR/tranlist'
      alias ccd_tranlist '$CCDPACK_DIR/tranlist'
#
      alias tranndf '$CCDPACK_DIR/tranndf'
      alias ccd_tranndf '$CCDPACK_DIR/tranndf'
#
      alias ccdwww '$CCDPACK_DIR/ccdwww'
      alias ccd_ccdwww '$CCDPACK_DIR/ccdwww'
#
      alias xreduce '$CCDPACK_DIR/xreduce'
      alias ccd_xreduce '$CCDPACK_DIR/xreduce'

#   NAG version of routines.
      alias nagmakemos '$CCDPACK_DIR/nagmakemos'
      alias ccd_nagmakemos '$CCDPACK_DIR/ccd_nagmakemos'

#   Setup conversion of header information for when using foreign data
#   access.
      if ( $?NDF_XTN ) then
         switch ($NDF_XTN)
            case *CCDPACK*:
               breaksw
            default:
               setenv NDF_XTN ${NDF_XTN},CCDPACK
         endsw
      else
         setenv NDF_XTN CCDPACK
      endif
      setenv NDF_IMP_CCDPACK '$CCDPACK_DIR/ccdimp.csh ^ndf'
      setenv NDF_EXP_CCDPACK '$CCDPACK_DIR/ccdexp.csh ^ndf'

#
#   Show that the CCDPACK commands are now available.
#
      echo ""
      echo "   CCDPACK commands are now available -- (Version PKG_VERS)"
      echo " "
      echo "   For help use the commands ccdhelp or ccdwww"
      echo " "

#   For IRAF data we really do need to keep hold of the bad pixels
#   so make sure of this, unless the NDF_TO_IRAF_PARS variable
#   is already set. In this case we assume that the user knows what
#   they are doing.
      if ( ! $?NDF_TO_IRAF_PARS ) then
         setenv NDF_TO_IRAF_PARS 'FILLBAD=\!'
      endif
#
# end
# $Id$
