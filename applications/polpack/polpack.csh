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
#     /star-test/bin/polpack is replaced by the path of the directory containing the 
#     package executable files when the package is installed. The string
#     HELP_DIR is likewise replaced by the path to the directory containing 
#     help libraries.

#  Authors:
#     DSB: D.S. Berry (STARLINK)
#     {enter_new_authors_here}

#  History:
#     29-JUN-1997 (DSB):
#       Original Version.
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
      setenv POLPACK_HELP /star-test/help/polpack/polpack
#
#  Define symbols for the applications and scripts.
#  ===============================================
#
      alias calpol    /star-test/bin/polpack/calpol
      alias polhelp   /star-test/bin/polpack/polhelp 
      alias polexp    /star-test/bin/polpack/polexp
      alias polimp    /star-test/bin/polpack/polimp
      alias polmap    /star-test/bin/polpack/polmap
      alias polka     /star-test/bin/polpack/polka
#
#  Now do the same with alternative names.
#
      alias pol_calpol    /star-test/bin/polpack/pol_calpol
      alias pol_polhelp   /star-test/bin/polpack/polhelp 
      alias pol_polexp    /star-test/bin/polpack/pol_polexp
      alias pol_polimp    /star-test/bin/polpack/pol_polimp
      alias pol_polmap    /star-test/bin/polpack/pol_polmap
      alias pol_polka     /star-test/bin/polpack/pol_polka
#
#
#  Set up the commands and environment variables needed to export and
#  import POLPACK extension information to and from foreign data formats.
#
      if ( $?NDF_XTN ) then
         switch ($NDF_XTN)
            case *POLPACK*:
               breaksw
            default:
               setenv NDF_XTN ${NDF_XTN},POLPACK
         endsw
      else
         setenv NDF_XTN POLPACK
      endif

      setenv NDF_IMP_POLPACK '/star-test/bin/polpack/polimp.csh ^ndf'
      setenv NDF_EXP_POLPACK '/star-test/bin/polpack/polexp.csh ^ndf'
      setenv NDF_IMP_POLPACK_COMPRESSED ' '
      setenv NDF_EXP_POLPACK_COMPRESSED ' '
      setenv NDF_IMP_POLPACK_GZIP ' '
      setenv NDF_EXP_POLPACK_GZIP ' '
#
#
# Tell the user that POLPACK commands are now available.
#
      echo ""
      echo "   POLPACK commands are now available -- (Version 1.1)"
      echo " "
      echo "   Type polhelp for help on POLPACK commands"
      echo "   Type 'showme sun???' to browse the hypertext documentation"
      echo " "
#
# end
