#!/bin/csh
#+          
#  Name:
#     hdstools.csh

#  Purpose:
#     Set up aliases for the HDSTOOLS package.

#  Type of Module:
#     C shell script.

#  Invocation:
#     source hdstools.csh

#  Description:
#     This procedure defines an alias for each HDSTOOLS command. The 
#     string install_bin (upper-case) is replaced by the path of the 
#     directory containing the package executable files when the package
#     is installed.  The string help_dir (upper-case) is likewise replaced
#     by the path to the directory containing the help files.

#  Authors:
#     BLY: M.J. Bly (Starlink, RAL)
#     AJC: A.J. Chipperfield (Starlink, RAL)
#     {enter_new_authors_here}

#  History:
#     23-JUN-1995 (BLY):
#       Original Version.
#     12-DEC-1996 (BLY):
#       Cosmetic mods.
#     18-SEP-2001 (AJC):
#       Modify for HDSTOOLS
#     {enter_changes_here}

#-

#  Prepare to run ADAM applications if this has not been done already.
#  ===================================================================
#
#  Here look to see if there is an ADAM_USER directory.  If there is not
#  check whether or not there is an adam file that is not a directory.
#  If there is, issue a warning and exit.  Otherwise create the required
#  directory.

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
#  Set up an environment variable pointing to the help library. 
#  This is refered to within the appliation interface files.

setenv HDSTOOLS_HELP INSTALL_HELP/hdstools

#
#  Locate the installed binaries, scripts etc.

setenv HDSTOOLS_DIR INSTALL_BIN

#
#  Define symbols for the applications and scripts.
#  ===============================================

alias hcopy $HDSTOOLS_DIR/hcopy
alias hcreate $HDSTOOLS_DIR/hcreate
alias hdelete $HDSTOOLS_DIR/hdelete
alias hdir $HDSTOOLS_DIR/hdir
alias hdisplay $HDSTOOLS_DIR/hdisplay
alias hfill $HDSTOOLS_DIR/hfill
alias hget $HDSTOOLS_DIR/hget
alias hhelp $HDSTOOLS_DIR/hhelp
alias hmodify $HDSTOOLS_DIR/hmodify
alias hread $HDSTOOLS_DIR/hread
alias hrename $HDSTOOLS_DIR/hrename
alias hreset $HDSTOOLS_DIR/hreset
alias hreshape $HDSTOOLS_DIR/hreshape
alias hretype $HDSTOOLS_DIR/hretype
alias htab $HDSTOOLS_DIR/htab
alias hwrite $HDSTOOLS_DIR/hwrite

#
#  Now do the same with alternative names.
#  ======================================

alias hdt_copy $HDSTOOLS_DIR/hcopy
alias hdt_create $HDSTOOLS_DIR/hcreate
alias hdt_delete $HDSTOOLS_DIR/hdelete
alias hdt_dir $HDSTOOLS_DIR/hdir
alias hdt_display $HDSTOOLS_DIR/hdisplay
alias hdt_fill $HDSTOOLS_DIR/hfill
alias hdt_get $HDSTOOLS_DIR/hget
alias hdt_help $HDSTOOLS_DIR/hhelp
alias hdt_modify $HDSTOOLS_DIR/hmodify
alias hdt_read $HDSTOOLS_DIR/hread
alias hdt_rename $HDSTOOLS_DIR/hrename
alias hdt_reset $HDSTOOLS_DIR/hreset
alias hdt_reshape $HDSTOOLS_DIR/hreshape
alias hdt_retype $HDSTOOLS_DIR/hretype
alias hdt_tab $HDSTOOLS_DIR/htab
alias hdt_write $HDSTOOLS_DIR/hwrite

#
#  Tell the user that HDSTOOLS commands are now available.
#  =======================================================

echo ""
echo "   HDSTOOLS commands are now available -- (Version PKG_VERS)"
echo " "
echo "   Type hhelp for help on HDSTOOLS commands"
echo " "

#
# end
