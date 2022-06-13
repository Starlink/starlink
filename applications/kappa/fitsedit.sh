#!/bin/sh
#+
#  Name:
#     FITSEDIT

#  Purpose:
#     Edits the FITS extension of an NDF.

#  Language:
#     Bourne shell

#  Type of Module:
#     Bourne shell script

#  Description:
#     This procedure allows you to use your favourite editor to
#     modify the FITS headers stored in an NDF's FITS extension.
#     There is limited validation of the FITS headers after editing.
#     A FITS extension is created if the NDF does not already have
#     one.

#  Usage:
#     fitsedit ndf

#  ADAM Parameters:
#     NDF = NDF (Read)
#        The name of the NDF whose FITS extension is to be edited.

#  Examples:
#     fitsedit m51b
#        This allows editing of the FITS headers in the NDF called m51b.

#  Notes:
#     -  This uses the environmental variable, EDITOR, to select
#     the editor.  If this variable is undefined vi is assumed.
#     -  The script lists the headers to a temporary file; allows text
#     editing; and then replaces the former FITS extension with the
#     modified version, performing some validation at this stage.

#  Related Applications:
#     KAPPA: FITSMOD, FITSEXP, FITSHEAD, FITSIMP, FITSLIST; Figaro: FITSKEYS.

#  Copyright:
#     Copyright (C) 1994 Science & Engineering Research Council.
#     Copyright (C) 1996, 1998, 2000 Central Laboratory of the Research
#     Councils.
#     Copyright (C) 2008 Science and Technology facilities Council.
#     All Rights Reserved.

#  Licence:
#     This program is free software; you can redistribute it and/or
#     modify it under the terms of the GNU General Public License as
#     published by the Free Software Foundation; either Version 2 of
#     the License, or (at your option) any later version.
#
#     This program is distributed in the hope that it will be
#     useful, but WITHOUT ANY WARRANTY; without even the implied
#     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
#     PURPOSE. See the GNU General Public License for more details.
#
#     You should have received a copy of the GNU General Public License
#     along with this program; if not, write to the Free Software
#     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
#     02110-1301, USA.

#  Authors:
#     Malcolm J. Currie (STARLINK)
#     David S. Berry (DSB):
#     {enter_new_authors_here}

#  History:
#     1994 September 28 (MJC):
#        Original version.
#     1994 November 4 (MJC):
#        Added ADAM-like help and abort facility for the parameter.
#     1996 January 16 (MJC):
#        Added suggested default and search path for foreign data
#        formats.
#     5-JUN-1998 (DSB):
#        Added facility to create a new FITS extension if there is no
#        existing FITS extension in the NDF.
#     2-FEB-2000 (DSB):
#        Guard against GLOBAL.sdf not existing by checking $status after
#        running parget. Interpret a single exclamation mark as an abort
#        request. Do not include an escaped new line before awk since this
#        produces a "null command" error. Explicitly remove any shell
#        metacharacters (such as "$") from the NDF name.
#     3-FEB-2000 (DSB):
#        Corrected implementation of yesterdays changes, and modified the
#        "echo -n" commands to use the csh built-in echo command in place
#        of the sh built-in echo command which does not seem to recognize
#        the -n option on Solaris.
#     2008 April 24 (MJC):
#        Expand the kappa alias that no longer works.
#     2022 June 13 (GSB):
#        Convert to sh.
#     {enter_further_changes_here}
#
#-

#   Ensure that processing to temporary file will work by deleting them.
#
if [ -e zzfitsedit.tmp~ ]; then
   rm zzfitsedit.tmp~
fi
if [ -e zzfitsedit.tmp ]; then
   rm zzfitsedit.tmp
fi
#
#  Make sure that KAPPA is available hiding it from view.
#
. $KAPPA_DIR/kappa.sh > /dev/null
#
#  See whether or not the NDF has been defined.
#
if [ "$#" -eq 0 ]; then
#
#   Set the logical that says whether or not a valid value has been
#   supplied.
#
   ok=
   while [ -z "$ok" ]; do
#
#   Ensure that the following invocation of parget will return a non-zero
#   status value if anything goes wrong.
#
      adam_exit_set=
      if [ -n "$ADAM_EXIT" ]; then
         adam_exit_set=1
      fi
      export ADAM_EXIT=1
#
#   Obtain the current DATA_ARRAY. Check that parget worked ok by testing
#   status. Also remove angle brackets introduced by parget (eg. replace
#   "$<KAPPA_DIR>/m31" by "$KAPPA_DIR/m31" ), and any NDF section specifier.
#
      defndf=`parget data_array GLOBAL`
      if ([ $? -ne 0 ] || [ -z "$defndf" ]); then
         defndf=
         prstring="NDF - Name of the NDF > "
      else
         defndf=`echo $defndf | sed -e 's/^\$<\(.*\)>\(.*\)/\$\1\2/' | sed -e 's/^\(.*\)(.*)/\1/'`
         prstring="NDF - Name of the NDF /@"$defndf"/ > "
         ndf=$defndf
      fi
#
#   Clear ADAM_EXIT unless it was already set.
#
      if [ -z "$adam_exit_set" ]; then
         unset ADAM_EXIT
      fi
#
#   Assume that the value will be fine unless we discover otherwise
#   later.  Prompt for the value.
#
      ok=1
      echo -n "$prstring" 1>&2
      read ndf
#
#   Write some help information, but continue in the loop.
#
      if [ "$ndf" = '?' ]; then
         echo '  ' 1>&2
         echo '   NDF  = NDF (Read)' 1>&2
         echo '      The name of the NDF whose FITS extension is to be edited.' 1>&2
         echo ' ' 1>&2
         ok=
#
#   Abort when requested.
#
      elif ( [ "$ndf" = '!!' ] || [ "$ndf" = '!' ] ); then
         exit
#
#   Reprompt when no value is given.
#
      elif ([ -z "$defndf" ] && [ -z "$ndf" ]); then
         echo 'No NDF specified.  Enter "!!" to abort.' 1>&2
         ok=

      else
#
#   Accept the default.
#
         if ([ -z "$ndf" ] && [ -n "$defndf" ]); then
            ndf="$defndf"
         fi
#
#   Remove any shell meta-characters
#
         eval ndf="$ndf"
#
#   Check that the supplied NDF exists.  If there is a specific
#   extension, test that the file exists.
#
         case "$ndf" in
         *.*)
            if [ ! -e $ndf ]; then
               echo "Data file \"$ndf\" does not exist." 1>&2
               ok=
            fi
            ;;
         *)
#
#   The filename does not have a file extension.  Thus it is either an
#   NDF or a foreign format defined by the NDF_FORMATS_IN environment
#   variable.  First test for an NDF.
#
            if [ ! -e "${ndf}.sdf" ]; then
#
#   The file might be in a foreign format.  Obtain the number and a list
#   of the valid file extensions in search-order from NDF_FORMATS_IN.
#   The first value gives the number of formats, so if this is 0,
#   NDF_FORMATS_IN is undefined.
#
               formats=`echo "$NDF_FORMATS_IN" | awk -f $KAPPA_DIR/nfi.awk`
               case "$formats" in
               0)
                  echo "NDF \"$ndf\" does not exist." 1>&2
                  ok=
                  ;;
               *)
#
#   Test for the existence of each format in the list, until a match
#   is found.  Set the flag to indicate failure to find a file, until one
#   is found.
#
                  ok=
                  set $formats
                  shift
                  for format in "$@"; do
                     if [ -e "${ndf}${format}" ]; then
                        ok=1
                        break
                     fi
                  done
#
#   Report the case where no foreign file could be found.
#
                  if [ -z "$ok" ]; then
                     echo "Data file \"$ndf\" does not exist." 1>&2
                  fi
               esac
            fi
         esac
      fi
   done
else
   ndf="$1"
fi
#
#   Determine which editor is to be used.
#
fitseditor=${EDITOR:-vi}
#
#   See if the NDF has a FITS extension.
#
ndftrace $ndf quiet
gotext=
if [ `parget nextn ndftrace` -gt 0 ]; then
   for ext in `parget extname ndftrace`; do
      if [ "$ext" = "FITS" ]; then
         gotext=1
      fi
   done
fi
#
#   If the NDF has a FITS extension, list it into a temporary file.
#   Otherwise warn the user (pausing to give some time to read the
#   message before the screen is cleared by the editor) and create a
#   temporary file containing a vestigial header.
#
if [ -n "$gotext" ]; then
   fitslist $ndf logfile=zzfitsedit.tmp
else
   echo "fitsedit: '$ndf' has no FITS extension. A new FITS extension will be created."

   sleep 5
   echo "SIMPLE  =                    T / File conforms to FITS standard" > zzfitsedit.tmp
fi
#
#   Call the selected editor to modify the listing.
#
if [ -e zzfitsedit.tmp ]; then
   $fitseditor zzfitsedit.tmp
fi
#
#   Place the edited version back into the NDF.
#
fitstext $ndf zzfitsedit.tmp
#
#   Tidy up.
#
if [ -e zzfitsedit.tmp~ ]; then
   rm zzfitsedit.tmp~
fi
if [ -e zzfitsedit.tmp ]; then
   rm zzfitsedit.tmp
fi
#
exit
