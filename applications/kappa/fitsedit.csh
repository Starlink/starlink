#!/bin/csh
#+
#  Name:
#     FITSEDIT
#
#  Purpose:
#     Edits the FITS extension of an NDF.
#
#  Type of Module:
#     C-shell script
#
#  Usage:
#     fitsedit ndf
#
#  Description:
#     This procedure allows you to use your favourite editor to
#     modify the FITS headers stored in an NDF's FITS extension.
#     There is limited validation of the FITS headers after editing.
#     A FITS extension is created if the NDF does not already have 
#     one.
#
#  ADAM Parameters:
#     NDF = NDF (Read)
#        The name of the NDF whose FITS extension is to be edited.
#
#  Notes:
#     -  This uses the environmental variable, EDITOR, to select
#     the editor.  If this variable is undefined vi is assumed.
#     -  The script lists the headers to a temporary file; allows text 
#     editing; and then replaces the former FITS extension with the
#     modified version, performing some validation at this stage.
#
#  Examples:
#     fitsedit m51b
#        This allows editing of the FITS headers in the NDF called m51b.
#
#  Related Applications:
#     KAPPA: FITSEXP, FITSHEAD, FITSIMP, FITSLIST; Figaro: FITSKEYS.
#
#  Authors:
#     Malcolm J. Currie (STARLINK)
#     David S. Berry (DSB):
#     {enter_new_authors_here}
#
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
#     {enter_further_changes_here}
#
#  Bugs:
#     {note_any_bugs_here}
#
#-
#
#   Ensure that processing to temporary file will work by deleting them.
#
if (-e zzfitsedit.tmp~) rm zzfitsedit.tmp~
if (-e zzfitsedit.tmp) rm zzfitsedit.tmp
#
#  Make sure that KAPPA is available hiding it from view.
#
alias echo 'echo >/dev/null'
kappa
unalias echo
#
#  See whether or not the NDF has been defined.
#
if ( $#argv == 0 ) then
#
#   Set the logical that says whether or not a valid value has been
#   supplied.
#
   set ok = 0
   while ( $ok == 0 )
#
#   Obtain the current DATA_ARRAY.
#
      set defndf = `parget data_array GLOBAL`
      if ( $defndf == "" ) then
         set prstring = "NDF - Name of the NDF > "
      else
         set prstring = "NDF - Name of the NDF /@"$defndf"/ > "
         set ndf = $defndf
      endif
#
#   Assume that the value will be fine unless we discover otherwise
#   later.  Prompt for the value.  We must prevent the ? from being
#   treated as a single-character pattern match.  Various combinations
#   of quotes and backslashes do not seem to work.  Hence we use noglob.
#
      set ok = 1
      set noglob
      sh -c "echo -n '$prstring' 1>&2"
      set ndf = $<
#
#   Write some help information, but continue in the loop.
#
      if ( $ndf == '?' ) then
         sh -c "echo '  ' 1>&2"
         sh -c "echo '   NDF  = NDF (Read)' 1>&2"
         sh -c "echo '      The name of the NDF whose FITS extension is to be edited.' 1>&2"
         sh -c "echo ' ' 1>&2"
         set ok = 0
#
#   Abort when requested.
#
      else if ( "$ndf" == \!\! ) then
         exit
#
#   Reprompt when no value is given.
#
      else if ( $defndf == "" && $ndf == "" ) then
         sh -c "echo 'No NDF specified.  Enter "\!\!" to abort.' 1>&2"
         set ok = 0

      else
#
#   Accept the default.
#
         if ( $ndf == "" && $defndf != "" ) then
            set ndf = $defndf
         endif
         unset noglob
#
#   Check that the supplied NDF exists.  If there is a specific
#   extension, test that the file exists.
#
         if ( $ndf:e != "" ) then
            if (! -e $ndf ) then
               sh -c "echo 'Data file "$ndf" does not exist.' 1>&2"
               set ok = 0
            endif
         else
#
#   The filename does not have a file extension.  Thus it is either an
#   NDF or a foreign format defined by the NDF_FORMATS_IN environment
#   variable.  First test for an NDF.
#
            set file = $ndf".sdf"
            if ( ! -e $file ) then
#
#   The file might be in a foreign format.  Obtain the number and a list
#   of the valid file extensions in search-order from NDF_FORMATS_IN.
#   The first value gives the number of formats, so if this is 0,
#   NDF_FORMATS_IN is undefined.
#
               set formats = `printenv | grep NDF_FORMATS_IN | \
                             awk -f $KAPPA_DIR/nfi.awk`
               if ( $formats[1] == 0 ) then
                  sh -c "echo 'NDF "$ndf" does not exist.' 1>&2"
                  set ok = 0
               else
                  set noformats = $formats[1]
                  shift formats
#
#   Test for the existence of each format in the list, until a match
#   is found.  Set the flag to indicate failure to find a file, until one
#   is found.
#
                  set ok = 0
                  set iform = 1
                  while ( $iform <= $noformats )
                     set file = $ndf$formats[$iform]
                     if ( ! -e $file ) then
                        @ iform = $iform + 1

                     else
                        set ok = 1
                        @ iform = $noformats + 1
                     endif
                  end
#
#   Report the case where no foreign file could be found.
#
                  if ( $ok == 0 ) then
                     sh -c "echo 'Data file "$ndf" does not exist.' 1>&2"
                  endif
               endif
            endif
         endif
      endif
      unset noglob
   end
else
   set ndf = $1
endif
#
#   Determine which editor is to be used.
#
set fitseditor =
if ($?EDITOR ) then
   set fitseditor = $EDITOR
else
   set fitseditor = vi
endif
#
#   See if the NDF has a FITS extension.
#
ndftrace $ndf quiet
set gotext = 0
if ( `parget nextn ndftrace` > 0 ) then
   foreach ext (`parget extname ndftrace`)
      if ( $ext == "FITS" ) set gotext = 1
   end
endif
#
#   If the NDF has a FITS extension, list it into a temporary file.
#   Otherwise warn the user (pausing to give some time to read the
#   message before the screen is cleared by the editor) and create a 
#   temporary file containing a vestigial header.
#
if ( $gotext == 1 ) then
   fitslist $ndf logfile=zzfitsedit.tmp
else
   echo "fitsedit: \'$ndf\' has no FITS extension. A new FITS extension"
   echo "will be created."   
   sleep 5
   echo "COMMENT   This FITS header was created by KAPPA:FITSEDIT" > zzfitsedit.tmp
   echo "COMMENT   (delete these comments if required.)" >> zzfitsedit.tmp
   echo "COMMENT " >> zzfitsedit.tmp
   echo "SIMPLE  =                    T / File conforms to FITS standard" >> zzfitsedit.tmp
endif
#
#   Call the selected editor to modify the listing.
#
if ( -e zzfitsedit.tmp ) then
   $fitseditor zzfitsedit.tmp
endif
#
#   Place the edited version back into the NDF.
#
fitstext $ndf zzfitsedit.tmp
#
#   Tidy up.
#
if (-e zzfitsedit.tmp~) rm zzfitsedit.tmp~
if (-e zzfitsedit.tmp) rm zzfitsedit.tmp
#
exit
