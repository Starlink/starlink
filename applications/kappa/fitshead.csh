#!/bin/csh
#+
#  Name:
#     FITSHEAD

#  Purpose:
#     Lists the headers of FITS files.

#  Language:
#     C-shell

#  Type of Module:
#     C-shell script

#  Description:
#     This procedure lists to standard output the headers of the primary
#     header and data unit, and any extensions present that are
#     contained within a set of input FITS files, or a range of
#     specified files on a tape.

#  Usage:
#     fitshead file [block] [start] [finish]

#  ADAM Parameters:
#     FILE  = FILENAME (Read)
#        A space-separated list of FITS files whose headers are to be
#        listed, or the name of a single no-rewind tape device.  The list
#        of files can include wildcard characters.
#     BLOCK = _INTEGER (Read)
#        The FITS blocking factor of the tape to list.  This is the tape
#        blocksize in bytes divided by the FITS record length of 2880
#        bytes.  BLOCK must be a positive integer, between 1 and 12,
#        otherwise you will be prompted for a new value.  Should the first
#        argument not be a tape device, this argument will be treated as
#        a file name. [1]
#     START = _INTEGER (Read)
#        The first file on the tape to list.  This defaults to 1, i.e.
#        the start of the tape.  It must be a positive integer,
#        otherwise you will be prompted for a new value.  Should the
#        first argument not be a tape device, this argument will be
#        treated as a file name. [1]
#     FINISH = _INTEGER (Read)
#        The last file on the tape to list.  This defaults to the end
#        of the tape.  It must be a positive integer and at least equal
#        to the value of start, otherwise you will be prompted for a new
#        value.  Should the first argument not be a tape device, this
#        argument will be treated as a file name. []

#  Examples:
#     fitshead /dev/nrmt1
#        This lists the FITS headers for all the files of the tape mounted
#        on device /dev/nrmt1.  The tape block size is 2880 bytes.
#     fitshead /dev/nrmt1 10 > tape.lis
#        This lists to file tape.lis the FITS headers for all the files of
#        the tape mounted on device /dev/nrmt1.  The tape blocking factor is
#        10, the tape's blocksize is 28800 bytes.
#     fitshead /dev/rmt/0n 2 3 5 >> tape.lis
#        This appends the FITS headers for files 3 to 5 of the tape mounted
#        on device /dev/rmt/0n to the file tape.lis.  The tape blocking factor
#        is 2, i.e. the tape's blocksize is 5760 bytes.
#     fitshead /dev/nrst2 prompt
#        This lists the FITS headers for files of the tape mounted on
#        device /dev/nrst2.  The command prompts you for the file limits
#        and tape blocking factor.
#     fitshead ~/fits/*.fit ~/data/p?.fi* | lpr
#        This prints the FITS headers in the files ~/fits/*.fit and
#        ~/data/p?.fi*.

#  Notes:
#     -  Prompting is directed to the standard error, so that the listings
#     may be redirected to a file.
#     -  If the blocking factor is unknown it is possible to obtain only
#     a part of the headers and some of the FITS data.  Unless the FITS
#     file is small, it is usually safe to set parameter BLOCK higher
#     than its true value.

#  Related Applications:
#     KAPPA: FITSEDIT, FITSEXP, FITSIMP, FITSLIST; Figaro: FITSKEYS.

#  Copyright:
#     Copyright (C) 1994 Science & Engineering Research Council.
#     Copyright (C) 2000, 2003 Central Laboratory of the Research
#     Councils.  All Rights Reserved.

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
#     {enter_new_authors_here}

#  History:
#     1994 November 4 (MJC):
#        Original version.
#     1994 November 10 (MJC):
#        Added ADAM-like prompting user interface.
#     13-APR-2000 (DSB):
#        Added "bs=80" to the dd command at the end of the script to prevent
#        an unwanted newline being added at 512 intervals.
#     2003 January 24 (MJC):
#        Revised the sed termination patterns.  No longer support
#        illegally formatted END lines.
#     {enter_further_changes_here}

#-

#   Specify the text search limits, and that we want to print the text
#   between these limits to standard output.  These look for lines
#   starting "SIMPLE  =" and "END " for a primary header and data unit,
#   and for lines starting "XTENSION=" and "END " for an extension.
#   If strict adherance to the FITS standard was followed the end of the
#   extraction should be /^END$/.  The conv=unblock in the dd command
#   strips trailing blanks.  Although sometimes people put in a
#   comment or equals sign, this cannot be accommodated while continuing
#   beyond a keyword beginning END.
#
set sedsim = '/^SIMPLE  = /,/^END$/p'
set sedext = '/^XTENSION= /,/^END$/p'
#
#   Process input file as standard input.
#   =====================================
#
#   If there is no argument the input files could be supplied through
#   standard input.  So continue the pipeline.  Format the output as
#   80-character lines.
#
if ( $#argv == 0 ) then
   dd cbs=80 conv=unblock | sed -n -e "$sedsim" -e "$sedext"
   exit
endif
#
#   Remove special keywords.
#   ========================
#
#   Look for a case-insensitive prompt string in the command line, meaning
#   prompt for the missing parameters; or the accept command by keyword
#   or its backslash abbreviation.  Form a new list and count of the
#   words on the command line, excluding these special keywords.
#
set i = 1
set cmd =
set narg = 0
set prompt = 0
while ($i <= $#argv)
   if ($argv[$i] =~ [Pp][Rr][Oo][Mm][Pp][Tt]) then
      set prompt = 1
   else if ($argv[$i] =~ [Aa][Cc][Cc][Ee][Pp][Tt] || \
            $argv[$i] =~ \\ ) then
   else
      @ narg = $narg + 1
      set cmd = ($cmd $argv[$i])
   endif
   @ i = $i + 1
end
#
#   Check the existence of the files.
#   =================================
#
#   Initialise logical that decides whether user needs to be prompted because
#   a supplied file does not exist.
#
set prfile = 0
if ($narg > 0) then
#
#   Find whether or not the first argument is a tape drive.
#   *** This may not be portable. ***
#
   if ( ( (! -f $cmd[1]) && -e $cmd[1] ) || $cmd[1] =~ /dev/*) then
#
#   Check that the tape or every file exists.  If any do not exist
#   record this so that the user can be prompt for a new value or values.
#
      if (! -e $cmd[1]) then
         sh -c "echo 'Tape unit "$cmd[1]" does not exist.' 1>&2"
         set prfile = 1
      endif
   else
      foreach file ($cmd[*])
         if (! -e $file) then
            sh -c "echo 'File "$file" does not exist.' 1>&2"
            set prfile = 1
         endif
      end
   endif
endif
#
#   Obtain the file(s) to be listed.
#   ================================
#
if ($narg < 1 || $prfile == 1) then
#
#   Set the logical that says whether or not a valid value has been
#   supplied.
#
   set ok = 0
   while ($ok == 0)
#
#   Assume that the value will be fine unless we discover otherwise
#   later.  Prompt for the value.  We must prevent the ? from being
#   treated as a single-character pattern match.  Various combinations
#   of quotes and backslashes do not seem to work.  Hence we use noglob.
#
      set ok = 1
      set noglob
      sh -c "echo -n 'FILE - List of FITS files or tape drive > ' 1>&2"
      set files = $<
#
#   Write some help information, but continue in the loop.
#
      if ($files[1] == '?') then
         sh -c "echo '  ' 1>&2"
         sh -c "echo '   file  = FILENAME (Read)' 1>&2"
         sh -c "echo '      A space-separated list of FITS files whose headers are to be' 1>&2"
         sh -c "echo '      listed, or the name of a single no-rewind tape device.  The list,' 1>&2"
         sh -c "echo '      of files can include wildcard characters.' 1>&2"
         sh -c "echo ' ' 1>&2"
         set ok = 0
#
#   Abort when requested.
#
      else if ("$files[1]" == \!\!) then
         exit
#
#   Reprompt when no value is given.
#
      else if ("$files" ==  ) then
         sh -c "echo 'No files given.  Enter "\!\!" to abort.' 1>&2"
         set ok = 0
#
#   Check that all the supplied files exist.
#
      else
         unset noglob
         foreach f ($files)
            if (! -e $f ) then
               sh -c "echo 'File "$f" does not exist.' 1>&2"
               set ok = 0
            endif
         end
      endif
      unset noglob
   end
else
   set files = ($cmd)
endif
#
#   Find whether or not the first argument is a tape drive.  Since the
#   user may have supplied a wildcarded set of files, these must be
#   expanded through the () syntax.  Assign a logical value for whether
#   or not the first file is a tape drive.  Once the first file has been
#   tested break out of the loop.
#
set i = 1
foreach f ($files)
   if ($i == 1) then
      if (-f $f) then
         set tape = 0
      else
         set tape = 1
      endif
      break
   endif
end
if ($tape == 1) then
#
#   Obtain the blocking factor.
#   ===========================
#
#   Use a logical to expression to decide whether or not the value
#   of the command line is valid.
#
   set badval = 1
   if ( $narg > 1 ) then
      @ badval = ( $cmd[2] !~ [0-9]* || $cmd[2] < 1 || $cmd[2] > 12 )
   else
      set badval = 0
   endif
#
#   Decide whether prompting is needed.
#
   if ( ($narg < 2 && $prompt == 1) || $badval == 1 ) then
#
#   Inform the user of the error before prompting.
#
      if ($badval == 1) then
         sh -c "echo 'Tape blocking factor must be an integer between 1 and 12.' 1>&2"
      endif
#
#   Set the logical that says whether or not a valid value has been
#   supplied.
#
      set ok = 0
      while ($ok == 0)
#
#   Assume that the value will be fine unless we discover otherwise
#   later. Prompt for the blocking factor.
#
         set ok = 1
         set noglob
         sh -c "echo -n 'BLOCK - Give the FITS blocking factor /1/ >' 1>&2"
         set block = $<
#
#   Write some help information, but continue in the loop.
#
         if ($block == '?') then
            sh -c "echo ' ' 1>&2"
            sh -c "echo '   BLOCK = _INTEGER (Read)' 1>&2"
            sh -c "echo '      The FITS blocking factor of the tape to list.  This is the tape' 1>&2"
            sh -c "echo '      blocksize in bytes divided by the FITS record length of 2880' 1>&2"
            sh -c "echo '      bytes.  BLOCK must be a positive integer, between 1 and 12,' 1>&2"
            sh -c "echo '      otherwise you will be prompted for a new value.  Should the first' 1>&2"
            sh -c "echo '      argument not be a tape device, this argument will be treated as' 1>&2"
            sh -c "echo '      a file name. [1]' 1>&2"
            sh -c "echo ' ' 1>&2"
            set ok = 0
#
#   Abort when requested.
#
         else if ("$block" == \!\!) then
            exit
#
#   Accept the value.  Take the the null value to mean use the default.
#
         else if ("$block" == \! || "$block" ==  ) then
            set block = 1

#   Use a logical to expression to decide whether or not the value
#   given is valid.
#
         else if (!($block =~ [0-9]* && $block > 0 && $block < 13)) then
            sh -c "echo 'Tape blocking factor must be an integer between 1 and 12.' 1>&2"
            set ok = 0
         else
            continue
         endif
         unset noglob
      end
#
#   No prompting is required so use the default value.
#
   else if ($narg < 2) then
      set block = 1
#
#   Use the validated value from the command line.
#
   else
      set block = $cmd[2]
   endif
#
#   Obtain the lower file limit.
#   ============================
#
#   Use a logical to expression to decide whether or not the value
#   of the command line is valid.
#
   if ( $narg > 2 ) then
      @ badval = ($cmd[3] !~ [0-9]* || $cmd[3] < 1 )
   else
      set badval = 0
   endif
#
#   Decide whether prompting is needed.
#
   if ( ($narg < 3 && $prompt == 1) || $badval == 1) then
#
#   Inform the user of the error before prompting.
#
      if ($badval == 1) then
         sh -c "echo 'Tape file number must be a positive integer.' 1>&2"
      endif
#
#   Set the logical that says whether or not a valid value has been
#   supplied.
#
      set ok = 0
      while ($ok == 0)
#
#   Assume that the value will be fine unless we discover otherwise
#   later.  Prompt for the lower file limit.
#
         set ok = 1
         set noglob
         sh -c "echo -n 'START - First tape file to list /1/ >' 1>&2"
         set start = $<
#
#   Write some help information, but continue in the loop.
#
         if ($start == '?') then
            sh -c "echo ' ' 1>&2"
            sh -c "echo '   START = _INTEGER (Read)' 1>&2"
            sh -c "echo '      The first file on the tape to list.  This defaults to 1, i.e.' 1>&2"
            sh -c "echo '      the start of the tape.  It must be a positive integer,' 1>&2"
            sh -c "echo '      otherwise you will be prompted for a new value. [1]' 1>&2"
            sh -c "echo ' ' 1>&2"
            set ok = 0
#
#   Abort when requested.
#
         else if ("$start" == \!\!) then
            exit
#
#   Accept the value.  Take the the null value to mean the end of the tape.
#
         else if ("$start" == \! || "$start" ==  ) then
            set start = 1

#   Use a logical to expression to decide whether or not the value
#   given is valid.
#
         else if (!($start =~ [0-9]* && $start > 0)) then
            sh -c "echo 'The start file on the tape must be a positive integer.' 1>&2"
            set ok = 0
         else
            continue
         endif
         unset noglob
      end
#
#   No prompting is required so use the default value.
#
   else if ($narg < 3) then
      set start = 1
#
#   Use the validated value from the command line.
#
   else
      set start = $cmd[3]
   endif
#
#   Obtain the upper file limit.
#   ============================
#
#   Use a logical to expression to decide whether or not the value
#   of the command line is valid.
#
   if ( $narg > 3 ) then
      @ badval = ( $cmd[4] !~ [0-9]* || $cmd[4] < $start )
   else
      set badval = 0
   endif
#
#   Decide whether prompting is needed.
#
   if ( ($narg < 4 && $prompt == 1) || $badval == 1) then
#
#   Inform the user of the error before prompting.
#
      if ($badval == 1) then
         sh -c "echo 'Last tape file number must be a positive integer greater than the start value ("$start")' 1>&2"
      endif
#
#   Set the logical that says whether or not a valid value has been
#   supplied.
#
      set ok = 0
      while ($ok == 0)
#
#   Assume that the value will be fine unless we discover otherwise
#   later.  Prompt for the upper file limit.
#
         set ok = 1
         set noglob
         sh -c "echo -n 'FINISH - Last tape file to list /last/ >' 1>&2"
         set finish = $<
#
#   Write some help information, but continue in the loop.
#
         if ($finish == '?') then
            sh -c "echo ' ' 1>&2"
            sh -c "echo '   FINISH = _INTEGER (Read)' 1>&2"
            sh -c "echo '      The last file on the tape to list.  This defaults to the end' 1>&2"
            sh -c "echo '      of the tape.  It must be a positive integer and at least equal' 1>&2"
            sh -c "echo '      to the value of start, otherwise you will be prompted for a new' 1>&2"
            sh -c "echo '      value. []' 1>&2"
            sh -c "echo ' ' 1>&2"
            set ok = 0
#
#   Abort when requested.
#
         else if ("$finish" == \!\!) then
            exit
#
#   Accept the value.  Take the string "last" or the null value to mean the
#   end of the tape.  Use 999999 to indicate that the headers for
#   the tape files to the end of the tape should be listed.  This ought to be
#   enough.
#
         else if ("$finish" == \! || $finish =~ [Ll][Aa][Ss][Tt] || "$finish" ==  ) then
            set finish = 999999

#   Use a logical to expression to decide whether or not the value
#   given is valid, i.e. is an integer not less than the start value.
#
         else if (!($finish =~ [0-9]* && $finish >= $start )) then
            sh -c "echo 'Last tape file number must be a positive integer greater than the start value ("$start")' 1>&2"
            set ok = 0
         else
            continue
         endif
         unset noglob
      end
#
#   No prompting is required so use the default value.
#
   else if ($narg < 4) then
      set finish = 999999
#
#   Use the validated value from the command line.
#
   else
      set finish = $cmd[4]
   endif
#
#   Position the tape.
#   ==================
#
#   Rewind the tape.
#
   mt -f $files[1] rewind
#
#   Skip over files as requested.
#
   if ($start > 1 ) then
      @ skip = $start - 1
      mt -f $files[1] fsf $skip
#
#   Look for an error.  Exit if unable to skip down the tape.
#
      if ($status > 0) then
         sh -c "echo 'Unable to move to file number "$start"' 1>&2"
         exit
      endif
   endif
#
#   Evaluate the tape blocksize.
#
   @ block = $block * 2880
#
#   Loop from the start to the end file.
#
   set i = $start
   while ($i <= $finish)
#
#   Have a blank line between headers.
#
      if ($i > $start > 1) echo " "
      echo "FITS headers in "$files[1]" file number "$i":"
#
#   List the headers for the current file on the tape.
#
      dd ibs=$block if=$files[1] cbs=80 conv=unblock | \
         sed -n -e "$sedsim" -e "$sedext"
#
#   The most-likely error is for the EOT to be found, so there is no
#   error message reported.  Break from the loop.
#
      if ($status > 0) break
#
#   Increment the file counter.
#
      @ i = $i + 1
   end
#
#   A list of files have been given.  The files have already been checked
#   for existence.
#
else
   set i = 0
   foreach file ($files)
      @ i = $i + 1
#
#   Have a blank line between headers.
#
      if ($i > 1) echo " "
      echo "FITS headers in "$file":"
#
#   List the headers for the current file.
#
      dd cbs=80 bs=80 conv=unblock if=$file | sed -n -e "$sedsim" -e "$sedext"
   end
endif
#
#   Exit the procedure.
#
# end
