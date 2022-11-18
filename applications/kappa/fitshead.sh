#!/bin/sh
#+
#  Name:
#     FITSHEAD

#  Purpose:
#     Lists the headers of FITS files.

#  Language:
#     Bourne shell

#  Type of Module:
#     Bourne shell script

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
#     2022 June 13 (GSB):
#        Convert to sh.
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
sedsim='/^SIMPLE  = /,/^END$/p'
sedext='/^XTENSION= /,/^END$/p'
#
#   Process input file as standard input.
#   =====================================
#
#   If there is no argument the input files could be supplied through
#   standard input.  So continue the pipeline.  Format the output as
#   80-character lines.
#
if [ "$#" -eq 0 ]; then
   dd cbs=80 conv=unblock | sed -n -e "$sedsim" -e "$sedext"
   exit
fi
#
#   Remove special keywords.
#   ========================
#
#   Look for a case-insensitive prompt string in the command line, meaning
#   prompt for the missing parameters; or the accept command by keyword
#   or its backslash abbreviation.  Form a new list and count of the
#   words on the command line, excluding these special keywords.
#
cmd=
prompt=
for arg in "$@"; do
   case "$arg" in
   [Pp][Rr][Oo][Mm][Pp][Tt])
      prompt=1
      ;;
   [Aa][Cc][Cc][Ee][Pp][Tt])
      ;;
   "\\")
      ;;
   *)
      cmd="$cmd $arg"
      ;;
   esac
done
#
#   Check the existence of the files.
#   =================================
#
#   Initialise logical that decides whether user needs to be prompted because
#   a supplied file does not exist.
#
prfile=
set $cmd
if [ "$#" -gt 0 ]; then
#
#   Find whether or not the first argument is a tape drive.
#   *** This may not be portable. ***
#
   case "$1" in
   /dev/*)
#
#   Check that the tape or every file exists.  If any do not exist
#   record this so that the user can be prompt for a new value or values.
#
      if [ ! -e "$1" ]; then
         echo "Tape unit \"$1\" does not exist." 1>&2
         prfile=1
      fi
      ;;
   *)
      if ( [ ! -f "$1" ] && [ -e "$1" ] ); then
         :
      else
         for file in "$@"; do
            if [ ! -e "$file" ]; then
               echo "File \"$file\" does not exist." 1>&2
               prfile=1
            fi
         done
      fi
      ;;
   esac
fi
#
#   Obtain the file(s) to be listed.
#   ================================
#
if ([ "$#" -lt 1 ] || [ -n "$prfile" ]); then
#
#   Set the logical that says whether or not a valid value has been
#   supplied.
#
   ok=
   while [ -z "$ok" ]; do
#
#   Assume that the value will be fine unless we discover otherwise
#   later.  Prompt for the value.
#
      ok=1
      echo -n 'FILE - List of FITS files or tape drive > ' 1>&2
      read files
#
#   Write some help information, but continue in the loop.
#
      if [ "$files" = '?' ]; then
         echo '  ' 1>&2
         echo '   file  = FILENAME (Read)' 1>&2
         echo '      A space-separated list of FITS files whose headers are to be' 1>&2
         echo '      listed, or the name of a single no-rewind tape device.  The list' 1>&2
         echo '      of files can include wildcard characters.' 1>&2
         echo ' ' 1>&2
         ok=
#
#   Abort when requested.
#
      elif ([ "$files" = '!!' ] || [ "$files" = '!' ]); then
         exit
#
#   Reprompt when no value is given.
#
      elif [ -z "$files" ]; then
         echo 'No files given.  Enter "!!" to abort.' 1>&2
         ok=
#
#   Check that all the supplied files exist.
#
      else
         for f  in $files; do
            if [ ! -e "$f" ]; then
               echo "File \"$f\" does not exist." 1>&2
               ok=
            fi
         done
      fi
   done

   set $files
fi
#
#   Find whether or not the first argument is a tape drive.  Since the
#   user may have supplied a wildcarded set of files, these must be
#   expanded through the () syntax.  Assign a logical value for whether
#   or not the first file is a tape drive.
#
if [ -f "$1" ]; then
   tape=
else
   tape=1
fi
if [ -n "$tape" ]; then
#
#   Obtain the blocking factor.
#   ===========================
#
#   Use a logical to expression to decide whether or not the value
#   of the command line is valid.
#
   badval=1
   if [ "$#" -gt 1 ]; then
      case "$2" in
      [0-9]|[0-9][0-9])
         if ([ "$2" -ge 1 ] && [ "$2" -le 12 ]); then
            badval=
         fi
         ;;
      esac
   else
      badval=
   fi
#
#   Decide whether prompting is needed.
#
   if (([ "$#" -lt 2 ] && [ -n "$prompt" ]) || [ -n "$badval" ]); then
#
#   Inform the user of the error before prompting.
#
      if [ -n "$badval" ]; then
         echo 'Tape blocking factor must be an integer between 1 and 12.' 1>&2
      fi
#
#   Set the logical that says whether or not a valid value has been
#   supplied.
#
      ok=
      while [ -z "$ok" ]; do
#
#   Assume that the value will be fine unless we discover otherwise
#   later. Prompt for the blocking factor.
#
         ok=1
         echo -n 'BLOCK - Give the FITS blocking factor /1/ > ' 1>&2
         read block
#
#   Write some help information, but continue in the loop.
#
         if [ "$block" = '?' ]; then
            echo ' ' 1>&2
            echo '   BLOCK = _INTEGER (Read)' 1>&2
            echo '      The FITS blocking factor of the tape to list.  This is the tape' 1>&2
            echo '      blocksize in bytes divided by the FITS record length of 2880' 1>&2
            echo '      bytes.  BLOCK must be a positive integer, between 1 and 12,' 1>&2
            echo '      otherwise you will be prompted for a new value.  Should the first' 1>&2
            echo '      argument not be a tape device, this argument will be treated as' 1>&2
            echo '      a file name. [1]' 1>&2
            echo ' ' 1>&2
            ok=
#
#   Abort when requested.
#
         elif [ "$block" = '!!' ]; then
            exit
#
#   Accept the value.  Take the the null value to mean use the default.
#
         elif ([ "$block" = '!' ] || [ -z "$block" ] ); then
            block=1

#   Use a logical to expression to decide whether or not the value
#   given is valid.
#
         else
            case "$block" in
            [0-9]|[0-9][0-9])
               if ([ "$block" -lt 1 ] || [ "$block" -gt 12 ]); then
                  echo 'Tape blocking factor must be an integer between 1 and 12.' 1>&2
                  ok=
               fi
               ;;
            *)
               echo 'Tape blocking factor must be an integer between 1 and 12.' 1>&2
               ok=
               ;;
            esac
         fi
      done
#
#   No prompting is required so use the default value.
#
   elif [ "$#" -lt 2 ]; then
      block=1
#
#   Use the validated value from the command line.
#
   else
      block="$2"
   fi
#
#   Obtain the lower file limit.
#   ============================
#
#   Use a logical to expression to decide whether or not the value
#   of the command line is valid.
#
   badval=1
   if [ "$#" -gt 2 ]; then
      case "$3" in
      [0-9]|[0-9][0-9]|[0-9][0-9][0-9])
         if [ "$3" -ge 1 ]; then
            badval=
         fi
         ;;
      esac
   else
      badval=
   fi
#
#   Decide whether prompting is needed.
#
   if (([ "$#" -lt 3 ] && [ -n "$prompt" ]) || [ -n "$badval" ]); then
#
#   Inform the user of the error before prompting.
#
      if [ -n "$badval" ]; then
         echo 'Tape file number must be a positive integer.' 1>&2
      fi
#
#   Set the logical that says whether or not a valid value has been
#   supplied.
#
      ok=
      while [ -z "$ok" ]; do
#
#   Assume that the value will be fine unless we discover otherwise
#   later.  Prompt for the lower file limit.
#
         ok=1
         echo -n 'START - First tape file to list /1/ > ' 1>&2
         read start
#
#   Write some help information, but continue in the loop.
#
         if [ "$start" = '?' ]; then
            echo ' ' 1>&2
            echo '   START = _INTEGER (Read)' 1>&2
            echo '      The first file on the tape to list.  This defaults to 1, i.e.' 1>&2
            echo '      the start of the tape.  It must be a positive integer,' 1>&2
            echo '      otherwise you will be prompted for a new value. [1]' 1>&2
            echo ' ' 1>&2
            ok=
#
#   Abort when requested.
#
         elif [ "$start" = '!!' ]; then
            exit
#
#   Accept the value.  Take the the null value to mean the end of the tape.
#
         elif ([ "$start" = '!' ] || [ -z "$start" ]); then
            start=1

#   Use a logical to expression to decide whether or not the value
#   given is valid.
#
         else
            case "$start" in
            [0-9]|[0-9][0-9]|[0-9][0-9][0-9])
               if [ "$start" -lt 1 ]; then
                  echo 'The start file on the tape must be a positive integer.' 1>&2
                  ok=
               fi
               ;;
            *)
               echo 'The start file on the tape must be a positive integer.' 1>&2
               ok=
               ;;
            esac
         fi
      done
#
#   No prompting is required so use the default value.
#
   elif [ "$#" -lt 3 ]; then
      start=1
#
#   Use the validated value from the command line.
#
   else
      start="$3"
   fi
#
#   Obtain the upper file limit.
#   ============================
#
#   Use a logical to expression to decide whether or not the value
#   of the command line is valid.
#
   badval=1
   if [ "$#" -gt 3 ]; then
      case "$4" in
      [0-9]|[0-9][0-9]|[0-9][0-9][0-9])
         if [ "$4" -ge "$start" ]; then
            badval=
         fi
         ;;
      esac
   else
      badval=
   fi
#
#   Decide whether prompting is needed.
#
   if (([ "$#" -lt 4 ] && [ -n "$prompt" ]) || [ -n "$badval" ]); then
#
#   Inform the user of the error before prompting.
#
      if [ -n "$badval" ]; then
         echo "Last tape file number must be a positive integer greater than the start value ($start)" 1>&2
      fi
#
#   Set the logical that says whether or not a valid value has been
#   supplied.
#
      ok=
      while [ -z "$ok" ]; do
#
#   Assume that the value will be fine unless we discover otherwise
#   later.  Prompt for the upper file limit.
#
         ok=1
         echo -n 'FINISH - Last tape file to list /last/ > ' 1>&2
         read finish
#
#   Write some help information, but continue in the loop.
#
         if [ "$finish" = '?' ]; then
            echo ' ' 1>&2
            echo '   FINISH = _INTEGER (Read)' 1>&2
            echo '      The last file on the tape to list.  This defaults to the end' 1>&2
            echo '      of the tape.  It must be a positive integer and at least equal' 1>&2
            echo '      to the value of start, otherwise you will be prompted for a new' 1>&2
            echo '      value. []' 1>&2
            echo ' ' 1>&2
            ok=
#
#   Abort when requested.
#
         elif [ "$finish" = '!!' ]; then
            exit
#
#   Accept the value.  Take the string "last" or the null value to mean the
#   end of the tape.  Use 999999 to indicate that the headers for
#   the tape files to the end of the tape should be listed.  This ought to be
#   enough.
#
         else
            case "$finish" in
            '!'|''|[Ll][Aa][Ss][Tt])
               finish=999999
               ;;
#   Use a logical to expression to decide whether or not the value
#   given is valid, i.e. is an integer not less than the start value.
#
            [0-9]|[0-9][0-9]|[0-9][0-9][0-9])
               if [ "$finish" -lt "$start" ]; then
                  echo 'Last tape file number must be a positive integer greater than the start value ("$start")' 1>&2
                  ok=
               fi
               ;;
            *)
               echo 'Last tape file number must be a positive integer greater than the start value ("$start")' 1>&2
               ok=
               ;;
            esac
         fi
      done
#
#   No prompting is required so use the default value.
#
   elif [ "$#" -lt 4 ]; then
      finish=999999
#
#   Use the validated value from the command line.
#
   else
      finish="$4"
   fi
#
#   Position the tape.
#   ==================
#
#   Rewind the tape.
#
   mt -f "$1" rewind
#
#   Skip over files as requested.
#
   if [ "$start" -gt 1 ]; then
      skip=$(( $start - 1 ))
      mt -f "$1" fsf $skip
#
#   Look for an error.  Exit if unable to skip down the tape.
#
      if [ $? -ne 0 ]; then
         echo "Unable to move to file number \"$start\"" 1>&2
         exit
      fi
   fi
#
#   Evaluate the tape blocksize.
#
   block=$(( $block * 2880 ))
#
#   Loop from the start to the end file.
#
   i=$start
   while [ $i -le $finish ]; do
#
#   Have a blank line between headers.
#
      if [ $i -gt $start ]; then
         echo " "
      fi
      echo "FITS headers in \"$1\" file number \"$i\":"
#
#   List the headers for the current file on the tape.
#
      dd ibs=$block if="$1" cbs=80 conv=unblock | \
         sed -n -e "$sedsim" -e "$sedext"
#
#   The most-likely error is for the EOT to be found, so there is no
#   error message reported.  Break from the loop.
#
      if [ $? -ne 0 ]; then
         break
      fi
#
#   Increment the file counter.
#
      i=$(( $i + 1 ))
   done
#
#   A list of files have been given.  The files have already been checked
#   for existence.
#
else
   i=0
   for file in "$@"; do
      i=$(( $i + 1 ))
#
#   Have a blank line between headers.
#
      if [ $i -gt 1 ]; then
         echo " "
      fi
      echo "FITS headers in \"$file\":"
#
#   List the headers for the current file.
#
      dd cbs=80 bs=80 conv=unblock if="$file" | \
         sed -n -e "$sedsim" -e "$sedext"
   done
fi
#
#   Exit the procedure.
#
# end
