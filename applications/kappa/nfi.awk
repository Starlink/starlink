#+
#  Name:
#     nfi.awk

#  Purpose:
#     Edits the NDF_FORMATS_IN to a list of file extensions.

#  Language:
#     AWK script

#  Type of Module:
#     awk script

#  Description:
#     This script extracts the list of file extensions in order
#     from the environment variable NDF_FORMATS_IN, and prints out the
#     number of formats and the file extensions in a space-separated
#     list.  When NDF_FORMATS_IN is undefined, the script prints 0.

#  Usage:
#     printenv | grep NDF_FORMATS_IN | awk -f nfi.awk

#  Copyright:
#     Copyright (C) 1996 Central Laboratory of the Research Councils.
#     All Rights Reserved.

#  Licence:
#     This program is free software; you can redistribute it and/or
#     modify it under the terms of the GNU General Public License as
#     published by the Free Software Foundation; either version 2 of
#     the License, or (at your option) any later version.
#
#     This program is distributed in the hope that it will be
#     useful, but WITHOUT ANY WARRANTY; without even the implied
#     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
#     PURPOSE. See the GNU General Public License for more details.
#
#     You should have received a copy of the GNU General Public License
#     along with this program; if not, write to the Free Software
#     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
#     02110-1301, USA

#  Authors:
#     Malcolm J. Currie (STARLINK)
#     {enter_new_authors_here}

#  History:
#     1996 January 16 (MJC):
#        Original version.
#     {enter__changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

{  if ( $0 != "" ) {

#  Split the string using the comma separator.  Ignore the leading
#  "NDF_FORMATS_IN=" as the file extensions between the parentheses
#  will be extracted below.
     nformats = split( $0, formats, "," )

#  Start the output with the number of formats.
     printf ( "%d", nformats )

#  Loop for all the formats.
     for ( i = 1; i <= nformats; i++ ) {

#  Extract the file extension.  Do this by finding the positions of
#  the parentheses.
        if ( poslp = index( formats[i], "(" ) ) {
           if ( posrp = index( substr( formats[i], poslp + 1 ), ")" ) ) {
              fileext = substr( formats[i], poslp + 1, posrp - 1 )

#  Check that the file extension has been extracted.
              if ( fileext ~ /.[a-zA-Z0-9_]*/ ){
                 printf (" %s", fileext )
              }
           }
        }
     }
  }

  else print "0"

}
