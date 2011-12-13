/*+
 *  Name:
 *     ems1Rform

 *  Purpose:
 *     Reformat the given text to a new width.

 *  Language:
 *     Starlink ANSI C

 *  Invocation:
 *     ems1Rform( text, maxlen, iposn, string, strlength )

 *  Description:
 *     This subroutine is called repeatedly to reformat the given text string
 *     to a new width (given by maxlen). The returned line always has a ragged
 *     right margin. The text in the returned string is formatted to end at a
 *     word end. A word in this context is a contiguous range of non-blank
 *     characters.

 *  Arguments:
 *     text = char* (Given)
 *        The character variable which contains the text to be reformatted.
 *        Leading blanks are preserved.
 *     maxlen = const int (Given)
 *        Maximum length of the output string
 *     iposn = int* (Given and Returned)
 *        On entry, this argument specifies the character position in text
 *        from which to start generating the next returned line.  It is given
 *        as the number of characters from the first character in text. If a
 *        value less than 0 is given, then 0 will be used. If a value greater
 *        than the declared length of the returned string is given, the
 *        returned string is initialised to blank space and iposn is reset
 *        to 0.
 *
 *        On exit, this argument is set to one more than the position in text
 *        of the last blank character which appears in the returned line
 *        string (i.e. the position at which the generation of the next output
 *        line should start). When the end of the given string is reached,
 *        iposn is returned set to 0.
 *     string = char* (Returned)
 *        The returned line of text, left justified. The length of this
 *        string must be at least maxlen.
 *     strlength = int* (Returned)
 *        The used length of string.

 *  Notes:
 *     - This routine should be called repeatedly to generate successive
 *     returned lines from the given text.  Initially, the pointer iposn
 *     should be set to 0; it will be updated after each call, ready to
 *     generate the next returned line. A value of 0 is returned for iposn
 *     when there is no more text to process. Trailing blanks in the given
 *     text are ignored, multiple blanks between words are maintained, a
 *     single blank is dropped in multiple blanks which occur at a new
 *     returned line.
 *     - This routine does not know the format of EMS/ERR error strings so
 *     if an input string is '!  very_long_word' the first return line will
 *     be '!  ' and the second line returned will be 'very_long_word' truncated
 *     to maxlen characters. This is probably not what was actually intended.

 *  Copyright:
 *     Copyright (C) 1991 Science & Engineering Research Council.
 *     Copyright (C) 2001 Central Laboratory of the Research Councils.
 *     Copyright (C) 2005 Particle Physics & Astronomy Research Council.
 *     Copyright (C) 2008 Science and Technology Facilties Council.
 *     All Rights Reserved.

 *  Licence:
 *     This program is free software; you can redistribute it and/or
 *     modify it under the terms of the GNU General Public License as
 *     published by the Free Software Foundation; either version 2 of
 *     the License, or (at your option) any later version.
 *
 *     This program is distributed in the hope that it will be
 *     useful,but WITHOUT ANY WARRANTY; without even the implied
 *     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
 *     PURPOSE. See the GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with this program; if not, write to the Free Software
 *     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
 *     02110-1301, USA

 *  Authors:
 *     PCTR: P.C.T. Rees (STARLINK)
 *     AJC: A.J. Chipperfield (STARLINK)
 *     RTP: R.T. Platon (STARLINK)
 *     PWD: Peter W. Draper (JAC, Durham University)
 *     {enter_new_authors_here}

 *  History:
 *     11-APR-1991 (PCTR):
 *        Original FORTRAN version.
 *     14-FEB-2001 (RTP):
 *        Rewritten in C based on the Fortran routine EMS1_PFORM
 *      6-MAR-2001 (AJC):
 *        Added maxlen argument
 *        Copy strings not assign pointers
 *     21-SEP-2001 (AJC):
 *        Handle case of no suitable break on line.
 *        Correct calculation of ilast (-1)
 *      7-DEC-2005 (PWD):
 *        Return blank string and 0 iposn when the input iposn is
 *        greater than the length of string (as per description of
 *        iposn in prologue).
 *     01-AUG-2008 (PWD):
 *        Correct prologue description of iposn argument (start at 0 not 1).
 *        Correct Fortran capitalisations of argument names in descriptions.
 *     {enter_further_changes_here}

 *  Bugs:
 *     {note_any_bugs_here}

 *-
 */

#include <string.h>
#include "ems_sys.h"
#include "ems_par.h"                 /* EMS_ public constants */
#include "ems1.h"                    /* EMS_ internal functions */
#include "ems.h"

void ems1Rform( const char *text, const int maxlen, int *iposn, char *string,
                int *strlength  )
{
    int ilast;              /* Last allowed index of the substring */
    int iplen;              /* Used length of the given text */
    int istart;             /* Start index of substring */

    TRACE( "ems1Rform" );

    /*  Get the declared lengths of the given and returned character
     *  variables. */
    iplen = strlen( text );

    /*  If the given string is not empty and the starting position does not
     *  lie beyond the end of the given text, then there is potentially
     *  something to return. */
    if ( ( iplen > 0 ) && ( *iposn < iplen ) ) {

        /*  If the starting position is before the beginning of the string,
         *  advance it to the first character position. */
        if ( *iposn < 0 ) *iposn = 0;

        /*  Initialise the start index, istart, and the allowed length, ilast,
         *  of the given string. */
        istart = *iposn;
        ilast = istart + maxlen - 1;

        /*  Check whether the entire given substring will fit into the
         *  returned string. */
        if ( ilast > iplen ) {

            /*  The given substring can fit into the returned string, assign
             *  the returned string and update the returned pointer. */
            (void)strcpy( string, &text[ istart ] );
            *strlength = iplen - istart;
            *iposn = 0;

        } else {
            /*  Loop backwards through the given substring to find the last
             *  blank space that will fit into the returned string. */
            for ( *iposn = ilast; *iposn >= istart; (*iposn)-- ) {
                if ( text[ *iposn ] == ' ' ) break;
            }
            /*  If no space was found output the whole chunk. */
            if ( *iposn <= istart ) *iposn = ilast;

            /*  Assign the returned string and update the returned string
             *  length and character pointer. */
            *iposn = *iposn + 1;
            (void)strncpy( string, &text[ istart ], (size_t)(*iposn-istart) );
            string[*iposn-istart] = '\0';
            *strlength = *iposn - istart;
        }
    }
    else if ( ( iplen > 0 ) && ( *iposn >= iplen ) ) {
        /*  Cannot print beyond end of string, set result to blank and iposn
         *  to 0 as per contract. */
        strcpy( string, "" );
        *iposn = 0;
    }

    return;
}
