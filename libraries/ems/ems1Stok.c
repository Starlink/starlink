/*
*+
*  Name:
*     EMS1STOK

*  Purpose:
*     Set the value of a token with character string.

*  Language:
*     Starlink ANSI C

*  Invocation:
*     ems1Stok( token, string )

*  Description:
*     This subroutine assigns the supplied character string to the named 
*     message token. Errors arising here will lead to indicators in the 
*     expanded message text.

*  Arguments:
*     token = const char* (Given)
*        The message token name.
*     string = const char* (Given)
*        The message token string.

*  Algorithm:
*     -  The token is looked up in the token table and inserted if it is not 
*     found. The supplied character string is then associated with the token.

*  Copyright:
*     Copyright (C) 1983 Science & Engineering Research Council.
*     Copyright (C) 2001 Central Laboratory of the Research Councils.
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     JRG: Jack Giddings (UCL)
*     SLW: Sid Wright (UCL)
*     PCTR: P.C.T. Rees (STARLINK)
*     RTP: R.T.Platon (STARLINK)
*     AJC: A.J.Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*    3-JAN-1983 (JRG):
*        Original FORTRAN version.
*    14-FEB-2001 (RTP):
*        Rewritten in C from Fortran Routine EMS1_STOK.
*     6-MAR-2001 (AJC):
*        Add maxlen argument for ems1Putc
*        Correct loop looking for existing token (and use strcasecmp).
*    {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "ems1.h"                    /* EMS1_ Internal functions */
#include "ems_par.h"                 /* EMS_ public constants */
#include "ems_sys.h"                 /* EMS_ private constants */
#include "ems_toktb.h"               /* Message token table */

void ems1Stok( const char *token, const char *string ) {
   int i;                         /* Loop index */
   int last;                      /* Search limit for token definition */
   int lstat;                     /* Local status */
   int tlen;                      /* Length of the token name */
   int tokln;                     /* Pointer within token string */

   TRACE("ems1Stok");

/*  Get the token length. */
   tlen = strlen( token );

/*  Act only for token names which are non-zero length. */
   if ( tlen > 0 ) { 

/*     Find any existing table entry for the token, TOKEN. */
      i = tokcnt[ tokmrk ];

      if ( tokmrk > EMS__BASE ) {
         last = tokhiw[ tokmrk - 1 ];
      } else {
         last = 0;
      }

/*     Search for the token name TOKEN in the token table. */
      while ( i > last ) {
         if ( strcasecmp( token, toknam[ i ] ) == 0 ) break;
         i--;
      }

/*     Allocate a new table entry for this token if one does not exist. */
      if ( i > last ) {

/*        Append the token string to the existing token string. */
         tokln = toklen[ i ] - 1;
         ems1Putc( string, EMS__SZTOK, tokstr[ i ], &tokln, &lstat );
         toklen[ i ] = tokln + 1;

      } else {
/*        This is a new token: first check that the message token table 
 *        is not full.
 */
         if ( tokcnt[ tokmrk ] < EMS__MXTOK ) {

/*           There is room in the message token table, so increment the
 *           token count and add the new token.
 */
            i = tokcnt[ tokmrk ] + 1;
            tokcnt[ tokmrk ] = i;
            tokhiw[ tokmrk ] = i;
            strcpy( toknam[ i ],token );
            toklen[ i ] = 0;

/*           Assign the token string. */
            tokln = toklen[ i] - 1;
            ems1Putc( string, EMS__SZTOK, tokstr[ i ], &tokln, &lstat );
            toklen[ i ] = tokln + 1;

        } else {

/*           There is no room in the message token table, so abort. */
            return;
         }
      }
   }

   return;
}
