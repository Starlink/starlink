#include "atl.h"
#include "f77.h"
#include "ast.h"
#include "mers.h"
#include "star/grp.h"
#include "sae_par.h"

F77_SUBROUTINE(atl_chrsplitre)( CHARACTER(STR), CHARACTER(REGEXP),
                                INTEGER(MATCHEND), INTEGER(IGRP),
                                INTEGER(STATUS) TRAIL(STR) TRAIL(REGEXP) ) {
/*
*+
*  Name:
*     ATL_CHRSPLITRE

*  Purpose:
*     Extract sub-strings matching a specified regular expression.

*  Language:
*     C, designed to be called from Fortran.

*  Invocation:
*     CALL ATL_CHRSPLITRE( STR, REGEXP, MATCHEND, IGRP, STATUS )

*  Description:
*     This routine compares the supplied string with the supplied
*     regular expression. If they match, each section of the test string
*     that corresponds to a parenthesised sub-string in the regular
*     expression is copied and stored in the returned GRP group.

*  Arguments:
*     STR = CHARACTER * ( * ) (Given)
*        Pointer to the string to be split.
*     REGEXP = CHARACTER * ( * ) (Given)
*        The regular expression. See "Template Syntax:" in the astChrSub
*        prologue. Note, this function differs from astChrSub in that any
*        equals signs (=) in the regular expression are treated literally.
*     MATCHEND = INTEGER (Returned)
*        The index of the character that follows the last character within
*        the supplied test string (STR) that matched any parenthesises
*        sub-section of "regexp". A value of 0 is returned if no matches
*        were found.
*     IGRP = INTEGER (Given and Returned)
*        An identifier for an existing GRP group, or GRP__NOID. If
*        GRP__NOID is supplied a new empty group is created and its
*        identifier returned. On exit, the group is extended by appending
*        to it a copy of each sub-string extracted from the supplied string.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - This routine is just a wrapper around the C function atlChrSplitRE.
*     - If a parenthesised sub-string in the regular expression is matched
*     by more than one sub-string within the test string, then only the
*     first is returned. To return multiple matches, the regular
*     expression should include multiple copies of the parenthesised
*     sub-string (for instance, separated by ".+?" if the intervening
*     string is immaterial).

*  Copyright:
*     Copyright (C) 2011 Science & Technology Facilities Council.
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
*     DSB: David S. Berry  (JAC, HAwaii)
*     {enter_new_authors_here}

*  History:
*     21-NOV-2011 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*+
*/

/* Local Variables: */
   GENPTR_CHARACTER(STR)
   GENPTR_CHARACTER(REGEXP)
   GENPTR_INTEGER(MATCHEND)
   GENPTR_INTEGER(IGRP)

   char **substrings;
   char *regexp;
   char *str;
   const char *matchend;
   int *old_status;
   int i;
   int n;
   int new;

   *MATCHEND = 0;

   if( *STATUS != SAI__OK ) return;

   astAt( "ATL_CHRSPLITRE", NULL, 0 );
   old_status = astWatch( STATUS );

   str = astString( STR, STR_length );
   regexp = astString( REGEXP, REGEXP_length );
   substrings = astChrSplitRE( str, regexp, &n, &matchend );

   if( astOK ) {
      Grp *grp = grpF2C( *IGRP, STATUS );
      if( !grp ) {
         grp = grpNew( " ", STATUS );
         new = 1;
      } else {
         new = 0;
      }

      if( grp ) {
         for( i = 0; i < n; i++ ) {
            grpPut1( grp, substrings[ i ], 0, STATUS );
         }
         if( new ) *IGRP = grpC2F( grp, STATUS );
      }

      if( matchend ) *MATCHEND = matchend - str + 1;

   }

   str = astFree( str );
   regexp = astFree( regexp );

   astWatch( old_status );
}

