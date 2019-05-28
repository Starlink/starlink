#include <stdlib.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf_err.h"
#include "mers.h"
#include "ndf_ast.h"
#include "ndf1.h"
#include <string.h>

void ndf1Nsplt( const char *name, int rel, size_t *n1, size_t *n2,
                size_t *s1, size_t *s2, int *status ){
/*
*+
*  Name:
*     ndf1Nsplt

*  Purpose:
*     Split an NDF name into an HDS name and a section specification.

*  Synopsis:
*     void ndf1Nsplt( const char *name, int rel, size_t *n1, size_t *n2,
*                     size_t *s1, size_t *s2, int *status )

*  Description:
*     This function analyses an absolute or relative NDF name and splits it
*     into the (absolute or relative) name of the HDS object and a section
*     specification. Both components are optional (unless it is an absolute
*     name, in which case the HDS name component must exist).

*  Parameters:
*     name
*        Pointer to a null terminated string holding the NDF name to be
*        analysed.
*     rel
*        If a non-zero value is given, then "name" will be interpreted as a
*        relative NDF name, lacking a container file specification.
*        Otherwise an absolute name, including a container file
*        specification, will be expected.
*     *n1
*        Returned holding the zero-based character position in "name" of
*        the start of the HDS object name.
*     *n2
*        Returned holding the zero-based character position in "name" of
*        the end of the HDS object name.
*     *s1
*        Returned holding the zero-based character position of the start
*        of the section specification.
*     *s2
*        Returned holding the zero-based character position of the end of
*        the section specification.
*     *status
*        The global status.

*  Notes:
*     -  Any blanks which surround either component will be omitted from
*     the character positions returned.
*     -  This function will perform some simple checks on the validity of
*     the NDF name. However, these are not complete and complete validity
*     can only be checked by actually locating the object which the name
*     describes.
*     -  If the HDS name and/or section spec. components do not exist, then
*     "n1" will be returned greater than "n2" and/or "s1" will be returned
*     greater than "s2".
*     -  The component returned as a section spec. may, in some cases,
*     actually be an HDS cell or slice specification applied to a non-
*     scalar HDS object. This ambiguity can only be resolved by locating
*     the object and checking its dimensionality.

*  Copyright:
*     Copyright (C) 2018 East Asian Observatory
*     All rights reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or modify
*     it under the terms of the GNU General Public License as published by
*     the Free Software Foundation; either version 2 of the License, or (at
*     your option) any later version.
*
*     This program is distributed in the hope that it will be useful, but
*     WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
*     General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     DSB: David S. Berry (EAO)

*  History:
*     3-APR-2019 (DSB):
*        Original version, based on equivalent Fortran function by RFWS.
*     28-MAY-2019 (DSB):
*        Fix bug that caused an HDS error report if the object name is blank.

*-
*/

/* Local Variables: */
   char  *p;             /* Pointer to next character to check */
   char  *pend;          /* Pointer to last character to check */
   char  *pzero;         /* Pointer to last zero-level character */
   char  *substr;        /* Local copy of substring to be analysed */
   int lpar;             /* Parenthesis level */
   size_t f1;            /* Start of container file spec. (junk) */
   size_t f2;            /* End of container file spec. (junk) */
   size_t lstr;          /* Length of substr */
   size_t p1;            /* Start of HDS path name */
   size_t p2;            /* End of HDS path name */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Make a null terminated local copy of the section of the supplied string
   to be analysed, excluding leading or trailing spaces. Initialise the
   returned indices for the start and end of the HDS name so that they
   contain the whole non-blank section of the supplied string. */
   substr = ndf1Strip( NULL, name, 1, 0, &lstr, n1, status );
   if( substr && lstr > 0 ) {
      *n2 = *n1 + lstr - 1;

/* If this is an absolute HDS object name (including a container file
   specification), then check it is not entirely blank and report an
   error if it is. */
      if( !rel ) {
         if( *substr == 0 ) {
            *status = NDF__NAMIN;
            errRep( " ", "Blank NDF name supplied.", status );

/* Otherwise, split the name into an HDS container file name and a path
   name. */
         } else {
            hdsSplit( substr, &f1, &f2, &p1, &p2, status );
         }

/* If it is a relative NDF name, then use the entire name as the HDS
   path name. */
      } else {
         p1 = 0;
         p2 = lstr - 1;
      }

/* Search through the path name counting nested levels of parenthesis. */
      if( *status == SAI__OK ) {
         lpar = 0;
         p = substr + p1 - 1;
         pend = substr + p2;
         pzero = pend;
         while( ++p <= pend ){

/* Note the last character which occurs at a nesting level of zero. */
            if( lpar == 0 ) pzero = p;
            if( *p == '(' ) {
               lpar++;
            } else if( *p == ')' ) {
               lpar--;
            }

/* If too many right parentheses are detected, then report an error and
   quit the loop. */
            if( lpar < 0 ) {
               *status = NDF__NAMIN;
               msgSetc( "NAME", substr );
               msgSetc( "THE", "the" );
               if( rel ) msgSetc( "THE", " relative" );
               errRep( " ", "Missing left parenthesis in ^THE NDF name "
                       "'^NAME'.", status );
               break;
            }
         }

/* If a missing right parenthesis is detected, then report an error. */
         if( *status == SAI__OK ) {
            if( lpar > 0 ) {
               *status = NDF__NAMIN;
               msgSetc( "NAME", substr );
               msgSetc( "THE", "the" );
               if( rel ) msgSetc( "THE", " relative" );
               errRep( " ", "Missing right parenthesis in ^THE NDF name "
                       "'^NAME'.", status );

/* If the last non-parenthesised character is not the final character
   in the path name, then there is a trailing section specification and
   this character marks the opening parenthesis. Record the location of
   the section specification. */
            } else if( pzero != pend ) {
               *s1 = pzero - substr + *n1;
               *s2 = p2 + *n1;

/* Replace the opening parenthesis with a terminating null so that
   astChrLen (called below) will return the used length of the string
   before it (i.e. excluding trailing blanks). */
               *pzero = 0;

/* Update the object name end position, removing any new trailing
   blanks. Since n1 and n2 are unsigned, we need to check explicitly for
   condifitions that might try to set n2 to a negativer value (i.e. cases
   where the object name is missing). */
               if( *s1 > 0 ) {
                  *n2 = *s1 - 1;
                  if( *n1 <= *n2 ) *n2 = *n1 + astChrLen( substr ) - 1;
               } else {
                  *n1 = 1;
                  *n2 = 0;
               }

/* Note if no trailing section specification was found. */
            } else {
               *s1 = 1;
               *s2 = 0;
            }
         }
      }
   }

/* Free the local copy of the name. */
   substr = astFree( substr );

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Nsplt", status );

}

