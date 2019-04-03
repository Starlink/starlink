#include <ctype.h>
#include <stdlib.h>
#include "sae_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "ndf_ast.h"
#include "mers.h"
#include <string.h>

void ndf1Psfmt( const char *fmt, size_t *f1, size_t *f2, size_t *e1,
                size_t *e2, int *status ){
/*
*+
*  Name:
*     ndf1Psfmt

*  Purpose:
*     Parse a foreign data format specification.

*  Synopsis:
*     void ndf1Psfmt( const char *fmt, size_t *f1, size_t *f2, size_t *e1,
*                     size_t *e2, int *status )

*  Description:
*     This function parses a foreign data format specification of the form
*     NAME(.ext) and locates the "NAME" and ".ext" fields which contain the
*     data format name and the associated file type extension string.
*     Extraneous blanks are ignored and the specificaton is checked for
*     validity.

*  Parameters:
*     fmt
*        Pointer to a null terminated string holding the foreign data
*        format specification to be parsed.
*     *f1
*        Returned holding the zero-based character position of the start
*        of the "NAME" field.
*     *f2
*        Returned holding the zero-based character position of the end
*        of the "NAME" field.
*     *e1
*        Returned holding the zero-based character position of the start
*        of the ".ext" field.
*     *e2
*        Returned holding the zero-based character position of the end
*        of the ".ext" field.
*     *status
*        The global status.

*  Copyright:
*     Copyright (C) 2018 East Asian Observatory
*     All rights reserved.

*  License:
*     This program is free software; you can redistribute it and/or modify
*     it under the terms of the GNU General Public License as published by
*     the Free Software Foundation; either version 2 of the License, or (at
*     your option) any later version.
*
*     This program is distributed in the hope that it will be useful, but
*     WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
*     General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program (see "slaConditions"); if not, write to the
*     Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
*     Boston, MA  02110-1301  USA

*  Licence:
*     This program is free software; you can redistribute it and/or modify
*     it under the terms of the GNU General Public License as published by
*     the Free Software Foundation; either version 2 of the License, or (at
*     your option) any later version.
*
*     This program is distributed in the hope that it will be useful,but
*     WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
*     General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA 02111-1307,
*     USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     DSB: David S. Berry (EAO)

*  History:
*     xxx (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/

/* Local Variables: */
   const char *p;        /* Pointer to next character to check */
   const char *pend;     /* Pointer to last character to check */
   size_t i1;            /* First non-blank character position */
   size_t i2;            /* Last non-blank character position */
   size_t j1;            /* Start of parentheses */
   size_t j2;            /* End of parentheses */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Find the first and last non-blank characters in the input string and
   report an error if the string is entirely blank. */
   astFandl( fmt, 1, 0, &i1, &i2 );
   if( i1 > i2 ) {
      *status = NDF__FMTIN;
      errRep( " ", "Blank data format specified.", status );

/* If OK, search for a parenthesised expression, which should contain
   the file type extension string. */
   } else {
      (void) astBrackets( fmt, 1, 0, '(', ')', 1, &j1, &j2, NULL, NULL, NULL );

/* Check that this expression is present. */
      if( j1 > j2 ) {
         *status = NDF__FMTIN;
         msgSetc( "FMT", fmt + i1 );
         errRep( " ", "Missing parenthesis in the data format "
                 "specification '^FMT'.", status );

/* Check that the parentheses are not adjacent. */
      } else if( j2 - j1 <= 1 ) {
         *status = NDF__FMTIN;
         msgSetc( "FMT", fmt + i1 );
         errRep( " ", "Missing file type extension in the data format "
                 "specification '^FMT'.", status );

/* Check that there is a data format name in front of it. */
      } else if( j1 == i1 ) {
         *status = NDF__FMTIN;
         msgSetc( "FMT", fmt + i1 );
         errRep( " ", "Missing format name in the data format "
                 "specification '^FMT'.", status );

/* Check that there is nothing following it. */
      } else if( j2 != i2 ) {
         *status = NDF__FMTIN;
         msgSetc( "FMT", fmt + i1 );
         errRep( " ", "Extra characters following the data format in the "
                 "specification '^FMT'.", status );

/* If OK, remove surrounding blanks from the name field and the file
   type string within the parentheses. */
      } else {
         astFandl( fmt, 0, j1 - 1, f1, f2 );
         astFandl( fmt, j1 + 1, j2 - 1, e1, e2 );

/* Check that the file type string is not blank. */
         if( *e2 - *e1 < 1 ) {
            *status = NDF__FMTIN;
            msgSetc( "FMT", fmt + i1 );
            errRep( " ", "Missing file type extension in the data format "
                    "specification '^FMT'.", status );

/* Check that the file type string starts with ".". */
         } else if( fmt[ *e1 ] != '.' ) {
            *status = NDF__FMTIN;
            msgSetc( "FMT", fmt + i1 );
            errRep( " ", "The leading '.' is missing from the file type "
                    "extension in the data format specification '^FMT'.",
                    status );

/* Examine each character in the format name for validity. Report an
   error if an invalid character is found. */
         } else {
            p = fmt + *f1 - 1;
            pend = fmt + *f2;
            while( ++p <= pend ) {
               if( !( isalnum( *p ) || *p == '_' || *p == '-' ) ) {
                  *status = NDF__FMTIN;
                  msgSetc( "FMT", fmt + *f1 );
                  errRepf( " ", "Invalid character '%c' encountered in "
                           "the data format name '^FMT'.", status, *p );
                  break;
               }
            }

/* Similarly, check each character in the file type extension for
   validity, reporting an error if necessary. */
            if( *status == SAI__OK ) {
               p = fmt + *e1 - 1;
               pend = fmt + *e2;
               while( ++p <= pend ) {
                  if( !( isalnum( *p ) || *p == '_' || *p == '-' || *p == '.' ) ) {
                     *status = NDF__FMTIN;
                     msgSetc( "EXT", fmt + *e1 );
                     errRepf( " ", "Invalid character '%c' encountered "
                             "in the file type extension '^EXT'.", status, *p );
                     break;
                  }
               }
            }
         }
      }
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Psfmt", status );

}

