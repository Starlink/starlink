#include <ctype.h>
#include <stdlib.h>
#include "sae_par.h"
#include "ndf1.h"
#include "mers.h"

void ndf1Gtxtn( const char *name, int mxxtn, int *def, char *xtn,
                size_t xtn_length, size_t xtn1[], size_t xtn2[],
                int *nxtn, int *status ){
/*
*+
*  Name:
*     ndf1Gtxtn

*  Purpose:
*     Get a list of NDF extension names from the environment.

*  Synopsis:
*     void ndf1Gtxtn( const char *name, int mxxtn, int *def, char *xtn,
*                     size_t xtn_length, size_t xtn1[], size_t xtn2[],
*                     int *nxtn, int *status )

*  Description:
*     This function obtains a list of NDF extension names from the
*     environment and parses and validates this list. It is intended for
*     obtaining lists of extensions which are to be recognised when
*     converting to or from foreign format data files.

*  Parameters:
*     name
*        Pointer to a null terminated string holding the name of the
*        environment variable which contains the list.
*     mxxtn
*        Maximum number of extension names that can be accommodated.
*     *def
*        Returned holding the whether the environment variable was defined.
*     xtn
*        Pointer to an array in which to return a null terminated string
*        holding the translation of the specified environment variable,
*        with all extension name fields validated and converted to upper
*        case.
*     xtn_length
*        The length of the supplied 'xtn' array. This should include
*        room for the terminating null.
*     xtn1
*        Returned holding the character positions in the "xtn" string at
*        which each extension name field begins. The supplied "xtn1" array
*        should have at least "mxxtn" elements.
*     xtn2
*        Returned holding the character positions in the "xtn" string at
*        which each extension name field ends. The supplied "xtn2" array
*        should have at least "mxxtn" elements.
*     *nxtn
*        Returned holding the number of extension names found.
*     *status
*        The global status.

*  Notes:
*     If the specified environment variable is not defined, then "def" will
*     be set to zero and no further extension name information will be
*     returned.

*  Copyright:
*     Copyright (C) 2018 East Asian Observatory
*     All rights reserved.

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
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     DSB: David S. Berry (EAO)

*  History:
*     xxx (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/

/* Local Variables: */
   int ixtn;             /* Loop counter for extension names */
   size_t f;             /* First character position */
   size_t i;             /* Current character position */
   size_t l;             /* Last character position */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Translate the environment variable to obtain a list of NDF extension
   names. */
   ndf1Gtenv( name, def, xtn, xtn_length, &l, status );

/* If the environment variable was defined, parse the resulting
   extension list to split it into separate fields containing extension
   names. Return the field positions and number of extension fields
   found. */
   *nxtn = 0;
   if( ( *status == SAI__OK ) && *def ) {
      ndf1Psffl( xtn, mxxtn, xtn1, xtn2, nxtn, status );

/* If OK, then loop to validate each extension name. */
      if( *status == SAI__OK ) {
         for( ixtn = 0; ixtn < *nxtn; ixtn++ ){
            f = xtn1[ ixtn ];
            l = xtn2[ ixtn ];

/* Check each name for validity. If OK, then convert it to upper case. */
            ndf1Chxnm( xtn, f, l, status );
            if( *status == SAI__OK ) {
               for( i = f; i <= l; i++ ) xtn[i] = toupper( xtn[i] );

/* Abort if a bad name is encountered. */
            } else {
               break;
            }
         }
      }

/* If an error occurred, then report contextual information. */
      if( *status != SAI__OK ) {
         msgSetc( "NAME", name );
         errRep( " ", "Error occurred while reading the ^NAME list of NDF "
                 "extension names (possible bad environment variable "
                 "setting).", status );
      }
   }

/* Call error tracing function if necessary. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Gtxtn", status );

}

