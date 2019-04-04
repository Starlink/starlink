#include <string.h>
#include "sae_par.h"
#include "prm_par.h"
#include "ndf_ast.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "mers.h"

void ndf1Move( const char *type, size_t n, const void *pntr1, void *pntr2,
               int *status ){
/*
*+
*  Name:
*     ndf1Move

*  Purpose:
*     Move a vectorised array to a new location.

*  Synopsis:
*     void ndf1Move( const char *type, size_t n, const void *pntr1,
*                    void *pntr2, int *status )

*  Description:
*     This function moves a vectorised array of any numeric data type from
*     one location to another. The source and destination arrays are passed
*     by pointer.

*  Parameters:
*     type
*        Pointer to a null terminated string holding the numeric data type
*        of the array to be moved; an HDS primitive numeric type string
*        (case insensitive).
*     n
*        Number of array elements to be moved.
*     pntr1
*        Pointer to the source array.
*     pntr2
*        Pointer to the destination array.
*     *status
*        The global status.

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
*     3-APR-2019 (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/

/* Local Variables: */
   size_t size;         /* No. of bytes to transfer */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Get the number of bytes to be transferred. */
   if( astChrMatch( type, "_BYTE" ) ) {
      size = n*VAL__NBB;
   } else if( astChrMatch( type, "_UBYTE" ) ) {
      size = n*VAL__NBUB;
   } else if( astChrMatch( type, "_DOUBLE" ) ) {
      size = n*VAL__NBD;
   } else if( astChrMatch( type, "_INTEGER" ) ) {
      size = n*VAL__NBI;
   } else if( astChrMatch( type, "_REAL" ) ) {
      size = n*VAL__NBR;
   } else if( astChrMatch( type, "_WORD" ) ) {
      size = n*VAL__NBW;
   } else if( astChrMatch( type, "_UWORD" ) ) {
      size = n*VAL__NBUW;
   } else if( astChrMatch( type, "_INT64" ) ) {
      size = n*VAL__NBK;

/* If a bad data type string was supplied then report an error. */
   } else {
      size = 0;
      *status = NDF__FATIN;
      msgSetc( "ROUTINE", "ndf1Move" );
      msgSetc( "BADTYPE", type );
      errRep( " ", "Function ^ROUTINE called with an invalid TYPE "
              "parameter of ^BADTYPE (internal programming error).", status );
   }

/* Transfer the values */
   if( size > 0 ) memcpy( pntr2, pntr1, size );

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Move", status );

}

