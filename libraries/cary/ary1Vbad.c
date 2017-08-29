#include <string.h>
#include "sae_par.h"
#include "ary1.h"
#include "mers.h"
#include "ary_err.h"
#include "dat_par.h"

void ary1Vbad( const char *type, size_t n, void *pntr, int *status ) {
/*
*+
*  Name:
*     ary1Vbad

*  Purpose:
*     Set all elements of a vectorised array to the "bad" value.

*  Synopsis:
*     void ary1Vbad( const char *type, size_t n, void *pntr, int *status )

*  Description:
*     The routine sets all elements of a vectorised array, of any
*     numeric data type, to the appropriate "bad" value. The array is
*     passed by pointer.

*  Parameters:
*     type
*        An HDS primitive numeric type string, specifying the data type
*        of the vectorised array (case insensitive).
*     n
*        Number of array elements.
*     pntr
*        Pointer to the array whose elements are to be set.
*     status
*        The global status.

*  Copyright:
*      Copyright (C) 2017 East Asian Observatory
*      All rights reserved.

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
*     RFWS: R.F. Warren-Smith (STARLINK)
*     DSB: David S. Berry (EAO)

*  History:
*     03-JUL-2017 (DSB):
*        Original version, based on equivalent Fortran routine by RFWS.

*-
*/

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Test the data type string supplied against each permitted value in turn,
   calling the appropriate routine to set the array elements to a "bad"
   value. */
   if( !strcasecmp( type, "_BYTE" ) ){
      ary1BadB( n, pntr, status );

   } else if( !strcasecmp( type, "_UBYTE" ) ){
      ary1BadUB( n, pntr, status );

   } else if( !strcasecmp( type, "_DOUBLE" ) ){
      ary1BadD( n, pntr, status );

   } else if( !strcasecmp( type, "_INTEGER" ) ){
      ary1BadI( n, pntr, status );

   } else if( !strcasecmp( type, "_REAL" ) ){
      ary1BadF( n, pntr, status );

   } else if( !strcasecmp( type, "_WORD" ) ){
      ary1BadW( n, pntr, status );

   } else if( !strcasecmp( type, "_UWORD" ) ){
      ary1BadUW( n, pntr, status );

   } else if( !strcasecmp( type, "_INT64" ) ){
      ary1BadK( n, pntr, status );

/* If the data type string was not valid, then report an error. */
   } else {
      *status = ARY__FATIN;
      msgSetc( "BADTYPE", type );
      errRep( "ARY1_VBAD_TYPE",
              "Routine ary1Vbad called with an invalid 'type' argument of"
              "'^BADTYPE' (internal programming error).", status );
   }

/* Call error tracing routine and exit. */
   if( *status != SAI__OK ) ary1Trace( "ary1Vbad", status );

}
