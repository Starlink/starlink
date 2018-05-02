#include <string.h>
#include "sae_par.h"
#include "ary1.h"
#include "mers.h"
#include "ary_err.h"

void ary1Vzero( const char *type, size_t n, void *pntr, int *status ) {
/*
*+
*  Name:
*     ary1Vzero

*  Purpose:
*     Set all elements of a vectorised array to zero.

*  Synopsis:
*     void ary1Vzero( const char *type, size_t n, void *pntr, int *status )

*  Description:
*     The routine sets all elements of a vectorised array, of any
*     numeric data type, to zero.

*  Parameters:
*     type
*        An HDS primitive numeric type string specifying the data type
*        of the array (case insensitive).
*     n
*        Number of array elements to be set to zero.
*     pntr
*        Pointer to the array whose elements are to be set to zero.
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

/* Test the data type against each valid value in turn, calling the
   appropriate routine to set the array elements to zero. */
   if( !strcasecmp( type, "_BYTE" ) ){
      ary1ZerB( n, pntr, status );

   } else if( !strcasecmp( type, "_UBYTE" ) ){
      ary1ZerUB( n, pntr, status );

   } else if( !strcasecmp( type, "_DOUBLE" ) ){
      ary1ZerD( n, pntr, status );

   } else if( !strcasecmp( type, "_INTEGER" ) ){
      ary1ZerI( n, pntr, status );

   } else if( !strcasecmp( type, "_REAL" ) ){
      ary1ZerF( n, pntr, status );

   } else if( !strcasecmp( type, "_WORD" ) ){
      ary1ZerW( n, pntr, status );

   } else if( !strcasecmp( type, "_UWORD" ) ){
      ary1ZerUW( n, pntr, status );

   } else if( !strcasecmp( type, "_INT64" ) ){
      ary1ZerK( n, pntr, status );

/* If the data type supplied was not valid, then report an error. */
   } else {
      *status = ARY__FATIN;
      msgSetc( "BADTYPE", type );
      errRep( " ", "Routine ary1Vzero called with an invalid TYPE argument of "
              "'^BADTYPE' (internal programming error).", status );
   }

/* Call error tracing routine and exit. */
   if( *status != SAI__OK ) ary1Trace( "ary1Vzero", status );

}
