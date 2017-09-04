#include "sae_par.h"
#include "ary1.h"
#include "mers.h"
#include "ary_err.h"
#include "dat_par.h"
#include <string.h>

void ary1Vftp( const char *ftype, size_t len, char *type, char *cmplx,
               int *status ) {
/*
*+
*  Name:
*     ary1Vftp

*  Purpose:
*     Check a full type specification for validity.

*  Synopsis:
*     void ary1Vftp( const char *ftype, size_t len, char *type, char *cmplx,
*                    int *status )

*  Description:
*     The routine checks that a full type specification is valid and
*     decomposes it into a primitive numeric type string and a logical
*     flag indicating if the full type is complex or not. An error is
*     reported if the full type specification is not valid.

*  Parameters:
*     ftype
*        The full type specification to be validated (case
*        insensitive).
*     len
*        The length of the "type" array.
*     type
*        Returned holding the primitive numeric type implied by FTYPE.
*     cmplx
*        Whether FTYPE specifies complex data or not.
*     status
*        The global status.

*  Notes:
*     -  To be valid, a full type specification must either be a
*     primitive numeric HDS type string, or one of these strings
*     prefixed with the string 'COMPLEX'.

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

/* Test the full type specification against all the permitted values,
   assigning the results accordingly. */

/* ...byte data types. */
   if( !strcasecmp( ftype, "_BYTE" ) ){
      ary1Ccpy( "_BYTE", len, type, status );
      *cmplx = 0;
   } else if( !strcasecmp( ftype, "COMPLEX_BYTE" ) ){
      ary1Ccpy( "_BYTE", len, type, status );
      *cmplx = 1;

/* ...unsigned byte data types. */
   } else if( !strcasecmp( ftype, "_UBYTE" ) ){
      ary1Ccpy( "_UBYTE", len, type, status );
      *cmplx = 0;
   } else if( !strcasecmp( ftype, "COMPLEX_UBYTE" ) ){
      ary1Ccpy( "_UBYTE", len, type, status );
      *cmplx = 1;

/* ...double precision data types. */
   } else if( !strcasecmp( ftype, "_DOUBLE" ) ){
      ary1Ccpy( "_DOUBLE", len, type, status );
      *cmplx = 0;
   } else if( !strcasecmp( ftype, "COMPLEX_DOUBLE" ) ){
      ary1Ccpy( "_DOUBLE", len, type, status );
      *cmplx = 1;

/* ...integer data types. */
   } else if( !strcasecmp( ftype, "_INTEGER" ) ){
      ary1Ccpy( "_INTEGER", len, type, status );
      *cmplx = 0;
   } else if( !strcasecmp( ftype, "COMPLEX_INTEGER" ) ){
      ary1Ccpy( "_INTEGER", len, type, status );
      *cmplx = 1;

/* ...real data types. */
   } else if( !strcasecmp( ftype, "_REAL" ) ){
      ary1Ccpy( "_REAL", len, type, status );
      *cmplx = 0;
   } else if( !strcasecmp( ftype, "COMPLEX_REAL" ) ){
      ary1Ccpy( "_REAL", len, type, status );
      *cmplx = 1;

/* ...word data types. */
   } else if( !strcasecmp( ftype, "_WORD" ) ){
      ary1Ccpy( "_WORD", len, type, status );
      *cmplx = 0;
   } else if( !strcasecmp( ftype, "COMPLEX_WORD" ) ){
      ary1Ccpy( "_WORD", len, type, status );
      *cmplx = 1;

/* ...unsigned word data types. */
   } else if( !strcasecmp( ftype, "_UWORD" ) ){
      ary1Ccpy( "_UWORD", len, type, status );
      *cmplx = 0;
   } else if( !strcasecmp( ftype, "COMPLEX_UWORD" ) ){
      ary1Ccpy( "_UWORD", len, type, status );
      *cmplx = 1;

/* ...64-bit integer data types. */
   } else if( !strcasecmp( ftype, "_INT64" ) ){
      ary1Ccpy( "_INT64", len, type, status );
      *cmplx = 0;
   } else if( !strcasecmp( ftype, "COMPLEX_INT64" ) ){
      ary1Ccpy( "_INT64", len, type, status );
      *cmplx = 1;

/* If the full type specification was not recognised, then report an error. */
   } else {
      *status = ARY__FTPIN;
      msgSetc( "BADFTYPE", ftype );
      errRep( " ", "Invalid full array type '^BADFTYPE' specified (possible"
              "programming error).", status );
   }

/* Call error tracing routine and exit. */
   if( *status != SAI__OK ) ary1Trace( "ary1Vftp", status );

}
