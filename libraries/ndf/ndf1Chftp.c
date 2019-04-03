#include <stdlib.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf_err.h"
#include "ndf1.h"
#include "ndf_ast.h"
#include "mers.h"

void ndf1Chftp( const char *ftype, char *type, size_t type_length,
                int *cmplx, int *status ){
/*
*+
*  Name:
*     ndf1Chftp

*  Purpose:
*     Check a full type specification for validity.

*  Synopsis:
*     void ndf1Chftp( const char *ftype, char *type, size_t type_length,
*                     int *cmplx, int *status )

*  Description:
*     This function checks that a full type specification is valid and
*     decomposes it into a primitive numeric type string and a logical flag
*     indicating if the full type is complex or not. An error is reported
*     if the full type specification is not valid.

*  Parameters:
*     ftype
*        Pointer to a null terminated string holding the full type
*        specification to be validated (case insensitive).
*     type
*        Pointer to an array in which to return a null terminated string
*        holding the primitive numeric type implied by "ftype".
*     type_length
*        The length of the supplied 'type' array. This should include
*        room for the terminating null.
*     *cmplx
*        Returned holding the whether "ftype" specifies complex data or
*        not.
*     *status
*        The global status.

*  Notes:
*     -  To be valid, a full type specification must either be a primitive
*     numeric HDS type string, or one of these strings prefixed with the
*     string "COMPLEX".

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

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Test the full type specification against all the permitted values,
   assigning the results accordingly. */

/* ...byte data types. */
   if( astChrMatch( ftype, "_BYTE" ) ) {
      ndf1Ccpy( "_BYTE", type, type_length, status );
      *cmplx = 0;
   } else if( astChrMatch( ftype, "COMPLEX_BYTE" ) ) {
      ndf1Ccpy( "_BYTE", type, type_length, status );
      *cmplx = 1;

/* ...unsigned byte data types. */
   } else if( astChrMatch( ftype, "_UBYTE" ) ) {
      ndf1Ccpy( "_UBYTE", type, type_length, status );
      *cmplx = 0;
   } else if( astChrMatch( ftype, "COMPLEX_UBYTE" ) ) {
      ndf1Ccpy( "_UBYTE", type, type_length, status );
      *cmplx = 1;

/* ...double precision data types. */
   } else if( astChrMatch( ftype, "_DOUBLE" ) ) {
      ndf1Ccpy( "_DOUBLE", type, type_length, status );
      *cmplx = 0;
   } else if( astChrMatch( ftype, "COMPLEX_DOUBLE" ) ) {
      ndf1Ccpy( "_DOUBLE", type, type_length, status );
      *cmplx = 1;

/* ...integer data types. */
   } else if( astChrMatch( ftype, "_INTEGER" ) ) {
      ndf1Ccpy( "_INTEGER", type, type_length, status );
      *cmplx = 0;
   } else if( astChrMatch( ftype, "COMPLEX_INTEGER" ) ) {
      ndf1Ccpy( "_INTEGER", type, type_length, status );
      *cmplx = 1;

/* ...real data types. */
   } else if( astChrMatch( ftype, "_REAL" ) ) {
      ndf1Ccpy( "_REAL", type, type_length, status );
      *cmplx = 0;
   } else if( astChrMatch( ftype, "COMPLEX_REAL" ) ) {
      ndf1Ccpy( "_REAL", type, type_length, status );
      *cmplx = 1;

/* ...word data types. */
   } else if( astChrMatch( ftype, "_WORD" ) ) {
      ndf1Ccpy( "_WORD", type, type_length, status );
      *cmplx = 0;
   } else if( astChrMatch( ftype, "COMPLEX_WORD" ) ) {
      ndf1Ccpy( "_WORD", type, type_length, status );
      *cmplx = 1;

/* ...unsigned word data types. */
   } else if( astChrMatch( ftype, "_UWORD" ) ) {
      ndf1Ccpy( "_UWORD", type, type_length, status );
      *cmplx = 0;
   } else if( astChrMatch( ftype, "COMPLEX_UWORD" ) ) {
      ndf1Ccpy( "_UWORD", type, type_length, status );
      *cmplx = 1;

/* ...64-bit integer data types. */
   } else if( astChrMatch( ftype, "_INT64" ) ) {
      ndf1Ccpy( "_INT64", type, type_length, status );
      *cmplx = 0;
   } else if( astChrMatch( ftype, "COMPLEX_INT64" ) ) {
      ndf1Ccpy( "_INT64", type, type_length, status );
      *cmplx = 1;

/* If the full type specification was not recognised, then report an
   error. */
   } else {
      *status = NDF__FTPIN;
      msgSetc( "BADFTYPE", ftype );
      errRep( " ", "Invalid full data type '^BADFTYPE' specified (possible "
              "programming error).", status );
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Chftp", status );

}

