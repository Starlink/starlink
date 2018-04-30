#include "sae_par.h"
#include "ary1.h"
#include "mers.h"
#include "ary_err.h"
#include "dat_par.h"
#include <string.h>

void ary1Vtyp( const char *type, char vtype[DAT__SZTYP + 1], int *status ) {
/*
*+
*  Name:
*     ary1Vtyp

*  Purpose:
*     Validate a type specification string.

*  Synopsis:
*     void ary1Vtyp( const char *type, char vtype[DAT__SZTYP + 1], int *status )

*  Description:
*     This function checks a data type specification string for validity.
*     To be valid it must specify one of the HDS primitive numeric data
*     types. An error is reported if the string supplied is not valid.

*  Parameters:
*     type
*        The data type specification to be checked (case insensitive).
*     vtype
*        If valid, this argument returns the data type string in upper
*        case.
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

/* Check the data type string supplied against each permitted value in
   turn, setting the returned argument accordingly. */

/* ...byte data type. */
   if( !strcasecmp( type, "_BYTE" ) ){
      ary1Ccpy( "_BYTE", DAT__SZTYP + 1, vtype, status );

/* ...unsigned byte data type. */
   } else if( !strcasecmp( type, "_UBYTE" ) ){
      ary1Ccpy( "_UBYTE", DAT__SZTYP + 1, vtype, status );

/* ...double precision data type. */
   } else if( !strcasecmp( type, "_DOUBLE" ) ){
      ary1Ccpy( "_DOUBLE", DAT__SZTYP + 1, vtype, status );

/* ...integer data type. */
   } else if( !strcasecmp( type, "_INTEGER" ) ){
      ary1Ccpy( "_INTEGER", DAT__SZTYP + 1, vtype, status );

/* ...real data type. */
   } else if( !strcasecmp( type, "_REAL" ) ){
      ary1Ccpy( "_REAL", DAT__SZTYP + 1, vtype, status );

/* ...word data type. */
   } else if( !strcasecmp( type, "_WORD" ) ){
      ary1Ccpy( "_WORD", DAT__SZTYP + 1, vtype, status );

/* ...unsigned word data type. */
   } else if( !strcasecmp( type, "_UWORD" ) ){
      ary1Ccpy( "_UWORD", DAT__SZTYP + 1, vtype, status );

/* ...64-bit integer data type. */
   } else if( !strcasecmp( type, "_INT64" ) ){
      ary1Ccpy( "_INT64", DAT__SZTYP + 1, vtype, status );

/* If the data type string is invalid, then report an error. */
   } else {
      *status = ARY__TYPIN;
      msgSetc( "BADTYPE", type );
      errRep( " ", "Invalid array data type '^BADTYPE' specified (possible "
              "programming error).", status );
   }

/* Call error tracing routine and exit. */
   if( *status != SAI__OK ) ary1Trace( "ary1Vtyp", status );

}
