#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "ndf_ast.h"
#include "mers.h"

void ndf1Pstyp( const char *type, int *itype, int *status ){
/*
*+
*  Name:
*     ndf1Pstyp

*  Purpose:
*     Parse a numeric data type.

*  Synopsis:
*     void ndf1Pstyp( const char *type, int *itype, int *status )

*  Description:
*     This function parses a string representing a numeric data type and
*     returns an integer code representing the data type identified. An
*     error is reported if an invalid data type string is supplied.

*  Parameters:
*     type
*        Pointer to a null terminated string holding the data type string
*        to be parsed.
*     *itype
*        Returned holding the type code identifying the data type.  These
*        integers have symbolic names NDF__TYPx (where "x" identifies the
*        data type) and are defined in the "ndf1.h" include file.
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

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Compare the type string with each permitted value in turn, obtaining
   the appropriate integer value for each data type. */

/* ...byte data. */
   if( astChrMatch( type, "_BYTE" ) ) {
      *itype = NDF__TYPB;

/* ...unsigned byte data. */
   } else if( astChrMatch( type, "_UBYTE" ) ) {
      *itype = NDF__TYPUB;

/* ...double precision data. */
   } else if( astChrMatch( type, "_DOUBLE" ) ) {
      *itype = NDF__TYPD;

/* ...integer data. */
   } else if( astChrMatch( type, "_INTEGER" ) ) {
      *itype = NDF__TYPI;

/* ...real data. */
   } else if( astChrMatch( type, "_REAL" ) ) {
      *itype = NDF__TYPR;

/* ...word data. */
   } else if( astChrMatch( type, "_WORD" ) ) {
      *itype = NDF__TYPW;

/* ...unsigned word data. */
   } else if( astChrMatch( type, "_UWORD" ) ) {
      *itype = NDF__TYPUW;

/* ...64-bit integer data. */
   } else if( astChrMatch( type, "_INT64" ) ) {
      *itype = NDF__TYPK;

/* If the data type was not recognised, then report an error. */
   } else {
      *status = NDF__TYPIN;
      msgSetc( "BADTYPE", type );
      errRep( " ", "Invalid numeric type '^BADTYPE' specified (possible "
              "programming error).", status );
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Pstyp", status );

}

