#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "mers.h"

void ndf1Qityp( int dtype, int itype, int *ok, int *status ){
/*
*+
*  Name:
*     ndf1Qityp

*  Purpose:
*     Query if a data type matches an implemented algorithm.

*  Synopsis:
*     void ndf1Qityp( int dtype, int itype, int *ok, int *status )

*  Description:
*     This function compares a numeric data type with the data type of an
*     algorithm which may be used to process it and returns a logical value
*     indicating if the data can be processed by the algorithm without loss
*     of precision.

*  Parameters:
*     dtype
*        An integer identifying the numeric type of the data to be
*        processed. One of the symbolic integers NDF__TYPx (where x is UB,
*        B, UW, W, I, K, R or D) should be used. These values are defined
*        in the "ndf1.h" include file.
*     itype
*        A similar symbolic integer identifying the data type which the
*        algorithm is designed to process.
*     *ok
*        The value non-zero is returned if there will be no loss of
*        precision in converting the data type identified by "dtype" into
*        that identified by "itype" and then processing the data.
*        Otherwise, the value zero is returned.
*     *status
*        The global status.

*  Notes:
*     -  The implementation of this function depends on the collating
*     sequence of data types established by the symbolic constants used to
*     identify them.

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

/* Compare the "dtype" data type code with each permitted value in turn,
   performing the appropriate test on the "itype" code. */

/* ...unsigned byte. */
   if( dtype == NDF__TYPUB ) {
      *ok = ( ( itype == NDF__TYPUB ) || ( itype >= NDF__TYPUW ) );

/* ...byte. */
   } else if( dtype == NDF__TYPB ) {
      *ok = ( ( itype == NDF__TYPB ) || ( itype >= NDF__TYPW ) );

/* ...unsigned word. */
   } else if( dtype == NDF__TYPUW ) {
      *ok = ( ( itype == NDF__TYPUW ) || ( itype >= NDF__TYPI ) );

/* ...word. */
   } else if( dtype == NDF__TYPW ) {
      *ok = ( itype >= NDF__TYPW );

/* ...integer. */
   } else if( dtype == NDF__TYPI ) {
      *ok = ( itype >= NDF__TYPI );

/* ...64-bit integer. */
   } else if( dtype == NDF__TYPK ) {
      *ok = ( ( itype == NDF__TYPK ) || ( itype >= NDF__TYPD ) );

/* ...real. */
   } else if( dtype == NDF__TYPR ) {
      *ok = ( itype >= NDF__TYPR );

/* ...double precision. */
   } else if( dtype == NDF__TYPD ) {
      *ok = ( itype >= NDF__TYPD );

/* If the "dtype" data type code was not recognised, then report an error. */
   } else {
      *status = NDF__FATIN;
      msgSetc( "ROUTINE", "ndf1Qityp" );
      msgSeti( "BADDTYPE", dtype );
      errRep( " ", "Function ^ROUTINE called with an invalid DTYPE "
              "parameter of ^BADDTYPE (internal programming error).", status );
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Qityp", status );

}

