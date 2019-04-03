#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"

void ndf1Mxtyp( int itype1, int itype2, int *itype, int *status ){
/*
*+
*  Name:
*     ndf1Mxtyp

*  Purpose:
*     Maximise two numeric data type codes.

*  Synopsis:
*     void ndf1Mxtyp( int itype1, int itype2, int *itype, int *status )

*  Description:
*     This function compares two integer codes representing numeric data
*     types and "maximises" them. It returns a new type code representing
*     the numeric data type with the lowest precision such that data stored
*     using either of the input data types can be converted to the new type
*     without loss of precision.

*  Parameters:
*     itype1
*        First numeric type code.
*     itype2
*        Second numeric type code.
*     *itype
*        Returned holding the "maximised" type code.
*     *status
*        The global status.

*  Notes:
*     -  The type codes which this function uses are symbolic constants
*     with names of the form NDF__TYPx (where x indicates the numeric
*     type). These constants are defined in the header file "ndf1.h".
*     -  The implementation of this function depends on the collating
*     sequence for numeric data types established by the definitions of
*     these type codes.

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
   int i;                /* Loop counter for refining the result */
   int istart;           /* First guess at the result */
   int ok1;              /* Loss of precision for type 1? */
   int ok2;              /* Loss of precision for type 2? */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Obtain the numerical maximum value of the two input type codes,
   constraining it to lie within range. */
   istart = NDF_MIN( NDF_MAX( NDF__TYPUB, NDF_MAX( itype1, itype2 ) ),
                     NDF__MXTYP );

/* Loop to increase this value until conversion from both input data
   types does not lose precision. */
   for( i = istart - 1; i < NDF__MXTYP; i++ ){

/* Test both conversions to see if precision is lost. */
      ndf1Qityp( itype1, i + 1, &ok1, status );
      ndf1Qityp( itype2, i + 1, &ok2, status );

/* Check for errors. */
      if( *status != SAI__OK ) {
         break;

/* If no precision is lost, then set the result. */
      } else if( ok1 && ok2 ) {
         *itype = i + 1;
         goto L2;
      }
   }
L2:

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Mxtyp", status );

}

