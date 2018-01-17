#include "sae_par.h"
#include "ary1.h"
#include "mers.h"
#include "dat_par.h"

void aryIsacc( Ary *ary, const char access[ARY__SZACC+1], int *isacc,
               int *status ) {
/*
*+
*  Name:
*     aryIsacc

*  Purpose:
*     Determine whether a specified type of array access is available.

*  Synopsis:
*     void aryIsacc( Ary *ary, const char access[ARY__SZACC+1], int *isacc,
*                    int *status )

*  Description:
*     This function determines whether a specified type of access to an
*     array is available, or whether it has been disabled. If access is
*     not available, then any attempt to access the array in this way
*     will fail.

*  Parameters:
*     ary
*        Array identifier.
*     access
*        The type of array access required: 'BOUNDS', 'DELETE',
*        'SHIFT', 'TYPE' or 'WRITE' (see the Notes section for
*        details).
*     isacc
*        Returned holding a flag indicating whether the specified type
*        of access is available.
*     status
*        The global status.

*  Notes:
*     The valid access types control the following operations on the
*     array:
*     -  'BOUNDS' permits the pixel-index bounds of a base array to be
*     altered.
*     -  'DELETE' permits deletion of the array.
*     -  'SHIFT' permits pixel-index shifts to be applied to a base
*     array.
*     -  'TYPE' permits the data type of the array to be altered.
*     -  'WRITE' permits new values to be written to the array, or the
*     array's state to be reset.

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

/* Local variables: */
   AryACB *acb;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Import the array identifier. */
   acb = (AryACB *) ary1Impid( ary, 1, 1, 1, status );

/* Determine whether access is available. */
   ary1Accok( acb, access, isacc, status );

/* If an error occurred, then report context information and call the error
   tracing routine. */
   if( *status != SAI__OK ){
      errRep( " ", "aryIsacc: Error enquiring whether access to an array is"
              " available.", status );
      ary1Trace( "aryIsacc", status );
   }

}
