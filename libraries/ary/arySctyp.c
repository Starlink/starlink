#include "sae_par.h"
#include "ary1.h"
#include "mers.h"
#include "dat_par.h"
#include <string.h>

void arySctyp( Ary *ary, char type[DAT__SZTYP+1], int *status ) {
/*
*+
*  Name:
*     arySctyp

*  Purpose:
*     Obtain the numeric type of a scaled array.

*  Synopsis:
*     void arySctyp( Ary *ary, char type[DAT__SZTYP+1], int *status )

*  Description:
*     This function returns the numeric type of a scaled array as an
*     upper-case character string (e.g. '_REAL'). The returned type
*     describes the values stored in the array, before they are unscaled
*     using the associated scale and zero values. Use aryType if you
*     need the data type of the array after it has been unscaled.

*  Parameters:
*     ary
*        Array identifier.
*     type
*        Numeric type of the array.
*     status
*        The global status.

*  Notes:
*     -  If the array is not stored in SCALED form, then this routine
*     returns the same type as the aryType function.
*     -  The symbolic constant DAT__SZTYP should be used to declare the
*     length of a character variable which is to hold the numeric type
*     of an array. This constant is defined in the header file
*     dat_par.h.

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
   AryDCB *dcb;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Import the array identifier. */
   acb = (AryACB *) ary1Impid( ary, 1, 1, 1, status );
   if( *status == SAI__OK ){
      ARY__DCB_LOCK_MUTEX;

/* Get the DCB pointer. */
      dcb = acb->dcb;

/* Ensure that type information is available. */
      ary1Dtyp( dcb, status );

/* Copy the numeric type string to the output argument. */
      strcpy( type, dcb->type );

      ARY__DCB_UNLOCK_MUTEX;
   }

/* If an error occurred, then report context information and call the error
   tracing routine. */
   if( *status != SAI__OK ){
      errRep( " ", "arySctyp: Error obtaining the numeric type of a scaled"
              " array.", status );
      ary1Trace( "arySctyp", status );
   }

}
