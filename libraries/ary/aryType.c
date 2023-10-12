#include "sae_par.h"
#include "ary1.h"
#include "mers.h"
#include "dat_par.h"

void aryType( Ary *ary, char type[DAT__SZTYP+1], int *status ) {
/*
*+
*  Name:
*     aryType

*  Purpose:
*     Obtain the numeric type of an array.

*  Synopsis:
*     void aryType( Ary *ary, char type[DAT__SZTYP+1], int *status )

*  Description:
*     This function returns the numeric type of an array as an upper-case
*     character string (e.g. '_REAL').

*  Parameters:
*     ary
*        Array identifier.
*     type
*        Numeric type of the array.
*     status
*        The global status.

*  Notes:
*     -  The symbolic constant DAT__SZTYP should be used for declaring the
*     length of a character array which is to hold the numeric type
*     of an array. This constant is defined in the header file dat_par.h

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

/* Get the DCB index for the data object. */
      dcb = acb->dcb;

/* Ensure that storage form, type and scaling information is available. */
      ary1Dfrm( dcb, status );
      ary1Dtyp( dcb, status );
      ary1Dscl( dcb, status );

/* Copy the numeric type string to the output argument. */
      ary1Extyp( dcb, type, status );

      ARY__DCB_UNLOCK_MUTEX;
   }

/* If an error occurred, then report context information and call the error
   tracing routine. */
   if( *status != SAI__OK ){
      errRep( " ", "aryType: Error obtaining the numeric type of an array.",
              status );
      ary1Trace( "aryType", status );
   }
}
