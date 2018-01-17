#include "sae_par.h"
#include "ary1.h"
#include "mers.h"
#include "dat_par.h"
#include <string.h>

void aryFtype( Ary *ary,  char ftype[ARY__SZFTP+1], int *status ) {
/*
*+
*  Name:
*     aryFtype

*  Purpose:
*     Obtain the full data type of an array.

*  Synopsis:
*     void aryFtype( Ary *ary,  char ftype[ARY__SZFTP+1], int *status )

*  Description:
*     This function returns the full data type of an array as an
*     upper-case character string (e.g. '_REAL' or 'COMPLEX_BYTE').

*  Parameters:
*     ary
*        Array identifier.
*     ftype
*        Returned holding the full data type of the array.
*     status
*        The global status.

*  Notes:
*     -  The symbolic constant ARY__SZFTP should be used for declaring the
*     length of a character variable to hold the full data type of an
*     array. This constant is defined in the header file ary.h.
*     - For "Scaled" arrays, the data type returned by this function is
*     the data type of the SCALE and ZERO terms, rather than the data
*     type of the stored array.

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
   char ty[DAT__SZTYP+1];

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Inport the array identifier. */
   acb = (AryACB *) ary1Impid( ary, 1, 1, 1, status );
   if( *status == SAI__OK ){

/* Get the DCB index for the data object. */
      dcb = acb->dcb;

/* Ensure that storage form, data type and scale information is available. */
      ary1Dfrm( dcb, status );
      ary1Dtyp( dcb, status );
      ary1Dscl( dcb, status );

/* For scaled arrays, return the data type of the scale and zero terms. For
   other storage forms, return the data type of the DCB entry. */
      ary1Extyp( dcb, ty, status );

/* Construct the full data type string and copy it to the output argument. */
      if( dcb->complex ){
         strcpy( ftype, "COMPLEX" );
         strcpy( ftype + 7, ty );
      } else {
         strcpy( ftype, ty );
      }
   }

/* If an error occurred, then report context information and call the error
   tracing routine. */
   if( *status != SAI__OK ){
      errRep( " ", "aryFtype: Error obtaining the full data type of an array.",
              status );
      ary1Trace( "aryFtype", status );
   }

}
