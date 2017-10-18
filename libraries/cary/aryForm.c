#include "sae_par.h"
#include "ary1.h"
#include "mers.h"
#include "dat_par.h"
#include <string.h>

void aryForm( Ary *ary, char form[ARY__SZFRM+1], int *status ) {
/*
*+
*  Name:
*     aryForm

*  Purpose:
*     Obtain the storage form of an array.

*  Synopsis:
*     void aryForm( Ary *ary, char form[ARY__SZFRM+1], int *status )

*  Description:
*     This function returns the storage form of an array as an upper-case
*     character string (e.g. 'SIMPLE').

*  Parameters:
*     ary
*        Array identifier.
*     form
*        Returned holding the storage form of the array.
*     status
*        The global status.

*  Notes:
*     -  The symbolic constant ARY__SZFRM should be used for declaring the
*     length of a character variable to hold the storage form of an
*     array. This constant is defined in the header file ary.h.
*     -  At present, the ARY_ routines only support "primitive", "scaled",
*     "simple" and "delta" arrays, so only the values 'PRIMITIVE', 'SCALED'
*     'DELTA' and 'SIMPLE' can be returned.

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

/* Inport the array identifier. */
   acb = (AryACB *) ary1Impid( ary, 1, 1, 1, status );
   if( *status == SAI__OK ){

/* Get the DCB index for the data object. */
      dcb = acb->dcb;

/* Ensure that form information is available. */
      ary1Dfrm( dcb, status );

/* Copy the form string to the output argument. */
      if( *status == SAI__OK ) strcpy( form, dcb->form );
   }

/* If an error occurred, then report context information and call the error
   tracing routine. */
   if( *status != SAI__OK ){
      errRep( " ", "aryform: Error determining the storage form of an array.",
              status );
      ary1Trace( "aryForm", status );
   }

}
