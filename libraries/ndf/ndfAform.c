#include <stdlib.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "ndf.h"
#include "mers.h"

void ndfAform_( int indf, const char *comp, int iaxis, char *form,
               size_t form_length, int *status ){
/*
*+
*  Name:
*     ndfAform

*  Purpose:
*     Obtain the storage form of an NDF axis array.

*  Synopsis:
*     void ndfAform( int indf, const char *comp, int iaxis, char *form,
*                    size_t form_length, int *status )

*  Description:
*     This function returns the storage form of a specified NDF axis array
*     component as an upper case character string (e.g. "PRIMITIVE").

*  Parameters:
*     indf
*        NDF identifier.
*     comp
*        Pointer to a null terminated string holding the name of the axis
*        array component whose storage form is required: "CENTRE",
*        "VARIANCE" or "WIDTH".
*     iaxis
*        Number of the NDF axis for which information is required.
*     form
*        Pointer to an array in which to return a null terminated string
*        holding the storage form of the axis array.
*     form_length
*        The length of the supplied 'form' array. This should include
*        room for the terminating null.
*     *status
*        The global status.

*  Notes:
*     -  The symbolic constant NDF__SZFRM may be used for declaring the
*     length of a character variable to hold the storage form of an NDF
*     axis array. This constant is defined in the header file "ndf.h".
*     -  At present, the NDF_ functions only support "primitive" and
*     "simple" arrays, so only the values "PRIMITIVE" and "SIMPLE" can be
*     returned.

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
   NdfACB *acb;          /* Pointer to NDF entry in the ACB */
   int iax1;             /* First axis to process */
   int iax2;             /* Last axis to process (junk) */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Import the NDF identifier. */
   ndf1Impid( indf, &acb, status );

/* Check the axis number for validity. */
   ndf1Van( acb, iaxis, 0, &iax1, &iax2, status );
   if( *status == SAI__OK ) {

/* Test the array name against each valid value in turn, calling the
   appropriate function to obtain the storage form. */

/* CENTRE array:
   ============ */
      if( ndf1Simlr( comp, 1, 0, "CENTRE", NDF__MINAB ) ||
          ndf1Simlr( comp, 1, 0, "CENTER", NDF__MINAB ) ) {
         ndf1Adfrm( iax1, acb, form, form_length, status );

/* VARIANCE array:
   ============== */
      } else if( ndf1Simlr( comp, 1, 0, "VARIANCE", NDF__MINAB ) ) {
         ndf1Avfrm( iax1, acb, form, form_length, status );

/* WIDTH array:
   =========== */
      } else if( ndf1Simlr( comp, 1, 0, "WIDTH", NDF__MINAB ) ) {
         ndf1Awfrm( iax1, acb, form, form_length, status );

/* If the array name was not recognised, then report an error. */
      } else {
         *status = NDF__CNMIN;
         msgSetc( "BADNAME", comp );
         errRep( " ", "Invalid axis array component name '^BADNAME' "
                 "specified (possible programming error).", status );
      }
   }

/* If an error occurred, then report context information and call the
   error tracing function. */
   if( *status != SAI__OK ) {
      errRep( " ", "ndfAform: Error obtaining the storage form of an NDF "
              "axis array.", status );
      ndf1Trace( "ndfAform", status );
   }

/* Restablish the original AST status pointer */
   NDF_FINAL

}

