#include <stdlib.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "prm_par.h"
#include "dat_err.h"
#include "ndf.h"
#include <string.h>
#include "star/util.h"
#include "mers.h"

void ndfAcget_( int indf, const char *comp, int iaxis, char *value,
               size_t value_length, int *status ){
/*
*+
*  Name:
*     ndfAcget

*  Purpose:
*     Obtain the value of an NDF axis character component.

*  Synopsis:
*     void ndfAcget( int indf, const char *comp, int iaxis, char *value,
*                    size_t value_length, int *status )

*  Description:
*     This function obtains the value of the specified axis character
*     component of an NDF (i.e. the value of the LABEL or UNITS component
*     for an NDF axis).

*  Parameters:
*     indf
*        NDF identifier.
*     comp
*        Pointer to a null terminated string holding the name of the axis
*        character component whose value is required: "LABEL" or "UNITS".
*     iaxis
*        Number of the axis for which a value is required.
*     value
*        Pointer to a null terminated string holding the component's value.
*     value_length
*        The length of the supplied 'value' array. This should include
*        room for the terminating null.
*     *status
*        The global status.

*  Notes:
*     -  If the requested axis component is in an undefined state and
*     "value" is set to a blank string on entry, then an appropriate
*     default value will be returned. If "value" is not blank on entry,
*     then it will be returned unchanged.
*     -  If the length of the "value" parameter is too short to accommodate
*     the returned result without losing significant (non-blank) trailing
*     characters, then this will be indicated by an appended ellipsis, i.e.
*     "...". No error will result.

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

*  Copyright:
*     Copyright (C) 2018 East Asian Observatory
*     All rights reserved.

*  History:
*     3-APR-2019 (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/

/* Local Variables: */
   NdfACB *acb;          /* Pointer to the NDF entry in the ACB */
   NdfDCB *dcb;          /* Pointer to data object entry in the DCB */
   char val[ 10 + VAL__SZI ];   /* Default value string */
   hdsdim lbnd[ NDF__MXDIM ];   /* Data object lower bounds */
   hdsdim ubnd[ NDF__MXDIM ];   /* Data object upper bounds */
   int iax1;             /* First (only) axis to process */
   int iax2;             /* Last (only) axis to process */
   int iccomp;           /* Character component identifier */
   int ndim;             /* Number of data object dimensions */
   int there;            /* Whether component exists */
   size_t n;             /* Position of start of ellipsis */
   size_t nc;            /* Default value string length */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Import the NDF identifier. */
   ndf1Impid( indf, &acb, status );

/* Validate the axis character component name. */
   ndf1Vaccn( comp, &iccomp, status );

/* Validate the axis number. */
   ndf1Van( acb, iaxis, 0, &iax1, &iax2, status );
   if( *status == SAI__OK ) {

/* Obtain an index to the data object entry in the DCB. */
      dcb = acb->dcb;

/* If this is an NDF section, then obtain the number of dimensions of
   the actual data object to which it refers from the ARY_ system
   identifier for its data array. */
      there = 1;
      if( acb->cut ) {
         aryBound( dcb->did, NDF__MXDIM, lbnd, ubnd, &ndim, status );

/* Note if the required axis exists in the actual data object. */
         if( *status == SAI__OK ) there = ( iax1 <= ndim );
      }

/* If the required axis exists, then ensure that axis character
   component information is available. */
      if( *status == SAI__OK ) {
         if( there ) {
            ndf1Dac( iax1, iccomp, dcb, status );
            if( *status == SAI__OK ) {

/* Note whether the required character component exists. */
               there = ( dcb->acloc[ iax1 ][ iccomp ] != NULL );
            }
         }
      }

/* If the component (or its axis) does not exist and the "value" parameter
   was blank on input, then create a default value. */
      if( *status == SAI__OK ) {
         if( !there ) {
            if( astChrLen( value ) == 0 ) {

/* The label component defaults to "Axis n". */
               if( iccomp == NDF__ALAB ) {
                  sprintf( val, "Axis %d", iax1 + 1 );

/* The units component defaults to "pixel". */
               } else if( iccomp == NDF__AUNI ) {
                  star_strlcpy( val, "pixel", sizeof( val ) );
                  nc = 5;
               }

/* Return the default value, adding an ellipsis if it is truncated. */
               star_strlcpy( value, val, value_length );
               if( nc > value_length - 1 ) {
                  n = NDF_MAX( 1, value_length - 3 );
                  star_strlcpy( value + n - 1, "...", value_length - n + 1 );
               }
            }

/* If the required component exists, then start a new error context and
   read its value. */
         } else {
            errMark();
            datGet0C( dcb->acloc[ iax1 ][ iccomp ], value,
                      value_length, status );

/* If the value was truncated, then annul the error and add an ellipsis. */
            if( ( *status == DAT__CONER ) || ( *status == DAT__TRUNC ) ) {
               errAnnul( status );
               n = NDF_MAX( 1, value_length - 3 );
               star_strlcpy( value + n - 1, "...", value_length - n + 1 );
            }

/* End the error context. */
            errRlse();
         }
      }
   }

/* If an error occurred, then report context information and call the
   error tracing function. */
   if( *status != SAI__OK ) {
      errRep( " ", "ndfAcget: Error obtaining the value of an NDF axis "
              "character component.", status );
      ndf1Trace( "ndfAcget", status );
   }

/* Restablish the original AST status pointer */
   NDF_FINAL

}
