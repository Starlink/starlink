#include <stdlib.h>
#include <math.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf.h"
#include "mers.h"

void ndfAclen_( int indf, const char *comp, int iaxis, size_t *length,
                int *status ){
/*
*+
*  Name:
*     ndfAclen

*  Purpose:
*     Determine the length of an NDF axis character component.

*  Synopsis:
*     void ndfAclen( int indf, const char *comp, int iaxis, size_t *length,
*                    int *status )

*  Description:
*     This function returns the length of the specified axis character
*     component of an NDF (i.e. the number of characters in the LABEL or
*     UNITS component of an NDF axis).

*  Parameters:
*     indf
*        NDF identifier.
*     comp
*        Pointer to a null terminated string holding the name of the axis
*        character component whose length is required: "LABEL" or "UNITS".
*     iaxis
*        Number of the NDF axis.
*     *length
*        Returned holding the component's length in characters.
*     *status
*        The global status.

*  Notes:
*     -  The length of an NDF axis character component is normally
*     determined by the length of the VALUE string assigned to it by a
*     previous call to ndfAcput (note that this could include trailing
*     blanks).
*     -  If the requested axis component is in an undefined state, then the
*     length returned will be the number of characters in the default value
*     which would be returned by the ndfAcget function.
*     -  A value of zero may be supplied for the "iaxis" parameter, in
*     which case the function will return the maximum component length for
*     all the NDF axes.

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

/* Local Variables: */
   NdfACB *acb;          /* Pointer to the NDF entry in the ACB */
   NdfDCB *dcb;          /* Pointer to data object entry in the DCB */
   hdsdim lbnd[ NDF__MXDIM ];      /* Data object lower bounds */
   hdsdim ubnd[ NDF__MXDIM ];      /* Data object upper bounds */
   int iax;              /* Loop counter for axes */
   int iax1;             /* First axis to process */
   int iax2;             /* Last axis to process */
   int iccomp;           /* Character component identifier */
   int ndim;             /* Number of data object dimensions */
   int there;            /* Whether component exists */
   size_t l;             /* Individal component length */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Import the NDF identifier. */
   ndf1Impid( indf, &acb, status );

/* Validate the axis character component name. */
   ndf1Vaccn( comp, &iccomp, status );

/* Validate the axis number. */
   ndf1Van( acb, iaxis, 1, &iax1, &iax2, status );
   if( *status == SAI__OK ) {

/* Obtain an index to the data object entry in the DCB. */
      dcb = acb->dcb;

/* Obtain the number of dimensions of the actual data object from the
   ARY_ system identifier for its data array. */
      aryBound( dcb->did, NDF__MXDIM, lbnd, ubnd, &ndim, status );

/* Loop to process each axis required. */
      if( *status == SAI__OK ) {
         *length = 0;
         for( iax = iax1; iax <= iax2; iax++ ){

/* See if the current axis exists within the actual data object. If so,
   then ensure that axis character component information is available
   for this axis. */
            there = ( iax <= ndim );
            if( there ) {
               ndf1Dac( iax, iccomp, dcb, status );

/* Note if the required component exists for this axis. */
               if( *status == SAI__OK ) there = ( dcb->acloc[ iax ][ iccomp ]
                                                  != NULL );
            }

/* If the component does not exist, then generate the length of the
   default component value (for the label component, this requires
   calculation of the number of decimal digits needed to store the axis
   number). */
            if( *status == SAI__OK ) {
               if( !there ) {

/* Label component... */
                  if( iccomp == NDF__ALAB ) {
                     l = (int)( log10( (float) iax + 0.5 ) ) + 6;

/* Units component... */
                  } else if( iccomp == NDF__AUNI ) {
                     l = 5;
                  }

/* If the required component exists, then obtain its length. */
               } else {
                  datLen( dcb->acloc[ iax ][ iccomp ], &l, status );
               }
            }

/* Quit considering axes if an error occurs. */
            if( *status != SAI__OK ) break;

/* Find the maximum length for all the axes considered. */
            *length = NDF_MAX( *length, l );
         }
      }
   }

/* If an error occurred, then report context information and call the
   error tracing function. */
   if( *status != SAI__OK ) {
      errRep( " ", "ndfAclen: Error determining the length of an NDF axis "
              "character component.", status );
      ndf1Trace( "ndfAclen", status );
   }

/* Restablish the original AST status pointer */
   NDF_FINAL

}

