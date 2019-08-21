#include <stdlib.h>
#include <string.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf.h"
#include "mers.h"

void ndfAcput_( const char *value, int indf, const char *comp, int iaxis,
               int *status ){
/*
*+
*  Name:
*     ndfAcput

*  Purpose:
*     Assign a value to an NDF axis character component.

*  Synopsis:
*     void ndfAcput( const char *value, int indf, const char *comp,
*                    int iaxis, int *status )

*  Description:
*     This function assigns a value to the specified axis character
*     component of an NDF (i.e. to the LABEL or UNITS component of an NDF
*     axis).

*  Parameters:
*     value
*        Pointer to a null terminated string holding the value to be
*        assigned.
*     indf
*        NDF identifier.
*     comp
*        Pointer to a null terminated string holding the name of the axis
*        character component whose value is to be assigned: "LABEL" or
*        "UNITS".
*     iaxis
*        Number of the axis to receive the new value.
*     *status
*        The global status.

*  Notes:
*     -  The entire "value" string (including trailing blanks if present)
*     is assigned to the specified axis component, whose length is adjusted
*     to accommodate it.
*     -  A value of zero may be given for the "iaxis" parameter, in which
*     case the function will assign the same value to all the NDF axes.
*     -  This function may only be used to assign values to the axes of a
*     base NDF. If an NDF section is supplied, then it it will return
*     without action. No error will result.

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
   NdfACB *acb;          /* Pointer to NDF entry in the ACB */
   NdfDCB *dcb;          /* Pointer to data object entry in the DCB */
   int iax;              /* Loop counter for axes */
   int iax1;             /* First axis to process */
   int iax2;             /* Last axis to process */
   int iccomp;           /* Axis character component identifier */
   size_t l;             /* Character component length */

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

/* Check that write access to the NDF is available. */
   ndf1Chacc( acb, "WRITE", status );
   if( *status == SAI__OK ) {

/* Check that this is a base NDF. Return without action if it is not. */
      if( !acb->cut ) {

/* Obtain an index to the data object entry in the DCB. */
         dcb = acb->dcb;

/* Ensure that an axis structure exists. */
         ndf1Acre( dcb, status );
         if( *status == SAI__OK ) {

/* Loop to assign character values to each specified axis. */
            for( iax = iax1; iax <= iax2; iax++ ){

/* Ensure that axis character component information is available. */
               ndf1Dac( iax, iccomp, dcb, status );
               if( *status == SAI__OK ) {

/* If the required component already exists, then determine its length. */
                  if( dcb->acloc[ iax ][ iccomp ] ) {
                     datLen( dcb->acloc[ iax ][ iccomp ], &l, status );
                     if( *status == SAI__OK ) {

/* If the length does not match that of the value to be assigned, then
   annul the component's locator and erase the component. */
                        if( l != strlen( value ) ) {
                           datAnnul( dcb->acloc[ iax ] + iccomp, status );
                           datErase( dcb->aloc[ iax ],
                                     Ndf_DCB_accn[ iccomp ], status );
                        }
                     }
                  }

/* If the required component does not (now) exist, then create one of
   the correct length. */
                  if( *status == SAI__OK ) {
                     if( !dcb->acloc[ iax ][ iccomp ] ) {
                        datNew0C( dcb->aloc[ iax ],
                                  Ndf_DCB_accn[ iccomp ],
                                  strlen( value ), status );

/* Obtain a locator to the component for storage in the DCB. */
                        datFind( dcb->aloc[ iax ],
                                 Ndf_DCB_accn[ iccomp ],
                                 dcb->acloc[ iax ] + iccomp, status );
                     }

/* Assign the new component value. */
                     datPut0C( dcb->acloc[ iax ][ iccomp ], value, status );
                  }
               }

/* Quit processing axes if an error occurs. */
               if( *status != SAI__OK ) break;
            }
         }
      }
   }

/* If an error occurred, then report context information and call the
   error tracing function. */
   if( *status != SAI__OK ) {
      errRep( " ", "ndfAcput: Error assigning a new value to an NDF axis "
              "character component.", status );
      ndf1Trace( "ndfAcput", status );
   }

/* Restablish the original AST status pointer */
   NDF_FINAL

}
