#include <stdlib.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf.h"
#include "mers.h"

void ndfClen_( int indf, const char *comp, size_t *length, int *status ){
/*
*+
*  Name:
*     ndfClen

*  Purpose:
*     Determine the length of an NDF character component.

*  Synopsis:
*     void ndfClen( int indf, const char *comp, size_t *length, int *status )

*  Description:
*     This function returns the length of the specified character component
*     of an NDF (i.e. the number of characters in the LABEL, TITLE or UNITS
*     component).

*  Parameters:
*     indf
*        NDF identifier.
*     comp
*        Pointer to a null terminated string holding the name of the
*        character component whose length is required: "LABEL", "TITLE" or
*        "UNITS".
*     *length
*        Returned holding the length of the component in characters.
*     *status
*        The global status.

*  Notes:
*     -  The length of an NDF character component is determined by the
*     length of the VALUE string assigned to it by a previous call to
*     ndfCput (note that this could include trailing blanks).
*     -  If the specified component is in an undefined state, then a length
*     of zero will be returned.

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
   NdfACB *acb;          /* Pointer to NDF entry in the DCB */
   NdfDCB *dcb;          /* Pointer to data object entry in the DCB */
   int iccomp;           /* Identifier for character component */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Import the NDF identifier. */
   ndf1Impid( indf, &acb, status );

/* Validate the component name. */
   ndf1Vccn( comp, &iccomp, status );
   if( *status == SAI__OK ) {

/* Obtain an index to the data object entry in the DCB. */
      dcb = acb->dcb;

/* Ensure that information about the required character component is
   available in the DCB. */
      ndf1Dc( dcb, iccomp, status );
      if( *status == SAI__OK ) {

/* If the component is not present in the NDF, then return a length of
   zero. */
         if( !dcb->cloc[ iccomp ] ) {
            *length = 0;

/* If it is present, then determine its length. */
         } else {
            datLen( dcb->cloc[ iccomp ], length, status );
         }
      }
   }

/* If an error occurred, then report context information and call the
   error tracing function. */
   if( *status != SAI__OK ) {
      errRep( " ", "ndfClen: Error determining the length of an NDF "
              "character component.", status );
      ndf1Trace( "ndfClen", status );
   }

/* Restablish the original AST status pointer */
   NDF_FINAL

}

