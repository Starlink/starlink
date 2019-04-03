#include <stdlib.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf.h"
#include "mers.h"

void ndfAmap_( int indf, const char *comp, int iaxis, const char *type,
              const char *mmod, void *pntr[], size_t *el, int *status ){
/*
*+
*  Name:
*     ndfAmap

*  Purpose:
*     Obtain mapped access to an NDF axis array.

*  Synopsis:
*     void ndfAmap( int indf, const char *comp, int iaxis,
*                    const char *type, const char *mmod, void *pntr[],
*                    size_t *el, int *status )

*  Description:
*     This function obtains mapped access to an NDF axis array, returning a
*     pointer to the mapped values and a count of the number of elements
*     mapped.

*  Parameters:
*     indf
*        NDF identifier.
*     comp
*        Pointer to a null terminated string holding the name of the axis
*        array component to be mapped: "CENTRE", "VARIANCE" (or "ERROR") or
*        "WIDTH".
*     iaxis
*        Number of the NDF axis whose array is to be mapped.
*     type
*        Pointer to a null terminated string holding the numeric type to be
*        used for access (e.g. "_REAL").
*     mmod
*        Pointer to a null terminated string holding the mapping mode for
*        access to the array: "READ", "UPDATE" or "WRITE".
*     pntr
*        Returned holding the pointer(s) to the mapped values (see the
*        Notes section).
*     *el
*        Returned holding the number of elements mapped.
*     *status
*        The global status.

*  Notes:
*     -  A comma-separated list of axis array component names may also be
*     given, in which case the function will map all the requested axis
*     arrays using the same numeric type and mapping mode. Pointers to the
*     values of these mapped arrays will be returned (in the specified
*     order) in the elements of the array "pntr", which must be of
*     sufficient size to accommodate them.

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
   NdfACB *acb;          /* Pointer to the NDF entry in the ACB */

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Check inherited global status. */
   if( *status == SAI__OK ) {

/* Import the NDF identifier. */
      ndf1Impid( indf, &acb, status );

/* Map the axis array. */
      ndf1Amap( iaxis, acb, comp, type, mmod, pntr, el, status );

/* If an error occurred, then report context information and call the
   error tracing function. */
      if( *status != SAI__OK ) {
         errRep( " ", "ndfAmap: Error obtaining mapped access to an NDF "
                 "axis array.", status );
         ndf1Trace( "ndfAmap", status );
      }
   }

/* Return a "safe" value for "el" under error conditions. */
   if( *status != SAI__OK ) *el = 1;

/* Restablish the original AST status pointer */
   NDF_FINAL

}

