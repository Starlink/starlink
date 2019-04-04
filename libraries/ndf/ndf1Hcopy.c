#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"

void ndf1Hcopy( HDSLoc *loc1, HDSLoc *loc2, int *status ){
/*
*+
*  Name:
*     ndf1Hcopy

*  Purpose:
*     Copy all the components of one HDS structure into another structure.

*  Synopsis:
*     void ndf1Hcopy( HDSLoc *loc1, HDSLoc *loc2, int *status )

*  Description:
*     This function copies all the components in an HDS structure into
*     another structure preserving their names. Any components in the
*     destination structure are first deleted.

*  Parameters:
*     loc1
*        Locator to input HDS structure.
*     loc2
*        Locator to the HDS structure which is to receive the copied
*        components.
*     *status
*        The global status.

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
   HDSLoc *tloc = NULL;  /* Temporary locator */
   char name[ DAT__SZNAM + 1 ];    /* Component name */
   int i;                /* Component index (one-based) */
   int ncomp;            /* Number of components */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Empty the destination structure. */
   datNcomp( loc2, &ncomp, status );
   for( i = 0; i < ncomp; i++ ){
      datIndex( loc2, i + 1, &tloc, status );
      datName( tloc, name, status );
      datAnnul( &tloc, status );
      datErase( loc2, name, status );
   }

/* Loop over all source components. */
   datNcomp( loc1, &ncomp, status );
   for( i = 1; i <= ncomp; i++ ){

/* Get the component's name. */
      datIndex( loc1, i, &tloc, status );
      datName( tloc, name, status );

/* Copy it into the destination structure. */
      datCopy( tloc, loc2, name, status );
      datAnnul( &tloc, status );
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Hcopy", status );

}

