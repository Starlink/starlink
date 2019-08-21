#include <stdlib.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"

void ndf1Hrst( HDSLoc *loc, int *status ){
/*
*+
*  Name:
*     ndf1Hrst

*  Purpose:
*     Reset an HDS primitive or structure.

*  Synopsis:
*     void ndf1Hrst( HDSLoc *loc, int *status )

*  Description:
*     This function resets an HDS primitive object or structure. If a
*     structure is supplied, then "resetting" amounts to erasing all its
*     components. If a structure array is supplied, then all components in
*     all of its elements are erased.

*  Parameters:
*     loc
*        Locator to HDS object.
*     *status
*        The global status.

*  Notes:
*     -  If a structure array is supplied, then it should be capable of
*     being vectorised using "datVec".

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
   HDSLoc *loccel = NULL;/* Locator to cell */
   HDSLoc *loccmp = NULL;/* Locator to cell component */
   HDSLoc *locvec = NULL;/* Locator to vectorised object */
   char name[ DAT__SZNAM + 1 ];    /* Component name */
   hdsbool_t prim;       /* Is object primitive? */
   hdsdim cell;          /* Cell subscript array */
   int i;                /* Loop counter for cell components */
   int icell;            /* Loop counter for array cells */
   int ncomp;            /* Number of cell components */
   size_t el;            /* Number of array elements */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Determine if the object is primitive. */
   datPrim( loc, &prim, status );
   if( *status == SAI__OK ) {

/* If so, then reset its value(s). */
      if( prim ) {
         datReset( loc, status );

/* If not primitive, then vectorise the object (it may be a structure
   array) and determine how many elements it has. */
      } else {
         datVec( loc, &locvec, status );
         datSize( locvec, &el, status );

/* Loop to empty each element in the structure (array). Obtain a locator
   to each cell in turn. */
         if( *status == SAI__OK ) {
            for( icell = 0; icell < el; icell++ ){
               cell = icell + 1;
               datCell( locvec, 1, &cell, &loccel, status );

/* Determine how many components the structure cell has. */
               datNcomp( loccel, &ncomp, status );
               if( *status == SAI__OK ) {

/* Loop to erase each component. Obtain a locator to the first component
   and determine its name. */
                  for( i = 0; i < ncomp; i++ ){
                     datIndex( loccel, 1, &loccmp, status );
                     datName( loccmp, name, status );

/* Annul the locator and erase the component. */
                     datAnnul( &loccmp, status );
                     datErase( loccel, name, status );
                  }
               }

/* Annul the locator to the structure (array) cell. */
               datAnnul( &loccel, status );
            }
         }

/* Annul the locator to the vectorised object. */
         datAnnul( &locvec, status );
      }
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Hrst", status );

}

