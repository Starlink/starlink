#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "mers.h"
#include <string.h>
#include "star/util.h"

void ndf1Vsftp( const char *ftype, NdfACB *acb, int *status ){
/*
*+
*  Name:
*     ndf1Vsftp

*  Purpose:
*     Set a new full data type for the variance component of an NDF.

*  Synopsis:
*     void ndf1Vsftp( const char *ftype, NdfACB *acb, int *status )

*  Description:
*     This function sets a new full data type for the variance component of
*     an NDF. If the component is defined, then its data values will be
*     converted as necessary. The NDF is identified by its ACB index.

*  Parameters:
*     ftype
*        Pointer to a null terminated string holding the new full data
*        type.
*     acb
*        Pointer to the NDF's ACB entry.
*     *status
*        The global status.

*  Notes:
*     -  The data type of the variance component of an NDF cannot be
*     changed with this function while mapped access to any part of it is
*     in effect.
*     -  The data type of the variance component of an NDF section cannot
*     be changed with this function. If the NDF specified is not a base
*     NDF, then it will return without action.

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
   NdfDCB *dcb;          /* Pointer to data object entry in the DCB */
   int valid;            /* Variance array identifier valid? */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure that variance information is available in the DCB and ACB. */
   ndf1Vimp( acb, status );
   if( *status == SAI__OK ) {

/* Obtain an index to the data object entry in the DCB. */
      dcb = acb->dcb;

/* Check that the variance component is not mapped for access through
   the current ACB entry. Report an error if it is. */
      if( acb->vmap ) {
         *status = NDF__ISMAP;
         ndf1Amsg( "NDF", acb );
         errRep( " ", "The variance component in the NDF structure ^NDF is "
                 "already mapped for access through the specified "
                 "identifier (possible programming error).", status );

/* Only take further action if the NDF is a base NDF. Check that the
   variance component is not mapped at all. Report an error if it is. */
      } else if( !acb->cut ) {
         if( dcb->nvmap != 0 ) {
            *status = NDF__ISMAP;
            ndf1Dmsg( "NDF", dcb );
            errRep( " ", "The variance component in the NDF structure ^NDF "
                    "is already mapped for access through another "
                    "identifier (possible programming error).", status );

/* See if the DCB ARY_ system identifier for the variance array is
   valid. If not, then the variance array does not exist. */
         } else {
            valid = aryValid( dcb->vid, status );
            if( *status == SAI__OK ) {

/* If it exists, then set a new full data type for it. */
               if( valid ) {
                  aryStype( ftype, dcb->vid, status );
                  ndf1Cmpac( dcb, "VARIANCE", status );

/* Otherwise, decompose the full type specification into a numeric type
   and a complex value flag and store these in the DCB as default
   attributes for the variance component. */
               } else {
                  ndf1Chftp( ftype, dcb->vtyp, sizeof( dcb->vtyp ),
                             &dcb->vcpx, status );
                  if( *status == SAI__OK ) {

/* If the default storage form is primitive and the new data type is
   complex, then convert the storage form to simple. */
                     if( ( !strcmp( dcb->vfrm, "PRIMITIVE" ) ) && dcb->vcpx ) {
                        star_strlcpy( dcb->vfrm, "SIMPLE",
                                      sizeof( dcb->vfrm ) );
                     }
                  }
               }
            }
         }
      }
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Vsftp", status );

}

