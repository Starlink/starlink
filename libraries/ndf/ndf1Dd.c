#include <string.h>
#include "star/util.h"
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "mers.h"
#include <string.h>
#include "star/util.h"

void ndf1Dd( NdfDCB *dcb, int *status ){
/*
*+
*  Name:
*     ndf1Dd

*  Purpose:
*     Ensure that DCB information is available for an NDF data array
*     component.

*  Synopsis:
*     void ndf1Dd( NdfDCB *dcb, int *status )

*  Description:
*     This function ensures that information is available in the DCB
*     for the data array component of an NDF. It does nothing if this
*     information is already available. Otherwise, it obtains the necessary
*     information by inspecting the actual data object, performing
*     necessary validation checks in the process.

*  Parameters:
*     dcb
*        Pointer to the DCB entry for which information is required.
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
   hdsbool_t there;      /* Whether the data component is present */
   int isacc;            /* Whether WRITE access is available */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* See if the required information is already available. There is
   nothing to do if it is. */
   if( !dcb->kd ) {

/* See if the DATA_ARRAY component is present in the data object. Report
   an error if it is not. */
      datThere( dcb->loc, "DATA_ARRAY", &there, status );
      if( *status == SAI__OK ) {
         if( !there ) {
            *status = NDF__NODAT;
            ndf1Dmsg( "NDF", dcb );
            errRep( " ", "The DATA_ARRAY component is missing from the NDF "
                    "structure ^NDF", status );

/* If there, then import it into the ARY_ system, storing the resulting
   array identifier in the DCB. */
         } else {
            aryFind( dcb->loc, "DATA_ARRAY", &dcb->did, status );

/* Get the storage form of the DATA array. */
            aryForm( dcb->did, dcb->dfrm, status );

/* Obtain the data array attributes needed as default values for other
   NDF components and store them in the DCB. */
            aryType( dcb->did, dcb->detyp, status );
            aryCmplx( dcb->did, &dcb->decpx, status );

            if( !strcmp( dcb->dfrm, "DELTA" ) || !strcmp( dcb->dfrm, "SCALED" ) ) {
               star_strlcpy( dcb->defrm, "SIMPLE", sizeof( dcb->defrm ) );
            } else {
               star_strlcpy( dcb->defrm, dcb->dfrm, sizeof( dcb->defrm ) );
            }

/* See if WRITE access to the data array is available and set the data
   object access mode accordingly. */
            aryIsacc( dcb->did, "WRITE", &isacc, status );
            if( *status == SAI__OK ) {
               if( isacc ) star_strlcpy( dcb->mod, "UPDATE",
                                         sizeof( dcb->mod ) );
            }

/* If an error occurred, then annul the data array identifier. */
            if( *status != SAI__OK ) aryAnnul( &dcb->did, status );
         }
      }

/* Note whether DCB information about the data component is now
   available. */
      dcb->kd = ( *status == SAI__OK );
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Dd", status );

}

