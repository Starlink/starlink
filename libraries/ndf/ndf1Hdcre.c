#include "star/hds.h"
#include "star/cmp.h"
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "mers.h"

void ndf1Hdcre( NdfDCB *dcb, int *status ){
/*
*+
*  Name:
*     ndf1Hdcre

*  Purpose:
*     Create a history component for an NDF, if necessary.

*  Synopsis:
*     void ndf1Hdcre( NdfDCB *dcb, int *status )

*  Description:
*     This function ensures that a history component exists for an NDF,
*     creating one if necessary. Associated locators are stored in the DCB.
*     The function returns without action if a history component already
*     exists.

*  Parameters:
*     dcb
*        Pointer to a DCB entry identifying the NDF for which a history
*        component is required.
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
*     xxx (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/

/* Local Variables: */
   char time[ NDF__SZHDT + 1 ];    /* Formatted creation time string */
   float sec;            /* Seconds field value */
   hdsdim dim;           /* Object dimension sizes */
   int ymdhm[ 5 ];       /* Integer date/time field values */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure that history structure information is available in the DCB. */
   ndf1Dh( dcb, status );
   if( *status == SAI__OK ) {

/* Check if a history component is already present. There is nothing to
   do if it is. */
      if( !dcb->hloc ) {

/* If not, then create a new scalar history component and obtain a
   locator for it to be stored in the DCB. */
         datNew( dcb->loc, "HISTORY", "HISTORY", 0, NULL, status );
         datFind( dcb->loc, "HISTORY", &dcb->hloc, status );

/* Obtain the current UTC date and time and convert it to standard history
   format. */
         ndf1Time( ymdhm, &sec, status );
         ndf1Fmhdt( ymdhm, sec, time, sizeof( time ), status );

/* Create a scalar CREATED component in the history structure and write
   the creation time to it. */
         datNew0C( dcb->hloc, "CREATED", NDF__SZHDT, status );
         cmpPut0C( dcb->hloc, "CREATED", time, status );

/* Also create a scalar CURRENT_RECORD component and initialise it to
   zero. */
         datNew0I( dcb->hloc, "CURRENT_RECORD", status );
         cmpPut0I( dcb->hloc, "CURRENT_RECORD", 0, status );

/* Create a 1-dimensional RECORDS array of structures (initially with
   10 elements). This will hold the history records themselves. Obtain
   a locator to this structure for storage in the DCB. */
         dim = 10;
         datNew( dcb->hloc, "RECORDS", "HIST_REC", 1, &dim, status );
         datFind( dcb->hloc, "RECORDS", &dcb->hrloc, status );

/* If an error occurred, then annul any new locators which may have
   been acquired and erase the history structure which may have been
   created. */
         if( *status != SAI__OK ) {
            datAnnul( &dcb->hrloc, status );
            datAnnul( &dcb->hloc, status );
            errBegin( status );
            datErase( dcb->loc, "HISTORY", status );
            errEnd( status );
         }

/* Note whether DCB history information is up to date. */
         dcb->kh = ( *status == SAI__OK );
      }
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Hdcre", status );

}

