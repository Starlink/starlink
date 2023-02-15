/*
*+
*  Name:
*     smf_startup

*  Purpose:
*     Flag samples at start of a data stream

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_startup( smfData *data, dim_t startup, int *status )

*  Arguments:
*     data = smfData * (Given and Returned)
*        The data that will be flagged. Locations of spikes
*        will have bit SMF__Q_SPIKE set on exit.
*     box = dim_t (Given)
*        The number of time slices to blank at the start of the
*        time-stream, following any padding.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     The first "startup" time slices in the supplied smfData (following
*     any padding) are flagged using the BADDA. This is to allow the
*     initial section of an observation, prior to the scan pattern being
*     fully estblished, to be excluded form the map. For instance, daisy
*     observations can start with a large loop in boresight position,
*     well outside the observation's main range of (DAz, DEl), prior to
*     the boresight movement achieving the regular daisy pattern.

*  Authors:
*     DSB: David Berry (EAO)
*     {enter_new_authors_here}

*  History:
*     5-APR-2021 (DSB):
*        Initial Version

*  Copyright:
*     Copyright (C) 2021 East Asian Observatory
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 3 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA
*-
*/

/* Starlink includes */
#include "mers.h"
#include "sae_par.h"

/* SMURF includes */
#include "libsmf/smf.h"

#define FUNC_NAME "smf_startup"

void smf_startup( smfData *data, dim_t startup, int *status ){

/* Local Variables */
   dim_t bstride;              /* Vector stride between bolometer samples */
   dim_t ibolo;                /* Index of bolometer */
   dim_t itime;                /* Index of time-slice */
   dim_t nbolo;                /* Number of bolometers */
   dim_t ntime;                /* Number of time-slices */
   dim_t t_first;              /* First non-padding time slice index */
   dim_t t_last;               /* Last time slice index to flag */
   dim_t tstride;              /* Vector stride between time samples */
   smf_qual_t *pqua0;          /* Pointer to quality for first bolo sample */
   smf_qual_t *pqua;           /* Pointer to quality for time slice */
   smf_qual_t *qua = NULL;     /* Pointer to quality flags */

/* Check inherited status. */
   if( *status != SAI__OK ) return;

/* Get a pointer to the quality array to use. */
   qua = smf_select_qualpntr( data, NULL, status );

/* Report an error if we have no quality array. */
   if( !qua && *status == SAI__OK ) {
     *status = SAI__ERROR;
     errRep( " ", FUNC_NAME ": No valid QUALITY array was provided", status );
   }

/* Check the supplied startup value is valid. */
   if( startup <= 0 && *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRepf( " ", FUNC_NAME ": Can't blank start: startup=%d, must be > 0",
              status, (int) startup );
   }

/* Obtain data dimensions, and the stride between adjacent elements on
   each axis (bolometer and time). Use the existing data order to avoid
   the cost of re-ordering. */
   smf_get_dims( data,  NULL, NULL, &nbolo, &ntime, NULL, &bstride, &tstride,
                 status );

/* Get the first and last time slice to use (this excludes padding and
   apodised end sections). */
   smf_get_goodrange( qua, ntime, tstride, (SMF__Q_PAD | SMF__Q_APOD),
                      &t_first, &t_last, status );

/* Modify the last time slice index to be the index of the last time to
   be blanked. */
   if( t_last > t_first + startup ) t_last = t_first + startup;

/* Initialise a pointer to the first quality value for the first bolometer. */
   pqua0 = qua;

/* We process each bolometer in turn. */
   for( ibolo = 0; ibolo < nbolo; ibolo++ ) {

/* Ignore bad bolometers. */
      if( !( *pqua0 & SMF__Q_BADB ) ) {

/* Initialise a pointer to the first quality value for the first time slice in
   the current bolometer. */
         pqua = pqua0 + t_first*tstride;

/* Ensure the initial section of the time slice is flagged with SMF__Q_BADDA. */
         for( itime = t_first; itime <= t_last; itime++ ) {
            *pqua |= SMF__Q_BADDA;
            pqua += tstride;
         }
      }

/* Update the pointer to the first quality values in the next bolometer. */
      pqua0 += bstride;
   }
}
