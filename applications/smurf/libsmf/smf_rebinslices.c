/*
*+
*  Name:
*     smf_rebinslices

*  Purpose:
*     Rebins time slices from a single input file into the output.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_rebinslices( void *job_data_ptr, int *status );

*  Arguments:
*     data_data_ptr = void * (Given)
*        Pointer to smfRebinMapData structure holding data needed to
*        perform the rebinning.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This function uses astRebinSeq to paste all time slices in the
*     current input NDF into the output image, using a simple regridding
*     of data. This routine is designed to be used with the thrAddJob
*     routine.

*  Authors:
*     DSB: David Berry (JAC, UClan)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     27-JUN-2008 (DSB):
*        Initial version.
*     24-NOV-2008 (DSB):
*        Update astRebinSeq argument "nused" correctly.
*     2009-11-03 (TIMJ):
*        Skip bad bolo2map mappings.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008-2009 Science and Technology Facilities Council.
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

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#include <stdint.h>

/* Starlink includes */
#include "ast.h"
#include "mers.h"
#include "sae_par.h"
#include "prm_par.h"
#include "ems.h"

/* SMURF includes */
#include "libsmf/smf.h"

void smf_rebinslices( void *job_data_ptr, int *status ){

/* Local Variables */
   AstMapping *bolo2map = NULL;  /* Combined mapping bolo->map coordinates */
   AstMapping *sky2map = NULL;   /* Mapping from celestial->map coordinates */
   AstSkyFrame *abskyfrm = NULL; /* Output SkyFrame (always absolute) */
   const double *params;         /* Pointer to array of spreading params */
   dim_t *nused;                 /* Point to count of i/p samples used */
   dim_t *udim;                  /* Output array upper GRID bounds */
   dim_t islice;                 /* Time slice index */
   dim_t lbnd_in[ 2 ];           /* Lower pixel bounds for input maps */
   dim_t nbol;                   /* # of bolometers in the sub-array */
   dim_t nslice;                 /* No. of time slices */
   dim_t ubnd_in[ 2 ];           /* Upper pixel bounds for input maps */
   double *boldata = NULL;       /* Pointer to bolometer data */
   double *bolovar;              /* Pointer to bolometer variance array */
   double *map;                  /* Pointer to output data array */
   double *variance;             /* Pointer to output variance array */
   double *weights;              /* Pointer to output weights array */
   fts2Port fts_port;            /* FTS-2 port */
   int moving;                   /* Are we tracking a moving source? */
   int rebinflags;               /* Control the rebinning procedure */
   int spread;                   /* Pixel spreading scheme */
   smfData *data;                /* Smurf data description */
   smfRebinMapData *job_data;    /* Pointer to data structure */
   static dim_t ldim[ 2 ] = {1,1}; /* Output array lower GRID bounds */

/* Check the inherited status */
   if( *status != SAI__OK ) return;

/* Extract values from the supplied structure into local variables. */
   job_data = (smfRebinMapData *) job_data_ptr;
   data = job_data->data;
   rebinflags = job_data->rebinflags;
   abskyfrm = job_data->abskyfrm;
   bolovar = job_data->bolovar;
   sky2map = job_data->sky2map;
   moving = job_data->moving;
   spread = job_data->spread;
   params = job_data->params;
   udim = job_data->udim;
   map = job_data->map;
   variance = job_data->variance;
   weights = job_data->weights;
   nused = &(job_data->nused);
   fts_port = job_data->fts_port;

/* Initialise the number of input samples used. */

/* Lock the supplied AST object pointers for exclusive use by this thread.
   The invoking thread should have unlocked them before creating starting
   this job). */
   astLock( abskyfrm, 0 );
   astLock( sky2map, 0 );
   smf_lock_data( data, 1, status );

/* Check that there really is valid data */
   boldata = (data->pntr)[ 0 ];
   if( boldata == NULL ) {
      if( *status == SAI__OK ) {
         *status = SAI__ERROR;
         emsRep( "", "Input data to rebinslices is NULL", status );
      }
   }

/* Calculate bounds in the input array */
   nbol = (data->dims)[0] * (data->dims)[ 1 ];
   lbnd_in[ 0 ] = 1;
   lbnd_in[ 1 ] = 1;
   ubnd_in[ 0 ] = (data->dims)[ 0 ];
   ubnd_in[ 1 ] = (data->dims)[ 1 ];
   nslice = (data->dims)[ 2 ];

/* Loop over all time slices in the data */
   for( islice = 0; islice < nslice; islice++, boldata += nbol ) {

/* Calculate the bolometer to map-pixel transformation for this tslice */
      bolo2map = smf_rebin_totmap( data, islice, abskyfrm, sky2map, moving,
                                   fts_port, status );
      /* skip if we did not get a mapping this time round */
      if (*status == SAI__OK && !bolo2map) continue;

/* Rebin this time slice */
      astRebinSeq8D( bolo2map, 0.0, 2, lbnd_in, ubnd_in, boldata,
                     bolovar, spread, params, rebinflags, 0.1, 1000000,
                     VAL__BADD, 2, ldim, udim, lbnd_in, ubnd_in,
                     map, variance, weights, nused );

/* Free AST objects created in this loop. */
      bolo2map = astAnnul( bolo2map );

/* Abort if an error has occurred. */
      if( *status != SAI__OK ) break;
   }

/* Unlock the supplied AST object pointers so that other threads can use
   them. */
   smf_lock_data( data, 0, status );
   astUnlock( abskyfrm, 1 );
   astUnlock( sky2map, 1 );
}
