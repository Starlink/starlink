#include "sae_par.h"
#include "cupid.h"
#include "mers.h"

void cupidFindback0( void *data, int *status ){
/*
*+
*  Name:
*     cupidFindback0

*  Purpose:
*     Initiate the  spatial filtering of a 3D array within a thread.

*  Language:
*     Starlink C

*  Synopsis:
*     void cupidFindback0( void *data, int *status ){

*  Description:
*     This is the main function that gets run in a worker thread in order
*     to filter a slice of the supplied base NDF.

*  Parameters:
*     data
*        Pointer to the data structure containing the information needed
*        by this function.
*     status
*        Pointer to the inherited status value.

*  Copyright:
*     Copyright (C) 2011 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David S. Berry
*     {enter_new_authors_here}

*  History:
*     13-SEP-2011 (DSB):
*        Original version.
*     {enter_further_changes_here}

*-
*/

/* Local Variables: */
   CupidFindback0Data *pdata;/* Pointer to structure holding requied info */
   double rms;               /* Global rms error in data */
   float wlim;               /* Min. fraction of good i/p values for a good o/p value */
   hdsdim islice;            /* Slice index */
   hdsdim nslice;            /* Number of slices to process */
   hdsdim slice_dim[ 3 ];    /* Dimensions of each significant slice axis */
   hdsdim slice_lbnd[ 3 ];   /* Lower bounds of each significant slice axis */
   hdsdim slice_size;        /* Number of pixels in each slice */
   int box[ 3 ];             /* Dimensions of each cell in pixels */
   int ndim;                 /* Total number of pixel axes in NDF */
   int newalg;               /* Use experimental algorithm variations? */
   int type;                 /* Integer identifier for data type */
   void *ipd1;               /* Pointer to input Data array */
   void *ipd2;               /* Pointer to output Data array */
   void *wa;                 /* Pointer to work array */
   void *wb;                 /* Pointer to work array */

/* Abort if an error has already occurred. */
   if( *status != SAI__OK ) return;

/* Get a pointer to the data structure, with the expected type. */
   pdata = (CupidFindback0Data *) data;

/* Store the information needed by the function (cupidFindback0) that
   does the work in a thread. */
   box[ 0 ] = pdata->box[ 0 ];
   box[ 1 ] = pdata->box[ 1 ];
   box[ 2 ] = pdata->box[ 2 ];
   ipd1 = pdata->ipd1;
   ipd2 = pdata->ipd2;
   islice = pdata->islice;
   ndim = pdata->ndim;
   nslice = pdata->nslice;
   rms = pdata->rms;
   slice_dim[ 0 ] = pdata->slice_dim[ 0 ];
   slice_dim[ 1 ] = pdata->slice_dim[ 1 ];
   slice_dim[ 2 ] = pdata->slice_dim[ 2 ];
   slice_lbnd[ 0 ] = pdata->slice_lbnd[ 0 ];
   slice_lbnd[ 1 ] = pdata->slice_lbnd[ 1 ];
   slice_lbnd[ 2 ] = pdata->slice_lbnd[ 2 ];
   slice_size = pdata->slice_size;
   type = pdata->type;
   newalg = pdata->newalg;
   wlim = pdata->wlim;

/* Report the bounds of the slice if required. */
   msgBlankif( MSG__VERB, status );
   msgOutiff( MSG__VERB, "", "   Processing slice %" HDS_DIM_FORMAT " of %" HDS_DIM_FORMAT "...", status,
              islice+1, nslice );
   msgBlankif( MSG__VERB, status );

/* Process this slice, then increment the pointer to the next slice. */
   if( type == CUPID__FLOAT ) {
      wa = astMalloc( sizeof( float )*slice_size );
      wb = astMalloc( sizeof( float )*slice_size );
      cupidFindback1F( wlim, ndim, slice_dim, slice_lbnd, box, rms, ipd1, ipd2,
                       wa, wb, newalg, status );
   } else {
      wa = astMalloc( sizeof( double )*slice_size );
      wb = astMalloc( sizeof( double )*slice_size );
      cupidFindback1D( wlim, ndim, slice_dim, slice_lbnd, box, rms, ipd1, ipd2,
                       wa, wb, newalg, status  );
   }

/* Free workspace. */
   wa = astFree( wa );
   wb = astFree( wb );

}

