/*
*+
*  Name:
*     smf_get_dims

*  Purpose:
*     Calculate nbolo, ntslice and ndata for a 3-d smfData

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_get_dims( const smfData *data, dim_t *nbolo, dim_t *ntslice, 
*                   dim_t *ndata, size_t *bstride, size_t *tstride, 
*                   int *status )

*  Arguments:
*     data = const smfData *data (Given)
*        Pointer to a smfData
*     nbolo = dim_t* (Returned)
*        Number of bolometers
*     ntslice = dim_t* (Returned)
*        Number of time slices
*     ndata = dim_t* (Returned)
*        Total number of data points (nbolo*ntslice)
*     bstride = size_t* (Returned)
*        How many elements to skip to get to the next bolometer at a given
*        time slice.
*     tstride - size_t* (Returned)
*        How many elements to skip to get to the next time slice for the 
*        current bolometer.
*     status = int * (Given and Returned)
*        Pointer to inherited status.

*  Returned Value:

*  Description:
*     Depending on the data order of a smfData the time axis is either the
*     first or last dimension. Check the order and calculate total numbers
*     of detectors, the number of time slices, and the total number of
*     data points. This routine will also set SMF__WDIM if the input smfData
*     are not 3-dimensional. The bstride and tstride parameters can be used to
*     help index the data array. To index the i'th bolometer and the j'th
*     tslice: value = data[i*bstride + j*tstride]

*  Authors:
*     Edward Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2008-08-27 (EC)
*        Initial version.
*     2008-12-03 (EC):
*        Add stride to API
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008 Science & Technology Facilities Council.
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
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     {note_any_bugs_here}

*-
*/
#include "sae_par.h"
#include "ast.h"
#include "mers.h"
#include "smf.h"
#include "smf_err.h"

#define FUNC_NAME "smf_get_dims"

void smf_get_dims( const smfData *data, dim_t *nbolo, dim_t *ntslice, 
                   dim_t *ndata, size_t *bstride, size_t *tstride, 
                   int *status ) {

  size_t bs;
  dim_t nb;
  dim_t nt;
  dim_t nd;
  size_t ts;

   /* Check the inherited status */
   if ( *status != SAI__OK ) return;

   /* Check for 3-d data */
   if( data->ndims != 3 ) {
     *status = SMF__WDIM;
     msgSeti("NDIMS",data->ndims);
     errRep(" ", FUNC_NAME 
            "Input data have ^NDIMS dimensions, should be 3.", status);
    return;
   }

   /* Calculate Dimensions */
   if( data->isTordered ) {
     nb = (data->dims)[0]*(data->dims)[1];
     nt = (data->dims)[2];
     bs = 1;
     ts = nb;
   } else {
     nt = (data->dims)[0];
     nb = (data->dims)[1]*(data->dims)[2];
     bs=nt;
     ts=1;
   }
   nd = nb*nt;

   if( nbolo ) *nbolo = nb;
   if( ntslice ) *ntslice = nt;
   if( ndata ) *ndata = nd;
   if( bstride ) *bstride = bs;
   if( tstride ) *tstride = ts;
}

