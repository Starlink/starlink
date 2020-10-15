/*
*+
*  Name:
*     smf_get_dims

*  Purpose:
*     Calculate nbolo, ntslice and other dimensional properties for a smfData

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     void smf_get_dims( const smfData *data, dim_t *nrows, dim_t *ncols,
*                        dim_t *nbolo, dim_t *ntslice, dim_t *ndata,
*                        dim_t *bstride, dim_t *tstride, int *status );

*  Arguments:
*     data = const smfData *data (Given)
*        Pointer to a smfData
*     nrows = dim_t* (Returned)
*        Number of rows.
*     ncols = dim_t* (Returned)
*        Number of columns.
*     nbolo = dim_t* (Returned)
*        Number of bolometers
*     ntslice = dim_t* (Returned)
*        Number of time slices
*     ndata = dim_t* (Returned)
*        Total number of data points (nbolo*ntslice)
*     bstride = dim_t* (Returned)
*        How many elements to skip to get to the next bolometer at a given
*        time slice.
*     tstride = dim_t* (Returned)
*        How many elements to skip to get to the next time slice for the
*        current bolometer.
*     status = int * (Given and Returned)
*        Pointer to inherited status.

*  Returned Value:

*  Description:
*     Depending on the data order of a smfData the time axis is either
*     the first or last dimension. Check the order and calculate total
*     numbers of detectors, the number of time slices, and the total
*     number of data points.
*
*     The bstride and tstride parameters can be used to help index the
*     data array. To index the i'th bolometer and the j'th tslice:
*     value = data[i*bstride + j*tstride].
*
*     2-dimensional data are treated as if they are 3-d time-ordered
*     bolometer data with a third dimension (number of time slices) equal
*     to one.
*
*     4-dimensional data can also be handled if they contain FFT data,
*     which is verified through a call to smf_isfft. In this case the
*     bolometer and time strides refer to a single component (real
*     or imaginary).
*
*     This routine will set SMF__WDIM if the input smfData are not 2-, 3-
*     or 4-dimensional.

*  Authors:
*     Edward Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2008-08-27 (EC)
*        Initial version.
*     2008-12-03 (EC):
*        Add stride to API
*     2010-10-29 (EC):
*        Handle 4d FFT data
*     2011-09-06 (TIMJ):
*        Allow 2d data (eg responsivity images) to return the number
*        of columns or rows.
*     2011-10-06 (TIMJ):
*        Check "data" is defined before dereferencing it.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2011 Science & Technology Facilities Council.
*     Copyright (C) 2008,2010 University of British Columbia
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
#include "sae_par.h"
#include "ast.h"
#include "mers.h"
#include "smf.h"
#include "smf_err.h"

#define FUNC_NAME "smf_get_dims"

void smf_get_dims( const smfData *data, dim_t *nrows, dim_t *ncols,
                   dim_t *nbolo, dim_t *ntslice, dim_t *ndata,
                   dim_t *bstride, dim_t *tstride, int *status ) {

  dim_t bs = 0;
  dim_t i = 0;
  dim_t nb = 0;
  dim_t nt = 0;
  dim_t nd = 0;
  dim_t ts = 0;
  dim_t nr = 0;
  dim_t nc = 0;
  dim_t nf = 0;
  dim_t rdims[2];
  dim_t fdims[2];

   /* Check the inherited status */
   if ( *status != SAI__OK ) goto finalize;
   if (!data) goto finalize;

   if( data->ndims == 3 || data->ndims == 2 ) {
     /* Calculate Dimensions */
     if( data->ndims == 2 || data->isTordered ) {
       nr = (data->dims)[SC2STORE__ROW_INDEX];
       nc = (data->dims)[SC2STORE__COL_INDEX];
       nb = (data->dims)[0]*(data->dims)[1];
       nt = (data->ndims == 2 ? 1 : (data->dims)[2]);
       bs = 1;
       ts = nb;
     } else {
       nr = (data->dims)[SC2STORE__ROW_INDEX+1];
       nc = (data->dims)[SC2STORE__COL_INDEX+1];
       nt = (data->dims)[0];
       nb = (data->dims)[1]*(data->dims)[2];
       bs=nt;
       ts=1;
     }
     nd = nb*nt;
   } else if( smf_isfft( data, rdims, &nb, fdims, NULL, NULL, status ) ) {
     /* FFT data are basically two consecutive blocks of bolo-ordered
        data, each of length nf frequencies * nbolos, for the real and
        imaginary parts respectively */
     nt = rdims[0];
     nf = fdims[0];
     nr = (data->dims)[SC2STORE__ROW_INDEX+1];
     nc = (data->dims)[SC2STORE__COL_INDEX+1];
     bs = nf;
     ts = 1;

     nd = 1;
     for( i=0; i<data->ndims; i++ ) nd *= data->dims[i];
   } else {
     if (*status != SAI__OK) errAnnul(status); /* Annul smf_isfft error */
     *status = SMF__WDIM;
     errRepf(" ", FUNC_NAME
             ": Can not work out rows, columns or time slices from %d-dimensional data",
             status, (int)data->ndims);
   }

 finalize:
   if( nrows ) *nrows = nr;
   if( ncols ) *ncols = nc;
   if( nbolo ) *nbolo = nb;
   if( ntslice ) *ntslice = nt;
   if( ndata ) *ndata = nd;
   if( bstride ) *bstride = bs;
   if( tstride ) *tstride = ts;
}
