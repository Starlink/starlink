/*
*+
*  Name:
*     smf_write_smfFilter

*  Purpose:
*     Write a smfFilter to disk

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     smf_write_smfFilter( ThrWorkForce *wf, const smfFilter *filt,
*                          const char *filename, const Grp *igrp,
*                          dim_t grpindex, int *status );

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads
*     filt = const smfFilter * (Given)
*        Pointer to 2d smfFilter to be written
*     filename = const char * (Given)
*        Name of output NDF if non-NULL. If NULL the filename is obtained
*        from the group.
*     igrp = const Grp * (Given)
*        Group containing the required filename. Can be NULL, in which
*        case the explicitly supplied filename is used.
*     grpindex = dim_t (Given)
*        Index into group.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Return Value:

*  Description:
*     This function packs a smfFilter into a smfData and then writes
*     it to a file. Currently nothing is done about WCS, or bnd values.

*  Notes:

*  Authors:
*     Ed Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2011-10-26 (EC):
*        Initial version.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2011 University of British Columbia.
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

/* System includes */
#include <stdlib.h>
#include <string.h>

/* Starlink includes */
#include "sae_par.h"
#include "mers.h"
#include "star/thr.h"
#include "ndf.h"

/* SMURF routines */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"

#define FUNC_NAME "smf_write_smfFilter"

void smf_write_smfFilter( ThrWorkForce *wf, const smfFilter *filt, const char *filename,
                          const Grp * igrp, dim_t grpindex, int *status ) {

  double *d = NULL;             /* Data array pointer */
  smfData *data=NULL;           /* smfData for output */
  dim_t i;                     /* Loop counter */
  dim_t nsamp;                 /* Number of samples in the filter */

  if( *status != SAI__OK ) return;

  if( !filt ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": NULL smfFilter supplied.", status );
    return;
  }

  /* In case we change the data type of smfFilters in the future try
     to catch that here */
  if( sizeof(filt->real) != smf_dtype_sz(SMF__DOUBLE,status) ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": warning -- smfFilter seems to have changed type!",
            status );
    return;
  }

  /* We will pack the data into an array that has an extra dimension to
     store the real/imaginary parts of the filter just like an FFT */

  data = smf_create_smfData( 0, status );

  if( *status == SAI__OK ) {
    data->dtype = SMF__DOUBLE;
    data->ndims = filt->ndims+1;

    nsamp=1;
    for( i=0; i<filt->ndims; i++ ) {
      data->dims[i] = filt->fdims[i];
      nsamp *= filt->fdims[i];
    }
    data->dims[data->ndims-1] = 2;

    /* Copy the real and imaginary parts into our new array consecutively */
    data->pntr[0] = astCalloc( nsamp*2, smf_dtype_sz(SMF__DOUBLE,status) );

    if( *status == SAI__OK ) {
      d = data->pntr[0];

      memcpy( d, filt->real, nsamp*sizeof(d) );

      if( filt->isComplex ) {
        memcpy( d + nsamp, filt->imag, nsamp*sizeof(d) );
      }
    }
  }

  /* Write out the file */
  smf_write_smfData( wf, data, NULL, filename, igrp, grpindex, 0, MSG__NORM,
                     0, NULL, NULL, status );

  if( data ) smf_close_file( wf, &data, status );

}
