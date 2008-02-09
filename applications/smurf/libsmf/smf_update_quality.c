
/*
*+
*  Name:
*     smf_update_quality

*  Purpose:
*     Update the quality array associated with a smfData

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     SMURF subroutine

*  Invocation:
*     smf_update_quality( smfData *data, unsigned char *badmask, 
*                         int *status );

*  Arguments:
*     data = smfData* (Given)
*        Pointer to smfData that will contain the updated QUALITY array
*     badmask = unsigned char* (Given)
*        If given, points to byte array with same dimensions as bolometers.
*        Each position that is non-zero will set SMF__Q_BAD for all data
*        for that detector. 
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine updates an existing QUALITY array. A mask
*     indicating which bolometers are bad and should be completely
*     ignored (SMF__Q_BADB) may be supplied. Additionally, the routine
*     will ensure that QUALITY has SMF__Q_BADS set for each bad data
*     point (VAL__BADD). If no DATA or QUALITY arrays are associated with the
*     smfData bad status is set (SAI__ERROR) and the function returns.

*  Notes:

*  Authors:
*     Ed Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2008-02-01 (EC):
*        Initial version.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2006 Particle Physics and Astronomy Research Council.
*     University of British Columbia. All Rights Reserved.

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


/* Starlink includes */
#include "mers.h"
#include "sae_par.h"
#include "star/ndg.h"
#include "prm_par.h"
#include "par_par.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "libsmf/smf_err.h"

/* Other includes */

#define FUNC_NAME "smf_update_quality"

void smf_update_quality( smfData *data, unsigned char *badmask, int *status ) {

  dim_t i;                      /* loop counter */
  dim_t j;                      /* loop counter */
  dim_t nbolo;                  /* Number of bolometers */
  dim_t ndata;                  /* Number of data points */
  dim_t ntslice;                /* Number of time slices */

  if ( *status != SAI__OK ) return;

  /* Check for QUALITY */
  if( !data->pntr[2] ) {
    *status = SAI__ERROR;
    errRep( FUNC_NAME, "smfData does not contain a QUALITY component", status);
    return;
  }

  /* Check for DATA */
  if( !data->pntr[0] ) {
    *status = SAI__ERROR;
    errRep( FUNC_NAME, "smfData does not contain a DATA component", status );
    return;
  }

  /* Verify double precision / 3-dimensional (time-domain) data */
  if( data->ndims != 3 ) {
    *status = SAI__ERROR;
    msgSeti("NDIMS",data->ndims);
    errRep(FUNC_NAME, 
	   "Don't know how to handle ^NDIMS dimensions, should be 3.", status);
    return;
  }

  if( data->dtype !=  SMF__DOUBLE) {
    *status = SAI__ERROR;
    errRep(FUNC_NAME, 
	   "Data is not double-precision", status);
    return;
  }

  /* Calculate data dimensions */
  if( data->isTordered ) {
    nbolo = (data->dims)[0]*(data->dims)[1];
    ntslice = (data->dims)[2];
  } else {
    ntslice = (data->dims)[0];
    nbolo = (data->dims)[1]*(data->dims)[2];
  }
  ndata = nbolo*ntslice;

  /* Apply badmask if available */

  if( badmask ) {
    /* Loop over detector */
    for( i=0; i<nbolo; i++ ) if( badmask[i] ) {
      /* Loop over time slice */
      for( j=0; j<ntslice; j++ ) {
	if( data->isTordered ) {
	  ((unsigned char*)data->pntr[2])[nbolo*j + i] |= SMF__Q_BADB;
	} else {
	  ((unsigned char*)data->pntr[2])[j + i*ntslice] |= SMF__Q_BADB;
	}
      }
    }
  }

  /* Synchronize quality and VAL__BADD in data array */
  for( i=0; i<ndata; i++ ) {    /* Loop over all samples */
    ((unsigned char*)data->pntr[2])[i] |= 
      (SMF__Q_BADS & (((double *)data->pntr[0])[i] == VAL__BADD) );
  }


}
