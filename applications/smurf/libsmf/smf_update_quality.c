
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
*     smf_update_quality( smfData *data, unsigned char *target, int syncbad,
*                         unsigned char *badmask, double badfrac,
*                         int *status );

*  Arguments:
*     data = smfData* (Given)
*        Pointer to smfData that will contain the updated QUALITY array
*     target = unsigned char* (Given)
*        If defined update this buffer instead of the QUALITY in data
*     syncbad = int (Given)
*        If set synchronize SMF__Q_BADS quality flag with VAL__BADD in data
*     badmask = unsigned char* (Given and Returned)
*        If given, points to byte array with same dimensions as bolometers.
*        Each position that is non-zero will set SMF__Q_BAD for all data
*        for that detector. 
*     badfrac = double (Given)
*        If nonzero, fraction of samples for entire bolo to be flagged as bad.
*        If badmask specified, it is updated.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine updates an existing QUALITY array. By default (if
*     target is NULL), the QUALITY component associated with data is
*     updated.  Alternatively a new buffer target can be specified. A
*     mask indicating which bolometers are bad and should be
*     completely ignored (SMF__Q_BADB) may be supplied. Additionally,
*     the routine will ensure that QUALITY has SMF__Q_BADS set for
*     each bad data point (VAL__BADD). If no DATA or QUALITY (unless
*     target specified) arrays are associated with the smfData bad
*     status is set (SAI__ERROR) and the function returns.

*  Notes:

*  Authors:
*     Ed Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2008-02-01 (EC):
*        Initial version.
*     2008-03-03 (EC):
*        Added target to interface
*     2008-03-25 (EC):
*        Added syncbad to interface
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

void smf_update_quality( smfData *data, unsigned char *target, int syncbad, 
			 unsigned char *badmask, double badfrac, 
			 int *status ) {

  unsigned char *badm=NULL;     /* The bad bolo mask */
  dim_t i;                      /* loop counter */
  dim_t j;                      /* loop counter */
  dim_t nbolo;                  /* Number of bolometers */
  dim_t ndata;                  /* Number of data points */
  dim_t nbad;                   /* Bad samples counter */
  dim_t ntslice;                /* Number of time slices */
  unsigned char *qual=NULL;     /* Pointer to the QUALITY array */

  if ( *status != SAI__OK ) return;

  /* Check for QUALITY */
  if( target ) {
    qual = target;                            /* QUALITY given by target */
  } else {
    if( data->pntr[2] ) {
      qual = (unsigned char *) data->pntr[2]; /* QUALITY given by smfData */
    } else {
      *status = SAI__ERROR;
      errRep( FUNC_NAME, "smfData does not contain a QUALITY component", 
	      status);
      return;
    }
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

  /* Check for valid badfrac */
  if( (badfrac < 0) || (badfrac > 1) ) {
    msgSeti( "BADFRAC", badfrac );
    errRep(FUNC_NAME, 
	   "Invalid badfrac: ^BADFRAC. Must be in range (0 -- 1).", status);
  }

  /* Calculate data dimensions */
  smf_get_dims( data, &nbolo, &ntslice, &ndata, status );

  /* Check for input badmask */
  if( badmask ) {
    badm = badmask;
  } else if( badfrac ) {
    /* Allocate a badm buffer if using fractional threshold and an external
       badmask was not specified */
    badm = smf_malloc( nbolo, sizeof(*badm), 1, status );
  }

  if( *status == SAI__OK ) {
    
    /* Synchronize SMF__Q_BADS quality and VAL__BADD in data array */
    if( syncbad ) {
      for( i=0; i<ndata; i++ ) {    /* Loop over all samples */
	qual[i] |= 
	  (SMF__Q_BADS & (((double *)data->pntr[0])[i] == VAL__BADD) );
      }
    }
    
    /* Apply badmask if available */
    if( badm ) {

      /* Loop over detector */
      for( i=0; i<nbolo; i++ ) {

	/* Update badmask if badfrac specified */
	if( badfrac && !badm[i] ) {
	  nbad = 0;

	  /* Loop over samples and count the number with SMF__Q_BADS set */
	  for( j=0; j<ntslice; j++ ) {
	    if( data->isTordered ) {
	      if( qual[nbolo*j + i] & SMF__Q_BADS ) nbad ++;
	    } else {
	      if( qual[j + i*ntslice] & SMF__Q_BADS ) nbad ++;
	    }
	  }

	  if( ((double) nbad / (double) ntslice) > badfrac ) {
	    badm[i] = 1;
	  }
	}

	/* Now apply the badmask */
	if( badm[i] ) {
	  for( j=0; j<ntslice; j++ ) {
	    if( data->isTordered ) {
	      qual[nbolo*j + i] |= SMF__Q_BADB;
	    } else {
	      qual[j + i*ntslice] |= SMF__Q_BADB;
	    }
	  }
	}
      }
    }
  }

  /* Free badm if local */
  if( badm && !badmask ) {
    badm = smf_free( badm, status );
  }

}
