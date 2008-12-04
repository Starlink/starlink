
/*
*+
*  Name:
*     smf_update_valbad

*  Purpose:
*     Synchronize VAL__BADD values in smfData with QUALITY

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     SMURF subroutine

*  Invocation:
*     smf_update_valbad( smfData *data, unsigned char mask, int *status );

*  Arguments:
*     data = smfData* (Given)
*        Pointer to smfData to be updated
*     mask = unsigned char (Given)
*        Bitmask of QUALITY flags to consider for setting VAL__BADD values
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine checks the QUALITY of a smfData for bits that are
*     present in the mask. When encountered the corresponding data
*     value is set to VAL__BADD. 

*  Notes:

*  Authors:
*     Ed Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2008-10-20 (EC):
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

#define FUNC_NAME "smf_update_valbad"

void smf_update_valbad( smfData *data, unsigned char mask, int *status ) {
  dim_t i;                      /* loop counter */
  dim_t nbolo;                  /* Number of bolometers */
  dim_t ndata;                  /* Number of data points */
  dim_t ntslice;                /* Number of time slices */
  unsigned char *qual=NULL;     /* Pointer to the QUALITY array */

  if ( *status != SAI__OK ) return;

  /* Check for QUALITY */
  if( data->pntr[2] ) {
    qual = (unsigned char *) data->pntr[2]; /* QUALITY given by smfData */
  } else {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": smfData does not contain a QUALITY component", 
            status);
    return;
  }

  /* Check for DATA */
  if( !data->pntr[0] ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": smfData does not contain a DATA component", 
            status );
    return;
  }

  /* Verify double precision / 3-dimensional (time-domain) data */
  if( data->ndims != 3 ) {
    *status = SAI__ERROR;
    msgSeti("NDIMS",data->ndims);
    errRep("", FUNC_NAME 
           ": Don't know how to handle ^NDIMS dimensions, should be 3.", 
           status);
    return;
  }

  if( data->dtype !=  SMF__DOUBLE) {
    *status = SAI__ERROR;
    errRep("", FUNC_NAME 
	   ": Data is not double-precision", status);
    return;
  }

  /* Calculate data dimensions */
  smf_get_dims( data, &nbolo, &ntslice, &ndata, NULL, NULL, status );

  if( *status == SAI__OK ) { 
    /* Synchronize VAL__BADD with QUALITY matching any mask bits */
    for( i=0; i<ndata; i++ ) {    /* Loop over all samples */
      if( qual[i]&mask ) 
        ((double *)data->pntr[0])[i] = VAL__BADD;
    }    
  }
}
