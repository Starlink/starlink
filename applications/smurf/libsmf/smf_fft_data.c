/*
*+
*  Name:
*     smf_fft_data

*  Purpose:
*     Calculate the forward or inverse FFT of a smfData

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     pntr = smf_fft_data( smfData *data, int inverse, int *status );

*  Arguments:
*     data = smfData * (Given)
*        Pointer to the input smfData
*     inverse = int (Given)
*        If set perform inverse transformation. Otherwise forward.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Return Value:
*     Pointer to newly created smfData containing the FFT (4-dimensional)

*  Description: 

*  Notes:

*  Authors:
*     Ed Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2008-07-09 (EC):
*        Initial version
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2006 Particle Physics and Astronomy Research
*     Council. University of British Columbia. All Rights Reserved.

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

/* System includes */
#include <stdlib.h>
#include <string.h>

/* Starlink includes */
#include "sae_par.h"
#include "mers.h"
#include "ndf.h"

/* SMURF routines */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"

#define FUNC_NAME "smf_create_smfFilter"

smfData *smf_fft_data( const smfData *data, int inverse, int *status ) {
  dim_t dim=0;                  /* Length of FFT */
  dim_t nbolo=0;                /* Number of detectors  */
  dim_t ntslice=0;              /* Number of time slices */
  smfData *retdata=NULL;        /* Pointer to new transformed smfData */

  if (*status != SAI__OK) return NULL;

  /* Data dimensions */
  if( data->ndims == 3 ) {
    if( data->isTordered ) { /* T is 3rd axis if time-ordered */
      nbolo = data->dims[0]*data->dims[1];
      ntslice = data->dims[2];
    } else {                 /* T is 1st axis if time-ordered */
      nbolo = data->dims[1]*data->dims[2];
      ntslice = data->dims[0];
    }
  } else if( template->ndims == 1 ) {
    /* If 1-d data, only one axis to choose from */
    ntslice = template->dims[0];
    nbolo=1;
  } else {
    *status = SAI__ERROR;
    errRep( FUNC_NAME, "smfData has strange dimensions", status );
  }

  if( data->dtype != SMF__DOUBLE ) {
    *status = SAI__ERROR;
    errRep( FUNC_NAME, "smfData is not double precision", status );
  }

  /* Create a new smfData, copying over everything except for the bolo
     data itself */

  retdata = smf_deepcopy_smfData( data, 0, SMF__NOCREATE_DATA | 
                                  SMF__NOCREATE_VARIANCE | 
                                  SMF__NOCREATE_QUALITY, status );

  if( *status == SAI__OK ) {
    
    /* Number of frequencies in the FFT */
    dim = filt->ntslice/2+1;
    
    /* Allocate space for the transformed bolo data */
    data->pntr[0] = smf_malloc( dim*nbolo*2, sizeof(double), 0, status );

    /* Perform the FFT */

    /* Fix up the time/frequency axis in the tswcs */
    

  }

}
