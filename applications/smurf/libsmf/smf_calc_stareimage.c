/*
*+
*  Name:
*     smf_calc_stareimage

*  Purpose:
*     Store a 2-D image inside a SCU2RED extension

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_calc_stareimage( smfData *data, int *status);

*  Arguments:
*     data = smfData * (Given)
*        Input data
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine calculates and stores 2-D STARE images constructed
*     from a 3-D time stream. The routine returns with no processing
*     if the data are not from a STARE observation.

*  Notes:

*  Authors:
*     Andy Gibb (UBC)
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2006-10-25 (AGG):
*        Initial version
*     2007-03-28 (TIMJ):
*        Free temporary buffer.
*     2007-04-05 (AGG):
*        Change OBSMODE to SAM_MODE
*     2007-04-17 (AGG):
*        Change verbose level for warning message to normal
*     2007-12-18 (AGG):
*        Update to use new smf_free behaviour
*     2008-05-29 (TIMJ):
*        Free avdata to prevent memory leak.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2006 University of British Columbia.
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 2007 Particle Physics and Astronomy Research Council.
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

/* Standard includes */
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

/* Starlink includes */
#include "sae_par.h"
#include "mers.h"
#include "star/hds.h"

/* SMURF includes */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"
#include "smurf_par.h"

#define FUNC_NAME "smf_calc_stareimage"

void smf_calc_stareimage( smfData *data, const int naver, int *status) {

  double *avdata = NULL;           /* Pointer to averaged data */
  int dims[2];                     /* Dimensions of the stored image */
  smfHead *hdr;                    /* Header information */
  int j;                           /* Loop counter */
  size_t npts;                     /* Number of points in the averaged data */
  int numimages;                   /* Number of output STARE images */
  char obsmode[LEN__METHOD+1];     /* Observing mode */
  HDSLoc *scu2redloc = NULL;       /* Locator to SCU2RED extension */
  double *zero = NULL;             /* Bolometer zero points */

  if ( *status != SAI__OK ) return;

  /* Check we have time-series data */
  if ( data->ndims != 3) {
    *status = SAI__ERROR;
    errRep(FUNC_NAME, 
	   "File does not contain time series data - unable to process", 
	   status);
    return;
  }

  /* Check we have flatfielded data */
  if ( !smf_history_check( data, "smf_flatfield", status) ) {
    if ( *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Data have not been flatfielded ", status);
      return;
    }
  }

  /* Do we have STARE data? */
  hdr = data->hdr;
  smf_fits_getS( hdr, "SAM_MODE", obsmode, LEN__METHOD+1, status );
  if ( strncmp( obsmode, "STARE", 5) == 0 ) {
    msgOutif(MSG__VERB," ", "Processing STARE data", status);

    if (smf_history_check( data, "smf_calc_stareimage", status) ) {
      msgOut(" ", "File contains STARE data which has already been processed: proceeding but this WILL OVERWRITE any STARE images already written into the SCU2RED extension", 
	     status);
    }

    /* Number of output STARE images */
    numimages = (data->dims)[2] / naver;
    /* Obtain a locator for the extension where for the images will
       be stored */
    scu2redloc = smf_get_xloc(data, "SCU2RED", "SCUBA2_MAP_ARR", "WRITE", 
			      0, NULL, status);

    /* Set the dimensions of the output images - they are all the same
       so they can be safely set outside the loop below */
    dims[0] = (data->dims)[0];
    dims[1] = (data->dims)[1];

    /* Loop over the number of output images */
    for ( j=0; j<numimages; j++) {
      /* Average the time stream data over the desired interval */
      smf_average_data( data, j*naver, naver, 1, &avdata, &npts, status );

      /* Temporary */
      zero = smf_malloc( npts, sizeof(double), 1, status );

      /* Store the averaged data as an image */
      smf_store_image( data, scu2redloc, j, 2, dims, naver, 0, 0, avdata, zero, 
		       status);

      avdata = smf_free( avdata, status );
      zero = smf_free( zero, status );
    }
    /* Add a history entry if everything's OK */
    smf_history_write(data, "smf_calc_stareimage", 
		      "STARE images calculated successfully", status);
    /* Release SCU2RED locator */
    datAnnul( &scu2redloc, status );
  } else {
    msgOutif(MSG__NORM," ", 
	     "Input file is not a STARE observation - ignoring", status);
  }

}

