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
*     smf_calc_stareimage( smfData *data, const int naver, int *status);

*  Arguments:
*     data = smfData * (Given)
*        Input data struct
*     naver = const int (Given)
*        Number of frames/samples to average to produce a STARE image
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
*     2008-07-11 (AGG):
*        Allow for lower-case SAM_MODE
*     2008-07-24 (TIMJ):
*        Use hdr->obsmode instead of SAM_MODE.
*     2008-11-13 (AGG):
*        - Warn user if number of frames to average is not an integer
*          factor of the number of time slices
*        - Recalculate number of frames to average if necessary
*     2009-0916 (TIMJ):
*        No longer attempt to write an unused ZERO array to file
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2006-2008 University of British Columbia.
*     Copyright (C) 2008-2009 Science and Technology Facilities Council.
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
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

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
  dim_t dims[2];                   /* Dimensions of the stored image */
  smfHead *hdr;                    /* Header information */
  int j;                           /* Loop counter */
  dim_t npts;                      /* Number of points in the averaged data */
  int numaver;                     /* Number of samples to average */
  int numimages;                   /* Number of output STARE images */
  dim_t numsamples;                /* Number of time slices (samples) */
  dim_t remainder;                 /* Remainder from dividing no of timeslices
				      by number of frames to average */
  HDSLoc *scu2redloc = NULL;       /* Locator to SCU2RED extension */
  double steptime;                 /* Step time per sample, sec */
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
  if ( hdr->obsmode == SMF__OBS_STARE ) {
    msgOutif(MSG__VERB," ", "Processing STARE data", status);

    if (smf_history_check( data, "smf_calc_stareimage", status) ) {
      msgOut(" ", "File contains STARE data which has already been processed: proceeding but this WILL OVERWRITE any STARE images already written into the SCU2RED extension",
	     status);
    }

    /* Number of output STARE images */
    numsamples = (data->dims)[2];

    /* If naver < 0 then re-calculate to give 1-second images */
    if ( naver < 0 ) {
      smf_fits_getD(hdr, "STEPTIME", &steptime, status);
      numaver = 1.0 / steptime;
    } else if ( naver > numsamples ) {
      msgOutif(MSG__NORM, "", "Warning: NAVER exceeds the number of samples - will average entire time stream to create a single image", status);
      numaver = (int) numsamples;
    } else {
      numaver = naver;
    }

    numimages = (int) ( numsamples / numaver );

    /* Warn user if the number to average is not a factor of the
       number of time slices  */
    remainder = numsamples % numaver;
    if ( remainder != 0 ) {
      msgSetk("R", remainder);
      msgSetk("N", numaver);
      msgSetk("T", numsamples);
      msgOutif(MSG__NORM, "", "Warning: NAVER (^N) is not a factor of the number of time slices (^T): final ^R samples will not be included in an image", status);
    }

    /* Helpful info for the user */
    msgSetk("N",numimages);
    if ( numimages == 1 ) {
      msgSetc("IM","image");
    } else {
      msgSetc("IM","images");
    }
    msgOutif( MSG__VERB, "", "Calculating ^N STARE ^IM", status );

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
      smf_average_dataD( data, j*numaver, numaver, 1, &avdata, &npts, status );

      /* Store the averaged data as an image */
      smf_store_image( data, scu2redloc, j, 2, dims, numaver, 0, 0, avdata, NULL,
		       status);

      avdata = astFree( avdata );
      zero = astFree( zero );
    }
    /* Add a history entry if everything's OK */
    smf_history_add(data, "smf_calc_stareimage",  status );

    /* Release SCU2RED locator */
    datAnnul( &scu2redloc, status );
  } else {
    msgOutif(MSG__NORM," ",
	     "Input file is not a STARE observation - ignoring", status);
  }

}

