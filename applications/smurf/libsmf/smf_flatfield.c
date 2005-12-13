/*
*+
*  Name:
*     smf_flatfield

*  Purpose:
*     Mid-level FLATFIELD implementation

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     SMURF subroutine

*  Invocation:
*     smf_flatfield( smfData *data, int *status );

*  Arguments:
*     data = smfData* (Given and Returned)
*        Pointer to a smfData struct
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This is a handler routine for determining if the lower-level
*     FLATFIELD task needs to be run. 

*  Notes:
*     - If an NDF file is involved then at the very least, the pointer
*     to the DATA array (pntr[0]) and its type (dtype) must be
*     defined.

*  Authors:
*     Andy Gibb (UBC)
*     {enter_new_authors_here}

*  History:
*     2005-12-01 (AGG):
*        Initial test version.
*     2005-12-12 (AGG):
*        Add checks on data type and dimensions for the case that the
*        data need flatfielding and odata exists
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2005 Particle Physics and Astronomy Research Council.
*     University of British Columbia.
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
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#if HAVE_CONFIG_H
#include <config.h>
#endif

#include <string.h>
#include <stdio.h>

#include "mers.h"
#include "prm_par.h"
#include "sae_par.h"
#include "msg_par.h"

#include "smurf_par.h"
#include "libsmf/smf.h"
#include "libsmf/smf_err.h"

void smf_flatfield ( const smfData *idata, smfData **odata, int *status ) {

  int checkflatstatus = 0;    /* Local variable for storing status */
  int i;                      /* Loop counter */
  int indims;
  int j;                      /* Loop counter */
  int nboll;                  /* Number of bolometers */
  int ndims;
  int nframes;                /* Number of time slices */
  int npts;                   /* Total number of data points */

  smfDA *da = NULL;           /* da struct for input data */
  smfFile *file = NULL;       /* file struct for input data */
  smfHead *hdr = NULL;        /* hdr struct for input data */

  void *ipntr[3];             /* Input D, Q and V arrays */
  void *opntr[3];             /* Output D, Q and V arrays */
  double *indata;             /* Pointer to input DATA */
  double *outdata;            /* Pointer to output DATA */
  int *tstream;               /* Pointer to raw time series data */
  smf_dtype dtype;            /* Data type */

  if ( *status != SAI__OK ) return;

  smf_check_flat( idata, status );
  /* Store status and annul for messaging system */
  checkflatstatus = *status;
  errAnnul(status);

  /* Data are flatfielded if status set to SMF__FLATN */
  if ( checkflatstatus == SMF__FLATN ) {

    /* check *odata */
    if ( *odata == NULL) {

      /* If NULL then we need to clone idata to odata */
      npts = (idata->dims)[0]*(idata->dims)[1]*(idata->dims)[2];
      ipntr[0] = (idata->pntr)[0];
      indata = ipntr[0];

      /*      for ( i=0; i<npts; i++) {
	printf("i = %d, indata = %g \n",i,indata[i]);
	}*/
      /*      *odata = smf_clone_data( idata, status );*/
      smf_clone_data( idata, odata, status );

      msgOut("smf_flatfield","Data FF, no odata", status);

    } else {

      /* Check dimensions and type */

      msgOut("smf_flatfield","Data FF, odata exists", status);
    }

  } else if ( checkflatstatus == SAI__OK ) { 

    /* OK data are not flatfielded: create smfData based on input and
       apply flatfield */

    /* Check if *odata exists */
    if ( *odata == NULL) {

      msgOutif(MSG__VERB, "smf_flatfield","Data not FF, no odata", status);
      /* If NULL then we need create odata not associated with a file */
      /* Allocate space for *odata and all necessary cpts */
      *odata = malloc( sizeof( smfData ) );
      hdr = malloc( sizeof( smfHead ) );
      da = malloc( sizeof( smfDA ) );

      /* Copy old hdr into the new hdr */
      memcpy( hdr, idata->hdr, sizeof( smfHead ) );
      /* Store hdr in odata */
      (*odata)->hdr = hdr; 

      /* Copy flatfield info to output struct */
      memcpy( da, idata->da, sizeof( smfDA ));
      (*odata)->da = da;

      /* Set data type */
      (*odata)->dtype = SMF__DOUBLE; 
      /* Set ndims and the dims array */
      (*odata)->ndims = idata->ndims; 
      for ( i=0; i<(*odata)->ndims; i++ ) {
	((*odata)->dims)[i] = (idata->dims)[i];
      }

      /* Useful numbers... */
      nboll = ((*odata)->dims)[0]*((*odata)->dims)[1];
      nframes = ((*odata)->dims)[2];
      npts = nboll * nframes;

      /* Store pointer to input data */
      ipntr[0] = (idata->pntr)[0];
      tstream = ipntr[0];

      /* Allocate space for output data */
      if ( (*odata)->dtype == SMF__DOUBLE ) {
	opntr[0] = malloc( npts * sizeof( double ) );
	outdata = opntr[0];
	/* Input data are ints: must re-cast as double */
	for (j=0; j<npts; j++) {
	  outdata[j] = (double)tstream[j];
	}
	/* Store output data */
	((*odata)->pntr)[0] = opntr[0];
      } else {
	/* Return an error: the output data must be doubles */
	if ( *status == SAI__OK) {
	  *status = SAI__ERROR;
	  errRep( "smf_flatfield", "Output data type not double", status);
	}
      }

      /* Apply flatfield calibration */
      smf_flatten( *odata, status);

    } else {

      /* OK, *odata exists */
      msgOutif(MSG__VERB, "smf_flatfield","Data not FF, odata exists", status);

      /* To proceed we must know that the data type is correct */
      dtype = (*odata)->dtype;
      /* And that the input and output dimensions are equal */
      ndims = (*odata)->ndims; 
      indims = idata->ndims; 

      if ( dtype != SMF__DOUBLE ) {
        *status = SAI__ERROR;
        errRep( "smf_flatfield", "Output data type is not set to _DOUBLE", status);
      } else if ( ndims != indims ) {
        msgSeti( "NDIMS", ndims);
        msgSeti( "IDIMS", indims);
        *status = SAI__ERROR;
        errRep( "smf_flatfield", "Number of dimensions in output, ^NDIMS, is not equal to number in input, ^IDIMS", status);
      } else {

	/* All's well so start populating the *odata struct */
	/* Does the header (hdr) exist? */
	hdr = (*odata)->hdr;
	if ( (*odata)->hdr == NULL) {
	  hdr = malloc( sizeof( smfHead ) );
	  /* Copy old hdr into the new hdr and store in *odata */
	  memcpy( hdr, idata->hdr, sizeof( smfHead ) );
	  (*odata)->hdr = hdr; 
	}

	/* Does the flatfield (smfDA) struct exist? */
	da = (*odata)->da;
	if ( da == NULL ) {
	  da = malloc( sizeof( smfDA ) );
	  /* Copy flatfield info into *odata */
	  memcpy( da, idata->da, sizeof( smfDA ) );
	  (*odata)->da = da;
	}

	/* Store pointer to input data */
	ipntr[0] = (idata->pntr)[0];
	tstream = ipntr[0];

	/* Store pointer to output data */
	opntr[0] = ((*odata)->pntr)[0];
	outdata = opntr[0];

	/* Number of bolometers, number of frames and total npts */
	nboll = ((*odata)->dims)[0]*((*odata)->dims)[1];
	nframes = ((*odata)->dims)[2];
	npts = nboll * nframes;

	/* Input data are ints: must re-cast as double */
	for (j=0; j<npts; j++) {
	  outdata[j] = (double)tstream[j];
	}

	((*odata)->pntr)[0] = opntr[0];
	/* Apply flatfield calibration */
	smf_flatten( *odata, status);

      }
    }
  } else {
    msgOut("smf_flatfield", 
	   "Hmm status is something other than it should be....", status);
  }

  *status = checkflatstatus;
}
