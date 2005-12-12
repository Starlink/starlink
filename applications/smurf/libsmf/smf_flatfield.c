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

*  Authors:
*     Andy Gibb (UBC)
*     {enter_new_authors_here}

*  History:
*     2005-12-01 (AGG):
*        Initial test version.
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

  int checkflatstatus = 0;
  int i;
  int npts;
  int j;
  int nframes;
  int nboll;                    /* Number of bolometers */

  smfDA *da;
  smfFile *file;
  smfHead *hdr;

  void *ipntr[3];
  void *opntr[3];
  double *indata;
  double *outdata;
  int *tstream;

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
    /* OK data are not flatfielded: create smfData and flatten */

    /* Check *odata */
    if ( *odata == NULL) {

      /* If NULL then we need create odata not associated with a file */
      /* Allocate space for *odata and all cpts */
      *odata = malloc( sizeof( smfData ) );
      hdr = malloc( sizeof( smfHead ) );
      /* Copy old hdr into the new hdr */
      memcpy( hdr, idata->hdr, sizeof( smfHead ) );
      (*odata)->hdr = hdr; /* Store hdr in odata */
      /* Set data type */
      (*odata)->dtype = SMF__DOUBLE; 

      /* Copy flatfield info to output struct */
      da = malloc( sizeof( smfDA ) );
      memcpy( da, idata->da, sizeof( smfDA ));
      (*odata)->da = da;

      /* Set ndims and the dims array */
      (*odata)->ndims = idata->ndims; 
      for ( i=0; i<(*odata)->ndims; i++ ) {
	((*odata)->dims)[i] = (idata->dims)[i];
      }

      /* Check if dtype == SMF__DOUBLE */

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
	  *status == SAI__ERROR;
	  errRep( "smf_flatfield", "Output data type not set to double", status);
	}
      }

      smf_flatten( *odata, status);
      msgOut("smf_flatfield","Data not FF, no odata", status);
    } else {

      /* Check dimensions and type */

      msgOut("smf_flatfield","Data not FF, odata exists", status);
    }


  } else {
    msgOut("smf_flatfield", 
	   "Hmm status is something other than it should be....", status);
  }

  *status = checkflatstatus;
}
