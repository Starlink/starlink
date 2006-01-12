/*
*+
*  Name:
*     smurf_extinction

*  Purpose:
*     Top-level EXTINCTION implementation

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     smurf_extinction( int *status );

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This is the main routine implementing the EXTINCTION task.

*  ADAM Parameters:


*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     Andy Gibb (UBC)
*     Edward Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2005-09-27 (TIMJ):
*        Initial test version
*     2005-09-27 (AGG):
*        Now uses smurf_par.h
*        Factored out extinction correction
*     2005-09-27 (EC)
*        Trap memmove when status is bad
*     2005-11-09 (AGG)
*        Allow user to specify the optical depth at the start and end
*        of a scan - could be useful if we supply a group of files
*     2005-11-10 (AGG)
*        Perform check for dimensionality of input file and prompt
*        user only for 2-D image data. Now uses Grp interface for
*        setting input/output files and stores data in a smfData struct.
*     2005-12-20 (AGG):
*        Calls smf_flatfield to automatically flatfield data if necessary.
*     2005-12-21 (AGG):
*        Now deals with timeseries data
*     2006-01-10 (AGG):
*        Now reads the tau from the header for timeseries data
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2005-2006 Particle Physics and Astronomy Research
*     Council. University of British Columbia. All Rights Reserved.

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

/* Standard includes */
#include <string.h>
#include <stdio.h>

/* Starlink includes */
#include "prm_par.h"
#include "sae_par.h"
#include "ast.h"
#include "ndf.h"
#include "mers.h"
#include "par.h"
#include "star/ndg.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "smurflib.h"
#include "smurf_par.h"

/* Simple default string for errRep */
#define FUNC_NAME "smurf_extinction"

void smurf_extinction( int * status ) {

  /* Local Variables */
  int flag;                  /* Flag for how group is terminated */
  int i;                     /* Loop counter */
  smfData *idata;            /* Data struct read from input file */
  Grp *igrp = NULL;          /* Input group */
  int indf = 0;              /* Input NDF identifier */
  int j;                     /* Loop counter */
  int nframes;               /* Number of time slices (frames) in file */
  int nin;                   /* Number of input data points */
  int nout;                  /* Number of output data points */
  smfData *odata = NULL;     /* Output data struct */
  Grp *ogrp = NULL;          /* Output group */
  smfHead *ohdr;             /* Pointer to header in odata */
  double *outdata;           /* Pointer to output data array */
  int outsize;               /* Total number of NDF names in the output group */
  int outndf = 0;            /* Output NDF identifier */
  AstFrameSet *owcs = NULL;  /* Pointer to output frame set */
  int size;                  /* Number of files in input group */
  float tau = 0.0;           /* Zenith tau at this wavelength */
  smfData *tdata = NULL;     /* Pointer to individual timeslice data struct */

  /* Main routine */

  ndfBegin();

  /*  msgOut( FUNC_NAME, "Inside EXTINCTION", status );*/

  /* Read the input file */
  ndgAssoc( "IN", 1, &igrp, &size, &flag, status );

  /* Get output file(s) : assumes a 1:1 correspondence between input
     and output files */
  ndgCreat( "OUT", igrp, &ogrp, &outsize, &flag, status );

  for (i=1; i<=size; i++) {

    /* FUTURE: Can we check at this stage if flatfield has been applied? */

    /* Open the input file solely to propagate it to the output file */
    ndgNdfas( igrp, i, "READ", &indf, status );
    ndgNdfpr( indf, "DATA", ogrp, i, &outndf, status );
    ndfAnnul( &indf, status);

    /* Open input file and store data in smfData struct */
    smf_open_file( igrp, i, "READ", &idata, status);

    /* Open output file */
    ndfStype( "_DOUBLE", outndf, "DATA", status);
    ndfMap( outndf, "DATA", "_DOUBLE", "WRITE", &outdata, &nout, status );
    ndfAnnul( &outndf, status);

    smf_open_file( ogrp, i, "WRITE", &odata, status);

    ohdr = idata->hdr;

    owcs = ohdr->wcs;
    /*    astShow(owcs);*/

    /* Flatfield if necessary */
    smf_flatfield( idata, &odata, status );

    if ( *status != SAI__OK ) {
      errRep(FUNC_NAME,
	     "Flatfielding error: status set bad on return from smf_flatfield", 
	     status);
    }

    /* FUTURE: Remove sky */

    /* Do we have 2-d image data? */
    if (odata->ndims == 2) {
      /* Yes - ask user for ZENITH optical depth */

      /* FUTURE: Could use parGdr0r with suggested limits based on
	 wavelength obtained from header to prevent users entering
	 unphysical values */

      /* Ask for tau */
      parGet0r( "ZENTAU", &tau, status);

      /* Check for existence of VARIANCE array */
      /* Check for covariance */
      /* smf_find_extension( indf, "COVAR", &cndf, status ); */

      nin = (idata->dims)[0] * (idata->dims)[1];
      nout = (odata->dims)[0] * (odata->dims)[1];

      /* Check input and output arrays are the same size */
      if ( *status != SAI__OK ) {
	if ( nin != nout) {
	  *status = SAI__ERROR;
	  msgSeti( "NIN", nin);
	  msgSeti( "NOUT", nout);
	  errRep( FUNC_NAME, "Number of input pixels not equal to the number of output pixels (^NIN != ^NOUT)", status);
	}
      }

      /* Call the extinction correction routine */
      smf_correct_extinction( odata, tau, status);
    
    } else if (odata->ndims == 3 ) {

      /* Loop over number of time slices/frames */
      nframes = (odata->dims)[2];
      for ( j=0; j<nframes; j++) {
	/* Call tslice_ast to update the WCS info for the particular timeslice */
	smf_tslice_ast( odata, j, status );
	smf_tslice( odata, &tdata, j, status );
	smf_correct_extinction( tdata, tau, status );
	smf_insert_tslice( &odata, tdata, j, status );
      }

    } else {
      /* Abort with an error if the number of dimensions is not 2 or 3 */
      if ( *status == SAI__OK) {
	*status = SAI__ERROR;
	msgSeti("ND", idata->ndims);
	errRep(FUNC_NAME,
	       "Number of dimensions of input file, ^ND, should be 2 or 3", 
	       status);
      }
    }

  }
  /* Tidy up after ourselves: release the resources used by the grp routines  */
  grpDelet( &igrp, status);
  grpDelet( &ogrp, status);

  ndfEnd( status );
}
