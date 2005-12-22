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

#include "smurf_par.h"
#include "prm_par.h"
#include "sae_par.h"
#include "ast.h"
#include "ndf.h"
#include "mers.h"
#include "par.h"
#include "star/ndg.h"

#include "libsmf/smf.h"
#include "smurflib.h"

void smurf_extinction( int * status ) {

  /* Local Variables */
  int indf = 0;              /* Input NDF identifier */
  int nin;                   /* Number of input data points */
  int nout;                  /* Number of output data points */
  int outndf = 0;              /* Output NDF identifier */
  float tau;                 /* Zenith tau at this wavelength */
  /*float tauArr[2];*/           /* Array containing the tau values */
  float taubeg;              /* Tau at start */
  /*float tauend;*/              /* Tau at end */
  /*int ntau;*/                  /* Number of tau values read from cmd line */
  /*AstFrameSet * iwcs = NULL;*/  /* Pointer to input frame set */
  Grp *igrp = NULL;
  Grp *ogrp = NULL;
  int size;                  /* Number of files in input group */
  int flag;                  /* */
  int i;                     /* Counter, index */
  int outsize;               /* Total number of NDF names in the output group */
  int nframes;               /* Number of time slices (frames) in file */

  /* Unused as yet */
  /*  void * outdataArr[1];     */     /* Pointer to output data */
  /* float * outdata = NULL;    */     /* Pointer to actual output data */
  AstFrameSet *owcs = NULL;     /* Pointer to output frame set */
  /* int ndfdims[NDF__MXDIM];   */     /* Dimensions of input NDF */
  /* int ndims;                 */     /* Number of active dimensions in input */
  /* dim_t indims[2];           */     /* Copy of the NDF dimensions */
  /* void * indataArr[1];       */     /* Pointer to input data */
  /* float * indata = NULL;     */     /* Pointer to actual input data */
  /* char datatype[NDF__SZTYP]; */     /* String for input DATA type */

  smfData *idata;
  /*  smfDA *ida = NULL;
  smfFile *ifile;
  smfHead *ihdr;*/
  /*  char *ipname;*/

  smfData *odata = NULL;
  /*  smfFile *ofile;*/
  /*  smfDA * oda;*/
  smfHead *ohdr;
  /*  char *opname;*/

  /*  dim_t index;*/
  int j;
  double *outdata;

  /* Main routine */

  ndfBegin();

  /*  msgOut("smurf_extinction","Inside EXTINCTION", status );*/

  /* Read the input file */
  ndgAssoc( "IN", 1, &igrp, &size, &flag, status );

  /* Get output file(s) : assumes a 1:1 correspondence between input
     and output files */
  ndgCreat( "OUT", igrp, &ogrp, &outsize, &flag, status );

  for (i=1; i<=size; i++) {

    /* Can we check at this stage if flatfield has been applied? */

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
      errRep("smurf_flatfield",
	     "Flatfielding error: status set bad on return from smf_flatfield", 
	     status);
    }

    /* Do we have 2-d image data? */
    if (odata->ndims == 2) {
      /* Yes - ask user for ZENITH optical depth */

      /* FUTURE: Could use parGdr0r with suggested limits based on
	 wavelength obtained from header to prevent users entering
	 unphysical values */
      /*      parGet1r( "ZENTAU", 2, tauArr, &ntau, status);*/
      parGet0r( "ZENTAU", &taubeg, status);

      /* Check number of tau values given */
      tau = taubeg;
    
      /* Check for existence of VARIANCE array */
      /* Check for covariance */
      /* smf_find_extension( indf, "COVAR", &cndf, status ); */

      nin = (idata->dims)[0] * (idata->dims)[1];
      nout = (odata->dims)[0] * (odata->dims)[1];

      if ( *status != SAI__OK ) {
	if ( nin != nout) {
	  *status = SAI__ERROR;
	  msgSeti( "NIN", nin);
	  msgSeti( "NOUT", nout);
	  errRep( "smurf_extinction", "Number of input pixels not equal to the number of output pixels (^NIN != ^NOUT)", status);
	}
      }

      smf_correct_extinction( odata, tau, status);
    
    } else if (odata->ndims == 3 ) {

      /* Loop over number of time slices/frames */
      nframes = (odata->dims)[2];
      for ( j=0; j<nframes; j++) {
	/* Call tslice_ast to update the WCS info for the particular timeslice */
	smf_tslice_ast( odata, j, status);
	ohdr = odata->hdr;
	ohdr->curslice = j;
	tau = 0.1;
	smf_correct_extinction( odata, tau, status );

      }

      /* Print something useful */

      
    } else {
      /* Abort with an error if the number of dimensions is not 2 or 3 */
      if ( *status == SAI__OK) {
	*status = SAI__ERROR;
	msgSeti("ND", idata->ndims);
	errRep("smurf_extinction", 
	       "Number of dimensions of input file, ^ND, should be 2 or 3", status);
      }
    }

  }
  /* Tidy up after ourselves: release the resources used by the grp routines  */
  grpDelet( &igrp, status);
  grpDelet( &ogrp, status);

  ndfEnd( status );
}
