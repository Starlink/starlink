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

#include "smurf_par.h"
#include "prm_par.h"
#include "sae_par.h"
#include "ast.h"
#include "ndf.h"
#include "mers.h"
#include "par.h"

#include "libsmf/smf.h"
#include "smurflib.h"

void smurf_extinction( int * status ) {

  /* Local Variables */
  char datatype[NDF__SZTYP];    /* String for input DATA type */
  void * indataArr[1];       /* Pointer to input data */
  float * indata = NULL;     /* Pointer to actual input data */
  int indf = 0;              /* Input NDF identifier */
  dim_t indims[2];           /* Copy of the NDF dimensions */
  int ndfdims[NDF__MXDIM];   /* Dimensions of input NDF */
  int ndims;                 /* Number of active dimensions in input */
  int nin;                   /* Number of input data points */
  int nout;                  /* Number of output data points */
  void * outdataArr[1];      /* Pointer to output data */
  float * outdata = NULL;    /* Pointer to actual output data */
  int outndf = 0;            /* Output NDF identifier */
  float tau;                 /* Zenith tau at this wavelength */
  float tauArr[2];           /* Array containing the tau values */
  float taubeg;              /* Tau at start */
  float tauend;              /* Tau at end */
  int ntau;                  /* Number of tau values read from cmd line */
  AstFrameSet * wcs = NULL;  /* Pointer to frame set */
  Grp *igrp = NULL;
  Grp *ogrp = NULL;
  int size;                  /* Number of files in input group */
  int flag;                  /* */
  int i;                     /* Counter, index */
  int outsize;               /* Total number of NDF names in the output group */

  smfDA * da;
  smfData * data;
  smfFile * file;
  smfHead * head;
  char *pname;

  /* Main routine */

  ndfBegin();

  /*  msgOut("smurf_extinction","Inside EXTINCTION", status );*/

  /* Read the input file */
  /*  ndfAssoc( "IN", "READ", &indf, status );*/
  ndgAssoc( "IN", 1, &igrp, &size, &flag, status );

  /* Get output file(s) */
  ndgCreat( "OUT", igrp, &ogrp, &outsize, &flag, status );


  for (i=1; i<=size; i++) {

    /* Q&D open the input file solely to propagate it to the output file */
    /* Copy from smurf_flatfield */
    ndgNdfas( igrp, i, "READ", &indf, status );
    ndgNdfpr( indf, " ", ogrp, i, &outndf, status );
    ndfAnnul( &indf, status);

    /* Open file and store input data in smfData struct */
    smf_open_file( igrp, i, "READ", &data, status);

    file = data->file;
    pname = file->name;
    da = data->da;

    /* Check if data are flatfielded */
    if ( da != NULL ) {
      msgOutif(MSG__VERB, " ", "Data require flatfielding", status);
      /* Call flat field routines... */

    } else { 
      msgOutif(MSG__VERB, " ", "Data already flatfielded", status);
    }

    /* Check dimensions of input data array */
    /*    ndfType( indf, "DATA", datatype, NDF__SZTYP, status);*/
    /*    ndfDim( indf, NDF__MXDIM, ndfdims, &ndims, status );*/

    /* If we have 2-D image data then get tau from command line else
       call smf_tslice_ast (_info?) to populate the sc2head and use WVM
       tau */

    /* Do we have 2-d image data? */
    if (data->ndims == 2) {
      /* Yes - ask user for optical depth */

      /* Get zenith tau from user */
      /* FUTURE: Could use parGdr0r with suggested limits based on
	 wavelength obtained from header to prevent users entering
	 unphysical values */
      parGet1r( "TAU", 2, tauArr, &ntau, status);

      /* Check number of tau values given */
      if ( ntau == 1 ) {
	taubeg = tauArr[0];
	tauend = taubeg;
      } else if (ntau == 2) {
	taubeg = tauArr[0];
	tauend = tauArr[1];
      } else {
	/* Something VERY weird happened - this should never be called */
	if ( *status == SAI__OK) {
	  *status = SAI__ERROR;
	  msgSeti("NT", ntau);
	  errRep("smurf_extinction", 
		 "Number of optical depth values given = ^NT, should be 1 or 2", status);
	}
      }
      tau = taubeg;
    
      /* Create output file as a modified version of the input file */
      /*      ndfProp( indf, "WCS", "OUT", &outndf, status );*/

      /* Obtain pointers to data array */
      /*      ndfMap( indf, "DATA", "_REAL", "READ", &indataArr, &nin, status);*/
      /*      ndfMap( outndf, "DATA", "_REAL", "WRITE", &outdataArr, &nout, status );*/

      if ( *status != SAI__OK ) {
	if ( nin != nout) {
	  *status = SAI__ERROR;
	  msgSeti( "NIN", nin);
	  msgSeti( "NOUT", nout);
	  errRep( "smurf_extinction", "Number of input pixels not equal to the number of output pixels (^NIN != ^NOUT)", status);
	}
      }

      /* Check for existence of VARIANCE array */

      /* Check for covariance */
      /* smf_find_extension( indf, "COVAR", &cndf, status ); */

      if ( *status == SAI__OK ) {
	indata = indataArr[0];
	outdata = outdataArr[0];
      }

      /* Need the dimensions of the data array */
      /*  ndfDim( indf, NDF__MXDIM, ndfdims, &ndims, status );*/

      if ( *status == SAI__OK ) {
	indims[0] = (dim_t)ndfdims[0];
	indims[1] = (dim_t)ndfdims[1];
      }
  
      /* Get the world coordinate information */
      ndfGtwcs( indf, &wcs, status );

      /* Select the AZEL system */
      if ( (wcs != NULL) || (*status == SAI__OK) ) 
	astSetC( wcs, "SYSTEM", "AZEL" );

      if (!astOK) {
	if (*status == SAI__OK) {
	  *status = SAI__ERROR;
	  errRep( "extinction", "Error from AST", status);
	}
      }

      if (*status == SAI__OK) 
	memmove( outdata, indata, (size_t)(nin*sizeof(indata[0])) );

      /* New API for following: pass smfData struct */
      /*      smf_correct_extinction( data, tau, status);*/
    

    } else if (data->ndims == 3 ) {

      /* Loop over number of time slices */
      /* Retrieve the specific timeslice */

      smf_tslice_ast( data, i, status);

      /* HACK: get it to complain when time-series data are passed */
      if ( *status == SAI__OK) {
	*status = SAI__ERROR;
	msgSeti("ND", data->ndims);
	errRep("smurf_extinction", 
	       "Number of dimensions of input file, ^ND, not yet supported", status);
      }
    } else {
      /* HACK: get it to complain when time-series data are passed */
      if ( *status == SAI__OK) {
	*status = SAI__ERROR;
	msgSeti("ND", data->ndims);
	errRep("smurf_extinction", 
	       "Number of dimensions of input file, ^ND, not yet supported", status);
      }
    }

  }
  /* Tidy up after ourselves: release the resources used by the grp routines  */
  grpDelet( &igrp, status);
  grpDelet( &ogrp, status);

  ndfEnd( status );
}
