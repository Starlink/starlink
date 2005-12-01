/*
*+
*  Name:
*     smurf_makemap

*  Purpose:
*     Top-level MAKEMAP implementation

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     smurf_makemap( int *status );

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This is the main routine implementing the MAKEMAP task.

*  ADAM Parameters:


*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     Andy Gibb (UBC)
*     Edward Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2005-09-27 (EC)
*        Clone from smurf_extinction
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
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 59 Temple Place,Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#if HAVE_CONFIG_H
#include <config.h>
#endif

#include <string.h>

#include "star/ndg.h"
#include "star/grp.h"

#include "smurf_par.h"
#include "prm_par.h"
#include "sae_par.h"
#include "ast.h"
#include "ndf.h"
#include "mers.h"

#include "libsmf/smf.h"
#include "smurflib.h"

#include "sc2da/sc2store_par.h"
#include "sc2da/sc2store_struct.h"
#include "sc2da/sc2math.h"
#include "sc2da/sc2store.h"
#include "sc2da/sc2ast.h"

void smurf_makemap( int *status ) {

  /* Local Variables */
  smfData *data=NULL;        /* pointer to SCUBA2 data structure */
  void **data_index;         /* Array of pointers to mapped arrays in ndf */
  smfFile *file=NULL;        /* SCUBA2 data file information */
  AstFitsChan *fitschan;     /* Fits channels to construct WCS header */
  AstFrameSet *outframeset;  /* Frameset for pixel->radec mapping */
  int flag;                  /* Flag */
  smfHead *hdr=NULL;         /* Pointer to data header this time slice */
  int i;                     /* Loop counter */
  Grp *igrp = NULL;          /* Group of input files */
  void *indataArr[1];        /* Pointer to input data */
  float *indata = NULL;      /* Pointer to actual input data */
  int indf = 0;              /* Input NDF identifier */
  dim_t indims[3];           /* Copy of the NDF dimensions */
  int j;                     /* Loop counter */
  int lbnd_in[2];            /* Lower pixel bounds for input maps */
  int lbnd_out[2];           /* Lower pixel bounds for output map */
  void *map=NULL;            /* Pointer to the rebinned map data */
  AstMapping *coordmap1;     /* Mapping bolo->celestial coordinates */
  AstMapping *coordmap2;     /* Mapping celestial->output map coordinates */
  AstCmpMap *coordmap;       /* Combination of coordmap1 and coordmap2 */
  int n;                     /* # elements in the output map */
  int ndfdims[NDF__MXDIM];   /* Dimensions of input NDF */
  int ndims;                 /* Number of active dimensions in input */
  int ondf;                  /* output NDF identifier */
  char *pname=NULL;          /* Name of currently opened data file */
  struct sc2head *sc2hdr=NULL; /* Pointer to sc2head for this time slice */
  int size;                  /* Number of files in input group */
  float tau;                 /* tau at this wavelength */
  int ubnd_in[2];            /* Upper pixel bounds for input maps */
  int ubnd_out[2];           /* Upper pixel bounds for output map */
  double *weights=NULL;      /* Weights array for output map */

  /* Main routine */
  ndfBegin();
  
  /* Get group of input files */
  ndgAssoc( "IN", 1, &igrp, &size, &flag, status );

  /* Create output ndf */

  /*ndgNdfas( igrp, 0, "READ", &indf, status );
    ndfProp( indf, "HISTORY", "OUT", &ondf, status );
    ndfAnnul( &indf, status); */

  lbnd_out[0] = 1;    /* dummy bounds for the output data */
  lbnd_out[1] = 512;
  ubnd_out[0] = 1;
  ubnd_out[1] = 512;

  ndfCreat( "OUT", "_REAL", 2, lbnd_out, ubnd_out, &ondf, status );
  ndfMap( ondf, "DATA", "_REAL", "WRITE", data_index, &n, status);

  map = data_index[0];

  weights = (double *) malloc( sizeof(double)*512*512 );

  /* First pass loop over all data to calculate output map dimensions */

  /* Second pass loop over all data to identify the extent of the map */

  for(i=1; i<=size; i++ ) {
    /* Read data from the ith input file in the group */
    smf_open_file( igrp, i, "READ", &data, status);

    file = data->file;
    pname =  file->name;
    msgSetc("FILE", pname);
    /*msgOutif(MSG__VERB, " ", "SMURF_MAKEMAP: check map dims from file ^FILE",
      status);*/
    msgOut(" ", "SMURF_MAKEMAP: check map dims from file ^FILE", 
	     status);
    
    /* Check that the data dimensions are 3 and get # time samples */
    if( data->ndims != 3 ) {
      *status = SAI__ERROR;
    }

    /* Get the astrometry for all the time slices in this data file */
    if( *status == SAI__OK) for( j=0; j<data->dims[2]; j++ ) {
      /*msgSeti("SLICE", j);
	msgOut(" ", "time: ^SLICE", status);*/

      smf_tslice_ast( data, j, status);

      hdr = data->hdr;
      sc2hdr = hdr->sc2head;

      /* Create a mapping for the output map from pixels -> RA, Dec 
	 using the base coordinates to get the coordinates of the tangent point
	 Note: Ast assumes angular quantities are in degrees */

      if( (i == 1) && (j == 0) )
      { 
	fitschan = astFitsChan ( NULL, NULL, "" );
   
	sc2ast_makefitschan( 256.0, 256.0, 0.001, 0.001, 
			     sc2hdr->tcs_tr_bc1*57.29577951, 
			     sc2hdr->tcs_tr_bc2*57.29577951, 
			     "RA---TAN", "DEC--TAN", 
			     fitschan, status );

	astClear( fitschan, "Card" );

	outframeset = astRead( fitschan );

	/* Set the System attribute for the SkyFrame in the reference WCS 
	   FrameSet to ICRS and extract the IRCS->REF_PIXEL mapping. */
	astSet( outframeset, "system=icrs" );

	coordmap1 = astGetMapping( outframeset, AST__CURRENT, AST__BASE );

      }

      /* Set the System attribute for the SkyFrame in the input WCS FrameSet
	 to ICRS and extract the IN_PIXEL->IRCS mapping. */
      astSet( data->hdr->wcs, "system=icrs" );
      coordmap2 = astGetMapping( data->hdr->wcs, AST__BASE, AST__CURRENT );

      /* Concatenate the two Mappings to get the IN_PIXEL->REF_PIXEL Mapping */
      coordmap = astCmpMap( coordmap1, coordmap2, 1, "" );

      lbnd_in[0] = 1;
      lbnd_in[1] = 1;
      ubnd_in[0] = (data->dims)[0];
      ubnd_in[1] = (data->dims)[1];

      if( data->dtype == SMF__FLOAT )
      {	
	/* msgOut(" ", "SMURF_MAKEMAP: Rebin timeslice", status); */
	astRebinSeqF(coordmap,0,
		     2,lbnd_in, ubnd_in,(data->pntr)[0], NULL, 
		     0, NULL, 0, 0, 0, -1,
		     2,lbnd_out,ubnd_out,
		     lbnd_in, ubnd_in,
		     map, NULL, weights);
      }
    }
    
    /* close data file */
    smf_close_file( &data, status);


  }

  ndfUnmap( ondf, "DATA", status);
  ndfAnnul( &ondf, status );
  ndfEnd( status );

  msgOutif(MSG__VERB," ","Map made", status);

  /* Tidy up after ourselves: release resources used by the grp routines  */
  grpDelet( &igrp, status);




}

