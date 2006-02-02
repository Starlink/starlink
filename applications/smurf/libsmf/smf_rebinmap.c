/*
*+
*  Name:
*     smf_rebinmap

*  Purpose:
*     Map-maker that simply rebins the data

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_rebinmap( Grp *igrp, int size, AstFrameSet *outframeset,
*                   int *lbnd_out, int *ubnd_out, 
*                   double *map, double *variance, double *weights,
*         	    int *status );

*  Arguments:
*     igrp = Grp* (Given)
*        Group of timestream NDF data files
*     size = int (Given)
*        Number of elements in igrp
*     outframeset = AstFrameSet* (Given)
*        Frameset containing the sky->output map mapping
*     lbnd_out = double* (Given)
*        2-element array pixel coord. for the lower bounds of the output map 
*     ubnd_out = double* (Given)
*        2-element array pixel coord. for the upper bounds of the output map 
*     map = double* (Returned)
*        The output map array 
*     variance = double* (Returned)
*        Variance of each pixel in map
*     weights = double* (Returned)
*        Relative weighting for each pixel in map
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This function does a simple regridding of data into a map
*     
*  Authors:
*     Edward Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2006-02-02 (EC):
*        Initial version.
*     {enter_further_changes_here}

*  Notes:
*     Currently lon_0 and lat_0 are interpreted only as ra/dec of tangent point

*  Copyright:
*     Copyright (C) 2005-2006 Particle Physics and Astronomy Research Council.
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

#include <stdio.h>

/* Starlink includes */
#include "ast.h"
#include "mers.h"
#include "sae_par.h"
#include "star/ndg.h"
#include "prm_par.h"

/* SMURF includes */
#include "libsmf/smf.h"

void smf_rebinmap( Grp *igrp,  int size, AstFrameSet *outframeset,
                   int *lbnd_out, int *ubnd_out, double *map, double *variance,
		   double *weights, int *status ) {

  /* Local Variables */
  AstMapping *bolo2sky=NULL;    /* Mapping bolo->celestial coordinates */
  AstCmpMap *bolo2map=NULL;     /* Combined mapping bolo->map coordinates */
  smfData *data=NULL;           /* pointer to  SCUBA2 data struct */
  smfFile *file=NULL;           /* SCUBA2 data file information */
  smfHead *hdr=NULL;            /* Pointer to data header this time slice */
  dim_t i;                      /* Loop counter */
  dim_t j;                      /* Loop counter */
  int lbnd_in[2];               /* Lower pixel bounds for input maps */
  int nbolo=0;                  /* # of bolometers in the sub-array */
  char *pname=NULL;             /* Name of currently opened data file */
  int rebinflags;               /* Control the rebinning procedure */
  struct sc2head *sc2hdr=NULL;  /* Pointer to sc2head for this time slice */
  AstMapping *sky2map=NULL;     /* Mapping celestial->map coordinates */
  int ubnd_in[2];               /* Upper pixel bounds for input maps */
  char *system;                 /* System */
  char wcssystem[81];           /* String containing system attribute */

  /* Main routine */
  if (*status != SAI__OK) return;

  /* Get the system from the outframeset */
  system = astGetC( outframeset, "system" );

  /* String for setting the system attribute */
  sprintf(wcssystem,"system=%s",system);

  for(i=1; i<=size; i++ ) {
    /* Read data from the ith input file in the group */      
    smf_open_file( igrp, i, "READ", 1, &data, status );
    
    if( *status == SAI__OK ) {
      file = data->file;
      pname =  file->name;
      msgSetc("FILE", pname);
      msgSeti("THISFILE", i);
      msgSeti("NUMFILES", size);
      msgOutif(MSG__VERB, " ", 
	       "SMF_REBINMAP: Processing ^THISFILE/^NUMFILES ^FILE",
	       status);
    }
    else
      errRep( "smf_rebinmap", "Couldn't open input file.", status );

    /* Check that the data dimensions are 3 (for time ordered data) */
    if( *status == SAI__OK ) {
      if( data->ndims != 3 ) {
	msgSetc("FILE", pname);
	msgSeti("THEDIMS", data->ndims);
	*status = SAI__ERROR;
	errRep("smf_rebinmap", 
	       "^FILE data has ^THEDIMS dimensions, should be 3.", 
	       status);
      }  else {
	/* # bolometers */
	nbolo = (data->dims)[0] * (data->dims)[1];
      }
    }

    /* Check that the input data type is double precision */
    if( *status == SAI__OK ) 
      if( data->dtype != SMF__DOUBLE) {
	msgSetc("FILE", pname);
	msgSetc("DTYPE",smf_dtype_string( data, status ));
	*status = SAI__ERROR;
	errRep("smurf_makemap", 
	       "^FILE has ^DTYPE data type, should be DOUBLE.",
	       status);
      }

    if( *status == SAI__OK) {
            
      /* Loop over all time slices in the data */      
      for( j=0; j<(data->dims)[2]; j++ ) {
	
	smf_tslice_ast( data, j, status);
	
	if( *status == SAI__OK ) {
	  hdr = data->hdr;
	  sc2hdr = hdr->sc2head;
	  
	  /* Calculate bounds in the input array */

	  lbnd_in[0] = 0;
	  lbnd_in[1] = 0;
	  ubnd_in[0] = (data->dims)[0]-1;
	  ubnd_in[1] = (data->dims)[1]-1;

	  /* Get bolo -> sky mapping 
	     Set the System attribute for the SkyFframe in input WCS 
	     FrameSet and extract the IN_PIXEL->Sky mapping. */	  

	  astSet( data->hdr->wcs, wcssystem );
	  bolo2sky = astGetMapping( data->hdr->wcs, AST__BASE, 
				    AST__CURRENT );
	  
	  /* Create sky to output grid mapping 
	     using the base coordinates to get the coordinates of the 
	     tangent point if it hasn't been done yet. */
	  
	  if( sky2map == NULL ) { 
	    /* Extract the Sky->REF_PIXEL mapping. */
	    astSet( outframeset, wcssystem );
	    sky2map = astGetMapping( outframeset, AST__CURRENT, 
				     AST__BASE );
	  }
	  
	  /* Concatenate Mappings to get IN_PIXEL->REF_PIXEL Mapping */
	  bolo2map = astCmpMap( bolo2sky, sky2map, 1, "" );


	  /*  Rebin this time slice*/
	  rebinflags = 0;
	  if( (i == 1) && (j == 0) )                    /* Flags start rebin */
	    rebinflags = rebinflags | AST__REBININIT;

	  if( (i == size) && (j == (data->dims)[2]-1) ) /* Flags end rebin */
	    rebinflags = rebinflags | AST__REBINEND;
	  
	  astRebinSeqD(bolo2map,0,
		       2,lbnd_in, ubnd_in,
		       (data->pntr)[0] + j*nbolo*sizeof(double), 
		       NULL, 
		       AST__NEAREST, NULL, rebinflags, 0, 0, VAL__BADD,
		       2,lbnd_out,ubnd_out,
		       lbnd_in, ubnd_in,
		       map, variance, weights);
	  

	  /* clean up ast objects */
	  if( bolo2sky != NULL ) {
	    astAnnul( bolo2sky );
	    bolo2sky = NULL;
	  }
	  
	  if( bolo2map != NULL ) {
	    astAnnul( bolo2map );
	    bolo2map = NULL;
	  }
	}

	/* Break out of loop over time slices if bad status */
	if (*status != SAI__OK) goto CLEANUP;

      }
    }

    /* Close the data file */
    if( data != NULL ) {
      smf_close_file( &data, status);
      data = NULL;
    }

    /* Break out of loop over data files if bad status */
    if (*status != SAI__OK) goto CLEANUP;
  }

  /* Clean Up */
 
 CLEANUP:
  if( sky2map != NULL )
    astAnnul( sky2map );
    
  if( bolo2sky != NULL ) 
    astAnnul( bolo2sky );
	  
  if( bolo2map != NULL )
    astAnnul( bolo2map );

  if( data != NULL )
    smf_close_file( &data, status);

}
