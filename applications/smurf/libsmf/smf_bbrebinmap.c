/*
*+
*  Name:
*     smf_bbrebinmap

*  Purpose:
*     Map-maker that simply rebins the data, applying a bad bolometer
*     mask if one exists.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_bbrebinmap( smfData *data, int index, int size, 
*                    AstFrameSet *outfset,
*                   int *lbnd_out, int *ubnd_out, 
*                   double *map, double *variance, double *weights,
*         	    int *status );

*  Arguments:
*     data = smfData* (Given)
*        Pointer to smfData struct
*     index = int (Given)
*        Index of element in igrp
*     size = int (Given)
*        Number of elements in igrp
*     outfset = AstFrameSet* (Given)
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
*     Tim Jenness (JAC, Hawaii)
*     Andy Gibb (UBC)
*     {enter_new_authors_here}

*  History:
*     2006-10-10 (JB)
*        Cloned from smf_rebinmap
*     2007-5-3 (DSB)
*        Adapt to new astRebinSeq signature.
*     2007-07-12 (EC):
*        -adding moving to interface
*        -Replaced calculation of bolo2map with a call to smf_rebincube_totmap

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

#define FUNC_NAME "smf_bbrebinmap"

void smf_bbrebinmap( smfData *data,  int indf, int index, int size, 
		     AstFrameSet *outfset, int moving, int *lbnd_out, 
		     int *ubnd_out, double *map, double *variance, 
		     double *weights, int *status ) {

  /* Local Variables */
  AstSkyFrame *abskyfrm = NULL; /* Output SkyFrame (always absolute) */
  int baddims;                  /* Number of dimensions in bad pixel mask */
  int badflag = 0;              /* Flag to indicate using bad pixel mask */
  int *badpixels;               /* Array of pixels with good/bad values */
  int bdims[2];                 /* Dimensions of bad pixel mask */
  int bndf;                     /* NDF identifier of bad pixel extension */
  AstMapping *bolo2map=NULL;    /* Combined mapping bolo->map coordinates */
  double  *boldata;             /* Pointer to bolometer data */
  HDSLoc *bpmloc=NULL;          /* NDF extension for bad bolometer mask */
  smfHead *hdr=NULL;            /* Pointer to data header this time slice */
  dim_t i;                      /* Loop counter */
  int j;                        /* Loop counter */
  int lbnd_in[2];               /* Lower pixel bounds for input maps */
  int n;                        /* Number of elements mapped by ndfMap */
  int nbol = 0;                 /* # of bolometers in the sub-array */
  int nused;                    /* No. of input values used */
  AstSkyFrame *oskyfrm = NULL;  /* SkyFrame from the output WCS Frameset */
  int place;                    /* NDF placeholder */
  int rebinflags;               /* Control the rebinning procedure */
  AstMapping *sky2map=NULL;     /* Mapping celestial->map coordinates */
  int ubnd_in[2];               /* Upper pixel bounds for input maps */
  const char *system;           /* Coordinate system */

  /* Main routine */
  if (*status != SAI__OK) return;

  /* Get the system from the outfset */
  system = astGetC( outfset, "system" );

  /* Retrieve the sky2map mapping from the output frameset (actually
     map2sky) */
  oskyfrm = astGetFrame( outfset, AST__CURRENT );
  sky2map = astGetMapping( outfset, AST__BASE, AST__CURRENT );
  /* Invert it to get Output SKY to output map coordinates */
  astInvert( sky2map );
  /* Create a SkyFrame in absolute coordinates */
  abskyfrm = astCopy( oskyfrm );
  astClear( abskyfrm, "SkyRefIs" );
  astClear( abskyfrm, "SkyRef(1)" );
  astClear( abskyfrm, "SkyRef(2)" );

  /* Calculate bounds in the input array */
  nbol = (data->dims)[0] * (data->dims)[1];
  lbnd_in[0] = 0;
  lbnd_in[1] = 0;
  ubnd_in[0] = (data->dims)[0]-1;
  ubnd_in[1] = (data->dims)[1]-1;

  /* Check to see if a bad bolo mask exists.  If it does, flag 
     data values corresponding to bad bolometers with VAL_BADD */

  ndfXloc ( indf, "BPM", "READ", &bpmloc, status );

  if ( *status == NDF__NOEXT ) {
     errAnnul ( status );
     *status = SAI__OK;
     msgOut(" ", "No bad bolo data available, ignoring bad values", status);
  } else {

     badflag = 1;
     
     /* Open the bad pixel mask extension and make sure that the dimensions
        match those of the data */

     ndfOpen ( bpmloc, " ", "READ", "OLD", &bndf, &place, status );
     ndfDim ( bndf, 2, bdims, &baddims, status ); 

     if ( baddims != 2 || bdims[0] != (data->dims)[0] || 
          bdims[1] != (data->dims)[1] ) {
        msgOut(" ", 
                 "Dimensions of data do not match bad pixel mask", 
                 status );
        ndfAnnul ( &bndf, status );
        badflag = 0;
     } else {

        /* Retrieve the bad pixel array */

        ndfMap( bndf, "DATA", "_INTEGER", "READ", &badpixels, &n, status ); 
     }

  }

  for( i=0; (i<(data->dims)[2]) && (*status == SAI__OK); i++ ) {
    
    /* Calculate the bolometer to map-pixel transformation for this tslice */
    bolo2map = smf_rebincube_totmap( data, i, abskyfrm, sky2map, moving, 
				     status );
    
    /*  Rebin this time slice*/
    rebinflags = 0;
    if ( badflag == 1 )
      rebinflags = rebinflags | AST__USEBAD;         /* Flags use bad vals */
    if( (index == 1) && (i == 0) )                    /* Flags start rebin */
      rebinflags = rebinflags | AST__REBININIT;
    
    if( (index == size) && (i == (data->dims)[2]-1) ) /* Flags end rebin */
      rebinflags = rebinflags | AST__REBINEND;
    boldata = (data->pntr)[0];
         
    /* If a bad pixel mask was retrieved, use it to flag the data
       from bad bolometers with VAL__BADD */
    if ( badflag == 1 ) {
      for ( j = 0; j < bdims[0]*bdims[1]; j++ ) {
	if ( badpixels[j] > 0 ) {
	  boldata[i*nbol + j] = VAL__BADD;
	}
      }
    }
    
    astRebinSeqD(bolo2map, 0.0,
		 2, lbnd_in, ubnd_in,
		 &(boldata[i*nbol]),
		 NULL, 
		 AST__NEAREST, NULL, rebinflags, 
		 0.1, 1000000, VAL__BADD,
		 2,lbnd_out,ubnd_out,
		 lbnd_in, ubnd_in,
		 map, variance, weights, &nused );
    
    /* clean up ast objects */
    bolo2map = astAnnul( bolo2map );
    
  
    /* Break out of loop over time slices if bad status */
    if (*status != SAI__OK) goto CLEANUP;
  }

  /* Free resources */
  
  if ( badflag == 1 ) {
    ndfUnmap ( bndf, "DATA", status );
    ndfAnnul ( &bndf, status );
  }
  
  ndfAnnul ( &indf, status );
  
  /* Close the data file */
  /*  if( data != NULL ) {
      smf_close_file( &data, status);
      data = NULL;
      }*/
  
  
  /* Clean Up */
 CLEANUP:
  if (sky2map) sky2map  = astAnnul( sky2map );
  if (bolo2map) bolo2map = astAnnul( bolo2map );
  if ( abskyfrm ) abskyfrm = astAnnul( abskyfrm );
  if ( oskyfrm ) oskyfrm = astAnnul( oskyfrm );  

}
