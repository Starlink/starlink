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
*     smf_rebinmap( smfData *data, int index, int size, 
*                    AstFrameSet *outframeset,
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
*     Tim Jenness (JAC, Hawaii)
*     Andy Gibb (UBC)
*     {enter_new_authors_here}

*  History:
*     2006-02-02 (EC):
*        Initial version.
*     2006-02-13 (TIMJ):
*        Use astSetC rather than astSet
*        Avoid an additional dereference
*     2006-03-23 (AGG):
*        Updated API: now takes a smfData rather than a Grp
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

void smf_rebinmap( smfData *data,  int index, int size, AstFrameSet *outframeset,
                   int *lbnd_out, int *ubnd_out, double *map, double *variance,
		   double *weights, int *status ) {

  /* Local Variables */
  AstMapping *bolo2sky=NULL;    /* Mapping bolo->celestial coordinates */
  AstCmpMap *bolo2map=NULL;     /* Combined mapping bolo->map coordinates */
  double  *boldata;             /* Pointer to bolometer data */
  smfHead *hdr=NULL;            /* Pointer to data header this time slice */
  dim_t j;                      /* Loop counter */
  int lbnd_in[2];               /* Lower pixel bounds for input maps */
  int nbol = 0;                 /* # of bolometers in the sub-array */
  int rebinflags;               /* Control the rebinning procedure */
  struct sc2head *sc2hdr=NULL;  /* Pointer to sc2head for this time slice */
  AstMapping *sky2map=NULL;     /* Mapping celestial->map coordinates */
  int ubnd_in[2];               /* Upper pixel bounds for input maps */
  const char *system;           /* Coordinate system */

  /* Main routine */
  if (*status != SAI__OK) return;

  /* Get the system from the outframeset */
  system = astGetC( outframeset, "system" );

  nbol = (data->dims)[0] * (data->dims)[1];

  /* Loop over all time slices in the data */      
  for( j=0; j<(data->dims)[2]; j++ ) {
	
    smf_tslice_ast( data, j, 1, status);
	
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

      astSetC( hdr->wcs, "SYSTEM", system );
      bolo2sky = astGetMapping( data->hdr->wcs, AST__BASE, 
				AST__CURRENT );
	  
      /* Create sky to output grid mapping 
	 using the base coordinates to get the coordinates of the 
	 tangent point if it hasn't been done yet. */
	  
      if( sky2map == NULL ) { 
	/* Extract the Sky->REF_PIXEL mapping. */
	astSetC( outframeset, "SYSTEM", system );
	sky2map = astGetMapping( outframeset, AST__CURRENT, 
				 AST__BASE );
      }
	  
      /* Concatenate Mappings to get IN_PIXEL->REF_PIXEL Mapping */
      bolo2map = astCmpMap( bolo2sky, sky2map, 1, "" );

      /*  Rebin this time slice*/
      rebinflags = 0;
      if( (index == 1) && (j == 0) )                    /* Flags start rebin */
	rebinflags = rebinflags | AST__REBININIT;

      if( (index == size) && (j == (data->dims)[2]-1) ) /* Flags end rebin */
	rebinflags = rebinflags | AST__REBINEND;
	  
      boldata = (data->pntr)[0];
      astRebinSeqD(bolo2map, 0.0,
		   2, lbnd_in, ubnd_in,
		   &(boldata[j*nbol]),
		   NULL, 
		   AST__NEAREST, NULL, rebinflags, 0.1, 1000000, VAL__BADD,
		   2,lbnd_out,ubnd_out,
		   lbnd_in, ubnd_in,
		   map, variance, weights);

      /* clean up ast objects */
      bolo2sky = astAnnul( bolo2sky );
      bolo2map = astAnnul( bolo2map );
    }

    /* Break out of loop over time slices if bad status */
    if (*status != SAI__OK) goto CLEANUP;
  }

  /* Close the data file */
  /*  if( data != NULL ) {
    smf_close_file( &data, status);
    data = NULL;
    }*/


  /* Clean Up */
 CLEANUP:
  if (sky2map) sky2map  = astAnnul( sky2map );
  if (bolo2sky) bolo2sky = astAnnul( bolo2sky );
  if (bolo2map) bolo2map = astAnnul( bolo2map );

  /*  if( data != NULL )
      smf_close_file( &data, status);*/

}
