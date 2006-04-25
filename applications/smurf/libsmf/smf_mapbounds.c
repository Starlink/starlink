/*
*+
*  Name:
*     smf_mapbounds

*  Purpose:
*     Automatically calculate the pixel bounds for a map given a projection

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_mapbounds( Grp *igrp,  int size, char *system, double lon_0, 
*                   double lat_0, int flag, double pixsize, int *lbnd_out, 
*                   int *ubnd_out, AstFrameSet **outframeset, int *status );

*  Arguments:
*     igrp = Grp* (Given)
*        Group of timestream NDF data files to retrieve pointing
*     size = int (Given)
*        Number of elements in igrp
*     system = char* (Given)
*        String indicating the type of projection (e.g. "icrs")
*     lon_0 = double (Given)
*        The longitude of the map reference point in the given system (radians)
*     lat_0 = double (Given)
*        The latitude of the map reference point in the given system (radians)
*     flag = int (Given)
*        If 0, use lon_0, lat_0. If non-zero BASE coordinates are adopted for
*        the reference point instead.
*     pixsize = double (Given)
*        Linear size of a map pixel (arcsec)
*     lbnd_out = double* (Returned)
*        2-element array pixel coord. for the lower bounds of the output map 
*     ubnd_out = double* (Returned)
*        2-element array pixel coord. for the upper bounds of the output map 
*     outframeset = AstFrameSet** (Returned)
*        Frameset containing the sky->output map mapping
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This function steps over a list of input files and calculates the
*     bolometer->output map pixel transformation for the corner bolometers
*     of the subarray in order to automatically determine the extent 
*     of the map.
*  Authors:
*     Edward Chapin (UBC)
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2006-02-02 (EC):
*        Initial version.
*     2006-02-13 (TIMJ):
*        Use astSetC rather than astSet
*        Avoid an additional dereference
*        Use sc2ast_makefitschan
*     2006-02-23 (EC):
*        Fixed bug in frameset generation - tangent point in pixel coordinates
*        was not getting set properly.

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

/* SMURF includes */
#include "libsmf/smf.h"
#include "sc2da/sc2ast.h"

void smf_mapbounds( Grp *igrp,  int size, char *system, double lon_0, 
		    double lat_0, int flag, double pixsize, int *lbnd_out, 
		    int *ubnd_out, AstFrameSet **outframeset, int *status ) {

  /* Local Variables */
  AstMapping *bolo2sky=NULL;   /* Mapping bolo->celestial coordinates */
  AstCmpMap *bolo2map=NULL;    /* Combined mapping bolo->map coordinates */
  smfData *data=NULL;          /* pointer to  SCUBA2 data struct */
  smfFile *file=NULL;          /* SCUBA2 data file information */
  AstFitsChan *fitschan=NULL;  /* Fits channels to construct WCS header */
  smfHead *hdr=NULL;           /* Pointer to data header this time slice */
  dim_t i;                     /* Loop counter */
  dim_t j;                     /* Loop counter */
  dim_t k;                     /* Loop counter */
  char *pname=NULL;            /* Name of currently opened data file */
  struct sc2head *sc2hdr=NULL; /* Pointer to sc2head for this time slice */
  AstMapping *sky2map=NULL;    /* Mapping celestial->map coordinates */
  int startboundcheck=1;       /* Flag for first check of map dimensions */
  double x_array_corners[4];   /* X-Indices for corner bolos in array */ 
  double x_map[4];             /* Projected X-coordinates of corner bolos */ 
  double y_array_corners[4];   /* Y-Indices for corner pixels in array */ 
  double y_map[4];             /* Projected X-coordinates of corner bolos */ 

  /* Main routine */
  if (*status != SAI__OK) return;

  *outframeset = NULL;

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
	       "SMF_MAPBOUNDS: Processing ^THISFILE/^NUMFILES ^FILE",
	       status);
    }
    else
      errRep( "smf_mapbounds", "Couldn't open input file.", status );

    /* Check that the data dimensions are 3 (for time ordered data) */
    if( *status == SAI__OK ) {
      if( data->ndims != 3 ) {
	msgSetc("FILE", pname);
	msgSeti("THEDIMS", data->ndims);
	*status = SAI__ERROR;
	errRep("smf_mapbounds", 
	       "^FILE data has ^THEDIMS dimensions, should be 3.", 
	       status);
      }
    }

    if( *status == SAI__OK) {
      /* Get the bolo offsets for each corner of the array */
      x_array_corners[0] = 0;
      x_array_corners[1] = 0;
      x_array_corners[2] = (data->dims)[0] - 1;
      x_array_corners[3] = (data->dims)[0] - 1;
      
      y_array_corners[0] = 0;
      y_array_corners[1] = (data->dims)[1] - 1;
      y_array_corners[2] = 0;
      y_array_corners[3] = (data->dims)[1] - 1;
            
      /* Get the astrometry for all the time slices in this data file */
      
      for( j=0; j<(data->dims)[2]; j++ ) {
	
	smf_tslice_ast( data, j, 1, status);
	
	if( *status == SAI__OK ) {
	  hdr = data->hdr;
	  sc2hdr = hdr->sc2head;
	  
	  /* Get bolo -> sky mapping 
	     Set the System attribute for the SkyFframe in input WCS 
	     FrameSet and extract the IN_PIXEL->Sky mapping. */	  
	  astSetC( hdr->wcs, "SYSTEM", system );
	  bolo2sky = astGetMapping( data->hdr->wcs, AST__BASE, 
				    AST__CURRENT );
	  
	  /* Create sky to output grid mapping */

	  if( sky2map == NULL ) { 

	    /* If requested get lon_0/lat_0 from pointing BASE coord. */
	    if( flag != 0 ) {
	      lon_0 = sc2hdr->tcs_tr_bc1;
	      lat_0 = sc2hdr->tcs_tr_bc2;
	    }

	    fitschan = astFitsChan ( NULL, NULL, "" );
	    sc2ast_makefitschan( 0, 0, (-pixsize/3600), (pixsize/3600),
				 (lon_0*57.29577951), (lat_0*57.29577951),
				 "RA---TAN", "DEC--TAN", fitschan, status );
	    astClear( fitschan, "Card" );
	    *outframeset = astRead( fitschan );
	    
	    /* Extract the Sky->REF_PIXEL mapping. */

	    astSetC( *outframeset, "SYSTEM", system );
	    sky2map = astGetMapping( *outframeset, AST__CURRENT, 
				     AST__BASE );
	  }
	  
	  /* Concatenate Mappings to get IN_PIXEL->REF_PIXEL Mapping */
	  bolo2map = astCmpMap( bolo2sky, sky2map, 1, "" );
	  
	  /* Check corner pixels in the array for their projected extent 
	     on the sky to set the pixel bounds */
	  astTran2( bolo2map, 4, x_array_corners, y_array_corners, 1, 
		    x_map, y_map );
	  
	  /* If starting the bound check initialize the values */
	  if( startboundcheck ) {
	    startboundcheck = 0;
	    lbnd_out[0] = x_map[0];
	    ubnd_out[0] = x_map[0];
	    lbnd_out[1] = y_map[0];
	    ubnd_out[1] = y_map[0];
	  }
	  
	  /* Update min/max for this time slice */
	  for( k=0; k<4; k++ ) {
	    if( x_map[k] < lbnd_out[0] ) lbnd_out[0] = x_map[k];
	    if( y_map[k] < lbnd_out[1] ) lbnd_out[1] = y_map[k];
	    if( x_map[k] > ubnd_out[0] ) ubnd_out[0] = x_map[k];
	    if( y_map[k] > ubnd_out[1] ) ubnd_out[1] = y_map[k];
	  }

	  /* clean up ast objects */
	  bolo2sky = astAnnul( bolo2sky );
	  bolo2map = astAnnul( bolo2map );

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

  /* Re-create the output frameset so that the CRPIX for the WCS are
     the location of the tangent point in pixel coordinates */

  if (*outframeset) *outframeset = astAnnul( *outframeset );
  if (fitschan) fitschan = astAnnul( fitschan );  


  fitschan = astFitsChan ( NULL, NULL, "" );
  sc2ast_makefitschan( -lbnd_out[0], -lbnd_out[1], 
		       (-pixsize/3600), (pixsize/3600),
		       (lon_0*57.29577951), (lat_0*57.29577951),
		       "RA---TAN", "DEC--TAN", fitschan, status );
  astClear( fitschan, "Card" );
  *outframeset = astRead( fitschan );
  astSetC( *outframeset, "SYSTEM", system );
  
  /* Change the pixel bounds to be consistent with the new CRPIX */

  ubnd_out[0] -= lbnd_out[0];
  lbnd_out[0] = 0;

  ubnd_out[1] -= lbnd_out[1];
  lbnd_out[1] = 0;

  /* Clean Up */
 
 CLEANUP:
  if (sky2map) sky2map  = astAnnul( sky2map );
  if (bolo2sky) bolo2sky = astAnnul( bolo2sky );
  if (bolo2map) bolo2map = astAnnul( bolo2map );
  if (fitschan) fitschan = astAnnul( fitschan );

  if( data != NULL )
    smf_close_file( &data, status);

}
