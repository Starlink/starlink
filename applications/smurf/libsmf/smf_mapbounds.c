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

/* SMURF includes */
#include "libsmf/smf.h"

void smf_mapbounds( Grp *igrp,  int size, char *system, double lon_0, 
		    double lat_0, int flag, double pixsize, int *lbnd_out, 
		    int *ubnd_out, AstFrameSet **outframeset, int *status ) {

  /* Local Variables */
  AstMapping *bolo2sky=NULL;   /* Mapping bolo->celestial coordinates */
  AstCmpMap *bolo2map=NULL;    /* Combined mapping bolo->map coordinates */
  smfData *data=NULL;          /* pointer to  SCUBA2 data struct */
  smfFile *file=NULL;          /* SCUBA2 data file information */
  AstFitsChan *fitschan=NULL;  /* Fits channels to construct WCS header */
  char fitshd[8][81];          /* Strings for each fits channel */
  smfHead *hdr=NULL;           /* Pointer to data header this time slice */
  dim_t i;                     /* Loop counter */
  dim_t j;                     /* Loop counter */
  dim_t k;                     /* Loop counter */
  char *pname=NULL;            /* Name of currently opened data file */
  struct sc2head *sc2hdr=NULL; /* Pointer to sc2head for this time slice */
  AstMapping *sky2map=NULL;    /* Mapping celestial->map coordinates */
  int startboundcheck=1;       /* Flag for first check of map dimensions */
  char wcssystem[81];          /* String containing system attribute */
  double x_array_corners[4];   /* X-Indices for corner bolos in array */ 
  double x_map[4];             /* Projected X-coordinates of corner bolos */ 
  double y_array_corners[4];   /* Y-Indices for corner pixels in array */ 
  double y_map[4];             /* Projected X-coordinates of corner bolos */ 

  /* Main routine */
  if (*status != SAI__OK) return;

  *outframeset = NULL;

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

	  astSet( data->hdr->wcs, wcssystem );
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
	    
	    sprintf( fitshd[0], "CRPIX1  = 256" );
	    astPutFits( fitschan, fitshd[0], 0 );
	    sprintf( fitshd[1], "CRPIX2  = 256" );
	    astPutFits( fitschan, fitshd[1], 0 );
	    sprintf( fitshd[2], "CD1_1   = %e", -pixsize/3600. );
	    astPutFits( fitschan, fitshd[2], 0 );
	    sprintf( fitshd[3], "CD2_2   = %e", pixsize/3600. );
	    astPutFits( fitschan, fitshd[3], 0 );
	    sprintf( fitshd[4], "CRVAL1  = %e", 
		     lon_0*57.29577951 );
	    astPutFits( fitschan, fitshd[4], 0 );
	    sprintf( fitshd[5], "CRVAL2  = %e", 
		     lat_0*57.29577951 );
	    astPutFits( fitschan, fitshd[5], 0 );
	    sprintf( fitshd[6], "CTYPE1  = 'RA---TAN'" );
	    astPutFits( fitschan, fitshd[6], 0 );
	    sprintf( fitshd[7], "CTYPE2  = 'DEC--TAN'" );
	    astPutFits( fitschan, fitshd[7], 0 );
	    
	    astClear( fitschan, "Card" );
	    
	    *outframeset = astRead( fitschan );
	    
	    /* Extract the Sky->REF_PIXEL mapping. */

	    astSet( *outframeset, wcssystem );
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
