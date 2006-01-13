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
*     2006-01-13 (EC)
*        Automatically determine map size
*        Use VAL__BADD for pixels with no data in output map
*     2006-01-04 (EC)
*        Properly setting rebinflags
*     2005-12-16 (EC)
*        Working for simple test case with astRebinSeq 
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
#include <stdio.h>

#include "ast.h"
#include "mers.h"
#include "par.h"
#include "prm_par.h"
#include "ndf.h"
#include "sae_par.h"
#include "star/hds.h"
#include "star/ndg.h"
#include "star/grp.h"

#include "smurf_par.h"
#include "smurflib.h"
#include "libsmf/smf.h"

#include "sc2da/sc2store_par.h"
#include "sc2da/sc2store_struct.h"
#include "sc2da/sc2math.h"
#include "sc2da/sc2store.h"
#include "sc2da/sc2ast.h"

void smurf_makemap( int *status ) {

  /* Local Variables */
  AstMapping *bolo2sky=NULL; /* Mapping bolo->celestial coordinates */
  AstCmpMap *bolo2map=NULL;  /* Combined mapping bolo->map coordinates */
  smfData *data_raw=NULL;    /* pointer to raw (or flat) SCUBA2 data struct */
  smfData *data=NULL;        /* pointer to flatfielded SCUBA2 data struct */
  void *data_index[1];       /* Array of pointers to mapped arrays in ndf */
  hdsdim dim[0];             /* index for HDS cell */
  smfFile *file=NULL;        /* SCUBA2 data file information */
  char fitshd[8][81];        /* Strings for each fits channel */
  AstFitsChan *fitschan;     /* Fits channels to construct WCS header */
  HDSLoc *fitsloc = NULL;    /* HDS locator to FITS header in output */
  HDSLoc *fitsloc2 = NULL;   /* Helper locator for writing FITS header */
  int flag;                  /* Flag */
  smfHead *hdr=NULL;         /* Pointer to data header this time slice */
  dim_t i;                   /* Loop counter */
  Grp *igrp = NULL;          /* Group of input files */
  dim_t j;                   /* Loop counter */
  dim_t k;                   /* Loop counter */
  int lbnd_in[2];            /* Lower pixel bounds for input maps */
  int lbnd_out[2];           /* Lower pixel bounds for output map */
  void *map=NULL;            /* Pointer to the rebinned map data */
  int n;                     /* # elements in the output map */
  int nbolo=0;               /* # of bolometers in the sub-array */
  int nfits=0;               /* # cards in fits header */
  int ondf;                  /* output NDF identifier */
  AstFrameSet *outframeset;  /* Frameset for pixel->radec mapping */
  int pass=0;                /* First pass to determine map dimensions */
  float pixsize=3;           /* Size of an output map pixel in arcsec */
  char *pname=NULL;          /* Name of currently opened data file */
  int rebinflags;            /* Control the rebinning procedure */
  struct sc2head *sc2hdr=NULL; /* Pointer to sc2head for this time slice */
  int size;                  /* Number of files in input group */
  AstMapping *sky2map=NULL;  /* Mapping celestial->map coordinates */
  int startboundcheck=1;     /* Flag for first check of map dimensions */
  int ubnd_in[2];            /* Upper pixel bounds for input maps */
  int ubnd_out[2];           /* Upper pixel bounds for output map */
  void *variance=NULL;       /* Pointer to the variance map */
  void *weights=NULL;        /* Pointer to the weights map */
  double x_array_corners[4]; /* X-Indices for corner bolos in array */ 
  double x_map[4];           /* Projected X-coordinates of corner bolos */ 
  double x_map_max=0;        /* Max X-coordinate of all bolos in map */
  double x_map_min=0;        /* Min X-coordinate of all bolos in map */
  double y_array_corners[4]; /* Y-Indices for corner pixels in array */ 
  double y_map[4];           /* Projected X-coordinates of corner bolos */ 
  double y_map_max=0;        /* Max Y-coordinate of all bolos in map */
  double y_map_min=0;        /* Min Y-coordinate of all bolos in map */

  /* Main routine */
  ndfBegin();
  
  /* Get group of input files */
  ndgAssoc( "IN", 1, &igrp, &size, &flag, status );

  /* Get the user defined pixel size */
  parGet0r( "PIXSIZE", &pixsize, status );
  if( pixsize <= 0 ) {
    msgSetr("PIXSIZE", pixsize);
    *status = SAI__ERROR;
    errRep("smurf_makemap", 
	   "Pixel size ^PIXSIZE is < 0.", status);
  }


  for( pass=0; pass<=1; pass++ ) {

    if( pass == 0 ) {
      msgOutif(MSG__VERB, " ", "SMURF_MAKEMAP: Pass 1 determine map bounds",
	       status);
    }

    if( pass == 1 ) {
      msgOutif(MSG__VERB, " ", "SMURF_MAKEMAP: Pass 2 regrid data",
	       status);

      ndfCreat( "OUT", "_DOUBLE", 2, lbnd_out, ubnd_out, &ondf, status );
      ndfMap( ondf, "DATA", "_DOUBLE", "WRITE", data_index, &n, status);
      map = data_index[0];
      ndfMap( ondf, "VARIANCE", "_DOUBLE", "WRITE", data_index, &n, status);
      variance = data_index[0];
      weights = (double *) malloc( sizeof(double) *
				   (ubnd_out[0]-lbnd_out[0]+1) *
				   (ubnd_out[1]-lbnd_out[1]+1) );
    }

    /* Loop over all data to identify the extent of the map */
    
    for(i=1; i<=size; i++ ) {
      /* Read data from the ith input file in the group */      
      smf_open_file( igrp, i, "READ", &data, status );

      /* flatfield the data */
      /*smf_flatfield( data_raw, &data, status );*/

      /* Kludge to regrid only a single time slice */
      /*(data->dims)[2] = 1;*/

      if( *status == SAI__OK ) {
	file = data->file;
	pname =  file->name;
	msgSetc("FILE", pname);
	msgOutif(MSG__VERB, " ", "SMURF_MAKEMAP: Processing ^FILE",
		 status);
	
	/* Check that the data dimensions are 3 (for time ordered data) */
	if( data->ndims != 3 ) {
	  msgSetc("FILE", pname);
	  msgSeti("THEDIMS", data->ndims);
	  *status = SAI__ERROR;
	  errRep("smurf_makemap", 
		 "^FILE data has ^THEDIMS dimensions, should be 3.", 
		 status);
	} else {
	  /* # bolometers */
	  nbolo = (data->dims)[0] * (data->dims)[1];
	}
	
	/* Check that the input data type is double precision */
	
	if( *status == SAI__OK ) 
	  if( data->dtype != SMF__DOUBLE) 
	    switch( data->dtype ) {
	    case SMF__NULL:
	      msgSetc("FILE", pname);
	      *status = SAI__ERROR;
	      errRep("smurf_makemap", 
		     "^FILE has NULL data type, should be DOUBLE.",
		     status);
	      break;
	      
	    case SMF__INTEGER:
	      msgSetc("FILE", pname);
	      *status = SAI__ERROR;
	      errRep("smurf_makemap", 
		     "^FILE has INTEGER data type, should be DOUBLE.",
		     status);
	      break;
	      
	    case SMF__FLOAT:
	      msgSetc("FILE", pname);
	      *status = SAI__ERROR;
	      errRep("smurf_makemap", 
		     "^FILE has FLOAT data type, should be DOUBLE.",
		     status);
	      break;
	      
	    default:
	      msgSetc("FILE", pname);
	      *status = SAI__ERROR;
	      errRep("smurf_makemap", 
		     "^FILE has unknown data type, should be DOUBLE.",
		     status);
	    }
      }
      else
	errRep( "smurf_makemap", "Couldn't open input file.", status );
  
      /* Calculate bounds in the input array */
      lbnd_in[0] = 0;
      lbnd_in[1] = 0;
      ubnd_in[0] = (data->dims)[0]-1;
      ubnd_in[1] = (data->dims)[1]-1;

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
      if( *status == SAI__OK) for( j=0; j<(data->dims)[2]; j++ ) {
	
	smf_tslice_ast( data, j, status);
	
	if( *status == SAI__OK ) {
	  hdr = data->hdr;
	  sc2hdr = hdr->sc2head;
	  
	  /* Get bolo -> sky mapping 
             Set the System attribute for the SkyFrame in input WCS FrameSet
	     to ICRS and extract the IN_PIXEL->IRCS mapping. */

	  astSet( data->hdr->wcs, "system=icrs" );
	  bolo2sky = astGetMapping( data->hdr->wcs, AST__BASE, AST__CURRENT );
	  
	  /* Create sky to output grid mapping 
	     using the base coordinates to get the coordinates of the tangent 
	     point if it hasn't been done yet.
	     Note: FITS assumes angular quantities are in degrees */

	  if( sky2map == NULL ) { 
	    fitschan = astFitsChan ( NULL, NULL, "" );
	    
	    sprintf( fitshd[0], "CRPIX1  = 256" );
	    astPutFits( fitschan, fitshd[0], 0 );
	    sprintf( fitshd[1], "CRPIX2  = 256" );
	    astPutFits( fitschan, fitshd[1], 0 );
	    sprintf( fitshd[2], "CD1_1   = %e", pixsize/3600. );
	    astPutFits( fitschan, fitshd[2], 0 );
	    sprintf( fitshd[3], "CD2_2   = %e", pixsize/3600. );
	    astPutFits( fitschan, fitshd[3], 0 );
	    sprintf( fitshd[4], "CRVAL1  = %e", 
		     sc2hdr->tcs_tr_bc1*57.29577951 );
	    astPutFits( fitschan, fitshd[4], 0 );
	    sprintf( fitshd[5], "CRVAL2  = %e", 
		     sc2hdr->tcs_tr_bc2*57.29577951 );
	    astPutFits( fitschan, fitshd[5], 0 );
	    sprintf( fitshd[6], "CTYPE1  = 'RA---TAN'" );
	    astPutFits( fitschan, fitshd[6], 0 );
	    sprintf( fitshd[7], "CTYPE2  = 'DEC--TAN'" );
	    astPutFits( fitschan, fitshd[7], 0 );
	    
	    astClear( fitschan, "Card" );
	    
	    outframeset = astRead( fitschan );
	    
	    /* Set the System attribute for the SkyFrame in the reference WCS 
	       FrameSet to ICRS and extract the IRCS->REF_PIXEL mapping. */
	    astSet( outframeset, "system=icrs" );
	    
	    sky2map = astGetMapping( outframeset, AST__CURRENT, AST__BASE );
	  }
	  
	  /* Concatenate the two Mappings to get IN_PIXEL->REF_PIXEL Mapping */
	  bolo2map = astCmpMap( bolo2sky, sky2map, 1, "" );	  

	  /* On the first pass check corner pixels in the array for their
             projected extent on the sky to set the pixel bounds */

	  if( pass == 0 ) {
	    astTran2( bolo2map, 4, x_array_corners, y_array_corners, 1, 
		      x_map, y_map );

	    /* If starting the bound check initialize the values */
	    if( startboundcheck ) {
	      startboundcheck = 0;
	      x_map_min = x_map[0];
	      x_map_max = x_map[0];
	      y_map_min = y_map[0];
	      y_map_max = y_map[0];
	    }

	    /* Update min/max for this time slice */
	    for( k=0; k<4; k++ ) {
	      if( x_map[k] < x_map_min ) x_map_min = x_map[k];
	      if( x_map[k] > x_map_max ) x_map_max = x_map[k];
	      if( y_map[k] < y_map_min ) y_map_min = y_map[k];
	      if( y_map[k] > y_map_max ) y_map_max = y_map[k];
	    }
	  }

	  /* On the second pass do the regridding */
	  if( pass == 1 ) {
	    /* Flags are required to start / end the rebinning operation */
	  
	    rebinflags = 0;
	    if( (i == 1) && (j == 0) ) 
	      rebinflags = rebinflags | AST__REBININIT;
	    if( (i == size) && (j == (data->dims)[2]-1) )
	      rebinflags = rebinflags | AST__REBINEND;

	    astRebinSeqD(bolo2map,0,
			 2,lbnd_in, ubnd_in,
			 (data->pntr)[0] + j*nbolo*sizeof(double), 
			 NULL, 
			 AST__NEAREST, NULL, rebinflags, 0, 0, VAL__BADD,
			 2,lbnd_out,ubnd_out,
			 lbnd_in, ubnd_in,
			 map, variance, weights);
	  }
	}
	
	/* clean up ast objects */
	if( bolo2sky != NULL )
	  astAnnul( bolo2sky );
	
	if( bolo2map != NULL )
	  astAnnul( bolo2map );
      }

      /* close data files */
      /*if( data_raw != NULL )
	smf_close_file( &data_raw, status );
      */

      if( data != NULL )
	smf_close_file( &data, status);


    }
    
    /* After first pass calculate the pixel bounds */
    if( pass == 0 ) {
      lbnd_out[0] = (int) (x_map_min + 0.5);
      lbnd_out[1] = (int) (y_map_min + 0.5);
      ubnd_out[0] = (int) (x_map_max + 0.5);
      ubnd_out[1] = (int) (y_map_max + 0.5);
    }	  
      
    /* After second pass create HDS locator to FITS header/write astrometry */
    if( pass == 1 ) {
      nfits = 8;
      ndfXnew ( ondf, "FITS", "_CHAR*80", 1, &nfits, &fitsloc, status );
      if( *status == SAI__OK ) {
	
	for( j=1; j<=nfits; j++ ) {
	  dim[0] = j;
	  datCell ( fitsloc, 1, dim, &fitsloc2, status );
	  datPut0C ( fitsloc2, fitshd[j-1], status );
	  datAnnul ( &fitsloc2, status );
	}
	datAnnul ( &fitsloc, status );
      }
    }
  }

  /* clean up */
  if( sky2map != NULL )
    astAnnul( sky2map );

  free( weights );

  ndfUnmap( ondf, "DATA", status);
  ndfUnmap( ondf, "VARIANCE", status);
  ndfAnnul( &ondf, status );
  ndfEnd( status );

  msgOutif(MSG__VERB," ","Map written.", status);

  /* Tidy up after ourselves: release resources used by the grp routines  */
  grpDelet( &igrp, status);
}
