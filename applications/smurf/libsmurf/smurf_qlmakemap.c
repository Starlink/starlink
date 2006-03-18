/*
*+
*  Name:
*     smurf_makemap

*  Purpose:
*     Top-level QUICK-LOOK MAKEMAP implementation

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     smurf_qlmakemap( int *status );

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This is the main routine implementing the MAKEMAP task.

*  ADAM Parameters:
*     IN = NDF (Read)
*          Input file(s)
*     PIXSIZE = REAL (Read)
*          Pixel size in output image, in arcsec
*     OUT = NDF (Write)
*          Output file

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     Andy Gibb (UBC)
*     Edward Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2006-03-16 (AGG):
*        Clone from smurf_makemap
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2006 Particle Physics and Astronomy Research
*     Council and the University of British Columbia.  All Rights
*     Reserved.

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

void smurf_qlmakemap( int *status ) {

  /* Local Variables */
  void *data_index[1];       /* Array of pointers to mapped arrays in ndf */
  int flag;                  /* Flag */
  Grp *igrp = NULL;          /* Group of input files */
  int lbnd_out[2];           /* Lower pixel bounds for output map */
  void *map=NULL;            /* Pointer to the rebinned map data */
  int n;                     /* # elements in the output map */
  int ondf;                  /* output NDF identifier */
  AstFrameSet *outframeset=NULL; /* Frameset containing sky->output mapping */
  float pixsize=3;           /* Size of an output map pixel in arcsec */
  int size;                  /* Number of files in input group */
  int ubnd_out[2];           /* Upper pixel bounds for output map */
  void *variance=NULL;       /* Pointer to the variance map */
  void *weights=NULL;        /* Pointer to the weights map */

  /* From smf_mapbounds */
  AstMapping *bolo2sky=NULL;   /* Mapping bolo->celestial coordinates */
  AstCmpMap *bolo2map=NULL;    /* Combined mapping bolo->map coordinates */
  smfData *data=NULL;          /* pointer to  SCUBA2 data struct */
  smfFile *file=NULL;          /* SCUBA2 data file information */
  AstFitsChan *fitschan=NULL;  /* Fits channels to construct WCS header */
  smfHead *hdr=NULL;           /* Pointer to data header this time slice */
  dim_t i;                     /* Loop counter */
  dim_t j;                     /* Loop counter */
  char *pname=NULL;            /* Name of currently opened data file */
  struct sc2head *sc2hdr=NULL; /* Pointer to sc2head for this time slice */
  AstMapping *sky2map=NULL;    /* Mapping celestial->map coordinates */
  /* From smf_rebinmap */
  double  *boldata;             /* Pointer to bolometer data */
  int lbnd_in[2];               /* Lower pixel bounds for input maps */
  int nbolo=0;                  /* # of bolometers in the sub-array */
  int rebinflags;               /* Control the rebinning procedure */
  int ubnd_in[2];               /* Upper pixel bounds for input maps */

  smfData *tmpdata;
  double maphght;
  double mapbnd;
  double lon_0;
  double lat_0;

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

  /* Calculate the map bounds */
  msgOutif(MSG__VERB, " ", "SMURF_MAKEMAP: Determine map bounds", status);
  /*  smf_mapbounds( igrp, size, "icrs", 0, 0, 1, pixsize, lbnd_out, ubnd_out, 
      &outframeset, status );*/


  /* Regrid the data */
  msgOutif(MSG__VERB, " ", "SMURF_MAKEMAP: Regrid data", status);
  /*  smf_rebinmap(igrp, size, outframeset, lbnd_out, ubnd_out, 
      map, variance, weights, status );*/

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
      if ( i == 1 ) {
	/* First time round get the map size from the header and
	   create the output file with basic frameset */
	/* Read first time slice just to get the header */
	smf_tslice_ast( data, 1, 1, status);
	hdr = data->hdr;
	sc2hdr = hdr->sc2head;
	/* Only need to retrieve one at this stage because maps are square */
	smf_fits_getD( hdr, "MAP_HGHT", &maphght, status );
	/* PONG patterns are currently square so the output map is symmetric
	   about the centre */
	mapbnd = maphght/(pixsize);
	lbnd_out[0] = 0;
	lbnd_out[1] = 0;
	ubnd_out[0] = mapbnd;
	ubnd_out[1] = mapbnd;

	lon_0 = sc2hdr->tcs_tr_bc1;
	lat_0 = sc2hdr->tcs_tr_bc2;
	fitschan = astFitsChan ( NULL, NULL, "" );
	sc2ast_makefitschan( mapbnd/10, mapbnd/2, 
			     (-pixsize/3600), (pixsize/3600),
			     (lon_0*57.29577951), (lat_0*57.29577951),
			     "RA---TAN", "DEC--TAN", fitschan, status );
	astClear( fitschan, "Card" );
	outframeset = astRead( fitschan );
	astSetC( outframeset, "SYSTEM", "ICRS" );

	/* Create the output NDF for the image and map arrays */
	ndfCreat( "OUT", "_DOUBLE", 2, lbnd_out, ubnd_out, &ondf, status );
	ndfMap( ondf, "DATA", "_DOUBLE", "WRITE", data_index, &n, status);
	map = data_index[0];
	ndfMap( ondf, "VARIANCE", "_DOUBLE", "WRITE", data_index, &n, status);
	variance = data_index[0];

	/* Allocate memory for weights and initialise to zero */
	weights = smf_malloc( (ubnd_out[0]-lbnd_out[0]+1) *
			      (ubnd_out[1]-lbnd_out[1]+1), sizeof(double),
			      1, status );

      }

      /* Loop over all time slices in the data */      
      for( j=0; j<(data->dims)[2]; j++ ) {
	
	smf_tslice_ast( data, j, 1, status);
	
	if( *status == SAI__OK ) {
	  hdr = data->hdr;
	  
	  astSetC( hdr->wcs, "SYSTEM", "icrs" );
	  bolo2sky = astGetMapping( hdr->wcs, AST__BASE, 
				    AST__CURRENT );
	  if( sky2map == NULL ) { 
	    /* Extract the Sky->REF_PIXEL mapping. */
	    astSetC( outframeset, "SYSTEM", "icrs" );
	    sky2map = astGetMapping( outframeset, AST__CURRENT, 
				     AST__BASE );
	  }
	  /* Concatenate Mappings to get IN_PIXEL->REF_PIXEL Mapping */
	  bolo2map = astCmpMap( bolo2sky, sky2map, 1, "" );

	  /* Calculate bounds in the input array */
	  lbnd_in[0] = 0;
	  lbnd_in[1] = 0;
	  ubnd_in[0] = (data->dims)[0]-1;
	  ubnd_in[1] = (data->dims)[1]-1;
	  /*  Rebin this time slice*/
	  rebinflags = 0;
	  if( (i == 1) && (j == 0) )                    /* Flags start rebin */
	    rebinflags = rebinflags | AST__REBININIT;
	  if( (i == size) && (j == (data->dims)[2]-1) ) /* Flags end rebin */
	    rebinflags = rebinflags | AST__REBINEND;
	  boldata = (data->pntr)[0];
	  astRebinSeqD(bolo2map,0.0,
		       2,lbnd_in, ubnd_in,
		       &(boldata[j*nbolo]), NULL, 
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
  if (sky2map) sky2map  = astAnnul( sky2map );
  if (bolo2sky) bolo2sky = astAnnul( bolo2sky );
  if (bolo2map) bolo2map = astAnnul( bolo2map );

  if( data != NULL )
    smf_close_file( &data, status);




  /* Write FITS header */
  ndfPtwcs( outframeset, ondf, status );

  if( outframeset != NULL ) {
    astAnnul( outframeset );
    outframeset = NULL;
  }
  
  ndfUnmap( ondf, "DATA", status);
  ndfUnmap( ondf, "VARIANCE", status);
  ndfAnnul( &ondf, status );

  smf_free( weights, status );
  grpDelet( &igrp, status);
  

  ndfEnd( status );
  
  msgOutif(MSG__VERB," ","Map written.", status);
}
