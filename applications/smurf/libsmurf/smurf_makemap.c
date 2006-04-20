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
*     David Berry (JAC, UCLan)
*     {enter_new_authors_here}

*  History:
*     2005-09-27 (EC):
*        Clone from smurf_extinction
*     2005-12-16 (EC):
*        Working for simple test case with astRebinSeq 
*     2006-01-04 (EC):
*        Properly setting rebinflags
*     2006-01-13 (EC):
*        Automatically determine map size
*        Use VAL__BADD for pixels with no data in output map
*     2006-01-25 (TIMJ):
*        Replace malloc with smf_malloc.
*     2006-01-25 (TIMJ):
*        sc2head is now embedded in smfHead.
*     2006-01-27 (TIMJ):
*        - Try to jump out of loop if status bad.
*        - sc2head is now a pointer again
*     2006-02-02 (EC):
*        - Broke up mapbounds/regridding into subroutines smf_mapbounds and
*          smf_rebinmap
*        - fits header written to output using ndfputwcs
*     2006-03-23 (AGG):
*        Update to take account of new API for rebinmap
*     2006-03-23 (DSB):
*        Guard against null pointer when reporting error.
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
  void *data_index[1];       /* Array of pointers to mapped arrays in ndf */
  smfData *data=NULL;           /* pointer to  SCUBA2 data struct */
  smfFile *file=NULL;           /* SCUBA2 data file information */
  int flag;                  /* Flag */
  dim_t i;                      /* Loop counter */
  Grp *igrp = NULL;          /* Group of input files */
  int lbnd_out[2];           /* Lower pixel bounds for output map */
  void *map=NULL;            /* Pointer to the rebinned map data */
  int n;                     /* # elements in the output map */
  int ondf;                  /* output NDF identifier */
  AstFrameSet *outframeset=NULL; /* Frameset containing sky->output mapping */
  float pixsize=3;           /* Size of an output map pixel in arcsec */
  char *pname=NULL;             /* Name of currently opened data file */
  int size;                  /* Number of files in input group */
  int ubnd_out[2];           /* Upper pixel bounds for output map */
  void *variance=NULL;       /* Pointer to the variance map */
  void *weights=NULL;        /* Pointer to the weights map */

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
  smf_mapbounds( igrp, size, "icrs", 0, 0, 1, pixsize, lbnd_out, ubnd_out, 
		 &outframeset, status );

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

  /* Regrid the data */
  msgOutif(MSG__VERB, " ", "SMURF_MAKEMAP: Regrid data", status);
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

    } else if( data && data->file ){
      file = data->file;
      pname =  file->name;
      msgSetc("FILE", pname);
      errRep( "smf_rebinmap", "Couldn't open input file ^FILE", status );

    } else {
      errRep( "smf_rebinmap", "Couldn't open input file", status );
    }

    /* Check that the data dimensions are 3 (for time ordered data) */
    if( *status == SAI__OK ) {
      if( data->ndims != 3 ) {
	msgSetc("FILE", pname);
	msgSeti("THEDIMS", data->ndims);
	*status = SAI__ERROR;
	errRep("smf_rebinmap", 
	       "^FILE data has ^THEDIMS dimensions, should be 3.", 
	       status);
      }
    }

    /* Check that the input data type is double precision */
    if( *status == SAI__OK ) 
      if( data->dtype != SMF__DOUBLE) {
	msgSetc("FILE", pname);
	msgSetc("DTYPE", smf_dtype_string( data, status ));
	*status = SAI__ERROR;
	errRep("smurf_makemap", 
	       "^FILE has ^DTYPE data type, should be DOUBLE.",
	       status);
      }

    /* Rebin the data onto the output grid */
    smf_rebinmap(data, i, size, outframeset, lbnd_out, ubnd_out, 
		 map, variance, weights, status );

    /* Close the data file */
    if( data != NULL ) {
      smf_close_file( &data, status);
      data = NULL;
    }
    /* Break out of loop over data files if bad status */
    if (*status != SAI__OK) {
      errRep("smurf_makemap", "Rebinning step failed", status);
    }
  }

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
