/*
*+
*  Name:
*     QLMAKEMAP

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
*     This is an optimized routine implementing a modified version of
*     the MAKEMAP task for the QUICK-LOOK SCUBA-2 pipeline. The
*     map-bounds are retrieved from the FITS header based on the
*     specified map size. The bolometer drifts are removed using the
*     fitted polynomials, the sky is removed by subtracting the mean
*     level per time slice and then the data are extinction corrected
*     using the MEANWVM tau value (at 225 GHz) from the FITS header.

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
*     2006-03-23 (AGG):
*        Use new and updated routines to estimate map bounds, rebin
*        map. Also carry out flatfield, sky removal and extinction
*        correction.
*     2006-04-21 (AGG):
*        Now use quicker MEAN sky subtraction rather than polynomials
*     2006-07-12 (AGG):
*        Return polynomial subtraction since it removes bolometer
*        drifts, not the sky
*     26-JUL-2006 (TIMJ):
*        Remove unused sc2da includes.
*     2007-01-12 (AGG):
*        Add SYSTEM parameter for specifying output coordinate system
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2006-2007 Particle Physics and Astronomy Research
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

#define TASK_NAME "smurf_qlmakemap"

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
  char system[10];           /* Celestial coordinate system for output image */
  int ubnd_out[2];           /* Upper pixel bounds for output map */
  void *variance=NULL;       /* Pointer to the variance map */
  void *weights=NULL;        /* Pointer to the weights map */

  smfData *data=NULL;          /* pointer to  SCUBA2 data struct */
  dim_t i;                     /* Loop counter */

  double tau;

  /* Main routine */
  ndfBegin();
  
  /* Get group of input files */
  ndgAssoc( "IN", 1, &igrp, &size, &flag, status );

  /* Get the celestial coordinate system for the output cube. */
  parChoic( "SYSTEM", "TRACKING", "TRACKING,FK5,ICRS,AZEL,GALACTIC,"
	    "GAPPT,FK4,FK4-NO-E,ECLIPTIC", 1, system, 10, status );

  /* Get the user defined pixel size */
  parGet0r( "PIXSIZE", &pixsize, status );
  if ( pixsize <= 0 || pixsize > 60. ) {
    msgSetr("PIXSIZE", pixsize);
    *status = SAI__ERROR;
    errRep(" ", 
	   "Invalid pixel size, ^PIXSIZE (must be positive but < 60 arcsec)", 
	   status);
  }

  /* Calculate the map bounds */
  msgOutif(MSG__VERB," ", 
	   "SMURF_QLMAKEMAP: Determine approx map bounds", status);
  smf_mapbounds_approx( igrp, size, system, 0, 0, 1, pixsize, lbnd_out, ubnd_out, 
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
  msgOutif(MSG__VERB," ", "SMURF_QLMAKEMAP: Regrid data", status);
  for(i=1; i<=size; i++ ) {
    /* Read data from the ith input file in the group */
    smf_open_and_flatfield( igrp, NULL, i, &data, status );

    /* Remove bolometer drifts */
    smf_subtract_poly( data, status );
    /* Remove sky */
    smf_subtract_plane( data, NULL, "MEAN", status );
    /* Correct extinction */
    smf_fits_getD( data->hdr, "MEANWVM", &tau, status);
    smf_correct_extinction( data, "CSOTAU", 1, tau, status);

    if( *status == SAI__OK) {
      smf_rebinmap(data, i, size, outframeset, lbnd_out, ubnd_out, 
		   map, variance, weights, status );

      /* Break out of loop over data files if bad status */
      if (*status != SAI__OK) {
	errRep(TASK_NAME, "Rebinning step failed", status);
      }
    }
    if( data != NULL ) {
      smf_close_file( &data, status);
      data = NULL;
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
  
  msgOutif(MSG__VERB," ", "Map written.", status);
}
