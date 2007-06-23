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
*     map-bounds are retrieved from the FITS header in the first file,
*     which are based on the specified map size. In practice, this
*     means that the output image will be much larger than
*     necessary. The bolometer drifts are removed using the fitted
*     polynomials, the sky is removed by subtracting the mean level
*     per time slice and then the data are extinction corrected using
*     the MEANWVM tau value (at 225 GHz) from the FITS header.

*  ADAM Parameters:
*     IN = NDF (Read)
*          Input file(s)
*     PIXSIZE = REAL (Read)
*          Pixel size in output image, in arcsec
*     SYSTEM = LITERAL (Read)
*          The celestial coordinate system for the output map
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
*     2007-01-30 (AGG):
*        Update due to API change for smf_rebinmap &
*        smf_mapbounds_approx. Also just pass in the index of the
*        first file in the input Grp to smf_mapbounds_approx
*     2007-02-27 (AGG):
*        Refactor the code to deal with global status consistently
*     2007-03-05 (EC):
*        Changed smf_correct_extinction interface
*     2007-03-20 (TIMJ):
*        Write an output FITS header
*     2007-06-22 (TIMJ):
*        Rework to handle PRV* as well as OBS*
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2006-2007 Particle Physics and Astronomy Research
*     Council. Copyright (C) 2006-2007 University of British Columbia.
*     Copyright (C) 2007 Science and Technology Facilities Council.
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

#if HAVE_CONFIG_H
#include <config.h>
#endif

/* Standard includes */
#include <string.h>
#include <stdio.h>

/* Starlink includes */
#include "ast.h"
#include "mers.h"
#include "par.h"
#include "prm_par.h"
#include "ndf.h"
#include "sae_par.h"
#include "star/hds.h"
#include "star/ndg.h"
#include "star/grp.h"
#include "star/kaplibs.h"

/* SMURF includes */
#include "smurf_par.h"
#include "smurflib.h"
#include "libsmf/smf.h"
#include "sc2da/sc2ast.h"

#define TASK_NAME "smurf_qlmakemap"

void smurf_qlmakemap( int *status ) {

  /* Local Variables */
  smfData *data = NULL;      /* Pointer to input SCUBA2 data struct */
  AstFitsChan *fchan = NULL; /* FitsChan holding output NDF FITS extension */
  smfFile *file=NULL;        /* Pointer to SCUBA2 data file struct */
  int flag;                  /* Flag */
  dim_t i;                   /* Loop counter */
  Grp *igrp = NULL;          /* Group of input files */
  int lbnd_out[2];           /* Lower pixel bounds for output map */
  void *map = NULL;          /* Pointer to the rebinned map data */
  int moving = 0;            /* Flag to denote a moving object */
  AstKeyMap * obsidmap = NULL; /* Map of OBSIDs from input data */
  smfData *odata=NULL;       /* Pointer to output SCUBA2 data struct */
  Grp *ogrp = NULL;          /* Group containing output file */
  int ondf = NDF__NOID;      /* output NDF identifier */
  AstFrameSet *outframeset = NULL; /* Frameset containing sky->output map mapping */
  int outsize;               /* Number of files in output group */
  float pixsize = 3.0;       /* Size of an output map pixel in arcsec */
  AstKeyMap * prvkeymap = NULL; /* Keymap of input files for PRVxxx headers */
  int size;                  /* Number of files in input group */
  int smfflags = 0;          /* Flags for creating a new smfData */
  char system[10];           /* Celestial coordinate system for output image */
  double tau;                /* 225 GHz optical depth */
  int ubnd_out[2];           /* Upper pixel bounds for output map */
  void *variance = NULL;     /* Pointer to the variance map */
  void *weights = NULL;      /* Pointer to the weights map */

  /* Main routine */
  ndfBegin();
  
  /* Get group of input files */
  ndgAssoc( "IN", 1, &igrp, &size, &flag, status );

  /* Get the celestial coordinate system for the output image. */
  parChoic( "SYSTEM", "TRACKING", "TRACKING,FK5,ICRS,AZEL,GALACTIC,"
	    "GAPPT,FK4,FK4-NO-E,ECLIPTIC", 1, system, 10, status );

  /* Get the user defined pixel size */
  parGet0r( "PIXSIZE", &pixsize, status );
  if ( pixsize <= 0 || pixsize > 60. ) {
    msgSetd( "PIXSIZE", pixsize );
    *status = SAI__ERROR;
    errRep( " ", "Invalid pixel size, ^PIXSIZE (must be positive but < 60 arcsec)", 
	   status );
  }

  /* Calculate the map bounds - from the FIRST FILE only! */
  msgOutif( MSG__VERB," ", 
	   "SMURF_QLMAKEMAP: Determine approx map bounds from first file", status );
  smf_mapbounds_approx( igrp, 1, system, pixsize, lbnd_out, ubnd_out, 
			&outframeset, &moving, status );
 

  /* Create an output smfData */
  ndgCreat( "OUT", NULL, &ogrp, &outsize, &flag, status );
  smfflags |= SMF__MAP_VAR;
  smf_open_newfile( ogrp, 1, SMF__DOUBLE, 2, lbnd_out, ubnd_out, smfflags, &odata, 
		    status );

  /* If created OK, retrieve pointers to data */
  if ( *status == SAI__OK ) {
    file = odata->file;
    ondf = file->ndfid;
    /* Map the data, variance, and weights arrays */
    map = (odata->pntr)[0];
    variance = (odata->pntr)[1];
  }
  /* Allocate memory for weights and initialise to zero */
  weights = smf_malloc( (ubnd_out[0]-lbnd_out[0]+1) *
			(ubnd_out[1]-lbnd_out[1]+1), sizeof(double),
			1, status );

  /* Create provenance keymap */
  prvkeymap = astKeyMap( "" );

  /* Loop over each input file, subtracting bolometer drifts, a mean
     sky level (per timeslice), correcting for extinction and
     regridding the data into the output map */
  msgOutif( MSG__VERB," ", "SMURF_QLMAKEMAP: Process data", status );
  for ( i=1; i<=size && *status == SAI__OK; i++ ) {
    /* Read data from the ith input file in the group */
    smf_open_and_flatfield( igrp, NULL, i, &data, status );

    /* Store the filename in the keymap for later - the GRP would be fine
       as is but we use a keymap in order to reuse smf_fits_add_prov */
    if (*status == SAI__OK)
      astMapPut0I( prvkeymap, data->file->name, 1, NULL );

    /* Handle output FITS header creation */
    smf_fits_outhdr( data->hdr->fitshdr, &fchan, &obsidmap, status );

    /* Remove bolometer drifts */
    smf_subtract_poly( data, status );

    /* Remove a mean sky level */
    smf_subtract_plane( data, NULL, "MEAN", status );

    /* Correct for atmospheric extinction using the mean WVM-derived
       225-GHz optical depth */
    smf_fits_getD( data->hdr, "MEANWVM", &tau, status );
    smf_correct_extinction( data, "CSOTAU", 1, tau, NULL, status );

    /* If all's well, add the data into the map */
    smf_rebinmap( data, i, size, outframeset, moving, lbnd_out, ubnd_out, 
		  map, variance, weights, status );

    smf_close_file( &data, status );
  }
  /* Write WCS FrameSet to output file */
  ndfPtwcs( outframeset, ondf, status );

/* Retrieve the unique OBSID keys from the KeyMap and populate the OBSnnnnn
   and PROVCNT headers from this information. */
  smf_fits_add_prov( fchan, "OBS", obsidmap, status ); 
  smf_fits_add_prov( fchan, "PRV", prvkeymap, status ); 
  
  astAnnul( prvkeymap );
  astAnnul( obsidmap );

/* If the FitsChan is not empty, store it in the FITS extension of the
   output NDF (any existing FITS extension is deleted). */
  if( astGetI( fchan, "NCard" ) > 0 ) kpgPtfts( ondf, fchan, status );

  /* Free the WCS pointer */
  if ( outframeset != NULL ) {
    astAnnul( outframeset );
    outframeset = NULL;
  }

  /* Tidy up and close the output file */  
  smf_close_file ( &odata, status );
  if ( ogrp != NULL ) grpDelet( &ogrp, status );

  smf_free( weights, status );
  grpDelet( &igrp, status );
  
  ndfEnd( status );
  
  msgOutif( MSG__VERB," ", "Output map written", status );
}
