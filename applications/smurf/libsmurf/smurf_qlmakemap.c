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
*     GENVAR = _LOGICAL (Read)
*          Flag to denote whether or not variances are to be generated
*          in the output file. The task runs slightly quicker if
*          variances are not generated. [FALSE]
*     IN = NDF (Read)
*          Input file(s)
*     PARAMS( 2 ) = _DOUBLE (Read)
*          An optional array which consists of additional parameters
*          required by the Sinc, SincSinc, SincCos, SincGauss, Somb,
*          SombCos, and Gauss spreading methods (see parameter SPREAD).
*	   
*          PARAMS( 1 ) is required by all the above schemes. It is used to 
*          specify how many pixels on either side of the output position
*          (that is, the output position corresponding to the centre of the 
*          input pixel) are to receive contributions from the input pixel.
*          Typically, a value of 2 is appropriate and the minimum allowed 
*          value is 1 (i.e. one pixel on each side). A value of zero or 
*          fewer indicates that a suitable number of pixels should be 
*          calculated automatically. [0]
*	   
*          PARAMS( 2 ) is required only by the SombCos, Gauss, SincSinc, 
*          SincCos, and SincGauss schemes.  For the SombCos, SincSinc, and
*          SincCos schemes, it specifies the number of pixels at which the
*          envelope of the function goes to zero.  The minimum value is
*          1.0, and the run-time default value is 2.0.  For the Gauss and
*          SincGauss scheme, it specifies the full-width at half-maximum
*          (FWHM) of the Gaussian envelope.  The minimum value is 0.1, and
*          the run-time default is 1.0.  On astronomical images and 
*          spectra, good results are often obtained by approximately 
*          matching the FWHM of the envelope function, given by PARAMS(2),
*          to the point-spread function of the input data.  []
*     PIXSIZE = REAL (Read)
*          Pixel size in output image, in arcsec
*     SPREAD = LITERAL (Read)
*          The method to use when spreading each input pixel value out
*          between a group of neighbouring output pixels. If SPARSE is set 
*          TRUE, then SPREAD is not accessed and a value of "Nearest" is
*          always assumed. SPREAD can take the following values:
*	   
*          - "Linear" -- The input pixel value is divided bi-linearly between 
*          the four nearest output pixels.  Produces smoother output NDFs than 
*          the nearest-neighbour scheme.
*	   
*          - "Nearest" -- The input pixel value is assigned completely to the
*          single nearest output pixel. This scheme is much faster than any
*          of the others. 
*	   
*          - "Sinc" -- Uses the sinc(pi*x) kernel, where x is the pixel
*          offset from the interpolation point (resampling) or transformed
*          input pixel centre (rebinning), and sinc(z)=sin(z)/z.  Use of 
*          this scheme is not recommended.
*	   
*          - "SincSinc" -- Uses the sinc(pi*x)sinc(k*pi*x) kernel. A
*          valuable general-purpose scheme, intermediate in its visual
*          effect on NDFs between the bi-linear and nearest-neighbour
*          schemes. 
*	   
*          - "SincCos" -- Uses the sinc(pi*x)cos(k*pi*x) kernel.  Gives
*          similar results to the "Sincsinc" scheme.
*	   
*          - "SincGauss" -- Uses the sinc(pi*x)exp(-k*x*x) kernel.  Good 
*          results can be obtained by matching the FWHM of the
*          envelope function to the point-spread function of the
*          input data (see parameter PARAMS).
*	   
*          - "Somb" -- Uses the somb(pi*x) kernel, where x is the pixel
*          offset from the transformed input pixel centre, and 
*          somb(z)=2*J1(z)/z (J1 is the first-order Bessel function of the 
*          first kind.  This scheme is similar to the "Sinc" scheme.
*	   
*          - "SombCos" -- Uses the somb(pi*x)cos(k*pi*x) kernel.  This
*          scheme is similar to the "SincCos" scheme.
*	   
*          - "Gauss" -- Uses the exp(-k*x*x) kernel. The FWHM of the Gaussian 
*          is given by parameter PARAMS(2), and the point at which to truncate 
*          the Gaussian to zero is given by parameter PARAMS(1).
*	   
*          For further details of these schemes, see the descriptions of 
*          routine AST_REBINx in SUN/211. ["Nearest"]
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
*     2007-07-05 (TIMJ):
*        Fix provenance file name handling.
*     2007-12-18 (AGG):
*        Update to use new smf_free behaviour
*     2008-02-12 (AGG):
*        - Add USEBAD parameter (default NO)
*        - Update to reflect change in API for smf_rebinmap
*     2008-02-13 (AGG):
*        Add SPREAD and PARAMS parameters to allow choice of
*        pixel-spreading scheme, update call to smf_rebinmap
*     2008-02-15 (AGG):
*        Weights array is now written as an NDF extension
*     2008-02-19 (AGG):
*        Add EXP_TIME array to output file
*     2008-02-20 (AGG):
*        Calculate median exposure time and write FITS entry
*     2008-03-11 (AGG):
*        Update call to smf_rebinmap
*     2008-04-01 (AGG):
*        Write WCS to EXP_TIME component in output file
*     2008-04-02 (AGG):
*        Write 2-D WEIGHTS component + WCS in output file, protect
*        against attempting to access NULL smfFile pointer
*     2008-04-16 (AGG):
*        Remove exp_time and weights components to ensure QLMAKEMAP
*        lives up to its name, add GENVAR parameter
*     2008-04-28 (AGG):
*        Write mean sky level to output parameter.
*     2008-05-01 (TIMJ):
*        Write output units.
*     2008-05-03 (AGG):
*        Only access variance if status is good
*     2008-05-28 (TIMJ):
*        Use smf_accumulate_prov. Break from loop if status is bad.
*     2008-07-25 (TIMJ):
*        Use kaplibs for grp in/out. Filter darks.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2006-2007 Particle Physics and Astronomy Research
*     Council. Copyright (C) 2006-2008 University of British Columbia.
*     Copyright (C) 2007-2008 Science and Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 3 of
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
  smfArray *darks = NULL;   /* Dark data */
  smfData *data = NULL;      /* Pointer to input SCUBA2 data struct */
  char data_units[SMF__CHARLABEL+1]; /* Units string */
  AstFitsChan *fchan = NULL; /* FitsChan holding output NDF FITS extension */
  smfFile *file=NULL;        /* Pointer to SCUBA2 data file struct */
  Grp * fgrp = NULL;         /* Filtered group, no darks */
  int genvar = 0;            /* Flag to denote whether to generate
				variances in output image */
  dim_t i;                   /* Loop counter */
  Grp *igrp = NULL;          /* Group of input files */
  int lbnd_out[2];           /* Lower pixel bounds for output map */
  double *map = NULL;        /* Pointer to the rebinned map data */
  size_t mapsize;            /* Number of pixels in output image */
  double meansky;            /* Mean sky level for current file */
  int moving = 0;            /* Flag to denote a moving object */
  int nparam = 0;            /* Number of extra parameters for pixel spreading scheme*/
  size_t nweights;           /* Number of elements in weights array */
  smfData *odata=NULL;       /* Pointer to output SCUBA2 data struct */
  Grp *ogrp = NULL;          /* Group containing output file */
  int ondf = NDF__NOID;      /* output NDF identifier */
  AstFrameSet *outframeset = NULL; /* Frameset containing sky->output map mapping */
  size_t outsize;            /* Number of files in output group */
  double overallmeansky = 0.0; /* Mean sky level across all input files */
  char pabuf[ 10 ];          /* Text buffer for parameter value */
  double params[ 4 ];        /* astRebinSeq parameters */
  double pixsize = 3.0;      /* Size of an output map pixel in arcsec */
  size_t size;               /* Number of files in input group */
  int smfflags = 0;          /* Flags for creating a new smfData */
  int spread;                /* Code for pixel spreading scheme */
  char system[10];           /* Celestial coordinate system for output image */
  double tau;                /* 225 GHz optical depth */
  int ubnd_out[2];           /* Upper pixel bounds for output map */
  void *variance = NULL;     /* Pointer to the variance map */
  double *weights = NULL;    /* Pointer to the weights array */

  if (*status != SAI__OK) return;

  /* initialisation */
  data_units[0] = '\0';

  /* Main routine */
  ndfBegin();
  
  /* Get group of input files */
  kpg1Rgndf( "IN", 0, 1, "", &igrp, &size, status );

  /* Filter out darks */
  smf_find_darks( igrp, &fgrp, NULL, 1, &darks, status );

  /* input group is now the filtered group so we can use that and
     free the old input group */
  size = grpGrpsz( fgrp, status );
  grpDelet( &igrp, status);
  igrp = fgrp;
  fgrp = NULL;

  if (size == 0) {
    msgOutif(MSG__NORM, " ","All supplied input frames were DARK,"
             " nothing from which to make a map", status );
    goto CLEANUP;
  }

  /* Get the celestial coordinate system for the output image. */
  parChoic( "SYSTEM", "TRACKING", "TRACKING,FK5,ICRS,AZEL,GALACTIC,"
	    "GAPPT,FK4,FK4-NO-E,ECLIPTIC", 1, system, 10, status );

  /* Get the user defined pixel size */
  parGet0d( "PIXSIZE", &pixsize, status );
  if ( pixsize <= 0 || pixsize > 60. ) {
    msgSetd( "PIXSIZE", pixsize );
    *status = SAI__ERROR;
    errRep( " ", "Invalid pixel size, ^PIXSIZE (must be positive but < 60 arcsec)", 
	   status );
  }

  /* Decide whether output map contains variance */
  parGet0l( "GENVAR", &genvar, status );

  /* Obtain desired pixel-spreading scheme */
  parChoic( "SPREAD", "NEAREST", "NEAREST,LINEAR,SINC,"
	    "SINCSINC,SINCCOS,SINCGAUSS,SOMB,SOMBCOS,GAUSS", 
	    1, pabuf, 10, status );

  smf_get_spread( pabuf, &spread, &nparam, status );

  /* Get an additional parameter vector if required. */
  if ( nparam > 0 ) parExacd( "PARAMS", nparam, params, status );

  /* Calculate the map bounds - from the FIRST FILE only! */
  msgOutif( MSG__VERB," ", 
	   "SMURF_QLMAKEMAP: Determine approx map bounds from first file", status );
  smf_mapbounds_approx( igrp, 1, system, pixsize, lbnd_out, ubnd_out, 
			&outframeset, &moving, status );
 
  /* Compute number of pixels in output map */
  mapsize = (ubnd_out[0] - lbnd_out[0] + 1) * (ubnd_out[1] - lbnd_out[1] + 1);

  /* Now allocate memory for weights array used by smf_rebinmap and
     initialize to zero */
  if ( genvar ) {
    nweights = 2*mapsize;
    /* Generate variances in output file */
    smfflags |= SMF__MAP_VAR;
  } else {
    nweights = mapsize;
  }
  weights = smf_malloc( nweights, sizeof(double), 1, status);

  /* Create an output smfData */
  kpg1Wgndf( "OUT", igrp, 1, 1, "More output files required...",
             &ogrp, &outsize, status );
  smf_open_newfile( ogrp, 1, SMF__DOUBLE, 2, lbnd_out, ubnd_out, smfflags, &odata, 
		    status );

  /* Map variance array if we want it in the output file - this should
     be OK even if no variances are required because it is initialized
     to a NULL pointer */

  /* If created OK, retrieve pointers to data */
  if ( *status == SAI__OK ) {
    file = odata->file;
    ondf = file->ndfid;
    /* Map the data array */
    map = (odata->pntr)[0];
    variance = (odata->pntr)[1];
  }

  /* Loop over each input file, subtracting bolometer drifts, a mean
     sky level (per timeslice), correcting for extinction and
     regridding the data into the output map */
  msgOutif( MSG__VERB," ", "SMURF_QLMAKEMAP: Process data files", status );
  for ( i=1; i<=size && *status == SAI__OK; i++ ) {
    /* Read data from the ith input file in the group */
    smf_open_and_flatfield( igrp, NULL, i, darks, &data, status );

    /* Check units are consistent */
    smf_check_units( i, data_units, data->hdr, status);

    /* Handle output FITS header creation */
    smf_fits_outhdr( data->hdr->fitshdr, &fchan, NULL, status );

    /* Remove bolometer drifts if a fit is present */
    smf_subtract_poly( data, NULL, 1, status );

    /* Remove a mean sky level - call low-level 1-D routine */
    smf_subtract_plane1( data, "MEAN", &meansky, status );
    if ( meansky != VAL__BADD ) {
      overallmeansky += meansky;
    }

    /* Correct for atmospheric extinction using the mean WVM-derived
       225-GHz optical depth */
    smf_fits_getD( data->hdr, "MEANWVM", &tau, status );
    smf_correct_extinction( data, "CSOTAU", 1, tau, NULL, status );

    /* Propagate provenace */
    smf_accumulate_prov( data, igrp, i, odata->file->ndfid,
                         "SMURF:QLMAKEMAP", status );

    msgOutif(MSG__VERB, " ", "SMURF_QLMAKEMAP: Beginning the REBIN step",
             status);

    /* Rebin the data onto the output grid */
    smf_rebinmap( data, i, size, outframeset, spread, params, moving, genvar,
		  lbnd_out, ubnd_out, map, variance, weights, status );

    smf_close_file( &data, status );

    if (*status != SAI__OK) break;
  }

  overallmeansky /= (double)size;
  parPut0d("MEANSKY", overallmeansky, status);

  /* Write WCS FrameSet to output file */
  ndfPtwcs( outframeset, ondf, status );

  /* write units - hack we do not have a smfHead */
  if (strlen(data_units)) ndfCput( data_units, ondf, "UNITS", status);
  ndfCput("Flux Density", ondf, "LABEL", status);

  /* Free up weights array */
  weights = smf_free( weights, status );

  /* If the FitsChan is not empty, store it in the FITS extension of the
     output NDF (any existing FITS extension is deleted). */
  if( fchan ){
    if( astGetI( fchan, "NCard" ) > 0 ) kpgPtfts( ondf, fchan, status );
    fchan = astAnnul( fchan );
  }

 CLEANUP:

  /* Free the WCS pointer */
  if ( outframeset != NULL ) {
    outframeset = astAnnul( outframeset );
  }

  /* Tidy up and close the output file */  
  smf_close_file ( &odata, status );
  if ( ogrp != NULL ) grpDelet( &ogrp, status );
  if (darks) smf_close_related( &darks, status );
  grpDelet( &igrp, status );
  
  ndfEnd( status );
  
  msgOutif( MSG__VERB," ", "Output map written", status );
}
