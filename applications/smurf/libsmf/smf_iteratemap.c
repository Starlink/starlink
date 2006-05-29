/*
*+
*  Name:
*     smf_iteratemap

*  Purpose:
*     Iterative map-maker

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_iteratemap( Grp *igrp, Grp *astgrp, Grp *atmgrp, Grp *ngrp, 
*                     int gsize, double *map, double *variance, 
*                     double *weights, int msize, int *status );

*  Arguments:
*     igrp = Grp* (Given)
*        Group of input data files
*     astgrp = Grp* (Given)
*        Group of output data files containing model of astronomical signal
*     atmgrp = Grp* (Given)
*        Group of output data files containing model of atmospheric signal
*     ngrp = Grp* (Given)
*        Group of output data files containing model of residual noise
*     gsize = int (Given)
*        Number of elements in the groups
*     map = double* (Returned)
*        The output map array 
*     variance = double* (Returned)
*        Variance of each pixel in map
*     weights = double* (Returned)
*        Relative weighting for each pixel in map
*     msize = int (Given)
*        Number of pixels in the 2d maps
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This function uses an iterative algorithm to separately estimate
*     the component of the bolometer signals that are astronomical signal,
*     atmospheric signal, and residual Gaussian noise, and creates a rebinned
*     map of the astronomical signal.
*     
*  Authors:
*     Edward Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2006-05-18 (EC):
*        Initial version.

*  Notes:

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
#include "ndf.h"
#include "sae_par.h"
#include "star/ndg.h"
#include "prm_par.h"

#include "par_par.h"

/* SMURF includes */
#include "libsmf/smf.h"

#define FUNC_NAME "smf_iteratemap"


void smf_iteratemap( Grp *igrp, Grp *astgrp, Grp *atmgrp, Grp *ngrp, 
		     int gsize, double *map, double *variance, double *weights,
		     int msize, int *status ) {

  /* Local Variables */
  int atmndf;                   /* Atmospheric signal NDF identifier */
  smfData *atmdata;             /* Pointer to atmospheric data struct */
  int astndf;                   /* Astronomical signal NDF identifier */
  smfData *astdata;             /* Pointer to astronomical data struct */
  int coordndf=NDF__NOID;       /* NDF identifier for coordinates */
  dim_t i;                      /* Loop counter */
  int indf;                     /* Input data NDF identifier */
  smfData *idata;               /* Pointer to input data struct */
  dim_t iter;                   /* Iteration number */
  dim_t j;                      /* Loop counter */
  dim_t k;                      /* Loop counter */
  int lbnd[1];                  /* Pixel bounds for 1d pointing array */
  int *lut;                     /* Pointer to mapcoord LUT */
  void *mapptr[1];              /* Pointer to array of mapped data*/
  double mean;                  /* Estimate of mean */
  smfData *ndata;               /* Pointer to noise data struct */
  int nmap;                     /* Number of elements mapped */
  int nbolo;                    /* Number of bolometers */
  int nndf;                     /* Residual noise NDF identifier */
  dim_t numiter;                /* Total number iterations */
  int rebinflags;               /* Flags to control rebinning */
  double sigma;                 /* Estimate of standard deviation */
  HDSLoc *smurfloc;             /* HDS locator to the SMURF extension */
  int ubnd[1];                  /* Pixel bounds for 1d pointing array */

  char filter[PAR__SZNAM+PAR__SZNAM+1];
  char taskname[PAR__SZNAM+1];
  int nloc1;                   /* Number of active HDS Locators at end */

  /* Main routine */
  if (*status != SAI__OK) return;

  memset( taskname, ' ', PAR__SZNAM );
  taskname[PAR__SZNAM] = '\0';
  F77_CALL(task_get_name)(taskname,  status, PAR__SZNAM);
  cnfImprt( taskname, PAR__SZNAM, taskname);
  strcpy(filter, "!SMURF_MON,!");
  strcat(filter, taskname );

  hdsInfoI( NULL, "LOCATORS", filter, &nloc1, status );
  printf("1: HDS locators: %i\n", nloc1);


  /* Create all of the model files by copying the input files */

  msgOut(" ", "SMF_ITERATEMAP: Create intermediate model files", status);

  for( i=1; i<=gsize; i++ ) {
    /* Open the input file solely to propagate it to the output file */
    ndgNdfas( igrp, i, "READ", &indf, status );

    ndgNdfpr( indf, " ", astgrp, i, &astndf, status );
    ndgNdfpr( indf, " ", atmgrp, i, &atmndf, status );
    ndgNdfpr( indf, " ", ngrp, i, &nndf, status );

    ndfAnnul( &indf, status);
    
    /* Set parameters of the DATA arrays in the output files */
    ndfStype( "_DOUBLE", astndf, "DATA", status);
    ndfStype( "_DOUBLE", atmndf, "DATA", status);
    ndfStype( "_DOUBLE", nndf, "DATA", status);

    /* We need to map them so that the DATA_ARRAY are defined on exit */
    ndfMap( astndf, "DATA", "_DOUBLE", "WRITE", &(mapptr[0]), &nmap, status );
    ndfMap( atmndf, "DATA", "_DOUBLE", "WRITE", &(mapptr[0]), &nmap, status );
    ndfMap( nndf, "DATA", "_DOUBLE", "WRITE", &(mapptr[0]), &nmap, status );

    /* Close output files */
    ndfAnnul( &astndf, status);
    ndfAnnul( &atmndf, status);
    ndfAnnul( &nndf, status);
  }

  hdsInfoI( NULL, "LOCATORS", filter, &nloc1, status );
  printf("1: HDS locators: %i\n", nloc1);

  numiter = 2;
  
  for( iter=0; iter<numiter; iter++ ) {
    
    msgSeti("ITER", iter+1);
    msgSeti("NUMITER", numiter);
    msgOut(" ", "SMF_ITERATEMAP: Iteration ^ITER / ^NUMITER ----------------", 
	   status);

    msgOut(" ", "SMF_ITERATEMAP: Calculate ATM/AST model", status);
    for( i=1; i<=gsize; i++ ) {

      /* Open files */
      smf_open_file( igrp, i, "READ", 0, &idata, status );
      smf_open_file( atmgrp, i, "WRITE", 0, &atmdata, status );
      smf_open_file( astgrp, i, "WRITE", 0, &astdata, status );
      
      nbolo = (idata->dims)[0] * (idata->dims)[1];
      
      /* Loop over time slices and calculate atmos/bolo signal */
      for( j=0; j<(idata->dims)[2]; j++ ) {
	/* First time around just use the array mean */
	if( iter == 0 ) {
	  smf_calc_stats( idata, "t", j, 0, 0, &mean, &sigma, status );
	  
	  for( k=0; k<nbolo; k++ ) {
	    /* Place mean into atmosphere model file */
	    ((double *)(atmdata->pntr)[0])[j*nbolo + k] = mean;

	    /* Calculate bolo signal minus atmosphere guess */
	    ((double *)(astdata->pntr)[0])[j*nbolo + k] = 
	      ((double *)(idata->pntr)[0])[j*nbolo + k] - mean; 

	    /* Set initial variance to 1 for all data points */
	    ((double *)(astdata->pntr)[1])[j*nbolo + k] = 1.; 
	  }
	  
	} else {
	  /* Subsequent iterations calc. atmos after removing source signal */

	  for( k=0; k<nbolo; k++ ) {
	    /* Temporarily store bolometer corrected by signal in the
               atmosphere data array at this time slice */
	    ((double *)(atmdata->pntr)[0])[j*nbolo + k] = 
	      ((double *)(idata->pntr)[0])[j*nbolo + k] -  
	      ((double *)(astdata->pntr)[0])[j*nbolo + k];  
	  }

	  /* Calculate corrected mean atmosphere for this time slice */
	  smf_calc_stats( atmdata, "t", j, 0, 0, &mean, &sigma, status );

	  for( k=0; k<nbolo; k++ ) {
	    /* Place mean into atmosphere model file */
	    ((double *)(atmdata->pntr)[0])[j*nbolo + k] = mean;
	    
	    /* Calculate bolo signal minus atmosphere guess */
	    ((double *)(astdata->pntr)[0])[j*nbolo + k] = 
	      ((double *)(idata->pntr)[0])[j*nbolo + k] - mean; 
	  }
	}
      }

      /* Close files */
      smf_close_file( &idata, status );
      smf_close_file( &atmdata, status );
      smf_close_file( &astdata, status );    
    }

    msgOut(" ", "SMF_ITERATEMAP: Rebin AST signal to estimate MAP", status);
    for( i=1; i<=gsize; i++ ) {
      /* Open files */
      smf_open_file( astgrp, i, "READ", 0, &astdata, status );
     
      nbolo = (astdata->dims)[0] * (astdata->dims)[1];
      
      /* Get the LUT from the SMURF.MAPCOORD extension */
      smurfloc = smf_get_xloc( astdata, "SMURF", "SMURF_Calculations",
			       "READ", 0, 0, status );

      lbnd[0] = 0;
      ubnd[0] = nbolo*(astdata->dims)[2]-1;
      coordndf = smf_get_ndfid( smurfloc, "MAPCOORD", "READ", "UNKNOWN",
				"_INTEGER", 1, lbnd, ubnd, status );

      ndfMap( coordndf, "DATA", "_INTEGER", "READ", mapptr, &nmap, 
	      status );    

      if( *status == SAI__OK ) {
	lut = mapptr[0];
      } else {
	errRep( FUNC_NAME, "Unable to map LUT in SMURF extension",
		status);
      }
      
      /* Set up the flags for the rebinner */
      rebinflags = 0;
      if( i == 1 )                                      
	rebinflags = rebinflags | AST__REBININIT;
      
      if( i == gsize ) 
	rebinflags = rebinflags | AST__REBINEND;

      smf_simplerebinmap( (double *)(astdata->pntr)[0], 
			  (double *)(astdata->pntr)[1], lut, 
			  nbolo*(astdata->dims)[2], rebinflags,
			  map, weights, variance, msize, status );

      /* Free resources associated with LUT */
      ndfUnmap( coordndf, "DATA", status );
      ndfAnnul( &coordndf, status );
      datAnnul( &smurfloc, status );

      /* Close files */
      smf_close_file( &astdata, status );   
    }

    msgOut(" ", "SMF_ITERATEMAP: Sample MAP, calculate NOISE", status);

    for( i=1; i<=gsize; i++ ) {
      /* Open files */
      smf_open_file( igrp, i, "READ", 0, &idata, status );
      smf_open_file( astgrp, i, "WRITE", 0, &astdata, status );
      smf_open_file( atmgrp, i, "READ", 0, &atmdata, status );
      smf_open_file( ngrp, i, "WRITE", 0, &ndata, status );
      
      nbolo = (astdata->dims)[0] * (astdata->dims)[1];
      
      /* Get the LUT from the SMURF.MAPCOORD extension */
      smurfloc = smf_get_xloc( astdata, "SMURF", "SMURF_Calculations",
			       "READ", 0, 0, status );

      lbnd[0] = 0;
      ubnd[0] = nbolo*(astdata->dims)[2]-1;
      coordndf = smf_get_ndfid( smurfloc, "MAPCOORD", "READ", "UNKNOWN",
				"_INTEGER", 1, lbnd, ubnd, status );

      ndfMap( coordndf, "DATA", "_INTEGER", "READ", mapptr, &nmap, 
	      status );    

      if( *status == SAI__OK ) {
	lut = mapptr[0];
      } else {
	errRep( FUNC_NAME, "Unable to map LUT in SMURF extension",
		status);
      }
      
      /* Loop over all data points in this file */
      for( j=0; j<nbolo*(astdata->dims)[2]; j++ ) {
	if( lut[j] != VAL__BADI ) {
	  /* Sample the map into astdata */
	  ((double *)(astdata->pntr)[0])[j] = map[lut[j]];
	  
	  /* Re-estimate the noise signal as the input - atmdata - astdata */
	  ((double *)(ndata->pntr)[0])[j] = ((double *)(idata->pntr)[0])[j] - 
	    ((double *)(atmdata->pntr)[0])[j] - map[lut[j]];
	}
      }

      /* Loop over bolometers and re-calculate noise parameters from
         new estimate of clean noise signals */

      for( j=0; j<nbolo; j++ ) {
	smf_calc_stats( ndata, "b", j, 0, 0, &mean, &sigma, status );      
	/* At each time slice insert noise estimate^2 into variance
           for each data point */

	for( k=0; k<(astdata->dims)[2]; k++ ) {
	  ((double *)(astdata->pntr)[1])[k*nbolo + j] = sigma*sigma;
	}

	/* Jump out if there was a problem */
	if( *status != SAI__OK ) j=nbolo-1;
      }

      /* Free resources associated with LUT */
      ndfUnmap( coordndf, "DATA", status );
      ndfAnnul( &coordndf, status );
      datAnnul( &smurfloc, status );

      /* Close files */
      smf_close_file( &idata, status );    
      smf_close_file( &astdata, status );    
      smf_close_file( &atmdata, status );    
      smf_close_file( &ndata, status );    
    }
  }
}
