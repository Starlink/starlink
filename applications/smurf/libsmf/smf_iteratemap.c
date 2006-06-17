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

void smf_iteratemap( Grp *igrp, AstKeyMap *keymap, double *map, 
		     double *variance, double *weights,
		     int msize, int *status ) {

  /* Local Variables */
  int astndf;                   /* Astronomical signal NDF identifier */
  smfData *astdata;             /* Pointer to astronomical data struct */
  Grp *astgrp=GRP__NOID;        /* Group of ast model files */
  const char *astmodel;         /* Name of astmodel group */
  int atmndf;                   /* Atmospheric signal NDF identifier */
  smfData *atmdata;             /* Pointer to atmospheric data struct */
  Grp *atmgrp=GRP__NOID;        /* Group of atmos model files */
  const char *atmmodel;         /* Name of atmmodel group */
  int coordndf=NDF__NOID;       /* NDF identifier for coordinates */
  int flag;                     /* Flag */
  int gsize;                    /* Number of files in group */
  dim_t i;                      /* Loop counter */
  int indf;                     /* Input data NDF identifier */
  smfData *idata;               /* Pointer to input data struct */
  dim_t iter;                   /* Iteration number */
  dim_t j;                      /* Loop counter */
  dim_t k;                      /* Loop counter */
  int *lut;                     /* Pointing lookup table */
  void *mapptr[3];              /* Pointer to array of mapped components */
  double mean;                  /* Estimate of mean */
  smfData *ndata;               /* Pointer to noise data struct */
  int nmap;                     /* Number of elements mapped */
  int nbolo;                    /* Number of bolometers */
  int nndf;                     /* Residual noise NDF identifier */
  Grp *ngrp=GRP__NOID;          /* Group of noise model files */
  const char *nmodel;           /* Name of noisemodel group */
  dim_t numiter;                /* Total number iterations */
  int rebinflags;               /* Flags to control rebinning */
  double sigma;                 /* Estimate of standard deviation */
  HDSLoc *smurfloc;             /* HDS locator to the SMURF extension */
  int ubnd[1];                  /* Pixel bounds for 1d pointing array */
  int vexists;                  /* flag for presence of VARIANCE component */

  char *test;

  /* Main routine */
  if (*status != SAI__OK) return;

  /* Get/check the CONFIG parameters stored in the keymap */

  if( astMapGet0C( keymap, "ASTMODEL", &astmodel ) ) {
    /* parPut0c( "ASTMODEL", astmodel, status ); */

    /* parGet0c( "ASTMODEL", &test, status );
       printf("Put ASTMODEL=%s\n", test); */

    ndgCreat( "ASTMODEL", igrp, &astgrp, &gsize, &flag, status );
  } else {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "ASTMODEL unspecified", status);      
  }

  if( astMapGet0C( keymap, "ATMMODEL", &atmmodel ) ) {
    /* parPut0c( "ATMMODEL", atmmodel, status ); */

    /* parGet0c( "ATMMODEL", &test, status );
       printf("Put ATMMODEL=%s\n", test); */

    ndgCreat( "ATMMODEL", igrp, &atmgrp, &gsize, &flag, status );
  } else {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "ATMMODEL unspecified", status);      
  }

  if( astMapGet0C( keymap, "NOISEMODEL", &nmodel ) ) {
    /* parPut0c( "NOISEMODEL", nmodel, status ); */

    /* parGet0c( "NOISEMODEL", &test, status );
       printf("Put NOISEMODEL=%s\n", test); */

    ndgCreat( "NOISEMODEL", igrp, &ngrp, &gsize, &flag, status );
  } else {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "NOISEMODEL unspecified", status);      
  }

  if( !astMapGet0I( keymap, "NUMITER", &numiter ) ) {
    *status = SAI__ERROR;
    errRep(FUNC_NAME, "NUMITER unspecified", status);      
  }
  
  /* Create all of the model files by copying the input files */
  msgOut(" ", "SMF_ITERATEMAP: Create intermediate model files", status);

  for( i=1; i<=gsize; i++ ) {

    /* Open the input file DATA array and propagate it to the output
       model files (QUALITY and VARIANCE only need to get stored
       once) */       

    ndgNdfas( igrp, i, "UPDATE", &indf, status );

    ndgNdfpr( indf, " ", astgrp, i, &astndf, status );
    ndgNdfpr( indf, " ", atmgrp, i, &atmndf, status );
    ndgNdfpr( indf, " ", ngrp, i, &nndf, status );

    /* MAP the variance in the input file to create if not there */
    ndfMap( indf, "VARIANCE", "_DOUBLE", "WRITE", &mapptr[1], &nmap, 
	    status );

    /* Annul the input NDF */
    ndfAnnul( &indf, status);
    
    /* Set parameters of the DATA/VARIANCE arrays, map them so that they're
       defined on exit and then annul */

    ndfMap( astndf, "DATA", "_DOUBLE", "WRITE", &(mapptr[0]), &nmap, status );
    ndfAnnul( &astndf, status);

    ndfMap( atmndf, "DATA", "_DOUBLE", "WRITE", &(mapptr[0]), &nmap, status );
    ndfAnnul( &atmndf, status);

    ndfMap( nndf, "DATA", "_DOUBLE", "WRITE", &(mapptr[0]), &nmap, status );
    ndfAnnul( &nndf, status);
  }

  for( iter=0; iter<numiter; iter++ ) {
    
    msgSeti("ITER", iter+1);
    msgSeti("NUMITER", numiter);
    msgOut(" ", "SMF_ITERATEMAP: Iteration ^ITER / ^NUMITER ----------------", 
	   status);

    msgOut(" ", "SMF_ITERATEMAP: Calculate ATM/AST model", status);
    for( i=1; i<=gsize; i++ ) {

      /* Open files */
      smf_open_file( igrp, i, "UPDATE", 0, &idata, status );
      smf_open_file( atmgrp, i, "WRITE", 0, &atmdata, status );
      smf_open_file( astgrp, i, "WRITE", 0, &astdata, status );
      
      /* Loop over time slices and calculate atmos/bolo signal */
      if( *status == SAI__OK ) {
	nbolo = (idata->dims)[0] * (idata->dims)[1];
      
	for( j=0; j<(idata->dims)[2]; j++ ) {
	  /* First time around just use the array mean for atmosphere*/
	  if( iter == 0 ) {
	    smf_calc_stats( idata, "t", j, 0, 0, &mean, &sigma, status );
	    
	    for( k=0; k<nbolo; k++ ) {
	      /* Place mean into atmosphere model file */
	      ((double *)(atmdata->pntr)[0])[j*nbolo + k] = mean;
	      
	      /* Calculate bolo signal minus atmosphere guess */
	      ((double *)(astdata->pntr)[0])[j*nbolo + k] = 
		((double *)(idata->pntr)[0])[j*nbolo + k] - mean; 

	      /* Set initial variance to 1 for all data points */
	      ((double *)(idata->pntr)[1])[j*nbolo + k] = 1.; 
	    }
	    
	  } else {
	    /* Subsequent iterations calc atmos after removing source signal */
	    
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
      }

      /* Close files */
      smf_close_file( &idata, status );
      smf_close_file( &atmdata, status );
      smf_close_file( &astdata, status );    
    }

    msgOut(" ", "SMF_ITERATEMAP: Rebin AST signal to estimate MAP", status);
    if( *status == SAI__OK ) {
      for( i=1; i<=gsize; i++ ) {
	/* Open files */
	smf_open_file( igrp, i, "READ", 0, &idata, status );
	smf_open_file( astgrp, i, "READ", 0, &astdata, status );
	
	nbolo = (astdata->dims)[0] * (astdata->dims)[1];
	
	/* Open the LUT from the SMURF.MAPCOORD extension */
	smf_open_mapcoord( idata, status );

	if( *status == SAI__OK ) {
	  lut = idata->lut;
	}

	/* Set up the flags for the rebinner */
	rebinflags = 0;
	if( i == 1 )                                      
	  rebinflags = rebinflags | AST__REBININIT;
	
	if( i == gsize ) 
	  rebinflags = rebinflags | AST__REBINEND;
	
	smf_simplerebinmap( (double *)(astdata->pntr)[0], 
			    (double *)(idata->pntr)[1], lut, 
			    nbolo*(astdata->dims)[2], rebinflags,
			    map, weights, variance, msize, status );
	
	/* Close files */
	smf_close_file( &idata, status );   
	smf_close_file( &astdata, status );   
      }
    }

    msgOut(" ", "SMF_ITERATEMAP: Sample MAP, calculate NOISE", status);

    if( *status == SAI__OK ) {
      for( i=1; i<=gsize; i++ ) {
	/* Open files */
	smf_open_file( igrp, i, "UPDATE", 0, &idata, status );
	smf_open_file( astgrp, i, "WRITE", 0, &astdata, status );
	smf_open_file( atmgrp, i, "READ", 0, &atmdata, status );
	smf_open_file( ngrp, i, "WRITE", 0, &ndata, status );
	
	nbolo = (astdata->dims)[0] * (astdata->dims)[1];

	/* Open the LUT from the SMURF.MAPCOORD extension */	
	smf_open_mapcoord( idata, status );

	if( *status == SAI__OK ) {
	  lut = idata->lut;
	}

	/* Loop over all data points in this file */
	if( *status == SAI__OK ) {
	  for( j=0; j<nbolo*(astdata->dims)[2]; j++ ) {
	    if( lut[j] != VAL__BADI ) {
	      /* Sample the map into astdata */
	      ((double *)(astdata->pntr)[0])[j] = map[lut[j]];
	      
	      /* Re-estimate the noise signal as input - atmdata - astdata */
	      ((double *)(ndata->pntr)[0])[j] = 
		((double *)(idata->pntr)[0])[j] - 
		((double *)(atmdata->pntr)[0])[j] - map[lut[j]];
	    }
	  }
	}
	
	/* Loop over bolometers and re-calculate noise parameters from
	   new estimate of clean noise signals */
	for( j=0; j<nbolo; j++ ) {
	  smf_calc_stats( ndata, "b", j, 0, 0, &mean, &sigma, status );      
	  /* At each time slice insert noise estimate^2 into variance
	     for each data point */
	  
	  for( k=0; k<(astdata->dims)[2]; k++ ) {
	    ((double *)(idata->pntr)[1])[k*nbolo + j] = sigma*sigma;
	  }
	  
	  /* Jump out if there was a problem */
	  if( *status != SAI__OK ) j=nbolo-1;
	}
	
	/* Close files */
	smf_close_file( &idata, status );    
	smf_close_file( &astdata, status );    
	smf_close_file( &atmdata, status );    
	smf_close_file( &ndata, status );    
      }
    }
  }

  /* Cleanup */
  if( astgrp != GRP__NOID ) grpDelet( &astgrp, status );
  if( atmgrp != GRP__NOID ) grpDelet( &atmgrp, status );
  if( ngrp != GRP__NOID ) grpDelet( &ngrp, status );

}
