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
*     smf_iteratemap( Grp *igrp, AstKeyMap *keymap, double *map, 
*		     double *variance, double *weights,
*		     int msize, int *status );

*  Arguments:
*     igrp = Grp* (Given)
*        Group of input data files
*     keymap = AstKeyMap* (Given)
*        keymap containing parameters to control map-maker
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
*     EC: Edward Chapin (UBC)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2006-05-18 (EC):
*        Initial version.
*     2006-06-24 (EC):
*        Parameters given by keymap
*     2006-08-07 (TIMJ):
*        GRP__NOID is not a Fortran concept.
*     2006-08-16 (EC):
*        Intermediate step: Old routine works with new model container code
*     2007-02-07 (EC):
*        Fixed bugs in implementation of models, order of execution.
*     2007-02-08 (EC):
*        Changed location of AST model calculation
*        Fixed pointer bug for variance of residual in map estimate
*     2007-02-12 (EC)
*        Enabled dyanmic usage of model components using function pointers
*     2007-03-02 (EC)
*        Added noise estimation from residual
*        Fixed counter bug

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

#include "sys/time.h"

#define FUNC_NAME "smf_iteratemap"

void smf_iteratemap( Grp *igrp, AstKeyMap *keymap, double *map, 
		     double *mapvar, double *weights,
		     int msize, int *status ) {

  /* Local Variables */
  int added;                    /* Number of names added to group */
  int astndf;                   /* Astronomical signal NDF identifier */
  smfData *ast;                 /* Pointer to astronomical data struct */
  double *ast_data=NULL;        /* Pointer to DATA component of ast */
  Grp *astgrp=NULL;             /* Group of ast model files */
  const char *astname;          /* Name of astmodel group */
  const char *asttemp=NULL;     /* Pointer to static strings created by ast */
  int atmndf;                   /* Atmospheric signal NDF identifier */
  const char *atmname;          /* Name of atmmodel group */
  Grp *cumgrp=NULL;             /* Group of cumulative model files */
  smfData *data;                /* Pointer to source data struct */
  double *data_data=NULL;       /* Pointer to DATA component of data */
  dim_t dsize;                  /* Size of data arrays in containers */
  int flag;                     /* Flag */
  dim_t i;                      /* Loop counter */
  int indf;                     /* Input data NDF identifier */
  smfData *cum;                 /* Pointer to input data struct */
  dim_t iter;                   /* Iteration number */
  int isize;                    /* Number of files in input group */
  dim_t j;                      /* Loop counter */
  dim_t k;                      /* Loop counter */
  int *lut=NULL;                /* Pointing lookup table */
  void *mapptr[3];              /* Pointer to array of mapped components */
  double mean;                  /* Estimate of mean */
  smfData **modeldata=NULL;     /* Array of pointers to model data */
  Grp **modelgrps=NULL;         /* Array of group ptrs/ each model component */
  smf_modeltype *modeltyps=NULL; /* Array of model types */
  smf_calcmodelptr modelptr=NULL; /* Pointer to current model calc function */
  int nbolo;                    /* Number of bolometers */
  int nmap;                     /* Number of elements mapped */
  int nmodels;                  /* Number of model components / iteration */
  int nndf;                     /* Residual noise NDF identifier */
  const char *nname;            /* Name of noisemodel group */
  smfData *noi;                 /* Pointer to noise model data struct */
  Grp *noigrp=NULL;             /* Group of noi model files */
  int numiter;                  /* Total number iterations */
  int rebinflags;               /* Flags to control rebinning */
  smfData *res;                 /* Pointer to residual data struct */
  double *res_data=NULL;        /* Pointer to DATA component of res */
  double *res_var=NULL;         /* Pointer to DATA component of res */
  Grp *resgrp=NULL;             /* Group of model residual files */
  double sigma;                 /* Estimate of standard deviation */

  /* Main routine */
  if (*status != SAI__OK) return;

  /* Get size of the input group */
  grpGrpsz( igrp, &isize, status );

  /* Get/check the CONFIG parameters stored in the keymap */

  if( *status == SAI__OK ) {
    /* Number of iterations */
    if( !astMapGet0I( keymap, "NUMITER", &numiter ) ) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "DIMM Failed: NUMITER unspecified", status);      
    }

    /* Type and order of models to fit */
    if( astMapGet0C( keymap, "MODELORDER", &asttemp ) ) {
      /* KLUDGE for now */
      nmodels = 1;
      modeltyps = smf_malloc( nmodels, sizeof(*modeltyps), 0, status );
      modeltyps[0] = SMF__COM;
    } else {
      msgOut(" ", "SMF_ITERATEMAP: MODELORDER unspecified - will only rebin!",
	     status);
      nmodels = 0;
    }

  } 

  /* Create groups of NDFs for time-series model components */
  msgOut(" ", "SMF_ITERATEMAP: Create model containers", status);

  if( nmodels > 0 ) {
    modeldata = smf_malloc( nmodels, sizeof(*modeldata), 0, status );  
    modelgrps = smf_malloc( nmodels, sizeof(*modelgrps), 0, status );  
  }

  /* These always get made */
  smf_model_create( igrp, SMF__CUM, &cumgrp, status );
  smf_model_create( igrp, SMF__RES, &resgrp, status );
  smf_model_create( igrp, SMF__AST, &astgrp, status );
  smf_model_create( igrp, SMF__NOI, &noigrp, status );

  /* Dynamically created models */
  for( j=0; j<nmodels; j++ ) {
    smf_model_create( igrp, modeltyps[j], &modelgrps[j], status );
  }

  /* Start the main iteration loop */
  if( *status == SAI__OK ) {

    for( iter=0; iter<numiter; iter++ ) {    
      msgSeti("ITER", iter+1);
      msgSeti("NUMITER", numiter);
      msgOut(" ", "SMF_ITERATEMAP: Iteration ^ITER / ^NUMITER ---------------",
             status);
      
      msgOut(" ", "SMF_ITERATEMAP: Calculate time-stream model components", 
	     status);
      for( i=1; i<=isize; i++ ) {
        /* Open files */

        smf_open_file( cumgrp, i, "UPDATE", 0, &cum, status );
        smf_open_file( resgrp, i, "UPDATE", 0, &res, status );
        smf_open_file( astgrp, i, "UPDATE", 0, &ast, status );

	for( j=0; j<nmodels; j++ ) {
	  smf_open_file( modelgrps[j], i, "UPDATE", 0, &modeldata[j], status );
	}

	/* Call the model calculations in the desired order. The first
           one should have flags set to 1 to zero the cumulative model */

	for( j=0; j<nmodels; j++ ) {
	  msgSetc("MNAME", smf_model_getname(modeltyps[j],status));
	  msgOut( " ", "  ^MNAME", status);
	  modelptr = smf_model_getptr( modeltyps[j], status );

	  (*modelptr)( cum, res, keymap, map, mapvar, modeldata[j],
		       j==0, status ); 	
	}

        /* Close files */
        smf_close_file( &cum, status );
        smf_close_file( &res, status );
        smf_close_file( &ast, status );    

	for( j=0; j<nmodels; j++ ) {
	  smf_close_file( &modeldata[j], status );
	}

	/* Set exit condition if bad status was set */
	if( *status != SAI__OK ) i=isize+1;
      }
      
      msgOut(" ", "SMF_ITERATEMAP: Rebin residual to estimate MAP", status);
      if( *status == SAI__OK ) {
        for( i=1; i<=isize; i++ ) {
	  smf_open_file( astgrp, i, "UPDATE", 0, &ast, status );
	  smf_open_file( resgrp, i, "UPDATE", 0, &res, status );
	  smf_open_file( cumgrp, i, "UPDATE", 0, &cum, status );
	  smf_open_file( noigrp, i, "UPDATE", 0, &noi, status );

	  if( *status == SAI__OK ) {

	  /* Add last iteration of astronomical signal back in */
	    ast_data = (double *)(ast->pntr)[0];
	    res_data = (double *)(res->pntr)[0];
	    res_var = (double *)(res->pntr)[1];

	    dsize = (ast->dims)[0]*(ast->dims)[1]*(ast->dims)[2];

	    for( j=0; j<dsize; j++ ) {

	      res_data[j] += ast_data[j];

	      /* Set ast_data back to 0 since we've moved all of the signal
                 into the map, and then it will get re-estimated by
                 calcmodel_ast at the start of the next iteration. */

	      ast_data[j] = 0;
	    }

	  }

	  /* Load the LUT from the mapcoord extension */
	  smf_open_mapcoord( res, status );

	  if( *status == SAI__OK ) {
	    /* Should check if bad status due to lack of extension, in
	       which case try calculating it */
	    lut = res->lut;
	  }

	  if( *status == SAI__OK ) {

	    /* Setup rebin flags */
	    rebinflags = 0;
	    if( i == 1 ) {
	      rebinflags = rebinflags | AST__REBININIT;
	    }
	    
	    if( i == isize ) {
	      rebinflags = rebinflags | AST__REBINEND;
	    }
	    
	  }

	  /* Rebin the residual + astronomical signal into a map */

	  smf_simplerebinmap( res_data, res_var, lut, dsize,
			      rebinflags, map, weights, mapvar,
			      msize, status );
	  
	  /* Calculate the AST model component. It is a special model
	     because it assumes that the map contains the best current
	     estimate of the astronomical sky. Since the map will have
	     been re-estimated at the end of the last iteration, we
	     always call the ast model calculation first before proceeding
	     with other model components. */
	  
	  smf_calcmodel_ast( cum, res, keymap, map, mapvar, ast, 0, status );
	  
	  /* Finally calculate the noise from the residual */

	  smf_calcmodel_noi( cum, res, keymap, map, mapvar, noi, 0, status );
	     
	  smf_close_file( &ast, status );    
	  smf_close_file( &res, status );
	  smf_close_file( &cum, status );	  
	  smf_close_file( &noi, status );	  
        }
      }
    }
  }

  /* Cleanup */

  if( cumgrp ) grpDelet( &cumgrp, status );
  if( resgrp ) grpDelet( &resgrp, status );
  if( astgrp ) grpDelet( &astgrp, status );  

  for( j=0; j<nmodels; j++ ) {
    if( modelgrps[j] ) grpDelet( &(modelgrps[j]), status );

  }

  if( modeltyps ) smf_free( modeltyps, status );

}
