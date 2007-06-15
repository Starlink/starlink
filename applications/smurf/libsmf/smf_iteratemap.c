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
*     2007-03-05 (EC)
*        Parse modelorder keyword in config file
*     2007-04-30 (EC)
*        Put map estimation in first chunk loop, only ast in second
*     2007-05-17 (EC)
*        Added missing status checks
*     2007-06-13 (EC):
*        - Use new DIMM binary file format 
*     2007-06-14 (EC):
*        - If config file has exportndf set, export DIMM components to *.sdf


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

/* Other includes */
#include "sys/time.h"


#define FUNC_NAME "smf_iteratemap"

void smf_iteratemap( Grp *igrp, AstKeyMap *keymap, double *map, 
		     double *mapvar, double *weights,
		     int msize, int *status ) {

  /* Local Variables */
  int added;                    /* Number of names added to group */
  int astndf;                   /* Astronomical signal NDF identifier */
  smfData *ast=NULL;            /* Pointer to astronomical data struct */
  double *ast_data=NULL;        /* Pointer to DATA component of ast */
  Grp *astgrp=NULL;             /* Group of ast model files */
  const char *astname;          /* Name of astmodel group */
  const char *asttemp=NULL;     /* Pointer to static strings created by ast */
  int atmndf;                   /* Atmospheric signal NDF identifier */
  const char *atmname;          /* Name of atmmodel group */
  smfData *data=NULL;           /* Pointer to source data struct */
  double *data_data=NULL;       /* Pointer to DATA component of data */
  int dimmflags;                /* Control flags for DIMM model components */
  dim_t dsize;                  /* Size of data arrays in containers */
  int exportNDF=0;              /* If set export DIMM files to NDF at end */
  int flag;                     /* Flag */
  dim_t i;                      /* Loop counter */
  int indf;                     /* Input data NDF identifier */
  dim_t iter;                   /* Iteration number */
  int isize;                    /* Number of files in input group */
  dim_t j;                      /* Loop counter */
  dim_t k;                      /* Loop counter */
  smfData *lut=NULL;            /* Pointer to pointing LUT data struct */
  int *lut_data=NULL;           /* Pointer to DATA component of lut */
  Grp *lutgrp=NULL;             /* Group of lut model files */
  void *mapptr[3];              /* Pointer to array of mapped components */
  double mean;                  /* Estimate of mean */
  smfData **modeldata=NULL;     /* Array of pointers to model data */
  Grp **modelgrps=NULL;         /* Array of group ptrs/ each model component */
  smf_modeltype *modeltyps=NULL; /* Array of model types */
  char modelname[4];            /* Name of current model component */
  smf_calcmodelptr modelptr=NULL; /* Pointer to current model calc function */
  int nbolo;                    /* Number of bolometers */
  int nmap;                     /* Number of elements mapped */
  int nmodels=0;                /* Number of model components / iteration */
  int nndf;                     /* Residual noise NDF identifier */
  const char *nname;            /* Name of noisemodel group */
  smfData *noi=NULL;            /* Pointer to noise model data struct */
  Grp *noigrp=NULL;             /* Group of noi model files */
  int numiter;                  /* Total number iterations */
  int pass;                     /* Two pass parsing of MODELORDER */
  int rebinflags;               /* Flags to control rebinning */
  smfData *res=NULL;            /* Pointer to residual data struct */
  double *res_data=NULL;        /* Pointer to DATA component of res */
  double *res_var=NULL;         /* Pointer to DATA component of res */
  Grp *resgrp=NULL;             /* Group of model residual files */
  double sigma;                 /* Estimate of standard deviation */
  smf_modeltype thismodel;      /* Type of current model */

  /* Main routine */
  if (*status != SAI__OK) return;

  /* Get size of the input group */
  grpGrpsz( igrp, &isize, status );

  /* Parse the CONFIG parameters stored in the keymap */

  if( *status == SAI__OK ) {
    /* Number of iterations */
    if( !astMapGet0I( keymap, "NUMITER", &numiter ) ) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "DIMM Failed: NUMITER unspecified", status);      
    }

    /* Will we export components to NDF at the end? */
    if( !astMapGet0I( keymap, "EXPORTNDF", &exportNDF ) ) {
      exportNDF = 0;
    }

    /* Type and order of models to fit from MODELORDER keyword */
    if( astMapGet0C( keymap, "MODELORDER", &asttemp ) ) {

      modelname[3] = 0;

      /* First pass count components, second pass allocate modeltyps & store */
      for( pass=1; pass<=2; pass++ ) {
	j=0; /* Count number of characters in sub-string */
	nmodels = 0;

	/* Loop over all characters in asttemp, count # valid model names */
	for( i=0; i<strlen(asttemp); i++ ) {
	  
	  /* If current asttemp character non-delimeter, copy to modelname */
	  if( asttemp[i] != ' ' ) {
	    modelname[j] = asttemp[i];
	    j++;
	  }
	  
	  /* If 3 characters in sub-string, extract type */
	  if( j == 3 ) {

	    thismodel = smf_model_gettype( modelname, status );	    

	    if( *status == SAI__OK ) {
	      
	      /* If second pass modeltyps is allocated - store value */
	      if( pass == 2 ) {
		modeltyps[nmodels] = thismodel;
	      }
	      nmodels++;
	      j = 0;
	    }
	  }
	}
	
	/* End of pass 1: allocate modeltyps */
	if( pass == 1 ) {
	  if( nmodels >= 1 ) {
	    modeltyps = smf_malloc( nmodels, sizeof(*modeltyps), 0, status );
	  } else {
	    msgOut(" ", "SMF_ITERATEMAP: No valid models in MODELORDER",
		   status );
	  }
	}
      }
    } else {
      msgOut(" ", "SMF_ITERATEMAP: MODELORDER unspecified - will only rebin!",
	     status);
      nmodels = 0;
    }
  } 

  msgSeti("NUMCOMP",nmodels);
  msgOut(" ", "SMF_ITERATEMAP: ^NUMCOMP model components in solution: ", 
	 status);
  for( i=0; i<nmodels; i++ ) {
    msgSetc("MNAME", smf_model_getname(modeltyps[i],status) );
    msgOut(" ", "  ^MNAME", status ); 
  }

  /* Create groups of NDFs for time-series model components */
  msgOut(" ", "SMF_ITERATEMAP: Create model containers", status);

  if( nmodels > 0 ) {
    modeldata = smf_malloc( nmodels, sizeof(*modeldata), 1, status );  
    modelgrps = smf_malloc( nmodels, sizeof(*modelgrps), 1, status );  
  }

  /* These always get made */

  smf_model_create( igrp, SMF__RES, &resgrp, status );
  smf_model_create( igrp, SMF__LUT, &lutgrp, status ); 
  smf_model_create( igrp, SMF__AST, &astgrp, status );

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
      
      for( i=1; i<=isize; i++ ) {
	msgSeti("CHUNK", i);
	msgSeti("NUMCHUNK", isize);
	msgOut(" ", "SMF_ITERATEMAP: Chunk ^CHUNK / ^NUMCHUNK", status);
	msgOut(" ", "SMF_ITERATEMAP: Calculate time-stream model components", 
	       status);

        /* Open model files */

	smf_open_model( resgrp, i, "UPDATE", &res, status );
	smf_open_model( lutgrp, i, "READ", &lut, status );
	smf_open_model( astgrp, i, "UPDATE", &ast, status );

	for( j=0; j<nmodels; j++ ) {
	  smf_open_model( modelgrps[j], i, "UPDATE", &modeldata[j], status );
	}

	/* Call the model calculations in the desired order. */

	if( *status == SAI__OK ) {
	  for( j=0; j<nmodels; j++ ) {
	    
	    /* Set up control flags for the model calculation */
	    dimmflags = 0;
	    if( iter==0 ) dimmflags |= SMF__DIMM_FIRSTITER;
	    if( j==0 ) dimmflags |= SMF__DIMM_FIRSTCOMP;
	    
	    msgSetc("MNAME", smf_model_getname(modeltyps[j],status));
	    msgOut( " ", "  ^MNAME", status);
	    modelptr = smf_model_getptr( modeltyps[j], status );
	    
	    if( *status == SAI__OK ) {
	      (*modelptr)( res, keymap, map, mapvar, modeldata[j],
			   dimmflags, status );
	    }

	    /* If bad status set exit condition */
	    if( *status != SAI__OK ) {
	      j = nmodels;
	    }
	  }
	}

	/* Once all the other map components have been calculated, but the
           previous iteration of AST back into the residual, zero ast,
           and rebin the noise+astro signal into the map */

	if( *status == SAI__OK ) {

	  /* Add last iteration of astronomical signal back in to residual */
	  ast_data = (double *)(ast->pntr)[0];
	  res_data = (double *)(res->pntr)[0];
	  res_var = (double *)(res->pntr)[1];
	  lut_data = (int *)(lut->pntr)[0];

	  dsize = (ast->dims)[0]*(ast->dims)[1]*(ast->dims)[2];

	  for( k=0; k<dsize; k++ ) {	  
	    res_data[k] += ast_data[k];
	  
	    /* Set ast_data back to 0 since we've moved all of the signal
	       into the map, and then it will get re-estimated by
	       calcmodel_ast at the start of the next iteration. */
	  
	    ast_data[k] = 0;
	  }
	}

	/* Load the LUT from the mapcoord extension */
	/* smf_open_mapcoord( res, status ); */

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

	msgOut(" ", "SMF_ITERATEMAP: Rebin residual to estimate MAP", status);
	smf_simplerebinmap( res_data, res_var, lut_data, dsize,
			    rebinflags, map, weights, mapvar,
			    msize, status );
	  
        /* Close files */

        smf_close_file( &res, status );
        smf_close_file( &ast, status );    
        smf_close_file( &lut, status );    

	for( j=0; j<nmodels; j++ ) {
	  smf_close_file( &modeldata[j], status );
	}

	/* Set exit condition if bad status was set */
	if( *status != SAI__OK ) i=isize+1;
      }
      

      /*if( iter == 1 ) return;*/

      if( *status == SAI__OK ) {
	msgOut(" ", "SMF_ITERATEMAP: Calculate ast", status);

        for( i=1; i<=isize; i++ ) {
	  smf_open_model( astgrp, i, "UPDATE", &ast, status );
	  smf_open_model( resgrp, i, "UPDATE", &res, status );
	  smf_open_model( lutgrp, i, "READ", &lut, status );

	  /* Calculate the AST model component. It is a special model
	     because it assumes that the map contains the best current
	     estimate of the astronomical sky. It gets called in this
             separate loop since the map estimate gets updated by
             each chunk in the main model component loop */

	  if( *status == SAI__OK ) {
	    lut_data = (int *)(lut->pntr)[0];
	    smf_calcmodel_ast( res, keymap, lut_data, map, mapvar, ast, 0, 
			       status );
	  }

	  /* Finally calculate the noise from the residual */

	  smf_close_file( &ast, status );    
	  smf_close_file( &res, status );
	  smf_close_file( &lut, status );
        }
      }
    }

    /* Export DIMM model components to NDF files.
       Note that we don't do LUT since it is originall an extension in the
       input flatfield data */
  
    if( exportNDF ) {
      msgOut(" ", "SMF_ITERATEMAP: Export model components to NDF files.", 
	     status);
      
      for( i=1; i<=isize; i++ ) {    /* Chunk loop */
	smf_open_model( resgrp, i, "READ", &res, status );
	smf_model_NDFexport( res, res->file->name, status );
	smf_close_file( &res, status );
	
	smf_open_model( astgrp, i, "READ", &ast, status );
	smf_model_NDFexport( ast, ast->file->name, status );
	smf_close_file( &ast, status );
	
	for( j=0; j<nmodels; j++ ) { /* Dynamic model component loop */
	  smf_open_model( modelgrps[j], i, "READ", &modeldata[j], status );
	  smf_model_NDFexport( modeldata[j], (modeldata[j])->file->name, 
			       status );
	  smf_close_file( &modeldata[j], status );
	}
      }
    }
  }

  /* Cleanup */

  if( resgrp ) grpDelet( &resgrp, status );
  if( astgrp ) grpDelet( &astgrp, status );  

  for( j=0; j<nmodels; j++ ) {
    if( modelgrps[j] ) grpDelet( &(modelgrps[j]), status );
  }

  if( modeltyps ) smf_free( modeltyps, status );
}
