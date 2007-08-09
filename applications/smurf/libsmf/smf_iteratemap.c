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
*     2007-07-10 (EC):
*        - Use smfGroups and smfArrays instead of groups and smfdatas
*        - fixed problem with function pointer type casting
*     2007-07-13 (EC):
*        - use arrays of pointers to all chunks to store modeldata/modelgroups
*     2007-07-20 (EC):
*        - fixed freeing of modeldata
*     2007-08-09 (EC):
*        - changed index order for model
*        - added memiter flag to config file - avoids doing file i/o 

*  Notes:

*  Copyright:
*     Copyright (C) 2005-2006 Particle Physics and Astronomy Research Council.
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
  smfArray **ast=NULL;          /* Astronomical signal */
  double *ast_data=NULL;        /* Pointer to DATA component of ast */
  smfGroup *astgroup=NULL;      /* smfGroup of ast model files */
  const char *asttemp=NULL;     /* Pointer to static strings created by ast */
  int dimmflags;                /* Control flags for DIMM model components */
  dim_t dsize;                  /* Size of data arrays in containers */
  int exportNDF=0;              /* If set export DIMM files to NDF at end */
  int flag;                     /* Flag */
  dim_t i;                      /* Loop counter */
  int idx=0;                    /* index within subgroup */
  smfGroup *igroup=NULL;        /* smfGroup corresponding to igrp */
  int indf;                     /* Input data NDF identifier */
  dim_t iter;                   /* Iteration number */
  int isize;                    /* Number of files in input group */
  dim_t j;                      /* Loop counter */
  dim_t k;                      /* Loop counter */
  dim_t l;                      /* Loop counter */
  smfArray **lut=NULL;          /* Pointing LUT for each file */
  int *lut_data=NULL;           /* Pointer to DATA component of lut */
  smfGroup *lutgroup=NULL;      /* smfGroup of lut model files */
  int memiter=0;                /* If set iterate completely in memory */
  smfArray ***model=NULL;       /* Array of pointers smfArrays for e. model */
  smfGroup **modelgroups=NULL;  /* Array of group ptrs/ each model component */
  smf_modeltype *modeltyps=NULL;/* Array of model types */
  char modelname[4];            /* Name of current model component */
  smf_calcmodelptr modelptr=NULL; /* Pointer to current model calc function */
  int nbolo;                    /* Number of bolometers */
  int nchunks=0;                /* Number of time-chunks of data */
  int nmap;                     /* Number of elements mapped */
  int nmodels=0;                /* Number of model components / iteration */
  int numiter;                  /* Total number iterations */
  int pass;                     /* Two pass parsing of MODELORDER */
  int rebinflags;               /* Flags to control rebinning */
  smfArray **res=NULL;          /* Residual signal */
  double *res_data=NULL;        /* Pointer to DATA component of res */
  smfGroup *resgroup=NULL;      /* smfGroup of model residual files */
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

    /* Do iterations completely in memory - minimize disk I/O */
    if( !astMapGet0I( keymap, "MEMITER", &memiter ) ) {
      memiter = 0;
    } 

    if( memiter ) {
      msgOut(" ", "SMF_ITERATEMAP: MEMITER set; perform iterations in memory",
	     status );
    } else {
      msgOut(" ", 
	     "SMF_ITERATEMAP: MEMITER not set; perform iterations on disk",
	     status );
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
    msgSetc( "MNAME", smf_model_getname(modeltyps[i], status) );
    msgOut(" ", "  ^MNAME", status ); 
  }

  /* Create an ordered smfGrp which keeps track of files corresponding to
     different subarrays (observed simultaneously), as well as time-ordering
     the files */

  smf_grp_related( igrp, isize, 1, &igroup, status );
  if( *status == SAI__OK ) {
    nchunks = igroup->ngroups;
    msgSeti( "NCHUNKS", nchunks );
    msgOut(" ", "SMF_ITERATEMAP: ^NCHUNKS time chunks", status);
  }

  /* Create containers for time-series model components */
  msgOut(" ", "SMF_ITERATEMAP: Create model containers", status);

  /* Components that always get made */
  if( igroup && (*status == SAI__OK) ) {

    /* there is one smfArray for RES, LUT and AST at each chunk */
    res = smf_malloc( nchunks, sizeof(*res), 1, status );
    lut = smf_malloc( nchunks, sizeof(*lut), 1, status );
    ast = smf_malloc( nchunks, sizeof(*ast), 1, status );

    smf_model_create( igroup, SMF__RES, &resgroup, memiter, res, status );
    smf_model_create( igroup, SMF__LUT, &lutgroup, memiter, lut, status ); 
    smf_model_create( igroup, SMF__AST, &astgroup, memiter, ast, status );
  }

  /* Dynamic components */
  if( igroup && (nmodels > 0) && (*status == SAI__OK) ) {

    /* Array of smfgroups (one for each dynamic model component) */
    modelgroups = smf_malloc( nmodels, sizeof(*modelgroups), 1, status );  

    /* nmodel array of pointers to nchunk smfArray pointers */
    model = smf_malloc( nmodels, sizeof(*model), 1, status );
    
    for( i=0; i<nmodels; i++ ) {
      model[i] = smf_malloc( nchunks, sizeof(**model), 1, status );

      smf_model_create( igroup, modeltyps[i], &modelgroups[i], memiter, 
			model[i], status );
    }
  }

  /* Start the main iteration loop */
  if( *status == SAI__OK ) {

    for( iter=0; iter<numiter; iter++ ) {    
      msgSeti("ITER", iter+1);
      msgSeti("NUMITER", numiter);
      msgOut(" ", "SMF_ITERATEMAP: Iteration ^ITER / ^NUMITER ---------------",
             status);
      
      for( i=0; i<nchunks; i++ ) {
	msgSeti("CHUNK", i+1);
	msgSeti("NUMCHUNK", nchunks);
	msgOut(" ", "SMF_ITERATEMAP: Chunk ^CHUNK / ^NUMCHUNK", status);
	msgOut(" ", "SMF_ITERATEMAP: Calculate time-stream model components", 
	       status);

        /* Open model files here if looping on-disk. Otherwise everything
           is already  open from the smf_model_create calls */

	if( !memiter ) {
	  
	  /* If memiter not set, use efficient access modes */
	  smf_open_related_model( resgroup, i, "UPDATE", &res[i], status );
	  smf_open_related_model( lutgroup, i, "READ", &lut[i], status );
	  smf_open_related_model( astgroup, i, "UPDATE", &ast[i], status );

	  for( j=0; j<nmodels; j++ ) {
	    smf_open_related_model( modelgroups[j], i, "UPDATE", &model[j][i], 
				    status );
	  }
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
	      (*modelptr)( res[i], keymap, map, mapvar, model[j][i],
			   dimmflags, status );
	    }

	    /* If bad status set exit condition */
	    if( *status != SAI__OK ) {
	      j = nmodels;
	    }
	  }
	}

	/* Once all the other map components have been calculated put the
           previous iteration of AST back into the residual, zero ast,
           and rebin the noise+astro signal into the map */

	msgOut(" ", "SMF_ITERATEMAP: Rebin residual to estimate MAP", status);

	if( *status == SAI__OK ) {

	  /* Loop over subgroup index (subarray) */
	  for( idx=0; idx<res[i]->ndat; idx++ ) {

	    /* Add last iter. of astronomical signal back in to residual */
	    ast_data = (double *)(ast[i]->sdata[idx]->pntr)[0];
	    res_data = (double *)(res[i]->sdata[idx]->pntr)[0];
	    lut_data = (int *)(lut[i]->sdata[idx]->pntr)[0];
	    
	    dsize = (ast[i]->sdata[idx]->dims)[0] *
	      (ast[i]->sdata[idx]->dims)[1] * (ast[i]->sdata[idx]->dims)[2];

	    for( k=0; k<dsize; k++ ) {	  
	      res_data[k] += ast_data[k];
	      
	      /* Set ast_data back to 0 since we've moved all of the signal
		 into the residual, and then it will get re-estimated by
		 calcmodel_ast after we finish estimating the map. */
	      
	      ast_data[k] = 0;
	    }

	    /* Setup rebin flags */
	    rebinflags = 0;
	    if( (i == 0) && (idx == 0) ) {   
	      /* First call to rebin clears the arrays */
	      rebinflags = rebinflags | AST__REBININIT;
	    }
	    
	    if( (i == nchunks-1) && (idx == res[i]->ndat-1) ) {
	      /* Final call to rebin re-normalizes */
	      rebinflags = rebinflags | AST__REBINEND;
	    }
	    
	    /* Rebin the residual + astronomical signal into a map */
	    smf_simplerebinmap( res_data, NULL, lut_data, dsize,
				rebinflags, map, weights, mapvar,
				msize, status );
	  }
	}

        /* Close files here if memiter not set */

	if( !memiter ) {

	  smf_close_related( &res[i], status );
	  smf_close_related( &ast[i], status );    
	  smf_close_related( &lut[i], status );    

	  for( j=0; j<nmodels; j++ ) {
	    smf_close_related( &model[j][i], status );
	  }
	}

	/* Set exit condition if bad status was set */
	if( *status != SAI__OK ) i=isize+1;
      }
      
      if( *status == SAI__OK ) {
	msgOut(" ", "SMF_ITERATEMAP: Calculate ast", status);

	for( i=0; i<nchunks; i++ ) {

	  /* Open files if memiter not set - otherwise they are still open
             from earlier call */
	  if( !memiter ) {

	    smf_open_related_model( astgroup, i, "UPDATE", &ast[i], status );
	    smf_open_related_model( resgroup, i, "UPDATE", &res[i], status );
	    smf_open_related_model( lutgroup, i, "READ", &lut[i], status );
	  }

	  /* Calculate the AST model component. It is a special model
	     because it assumes that the map contains the best current
	     estimate of the astronomical sky. It gets called in this
             separate loop since the map estimate gets updated by
             each chunk in the main model component loop */

	  smf_calcmodel_ast( res[i], keymap, lut[i], map, mapvar, ast[i], 0, 
			     status );

	  /* Close files if memiter not set */
	  if( !memiter ) {

	    smf_close_related( &ast[i], status );    
	    smf_close_related( &res[i], status );
	    smf_close_related( &lut[i], status );
	  }
        }
      }
    }

    /* Export DIMM model components to NDF files.
       Note that we don't do LUT since it is originally an extension in the
       input flatfielded data */
  
    if( exportNDF && (*status == SAI__OK) ) {
      msgOut(" ", "SMF_ITERATEMAP: Export model components to NDF files.", 
	     status);
      
      for( i=0; i<nchunks; i++ ) {  /* Chunk loop */
	msgSeti("CHUNK", i+1);
	msgSeti("NUMCHUNK", nchunks);
	msgOut(" ", "  Chunk ^CHUNK / ^NUMCHUNK", status);

	/* Open each subgroup, loop over smfArray elements and export,
           then close subgroup. DIMM open/close not needed if memiter set */

	if( !memiter ) 
	  smf_open_related_model( resgroup, i, "READ", &res[i], status );

	for( idx=0; idx<res[i]->ndat; idx++ ) {
	  smf_model_NDFexport( res[i]->sdata[idx], 
			       res[i]->sdata[idx]->file->name, 
			       status );
	}
	if( !memiter ) 
	  smf_close_related( &res[i], status );

	if( !memiter ) 
	  smf_open_related_model( astgroup, i, "READ", &ast[i], status );

	for( idx=0; idx<ast[i]->ndat; idx++ ) {
	  smf_model_NDFexport( ast[i]->sdata[idx], 
			       ast[i]->sdata[idx]->file->name, 
			       status );
	}
	if( !memiter ) 
	  smf_close_related( &ast[i], status );
	
	for( j=0; j<nmodels; j++ ) { 
	  if( !memiter ) 
	    smf_open_related_model( modelgroups[j], i, "READ", &model[j][i], 
				    status );
	  
	  for( idx=0; idx<model[j][i]->ndat; idx++ ) {
	    smf_model_NDFexport( model[j][i]->sdata[idx], 
				 model[j][i]->sdata[idx]->file->name, 
				 status);
	    
	  }
	  if( !memiter ) 
	    smf_close_related( &model[j][i], status );
	}
      }
    }
  }

  /* Cleanup. If memiter set all files get closed here */

  if( modeltyps ) smf_free( modeltyps, status );
  modeltyps = NULL;

  /* fixed model smfGroups */
  if( resgroup ) smf_close_smfGroup( &resgroup, status );
  if( astgroup ) smf_close_smfGroup( &astgroup, status );  
  if( lutgroup ) smf_close_smfGroup( &lutgroup, status );  

  /* dynamic model smfArrays */
  for( i=0; i<nchunks; i++ ) {
    if( res[i] ) smf_close_related( &res[i], status );
    if( ast[i] ) smf_close_related( &ast[i], status );
    if( lut[i] ) smf_close_related( &lut[i], status );
  }

  /* dynamic model smfGroups */
  if( modelgroups ) {
    for( i=0; i<nmodels; i++ ) {
      if( modelgroups[i] ) smf_close_smfGroup( &modelgroups[i], status );
    }

    /* Free array of smfGroup pointers at this time chunk */
    smf_free( modelgroups, status );
    modelgroups = NULL;
  }
    
  /* dynamic model smfArrays */
  if( model ) {
    for( i=0; i<nmodels; i++ ) {
      if( model[i] ) {
	for( j=0; j<nchunks; j++ ) {
	  /* Close each model component smfArray at each time chunk */
	  if( model[i][j] ) 
	    smf_close_related( &(model[i][j]), status );
	}
	
	/* Free array of smfArray pointers for this model */
	smf_free( model[i], status );
	model[i] = NULL;
      }
    }
    smf_free( model, status );
    model = NULL;
  }
  
  /* finally close igroup */
  if( igroup ) {
    smf_close_smfGroup( &igroup, status );
  }
  
}
