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
*     smf_iteratemap( Grp *igrp, AstKeyMap *keymap, 
*                    AstFrameSet *outfset, int moving, 
*	             int *lbnd_out, int *ubnd_out, size_t maxmem, 
*                    double *map, unsigned int *hitsmap, double *mapvar, 
*                    double *weights, int *status );

*  Arguments:
*     igrp = Grp* (Given)
*        Group of input data files
*     keymap = AstKeyMap* (Given)
*        keymap containing parameters to control map-maker
*     outfset = AstFrameSet* (Given)
*        Frameset containing the sky->output map mapping if calculating
*        pointing LUT on-the-fly
*     moving = int (Given)
*        Is coordinate system tracking moving object? (if outfset specified)
*     lbnd_out = double* (Given)
*        2-element array pixel coord. for the lower bounds of the output map
*        (if outfset specified) 
*     maxmem = size_t (Given)
*        Maximum memory that me be used by smf_iteratemap (bytes)
*     map = double* (Returned)
*        The output map array 
*     hitsmap = unsigned int* (Returned)
*        Number of samples that land in a pixel (ignore if NULL pointer)
*     mapvar = double* (Returned)
*        Variance of each pixel in map
*     weights = double* (Returned)
*        Relative weighting for each pixel in map
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
*     2007-08-17 (EC):
*        Added nofile flag to smf_model_create to avoid creating .dimm files
*        in memiter=1 case.
*     2007-11-15 (EC):
*        -Use smf_concat_smfGroup for memiter=1 case.
*        -Modified interface to hand projection from caller to concat_smfGroup
*        -Check for file name existence when calling smf_model_NDFexport
*     2007-11-15 (EC):
*        -Switch to bolo-ordering the data. Compiles but still buggy.
*     2007-12-14 (EC):
*        -set file access to UPDATE for model components (was READ)
*        -modified smf_calc_mapcoord interface
*     2007-12-18 (AGG):
*        Update to use new smf_free behaviour
*     2008-01-22 (EC):
*        Added hitsmap to interface
*     2008-01-25 (EC):
*        Handle non-flatfielded input data. Pointing LUT is now calculated
*        in smf_model_create rather than requiring calls to smf_calc_mapcoord.
*     2008-03-04 (EC):
*        -Modified model calculation to use smfDIMMData in interface
*        -Added QUAlity component
*     2008-04-03 (EC):
*        - Use QUALITY in map-maker
*        - Added data cleaning options to CONFIG file
*     2008-04-14 (EC):
*        - Fixed memory-deallocation (res/ast/qua...)
*        - Added QUALITY/VARIANCE to NDFexport
*     2008-04-16 (EC):
*        - Added outer loop to handle multiple cont. chunks for memiter=1 case
*     2008-04-17 (EC):
*        - Added maxlen to config file, modified smf_grp_related
*        - Use variance stored in NOI to estimate map
*        - Don't include VARIANCE of input files when concatenating
*        - Store VARIANCE (from NOI) in residual 
*     2008-04-18 (EC):
*        Calculate and display chisquared for each chunk
*     2008-04-23 (EC):
*        - Added sample variance estimate for varmap
*        - Propagate header information to exported model components
*        - Added CHITOL config parameter to control stopping
*     2008-04-24 (EC):
*        - Improved status checking
*        - extra checks for valid pointers before exporting model components
*     2008-04-28 (EC):
*        - Added memory usage check
*     2008-04-29 (EC)
*        Check for VAL__BADD in map to avoid propagating to residual
*     2008-04-30 (EC)
*        - Undo EXTinction correction after calculating AST
*     2008-05-01 (EC)
*        - More intelligent auto-sizing of concantenated chunks
*     2008-05-02 (EC)
*        - Use different levels of verbosity in messages
*     2008-06-12 (EC)
*        - renamed smf_model_NDFexport smf_NDFexport
*        - added edge and notch frequency-domain filters
*     2008-07-03 (EC)
*        - Added padstart/padend to config files
*     {enter_further_changes_here}

*  Notes:

*  Copyright:
*     Copyright (C) 2006 Particle Physics and Astronomy Research Council.
*     Copyright (C) 2006-2008 University of British Columbia
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
#include "libsmf/smf_err.h"
#include "jcmt/state.h"

/* Other includes */
#include "sys/time.h"


#define FUNC_NAME "smf_iteratemap"

void smf_iteratemap( Grp *igrp, AstKeyMap *keymap, 
		     AstFrameSet *outfset, int moving, 
	             int *lbnd_out, int *ubnd_out, size_t maxmem, 
                     double *map, unsigned int *hitsmap, double *mapvar, 
		     double *weights, int *status ) {

  /* Local Variables */
  size_t aiter;                 /* Actual iterations of sigma clipper */
  smfArray **ast=NULL;          /* Astronomical signal */
  double *ast_data=NULL;        /* Pointer to DATA component of ast */
  smfGroup *astgroup=NULL;      /* smfGroup of ast model files */
  const char *asttemp=NULL;     /* Pointer to static strings created by ast */
  double badfrac;               /* Bad bolo fraction for flagging */
  int baseorder;                /* Order of poly for baseline fitting */
  double *chisquared=NULL;      /* chisquared for each chunk each iter */
  double chitol=0;              /* chisquared change tolerance for stopping */
  size_t contchunk;             /* Which chunk in outer loop */
  int converged=0;              /* Has stopping criteria been met? */
  smfDIMMData dat;              /* Struct passed around to model components */
  smfData *data=NULL;           /* Temporary smfData pointer */
  dim_t dcbox=0;                /* Box size for fixing DC steps */
  double dcthresh;              /* Threshold for fixing DC steps */
  int dimmflags;                /* Control flags for DIMM model components */
  int dofft=0;                  /* Set if freq. domain filtering the data */
  dim_t dsize;                  /* Size of data arrays in containers */
  double dtemp;                 /* temporary double */
  int exportNDF=0;              /* If set export DIMM files to NDF at end */
  smfFilter *filt=NULL;         /* Pointer to filter struct */
  double f_edgelow=0;           /* Freq. cutoff for low-pass edge filter */
  double f_edgehigh=0;          /* Freq. cutoff for high-pass edge filter */
  double f_notchlow[SMF__MXNOTCH]; /* Array low-freq. edges of notch filters */
  double f_notchhigh[SMF__MXNOTCH];/* Array high-freq. edges of notch filters */
  int f_nnotch=0;               /* Number of notch filters in array */
  int f_nnotch2=0;              /* Number of notch filters in array */
  int haveext=0;                /* Set if EXT is one of the models */
  int havenoi=0;                /* Set if NOI is one of the models */
  smfHead *hdr=NULL;            /* Pointer to smfHead */
  dim_t i;                      /* Loop counter */
  dim_t idx=0;                  /* index within subgroup */
  smfGroup *igroup=NULL;        /* smfGroup corresponding to igrp */
  int iter;                     /* Iteration number */
  int isize;                    /* Number of files in input group */
  dim_t j;                      /* Loop counter */
  dim_t k;                      /* Loop counter */
  double *lastchisquared=NULL;  /* chisquared for last iter */
  smfArray **lut=NULL;          /* Pointing LUT for each file */
  int *lut_data=NULL;           /* Pointer to DATA component of lut */
  smfGroup *lutgroup=NULL;      /* smfGroup of lut model files */
  unsigned char mask;           /* Bitmask for QUALITY flags */
  dim_t maxconcat;              /* Longest continuous chunk length in samples*/
  int maxiter=0;                /* Maximum number of iterations */
  dim_t maxlen=0;               /* Max chunk length in samples */
  int memiter=0;                /* If set iterate completely in memory */
  size_t memneeded;             /* Memory required for map-maker */
  smfArray ***model=NULL;       /* Array of pointers smfArrays for ea. model */
  smfGroup **modelgroups=NULL;  /* Array of group ptrs/ each model component */
  smf_modeltype *modeltyps=NULL;/* Array of model types */
  char modelname[4];            /* Name of current model component */
  smf_calcmodelptr modelptr=NULL; /* Pointer to current model calc function */
  dim_t msize;                  /* Number of elements in map */
  dim_t nchunks=0;              /* Number of chunks within iteration loop */
  size_t ncontchunks=0;         /* Number continuous chunks outside iter loop*/
  dim_t nmodels=0;              /* Number of model components / iteration */
  int numiter;                  /* Total number iterations */
  dim_t padEnd=0;               /* How many samples of padding at the end */
  dim_t padStart=0;             /* How many samples of padding at the start */
  size_t pass;                  /* Two pass parsing of MODELORDER */
  smfArray **qua=NULL;          /* Quality flags for each file */
  unsigned char *qua_data=NULL; /* Pointer to DATA component of qua */
  smfGroup *quagroup=NULL;      /* smfGroup of quality model files */
  int quit=0;                   /* flag indicates when to quit */
  int rebinflags;               /* Flags to control rebinning */
  smfArray **res=NULL;          /* Residual signal */
  double *res_data=NULL;        /* Pointer to DATA component of res */
  smfGroup *resgroup=NULL;      /* smfGroup of model residual files */
  size_t spikeiter=0;           /* Number of iter for spike detection */
  double spikethresh;           /* Threshold for spike detection */
  double steptime;              /* Length of a sample in seconds */
  int temp;                     /* temporary signed integer */
  unsigned int *thishits=NULL;  /* Pointer to this hits map */
  double *thismap=NULL;         /* Pointer to this map */
  smf_modeltype thismodel;      /* Type of current model */
  double *thisweight=NULL;      /* Pointer to this weights map */
  double *thisvar=NULL;         /* Pointer to this variance map */
  size_t try;                   /* Try to concatenate this many samples */
  int untilconverge=0;          /* Set if iterating to convergence */
  double *var_data=NULL;        /* Pointer to DATA component of NOI */
  int varmapmethod=0;           /* Method for calculating varmap */
  dim_t whichext=0;             /* Model index of EXT if present */
  dim_t whichnoi=0;             /* Model index of NOI if present */

  /* Main routine */
  if (*status != SAI__OK) return;

  /* Calculate number of elements in the map */
  if( (ubnd_out[0]-lbnd_out[0] < 0) || (ubnd_out[1]-lbnd_out[1] < 0) ) {
    *status = SAI__ERROR;
    msgSeti("L0",lbnd_out[0]);
    msgSeti("L1",lbnd_out[1]);
    msgSeti("U0",ubnd_out[0]);
    msgSeti("U1",ubnd_out[1]);
    errRep(FUNC_NAME, "Invalid mapbounds: LBND=[^L0,^L1] UBND=[^U0,^U1]", 
           status);      
  }

  msize = (dim_t) (ubnd_out[0]-lbnd_out[0]+1) * 
    (dim_t) (ubnd_out[1]-lbnd_out[1]+1);

  /* Get size of the input group */
  isize = grpGrpsz( igrp, status );

  /* Parse the CONFIG parameters stored in the keymap */

  if( *status == SAI__OK ) {
    /* Number of iterations */
    if( !astMapGet0I( keymap, "NUMITER", &numiter ) ) {
      numiter = -20;
    }

    if( numiter == 0 ) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "NUMITER cannot be 0", status);      
    } else {
      if( numiter < 0 ) {
	/* If negative, iterate to convergence or abs(numiter), whichever comes
           first */
	maxiter = abs(numiter);
	untilconverge = 1;
      } else {
	/* Otherwise iterate a fixed number of times */
	maxiter = numiter;
	untilconverge = 0;
      }
    }

    /* Chisquared change tolerance for stopping */
    if( !astMapGet0D( keymap, "CHITOL", &chitol ) ) {
      chitol = 0.0001;
    }

    if( chitol <= 0 ) {
      *status = SAI__ERROR;
      msgSetd("CHITOL",chitol);
      errRep(FUNC_NAME, 
	     "SMF_ITERATEMAP: CHITOL is ^CHITOL, must be > 0", status);      
    }

    /* Do iterations completely in memory - minimize disk I/O */
    if( !astMapGet0I( keymap, "MEMITER", &memiter ) ) {
      memiter = 0;
    } 

    if( memiter ) {
      msgOutif(MSG__VERB, " ", 
	       "SMF_ITERATEMAP: MEMITER set; perform iterations in memory",
	     status );
    } else {
      msgOutif(MSG__VERB, " ", 
	     "SMF_ITERATEMAP: MEMITER not set; perform iterations on disk",
	     status );
    }

    /* Method to use for calculating the variance map */
    if( !astMapGet0I( keymap, "VARMAPMETHOD", &varmapmethod ) ) {
      varmapmethod = 0;
    }

    if( varmapmethod ) {
      msgOutif(MSG__VERB, " ", 
	     "SMF_ITERATEMAP: Will use sample variance to estimate variance map",
	     status );
    } else {
      msgOutif(MSG__VERB, " ", 
	     "SMF_ITERATEMAP: Will use error propagation to estimate variance map",
	     status );
    }

    /* Will we export components to NDF at the end? */
    if( !astMapGet0I( keymap, "EXPORTNDF", &exportNDF ) ) {
      exportNDF = 0;
    } 

    /* Data-cleaning parameters (should match SC2CLEAN) */
    
    if( !astMapGet0D( keymap, "BADFRAC", &badfrac ) ) {
      badfrac = 0;
    }

    if( !astMapGet0D( keymap, "DCTHRESH", &dcthresh ) ) {
      dcthresh = 0;
    }

    if( astMapGet0I( keymap, "DCBOX", &temp ) ) {
      if( temp < 0 ) {
	*status = SAI__ERROR;
	errRep(FUNC_NAME, "dcbox cannot be < 0.", status );
      } else {
	dcbox = (dim_t) temp;
      }
    } else {
      dcbox = 0;
    }

    if( !astMapGet0I( keymap, "ORDER", &baseorder ) ) {
      baseorder = -1;
    }

    if( !astMapGet0D( keymap, "SPIKETHRESH", &spikethresh ) ) {
      spikethresh = 0;
    }

    if( astMapGet0I( keymap, "SPIKEITER", &temp ) ) {
      if( temp < 0 ) {
	*status = SAI__ERROR;
	errRep(FUNC_NAME, "spikeiter cannot be < 0.", status );
      } else {
	spikeiter = (size_t) temp;
      }
    } else {
      spikeiter = 0;
    }

    if( astMapGet0D( keymap, "FILT_EDGELOW", &f_edgelow ) ) {
      dofft = 1;
    } else {
      f_edgelow = 0;
    }

    if( astMapGet0D( keymap, "FILT_EDGEHIGH", &f_edgehigh ) ) {
      dofft=1;
    } else {
      f_edgehigh = 0;
    }

    if( !astMapGet1D( keymap, "FILT_NOTCHLOW", SMF__MXNOTCH, &f_nnotch, 
                     f_notchlow ) ) {
      f_nnotch=0;
    }

    if( !astMapGet1D( keymap, "FILT_NOTCHHIGH", SMF__MXNOTCH, &f_nnotch2, 
                     f_notchhigh ) ) {
      f_nnotch2=0;
    }

    if( f_nnotch ) {
      /* Number of upper and lower edges must match */
      if( f_nnotch != f_nnotch2 ) {
        *status = SAI__ERROR;
        errRep(FUNC_NAME, 
               "Elements in FILT_NOTCHHIGH != number in FILT_NOTCHLOW", 
               status);      
      } else {
        dofft = 1;
      }
    }

    /* Maximum length of a continuous chunk */
    if( astMapGet0D( keymap, "MAXLEN", &dtemp ) ) {

      if( dtemp < 0.0 ) {
	/* Trap negative MAXLEN */
	*status = SAI__ERROR;
	errRep(FUNC_NAME, "Negative value for MAXLEN supplied.", status);
      } else if( dtemp == 0 ) {
	/* 0 is OK... gets ignored later */
	maxlen = 0;
      } else {
	/* Obtain sample length from header of first file in igrp */
	smf_open_file( igrp, 1, "READ", SMF__NOCREATE_DATA, &data, status );
	if( (*status == SAI__OK) && (data->hdr) ) {
	  smf_fits_getD(data->hdr, "STEPTIME", &steptime, status);

	  if( steptime > 0 ) {
	    maxlen = (dim_t) (dtemp / steptime);
	  } else {
	    /* Trap invalud sample length in header */
	    *status = SAI__ERROR;
	    errRep(FUNC_NAME, "Invalid STEPTIME in FITS header.", status);
	  }
	}
	smf_close_file( &data, status );
      }

    } else {
      maxlen = 0;
    }

    /* Padding */

    if( astMapGet0I( keymap, "PADSTART", &temp ) ) {
      if( temp < 0 ) {
	*status = SAI__ERROR;
	errRep(FUNC_NAME, "PADSTART cannot be < 0.", status );
      } else {
	padStart = (dim_t) temp;
      }
    } else {
      padStart = 0;
    }

    if( astMapGet0I( keymap, "PADEND", &temp ) ) {
      if( temp < 0 ) {
	*status = SAI__ERROR;
	errRep(FUNC_NAME, "PADEND cannot be < 0.", status );
      } else {
	padEnd = (dim_t) temp;
      }
    } else {
      padEnd = 0;
    }

    /* Type and order of models to fit from MODELORDER keyword */
    havenoi = 0;
    haveext = 0;
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

		/* set havenoi/whichnoi */
		if( thismodel == SMF__NOI ) {
		  havenoi = 1;
		  whichnoi = nmodels; 
		}

		/* set haveext/whichext */
		if( thismodel == SMF__EXT ) {
		  haveext = 1;
		  whichext = nmodels; 
		}
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

  /* If !havenoi can't measure convergence, so turn off untilconverge */
  if( !havenoi ) {
    untilconverge = 0;
  }

  if( untilconverge ) {
    msgSeti("MAX",maxiter);
    msgOut(" ", 
	   "SMF_ITERATEMAP: Iterate to convergence (max ^MAX)",
	   status );
    msgSetd("CHITOL",chitol);
    msgOut(" ", 
	   "SMF_ITERATEMAP: Stopping criteria is a change in chi^2 < ^CHITOL",
	   status);
  } else {
    msgSeti("MAX",maxiter);
    msgOut(" ", "SMF_ITERATEMAP: ^MAX Iterations", status );
  }

  msgSeti("NUMCOMP",nmodels);
  msgOutif(MSG__VERB," ", 
	   "SMF_ITERATEMAP: ^NUMCOMP model components in solution: ", 
	 status);
  for( i=0; i<nmodels; i++ ) {
    msgSetc( "MNAME", smf_model_getname(modeltyps[i], status) );
    msgOutif(MSG__VERB,
	     " ", "  ^MNAME", status ); 
  }

  /* Create an ordered smfGrp which keeps track of files corresponding to
     different subarrays (observed simultaneously), as well as time-ordering
     the files. Now added "chunk" to smfGroup as well -- this is used to
     concatenate _only_ continuous pieces of data. We subtract padStart
     and padEnd from maxlen since these also add to the file length. */

  smf_grp_related( igrp, isize, 1, maxlen-padStart-padEnd, &maxconcat, 
                   &igroup, status );

  /* Once we've run smf_grp_related we know how many subarrays there
     are.  We also know the maximum length of a concatenated piece of
     data, and which model components were requested. Use this
     information to check that enough memory is available. */

  if( *status == SAI__OK ) {

    smf_checkmem_dimm( maxconcat, INST__SCUBA2, igroup->nrelated, modeltyps,
		       nmodels, maxmem, &memneeded, status );

    if( *status == SMF__NOMEM ) {
      /* If we need too much memory, generate a warning message and then try
	 to re-group the files using smaller chunks */

      errAnnul( status );
      msgOut( " ", "SMF_ITERATEMAP: *** WARNING ***", status );
      msgSeti( "LEN", maxconcat );
      msgSeti( "AVAIL", maxmem/SMF__MB );
      msgSeti( "NEED", memneeded/SMF__MB );
      msgOut( " ", "  ^LEN continuous samples requires ^NEED Mb > ^AVAIL Mb", 
	      status );

      /* Try is meant to be the largest chunks of ~equal length that fit in
	 memory */
      try = maxconcat / ((size_t) ((double) memneeded/maxmem)+1)+1;

      msgSeti( "TRY", try );
      msgOut( " ", "  Will try to re-group data in chunks < ^TRY samples long",
	      status);
      msgOut( " ", "SMF_ITERATEMAP: ***************", status );
      
      /* Close igroup if needed before re-running smf_grp_related */
      
      if( igroup ) {
	smf_close_smfGroup( &igroup, status );
      }

      smf_grp_related( igrp, isize, 1, try, &maxconcat, &igroup, status );
    }
  }

  if( *status == SAI__OK ) {

    if( memiter ) {
      /* only one concatenated chunk within the iteration loop */
      nchunks = 1;

      /* however, there are multiple large continuous pieces outside the
         iteration loop */
      ncontchunks = igroup->chunk[igroup->ngroups-1]+1;
    } else {
      /* Otherwise number of chunks is just number of objects in the
         input group */
      nchunks = igroup->ngroups;

      /* No looping over larger continuous chunks outside the iteration loop */
      ncontchunks = 1;
    }

    if( memiter ) {
      msgSeti( "NCONTCHUNKS", ncontchunks );
      msgOutif(MSG__VERB," ", 
	     "SMF_ITERATEMAP: ^NCONTCHUNKS large continuous chunks outside iteration loop.", 
	     status);
    } else {
      msgSeti( "NCHUNKS", nchunks );
      msgOutif(MSG__VERB," ", 
	     "SMF_ITERATEMAP: ^NCHUNKS file chunks inside iteration loop.", 
	     status);
    }
  }


  /* There are two loops over files apart from the iteration. In the
     memiter=1 case the idea is to concatenate all continuous data
     into several large chunks, and iterate each one of those to
     completion without any file i/o. These are called
     "contchunk". Inside the iteration loop, if memiter=0, loop over
     each input file; those are called "chunk". Lots of file i/o,
     poorer map solution, but runs with less memory. */

  for( contchunk=0; contchunk<ncontchunks; contchunk++ ) {

    if( memiter ) {
      msgSeti("CHUNK", contchunk+1);
      msgSeti("NUMCHUNK", ncontchunks);
      msgOut( " ", 
	      "SMF_ITERATEMAP: Continuous chunk ^CHUNK / ^NUMCHUNK =========",
	      status);
    }

    if( *status == SAI__OK ) {

      /* Setup the map estimate from the current contchunk. */
      if( contchunk == 0 ) {
	/* For the first chunk, calculate the map in-place */
	thismap = map;
	thishits = hitsmap;
	thisvar = mapvar;
	thisweight = weights;
      } else if( contchunk == 1 ) {
	/* Subsequent chunks are done in new map arrays and then added to
           the first */
	thismap = smf_malloc( msize, sizeof(*thismap), 0, status ); 
	thishits = smf_malloc( msize, sizeof(*thishits), 0, status ); 
	thisvar = smf_malloc( msize, sizeof(*thisvar), 0, status ); 
	thisweight = smf_malloc( msize, sizeof(*thisweight), 0, status ); 
      }

      if( memiter ) {

	/* If memiter=1 concat everything in this contchunk into a
	   single smfArray. Note that the pointing LUT gets generated in
	   smf_concat_smfGroup below. */

	msgSeti("C",contchunk+1);
	msgOutif(MSG__VERB," ", 
	       "SMF_ITERATEMAP: Concatenating files in continuous chunk ^C", 
	       status);

	/* Allocate length 1 array of smfArrays. */   
	res = smf_malloc( nchunks, sizeof(*res), 1, status );

	/* Concatenate (no variance since we calculate it ourselves -- NOI) */
	smf_concat_smfGroup( igroup, NULL, contchunk, 0, outfset, moving, lbnd_out, 
			     ubnd_out, padStart, padEnd, 
                             SMF__NOCREATE_VARIANCE, &res[0], status );
      } 
    }
    
    /* Allocate space for the chisquared array */
    if( havenoi ) {
      chisquared = smf_malloc( nchunks, sizeof(*chisquared), 1, status );
      lastchisquared = smf_malloc( nchunks, sizeof(*chisquared), 1, status );
    }

    /* Create containers for time-series model components */
    msgOutif(MSG__VERB," ", "SMF_ITERATEMAP: Create model containers", status);

    /* Components that always get made */
    if( igroup && (*status == SAI__OK) ) {

      /* there is one smfArray for LUT, AST and QUA at each chunk */
      lut = smf_malloc( nchunks, sizeof(*lut), 1, status );
      ast = smf_malloc( nchunks, sizeof(*ast), 1, status );
      qua = smf_malloc( nchunks, sizeof(*qua), 1, status );

      if( memiter ) {
	/* If iterating in memory then RES has already been created from
	   the concatenation of the input data. Create the other
	   required models using res[0] as a template. Assert
	   bolo-ordered data although the work has already been done at
	   the concatenation stage. */

	smf_model_create( NULL, res, nchunks, SMF__LUT, 0, 
			  NULL, 0, NULL, NULL,
			  NULL, memiter, 
			  memiter, lut, status ); 

	smf_model_create( NULL, res, nchunks, SMF__AST, 0, 
			  NULL, 0, NULL, NULL,
			  NULL, memiter, 
			  memiter, ast, status );

	smf_model_create( NULL, res, nchunks, SMF__QUA, 0, 
			  NULL, 0, NULL, NULL,
			  NULL, memiter, 
			  memiter, qua, status );

	/* Since a copy of the LUT is still open in res[0] free it up here */
	for( i=0; i<res[0]->ndat; i++ ) {
	  if( res[0]->sdata[i] ) {
	    smf_close_mapcoord( res[0]->sdata[i], status );
	  }
	}
      
      } else {
	/* If iterating using disk i/o need to create res and other model 
	   components using igroup as template. In this case the pointing
	   LUT probably doesn't exist, so give projection information to
	   smf_model_create. Also assert bolo-ordered template 
	   (in this case res). */

	res = smf_malloc( nchunks, sizeof(*res), 1, status );

	smf_model_create( igroup, NULL, 0, SMF__RES, 0, 
			  NULL, 0, NULL, NULL,
			  &resgroup, memiter, 
			  memiter, res, status );

	smf_model_create( igroup, NULL, 0, SMF__LUT, 0, 
			  outfset, moving, lbnd_out, ubnd_out,
			  &lutgroup, memiter, 
			  memiter, lut, status ); 

	smf_model_create( igroup, NULL, 0, SMF__AST, 0, 
			  NULL, 0, NULL, NULL,
			  &astgroup, memiter, 
			  memiter, ast, status );

	smf_model_create( igroup, NULL, 0, SMF__QUA, 0, 
			  NULL, 0, NULL, NULL,
			  &quagroup, memiter, 
			  memiter, qua, status );
      }
    }

    /* Dynamic components */
    if( igroup && (nmodels > 0) && (*status == SAI__OK) ) {

      /* nmodel array of pointers to nchunk smfArray pointers */
      model = smf_malloc( nmodels, sizeof(*model), 1, status );

      if( memiter != 1 ) {
	/* Array of smfgroups (one for each dynamic model component) */
	modelgroups = smf_malloc( nmodels, sizeof(*modelgroups), 1, status );  
      }

      for( i=0; i<nmodels; i++ ) {
	model[i] = smf_malloc( nchunks, sizeof(**model), 1, status );
      
	if( memiter ) {
	  smf_model_create( NULL, res, nchunks, modeltyps[i], 0, 
			    NULL, 0, NULL, NULL,
			    NULL, memiter, memiter, model[i], status ); 

	} else {
	  smf_model_create( igroup, NULL, 0, modeltyps[i], 0, 
			    NULL, 0, NULL, NULL, &modelgroups[i], 
			    memiter, memiter, model[i], status );
	}
      }
    }
 
    /* Start the main iteration loop */
    if( *status == SAI__OK ) {

      /* Stuff pointers into smfDIMMData to pass around to model component
	 solvers */

      memset( &dat, 0, sizeof(dat) ); /* Initialize structure */
      dat.res = res;
      dat.qua = qua;
      dat.lut = lut;
      dat.map = thismap;
      dat.mapvar = thisvar;
      dat.chisquared = chisquared;
      if( havenoi ) {
	dat.noi = model[whichnoi];
      } else {
	dat.noi = NULL;
      }
      if( haveext ) {
	dat.ext = model[whichext];
      } else {
	dat.ext = NULL;
      }

      quit = 0;
      iter = 0;
      while( !quit ) {
	msgSeti("ITER", iter+1);
	msgSeti("MAXITER", maxiter);
	msgOut(" ", 
	       "SMF_ITERATEMAP: Iteration ^ITER / ^MAXITER ---------------",
	       status);
      
	/* Assume we've converged until we find a chunk that hasn't */
	if( iter > 0 ) {
	  converged = 1;
	} else {
	  converged = 0;
	}

	for( i=0; i<nchunks; i++ ) {
	  if( !memiter ) {
	    msgSeti("CHUNK", i+1);
	    msgSeti("NUMCHUNK", nchunks);
	    msgOut(" ", "SMF_ITERATEMAP: File chunk ^CHUNK / ^NUMCHUNK", 
		   status);
	  }

	  /* Open model files here if looping on-disk. Otherwise everything
	     is already open from the smf_model_create calls */

	  if( !memiter ) {
	  
	    /* If memiter not set open this chunk here */
	    smf_open_related_model( resgroup, i, "UPDATE", &res[i], status );
	    smf_open_related_model( lutgroup, i, "UPDATE", &lut[i], status );
	    smf_open_related_model( astgroup, i, "UPDATE", &ast[i], status );
	    smf_open_related_model( quagroup, i, "UPDATE", &qua[i], status );
	  
	    for( j=0; j<nmodels; j++ ) {
	      smf_open_related_model( modelgroups[j], i, "UPDATE", 
				      &model[j][i], status );
	    }
	  } 

	  /* If first iteration pre-condition the data */
	  if( iter == 0 ) {
	    msgOut(" ", "SMF_ITERATEMAP: Pre-conditioning chunk", status);
	    for( idx=0; idx<res[i]->ndat; idx++ ) {
	      /* Synchronize quality flags */

	      data = res[i]->sdata[idx];
	      qua_data = (unsigned char *) qua[i]->sdata[idx]->pntr[0];

	      msgOutif(MSG__VERB," ", "  update quality", status);
	      smf_update_quality( data, qua_data, 1, NULL, badfrac, status );

	      if( baseorder >= 0 ) {
		msgOutif(MSG__VERB," ", "  fit polynomial baselines", status);
		smf_scanfit( data, qua_data, baseorder, status );

		msgOutif(MSG__VERB," ", "  remove polynomial baselines", 
			 status);
		smf_subtract_poly( data, qua_data, 0, status );
	      }

	      if( dcthresh && dcbox ) {
		msgOutif(MSG__VERB," ", "  correct steps", status);
		smf_correct_steps( data, qua_data, 20., 1000, status );
	      }

	      if( spikethresh ) {
		msgOutif(MSG__VERB," ", "  flag spikes...", status);
		smf_flag_spikes( data, qua_data, ~SMF__Q_JUMP,
				 spikethresh, spikeiter, 100, 
				 &aiter, NULL, status );
		msgSeti("AITER",aiter);
		msgOutif(MSG__VERB," ", "  ...finished in ^AITER iterations",
		       status); 
	      }

              /* filter the data */
              if( dofft ) {

                msgOutif( MSG__VERB," ", "  frequency domain filter", status );
              
                filt = smf_create_smfFilter( data, status );
                
                if( f_edgelow ) {
                  smf_filter_edge( filt, f_edgelow, 1, status );
                }

                if( f_edgehigh ) {
                  smf_filter_edge( filt, f_edgehigh, 0, status );
                }

                if( f_nnotch ) {
                  smf_filter_notch( filt, f_notchlow, f_notchhigh, f_nnotch,
                                    status );
                }

                smf_filter_execute( data, filt, status );

                filt = smf_free_smfFilter( filt, status );
              }
	    }
	  }

	  msgOut(" ", 
		 "SMF_ITERATEMAP: Calculate time-stream model components", 
		 status);

	  /* Call the model calculations in the desired order. */
	  if( *status == SAI__OK ) {
	    for( j=0; j<nmodels; j++ ) {
	    
	      /* Set up control flags for the model calculation */
	      dimmflags = 0;
	      if( iter==0 ) dimmflags |= SMF__DIMM_FIRSTITER;
	      if( j==0 ) dimmflags |= SMF__DIMM_FIRSTCOMP;
	    
	      msgSetc("MNAME", smf_model_getname(modeltyps[j],status));
	      msgOutif(MSG__VERB," ", "  ^MNAME", status);
	      modelptr = smf_model_getptr( modeltyps[j], status );
	    
	      if( *status == SAI__OK ) {
		(*modelptr)( &dat, i, keymap, model[j], dimmflags, status );
	      }

	      /* If bad status set exit condition */
	      if( *status != SAI__OK ) {
		j = nmodels;
	      }
	    }
	  }

	  /* Once all the other model components have been calculated put the
	     previous iteration of AST back into the residual, zero ast,
	     and rebin the noise+astro signal into the map */

	  msgOut(" ", "SMF_ITERATEMAP: Rebin residual to estimate MAP", 
		 status);

	  if( *status == SAI__OK ) {

	    /* Loop over subgroup index (subarray) */
	    for( idx=0; idx<res[i]->ndat; idx++ ) {

	      /* Add last iter. of astronomical signal back in to residual */
	      ast_data = (double *)(ast[i]->sdata[idx]->pntr)[0];
	      res_data = (double *)(res[i]->sdata[idx]->pntr)[0];
	      lut_data = (int *)(lut[i]->sdata[idx]->pntr)[0];
	      qua_data = (unsigned char *)(qua[i]->sdata[idx]->pntr)[0];

	      if( havenoi ) {
		var_data = (double *)(dat.noi[i]->sdata[idx]->pntr)[0];
	      } else {
		var_data = NULL;
	      }

	      dsize = (ast[i]->sdata[idx]->dims)[0] *
		(ast[i]->sdata[idx]->dims)[1] * (ast[i]->sdata[idx]->dims)[2];

	      /* Ignore data with these QUALITY flags */
	      mask = ~SMF__Q_JUMP;

	      for( k=0; k<dsize; k++ ) {	  
		if( !(qua_data[k]&mask) && (ast_data[k] != VAL__BADD) ) {
		  res_data[k] += ast_data[k];
		}

		/* Not really necessary.
		   Set ast_data back to 0 since we've moved all of the signal
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
	      smf_simplerebinmap( res_data, var_data, lut_data, qua_data, mask,
				  dsize, varmapmethod, rebinflags, thismap, 
				  thisweight, thishits, thisvar, msize, 
				  status );
	    }
	  }

	  /* Close files here if memiter not set */

	  if( !memiter ) {
	    smf_close_related( &res[i], status );
	    smf_close_related( &ast[i], status );    
	    smf_close_related( &lut[i], status );    
	    smf_close_related( &qua[i], status );    

	    for( j=0; j<nmodels; j++ ) {
	      smf_close_related( &model[j][i], status );
	    }
	  }

	  /* Set exit condition if bad status was set */
	  if( *status != SAI__OK ) i=isize+1;

	  /* If NOI was present, we now have an estimate of chisquared */
	  if( chisquared ) {
	    msgSeti("CHUNK",i+1);
	    msgSetd("CHISQ",chisquared[i]);
	    msgOut( " ", 
		    "SMF_ITERATEMAP: *** CHISQUARED = ^CHISQ for chunk ^CHUNK",
		    status);

	    if( iter > 0 ) {
	      msgSetd("DIFF", chisquared[i]-lastchisquared[i]);
	      msgOut( " ",
		      "SMF_ITERATEMAP: *** change: ^DIFF", status );
	    }

	    /* Check for the stopping criteria */
	    if( untilconverge ) {
	      if( iter > 0 ) {
		if( (lastchisquared[i]-chisquared[i]) > chitol ) {
		  /* Found a chunk that isn't converged yet */
		  converged=0;
		}
	      } 	    
	    }

	    /* Update lastchisquared */
	    lastchisquared[i] = chisquared[i];

	  }
	}
      
	if( *status == SAI__OK ) {
	  msgOut(" ", "SMF_ITERATEMAP: Calculate ast", status);

	  for( i=0; i<nchunks; i++ ) {

	    /* Open files if memiter not set - otherwise they are still open
	       from earlier call */
	    if( !memiter ) {
	      smf_open_related_model( astgroup, i, "UPDATE", &ast[i], status );
	      smf_open_related_model( resgroup, i, "UPDATE", &res[i], status );
	      smf_open_related_model( lutgroup, i, "UPDATE", &lut[i], status );
	      smf_open_related_model( quagroup, i, "UPDATE", &qua[i], status );
	      
	      if( haveext ) {
		smf_open_related_model( modelgroups[whichext], i, "UPDATE", 
					&model[whichext][i], status );
	      }
	    }

	    /* Calculate the AST model component. It is a special model
	       because it assumes that the map contains the best current
	       estimate of the astronomical sky. It gets called in this
	       separate loop since the map estimate gets updated by
	       each chunk in the main model component loop */

	    smf_calcmodel_ast( &dat, i, keymap, ast, 0, status );

	    /* If EXTinction was applied during this iteration, AST and RES
               are currently in units of Jy. Un-do the EXTinction correction
               here so that RES is in the right units again before starting
               the next iteration */

	    if( haveext ) {
	      smf_calcmodel_ext( &dat, i, keymap, model[whichext], 
				 SMF__DIMM_INVERT, status );
	    }

	    /* Close files if memiter not set */
	    if( !memiter ) {
	      smf_close_related( &ast[i], status );    
	      smf_close_related( &res[i], status );
	      smf_close_related( &lut[i], status );
	      smf_close_related( &qua[i], status );

	      if( haveext ) {
		smf_close_related( &model[whichext][i], status );
	      }
	    }
	  }
	}

	/* Increment iteration counter */
	iter++;

	if( *status == SAI__OK ) {

	  /* Check that we've exceeded maxiter */
	  if( iter >= maxiter ) {
	    quit = 1;
	  }

	  /* Check for convergence */
	  if( untilconverge && converged ) {
	    quit = 1;
	  }

	} else {
	  quit = 1;
	}
      }

      msgSeti("ITER",iter);
      msgOut( " ", 
	      "SMF_ITERATEMAP: ****** Completed in ^ITER iterations", status);
      if( untilconverge && converged ) {
	msgOut( " ", 
		"SMF_ITERATEMAP: ****** Solution CONVERGED",
		status);
      }

      /* Export DIMM model components to NDF files.
	 Note that we don't do LUT since it is originally an extension in the
	 input flatfielded data.
	 Also - check that a filename is defined in the smfFile! */
  
      if( exportNDF && (*status == SAI__OK) ) {
	msgOut(" ", "SMF_ITERATEMAP: Export model components to NDF files.", 
	       status);
      
	for( i=0; i<nchunks; i++ ) {  /* Chunk loop */
	  msgSeti("CHUNK", i+1);
	  msgSeti("NUMCHUNK", nchunks);
	  msgOutif(MSG__VERB," ", "  Chunk ^CHUNK / ^NUMCHUNK", status);

	  /* Open each subgroup, loop over smfArray elements and export,
	     then close subgroup. DIMM open/close not needed if memiter set.
             Note that QUA and NOI get stuffed into the QUALITY and
             VARIANCE components of the residual. Also notice that 
             everything must be changed to time-ordered data before
             writing ICD-compliant files. */

	  if( !memiter ) { /* Open if we're doing disk i/o */
	    /* Fixed components */
	    smf_open_related_model( resgroup, i, "UPDATE", &res[i], status );
	    smf_open_related_model( quagroup, i, "UPDATE", &qua[i], status );
	    smf_open_related_model( astgroup, i, "UPDATE", &ast[i], status );

	    /* Dynamic components */
	    for( j=0; j<nmodels; j++ ) { 
	      smf_open_related_model( modelgroups[j], i, "UPDATE", 
				      &model[j][i], status );	      
	    }
	  }

	  /* Loop over subgroup (subarray), re-order and export */
	  for( idx=0; idx<res[i]->ndat; idx++ ) {
	    smf_dataOrder( res[i]->sdata[idx], 1, status );
	    smf_dataOrder( qua[i]->sdata[idx], 1, status );
	    smf_dataOrder( ast[i]->sdata[idx], 1, status );

	    for( j=0; j<nmodels; j++ ) { 
	      
	      /* Check for existence of the model for this subarray - in
		 some cases, like COM, there is only a file for one subarray,
		 unlike RES from which the range of idx is derived */
	      if( model[j][i]->sdata[idx] ) {
		smf_dataOrder( model[j][i]->sdata[idx], 1, status );
		if( *status == SMF__WDIM ) {
		  /* fails if not 3-dimensional data. Just annul and write out 
		     data as-is. */
		  errAnnul(status);
		  model[j][i]->sdata[idx]->isTordered=1;
		}
	      }
	    }

	    if( *status == SAI__OK ) {
	      if( memiter ) {
		/* Pointer to the header in the concatenated data */
		hdr = res[i]->sdata[idx]->hdr;
	      } else {
		/* Open the header of the original input file in memiter=0
		   case since it won't have been stored in the .DIMM files */
		smf_open_file( igrp, resgroup->subgroups[i][idx], "READ", 
			       SMF__NOCREATE_DATA, &data, status );
		if( *status == SAI__OK ) {
		  hdr = data->hdr;
		}
	      }
	    }

	    /* QUA becomes the quality component of RES. NOI becomes
               the variance component of RES if present. */
	    if( *status == SAI__OK ) {
	      if( havenoi ) {
		var_data = (double *)(model[whichnoi][i]->sdata[idx]->pntr)[0];
	      } else {
		var_data = NULL;
	      }
	      
	      if( (res[i]->sdata[idx]->file->name)[0] ) {
		smf_NDFexport( res[i]->sdata[idx], var_data, 
				     qua[i]->sdata[idx]->pntr[0], hdr,
				     res[i]->sdata[idx]->file->name, 
				     status );
	      } else {
		msgOut( " ", 
			"SMF__ITERATEMAP: Can't export RES -- NULL filename",
			status);
	      }
	      
	      if( (ast[i]->sdata[idx]->file->name)[0] ) {
		smf_NDFexport( ast[i]->sdata[idx], NULL, NULL, hdr, 
				     ast[i]->sdata[idx]->file->name, 
				   status );
	      } else {
		msgOut( " ", 
			"SMF__ITERATEMAP: Can't export AST -- NULL filename",
			status);
	      }
	    }

	    /* Dynamic components excluding NOI */
	    for( j=0; j<nmodels; j++ ) 
	      /* Remember to check again whether model[j][i]->sdata[idx] exists
                 for cases like COM */
	      if( (*status == SAI__OK) && (modeltyps[j] != SMF__NOI) &&
		  model[j][i]->sdata[idx] ) {
		if( (model[j][i]->sdata[idx]->file->name)[0] ) {
		  smf_NDFexport( model[j][i]->sdata[idx], NULL, NULL,  
				       hdr, 
				       model[j][i]->sdata[idx]->file->name, 
				       status);
		} else {
		  msgSetc("MOD",smf_model_getname(modeltyps[j], status) );
		  msgOut( " ", 
			  "SMF__ITERATEMAP: Can't export ^MOD: NULL filename",
			  status);
		}
	      }
	    
	    /* Close the input file containing the header */
	    if( !memiter ) {
	      smf_close_file( &data, status );
	    }
	  }
	  
	  if( !memiter ) { /* Close files if doing disk i/o */
	    smf_close_related( &res[i], status );
	    smf_close_related( &qua[i], status );
	    smf_close_related( &ast[i], status );
	    
	    for( j=0; j<nmodels; j++ ) { 
	      smf_close_related( &model[j][i], status );
	    }
	  }
	}
      }
    }

    /* Cleanup things used specifically in this contchunk */

    /* fixed model smfGroups */
    if( resgroup ) smf_close_smfGroup( &resgroup, status );
    if( astgroup ) smf_close_smfGroup( &astgroup, status );  
    if( lutgroup ) smf_close_smfGroup( &lutgroup, status );  
    if( quagroup ) smf_close_smfGroup( &quagroup, status );  

    /* fixed model smfArrays */
    if( res ) {
      for( i=0; i<nchunks; i++ ) {
	if( res[i] ) smf_close_related( &res[i], status );
      }
      res = smf_free( res, status );
    }

    if( ast ) {
      for( i=0; i<nchunks; i++ ) {
	if( ast[i] ) smf_close_related( &ast[i], status );
      }
      ast = smf_free( ast, status );
    }

    if( lut ) {
      for( i=0; i<nchunks; i++ ) {
	if( lut[i] ) smf_close_related( &lut[i], status );
      }
      lut = smf_free( lut, status );
    }
  
    if( qua ) {
      for( i=0; i<nchunks; i++ ) {
	if( qua[i] ) smf_close_related( &qua[i], status );
      }
      qua = smf_free( qua, status );
    }

    /* dynamic model smfGroups */
    if( modelgroups ) {
      for( i=0; i<nmodels; i++ ) {
	if( modelgroups[i] ) smf_close_smfGroup( &modelgroups[i], status );
      }

      /* Free array of smfGroup pointers at this time chunk */
      modelgroups = smf_free( modelgroups, status );
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
	  model[i] = smf_free( model[i], status );
	}
      }
      model = smf_free( model, status );
    }

    /* In the multiple contchunk case, add maps together */

    if( contchunk >= 1 ) {
      msgOut( " ", "SMF_ITERATEMAP: Adding map estimated from this continuous chunk to total", status);
      smf_simpleaddmap( map, weights, hitsmap, mapvar, thismap, thisweight,
			thishits, thisvar, msize, status );
    }

    /* Free chisquared array */
    if( chisquared) chisquared = smf_free( chisquared, status );
    if( lastchisquared) lastchisquared = smf_free( lastchisquared, status );
  }
    
  /* The second set of map arrays get freed in the multiple contchunk case */
  if( thismap != map ) thismap = smf_free( thismap, status );
  if( thishits != hitsmap ) thishits = smf_free( thishits, status );
  if( thisvar != mapvar ) thisvar = smf_free( thisvar, status );
  if( thisweight != weights ) thisweight = smf_free( thisweight, status );

  if( modeltyps ) modeltyps = smf_free( modeltyps, status );

  if( igroup ) {
    smf_close_smfGroup( &igroup, status );
  }


 
}
