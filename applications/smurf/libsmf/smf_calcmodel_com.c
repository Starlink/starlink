/*
*+
*  Name:
*     smf_calcmodel_com

*  Purpose:
*     Calculate the COMmon-mode model signal component

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_calcmodel_com( smfWorkForce *wf, smfDIMMData *dat, int
*			 chunk, AstKeyMap *keymap, smfArray
*			 **allmodel, int flags, int *status)

*  Arguments:
*     wf = smfWorkForce * (Given)
*        Pointer to a pool of worker threads
*     dat = smfDIMMData * (Given)
*        Struct of pointers to information required by model calculation
*     chunk = int (Given)
*        Index of time chunk in allmodel to be calculated
*     keymap = AstKeyMap * (Given)
*        Parameters that control the iterative map-maker
*     allmodel = smfArray ** (Returned)
*        Array of smfArrays (each time chunk) to hold result of model calc
*     flags = int (Given )
*        Control flags: not used 
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Calculate the common-mode signal measured at every time-slice.

*  Notes:

*  Authors:
*     Edward Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2006-07-10 (EC):
*        Initial Version
*     2006-11-02 (EC):
*        Updated to correctly modify cumulative and residual models
*     2007-02-08 (EC):
*        Fixed bug in replacing previous model before calculating new one
*     2007-03-05 (EC)
*        Modified bit flags
*        Modified data array indexing to avoid unnecessary multiplies
*     2007-05-23 (EC)
*        - Removed CUM calculation
*        - Added COM_BOXCAR parameter to CONFIG file
*     2007-07-10 (EC)
*        Use smfArray instead of smfData
*     2007-07-13 (EC):
*        Calculate only 1 model component for each smfArray
*     2007-07-16 (EC):
*        Modified range checking for range of smfData's in smfArray
*     2007-08-09 (EC):
*        Fixed bug in replacement of model in residual before calculating
*        new model for current iteration.
*     2007-12-14 (EC):
*        Updated to use bolo-ordered data, disabled boxcar smoothing
*     2007-12-18 (AGG):
*        Update to use new smf_free behaviour
*     2008-03-04 (EC)
*        Modified interface to use smfDIMMData
*     2008-04-03 (EC)
*        Use QUALITY
*     2008-04-14 (EC)
*        - enabled boxcar smoothing again
*        - improved QUALITY masking
*     2008-05-02 (EC)
*        - Added damping to boxcar smooth: COM_BOXFACT, COM_BOXMIN
*     2008-12-12 (EC)
*        - Solve for GAIn if requested
*     2008-12-17 (EC)
*        - Look at spread in GAIn correlation coefficients to iteratively
*          flag additional bad bolometers
*     2008-12-18 (EC)
*        - Additionally look at spread in gain coefficients to flag bad bolos 
*     2009-03-09 (EC)
*        - Fit common mode gain to data, instead of using it to modify flatfield
*     2009-04-17 (EC)
*        - switch to subkeymap notation in config file 
*     2009-04-30 (EC)
*        Parallelize the slow bits: undoing old common mode, calculating new
*        common mode, and fitting common mode template to all the bolos.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2006-2009 University of British Columbia.
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

/* Starlink includes */
#include "mers.h"
#include "ndf.h"
#include "sae_par.h"
#include "star/ndg.h"
#include "prm_par.h"
#include "par_par.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "libsmf/smf_err.h"

/* ------------------------------------------------------------------------ */
/* Local variables and functions */

/* Structure containing information about blocks of bolos that each
 thread will process*/
typedef struct smfCalcmodelComData {
  size_t b1;               /* Index of first bolometer of block */
  size_t b2;               /* Index of last bolometer of block */
  size_t bstride;          /* bolometer stride for res/qua */
  double *gai_data;        /* pointer to gain model (can be NULL) data */
  size_t gbstride;         /* gain bolo stride */
  size_t gcstride;         /* gain coefficient stride */
  dim_t idx;               /* Index within subgroup */
  int ijob;                /* Job identifier */
  unsigned char mask_cor;  /* Ignore quality mask for correction */
  unsigned char mask_meas; /* Ignore quality mask for measurement */
  double *model_data;      /* pointer to common mode data */
  dim_t nbolo;             /* number of bolometers */
  dim_t ntslice;           /* number of time slices */
  int operation;           /* 0=undo COM, 1=new COM, 2=fit COM */
  double *res_data;        /* Pointer to common residual data */
  size_t t1;               /* Index of first timeslice of block */
  size_t t2;               /* Index of last timeslice of block */
  size_t tstride;          /* time stride for res/qua */
  unsigned char *qua_data; /* Pointer to common quality data */
  double *weight;          /* Weight at each point in model */
} smfCalcmodelComData;

/* Function to be executed in thread: task depends on value of operation */

void smfCalcmodelComPar( void *job_data_ptr, int *status );

void smfCalcmodelComPar( void *job_data_ptr, int *status ) {
  size_t bstride;          /* bolometer stride for res/qua */
  double g;                /* gain coeff */
  double *gai_data;        /* pointer to gain model (can be NULL) data */
  size_t gbstride;         /* gain bolo stride */
  size_t gcstride;         /* gain coefficient stride */
  size_t i;                /* Loop counter */
  dim_t idx;               /* Index within subgroup */
  size_t j;                /* Loop counter */
  double lastmean;         /* Last values of common mode */
  unsigned char mask_cor;  /* Ignore quality mask for correction */
  unsigned char mask_meas; /* Ignore quality mask for measurement */
  double *model_data;      /* pointer to common mode data */
  dim_t nbolo;             /* number of bolometers */
  dim_t ntslice;           /* number of time slices */
  double off;              /* offset coeff */
  smfCalcmodelComData *pdata=NULL; /* Pointer to job data */
  double *res_data;        /* Pointer to common residual data */
  double sum;              /* Sum at current time slice */
  size_t tstride;          /* time stride for res/qua */
  unsigned char *qua_data; /* Pointer to common quality data */
  double *weight=NULL;     /* Weight at each point in model */


  if( *status != SAI__OK ) return;

  /* Pointer to the data that this thread will process */
  pdata = job_data_ptr;

  /* Check for valid inputs */
  if( !pdata ) {
    *status = SAI__ERROR;
    errRep( "", "smfCalcmodelComPar: No job data supplied", status );
    return;
  }

  bstride = pdata->bstride;
  gai_data = pdata->gai_data;
  gbstride = pdata->gbstride;
  gcstride = pdata->gcstride;
  idx = pdata->idx;
  mask_cor = pdata->mask_cor;
  mask_meas = pdata->mask_meas;
  model_data = pdata->model_data; /* Careful! */
  nbolo = pdata->nbolo;
  ntslice = pdata->ntslice;
  res_data = pdata->res_data;
  tstride = pdata->tstride;
  qua_data = pdata->qua_data;
  weight = pdata->weight;

  /* if b1 past end of the work, nothing to do so we return */
  if( pdata->b1 >= nbolo ) {
    msgOutif( MSG__DEBUG, "", 
               "smfCalcmodelComPar: nothing for thread to do, returning",
               status);
    return;
  }

  if( pdata->operation == 0 ) {
    /* undo the previous iteration of the model, each thread handles a
     block of bolos */

    /* Debugging message indicating thread started work */
    msgOutiff( MSG__DEBUG, "", 
               "smfCalcmodelComPar(%i): thread starting on bolos %zu -- %zu",
               status, pdata->operation, pdata->b1, pdata->b2 );


    for( i=0; i<ntslice; i++ ) { /* Loop over time slice */
      lastmean = model_data[i];
      for( j=pdata->b1; j<=pdata->b2; j++ ) {
        if( gai_data ) {
          /* if GAIn model was fit, the common mode has a gain (1st
             plane of the model) and an offset (2nd plane of the model). */
          g = gai_data[j*gbstride];
          off = gai_data[j*gbstride + gcstride];
        } else {
          off = 0;
          g = 1;
        }
        
        /* Put the last iteration back in */
        if( !(qua_data[i*tstride+j*bstride]&mask_cor) ) {
          res_data[i*tstride+j*bstride] += g*lastmean+off;
        }
      }
    }

    msgOutiff( MSG__DEBUG, "", 
               "smfCalcmodelComPar(%i): thread finishing bolos %zu -- %zu",
               status, pdata->operation, pdata->b1, pdata->b2 );

  } else if( pdata->operation == 1 ) {
    /* Calculate the new common mode averaging over all detectors. Each thread
       handles a block of time slices */

    /* Debugging message indicating thread started work */
    msgOutiff( MSG__DEBUG, "", 
               "smfCalcmodelComPar(%i): thread starting on tslices %zu -- %zu",
               status, pdata->operation, pdata->t1, pdata->t2 );

    for( i=pdata->t1; i<=pdata->t2; i++ ) {
      /* Loop over bolometers to put the previous common-mode
         signal back in at each time-slice, and calculate the sum of
         all the detectors with good data. */
      sum = 0;   /* Initialize sum to 0 */        
      for( j=0; j<nbolo; j++ ) {
        if( !(qua_data[i*tstride+j*bstride]&mask_meas) ) {
          sum += res_data[i*tstride+j*bstride];
          weight[i]++;
        }
      }
      
      /* Store the sum here. Init if first subarray, otherwise add to
         sum from previous subarrays. Renormalization happens outside
         thread after loop over subarray */
      
      if( idx == 0 ) {
        model_data[i] = sum;
      } else {
        model_data[i] += sum;
      }
    }

    msgOutiff( MSG__DEBUG, "", 
               "smfCalcmodelComPar(%i): thread finishing tslices %zu -- %zu",
               status, pdata->operation, pdata->t1, pdata->t2 );

  } else if( pdata->operation == 2 ) {
    /* Loop over the block of bolos for this thread and fit the template */

    /* Debugging message indicating thread started work */
    msgOutiff( MSG__DEBUG, "", 
               "smfCalcmodelComPar(%i): thread starting on bolos %zu -- %zu",
               status, pdata->operation, pdata->b1, pdata->b2 );

    for( i=pdata->b1; (*status==SAI__OK) && (i<=pdata->b2); i++ ) {
      if( !(qua_data[i*bstride]&SMF__Q_BADB) ) {          
        smf_templateFit1D( res_data+i*bstride, qua_data+i*bstride, 
                           mask_meas, mask_cor, ntslice, tstride, 
                           model_data, 0, gai_data+i*gbstride, 
                           gai_data+gcstride+i*gbstride, 
                           gai_data+2*gcstride+i*gbstride, status ); 
        
        /* If divide-by-zero detected, flag bolo as bad */
        if( *status==SMF__DIVBZ ) {
          for( j=0; j<ntslice; j++ ) {
            qua_data[i*bstride+j*tstride] |= SMF__Q_BADB;
          }
          errAnnul( status );
        }
      }
    }

    msgOutiff( MSG__DEBUG, "", 
               "smfCalcmodelComPar(%i): thread finishing bolos %zu -- %zu",
               status, pdata->operation, pdata->b1, pdata->b2 );

  } else {
    *status = SAI__ERROR;
    errRep( "", "smfCalcmodelComPar: invalid operation specifier", status );
  }
  
}

/* ------------------------------------------------------------------------ */

#define FUNC_NAME "smf_calcmodel_com"

void smf_calcmodel_com( smfWorkForce *wf, smfDIMMData *dat, int chunk, 
                        AstKeyMap *keymap, smfArray **allmodel, int flags, 
                        int *status) {

  /* Local Variables */
  size_t bstride;               /* Bolometer stride in data array */
  int boxcar=0;                 /* width in samples of boxcar filter */ 
  double boxcard=0;             /* double precision version of boxcar */ 
  double boxfact=0;             /* Box width damping parameter */
  int boxmin=0;                 /* Min boxcar width if boxfact set */
  int do_boxcar=0;              /* flag to do boxcar smooth */
  int do_boxfact=0;             /* flag to damp boxcar width */
  int do_boxmin=0;              /* flag for minimum boxcar */
  dim_t cgood;                  /* Number of good corr. coeff. samples */
  double cmean;                 /* mean of common-mode correlation coeff */
  double *corr=NULL;            /* Array to hold correlation coefficients */
  double csig;                  /* standard deviation "                  */
  double *gcoeff=NULL;          /* Array to hold gain coefficients */
  dim_t ggood;                  /* Number of good gain. coeff. samples */
  double gmean;                 /* mean of common-mode correlation gain */
  double gsig;                  /* standard deviation "                  */
  double g;                     /* temporary gain */
  smfArray *gai=NULL;           /* Pointer to GAI at chunk */
  double *gai_data=NULL;        /* Pointer to DATA component of GAI */
  size_t gbstride;              /* GAIn bolo stride */
  size_t gcstride;              /* GAIn coeff stride */
  dim_t i;                      /* Loop counter */
  int ii;                       /* Loop counter */
  dim_t idx=0;                  /* Index within subgroup */
  dim_t j;                      /* Loop counter */
  smfCalcmodelComData *job_data=NULL; /* Array of job data */
  AstKeyMap *kmap=NULL;         /* Local keymap */
  unsigned char mask_cor;       /* Ignore quality mask for correction */
  unsigned char mask_meas;      /* Ignore quality mask for measurement */
  smfArray *model=NULL;         /* Pointer to model at chunk */
  double *model_data=NULL;      /* Pointer to DATA component of model */
  double *model_data_copy=NULL; /* Copy of model_data */
  dim_t nbolo=0;                /* Number of bolometers */
  dim_t ndata=0;                /* Total number of data points */
  size_t newbad;                /* Number of new bolos being flagged as bad */
  dim_t ntslice=0;              /* Number of time slices */
  int nw;                       /* Number of worker threads */
  double off;                   /* Temporary offset */ 
  smfCalcmodelComData *pdata=NULL; /* Pointer to job data */
  smfArray *qua=NULL;           /* Pointer to QUA at chunk */
  int quit;                     /* While loop quit flag */
  unsigned char *qua_data=NULL; /* Pointer to quality data */
  smfArray *res=NULL;           /* Pointer to RES at chunk */
  double *res_data=NULL;        /* Pointer to DATA component of res */
  size_t step;                  /* step size for dividing up work */
  dim_t thisnbolo=0;            /* Check each file same dims as first */
  dim_t thisndata=0;            /* "                                  */
  dim_t thisntslice=0;          /* "                                  */
  size_t tstride;               /* Time slice stride in data array */
  double *weight=NULL;          /* Weight at each point in model */
                                   
  /* Main routine */
  if (*status != SAI__OK) return;

  /* How many threads do we get to play with */
  nw = wf ? wf->nworker : 1;

  /* Obtain pointer to sub-keymap containing COM parameters */
  if( !astMapGet0A( keymap, "COM", &kmap ) ) {
    kmap = NULL;
  }

  /* Obtain pointers to relevant smfArrays for this chunk */
  res = dat->res[chunk];
  qua = dat->qua[chunk];
  model = allmodel[chunk];
  if(dat->gai) gai = dat->gai[chunk];
  
  /* Parse parameters */

  if( kmap ) {
    /* Check for smoothing parameters in the CONFIG file */
    if( astMapGet0I( kmap, "BOXCAR", &boxcar) ) {
      do_boxcar = 1;
    }

    /* Check for damping parameter on boxcar */
    if( astMapGet0D( kmap, "BOXFACT", &boxfact) ) {
      do_boxfact = 1;

      /* If first iteration, set BOXCARD (this value will get decreased) */
      if( flags&SMF__DIMM_FIRSTITER ) {
        astMapPut0D( kmap, "BOXCARD", (double) boxcar, NULL );
      }

      if( !astMapGet0D( kmap, "BOXCARD", &boxcard) ) {
        *status = SAI__ERROR;
        errRep( "", FUNC_NAME ": Failed to retrieve BOXCARD from keymap", 
                status);
      } else {
        /* Use damped boxcar for smoothing width */
        boxcar = (int) boxcard;
      }
    }

    /* Check for minimum boxcar width*/
    if( astMapGet0I( kmap, "BOXMIN", &boxmin) ) {
      do_boxmin = 1;
    }
  }

  if( do_boxcar ) {
    msgSeti("BOX",boxcar);
    msgOutif(MSG__VERB, " ", "    boxcar width ^BOX",status);
  } 

  /* Assert bolo-ordered data */
  for( idx=0; idx<res->ndat; idx++ ) if (*status == SAI__OK ) {
    smf_dataOrder( res->sdata[idx], 0, status );
    smf_dataOrder( qua->sdata[idx], 0, status );
    if(dat->gai) smf_dataOrder( gai->sdata[idx], 0, status );
  }

  /* The common mode signal is stored as a single 1d vector for all 4
     subarrays.  The corresponding smfData is at position 0 in the
     model sdata. */
  
  if( model->sdata[0] ) {
    /* Pointer to model data array */
    model_data = (model->sdata[0]->pntr)[0];

    /* Copy of model data array */
    model_data_copy = smf_malloc( (model->sdata[0]->dims)[0], 
			  sizeof(*model_data_copy), 1, status );
    if( *status == SAI__OK ) {
      memcpy( model_data_copy, model_data, (model->sdata[0]->dims)[0] *
	      sizeof(*model_data_copy) );
    }
    /* Temporary buffer to store weights */
    weight = smf_malloc( (model->sdata[0]->dims)[0], sizeof(*weight), 0,
			 status );
  } else {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": Model smfData was not loaded!", status);      
  }

  /* Which QUALITY bits should be checked for correcting sample. Ensure that
     this matches the mask used in smf_calcmodel_gai! */
  mask_cor = ~(SMF__Q_JUMP|SMF__Q_SPIKE|SMF__Q_APOD|SMF__Q_STAT);

  /* Which QUALITY bits should be checked for measuring the mean */
  mask_meas = mask_cor;


  /* Allocate job data for threads */
  job_data = smf_malloc( nw, sizeof(*job_data), 1, status );

  /* Loop over index in subgrp (subarray) and put the previous iteration
     of the common mode back into the signal */
  for( idx=0; idx<res->ndat; idx++ ) if (*status == SAI__OK ) {
    /* Obtain dimensions of the data */
    smf_get_dims( res->sdata[idx],  NULL, NULL, &thisnbolo, &thisntslice, 
                  &thisndata, &bstride, &tstride, status);
      
    if(dat->gai) {
      smf_get_dims( gai->sdata[idx],  NULL, NULL, NULL, NULL, NULL,
                    &gbstride, &gcstride, status);
    }
      
    if( idx == 0 ) {
      /* Store dimensions of the first file */
      nbolo = thisnbolo;
      ntslice = thisntslice;
      ndata = thisndata;      

      /*  --- Set up the division of labour for threads --- */ 

      /* Mutually exclusive blocks of bolos */

      if( nw > (int) nbolo ) {
        step = 1;
      } else {
        step = nbolo/nw;
      }

      for( ii=0; (*status==SAI__OK)&&(ii<nw); ii++ ) {
        pdata = job_data + ii;

        pdata->b1 = ii*step;
        pdata->b2 = (ii+1)*step-1;
        
        /* Ensure that the last thread picks up any left-over bolometers */
        if( (ii==(nw-1)) && (pdata->b1<(nbolo-1)) ) {
          pdata->b2=nbolo-1;
        }
      }

      /* Mutually exclusive blocks of time slices */

      if( (ntslice < 1000) && (nw > (int) ntslice) ) {
        step = 1;
      } else {
        step = ntslice/nw;
      }

      for( ii=0; (*status==SAI__OK)&&(ii<nw); ii++ ) {
        pdata = job_data + ii;

        /* Blocks of bolos */
        pdata->t1 = ii*step;
        pdata->t2 = (ii+1)*step-1;
        
        /* Ensure that the last thread picks up any left-over bolometers */
        if( (ii==(nw-1)) && (pdata->t1<(ntslice-1)) ) {
          pdata->t2=ntslice-1;
        }
      }

    } else {
      /* Check that dimensions haven't changed */
      if( (thisnbolo != nbolo) || (thisntslice != ntslice) || 
          (thisndata != ndata) ) {
        *status = SAI__ERROR;        
        errRep( "", FUNC_NAME 
                ": smfData's in smfArray have different dimensions!", 
                status);      
      }
    }

    /* Get pointers to data/quality/model */
    res_data = (res->sdata[idx]->pntr)[0];
    qua_data = (qua->sdata[idx]->pntr)[0];
    if( dat->gai ) {
      gai_data = (gai->sdata[idx]->pntr)[0];
    }

    if( (res_data == NULL) || (model_data == NULL) || (qua_data == NULL) ) {
      *status = SAI__ERROR;
      errRep( "", FUNC_NAME ": Null data in inputs", status);      
    } else {

      /* Set up the job data and undo previous iteration of model */

      for( ii=0; (*status==SAI__OK)&&(ii<nw); ii++ ) {
        pdata = job_data + ii;
        
        pdata->bstride = bstride;
        pdata->gai_data = gai_data;
        pdata->gbstride = gbstride;
        pdata->gcstride = gcstride;
        pdata->idx = idx;
        pdata->mask_cor = mask_cor;
        pdata->mask_meas = mask_meas;
        pdata->model_data = model_data_copy; /* Careful! */
        pdata->nbolo = nbolo;
        pdata->ntslice = ntslice;
        pdata->operation = 0;
        pdata->res_data = res_data;
        pdata->tstride = tstride;
        pdata->qua_data = qua_data;
        pdata->ijob = -1;
        pdata->weight = weight;
        
        /* Submit the job immediately */
        pdata->ijob = smf_add_job( wf, SMF__REPORT_JOB, pdata, 
                                   smfCalcmodelComPar, NULL, status );
      }
      
      /* Wait until all of the submitted jobs have completed */
      smf_wait( wf, status );      
    }
  }    

  /* Outer loop re-calculates common-mode until the list of "good" bolometers
     converges. Without a fit for gain/offset this loop only happens once.
     Otherwise it keeps going until the correlation coefficients of the
     template fits settle down to a list with no N-sigma outliers */
  
  quit = 0;
  while( !quit && (*status==SAI__OK) ) {
    /* Initialize to assumption that we'll finish this iteration */
    quit = 1;

    /* initialize model data and weights to 0 */
    memset(model_data,0,(model->sdata[0]->dims)[0]*sizeof(*model_data));
    memset(weight,0,(model->sdata[0]->dims)[0]*sizeof(*weight));

    /* Loop over index in subgrp (subarray) */
    for( idx=0; (idx<res->ndat)&&(*status==SAI__OK); idx++ ) {

      /* Get pointers to data/quality/model */
      res_data = (res->sdata[idx]->pntr)[0];
      qua_data = (qua->sdata[idx]->pntr)[0];
      if( dat->gai ) {
        gai_data = (gai->sdata[idx]->pntr)[0];
      }


      /* Set up the job data and calculate new common mode */

      for( ii=0; (*status==SAI__OK)&&(ii<nw); ii++ ) {
        pdata = job_data + ii;
        
        pdata->bstride = bstride;
        pdata->gai_data = gai_data;
        pdata->gbstride = gbstride;
        pdata->gcstride = gcstride;
        pdata->idx = idx;
        pdata->mask_cor = mask_cor;
        pdata->mask_meas = mask_meas;
        pdata->model_data = model_data; /* Careful! */
        pdata->nbolo = nbolo;
        pdata->ntslice = ntslice;
        pdata->operation = 1;
        pdata->res_data = res_data;
        pdata->tstride = tstride;
        pdata->qua_data = qua_data;
        pdata->ijob = -1;
        pdata->weight = weight;

        /* Submit the job immediately */
        pdata->ijob = smf_add_job( wf, SMF__REPORT_JOB, pdata, 
                                   smfCalcmodelComPar, NULL, status );
      }
      
      /* Wait until all of the submitted jobs have completed */
      smf_wait( wf, status );      
    }

    /* Re-normalize the model, or set model to 0 if no data. */
    for( i=0; i<ntslice; i++ ) {
      if( weight[i] ) {
        model_data[i] /= weight[i];
      } else {
        model_data[i] = 0;
      }
    }
      
    /* boxcar smooth if desired */
    if( do_boxcar ) {
      /* Do the smooth */
      smf_boxcar1D( model_data, ntslice, boxcar, NULL, 0, status );
    }

    /* Common mode is calculated. Now try to fit it. */
    for( idx=0; (idx<res->ndat)&&(*status==SAI__OK); idx++ ) {
      smf_get_dims( res->sdata[idx],  NULL, NULL, NULL, NULL, NULL, &bstride,
                    &tstride, status );

      /* Get pointers */
      res_data = (double *)(res->sdata[idx]->pntr)[0];
      qua_data = (unsigned char *)(qua->sdata[idx]->pntr)[0];

      if( dat->gai ) {

        /* If GAIn model supplied, fit gain/offset of sky model to
           each bolometer independently. It is stored as 3 planes of
           nbolo samples: the first holds bolo gains, the second offsets, and
           the third correlation coefficients. The fitting is a bottleneck,
           so we use multiple threads to handle blocks of bolos in parallel.
        */

        smf_get_dims( gai->sdata[idx],  NULL, NULL, NULL, NULL, NULL,
                      &gbstride, &gcstride, status);
        gai_data = (gai->sdata[idx]->pntr)[0];
        
        /* Set up the job data and fit template to blocks of bolos */

        for( ii=0; (*status==SAI__OK)&&(ii<nw); ii++ ) {
          pdata = job_data + ii;

          pdata->bstride = bstride;
          pdata->gai_data = gai_data;
          pdata->gbstride = gbstride;
          pdata->gcstride = gcstride;
          pdata->idx = idx;
          pdata->mask_cor = mask_cor;
          pdata->mask_meas = mask_meas;
          pdata->model_data = model_data;
          pdata->nbolo = nbolo;
          pdata->ntslice = ntslice;
          pdata->operation = 2;
          pdata->res_data = res_data;
          pdata->tstride = tstride;
          pdata->qua_data = qua_data;
          pdata->ijob = -1;
          pdata->weight = weight;

          /* Submit the job immediately */
          pdata->ijob = smf_add_job( wf, SMF__REPORT_JOB, pdata, 
                                     smfCalcmodelComPar, NULL, status );
        }

        /* Wait until all of the submitted jobs have completed */
        smf_wait( wf, status );

        /* Calculate mean and r.m.s. of correlation coefficients and
           gains to flag outlier bolometers as bad */
        gcoeff = smf_malloc( nbolo, sizeof(*gcoeff), 0, status );
        corr = smf_malloc( nbolo, sizeof(*corr), 0, status );

        for( i=0; (*status==SAI__OK) && (i<nbolo); i++ ) {
          /* Copy correlation coefficients into an array that has VAL__BADD
             set at locations of bad bolometers. Can't use the main quality
             array directly as the stride may be different */          
          if( !(qua_data[i*bstride]&SMF__Q_BADB) && (gai_data[i*gbstride]) ) {
            gcoeff[i] = log(fabs(gai_data[i*gbstride]));
            corr[i] = gai_data[2*gcstride+i*gbstride];
          } else {
            gcoeff[i] = VAL__BADD;
            corr[i] = VAL__BADD;
          }
        }
        
        smf_stats1( corr, 1, nbolo, NULL, 0, &cmean, &csig, &cgood, status );
        msgSeti("N",cgood);
        msgOutif( MSG__VERB, "", FUNC_NAME ": ^N good bolos", status );
        msgOutiff( MSG__DEBUG, " ", FUNC_NAME 
                   ": corr coeff %8.5f +/- %8.5f", status, cmean, csig );

        smf_stats1( gcoeff, 1, nbolo, NULL, 0, &gmean, &gsig, &ggood, status );
        msgOutiff( MSG__DEBUG, " ", FUNC_NAME 
                   ": log(abs(gain coeff)) %8.5f +/- %8.5f", status,
                   gmean, gsig);

        /* Flag new bad bolometers */
        newbad = 0;
        for( i=0; (*status==SAI__OK) && (i<nbolo); i++ ) {
          if( (corr[i]==VAL__BADD) || (fabs(corr[i]-cmean) > 5*csig) ||
              (fabs(gcoeff[i]-gmean) > 5*gsig) || (fabs(gcoeff[i]-gmean)>3) ) {
            /* If this bolometer wasn't previously flagged as bad 
               do it here, and set quit=0 */
            if( !(qua_data[i*bstride]&SMF__Q_BADB) ) { 
              quit = 0;
              for( j=0; j<ntslice; j++ )
                qua_data[i*bstride + j*tstride] |= SMF__Q_BADB;
              gai_data[i*gbstride] = 1;              /* Set gain to 1 */
              gai_data[i*gbstride+gcstride] = 0;     /* offset to 0 */
              gai_data[i*gbstride+2*gcstride] = 0;   /* Zero correlation */
              newbad++;
            }
          } 
        }

        if( newbad ) {
          msgSeti("NEW",newbad);
          msgOutif( MSG__VERB, "", FUNC_NAME 
                    ": flagged ^NEW new bad bolos", status );
        }

        gcoeff = smf_free( gcoeff, status );
        corr = smf_free( corr, status );

      } else {
        /* If we're not fitting a gain and offset, just remove common-mode
           immediately and quit while loop. */
        quit = 1;
        for( i=0; i<nbolo; i++ ) {
          for( j=0; j<ntslice; j++ ) {
            /* update the residual */
            if( !(qua_data[i*bstride + j*tstride]&mask_cor) ) {
              res_data[i*bstride + j*tstride] -= model_data[j];
            }
          }
        }    
      }
    }

    if( !quit ) {
      /* around we go again... */
      msgOutif( MSG__DEBUG, "", FUNC_NAME ": Common mode not yet converged",
                status );
    }
  }

  if( dat->gai) {
    /* Once the common mode converged and if we're fitting the common
       mode gain and offset, remove scaled template here. */
    for( idx=0; (idx<res->ndat)&&(*status==SAI__OK); idx++ ) {
      smf_get_dims( res->sdata[idx],  NULL, NULL, NULL, NULL, NULL, &bstride,
                    &tstride, status );
      
      /* Get pointers */
      res_data = (double *)(res->sdata[idx]->pntr)[0];
      qua_data = (unsigned char *)(qua->sdata[idx]->pntr)[0];
      smf_get_dims( gai->sdata[idx],  NULL, NULL, NULL, NULL, NULL,
                    &gbstride, &gcstride, status);
      gai_data = (gai->sdata[idx]->pntr)[0];
      
      for( i=0; (*status==SAI__OK) && (i<nbolo); i++ ) 
        if( !(qua_data[i*bstride]&SMF__Q_BADB) ) {          
          /* The gain is the amplitude of the common mode template in data */
          g = gai_data[i*gbstride];
          if( (g!=VAL__BADD) && (g!=0) ){
            off = gai_data[i*gbstride+gcstride];
            gai_data[i*gbstride] = g;
            
            /* Remove the template */          
            for( j=0; j<ntslice; j++ ) {
              if( !(qua_data[i*bstride + j*tstride]&mask_cor) ) {
                res_data[i*bstride+j*tstride] -= (g*model_data[j] + off);
              }
            }
          } else {
            *status = SAI__ERROR;
            msgSeti("BOLO",i);
            errRep("", FUNC_NAME ": invalid gain encountered for bolo ^BOLO", 
                   status);
          }    
        }
    }
  }

  /* If damping boxcar smooth, reduce window size here */
  if( do_boxcar && do_boxfact && (*status == SAI__OK) ) {
    boxcard = boxcard * boxfact;
    /* Enforce minimum if available */
    if( do_boxmin ) {
      if( boxcard < boxmin ) boxcard = (double) boxmin;
    }
    /* Update value in the keymap so we can read it next iteration */
    astMapPut0D( kmap, "BOXCARD", boxcard, NULL );
  }
  
  /* Clean up */
  if( weight)  weight = smf_free( weight, status );
  if( model_data_copy ) model_data_copy = smf_free( model_data_copy, status );

  /* Clean up the job data array */
  if( job_data ) {
    job_data = smf_free( job_data, status );
  }

}
