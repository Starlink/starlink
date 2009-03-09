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
*     smf_calcmodel_com( smfDIMMData *dat, int chunk, AstKeyMap *keymap, 
*			 smfArray **allmodel, int flags, int *status)

*  Arguments:
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

#define FUNC_NAME "smf_calcmodel_com"

void smf_calcmodel_com( smfDIMMData *dat, int chunk, AstKeyMap *keymap, 
			smfArray **allmodel, int flags, int *status) {

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
  dim_t idx=0;                  /* Index within subgroup */
  dim_t j;                      /* Loop counter */
  double lastmean;              /* Mean from previous iteration */
  unsigned char mask_cor;       /* Ignore quality mask for correction */
  unsigned char mask_meas;      /* Ignore quality mask for measurement */
  smfArray *model=NULL;         /* Pointer to model at chunk */
  double *model_data=NULL;      /* Pointer to DATA component of model */
  double *model_data_copy=NULL; /* Copy of model_data */
  dim_t nbolo=0;                /* Number of bolometers */
  dim_t ndata=0;                /* Total number of data points */
  size_t newbad;                /* Number of new bolos being flagged as bad */
  dim_t ntslice=0;              /* Number of time slices */
  double off;                   /* Temporary offset */ 
  smfArray *qua=NULL;           /* Pointer to QUA at chunk */
  int quit;                     /* While loop quit flag */
  unsigned char *qua_data=NULL; /* Pointer to quality data */
  smfArray *res=NULL;           /* Pointer to RES at chunk */
  double *res_data=NULL;        /* Pointer to DATA component of res */
  dim_t thisnbolo=0;            /* Check each file same dims as first */
  dim_t thisndata=0;            /* "                                  */
  dim_t thisntslice=0;          /* "                                  */
  double sum;                   /* Sum of data at current tslice */
  size_t tstride;               /* Time slice stride in data array */
  double *weight=NULL;          /* Weight at each point in model */
                                   
  /* Main routine */
  if (*status != SAI__OK) return;

  /* Obtain pointers to relevant smfArrays for this chunk */
  res = dat->res[chunk];
  qua = dat->qua[chunk];
  model = allmodel[chunk];
  if(dat->gai) gai = dat->gai[chunk];
  
  /* Check for smoothing parameters in the CONFIG file */
  if( astMapGet0I( keymap, "COM_BOXCAR", &boxcar) ) {
    do_boxcar = 1;
  }

  /* Check for damping parameter on boxcar */
  if( astMapGet0D( keymap, "COM_BOXFACT", &boxfact) ) {
    do_boxfact = 1;

    /* If first iteration, set COM_BOXCARD (this value will get decreased) */
    if( flags&SMF__DIMM_FIRSTITER ) {
      astMapPut0D( keymap, "COM_BOXCARD", (double) boxcar, NULL );
    }

    if( !astMapGet0D( keymap, "COM_BOXCARD", &boxcard) ) {
      *status = SAI__ERROR;
      errRep( "", FUNC_NAME ": Failed to retrieve COM_BOXCARD from keymap", 
              status);
    } else {
      /* Use damped boxcar for smoothing width */
      boxcar = (int) boxcard;
    }
  }

  /* Check for minimum boxcar width*/
  if( astMapGet0I( keymap, "COM_BOXMIN", &boxmin) ) {
    do_boxmin = 1;
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
  mask_meas = ~(SMF__Q_JUMP|SMF__Q_STAT); 

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
      /* Loop over time slice */
      for( i=0; i<ntslice; i++ ) {        
        lastmean = model_data_copy[i];
        for( j=0; j<nbolo; j++ ) {
          if( dat->gai ) {
            /* if GAIn model was fit, there is an additional offset (the gain
               has already been un-done in smf_iteratemap at the end of the
               previous iteration). Offset is stored in the second plane of
               the gain model. */
            g = 1./gai_data[j*gbstride];
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

      /* Loop over time slice and calculate the average of all the good 
         detectors at each time slice */
      for( i=0; i<ntslice; i++ ) {
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
           sum from previous subarrays. */

        if( idx == 0 ) {
          model_data[i] = sum;
        } else {
          model_data[i] += sum;
        }
      }
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
           the third correlation coefficients. */
        smf_get_dims( gai->sdata[idx],  NULL, NULL, NULL, NULL, NULL,
                      &gbstride, &gcstride, status);
        gai_data = (gai->sdata[idx]->pntr)[0];
        
        for( i=0; (*status==SAI__OK) && (i<nbolo); i++ ) {
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
        msgSetd("MEAN",cmean);
        msgSetd("SIG",csig);
        msgOutif( MSG__DEBUG, " ", FUNC_NAME 
                  ": corr coeff ^MEAN +/- ^SIG", status );

        smf_stats1( gcoeff, 1, nbolo, NULL, 0, &gmean, &gsig, &ggood, status );
        msgSetd("MEAN",gmean);
        msgSetd("SIG",gsig);
        msgOutif( MSG__DEBUG, " ", FUNC_NAME 
                  ": log(abs(gain coeff))  ^MEAN +/- ^SIG", status );

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
    astMapPut0D( keymap, "COM_BOXCARD", boxcard, NULL );
  }
  
  /* Clean up */
  if( weight)  weight = smf_free( weight, status );
  if( model_data_copy ) model_data_copy = smf_free( model_data_copy, status );
}
