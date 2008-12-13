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
*     {enter_further_changes_here}


*  Copyright:
*     Copyright (C) 2005-2006 Particle Physics and Astronomy Research Council.
*     University of British Columbia.
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

#define FUNC_NAME "smf_calcmodel_com"

void smf_calcmodel_com( smfDIMMData *dat, int chunk, AstKeyMap *keymap, 
			smfArray **allmodel, int flags, int *status) {

  /* Local Variables */
  dim_t base;                   /* Store base index for data array offsets */
  size_t bstride;               /* Bolometer stride in data array */
  int boxcar=0;                 /* width in samples of boxcar filter */ 
  double boxcard=0;             /* double precision version of boxcar */ 
  double boxfact=0;             /* Box width damping parameter */
  int boxmin=0;                 /* Min boxcar width if boxfact set */
  int do_boxcar=0;              /* flag to do boxcar smooth */
  int do_boxfact=0;             /* flag to damp boxcar width */
  int do_boxmin=0;              /* flag for minimum boxcar */
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
  dim_t ntslice=0;              /* Number of time slices */
  double off;                   /* Temporary offset */ 
  smfArray *qua=NULL;           /* Pointer to QUA at chunk */
  unsigned char *qua_data=NULL; /* Pointer to quality data */
  smfArray *res=NULL;           /* Pointer to RES at chunk */
  double *res_data=NULL;        /* Pointer to DATA component of res */
  double sum=0;                 /* Array sum */
  dim_t thisnbolo=0;            /* Check each file same dims as first */
  dim_t thisndata=0;            /* "                                  */
  dim_t thisntslice=0;          /* "                                  */
  double thissum;               /* Sum of data at current tslice */
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

  }

  /* Check for minimum boxcar width*/
  if( astMapGet0I( keymap, "COM_BOXMIN", &boxmin) ) {
    do_boxmin = 1;
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

      /* Set model_data to 0 now since it will now be re-calculated */
      memset( model_data, 0, (model->sdata[0]->dims)[0] * 
	      sizeof(*model_data_copy) );
      
    }

    /* Temporary buffer to store weights */
    weight = smf_malloc( (model->sdata[0]->dims)[0], sizeof(*weight), 1,
			 status );
  } else {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": Model smfData was not loaded!", status);      
  }

  /* Loop over index in subgrp (subarray) */
  for( idx=0; idx<res->ndat; idx++ ) if (*status == SAI__OK ) {
    /* Obtain dimensions of the data */
    smf_get_dims( res->sdata[idx], &thisnbolo, &thisntslice, &thisndata,
                  &bstride, &tstride, status);
      
    if(dat->gai) {
      smf_get_dims( gai->sdata[idx], NULL, NULL, NULL,
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

    /* Get pointer to DATA component of residual */
    res_data = (res->sdata[idx]->pntr)[0];

    /* Geta pointer to the QUAlity array */
    qua_data = (qua->sdata[idx]->pntr)[0];

    if( dat->gai ) {
      gai_data = (gai->sdata[idx]->pntr)[0];
    }

    /* Which QUALITY bits should be checked for correcting samples */
    mask_cor = ~(SMF__Q_JUMP|SMF__Q_SPIKE|SMF__Q_APOD);

    /* Which QUALITY bits should be checked for measuring the mean */
    mask_meas = ~(SMF__Q_JUMP); 

    if( (res_data == NULL) || (model_data == NULL) || (qua_data == NULL) ) {
      *status = SAI__ERROR;
      errRep( "", FUNC_NAME ": Null data in inputs", status);      
    } else {
    
      /* Loop over time slice */
      for( i=0; i<ntslice; i++ ) {
	
	/* Loop over bolometers to put the previous common-mode
	   signal back in at each time-slice, and calculate the sum of
	   all the detectors with good data. */

	lastmean = model_data_copy[i];
	sum = 0;
	base = 0; /* Offset to the start of the j'th bolometer in the buffer */
        thissum = 0; /* Initialize sum to 0 */
        
        for( j=0; j<nbolo; j++ ) {
          if( dat->gai ) {
            /* if GAIn model was fit, there is an additional offset (the gain
               has already been un-done in smf_iteratemap at the end of the
               previous iteration. Offset is stored in the second plane of
               the gain model. Don't use bstride/tstride here because they
               refer to the whole ntslices data cube, but we know things
               are in bolo order. */
            g = 1./gai_data[j*gbstride];
            off = gai_data[j*gbstride + gcstride];
          } else {
            off = 0;
            g = 1;
          }
          
          if( !(qua_data[base+i]&mask_cor) ) {
            res_data[base+i] += g*lastmean+off;
          }
          
          if( !(qua_data[base+i]&mask_meas) ) {
            thissum += res_data[base+i];
            weight[i]++;
          }
          base += ntslice;
        }

        /* Store the sum here. Init if first subarray, otherwise add to
           sum from previous subarrays. */

        if( idx == 0 ) {
          model_data[i] = thissum;
        } else {
          model_data[i] += thissum;
        }
      }
    }
  }
    
  /* Re-normalize the model, or set model to VAL__BADD if no data. */
  for( i=0; i<ntslice; i++ ) {
    if( weight[i] ) {
      model_data[i] /= weight[i];
    } else {
      model_data[i] = VAL__BADD;
    }
  }

  /* boxcar smooth if desired */
  if( do_boxcar ) {

    if( do_boxfact ) {
      if( !astMapGet0D( keymap, "COM_BOXCARD", &boxcard) ) {
	*status = SAI__ERROR;
	errRep( "", FUNC_NAME ": Failed to retrieve COM_BOXCARD from keymap", 
	       status);
      } else {
	/* Use damped boxcar for smoothing width */
	boxcar = (int) boxcard;
      }

    } 

    msgSeti("BOX",boxcar);
    msgOutif(MSG__VERB, " ", "    boxcar width ^BOX",status);

    /* Do the smooth */
    smf_boxcar1D( model_data, ntslice, boxcar, NULL, 0, status );

    /* If damping, apply it here */
    if( do_boxfact && (*status == SAI__OK) ) {
      boxcard = boxcard * boxfact;

      /* Enforce minimum if available */
      if( do_boxmin ) {
	if( boxcard < boxmin ) boxcard = (double) boxmin;
      }

      /* Update value in the keymap */
      astMapPut0D( keymap, "COM_BOXCARD", boxcard, NULL );
    }

  }

  /* remove common mode from residual */
  for( idx=0; idx<res->ndat; idx++ ) if (*status == SAI__OK ) {
    smf_get_dims( res->sdata[idx], NULL, NULL, NULL, &bstride, &tstride, 
                  status );

    /* Get pointer to DATA component of residual */
    res_data = (double *)(res->sdata[idx]->pntr)[0];
      
    /* Geta pointer to the QUAlity array */
    qua_data = (unsigned char *)(qua->sdata[idx]->pntr)[0];
    
    if( dat->gai ) {
      smf_get_dims( gai->sdata[idx], NULL, NULL, NULL,
                    &gbstride, &gcstride, status);
      gai_data = (gai->sdata[idx]->pntr)[0];

      /* If GAIn model supplied, fit gain/offset of sky model to
         each bolometer independently. It is stored as 3 planes of
         nbolo samples: the first holds bolo gains, the second offsets, and
         the third correlation coefficients. */
      
      for( i=0; (*status==SAI__OK) && (i<nbolo); i++ ) {
        if( !(qua_data[i*bstride]&SMF__Q_BADB) ) {          
          smf_templateFit1D( res_data+i*bstride, qua_data+i*bstride, 
                             mask_cor, ntslice, tstride, model_data, 1,
                             gai_data+i*gbstride, gai_data+gcstride+i*gbstride, 
                             gai_data+2*gcstride+i*gbstride, status );
          
          /* The calculated gain is the fit of the common mode to the data.
             Calculate its inverse which gives the correction of the bolometer
             to look like the common-mode signal */
          if( (gai_data[i*gbstride] != VAL__BADD) && (gai_data[i*gbstride]>0) ){
            gai_data[i*gbstride] = 1./gai_data[i*gbstride];
          }

          /* Now scale the data by this gain correction */
          if( *status==SAI__OK ) for( j=0; j<ntslice; j++ ) {
              if( !(qua_data[i*bstride + j*tstride]&mask_cor) ) {
                res_data[i*bstride + j*tstride] *= gai_data[i*gbstride];
              }
            }
        }
      }
    } else {
      /* Otherwise assume a straight common-mode for all of the detectors */    
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
  
  /* Clean up */
  if( weight) 
    weight = smf_free( weight, status );
  if( model_data_copy ) 
    model_data_copy = smf_free( model_data_copy, status );
}
