/*
*+
*  Name:
*     smf_calcmodel_two

*  Purpose:
*     Calculate two-component common-mode when relative gains known

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_calcmodel_two( ThrWorkForce *wf, smfDIMMData *dat, int
*			 chunk, AstKeyMap *keymap, smfArray
*			 **allmodel, int flags, int *status)

*  Arguments:
*     wf = ThrWorkForce * (Given)
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
*     In addition to the dark signals detected by the dark squids
*     (e.g.  magnetic field pickup), there are optical and thermal
*     loads on the bolometers. The dominant changing thermal load is
*     the large-amplitude fridge oscillations which have a period in
*     the range ~20--30s. Optical loading is, in theory, dominated by
*     sky variations, although there could also be optical loading
*     from within the cryostat (e.g. from the FPU). There is some
*     evidence that the relative responses to thermal and optical loads
*     varies across the focal plane, comparing the amplitudes of
*     astronomical calibrators in single-bolo maps to fridge oscillations
*     measured with the shutter close.
*
*     We therefore model the optical and thermal signals detected by the
*     i'th bolometer as
*
*        B_i(t) = a_i*O(t) + b_i*T(t)
*
*     where the a_i and b_i are the amplitudes of the optical, O(t),
*     and thermal, T(t), signals over time.
*
*     If the a_i and b_i are measured in advance, we can estimate O(t)
*     and T(t) as a linear combination of the bolometer data,
*     B_i(t). The only requirement is that we have at least two
*     working bolometers to constrain the solution.
*
*     First some useful functions of the a_i and b_i to simplify the
*     final solution:
*
*             D = [sum(a_i^2)*sum(b_i^2) - (sum(a_i*b_i)^2]
*         alpha = [1/sum(a_i^2)] * [1 + sum(a_i*b_i)/D]
*          beta = -sum(a_i*b_i)/D
*         gamma = sum(a_i^2)/D
*
*     We then calculate our estimates of O(t) and T(t) using the
*     follwing linear combinations of the bolometer data:
*
*          O(t) = alpha*sum(a_i*B_i(t)) +  beta*sum(b_i*B_i(t))
*          T(t) =  beta*sum(a_i*B_i(t)) + gamma*sum(b_i*B_i(t))
*
*     Once O(t) and T(t) are estimated, they are scaled by the a_i and
*     b_i respectively and subtracted from the data.

*  Notes:

*  Authors:
*     EC: Ed Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2010-06-07 (EC):
*        Initial version, loosely based on smf_calcmodel_dks
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2010 University of British Columbia.
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
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

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

#define FUNC_NAME "smf_calcmodel_two"

void smf_calcmodel_two( ThrWorkForce *wf __attribute__((unused)),
                        smfDIMMData *dat, int chunk, AstKeyMap *keymap,
                        smfArray **allmodel, int flags __attribute__((unused)),
                        int *status) {

  /* Local Variables */
  double *a=NULL;               /* pointer to coefficients 1st component */
  double *b=NULL;               /* pointer to coefficients 2nd component */
  dim_t bstride;               /* bolo stride */
  double *ccomp=NULL;           /* component time series pointer */
  double *ccoeff=NULL;          /* component bolo coeffs pointer */
  double *comp=NULL;            /* component time series pointer */
  double *coeff=NULL;           /* component bolo coeffs pointer */
  double dchisq=0;              /* this - last model residual chi^2 */
  dim_t i;                     /* Loop counter */
  dim_t idx=0;                  /* Index within subgroup */
  dim_t index;                  /* index into data buffer */
  dim_t j;                      /* Loop counter */
  dim_t jt1;
  dim_t jt2;
  dim_t jf1;                   /* Starting tslice that should be fit */
  dim_t jf2;                   /* Final tslice that should be fit */
  dim_t k;                      /* Loop counter */
  AstKeyMap *kmap=NULL;         /* Pointer to PLN-specific keys */
  smfArray *model=NULL;         /* Pointer to model at chunk */
  double *model_data=NULL;      /* Pointer to DATA component of model */
  double *model_data_copy=NULL; /* Copy of model_data for one bolo */
  dim_t nbolo=0;                /* Number of bolometers */
  dim_t ndata=0;                /* Total number of data points */
  dim_t ndchisq=0;             /* number of elements contributing to dchisq */
  dim_t nmodel=0;               /* Total number of elements in model buffer */
  smfArray *noi=NULL;           /* Pointer to NOI at chunk */
  double *noi_data=NULL;        /* Pointer to DATA component of model */
  dim_t noibstride;            /* bolo stride for noise */
  dim_t nointslice;             /* number of time slices for noise */
  dim_t noitstride;            /* Time stride for noise */
  dim_t ntslice=0;              /* Number of time slices */
  smfArray *qua=NULL;           /* Pointer to QUA at chunk */
  smf_qual_t *qua_data=NULL; /* Pointer to quality data */
  smfArray *res=NULL;           /* Pointer to RES at chunk */
  double *res_data=NULL;        /* Pointer to DATA component of res */
  dim_t tstride;               /* Time slice stride in data array */

  /* Main routine */
  if (*status != SAI__OK) return;

  /* Obtain pointer to sub-keymap containing PLN parameters. Something will
     always be available.*/
  astMapGet0A( keymap, "TWO", &kmap );

  /* Obtain pointers to relevant smfArrays for this chunk */
  res = dat->res[chunk];
  qua = dat->qua[chunk];
  model = allmodel[chunk];

  if(dat->noi) {
    noi = dat->noi[chunk];

    nmodel = (model->sdata[0])->dims[0] * (model->sdata[0])->dims[1];

    model_data_copy = astCalloc( nmodel, sizeof(*model_data_copy) );
  }

  /* Assert bolo-ordered data */
  smf_model_dataOrder( wf, dat, allmodel, chunk, SMF__RES|SMF__QUA|SMF__NOI, 0,
                       status );

  smf_get_dims( res->sdata[0],  NULL, NULL, NULL, &ntslice,
                &ndata, NULL, NULL, status);


  /* Loop over index in subgrp (subarray) */
  for( idx=0; (*status==SAI__OK)&&(idx<res->ndat); idx++ ) {

    /* Obtain dimensions of the data */
    smf_get_dims( res->sdata[idx], NULL, NULL, &nbolo, &ntslice, &ndata,
                  &bstride, &tstride, status);

    /* Get pointer to DATA component of residual */
    res_data = (res->sdata[idx]->pntr)[0];

    /* Get pointer to DATA components of the model */
    model_data = (model->sdata[idx]->pntr)[0];

    /* Geta pointer to the QUAlity array */
    qua_data = (qua->sdata[idx]->pntr)[0];

    /* Identify the range of data for chi^2 calc using SMF__Q_BOUND (i.e.
       exclude the apodized region) */
    if( qua ) {
      smf_get_goodrange( qua_data, ntslice, tstride, SMF__Q_BOUND, &jf1, &jf2,
                         status );
    } else {
      jf1 = 0;
      jf2 = ntslice-1;
    }

    /* Total range using SMF__Q_PAD to avoid causing discontinuities when
       we fit/remove two-component common-mode (i.e. include apodized
       region) */
    if( qua ) {
      smf_get_goodrange( qua_data, ntslice, tstride, SMF__Q_PAD, &jt1, &jt2,
                         status );
    } else {
      jt1 = 0;
      jt2 = ntslice-1;
    }

    if( (res_data == NULL) || (model_data == NULL) || (qua_data == NULL) ) {
      *status = SAI__ERROR;
      errRep("", FUNC_NAME ": Null data in inputs", status);
    } else {

      /* If this isn't first iteration, put the previous iteration
         back into the signal */
      if( !(flags&SMF__DIMM_FIRSTITER) ) {

        /* Loop over component */
        for( i=0; (*status==SAI__OK)&&(i<2); i++ ) {

          /* get pointer to component time series and bolo coeffs */
          comp = model_data;
          comp += i*(ntslice+nbolo);
          coeff = comp + ntslice;

          /* Loop over bolometer */
          for( j=0; (*status==SAI__OK)&&(j<nbolo); j++ ) {

            /* index to start of the j bolo time series in data */
            index = j*bstride;

            /* Continue if the bolo is OK */
            if( !(qua_data[index]&SMF__Q_BADB) && (coeff[j]!=VAL__BADD) ) {

              for( k=jt1; k<=jt2; k++ ) {
                if( (res_data[index+k*tstride]!=VAL__BADD) &&
                    (comp[k]!=VAL__BADD) ) {
                  res_data[index+k*tstride] += comp[k]*coeff[j];
                }
              }
            }
          }
        }
      }

      /* Make a copy of the model to check convergence */
      if( noi ) {
        memcpy( model_data_copy, model_data, nmodel*sizeof(*model_data_copy) );
      }

      /* Then re-fit and remove the components */
      msgOutif( MSG__VERB, "", "   re-fitting components", status );

      if( *status == SAI__OK ) {
        double d;
        double s_asq=0;
        double s_bsq=0;
        double s_ab=0;
        double denom=0;
        double alpha=0;
        double beta=0;
        double gamma=0;
        double *compa, *compb;
        double v1, v2;

        compa = model_data;
        a = compa + ntslice;
        compb = model_data + ntslice + nbolo;
        b = compb + ntslice;

        for( i=0; i<nbolo; i++ ) {

          /* Only calculate sum for working detectors */
          if( !(qua_data[i*bstride]&SMF__Q_BADB) ) {
            if( (a[i] == VAL__BADD) || (b[i] == VAL__BADD) ) {
              /* If we have any working bolometers matched to a bad gain map
                 value we need to turn off that detector as well */
              for( j=0; j<ntslice; j++ ) {
                qua_data[i*bstride+j*tstride] |= SMF__Q_BADB;
              }
            } else {
              s_asq += a[i]*a[i];
              s_bsq += b[i]*b[i];
              s_ab += a[i]*b[i];
            }
          }
        }

        denom = s_asq*s_bsq - s_ab*s_ab;
        alpha = (1/s_asq)*(1 + s_ab*s_ab/denom);
        beta = -s_ab/denom;
        gamma = s_asq/denom;

        /* Note that with the formulation we have used we need to
           calculate v1 and v2 at every time slice using _all_ working
           bolometers -- this is for consistency with the
           normalization established in the above quantities alpha,
           beta and gamma. We can't start dropping individual samples
           in some detectors using SMF__Q_FIT without calculating
           different a alpha, beta and gamma for each time slice. */

        for( i=0; i<ntslice; i++ ) {
          v1 = 0;
          v2 = 0;
          for( j=0; j<nbolo; j++ ) {
            if( !(qua_data[j*bstride]&SMF__Q_BADB) ) {
              index = i*tstride + j*bstride;
              d = res_data[index];
              v1 += a[j]*d;
              v2 += b[j]*d;
            }
          }
          compa[i] = alpha*v1 + beta*v2;
          compb[i] = beta*v1 + gamma*v2;
        }

        /* Now remove model from the residual. Loop over component */
        for( i=0; (*status==SAI__OK)&&(i<2); i++ ) {

          /* get pointer to component time series and bolo coeffs */
          comp = model_data;
          comp += i*(ntslice+nbolo);
          coeff = comp + ntslice;

          /* Loop over bolometer */
          for( j=0; (*status==SAI__OK)&&(j<nbolo); j++ ) {

            /* index to start of the j bolo time series in data */
            index = j*bstride;

            /* Continue if the bolo is OK */
            if( !(qua_data[index]&SMF__Q_BADB) ) {
              for( k=jt1; k<=jt2; k++ ) {
                res_data[index+k*tstride] -= comp[k]*coeff[j];
              }
            }
          }
        }
      }

      /* How has the model changed? */
      if( noi ) {

        smf_get_dims( noi->sdata[idx],  NULL, NULL, NULL, &nointslice,
                      NULL, &noibstride, &noitstride, status);
        noi_data = (double *)(noi->sdata[idx]->pntr)[0];

        /* Loop over component */
        for( i=0; (*status==SAI__OK)&&(i<2); i++ ) {

          /* get pointer to component time series and bolo coeffs, for
             both the new model and old one to which we are comparing */
          comp = model_data;
          comp += i*(ntslice+nbolo);
          coeff = comp + ntslice;

          ccomp = model_data_copy;
          ccomp += i*(ntslice+nbolo);
          ccoeff = ccomp + ntslice;

          /* Loop over bolometer */
          for( j=0; (*status==SAI__OK)&&(j<nbolo); j++ ) {

            /* index to start of the j bolo time series in data */
            index = j*bstride;

            /* Continue if the bolo is OK */
            if( !(qua_data[index]&SMF__Q_BADB) && (coeff[j]!=VAL__BADD) ) {

              for( k=jf1; k<=jf2; k++ ) {

                if( !(qua_data[j*bstride+k*tstride]&SMF__Q_GOOD) ) {
                  dchisq += (comp[k]*coeff[j] - ccomp[k]*ccoeff[j]) *
                    (comp[k]*coeff[j] - ccomp[k]*ccoeff[j]) /
                    noi_data[j*noibstride + (k%nointslice)*noitstride];
                  ndchisq++;
                }
              }
            }
          }
        }
      }
    }
  }

  /* Print normalized residual chisq for this model */
  if( (*status==SAI__OK) && noi && (ndchisq>0) ) {
    dchisq /= (double) ndchisq;
    msgOutiff( MSG__VERB, "", "    normalized change in model: %lf", status,
               dchisq );
  }

  if( kmap ) kmap = astAnnul( kmap );
  model_data_copy = astFree( model_data_copy );

}
