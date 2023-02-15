/*
*+
*  Name:
*     smf_clean_dksquid

*  Purpose:
*     Clean raw smfData by removing a scaled, offset version of the dark squid

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     smf_clean_dksquid( smfData *indata, smf_qual_t mask, dim_t window, smfData *model,
*                        int calcdk, int nofit, int replacebad, int *status ) {

*  Arguments:
*     indata = smfData * (Given)
*        Pointer to the input smfData. Should be raw, un-flatfielded.
*     mask = smf_qual_t (Given)
*        Use to define which bits in quality are relevant to ignore indata
*     window = dim_t (Given)
*        Width of boxcar smooth for squid before fitting and removing
*     model = smfData * (Given)
*        If supplied, dark squids stored in model created by smf_model_create
*     calcdk = int (Given)
*        If set, calc & store dark squids in model along with fit coefficients
*     nofit = int (Given)
*        If set, just obtain smoothed dark squid signals and don't fit/remove
*     replacebad = int (Given)
*        If set, replace dead squids in model with average of working ones(Given)
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Return Value:

*  Description:
*     Columns of SCUBA-2 detectors exhibit a strong correlated signal
*     which is shown clearly in the dark squids. As the dark squids do
*     not see astronomical or atmospheric signal they can be used as a
*     template to remove this correlated noise. The dark squids are
*     smoothed to avoid increasing the white noise level of detectors
*     once they are subtracted. They are fit to the raw
*     (un-flatfielded) detector by solving for a gain and offset using
*     the explicit least-squares solution. If a dark squid is constant
*     this routine flags every detector in the column as SMF__Q_BADB.
*
*     If a model smfData created with smf_model_create (SMF__DKS) is
*     supplied, it can be used to override obtaining the dark squid
*     signals from the DA extension of indata (calcdk=0 case), or
*     alternatively it can be used to store the smoothed dark squid
*     signals for future use (calcdk=1 case). In either case, if it is
*     supplied the fitted gain and offset coefficients for each
*     detector in the columns are stored. The step of fitting /
*     removing templates can also be skipped for the purpose of
*     initializing models only by setting the nofit flag.

*  Notes:

*  Authors:
*     Ed Chapin (UBC)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2008-08-27 (EC):
*        Initial version
*     2008-09-11 (EC):
*        -flag SMF__Q_BADB all bolos in column if dark squid is dead
*        -fixed array index bug
*     2008-10-01 (EC):
*        -fixed logic bugs
*        -added mask for quality
*        -flag bolo as bad if fit failed
*     2009-01-06 (EC):
*        Stride-ify
*     2009-07-23 (TIMJ):
*        Use msgFlevok rather than msgIflev
*     2010-05-14 (EC):
*        - add "replacebad" to replace dead DKsquids with average of working ones
*        - remove means of DKsquids
*     2010-05-28 (EC):
*        Report number of new BADBOL if replacebad=0
*     2010-06-01 (EC):
*        Only fit gain of DKS, not gain and offset (to avoid COM offset degen.)
*     2010-06-10 (EC):
*        Don't remove the means here since that is handled by smf_clean_smfData
*     2011-03-28 (DSB):
*        Check for VAL__BADD when checking if dark squid ever changes.

*  Copyright:
*     Copyright (C) 2009-2011 Science & Technology Facilities Council.
*     Copyright (C) 2008-2010 University of British Columbia.
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
#include "ast.h"
#include "sae_par.h"
#include "mers.h"
#include "ndf.h"
#include "prm_par.h"

/* SMURF routines */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"

#define FUNC_NAME "smf_clean_dksquid"

void smf_clean_dksquid( smfData *indata, smf_qual_t mask, dim_t window, smfData *model,
                        int calcdk, int nofit, int replacebad, int *status ) {

  dim_t b;                /* Bolometer index */
  dim_t bstride;         /* Bolometer index stride */
  double corr;            /* Linear correlation coefficient */
  double *corrbuf=NULL;   /* Array of correlation coeffs all bolos this col */
  int needDA=0;           /* Do we need dksquids from the DA? */
  int *dkgood=NULL;       /* Flag for non-constant dark squid */
  double *dksquid=NULL;   /* Buffer for smoothed dark squid */
  double *dkav=NULL;      /* Buffer for average dark squid */
  double firstdk;         /* First value in dksquid signal */
  double gain;            /* Gain parameter from template fit */
  double *gainbuf=NULL;   /* Array of gains for all bolos in this col */
  dim_t i;               /* Loop counter */
  dim_t jt1;
  dim_t jt2;
  dim_t jf1;             /* Starting tslice that should be fit */
  dim_t jf2;             /* Final tslice that should be fit */
  dim_t j;               /* Loop counter */
  dim_t k;               /* Loop counter */
  dim_t nbad=0;          /* Number of new bad bolos due to bad dark squid */
  dim_t nbolo;            /* Number of bolometers */
  dim_t ncol;             /* Number of columns */
  dim_t ndata;            /* Number of data points */
  dim_t nfit;            /* number of samples over good range to fit */
  dim_t ngood=0;         /* number of good dark squids */
  dim_t nrow;             /* Number of rows */
  dim_t ntot;
  dim_t ntslice;          /* Number of time slices */
  double offset;          /* Offset parameter from template fit */
  double *offsetbuf=NULL; /* Array of offsets for all bolos in this col */
  int pass;               /* two passes over data to get estimate of average */
  smf_qual_t *qua=NULL;/* Pointer to quality array */
  dim_t tstride;         /* Time slice index stride */

  if (*status != SAI__OK) return;

  /* Check for NULL smfData pointer */
  if( !indata ) {
    *status = SAI__ERROR;
    errRep( " ", FUNC_NAME
            ": possible programming error, smfData pointer is NULL", status );
    return;
  }

  /* Decide if we need the DA extension or not */
  if( (!model) || (model && calcdk) ) {
    needDA = 1;
    if( !indata->da) {
      /* Check for NULL smfDA */
      *status = SAI__ERROR;
      errRep( " ", FUNC_NAME
              ": possible programming error, no smfDA struct in smfData",
              status);
      return;
    } else if( !indata->da->dksquid) {
      /* Check for NULL dksquid */
      *status = SAI__ERROR;
      errRep( " ", FUNC_NAME
              ": possible programming error, no dksquid array in smfData",
              status);
      return;
    }

    /* Assert the correct data order here */
    smf_dataOrder( NULL, indata->da->dksquid, 1, status );
  }

  /* Check for 3-d data and get dimensions */
  smf_get_dims( indata, &nrow, &ncol, &nbolo, &ntslice, &ndata, &bstride,
                &tstride, status );

  /* Identify the range of data that should be fit using SMF__Q_BOUND */
  if( qua ) {
    smf_get_goodrange( qua, ntslice, tstride, SMF__Q_BOUND, &jf1, &jf2,
                       status );
  } else {
    jf1 = 0;
    jf2 = ntslice-1;
  }
  nfit = jf2-jf1+1;

  /* Total total range only using SMF__Q_PAD */
  if( qua ) {
    smf_get_goodrange( qua, ntslice, tstride, SMF__Q_BOUND, &jt1, &jt2,
                       status );
  } else {
    jt1 = 0;
    jt2 = ntslice-1;
  }
  ntot = jt2-jt1+1;

  if( model ) {
    /* Check for valid model dimensions if supplied */
    if( model->dtype != SMF__DOUBLE ) {
      msgSetc("DT", smf_dtype_str(model->dtype, status) );
      *status = SAI__ERROR;
      errRep(" ", FUNC_NAME ": Data type ^DT for model not supported.",
             status );
      return;
    }

    if( (model->ndims != 2) ||
        (model->dims[0] != ntslice+nrow*3) ||
        (model->dims[1] != ncol) ) {
      *status = SAI__ERROR;
      errRep(" ", FUNC_NAME ": model has incorrect dimensions", status );
      return;
    }
  } else {
    /* Otherwise allocate space for local dksquid buffer */
    dksquid = astCalloc( ntslice, sizeof(*dksquid) );
  }

  /* Pointer to quality */
  qua = smf_select_qualpntr( indata, 0, status );

  /* Two passes: in the first we calculate an average dark squid to use as
     a surrogate for columns with dead dark squids. In the second we do the
     actual cleaning etc. */

  dkgood = astCalloc( ncol, sizeof(*dkgood) );
  dkav = astCalloc( ntslice, sizeof(*dkav) );

  for( pass=0; (*status==SAI__OK)&&(pass<2); pass++ ) {

    /* Loop over columns */
    for( i=0; (*status==SAI__OK)&&(i<ncol); i++ ) {

      /* Point dksquid, gainbuf, offsetbuf and corrbuf to the right
         place in model if supplied. */
      if( model ) {
        dksquid = model->pntr[0];
        dksquid += i*(ntslice+nrow*3);
        gainbuf = dksquid + ntslice;
        offsetbuf = gainbuf + nrow;
        corrbuf = offsetbuf + nrow;
      }

      /* First pass is just to copy the dark squid over to the model and replace
         dead dark squids with the average */
      if( pass==0 ) {

        /* Copy dark squids from the DA extension into dksquid */
        if( needDA && calcdk && model ) {
          double *ptr = indata->da->dksquid->pntr[0];
          for( j=0; j<ntslice; j++ ) {
            dksquid[j] = ptr[i+ncol*j];
          }
        }

        /* Check for a good dark squid by seeing if it ever changes */
        firstdk = VAL__BADD;
        for( j=jt1; j<=jt2; j++ ) {
          if(  dksquid[j] != VAL__BADD ) {
            if( firstdk == VAL__BADD ) {
              firstdk = dksquid[j];
            } else if( dksquid[j] != firstdk ) {
              dkgood[i] = 1;
              ngood++;

              /* Add good squid to average dksquid */
              for( k=jt1; k<=jt2; k++ ) {
                dkav[k] += dksquid[k];
              }
              break;
            }
          }
        }
      }

      /* Second pass actually do the fitting / replace with average dksquid if
         dksquid was dead */
      if( pass==1 ) {

        /* Do some dksquid initialization if requested  */
        if( (*status==SAI__OK) && needDA && calcdk && model && dkgood[i] ) {
          /* Smooth the dark squid template */
          smf_boxcar1D( &dksquid[jt1], ntot, 1, window, NULL, 0, 1, NULL, status );
        }

        /* Initialize fit coeffs to VAL__BADD */
        if( (*status == SAI__OK) && model ) {
          for( j=0; j<nrow; j++ ) {
            gainbuf[j] = VAL__BADD;
            offsetbuf[j] = VAL__BADD;
            corrbuf[j] = VAL__BADD;
          }
        }

        /* Loop over rows, removing the fitted dksquid template. */
        for( j=0; (!nofit) && (*status==SAI__OK) && (j<nrow); j++ ) {

          /* Calculate bolometer index from row/col counters */
          if( SC2STORE__COL_INDEX ) {
            b = i*nrow + j;
          } else {
            b = i + j*ncol;
          }

          /* If dark squid is bad, flag entire bolo as bad if it isn't already */
          if( !dkgood[i] && qua && !(qua[b*bstride]&SMF__Q_BADB) ) {
            nbad++;
            for( k=0; k<ntslice; k++ ) {
              qua[b*bstride+k*tstride] |= SMF__Q_BADB;
            }
          }

          /* Try to fit if we think we have a good dark squid and bolo, and only
             the goodrange of data (excluding padding etc.) */
          if((!qua && dkgood[i]) ||
             (qua && dkgood[i] && !(qua[b*bstride]&SMF__Q_BADB))) {
            double *d_d;
            int *d_i;

            switch( indata->dtype ) {
            case SMF__DOUBLE:
              d_d = (double *) indata->pntr[0];
              smf_templateFit1D( &d_d[b*bstride+jf1*tstride],
                                 &qua[b*bstride+jf1*tstride], NULL, NULL,
                                 mask, mask, nfit, tstride, &dksquid[jf1], 1,
                                 1, &gain, &offset, &corr, status );
              break;

            case SMF__INTEGER:
              d_i = (int *) indata->pntr[0];
              smf_templateFit1I( &d_i[b*bstride+jf1*tstride],
                                 &qua[b*bstride+jf1*tstride], NULL, NULL, mask,
                                 mask, nfit, tstride, &dksquid[jf1], 1, 1,
                                 &gain, &offset, &corr, status );
              break;

            default:
              msgSetc( "DT", smf_dtype_string( indata, status ));
              *status = SAI__ERROR;
              errRep( " ", FUNC_NAME
                      ": Unsupported data type for dksquid cleaning (^DT)",
                      status );
            }

            if( *status == SMF__INSMP || *status == SMF__DIVBZ ) {
              int wasinsmp = (*status == SMF__INSMP ? 1 : 0 );
              /* Annul SMF__INSMP as it was probably due to a bad bolometer */
              errAnnul( status );
              msgOutiff( MSG__DEBUG, "", FUNC_NAME
                         ": ROW,COL (%zu,%zu) %s", status, j, i,
                         (wasinsmp ? "insufficient good samples" : "division by zero" ));

              /* Flag entire bolo as bad if it isn't already */
              if( qua && !(qua[b*bstride]&SMF__Q_BADB) ) {
                for( k=0; k<ntslice; k++ ) {
                  qua[b*bstride+k*tstride] |= SMF__Q_BADB;
                }
              }
            } else {
              /* Store gain and offset in model */
              if( model ) {
                gainbuf[j] = gain;
                offsetbuf[j] = offset;
                corrbuf[j] = corr;
              }

              if( msgFlevok( MSG__DEBUG1, status ) ) {
                msgSetk( "COL", i );
                msgSetk( "ROW", j );
                msgSetd( "GAI", gain );
                msgSetd( "OFF", offset );
                msgSetd( "CORR", corr );
                msgOutif( MSG__DEBUG1, "", FUNC_NAME
                          ": ROW,COL (^ROW,^COL) GAIN,OFFSET,CORR "
                          "(^GAI,^OFF,^CORR)", status );
              }
            }
          }
        }
      }
    }

    /* Re-normalize the average dark-squid here at the end of pass 0 */
    if( (pass==0) && (ngood) && (*status==SAI__OK) ) {
      for( j=jt1; j<=jt2; j++ ) {
        dkav[j] /= ngood;
      }
    }

    /* Replace bad bolos in model with the average? */
    if( replacebad && calcdk && needDA && model && (*status==SAI__OK) ) {
      for( i=0; i<ncol; i++ ) {
        dksquid = model->pntr[0];
        dksquid += i*(ntslice+nrow*3);

        if( !dkgood[i] ) {
          memcpy( dksquid, dkav, sizeof(*dksquid)*ntslice );
          dkgood[i] = 1;
        }
      }
    }
  }

  /* Report number of new bad bolos that were flagged */
  if( !replacebad && nbad ) {
    msgOutiff( MSG__VERB, "", FUNC_NAME
               ": %zu new bolos flagged bad due to dead DKS", status, nbad );
  }

  /* Free dksquid only if it was a local buffer */
  if( !model && dksquid ) dksquid = astFree( dksquid );

  dkgood = astFree( dkgood );
  dkav = astFree( dkav );

}
