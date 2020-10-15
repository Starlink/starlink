/*
*+
*  Name:
*     smf_calcmodel_com_old

*  Purpose:
*     Calculate the COMmon-mode model signal component (old version)

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_calcmodel_com_old( ThrWorkForce *wf, smfDIMMData *dat, int
*                            chunk, AstKeyMap *keymap, smfArray
*			     **allmodel, int flags, int *status)

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
*     Calculate the common-mode signal measured at every time-slice.
*
*     This contains the old lagorithm that rejectes whole blocks of time
*     slices from unusual bolometers, thus causing discontinuities at the
*     block boundsaries. It is retained until such time as the new
*     algorithm (implemented insmf_calcmodel_com, based on sigma-clipping
*     the boloemeter values at each tgime slice independently) has had
*     more extensive verification.

*  Notes:

*  Authors:
*     Edward Chapin (UBC)
*     David Berry (JAC)
*     Tim Jenness (JAC)
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
*     2009-09-30 (EC)
*        Measure normalized change in model between iterations (dchisq)
*     2009-10-25 (EC)
*        Add back in option of using commod-mode to flatfield data
*     2009-11-24 (EC)
*        - bad bolo flagging from common-mode fit can be controlled with
*          config params: corr_tol, gain_tol, gain_abstol
*        - only throw out bolos with correlation coeffs that are *lower*
*     2009-11-30 (EC)
*        - optionally delay calculation until after 1st iteration (notfirst)
*     2010-02-10 (DSB)
*        - Changed to fit a separate gain and offset to each block of
*        "gain_box" time slices, rather than fitting a single gain and
*        offset to the whole time stream.
*     2010-02-18 (DSB):
*        - Changed the default for GAIN_BOX from 2000 to 6000.
*        - Put a lower absolute limit (0.2) on acceptable correlation
*          coefficients.
*        - The initial guess at the gain of a bolometer used to be 1.0. It is
*          now the RMS value of the bolometer data stream.
*     2010-02-23 (DSB):
*        - If there is a short block at the end of a time stream, include it in
*        the penultimate block. This means the last block will be between
*        gain_box and 2*gain_box in length.
*        - Use only unflagged data when calculating the fit between residuals
*        and common mode.
*        - Reject blocks that have anomolous gains when compared to the other
*        blocks for the same bolometer.
*        - Reject entire bolometers if they have too few good blocks.
*     2010-02-25 (TIMJ):
*        Fix 32-bit incompatibility.
*        Remove C90 incompatibility by not mixing declarations with code after
*        a block has started.
*     2010-02-26 (DSB):
*        - Fix problem with short scans containing fewer than "gain_box"
*        time slices.
*     2010-03-19 (EC):
*        Use new SMF__Q_COM flag instead of SMF__Q_BADS for clarity.
*     2010-04-01 (DSB):
*        - Check for VAL__BADD in the supplied model array.
*        - Avoid some "type-punned" compiler warnings.
*     2010-04-09 (DSB):
*        - Prevent segvio if no gain model is supplied.
*     2010-05-04 (TIMJ):
*        Simplify KeyMap access. We now trigger an error if a key is missing
*        and we ensure all keys have corresponding defaults.
*     2010-05-12 (DSB):
*        Added com.gain_is_one. Setting this to one causes all
*        bolometer gains to be forced to 1.0 (and offsets to 0.0),
*        but retains the real correlation coefficient so that bad
*        bolometer blocks are still identified and rejected.
*     2010-05-13 (DSB):
*        - Added com.fit_box. Specifies the length (in time slices) over
*        which the least squres fit (used to determine each gain and
*        offset) is performed. FIT_BOX defaults to the value supplied for
*        GAIN_BOX.
*        - Speed up this function by only re-calculating gains and offsets
*        for blocks that have not already converged.
*     2010-06-09 (DSB):
*        - If com.gain_is_one is set, force gains to unity on iteration 1.
*     2010-06-10 (DSB):
*        Use smf_tophat1D to smooth the common mode, rather than
*        smf_boxcar1D (which can result in dicontinuities at the start
*        and end of the smoothed common mode).
*     2010-06-11 (DSB):
*        Display the number of bolometers rejected for each reason if
*        MSG_FILTER==DEBUG.
*     2010-06-13 (DSB):
*        Added com.offset_is_zero. Setting this to one causes all
*        bolometer offsets to be forced to 0.0. Setting com.gain_is_one
*        to 1 no longer forces offsets to zero.
*     2010-06-16 (DSB):
*        Refactor all the code for finding gains and offsets, and
*        rejecting aberrant bolometers, into smf_find_gains_array.
*     2010-12-14 (DSB):
*        Use sqrt, not sqrtf (sqrtf can cause "inf" gain values for bad
*        data, which then pollute all other gain values).
*     2011-05-11 (DSB):
*        Change COM to be the mean of the remaining bolometer values,
*        without any scaling. Previously, the remaining bolometers were
*        scaled using the inverse of the GAI model, before being added
*        together to form COM, but this introduces a degeneracy between
*        COM and GAI that could cause instabilities (i.e. high GAI*low
*        COM is the same as low GAI*high COM).
*     2010-08-16 (DSB)
*        - Add "perarray" config parameter - if non-zero, a separate COM
*        model is calculated for each subarray.
*     2012-02-23 (DSB):
*        Add facility to exclude masked regions from COM estimate.
*     2012-05-15 (DSB):
*        Renamed as smf_calcmodel_com_old, and deprecated in favour of
*        the new sigma-clipping algorithm.

*  Copyright:
*     Copyright (C) 2006-2010 University of British Columbia.
*     Copyright (C) 2010-2012 Science and Technology Facilities Council.
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

/* System includes */
#include <math.h>

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
   thread will process */
typedef struct smfCalcmodelComData {
  dim_t b1;               /* Index of first bolometer of block */
  dim_t b2;               /* Index of last bolometer of block */
  dim_t bstride;          /* bolometer stride for res/qua */
  double *gai_data;        /* pointer to gain model (can be NULL) data */
  dim_t gain_box;          /* Number of time slices per block */
  dim_t gbstride;         /* gain bolo stride */
  dim_t gcstride;         /* gain coefficient stride */
  int gflat;               /* correct flatfield using GAI */
  dim_t idx;               /* Index within subgroup */
  int ijob;                /* Job identifier */
  int *lut_data;           /* Array holding themap index for each sample */
  unsigned char *mask;    /* Pointer to 2D mask map */
  double *model_data;      /* pointer to common mode data */
  dim_t nblock;            /* No of time slice blocks per bolometer */
  dim_t nbolo;             /* number of bolometers */
  int nogains;             /* Force all gains to unity? */
  dim_t ntslice;           /* number of time slices */
  int operation;           /* 0=undo COM, 1=new COM, 2=fit COM */
  double *res_data;        /* Pointer to common residual data */
  dim_t t1;               /* Index of first timeslice of block */
  dim_t t2;               /* Index of last timeslice of block */
  dim_t tstride;          /* time stride for res/qua */
  smf_qual_t *qua_data;    /* Pointer to common quality data */
  double *weight;          /* Weight at each point in model */
} smfCalcmodelComData;



/* Function to be executed in thread: task depends on value of operation */

void smfCalcmodelComPar( void *job_data_ptr, int *status );

void smfCalcmodelComPar( void *job_data_ptr, int *status ) {
  dim_t bstride;          /* bolometer stride for res/qua */
  double *gai_data;        /* pointer to gain model (can be NULL) data */
  double gain;             /* Gain value */
  dim_t gain_box;          /* Nominal number of time slices per block */
  dim_t gbstride;         /* gain bolo stride */
  dim_t gcstride;         /* gain coefficient stride */
  dim_t i;                /* Loop counter */
  dim_t idx;               /* Index within subgroup */
  dim_t j;                /* Loop counter */
  int *lut_data;           /* Array holding themap index for each sample */
  unsigned char *mask;     /* Pointer to 2D mask map */
  double *model_data;      /* pointer to common mode data */
  dim_t nbolo;             /* number of bolometers */
  dim_t nblock;            /* Number of time blocks */
  int nogains;             /* Force all gains to unity? */
  dim_t nsum;              /* Number of values summed in "sum" */
  dim_t ntslice;           /* number of time slices */
  smfCalcmodelComData *pdata=NULL; /* Pointer to job data */
  double *res_data;        /* Pointer to common residual data */
  double sum;              /* Running sum of values */
  dim_t tstride;          /* time stride for res/qua */
  smf_qual_t *qua_data;    /* Pointer to common quality data */
  double *weight=NULL;     /* Weight at each point in model */
  double *wg;              /* Work array holding gain values */
  double *woff;            /* Work array holding offset values */

  struct timeval tv1;      /* Timers */
  struct timeval tv2;      /* Timers */


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
  model_data = pdata->model_data; /* Careful! */
  nbolo = pdata->nbolo;
  ntslice = pdata->ntslice;
  res_data = pdata->res_data;
  tstride = pdata->tstride;
  qua_data = pdata->qua_data;
  weight = pdata->weight;
  gain_box = pdata->gain_box;
  nogains = pdata->nogains;
  nblock = pdata->nblock;
  mask = pdata->mask;
  lut_data = pdata->lut_data;

  /* Cannot apply a mask if there is no LUT. */
  if( !lut_data ) mask = NULL;

  /* Undo the previous iteration of the model, each thread handles a
     block of bolos */
  if( pdata->operation == 0 ) {

    /* If b1 past end of the work, nothing to do so we return */
    if( pdata->b1 >= nbolo ) {
      msgOutif( SMF__TIMER_MSG, "",
                "smfCalcmodelComPar: nothing for thread to do, returning",
                status);
      return;
    }

    /* Debugging message indicating thread started work */
    msgOutiff( SMF__TIMER_MSG, "",
               "smfCalcmodelComPar(%i): thread starting on bolos %zu -- %zu",
               status, pdata->operation, pdata->b1, pdata->b2 );

    /*  Allocate work space. */
    woff = astMalloc( sizeof( *woff )*ntslice );
    wg = astMalloc( sizeof( *wg )*ntslice );

    /* Loop round all the bolometers being processed by this thread. */
    for( j = pdata->b1; j <= pdata->b2 && *status == SAI__OK; j++ ) {
      /* Initialise the index of the first time slice for the current
         bolometer within the res_data and qua_data arrays. */
      dim_t ijindex = j*bstride;

      /* Initialise sums iused to fidn RMS bolometer value. */
      sum = 0.0;
      nsum = 0;

      /* Skip bad bolometers */
      if( !(qua_data[ ijindex ] & SMF__Q_BADB ) ) {

        /* Get the gain and offset for each time slice. */
        smf_gandoff( j, 0, ntslice-1, ntslice, gbstride, gcstride, gai_data,
                     nblock, gain_box, wg, woff, NULL, status );

        /* Loop over all time slices. */
        for( i = 0; i < ntslice; i++ ) {

          /* Add scaled template back on. Note that this also applies
             in the case that the common-mode was used to flatfield,
             since the gain correction will already have been un-done in
             a call to smf_calcmodel_gai in smf_iteratemap. */
          if( !(qua_data[ ijindex ] & SMF__Q_MOD ) &&
               model_data[ i ] != VAL__BADD ) {

            res_data[ ijindex ] += wg[ i ]*model_data[ i ] + woff[ i ];

            /* Increment the sums used to find the RMS bolometer value. */
            if( !(qua_data[ ijindex ] & SMF__Q_FIT ) ) {
               sum += res_data[ ijindex ]*res_data[ ijindex ];
               nsum++;
            }
          }

          /* Advance to next element of res_data and qua_data arrays. */
          ijindex += tstride;
        }

        /* If all bolometer values are zero or bad, indicate that the
           whole bolometer is bad by assigning SMF__Q_BADB to all samples. */
        if( sum <= 0.0 ) {
          ijindex = j*bstride;
          for( i = 0; i < ntslice; i++ ) {
            qua_data[ ijindex ] |= ( SMF__Q_BADB | SMF__Q_COM );
            ijindex += tstride;
          }
        }
      }

      /* Find the RMS bolometer value. We use this as the new initial
         estimate of the bolometer gain. This means that the bolometers
         will be normalized to a common level when forming the first
         estimate of the common mode signal. */
      gain = ( sum > 0.0 ) ? sqrt( sum/nsum ) : VAL__BADD;

      /* Initialise the bolometer fit parameters for every block. */
      if( gai_data ) {
         ijindex = j*gbstride;
         for( i = 0; i < nblock; i++ ) {
           gai_data[ ijindex ] = nogains ? 1.0 : gain;      /* Gain */
           gai_data[ ijindex + nblock*gcstride ] = 0.0;      /* Offset */
           gai_data[ ijindex + 2*nblock*gcstride ] = 1.0;      /* Correlation */
           ijindex += gcstride;
         }
      }
    }

    /*  Free work space. */
    woff = astFree( woff );
    wg = astFree( wg );

    msgOutiff( SMF__TIMER_MSG, "",
               "smfCalcmodelComPar(%i): thread finishing bolos %zu -- %zu",
               status, pdata->operation, pdata->b1, pdata->b2 );






  /* Calculate the new common mode averaging over all detectors. Each thread
     handles a block of time slices */
  } else if( pdata->operation == 1 ) {

    /* if t1 past end of the work, nothing to do so we return */
    if( pdata->t1 >= ntslice ) {
      msgOutif( SMF__TIMER_MSG, "",
                "smfCalcmodelComPar: nothing for thread to do, returning",
                status);
      return;
    }

    /* Debugging message indicating thread started work */
    msgOutiff( SMF__TIMER_MSG, "",
               "smfCalcmodelComPar(%i): thread starting on tslices %zu -- %zu",
               status, pdata->operation, pdata->t1, pdata->t2 );
    smf_timerinit( &tv1, &tv2, status);

    /* Initialise the arrays if this is the first chunk. */
    if( idx == 0 ) {
      for( i = pdata->t1; i <= pdata->t2; i++ ) {
        model_data[ i ] = 0.0;
        weight[ i ] = 0.0;
      }
    }

    /* Loop round all bolometers. */
    for( j = 0; j < nbolo && *status == SAI__OK; j++ ) {

      /* Initialise the index of the first time slice for the current
         bolometer within the res_data and qua_data arrays. */
      dim_t ijindex = j*bstride;

      /* Skip bad bolometers */
      if( !(qua_data[ ijindex ] & SMF__Q_BADB ) ) {

      /* Get the index of the "t1"th time slice for the current
         bolometer within the res_data and qua_data arrays. */
        ijindex += pdata->t1*tstride;

        /* Loop round all the time slices being processed by this thread. */
        for( i = pdata->t1; i <= pdata->t2; i++ ) {

          /* Skip previously flagged samples or bolometers that have zero
             gain. Otherwise, increment the running sums. */
          if( !( qua_data[ ijindex ] & SMF__Q_FIT ) &&
                 model_data[ i ] != VAL__BADD ) {

            /* Do not include samples that fall within the "source"
               areas of the map, as identified by a supplied mask. */
            int ilut = VAL__BADI;
            if (lut_data) ilut = lut_data[ ijindex ];

            if( !mask || ilut == VAL__BADI || mask[ ilut ] ) {
              model_data[ i ] += res_data[ ijindex ];
              weight[ i ]++;
            }
          }

          /* Advance to next element of res_data and qua_data arrays. */
          ijindex += tstride;

        }
      }
    }

    msgOutiff( SMF__TIMER_MSG, "",
               "smfCalcmodelComPar(%i): thread finishing tslices %zu -- %zu (%.3f sec)",
               status, pdata->operation, pdata->t1, pdata->t2,
               smf_timerupdate(&tv1, &tv2, status) );



  } else {
    *status = SAI__ERROR;
    errRep( "", "smfCalcmodelComPar: invalid operation specifier", status );
  }

}

/* ------------------------------------------------------------------------ */

#define FUNC_NAME "smf_calcmodel_com_old"

void smf_calcmodel_com_old( ThrWorkForce *wf, smfDIMMData *dat, int chunk,
                            AstKeyMap *keymap, smfArray **allmodel, int flags,
                            int *status) {

  /* Local Variables */
  dim_t bstride;               /* Bolometer stride in data array */
  dim_t boxcar=0;               /* width in samples of boxcar filter */
  double boxcard=0;             /* double precision version of boxcar */
  double boxfact=0;             /* Box width damping parameter */
  int boxmin=0;                 /* Min boxcar width if boxfact set */
  int do_boxcar=0;              /* flag to do boxcar smooth */
  int do_boxfact=0;             /* flag to damp boxcar width */
  int do_boxmin=0;              /* flag for minimum boxcar */
  int fillgaps;                 /* Are there any new gaps to fill? */
  dim_t gain_box;               /* Time slices per block */
  int gflat=0;                  /* If set use GAIn to adjust flatfield */
  double g=0;                   /* temporary gain */
  double g_copy=0;              /* copy of g */
  smfArray *gai=NULL;           /* Pointer to GAI at chunk */
  double *gai_data=NULL;        /* Pointer to DATA component of GAI */
  double **gai_data_copy=NULL;  /* copy of gai_data for all subarrays */
  double *gai_copy=NULL;        /* copy of gai_data for one subarray */
  dim_t gbstride;              /* GAIn bolo stride */
  dim_t gcstride;              /* GAIn coeff stride */
  AstKeyMap *gkmap=NULL;        /* Local GAIn keymap */
  dim_t i;                      /* Loop counter */
  dim_t iblock;                 /* Index of time block */
  int icom;                     /* Index of current COM model */
  int ii;                       /* Loop counter */
  dim_t idx_hi;                 /* Highest subarray index in current COM model */
  dim_t idx_lo;                 /* Lowest subarray index in current COM model */
  dim_t idx=0;                  /* Index within subgroup */
  dim_t j;                      /* Loop counter */
  smfCalcmodelComData *job_data=NULL; /* Array of job data */
  AstKeyMap *kmap=NULL;         /* Local keymap */
  double dchisq=0;              /* this - last model residual chi^2 */
  smfArray *lut=NULL;           /* Pointer to LUT at chunk */
  int *lut_data = NULL;         /* Array holding themap index for each sample */
  unsigned char *mask;          /* Pointer to 2D mask map */
  smfArray *model=NULL;         /* Pointer to model at chunk */
  double *model_data=NULL;      /* Pointer to DATA component of model */
  double *model_data_copy=NULL; /* Copy of model_data */
  int nbad;                     /* Number of rejected bolo-blocks */
  dim_t nbolo=0;                /* Number of bolometers */
  dim_t nblock=0;               /* Number of time blocks */
  dim_t ncom;                   /* Number of COM models */
  dim_t ndata=0;                /* Total number of data points */
  int *nrej=NULL;               /* Array holding no. of rejections per block */
  int nogains;                  /* Force all gains to unity? */
  smfArray *noi=NULL;           /* Pointer to NOI at chunk */
  double *noi_data=NULL;        /* Pointer to DATA component of model */
  dim_t noibstride;            /* bolo stride for noise */
  dim_t nointslice;             /* number of time slices for noise */
  dim_t noitstride;            /* Time stride for noise */
  int noremove=0;               /* Don't remove COM from time-series */
  int notfirst=0;               /* flag for delaying until after 1st iter */
  dim_t ndchisq=0;             /* number of elements contributing to dchisq */
  dim_t ntslice=0;              /* Number of time slices */
  int nw;                       /* Number of worker threads */
  AstObject *obj=NULL;          /* Used to avoid "type-punned" compiler warnings */
  double off=0;                 /* Temporary offset */
  double off_copy=0;            /* copy of off */
  smfCalcmodelComData *pdata=NULL; /* Pointer to job data */
  int perarray;                 /* Use a separate common mode for each array? */
  smfArray *qua=NULL;           /* Pointer to QUA at chunk */
  int quit;                     /* While loop quit flag */
  smf_qual_t *qua_data=NULL; /* Pointer to quality data */
  smfArray *res=NULL;           /* Pointer to RES at chunk */
  double *res_data=NULL;        /* Pointer to DATA component of res */
  dim_t step;                  /* step size for dividing up work */
  dim_t thisnbolo=0;            /* Check each file same dims as first */
  dim_t thisndata=0;            /* "                                  */
  dim_t thisntslice=0;          /* "                                  */
  dim_t tstride;               /* Time slice stride in data array */
  double *weight=NULL;          /* Weight at each point in model */
  double *wg;                   /* Work array holding gain values */
  double *woff;                 /* Work array holding offset values */
  double *wg_copy;              /* Work array holding old gain values */
  double *woff_copy;            /* Work array holding old offset values */

  /* Main routine */
  if (*status != SAI__OK) return;

  /* See if a mask should be used to exclude bright source areas from
     the COM model. */
  mask = smf_get_mask( wf, SMF__COM, keymap, dat, flags, NULL, status );

  /* Obtain pointers to relevant smfArrays for this chunk */
  res = dat->res[chunk];
  qua = dat->qua[chunk];
  if (dat->lut) lut = dat->lut[chunk];
  model = allmodel[chunk];
  if(dat->gai) {
    gai = dat->gai[chunk];

    /* Make a copy of gai_data (each subarray) for calculating convergence */
    gai_data_copy = astMalloc( (gai->ndat)*sizeof(*gai_data_copy) );
    for( idx=0; (idx<gai->ndat)&&(*status==SAI__OK); idx++ ) {

      smf_get_dims( gai->sdata[idx],  NULL, NULL, NULL, NULL,
                    &thisndata, NULL, NULL, status);

      gai_data_copy[idx] = astMalloc( thisndata*sizeof(*gai_data_copy[idx]) );

      gai_data = (gai->sdata[idx]->pntr)[0];

      memcpy( gai_data_copy[idx], gai_data, thisndata *
              sizeof(*gai_data_copy[idx]) );
    }
  }
  if(dat->noi) noi = dat->noi[chunk];

  /* Obtain pointer to sub-keymap containing COM parameters */
  astMapGet0A( keymap, "COM", &obj );
  kmap = (AstKeyMap *) obj;
  obj = NULL;

  /* Obtain pointer to sub-keymap containing GAI parameters */
  astMapGet0A( keymap, "GAI", &obj );
  gkmap = (AstKeyMap *) obj;

  /* Get the number of time slices per block. */
  smf_get_nsamp( kmap, "GAIN_BOX", res->sdata[0], &gain_box, status );

  /* See if gain is to be forced to default values. */
  astMapGet0I( kmap, "GAIN_IS_ONE", &nogains );

  /* Check for smoothing parameters in the CONFIG file */
  smf_get_nsamp( kmap, "BOXCAR", res->sdata[0], &boxcar, status );
  if( boxcar > 0 ) {
    do_boxcar = 1;
    msgSetk("BOX",boxcar);
    msgOutif(MSG__VERB, " ", "    boxcar width ^BOX", status);
  }

  /* Check for damping parameter on boxcar */
  astMapGet0D( kmap, "BOXFACT", &boxfact);
  if (boxfact > 0) do_boxfact = 1;

  /* If first iteration, set BOXCARD (this value will get decreased) */
  if( flags&SMF__DIMM_FIRSTITER ) {
    astMapPut0D( kmap, "BOXCARD", (double) boxcar, NULL );
  }

  /* Use damped boxcar for smoothing width */
  astMapGet0D( kmap, "BOXCARD", &boxcard);
  boxcar = (dim_t) boxcard;

  /* Check for minimum boxcar width*/
  astMapGet0I( kmap, "BOXMIN", &boxmin);
  if (boxmin > 0 ) do_boxmin = 1;

  /* Bolo rejection parameters */
  astMapGet0I(kmap, "NOTFIRST", &notfirst);

  /* Don't remove common-mode from time-series? */
  astMapGet0I(kmap, "NOREMOVE", &noremove);

  /* Are we using gains to adjust the flatfield? */
  astMapGet0I( gkmap, "FLATFIELD", &gflat );

  /* If gflat specified but no GAI model, warn user */
  if( gflat && !(dat->gai) ) {
    msgOut( "", FUNC_NAME
            ": *** WARNING: GAI.FLATFIELD set but GAI not part of MODELORDER",
            status);
  }

  /* Are we skipping the first iteration? */
  if( notfirst && (flags & SMF__DIMM_FIRSTITER) ) {
    msgOutif( MSG__VERB, "", FUNC_NAME
              ": skipping COM this iteration", status );
    return;
  }

  /* How many threads do we get to play with */
  nw = wf ? wf->nworker : 1;

  /* Allocate job data for threads */
  job_data = astCalloc( nw, sizeof(*job_data) );

  /* Assert bolo-ordered data */
  smf_model_dataOrder( wf, dat, NULL, chunk, SMF__RES|SMF__QUA|SMF__GAI|SMF__NOI, 0,
                       status );


  /* Use separate common modes for each sub-array? */
  astMapGet0I( kmap, "PERARRAY", &perarray );

  /* If "perarray" is non-zero, a separate common mode signal is calculated
     for each available subarrays and is stored as a separate 1d vector.
     If "perarray" is zero, a single common mode signal is calculated from
     all available subarrays and is stored as a 1d vector. The corresponding
     smfData is at position 0 in the model sdata. Store the number of COM
     models to create. */
  if( perarray ) {
    ncom = model->ndat;
    if( (int) res->ndat != ncom && *status == SAI__OK  ) {
       *status = SAI__ERROR;
       errRep( "", FUNC_NAME ": COM model and residuals contain different "
               "number of data arrays!", status);
    }
  } else {
    ncom = 1;
  }

  /* Loop round creating each COM model. */
  for( icom = 0; icom < ncom && *status == SAI__OK; icom++ ) {

    /* Set the index of the first and last subarray that contributes to the
       current COM model. */
    if( perarray ) {
       idx_lo = icom;
       idx_hi = icom;
       msgSeti( "I", icom + 1 );
       msgSetk( "N", ncom );
       msgOutif( MSG__VERB, "", "  Calculating COM model for array ^I of ^N",
                 status );
    } else {
       idx_lo = 0;
       idx_hi = res->ndat - 1;
    }

    /* Check space is available for the current COM model. */
    if( model->sdata[icom] ) {

      /* Pointer to model data array */
      model_data = (model->sdata[icom]->pntr)[0];

      /* Copy of model data array */
      model_data_copy = astCalloc( (model->sdata[icom]->dims)[0],
                                   sizeof(*model_data_copy) );
      if( *status == SAI__OK ) {
        memcpy( model_data_copy, model_data, (model->sdata[icom]->dims)[0] *
  	      sizeof(*model_data_copy) );
      }
      /* Temporary buffer to store weights */
      weight = astMalloc( ((model->sdata[icom]->dims)[0])*sizeof(*weight) );

      /* Find the number of blocks of time slices per bolometer. Each
         block contains "gain_box" time slices (except possibly for the
         last time slice which may contain more than gain_box but will
         be less than 2*gain_box). Each block of time slices from a
         single bolometer has its own gain, offset and correlation
         values. */
      nblock = (model->sdata[icom]->dims)[0]/gain_box;
      if( nblock == 0 ) nblock = 1;

    } else {
      *status = SAI__ERROR;
      errRep( "", FUNC_NAME ": Model smfData was not loaded!", status);
    }




    /* Add the previous estimate of the common mode signal back on to the
       bolometer residuals. If we do not yet have an estimate of the common
       mode signal, then the bolometer residuals are left unchanged, but
       the gain values in the "gai" model are set to the rms value of the
       bolometer data stream. This is to ensure that the very high gain
       bolometers do not dominate the initial estimate of the common mode
       signal calculated below.
       --------------------------------------------------------------  */

    /* Loop over the subarrays that contribute to the current COM model. */
    for( idx=idx_lo; idx<=idx_hi; idx++ ) if (*status == SAI__OK ) {
      /* Obtain dimensions of the data */
      smf_get_dims( res->sdata[idx],  NULL, NULL, &thisnbolo, &thisntslice,
                    &thisndata, &bstride, &tstride, status);

      if(gai) smf_get_dims( gai->sdata[idx],  NULL, NULL, NULL, NULL, NULL,
                            &gbstride, &gcstride, status);

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

          /* Ensure that the last thread picks up any left-over tslices */
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

      /* Get pointers to data/quality/lut/model */
      res_data = (res->sdata[idx]->pntr)[0];
      qua_data = (qua->sdata[idx]->pntr)[0];
      if( gai ) {
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
          pdata->gflat = gflat;
          pdata->idx = idx;
          pdata->model_data = model_data_copy; /* Careful! */
          pdata->nbolo = nbolo;
          pdata->ntslice = ntslice;
          pdata->operation = 0;
          pdata->res_data = res_data;
          pdata->tstride = tstride;
          pdata->qua_data = qua_data;
          pdata->ijob = -1;
          pdata->weight = weight;
          pdata->gain_box = gain_box;
          pdata->nogains = nogains;
          pdata->nblock = nblock;
        }
      }

      for( ii=0; ii<nw; ii++ ) {
        /* Submit the job */
        pdata = job_data + ii;
        pdata->ijob = thrAddJob( wf, THR__REPORT_JOB, pdata,
                                   smfCalcmodelComPar, 0, NULL, status );
      }
      /* Wait until all of the submitted jobs have completed */
      thrWait( wf, status );
    }









    /* Now calculate a new common mode equal to the average of the scaled
       bolometer time series (averaged over all sub-arrays). Then (if using
       a GAI model) determine a set of gains and offsets for each bolometer
       (one for each block of "gain_box" time slices) by comparing the bolometer
       values with the new common mode. Reject any bolometers that are
       insufficiently similar to the average common mode, or that have
       unusual gains, and then loop to form a new average common mode,
       excluding the rejected bolometers. Iterate until converged.
       ---------------------------------------------------------------- */

    /* If we are using a GAI model, create an integer array to hold the
       number of bolometers rejected from each block on the previous call to
       smf_find_gains_array. Initialise to hold an arbitrary non-zero value
       (1) to indicate no blocks have yet converged. */
    if( gai ) {
      nrej = astMalloc( nblock*sizeof( *nrej ) );
      if( *status == SAI__OK ) {
        for( iblock = 0; iblock < nblock; iblock++ ) nrej[ iblock ] = 1;
      }
    }

    /* Outer loop re-calculates common-mode until the list of "good" blocks
       converges. Without a fit for gain/offset this loop only happens once.
       Otherwise it keeps going until the correlation coefficients of the
       template fits settle down to a list with no N-sigma outliers. */

    fillgaps = 0;
    quit = 0;
    while( !quit && (*status==SAI__OK) ) {

      /* Calculate a new estimate of the common mode signal excluding
         blocks rejected on any previous passes round this loop. The new
         common mode signal is the mean of the remaining bolometer data.
         Initialize model data and weights to 0 */
      memset(model_data,0,(model->sdata[icom]->dims)[0]*sizeof(*model_data));
      memset(weight,0,(model->sdata[icom]->dims)[0]*sizeof(*weight));

      /* Loop over the subarrays that contribute to the current COM model. */
      for( idx=idx_lo; (idx<=idx_hi)&&(*status==SAI__OK); idx++ ) {

        /* Get pointers to data/quality/gain values for the current
           sub-array. */
        res_data = (res->sdata[idx]->pntr)[0];
        qua_data = (qua->sdata[idx]->pntr)[0];
        if( gai ) gai_data = (gai->sdata[idx]->pntr)[0];
        if (lut) lut_data = (lut->sdata[idx]->pntr)[0];

        /* Set up the job data to update "model_data" and "weight" to
           include the contribution from the current sub-array. */
        for( ii=0; (*status==SAI__OK)&&(ii<nw); ii++ ) {
          pdata = job_data + ii;

          pdata->bstride = bstride;
          pdata->gai_data = gai_data;
          pdata->gbstride = gbstride;
          pdata->gcstride = gcstride;
          pdata->gflat = gflat;
          pdata->idx = idx;
          pdata->model_data = model_data; /* Careful! */
          pdata->nbolo = nbolo;
          pdata->ntslice = ntslice;
          pdata->operation = 1;
          pdata->res_data = res_data;
          pdata->tstride = tstride;
          pdata->qua_data = qua_data;
          pdata->ijob = -1;
          pdata->weight = weight;
          pdata->mask = mask;
          pdata->lut_data = lut_data;
        }

        /* Submit the jobs to the workforce and Wait until they have all
           completed */
        for( ii=0; ii<nw; ii++ ) {
          /* Submit the job */
          pdata = job_data + ii;
          pdata->ijob = thrAddJob( wf, THR__REPORT_JOB, pdata,
                                     smfCalcmodelComPar, 0, NULL, status );
        }

        thrWait( wf, status );
      }

      /* Re-normalize the model, or set model bad if no data. */
      for( i=0; i<ntslice; i++ ) {
        if( weight[i] ) {
          model_data[i] /= weight[i];
        } else {
          model_data[i] = VAL__BADD;
        }
      }

      /* boxcar smooth if desired */
      if( do_boxcar ) smf_tophat1D( model_data, ntslice, (int) boxcar, NULL, 0,
                                    0.0, status );

      /* Now try to fit the remaining bolometer data to the new common mode
         estimate. Each bolometer is split into blocks of "gain_box" time
         slices, and a linear fit is performed between the bolometer values
         in a block and the corresponding values in the new common mode
         estimate. This also rejects aberrant bolometers from all sub-arrays.
         If we do not have a GAI model, then zero bolometers are rejected. */
      if( perarray ) {

         nbad = gai ? smf_find_gains( wf, 0, res->sdata[icom], mask,
                                      lut ? lut->sdata[icom] : NULL,
                                      model_data, kmap, SMF__Q_GOOD,
                                      SMF__Q_COM, gai->sdata[icom], nrej,
                                      status ) : 0;
      } else {
         nbad = gai ? smf_find_gains_array( wf, 0, res, mask, lut, model_data,
                                            kmap, SMF__Q_GOOD, SMF__Q_COM,
                                            gai, nrej, status ) : 0;
      }

      /* If no bolometers were rejected we can quit the loop. */
      if( nbad > 0 ) {
        fillgaps = 1;
        msgOutif( MSG__DEBUG, "", "    Common mode not yet converged",
                  status );
      } else {
        quit = 1;
        msgOutif( MSG__DEBUG, "", "    Common mode converged",
                  status );
      }

    }



   /* Once the common mode converged form the residuals by removing the scaled
      common mode estimate from the bolometer data (this is designed to work
      even if we are not using a GAI model).
      ----------------------------------------------------------------- */

    if( !noremove ) {

      /*  Allocate work space. */
      woff = astMalloc( sizeof( *woff )*ntslice );
      wg = astMalloc( sizeof( *wg )*ntslice );
      woff_copy = astMalloc( sizeof( *woff_copy )*ntslice );
      wg_copy = astMalloc( sizeof( *wg_copy )*ntslice );

      for( idx=idx_lo; (idx<=idx_hi)&&(*status==SAI__OK); idx++ ) {
        smf_get_dims( res->sdata[idx],  NULL, NULL, NULL, NULL, NULL, &bstride,
                      &tstride, status );

        /* Get pointers */
        res_data = (double *)(res->sdata[idx]->pntr)[0];
        qua_data = (smf_qual_t *)(qua->sdata[idx]->pntr)[0];
        if( gai ) {
          smf_get_dims( gai->sdata[idx],  NULL, NULL, NULL, NULL, NULL,
                        &gbstride, &gcstride, status);
          gai_data = (gai->sdata[idx]->pntr)[0];
          gai_copy = gai_data_copy[ idx ];
        }
        if( noi ) {
          smf_get_dims( noi->sdata[idx],  NULL, NULL, NULL, &nointslice,
                        NULL, &noibstride, &noitstride, status);
          noi_data = (double *)(noi->sdata[idx]->pntr)[0];
        }

        /* Loop round all bolometers. */
        for( j = 0 ; j < nbolo && *status == SAI__OK; j++ ) {

          /* Initialise the index of the first time slice for the current
             bolometer within the res_data and qua_data arrays. */
          dim_t ijindex = j*bstride;

          /* Skip bad bolometers */
          if( !(qua_data[ ijindex ] & SMF__Q_BADB ) ) {
            dim_t inbase = j*noibstride;

            /* Get the gain and offset for each time slice. */
            smf_gandoff( j, 0, ntslice-1, ntslice, gbstride, gcstride,
                         gai_data, nblock, gain_box, wg, woff, NULL, status );

            /* Also get the previous gain and offset for each time slice. */
            smf_gandoff( j, 0, ntslice-1, ntslice, gbstride, gcstride,
                         gai_copy, nblock, gain_box, wg_copy, woff_copy,
                         NULL, status );

            /* Loop over all time slices. */
            for( i = 0; i < ntslice; i++ ) {

              g = wg[ i ];
              off = woff[ i ];
              g_copy = wg_copy[ i ];
              off_copy = woff_copy[ i ];

              if( (g!=VAL__BADD) && (g!=0) ) {

                /* Important: if flat-fielding data re-scale noi. May have
                   different dimensions from res. */
                if( gflat && noi && i < nointslice &&
                    noi_data[ inbase + i*noitstride ] != VAL__BADD ) {
                  noi_data[ inbase + i*noitstride ] /= (g*g);
                }

                /* Remove the common mode */
                if( !( qua_data[ijindex] & SMF__Q_MOD ) ){
                  if( model_data[ i ] != VAL__BADD ) {
                    if( gflat ) {
                      /* If correcting the flatfield, scale data to match
                         template amplitude first, then remove */
                      res_data[ijindex] =
                        (res_data[ijindex] - off)/g - model_data[i];
                    } else {
                      /* Otherwise subtract scaled template off data */
                      res_data[ijindex] -= (g*model_data[i] + off);
                    }

                    /* also measure contribution to dchisq */
                    if( noi &&
                        (noi_data[inbase+(i%nointslice)*noitstride] != 0) &&
                        !(qua_data[ijindex]&SMF__Q_GOOD) ) {

                      if( gflat ) {
                        /* Compare change in template + offset/g */
                        dchisq += ( (model_data[i] + off/g) -
                                    (model_data_copy[i] + off_copy/g_copy) ) *
                          ( (model_data[i] + off/g) -
                            (model_data_copy[i] + off_copy/g_copy) ) /
                          noi_data[inbase + (i%nointslice)*noitstride];
                        ndchisq++;
                      } else {
                        /* Otherwise scale the template and measure change*/
                        dchisq += ((g*model_data[i] + off) -
                                   (g_copy*model_data_copy[i] + off_copy)) *
                          ((g*model_data[i] + off) -
                           (g_copy*model_data_copy[i] + off_copy)) /
                          noi_data[inbase + (i%nointslice)*noitstride];
                        ndchisq++;
                      }
                    }
                  } else {
                    qua_data[ijindex] |= SMF__Q_COM;
                  }
                }
              }
              ijindex += tstride;
            }
          }
        }
      }

      /*  Free work space. */
      woff = astFree( woff );
      wg = astFree( wg );
      woff_copy = astFree( woff_copy );
      wg_copy = astFree( wg_copy );
    }

    /* If any blocks of bolometer values were flagged, replace the bad
       bolometer values with artifical data that is continuous with the
       surround good data. This is needed since subsequent filtering
       operations may not ignore flagged bolometer values but may instead
       use the bolometer values literally. */
    if( fillgaps ) astMapGet0I( keymap, "FILLGAPS", &fillgaps );
    if( fillgaps ) {
      for( idx=idx_lo; (idx<=idx_hi)&&(*status==SAI__OK); idx++ ) {
        smf_fillgaps( wf, res->sdata[idx], SMF__Q_COM, status );
      }
    }

    /* If we're not removing the common-mode, set it to 0 here */
    if( noremove && (*status==SAI__OK) ) {
      memset(model_data,0,(model->sdata[icom]->dims)[0]*sizeof(*model_data));
    }

    /* Free memory used just to calculate the current COM model. */
    weight = astFree( weight );
    model_data_copy = astFree( model_data_copy );
    nrej = astFree( nrej );
  }

  /* Print normalized residual chisq for this model */
  if( (*status==SAI__OK) && noi && (ndchisq>0) ) {
    dchisq /= (double) ndchisq;
    msgOutiff( MSG__VERB, "", "    normalized change in model: %lf", status,
               dchisq );
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

  /* Annul objects. Unlike freeing a NULL memory pointer, annulling a NULL
     object pointer causes an error, so check before annulling. */
  if( kmap ) kmap = astAnnul( kmap );
  if( gkmap ) gkmap = astAnnul( gkmap );

  /* Free memory used to calculate all COM models. */
  job_data = astFree( job_data );
  gai_data_copy = astFreeDouble( gai_data_copy );

}


