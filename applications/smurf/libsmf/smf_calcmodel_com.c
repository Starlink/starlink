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

*  Copyright:
*     Copyright (C) 2006-2010 University of British Columbia.
*     Copyright (C) 2010 Science and Technology Facilities Council.
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

/* Local macros */
#define BAD_FIT VAL__MAXD/2

/* ------------------------------------------------------------------------ */
/* Local variables and functions */

/* Structure containing information about blocks of bolos that each
 thread will process*/
typedef struct smfCalcmodelComData {
  size_t b1;               /* Index of first bolometer of block */
  size_t b2;               /* Index of last bolometer of block */
  size_t bstride;          /* bolometer stride for res/qua */
  dim_t fit_box;           /* Number of time slices used in each fit */
  double *gai_data;        /* pointer to gain model (can be NULL) data */
  dim_t gain_box;          /* Number of time slices per block */
  size_t gbstride;         /* gain bolo stride */
  size_t gcstride;         /* gain coefficient stride */
  int gflat;               /* correct flatfield using GAI */
  dim_t idx;               /* Index within subgroup */
  int ijob;                /* Job identifier */
  double *model_data;      /* pointer to common mode data */
  dim_t nblock;            /* No of time slice blocks per bolometer */
  dim_t nbolo;             /* number of bolometers */
  int nogains;             /* Force all gains to unity? */
  dim_t ntslice;           /* number of time slices */
  int operation;           /* 0=undo COM, 1=new COM, 2=fit COM */
  double *res_data;        /* Pointer to common residual data */
  size_t t1;               /* Index of first timeslice of block */
  size_t t2;               /* Index of last timeslice of block */
  size_t tstride;          /* time stride for res/qua */
  unsigned char *qua_data; /* Pointer to common quality data */
  double *weight;          /* Weight at each point in model */
  int *converged;          /* Flags saying if each bock has converged */
} smfCalcmodelComData;



/* Function to be executed in thread: task depends on value of operation */

void smfCalcmodelComPar( void *job_data_ptr, int *status );

void smfCalcmodelComPar( void *job_data_ptr, int *status ) {
  dim_t block_cstride;     /* Stride between block parameters */
  dim_t block_tstride;     /* Stride between time slice blocks */
  dim_t block_size;        /* Number of time slices in block */
  size_t bstride;          /* bolometer stride for res/qua */
  int *converged;          /* Flags saying if each bock has converged */
  dim_t fit_box;           /* Number of time slices used in each fit */
  double *gai_data;        /* pointer to gain model (can be NULL) data */
  double gain;             /* Gain value */
  dim_t gain_box;          /* Nominal number of time slices per block */
  size_t gbstride;         /* gain bolo stride */
  size_t gcstride;         /* gain coefficient stride */
  int gflat;               /* correct flatfield using GAI */
  size_t i;                /* Loop counter */
  dim_t iblock;            /* Index for block of time slices */
  dim_t idx;               /* Index within subgroup */
  size_t j;                /* Loop counter */
  double *m;               /* Pointer to next model data value */
  double *model_data;      /* pointer to common mode data */
  dim_t nbolo;             /* number of bolometers */
  dim_t nblock;            /* Number of time blocks */
  int nogains;             /* Force all gains to unity? */
  dim_t nsum;              /* Number of values summed in "sum" */
  dim_t ntime;             /* Number of remaining time slices */
  dim_t ntslice;           /* number of time slices */
  smfCalcmodelComData *pdata=NULL; /* Pointer to job data */
  double *pg;              /* Pointer to next gain value */
  double *poff;            /* Pointer to next offset value */
  double *res_data;        /* Pointer to common residual data */
  double sum;              /* Running sum of values */
  size_t tstride;          /* time stride for res/qua */
  unsigned char *qua_data; /* Pointer to common quality data */
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
  gflat = pdata->gflat;
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
  fit_box = pdata->fit_box;
  nblock = pdata->nblock;
  converged = pdata->converged;


  /* Undo the previous iteration of the model, each thread handles a
     block of bolos */
  if( pdata->operation == 0 ) {

    /* If b1 past end of the work, nothing to do so we return */
    if( pdata->b1 >= nbolo ) {
      msgOutif( MSG__DEBUG, "",
                "smfCalcmodelComPar: nothing for thread to do, returning",
                status);
      return;
    }

    /* Debugging message indicating thread started work */
    msgOutiff( MSG__DEBUG, "",
               "smfCalcmodelComPar(%i): thread starting on bolos %zu -- %zu",
               status, pdata->operation, pdata->b1, pdata->b2 );

    /*  Allocate work space. */
    woff = astMalloc( sizeof( *woff )*ntslice );
    wg = astMalloc( sizeof( *wg )*ntslice );

    /* Loop round all the bolometers being processed by this thread. */
    for( j = pdata->b1; j <= pdata->b2 && *status == SAI__OK; j++ ) {
      /* Initialise the index of the first time slice for the current
         bolometer within the res_data and qua_data arrays. */
      size_t ijindex = j*bstride;

      /* Initialise sums iused to fidn RMS bolometer value. */
      sum = 0.0;
      nsum = 0;

      /* Skip bad bolometers */
      if( !(qua_data[ ijindex ] & SMF__Q_BADB ) ) {

        /* Get the gain and offset for each time slice. */
        smf_gandoff( j, 0, ntslice-1, ntslice, gbstride, gcstride, gai_data,
                     nblock, gain_box, wg, woff, status );

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
            sum += res_data[ ijindex ]*res_data[ ijindex ];
            nsum++;
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
      gain = ( sum > 0.0 ) ? sqrtf( sum/nsum ) : VAL__BADD;

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

    msgOutiff( MSG__DEBUG, "",
               "smfCalcmodelComPar(%i): thread finishing bolos %zu -- %zu",
               status, pdata->operation, pdata->b1, pdata->b2 );






  /* Calculate the new common mode averaging over all detectors. Each thread
     handles a block of time slices */
  } else if( pdata->operation == 1 ) {

    /* if t1 past end of the work, nothing to do so we return */
    if( pdata->t1 >= ntslice ) {
      msgOutif( MSG__DEBUG, "",
                "smfCalcmodelComPar: nothing for thread to do, returning",
                status);
      return;
    }

    /* Debugging message indicating thread started work */
    msgOutiff( MSG__DEBUG, "",
               "smfCalcmodelComPar(%i): thread starting on tslices %zu -- %zu",
               status, pdata->operation, pdata->t1, pdata->t2 );
    smf_timerinit( &tv1, &tv2, status);

    /*  Allocate work space. */
    woff = astMalloc( sizeof( *woff )*( pdata->t2 - pdata->t1 + 1 ) );
    wg = astMalloc( sizeof( *wg )*( pdata->t2 - pdata->t1 + 1 ) );

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
      size_t ijindex = j*bstride;

      /* Skip bad bolometers */
      if( !(qua_data[ ijindex ] & SMF__Q_BADB ) ) {

      /* Get the index of the "t1"th time slice for the current
         bolometer within the res_data and qua_data arrays. */
        ijindex += pdata->t1*tstride;

        /* Get the bolometer's gain and offset for the required time slices. */
        smf_gandoff( j, pdata->t1, pdata->t2, ntslice, gbstride, gcstride,
                     gai_data, nblock, gain_box, wg, woff, status );

        /* Loop round all the time slices being processed by this thread. */
        pg = wg;
        poff = woff;
        for( i = pdata->t1; i <= pdata->t2; i++,pg++,poff++ ) {

          /* Skip previously flagged samples or bolometers that have zero
             gain. Otherwise, increment the running sums. */
          if( !( qua_data[ ijindex ] & SMF__Q_FIT ) && *pg != 0.0 &&
               model_data[ i ] != VAL__BADD ) {
            model_data[ i ] += ( res_data[ ijindex ] - *poff)/(*pg);
            weight[ i ]++;
          }

          /* Advance to next element of res_data and qua_data arrays. */
          ijindex += tstride;

        }
      }
    }

    /*  Free work space. */
    woff = astFree( woff );
    wg = astFree( wg );

    msgOutiff( MSG__DEBUG, "",
               "smfCalcmodelComPar(%i): thread finishing tslices %zu -- %zu (%.3f sec)",
               status, pdata->operation, pdata->t1, pdata->t2,
               smf_timerupdate(&tv1, &tv2, status) );



  } else if( pdata->operation == 2 ) {
    /* Loop over the block of bolos for this thread and fit the template */

    /* if b1 past end of the work, nothing to do so we return */
    if( pdata->b1 >= nbolo ) {
      msgOutif( MSG__DEBUG, "",
                "smfCalcmodelComPar: nothing for thread to do, returning",
                status);
      return;
    }

    /* Debugging message indicating thread started work */
    msgOutiff( MSG__DEBUG, "",
               "smfCalcmodelComPar(%i): thread starting on bolos %zu -- %zu",
               status, pdata->operation, pdata->b1, pdata->b2 );
    smf_timerinit( &tv1, &tv2, status);

    int delta_box = ( fit_box - gain_box )/2;
    block_cstride = gcstride*nblock;
    block_tstride = gain_box*tstride;
    for( i=pdata->b1; (*status==SAI__OK) && (i<=pdata->b2); i++ ) {
      size_t ibase = i*bstride;
      size_t igbase = i*gbstride;

      if( !(qua_data[ibase]&SMF__Q_BADB) ) {

        /* Initialise the number of time slices still to be processed, and then
           loop round each block of time slices, fitting the common mode to the
           bolometer signal and finding a gain, offset and correlation for each
           block. The final block may contain more than "gain_box" slices. */
        ntime = ntslice;
        m = model_data;
        for( iblock=0; iblock < nblock && *status == SAI__OK; iblock++ ){

          /* Calculate the number of time slices in this block. The last
          block (index iblock-1 ) contains all remaining time slices,
          which may not be gain_box in number. */
          block_size = ( iblock < nblock - 1 ) ? gain_box : ntime;

          /* Skip blocks that have already been rejected, or have already
             converged. */
          if( gai_data[ igbase ] != VAL__BADD && !converged[ iblock ] ) {

            size_t box_start = ntslice - ntime;
            size_t box_end = box_start + block_size - 1;

            int fit_start = box_start - delta_box;
            int fit_end = box_end + delta_box;

            if( fit_start < 0 ) fit_start = 0;
            if( fit_end >= (int) ntslice ) fit_end = ntslice - 1;

            int fit_off = box_start - fit_start;
            int fit_size = fit_end - fit_start + 1;

            size_t ibase_box = ibase - fit_off*tstride;
            double *m_box = m - fit_off*tstride;

            smf_templateFit1D( res_data+ibase_box, qua_data+ibase_box,
                               SMF__Q_GOOD, SMF__Q_MOD, fit_size,
                               tstride, m_box, 0, 0, gai_data+igbase,
                               gai_data+block_cstride+igbase,
                               gai_data+2*block_cstride+igbase, status );

            /* If no fit could be performed. */
            if( *status == SMF__DIVBZ || *status == SMF__INSMP ) {
              for( j=0; j<block_size; j++ ) {
                qua_data[ibase+j*tstride] |= SMF__Q_COM;
              }
              *(gai_data+igbase) = BAD_FIT;
              errAnnul( status );

            /* If we are ignoring gains, force gains to 1.0 and offsets
               to 0.0. Retain the correlation coeffs in order to flag bad
               blocks. */
            } else if( nogains ) {
               *(gai_data+igbase) = 1.0;
               *(gai_data+block_cstride+igbase) = 0.0;
            }
          }

          ntime -= block_size;
          ibase += block_tstride;
          m += block_tstride;
          igbase += gcstride;
        }

      /* Store bad gain values for every block if the whole bolometer is bad. */
      } else {
        for( j = 0; j < 3*nblock; j++ ){
          gai_data[ igbase ] = VAL__BADD;
          igbase += gcstride;
        }
      }
    }

    msgOutiff( MSG__DEBUG, "",
               "smfCalcmodelComPar(%i): thread finishing bolos %zu -- %zu (%.3f sec)",
               status, pdata->operation, pdata->b1, pdata->b2,
               smf_timerupdate(&tv1, &tv2, status) );




  } else {
    *status = SAI__ERROR;
    errRep( "", "smfCalcmodelComPar: invalid operation specifier", status );
  }

}

/* ------------------------------------------------------------------------ */

#define NREASON 11
#define FUNC_NAME "smf_calcmodel_com"

void smf_calcmodel_com( smfWorkForce *wf, smfDIMMData *dat, int chunk,
                        AstKeyMap *keymap, smfArray **allmodel, int flags,
                        int *status) {

  /* Local Variables */
  int allbad;                   /* All bolometers bad at this block? */
  dim_t block_size;             /* Number of time slices in block */
  size_t bstride;               /* Bolometer stride in data array */
  int boxcar=0;                 /* width in samples of boxcar filter */
  double boxcard=0;             /* double precision version of boxcar */
  double boxfact=0;             /* Box width damping parameter */
  int boxmin=0;                 /* Min boxcar width if boxfact set */
  int *converged;               /* Flags saying if each bock has converged */
  int do_boxcar=0;              /* flag to do boxcar smooth */
  int do_boxfact=0;             /* flag to damp boxcar width */
  int do_boxmin=0;              /* flag for minimum boxcar */
  double c;                     /* temporary corr coeff */
  size_t cgood;                 /* Number of good corr. coeff. samples */
  double cmean;                 /* mean of common-mode correlation coeff */
  double *corr=NULL;            /* Array to hold correlation coefficients */
  double corr_abstol=0;         /* Absolute lowest corr. coeff. limit */
  dim_t corr_offset=0;          /* Offset from gain to correlation value */
  double corr_tol=0;            /* n-sigma correlation coeff. tolerance */
  double csig;                  /* standard deviation "                  */
  double csum;                  /* Sum of corr coeffs */
  int fillgaps;                 /* Are there any new gaps to fill? */
  int first;                    /* First pass round loop? */
  dim_t fit_box=0;              /* No. of time slices used in template fit */
  double *gcoeff=NULL;          /* Array to hold gain coefficients */
  int gflat=0;                  /* If set use GAIn to adjust flatfield */
  size_t ggood;                 /* Number of good gain. coeff. samples */
  dim_t glim = 1;               /* Lowest acceptable number of good blocks */
  double gmax;                  /* Largest acceptable gain */
  double gmin;                  /* Smallest acceptable gain */
  double gmean;                 /* mean of common-mode correlation gain */
  double gsig;                  /* standard deviation "                  */
  double g=0;                   /* temporary gain */
  double g_copy=0;              /* copy of g */
  smfArray *gai=NULL;           /* Pointer to GAI at chunk */
  double *gai_data=NULL;        /* Pointer to DATA component of GAI */
  double **gai_data_copy=NULL;  /* copy of gai_data for all subarrays */
  double gain_abstol=0;         /* absolute gain coeff. tolerance */
  dim_t gain_box=0;             /* No. of time slices in a block */
  double gain_fgood=0;          /* Fraction of good blocks required */
  double gain_rat=0;            /* Ratio of usable gains */
  double gain_tol=0;            /* n-sigma gain coeff. tolerance */
  size_t gbstride;              /* GAIn bolo stride */
  size_t gcstride;              /* GAIn coeff stride */
  AstKeyMap *gkmap=NULL;        /* Local GAIn keymap */
  dim_t i;                      /* Loop counter */
  dim_t iblock;                 /* Index of time block */
  int ii;                       /* Loop counter */
  int ireason;                  /* Index of current reason for block rejection */
  int ival = 0;                 /* Integer value */
  dim_t idx=0;                  /* Index within subgroup */
  dim_t j;                      /* Loop counter */
  smfCalcmodelComData *job_data=NULL; /* Array of job data */
  AstKeyMap *kmap=NULL;         /* Local keymap */
  double dchisq=0;              /* this - last model residual chi^2 */
  smfArray *model=NULL;         /* Pointer to model at chunk */
  double *model_data=NULL;      /* Pointer to DATA component of model */
  double *model_data_copy=NULL; /* Copy of model_data */
  dim_t nbolo=0;                /* Number of bolometers */
  dim_t nblock=0;               /* Number of time blocks */
  int nconverged;               /* Number of converged blocks */
  dim_t ndata=0;                /* Total number of data points */
  size_t newbad;                /* Number of new bolos being flagged as bad */
  int nogains;                  /* Force all gains to unity? */
  smfArray *noi=NULL;           /* Pointer to NOI at chunk */
  double *noi_data=NULL;        /* Pointer to DATA component of model */
  size_t noibstride;            /* bolo stride for noise */
  dim_t nointslice;             /* number of time slices for noise */
  size_t noitstride;            /* Time stride for noise */
  int notfirst=0;               /* flag for delaying until after 1st iter */
  size_t ndchisq=0;             /* number of elements contributing to dchisq */
  dim_t ntime;                  /* Number of remaining time slices */
  dim_t ntslice=0;              /* Number of time slices */
  int nw;                       /* Number of worker threads */
  AstObject *obj;               /* Used to avoid "type-punned" compiler warnings */
  double off=0;                 /* Temporary offset */
  dim_t off_offset=0;           /* Offset from offset to correlation value */
  double off_copy=0;            /* copy of off */
  smfCalcmodelComData *pdata=NULL; /* Pointer to job data */
  smfArray *qua=NULL;           /* Pointer to QUA at chunk */
  int quit;                     /* While loop quit flag */
  unsigned char *qua_data=NULL; /* Pointer to quality data */
  int reason[ NREASON ];        /* No. of blocks rejected for each reason */
  smfArray *res=NULL;           /* Pointer to RES at chunk */
  double *res_data=NULL;        /* Pointer to DATA component of res */
  size_t step;                  /* step size for dividing up work */
  dim_t thisnbolo=0;            /* Check each file same dims as first */
  dim_t thisndata=0;            /* "                                  */
  dim_t thisntslice=0;          /* "                                  */
  int totgood;                  /* Number of good bolo blocks remaining */
  size_t tstride;               /* Time slice stride in data array */
  double *weight=NULL;          /* Weight at each point in model */
  double *wg;                   /* Work array holding gain values */
  double *woff;                 /* Work array holding offset values */
  double *wg_copy;              /* Work array holding old gain values */
  double *woff_copy;            /* Work array holding old offset values */

  const char *reason_text[ NREASON ] = {
                                   "fit failed",
                                   "gain is negative",
                                   "corr_abstol test failed",
                                   "corr_tol test failed",
                                   "corr_abstol test failed (2)",
                                   "gain_tol test failed",
                                   "gain_abstol test failed",
                                   "unknown cause",
                                   "gain_rat test failed - gain too high",
                                   "gain_rat test failed - gain too low",
                                   "gain_fgood test failed" };



  /* Main routine */
  if (*status != SAI__OK) return;

  /* How many threads do we get to play with */
  nw = wf ? wf->nworker : 1;

  /* Obtain pointer to sub-keymap containing COM parameters */
  astMapGet0A( keymap, "COM", &obj );
  kmap = (AstKeyMap *) obj;
  obj = NULL;

  /* Obtain pointer to sub-keymap containing GAI parameters */
  astMapGet0A( keymap, "GAI", &obj );
  gkmap = (AstKeyMap *) obj;

  /* Parse parameters */

  /* Check for smoothing parameters in the CONFIG file */
  astMapGet0I( kmap, "BOXCAR", &boxcar);
  if (boxcar > 0 ) do_boxcar = 1;

  /* Check for damping parameter on boxcar */
  astMapGet0D( kmap, "BOXFACT", &boxfact);
  if (boxfact > 0) do_boxfact = 1;

  /* If first iteration, set BOXCARD (this value will get decreased) */
  if( flags&SMF__DIMM_FIRSTITER ) {
    astMapPut0D( kmap, "BOXCARD", (double) boxcar, NULL );
  }

  astMapGet0D( kmap, "BOXCARD", &boxcard);
  /* Use damped boxcar for smoothing width */
  boxcar = (int) boxcard;

  /* Check for minimum boxcar width*/
  astMapGet0I( kmap, "BOXMIN", &boxmin);
  if (boxmin > 0 ) do_boxmin = 1;

  /* Bolo rejection parameters */
  astMapGet0D(kmap, "CORR_TOL", &corr_tol);
  astMapGet0D(kmap, "CORR_ABSTOL", &corr_abstol);
  astMapGet0D(kmap, "GAIN_TOL", &gain_tol);
  astMapGet0D(kmap, "GAIN_FGOOD", &gain_fgood);
  astMapGet0D(kmap, "GAIN_RAT", &gain_rat);
  astMapGet0I(kmap, "GAIN_BOX", &ival);
  gain_box = ival;
  astMapGet0D(kmap, "GAIN_ABSTOL", &gain_abstol);
  astMapGet0I(kmap, "NOTFIRST", &notfirst);
  astMapGet0I(kmap, "GAIN_IS_ONE", &nogains );

  if (*status == SAI__OK && gain_box == 0) {
    *status = SAI__ERROR;
    errRep( "", "COM.gain_box must be greater than 0", status );
  }

  if( do_boxcar ) {
    msgSeti("BOX",boxcar);
    msgOutif(MSG__VERB, " ", "    boxcar width ^BOX", status);
  }

  /* FIT_BOX defaults to GAIN_BOX (it should be null in the defaults file. */
  if( astMapGet0I(kmap, "FIT_BOX", &ival) ) {
     fit_box = ival;
     if( fit_box < gain_box ) {
        *status = SAI__ERROR;
       msgSeti( "F", (int) fit_box );
       msgSeti( "G", (int) gain_box );
       errRep( "", "COM.FIT_BOX (^F) must not be smaller than COM.GAIN_BOX "
               "(^G)", status );
     }
  } else {
     fit_box = gain_box;
  }


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

  /* Assert bolo-ordered data */
  smf_model_dataOrder( dat, NULL, chunk, SMF__RES|SMF__QUA|SMF__GAI|SMF__NOI, 0,
                       status );

  /* Obtain pointers to relevant smfArrays for this chunk */
  res = dat->res[chunk];
  qua = dat->qua[chunk];
  model = allmodel[chunk];
  if(dat->gai) {
    gai = dat->gai[chunk];

    /* Make a copy of gai_data (each subarray) for calculating convergence */
    gai_data_copy = astCalloc( gai->ndat, sizeof(*gai_data_copy), 0 );
    for( idx=0; (idx<gai->ndat)&&(*status==SAI__OK); idx++ ) {

      smf_get_dims( gai->sdata[idx],  NULL, NULL, NULL, NULL,
                    &thisndata, NULL, NULL, status);

      gai_data_copy[idx] = astCalloc( thisndata, sizeof(*gai_data_copy[idx]), 0 );

      gai_data = (gai->sdata[idx]->pntr)[0];

      memcpy( gai_data_copy[idx], gai_data, thisndata *
              sizeof(*gai_data_copy[idx]) );
    }
  }
  if(dat->noi) noi = dat->noi[chunk];

  /* The common mode signal is stored as a single 1d vector for all 4
     subarrays.  The corresponding smfData is at position 0 in the
     model sdata. */

  if( model->sdata[0] ) {
    /* Pointer to model data array */
    model_data = (model->sdata[0]->pntr)[0];

    /* Copy of model data array */
    model_data_copy = astCalloc( (model->sdata[0]->dims)[0],
                                 sizeof(*model_data_copy), 1 );
    if( *status == SAI__OK ) {
      memcpy( model_data_copy, model_data, (model->sdata[0]->dims)[0] *
	      sizeof(*model_data_copy) );
    }
    /* Temporary buffer to store weights */
    weight = astCalloc( (model->sdata[0]->dims)[0], sizeof(*weight), 0 );

    /* Find the number of blocks of time slices per bolometer. Each
       block contains "gain_box" time slices (except possibly for the
       last time slice which may contain more than gain_box but will
       be less than 2*gain_box). Each block of time slices from a
       single bolometer has its own gain, offset and correlation
       values. */
    nblock = (model->sdata[0]->dims)[0]/gain_box;
    if( nblock == 0 ) nblock = 1;

    /* Find the minimum number of good blocks required for a usable
       bolometer. */
    glim = ceil( nblock*gain_fgood );
    if( glim < 1 ) glim = 1;

  } else {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": Model smfData was not loaded!", status);
  }

  /* Allocate job data for threads */
  job_data = astCalloc( nw, sizeof(*job_data), 1 );











  /* Add the previous estimate of the common mode signal back on to the
     bolometer residuals. If we do not yet have an estimate of the common
     mode signal, then the bolometer residuals are left unchanged, but
     the gain values in the "gai" model are set to the rms value of the
     bolometer data stream. This is to ensure that the very high gain
     bolometers do not dominate the initial estimate of the common mode
     signal calculated below.
     --------------------------------------------------------------  */

  /* Loop over index in subgrp (subarray). */
  for( idx=0; idx<res->ndat; idx++ ) if (*status == SAI__OK ) {
    /* Obtain dimensions of the data */
    smf_get_dims( res->sdata[idx],  NULL, NULL, &thisnbolo, &thisntslice,
                  &thisndata, &bstride, &tstride, status);

    if(gai) {
      smf_get_dims( gai->sdata[idx],  NULL, NULL, NULL, NULL, NULL,
                    &gbstride, &gcstride, status);
      off_offset = 1*nblock*gcstride;
      corr_offset = 2*nblock*gcstride;
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

    /* Get pointers to data/quality/model */
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
        pdata->fit_box = fit_box;
        pdata->nblock = nblock;
        pdata->converged = NULL;
      }
    }

    for( ii=0; ii<nw; ii++ ) {
      /* Submit the job */
      pdata = job_data + ii;
      pdata->ijob = smf_add_job( wf, SMF__REPORT_JOB, pdata,
                                 smfCalcmodelComPar, NULL, status );
    }
    /* Wait until all of the submitted jobs have completed */
    smf_wait( wf, status );
  }




  /* Outer loop re-calculates common-mode until the list of "good" blocks
     converges. Without a fit for gain/offset this loop only happens once.
     Otherwise it keeps going until the correlation coefficients of the
     template fits settle down to a list with no N-sigma outliers.
     ---------------------------------------------------------------- */

  /* Allocate memory for a set of flags, one for each block, that
     indicate if the block has converged or not. */
  converged = astCalloc( nblock, sizeof( *converged ), 0 );

  fillgaps = 0;
  first = 1;
  quit = 0;
  while( !quit && (*status==SAI__OK) ) {


    /* Initialize to assumption that we'll finish this iteration */
    quit = 1;

    /* initialize model data and weights to 0 */
    memset(model_data,0,(model->sdata[0]->dims)[0]*sizeof(*model_data));
    memset(weight,0,(model->sdata[0]->dims)[0]*sizeof(*weight));

    /* If this is the first pass, initialise the convergence flags to
       zero. */
    if( first ) memset( converged, 0, nblock*sizeof( *converged ) );

    /* Loop over index in subgrp (subarray) */
    for( idx=0; (idx<res->ndat)&&(*status==SAI__OK); idx++ ) {

      /* Get pointers to data/quality/model */
      res_data = (res->sdata[idx]->pntr)[0];
      qua_data = (qua->sdata[idx]->pntr)[0];
      if( gai ) gai_data = (gai->sdata[idx]->pntr)[0];




      /* Calculate a new estimate of the common mode signal excluding
         blocks rejected on previous passes round the outer loop. The new
         common mode signal is the mean of the remaining scaled bolometer
         data (scaled using the inverse of the previous linear fit). The
         initial gain will have been set to the bolometer RMS value at
         the same time that the old common mode was added back on to the
         residuals (above).
         ---------------------------------------------------------- */

      /* Set up the job data and calculate new common mode */
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
        pdata->converged = converged;
      }

      for( ii=0; ii<nw; ii++ ) {
        /* Submit the job */
        pdata = job_data + ii;
        pdata->ijob = smf_add_job( wf, SMF__REPORT_JOB, pdata,
                                   smfCalcmodelComPar, NULL, status );
      }

      /* Wait until all of the submitted jobs have completed */
      smf_wait( wf, status );
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
    if( do_boxcar ) {
      /* Do the smooth */

      smf_tophat1D( model_data, ntslice, boxcar, NULL, 0, status );
    }


    /* Loop over all chunks. */
    for( idx=0; (idx<res->ndat)&&(*status==SAI__OK); idx++ ) {







    /* Now try to fit the remaining bolometer data to the new common mode
       estimate. Each bolometer is split into blocks of "gain_box" time
       slices, and a linear fit is performed between the bolometer values
       in a block and the corresponding values in the new common mode
       estimate.
       ---------------------------------------------------------------- */

      smf_get_dims( res->sdata[idx],  NULL, NULL, NULL, NULL, NULL, &bstride,
                    &tstride, status );

      /* Get pointers */
      res_data = (double *)(res->sdata[idx]->pntr)[0];
      qua_data = (unsigned char *)(qua->sdata[idx]->pntr)[0];
      if( noi ) {
        smf_get_dims( noi->sdata[idx],  NULL, NULL, NULL, &nointslice,
                      NULL, &noibstride, &noitstride, status);
        noi_data = (double *)(noi->sdata[idx]->pntr)[0];
      }

      if( gai ) {

        /* If GAIn model supplied, fit gain/offset of sky model to each
           block of time slices from each bolometer independently. These
           are stored as 3 group of planes, each plane holding nbolo samples:
           the first group holds bolo gains, the second offsets, and the
           third correlation coefficients. Each group contains "nblock"
           planes (i.e. one for each block of time slices).The fitting is a
           bottleneck, so we use multiple threads to handle blocks of bolos
           in parallel.
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
          pdata->gflat = gflat;
          pdata->idx = idx;
          pdata->model_data = model_data;
          pdata->nbolo = nbolo;
          pdata->ntslice = ntslice;
          pdata->operation = 2;
          pdata->res_data = res_data;
          pdata->tstride = tstride;
          pdata->qua_data = qua_data;
          pdata->ijob = -1;
          pdata->weight = weight;
          pdata->gain_box = gain_box;
          pdata->nogains = nogains;
          pdata->fit_box = fit_box;
          pdata->nblock = nblock;
          pdata->converged = converged;
        }

        for( ii=0; ii<nw; ii++ ) {
          /* Submit the job */
          pdata = job_data + ii;
          pdata->ijob = smf_add_job( wf, SMF__REPORT_JOB, pdata,
                                     smfCalcmodelComPar, NULL, status );
        }
        /* Wait until all of the submitted jobs have completed */
        smf_wait( wf, status );

        /* Identify blocks for which the fit is either too poor to use
           (i.e. low correlation), or very different from the fits for
           the other bolometers at the same block.
           --------------------------------------------------      */

        /* Allocate work space. */
        gcoeff = astCalloc( nbolo, sizeof(*gcoeff), 0 );
        corr = astCalloc( nbolo, sizeof(*corr), 0 );

        /* For debugging purposes, we record the reasons why any
           bolo-blocks are rejected. Initialise the number of bolo-blocks
           rejected for each reason. */
        memset( reason, 0, NREASON*sizeof( *reason ) );

        /* Loop over all blocks of time slices */
        newbad = 0;
        ntime = ntslice;
        int prev_block_converged = 1;

        for( iblock = 0; iblock < nblock && *status == SAI__OK; iblock++ ) {

          /* Skip blocks that have already converged. */
          if( ! converged[ iblock ] ) {

             size_t igbase = iblock*gcstride;
             size_t ibase = 0;
             int block_converged = 1;

             block_size = ( iblock < nblock - 1 ) ? gain_box : ntime;
             ntime -= block_size;

             allbad = 1;
             for( i = 0; i < nbolo; i++ ) {

               /* Copy correlation coefficients into an array that has VAL__BADD
                  set at locations of bad bolometers. Blocks with negative
                  gains or correlations are considered to be bad. */

               if( !( qua_data[ i*bstride ] & SMF__Q_BADB) ) {
                 gcoeff[ i ] = gai_data[ igbase ];
                 corr[ i ] = gai_data[ igbase + corr_offset ];

                 if( gcoeff[ i ] == BAD_FIT ) {
                    gcoeff[ i ] = VAL__BADD;
                    reason[ 0 ]++;
                    newbad++;
                 }

               } else {
                 gcoeff[ i ] = VAL__BADD;
               }

               if( gcoeff[ i ] != VAL__BADD ) {

                 if( gcoeff[ i ] <= 0.0 ) {
                    reason[ 1 ]++;
                    newbad++;
                    gcoeff[ i ] = VAL__BADD;
                    corr[ i ] = VAL__BADD;

                 } else if( corr[ i ] <= corr_abstol ) {
                    reason[ 2 ]++;
                    newbad++;
                    gcoeff[ i ] = VAL__BADD;
                    corr[ i ] = VAL__BADD;

                 } else {
                    gcoeff[ i ] = log( gcoeff[ i ] );
                    allbad = 0;
                 }

               } else {
                 corr[ i ] = VAL__BADD;
               }

               igbase += gbstride;
             }

             if( !allbad ) {
                smf_stats1D( corr, 1, nbolo, NULL, 0, 0, &cmean, &csig, &cgood,
                             status );
                if( *status == SMF__INSMP ) {
                   errAnnul( status );
                   allbad = 1;
                }
             } else {
                cgood = 0;
             }

             if( !allbad ) {
                smf_stats1D( gcoeff, 1, nbolo, NULL, 0, 0, &gmean, &gsig, &ggood, status );
                if( *status == SMF__INSMP ) {
                   errAnnul( status );
                   allbad = 1;
                   cgood = 0;
                }
             } else {
                ggood = 0;
             }

             /* Flag new bad bolometers in the current block of time slices */
             igbase = iblock*gcstride;
             ibase = iblock*gain_box*tstride;

             for( i = 0; i < nbolo && *status == SAI__OK; i++ ) {

               /* Skip blocks that have already been rejected., */
               if( gai_data[ igbase ] != VAL__BADD ) {

                 /* Reject blocks that 1) have a negative correlation or
                    gain (indicated by bad values), or 2) have an unusually
                    low correlation coeffcient, or 3) have an unusally low
                    or high gain. */
                 if( corr[ i ] == VAL__BADD || gcoeff[ i ] == VAL__BADD ||
                  ( (cmean - corr[ i ]) > corr_tol*csig ) ||    /*flag worse bols*/
                     ( corr[ i ] < corr_abstol ) ||
                     ( fabs( gcoeff[ i ]-gmean ) > gain_tol*gsig ) ||
                     ( fabs( gcoeff[ i ]-gmean ) > gain_abstol ) ) {

                   quit = 0;

                   for( j = 0; j < block_size; j++ ) {
                     qua_data[ ibase + j*tstride] |= SMF__Q_COM;
                   }

                   gai_data[ igbase ] = VAL__BADD;
                   gai_data[ igbase + off_offset ] = VAL__BADD;
                   gai_data[ igbase + corr_offset ] = VAL__BADD;

                   block_converged = 0;

                   /* Increment the number of rejected bolo-blocks, and
                      record the reasons why blocks are rejected. Don't need
                      to record VAL__BADD failures because they will have
                      already been included in earlier reasons. */
                   if( corr[ i ] != VAL__BADD && gcoeff[ i ] != VAL__BADD ){
                      newbad++;

                      if( ( (cmean - corr[ i ]) > corr_tol*csig ) ) {
                         reason[ 3 ]++;
                      } else if( corr[ i ] < corr_abstol ) {
                         reason[ 4 ]++;
                      } else if( fabs( gcoeff[ i ]-gmean ) > gain_tol*gsig ) {
                         reason[ 5 ]++;
                      } else if( fabs( gcoeff[ i ]-gmean ) > gain_abstol ) {
                         reason[ 6 ]++;
                      } else {
                         reason[ 7 ]++;
                      }
                   }
                 }
               }

               igbase += gbstride;
               ibase += bstride;
             }

             /* If the previous block has not yet converged, we cannot
                assume the current block has converged since the gains
                and offsets are interpolated between blocks. So set the
                block_converged flag to zero in this case. Also save  the
                original "block_converged" flag for use with the next block. */
             if( block_converged ) {
                if( !prev_block_converged ) block_converged = 0;
                prev_block_converged = 1;
             } else {
                prev_block_converged = 0;
             }

             /* If this block has converged, flag it as converged in the
             "converged" array. */
             if( block_converged ) {
                converged[ iblock ] = 1;

             /* If this block has not converged, flag it as unconverged in
                the "converged" array, and also flag the previous block as
                unconverged since the gains and offsets are interpolated
                between blocks. */
             } else {
                converged[ iblock ] = 0;
                if( iblock > 0 ) converged[ iblock - 1 ] = 0;
             }
          }
        }


        /* Now consider the consistency of the 'nblock' gain values for
           each individual bolometer. We reject blocks for which the a
           gain value is very different to the other gain values for the same
           bolometer. We also reject the entire boloemeter if it has too
           few good blocks. Loop round all remaining bolometers. */
        totgood = 0;
        for( i = 0; i < nbolo && *status == SAI__OK; i++ ) {
          size_t ibase = i*bstride;
          if( !( qua_data[ ibase ] & SMF__Q_BADB) ) {
            size_t igbase = i*gbstride;

            /* Find the count of blocks with good gain values, and find
               the mean gain value for this bolometer, use the
               correlation coefficients as weights. */
            gmean = 0.0;
            ggood = 0;
            csum = 0.0;

            for( iblock = 0; iblock < nblock; iblock++ ) {
              g = gai_data[ igbase ];
              c = gai_data[ igbase + corr_offset ];
              if( g != VAL__BADD && c != VAL__BADD ) {
                gmean += g*c;
                csum += c;
                ggood++;
              }
              igbase += gcstride;
            }

            if( ggood >= glim && csum > 0.0 ) {
              gmean /= csum;

              /* Get the maximum and minimum acceptable gain for this
                 bolometer. */
              gmax = gmean*gain_rat;
              gmin = gmean/gain_rat;

              /* Loop round all blocks, setting any bad that have gains
                 outside the above limits. */
              ntime = ntslice;
              igbase = i*gbstride;
              for( iblock = 0; iblock < nblock; iblock++ ) {
                block_size = ( iblock < nblock - 1 ) ? gain_box : ntime;
                ntime -= block_size;

                g = gai_data[ igbase ];
                if( g != VAL__BADD && ( g > gmax || g < gmin ) ) {

                  for( j = 0; j < block_size; j++ ) {
                    qua_data[ ibase ] |= SMF__Q_COM;
                    ibase += tstride;
                  }

                  gai_data[ igbase ] = VAL__BADD;
                  gai_data[ igbase + off_offset ] = VAL__BADD;
                  gai_data[ igbase + corr_offset ] = VAL__BADD;

                  newbad++;
                  ggood--;
                  quit = 0;

                  /* Flag this block and it's neighbours as unconverged. */
                  if( iblock > 0 ) converged[ iblock - 1 ] = 0;
                  converged[ iblock ] = 0;
                  if( iblock < nblock - 1 ) converged[ iblock + 1 ] = 0;

                  /* Record the reasons why blocks are rejected. */
                  if( g > gmax ) {
                     reason[ 8 ]++;
                  } else {
                     reason[ 9 ]++;
                  }

                } else {
                  ibase += block_size*tstride;
                }

                igbase += gcstride;
              }


            /* If there were insifficient good blocks for the current
            bolometer, set the whole bolometer bad. */
            } else {

              for( j = 0; j < ntslice; j++ ) {
                qua_data[ ibase ] |= ( SMF__Q_BADB | SMF__Q_COM );
                ibase += tstride;
              }

              igbase = i*gbstride;
              for( iblock = 0; iblock < nblock; iblock++ ) {
                gai_data[ igbase  ] = VAL__BADD;
                gai_data[ igbase + off_offset ] = VAL__BADD;
                gai_data[ igbase + corr_offset ] = VAL__BADD;
                igbase += gcstride;
              }

              newbad += ggood;
              reason[ 10 ] += ggood;

              quit = 0;
              ggood = 0;
            }

            totgood += ggood;

          }
        }

        nconverged = 0;
        for( iblock = 0; iblock < nblock; iblock++ ) {
           if( converged[ iblock ] ) nconverged++;
        }
        msgSeti( "N", nconverged );
        msgSeti( "M", nblock );
        msgOutif( MSG__VERB, "",
                  "    ^N out of ^M time-slice blocks have now converged", status );

        msgSeti( "NEW", newbad );
        msgOutif( MSG__VERB, "",
                  "    flagged ^NEW new bad bolo time-slice blocks", status );

        for( ireason = 0; ireason < NREASON; ireason++ ) {
           if( reason[ ireason ] > 0 ) {
              msgSeti( "N", reason[ ireason ] );
              msgSetc( "T", reason_text[ ireason ] );
              msgOutif( MSG__DEBUG, "",
                        "       (^N were flagged because ^T)", status );
           }
        }

        msgSeti( "NG", totgood );
        msgSeti( "T", nblock*nbolo );
        msgOutif( MSG__VERB, "",
                  "    Out of ^T, ^NG bolo time-slice blocks are still good",
                  status );
        if( newbad ) fillgaps = 1;

        gcoeff = astFree( gcoeff );
        corr = astFree( corr );

      } else {
        /* If we're not fitting a gain and offset, just remove common-mode
           immediately and quit while loop. */
        quit = 1;
        for( i=0; i<nbolo; i++ ) {
          size_t ibase = i*bstride;
          for( j=0; j<ntslice; j++ ) {
            size_t ijindex = ibase + j*tstride;

            /* update the residual */
            if( !(qua_data[ijindex]&SMF__Q_MOD) ) {
              if( model_data[j] != VAL__BADD ) {
                res_data[ijindex] -= model_data[j];

                /* also measure contribution to dchisq */
                if( noi && !(qua_data[ijindex]&SMF__Q_GOOD)) {
                  dchisq += (model_data[j] - model_data_copy[j]) *
                    (model_data[j] - model_data_copy[j]) /
                    noi_data[i*noibstride + (j%nointslice)*noitstride];
                  ndchisq++;
                }
              } else {
                 qua_data[ijindex] |= SMF__Q_COM;
              }
            }
          }
        }
      }
    }

    if( !quit ) {
      /* around we go again... */
      msgOutif( MSG__DEBUG, "", "    Common mode not yet converged",
                status );
    } else {
      msgOutif( MSG__DEBUG, "", "    Common mode converged",
                status );
    }

    first = 0;
  }

/* Free the convergence flags. */
  converged = astFree( converged );



 /* Once the common mode converged and if we're fitting the common mode
    gain and offset, form the residuals by removing the scaled common mode
    estimate from the bolometer data.
    ----------------------------------------------------------------- */
  if( gai) {

    /*  Allocate work space. */
    woff = astMalloc( sizeof( *woff )*ntslice );
    wg = astMalloc( sizeof( *wg )*ntslice );
    woff_copy = astMalloc( sizeof( *woff_copy )*ntslice );
    wg_copy = astMalloc( sizeof( *wg_copy )*ntslice );

    for( idx=0; (idx<res->ndat)&&(*status==SAI__OK); idx++ ) {
      smf_get_dims( res->sdata[idx],  NULL, NULL, NULL, NULL, NULL, &bstride,
                    &tstride, status );

      /* Get pointers */
      res_data = (double *)(res->sdata[idx]->pntr)[0];
      qua_data = (unsigned char *)(qua->sdata[idx]->pntr)[0];
      smf_get_dims( gai->sdata[idx],  NULL, NULL, NULL, NULL, NULL,
                    &gbstride, &gcstride, status);
      gai_data = (gai->sdata[idx]->pntr)[0];
      if( noi ) {
        smf_get_dims( noi->sdata[idx],  NULL, NULL, NULL, &nointslice,
                      NULL, &noibstride, &noitstride, status);
        noi_data = (double *)(noi->sdata[idx]->pntr)[0];
      }

      /* Loop round all bolometers. */
      for( j = 0 ; j < nbolo && *status == SAI__OK; j++ ) {

        /* Initialise the index of the first time slice for the current
           bolometer within the res_data and qua_data arrays. */
        size_t ijindex = j*bstride;

        /* Skip bad bolometers */
        if( !(qua_data[ ijindex ] & SMF__Q_BADB ) ) {
          size_t inbase = j*noibstride;

          /* Get the gain and offset for each time slice. */
          smf_gandoff( j, 0, ntslice-1, ntslice, gbstride, gcstride,
                       gai_data, nblock, gain_box, wg, woff, status );

          /* Also get the previous gain and offset for each time slice. */
          smf_gandoff( j, 0, ntslice-1, ntslice, gbstride, gcstride,
                       gai_data_copy[ idx ], nblock, gain_box, wg_copy,
                       woff_copy, status );

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
     operations may not ignored flagged bolometer values but may instead
     use the bolometer values literally. */
  if( fillgaps ) {
    for( idx=0; (idx<res->ndat)&&(*status==SAI__OK); idx++ ) {
      qua_data = (unsigned char *)(qua->sdata[idx]->pntr)[0];
      smf_fillgaps( wf, res->sdata[idx], qua_data, SMF__Q_COM, status );
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

  /* Print normalized residual chisq for this model */
  if( (*status==SAI__OK) && noi && (ndchisq>0) ) {
    dchisq /= (double) ndchisq;
    msgOutiff( MSG__VERB, "", "    normalized change in model: %lf", status,
               dchisq );
  }

  /* Clean up */
  if( weight)  weight = astFree( weight );
  if( model_data_copy ) model_data_copy = astFree( model_data_copy );

  if( gai_data_copy ) {
    for( idx=0; idx<gai->ndat; idx++ ) {
      if( gai_data_copy[idx] ) {
        gai_data_copy[idx] = astFree( gai_data_copy[idx] );
      }
    }
    gai_data_copy = astFree( gai_data_copy );
  }

  /* Clean up the job data array */
  if( job_data ) {
    job_data = astFree( job_data );
  }

  if( kmap ) kmap = astAnnul( kmap );
  if( gkmap ) gkmap = astAnnul( gkmap );
}


