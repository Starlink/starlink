/*
*+
*  Name:
*     smf_correct_steps

*  Purpose:
*     Locate and repair DC steps

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_correct_steps( smfWorkForce *wf, smfData *data,
*                        unsigned char *quality,
*                        double dcthresh, double dcthresh2, dim_t dcbox,
*                        int dcflag, size_t *nsteps, int *status )

*  Arguments:
*     wf = smfWorkForce * (Given)
*        Pointer to a pool of worker threads (can be NULL)
*     data = smfData * (Given and Returned)
*        The data that will be repaired (in-place)
*     quality = unsigned char * (Given and Returned)
*        If set, use this buffer instead of QUALITY associated with data.
*        If NULL, use the QUALITY associated with data. Locations of steps
*        will have bit SMF__Q_JUMP set.
*     dcthresh = double (Given)
*        N-sigma threshold for primary DC jump to be detected.
*     dcthresh2 = double (Given)
*        N-sigma threshold for secondary DC jump to be detected.
*        Secondary jumps are those induced by a primary jump in another 
*        bolometer. Secondary jumps are assumed to occur at the same time 
*        slice as the primary jump that induces them. dcthresh2 should 
*        have a lower value than dcthresh.
*     dcbox = dim_t (Given)
*        Length of box (in samples) over which to calculate statistics
*     dcflag = int (Given)
*        if 0 handle all bolos independently and attempt to fix steps
*        if 1 just flag entire bolo as bad if step encountered
*        if 2 identify steps, and then repair/flag all bolometers at those 
*        spots that show a dc step of greater than "dcthresh2" times the
*        noise level.
*     nsteps = size_t* (Returned)
*        Number of DC steps encountered (number of flagged bolos if dcflag
*        set). Can be NULL.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     First estimate white-noise level as r.m.s. in box of length
*     dcbox.  Then calculate running averages in two adjacent
*     intervals of length dcbox. Jumps are flagged and corrected at
*     peak values of difference between the averages in two
*     intervals. If dcflag is set to 1, instead of repairing the
*     step just flag entire bolometer as SMF__Q_BADB.

*  Notes:

*  Authors:
*     Edward Chapin (UBC)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     DSB: David Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2008-03-05 (EC):
*        Initial Version
*     2008-04-01 (EC):
*        - Use smf_quick_noise to estimate signal r.m.s.
*        - wrap-around at ends of the bolometer data
*     2008-10-16 (EC):
*        - remove wrapping at ends of bolo data
*        - option to flag bolo as bad if steps detected
*     2009-07-23 (TIMJ):
*        Use msgFlevok rather than msgIflev
*     2009-11-17 (EC):
*        stridify and fix numerous array index bugs
*     2010-01-08 (EC):
*        add flagging of all bolos at step locations
*     2010-01-08 (TIMJ):
*        Private routines must be static to hide them from others.
*     2010-01-14 (EC):
*        In single bolo case flag 2*box window instead of single sample
*     2010-01-20 (DSB):
*        - Added argument dcthresh2. 
*        - Use threads.
*     2010-01-21 (EC):
*        Fixed up to work in single-thread mode
*     2010-02-25 (TIMJ):
*        Fix 32-bit incompatibility.

*  Copyright:
*     Copyright (C) 2009-2010 Science and Technology Facilities Council.
*     Copyright (C) 2005-2006 Particle Physics and Astronomy Research Council.
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

/* Other includes */
#include <math.h>


#define FUNC_NAME "smf_correct_steps"

/* Structure containing information about blocks of bolos to be
   corrected by each thread. */
typedef struct smfCorrectStepsData {
  dim_t dcbox;                  /* Size of statistics box */
  dim_t ntslice;                /* Number of time slices */
  double *alljump;              /* Thread-common buffer containing DC jumps */
  double *dat;                  /* Pointer to bolo data */
  double dcthresh;              /* Threshold for primary steps */
  int dcflag;                   /* Correction method */
  pthread_mutex_t *alljump_mutex;/* Mutex guarding access to "alljump" */
  size_t b1;                    /* Index of first bolometer to be filledd */
  size_t b2;                    /* Index of last bolometer to be filledd */
  size_t bstride;               /* bolo stride */
  size_t iend;                  /* Index end of data stream */
  size_t istart;                /* Index start of data stream */
  size_t ns;                    /* Number of steps encountered */
  size_t tstride;               /* time slice stride */
  smfData *data;                /* Supplied data structure */
  unsigned char *qua;           /* Pointer to quality array */
} smfCorrectStepsData;


typedef struct smfCorrectStepsData2 {
  dim_t dcbox;                  /* Size of statistics box */
  dim_t ntslice;                /* Number of time slices */
  double *alljump;              /* Thread-common buffer containing DC jumps */
  double *dat;                  /* Pointer to bolo data */
  double dcstep;                /* Threshold for secondary steps */
  int dcflag;                   /* Correction method */
  size_t b1;                    /* Index of first bolometer to be filledd */
  size_t b2;                    /* Index of last bolometer to be filledd */
  size_t bstride;               /* bolo stride */
  size_t iend;                  /* Index end of data stream */
  size_t istart;                /* Index start of data stream */
  size_t tstride;               /* time slice stride */
  unsigned char *qua;           /* Pointer to quality array */
} smfCorrectStepsData2;



/* Prototype for the function to be executed in each thread. */
static void smfCorrectStepsParallel( void *job_data_ptr, int *status );
static void smfCorrectStepsParallel2( void *job_data_ptr, int *status );


/* -------------------------------------------------------------------------- */
/* Local routine for correcting baseline steps */

static void smf__correct_steps_baseline( double *dat, unsigned char *qua,
                                         dim_t ntslice, size_t tstride,
                                         double *alljump ) {
  double baseline;
  size_t i;

  baseline = 0;
  for( i=1; i<ntslice; i++ ) {
    if( alljump[i] ) {
      /* Update the baseline at each sample in which jumps occured */
      baseline += alljump[i];

      /* Flag the jump in QUALITY */
      qua[i*tstride] |= SMF__Q_JUMP;
    }

    /* Correct the data by the current baseline estimate */
    dat[i*tstride] -= baseline;
  }
}

/* ------------------------------------------------------------------------- */
/* Public routine */

void smf_correct_steps( smfWorkForce *wf, smfData *data, unsigned char *quality,
                        double dcthresh, double dcthresh2, dim_t dcbox, 
                        int dcflag, size_t *nsteps, int *status ) {

  /* Local Variables */
  double *alljump=NULL;         /* Buffer containing DC jumps */
  pthread_mutex_t alljump_mutex;/* Mutex guarding access to "alljump" */
  dim_t bpt;                    /* Number of bolos per thread */
  double *dat=NULL;             /* Pointer to bolo data */
  double dcstep;                /* Size of DC steps to detect */
  size_t bstride;               /* Bolo stride */
  dim_t i;                      /* Loop Counter */
  size_t iend;                  /* Index end of data stream */
  size_t istart;                /* Index start of data stream */
  dim_t nbolo=0;                /* Number of bolometers */
  size_t ns=0;                  /* Number of steps encountered */
  dim_t ntslice=0;              /* Number of time slices */
  unsigned char *qua=NULL;      /* Pointer to quality flags */
  size_t tstride;               /* Bolo stride */
  smfCorrectStepsData *job_data;/* Structures holding data for worker threads */
  smfCorrectStepsData2 *job_data2;/* Structures holding data for worker threads */
  smfCorrectStepsData *pdata;   /* Pointer to data for next worker thread */
  smfCorrectStepsData2 *pdata2; /* Pointer to data for next worker thread */

  /* Main routine */
  if (*status != SAI__OK) return;

  if (!smf_dtype_check_fatal( data, NULL, SMF__DOUBLE, status )) return;

  /* Assert bolo-ordered data to make life easier */
  smf_dataOrder( data, 0, status );

  /* Pointers to data and quality */
  dat = data->pntr[0];
  if( quality ) {
    qua = quality;
  } else {
    qua = data->pntr[2];
  }

  if( !qua ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": No valid QUALITY array was provided", status );
    return;
  }

  if( !dat ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": smfData does not contain a DATA component",
            status );
    return;
  }

  if( *status == SAI__OK ) {
    /* obtain data dimensions */
    smf_get_dims( data,  NULL, NULL, &nbolo, &ntslice, NULL, &bstride, &tstride,
                  status );

    /* If data stream too short for box size generate error */
    if( (dcbox*2) > ntslice ) {
      *status = SAI__ERROR;
      msgSeti("NTSLICE",ntslice);
      msgSeti("DCBOX",ntslice);
      errRep("", FUNC_NAME
             ": Can't find jumps: ntslice=^NTSLICE, must be > dcbox"
             "(=^DCBOX)*2", status);
    }

    /* Check for valid threshold */
    if( dcthresh <= 0 ) {
      *status = SAI__ERROR;
      msgSeti("DCTHRESH",dcthresh);
      errRep("", FUNC_NAME
             ": Can't find jumps: dcthresh (^dcthresh) must be > 0", status);
    }
  }

  /* Repair DC steps */
  if( dcbox && dcthresh && (*status == SAI__OK) ) {

    /* Determine how many bolometers to process in each thread, and create
       the structures used to pass data to the threads. */
    if( wf ) {
       bpt = nbolo/wf->nworker;
       if( wf->nworker*bpt < nbolo ) bpt++;
       job_data = astMalloc( sizeof( smfCorrectStepsData )*wf->nworker );
    } else {
       bpt = nbolo;
       job_data = astMalloc( sizeof( smfCorrectStepsData ) );
    }

    /* For dcflag==2, all threads need to use the same "alljump" buffer,
       so allocate it now, and initialise a mutex to guard access to it. 
       For other dclfags values, each thread creates its own alljump array. */
    if( dcflag == 2 ) {
      alljump = smf_malloc( ntslice, sizeof(*alljump), 1, status );
      smf_mutex_init( &alljump_mutex, status );
    } else {
      alljump = NULL;
    }

    /* identify first and last samples before/after padding+apodization */
    smf_get_goodrange( qua, ntslice, 1, SMF__Q_PAD|SMF__Q_APOD,
                       &istart, &iend, status );

    /* Begin a job context. */
    smf_begin_job_context( wf, status );

    /* Loop over bolometer in groups of "bpt". */
    pdata = job_data;
    for( i = 0; i < nbolo; i += bpt, pdata++ ) {
      /* Store information for this group in the next smfCorrectStepsData
         structure. */
      pdata->alljump = alljump;
      pdata->alljump_mutex = &alljump_mutex;
      pdata->b1 = i;
      pdata->b2 = i + bpt - 1;
      pdata->bstride = bstride;
      pdata->dat = dat;
      pdata->data = data;
      pdata->dcbox = dcbox;
      pdata->dcflag = dcflag;
      pdata->dcthresh = dcthresh;
      pdata->iend = iend;
      pdata->istart = istart;
      pdata->ntslice = ntslice;
      pdata->qua = qua;
      pdata->tstride = tstride;

      /* Submit a job to the workforce to process this group of bolometers. */
      (void) smf_add_job( wf, 0, pdata, smfCorrectStepsParallel, NULL, 
                          status );
    }

    /* Wait until all jobs in the current job context have completed. */
    smf_wait( wf, status );

    /* End the job context. */
    smf_end_job_context( wf, status );

    /* Add up the number of samples flagged by each thread. */
    pdata = job_data;
    for( i = 0; i < nbolo; i += bpt, pdata++ ) ns += pdata->ns;

    /* Free resouces. */
    if( dcflag == 2 ) {
      pthread_mutex_destroy( &alljump_mutex );
    }
    job_data = astFree( job_data );


    /* If dcflag==2, go back to all locations of steps in each bolometer
       and correct / flag (handle small DC steps correlated with big ones) */
    if( (dcflag == 2) && (*status==SAI__OK) ) {

      /* Set the minimum step size to be corrected to dcthresh2*noise level. */
      dcstep = dcthresh2/dcthresh;

      /* Create the structures used to pass data to the threads. */
      if( wf ) {
         job_data2 = astMalloc( sizeof( smfCorrectStepsData2 )*wf->nworker );
      } else {
         job_data2 = astMalloc( sizeof( smfCorrectStepsData2 ) );
      }

      /* Begin a job context. */
      smf_begin_job_context( wf, status );

      /* Loop over bolometer in groups of "bpt". */
      pdata2 = job_data2;
      for( i = 0; i < nbolo; i += bpt, pdata2++ ) {

        /* Store information for this group in the next smfCorrectStepsData
           structure. */
        pdata2->alljump = alljump;
        pdata2->b1 = i;
        pdata2->b2 = i + bpt - 1;
        pdata2->bstride = bstride;
        pdata2->dat = dat;
        pdata2->dcbox = dcbox;
        pdata2->dcstep = dcstep;
        pdata2->iend = iend;
        pdata2->istart = istart;
        pdata2->ntslice = ntslice;
        pdata2->qua = qua;
        pdata2->tstride = tstride;
  
        /* Submit a job to the workforce to process this group of bolos. */
        (void) smf_add_job( wf, 0, pdata2, smfCorrectStepsParallel2, NULL, 
                            status );
      }

      /* Wait until all jobs in the current job context have completed. */
      smf_wait( wf, status );

      /* End the job context. */
      smf_end_job_context( wf, status );

      /* Free resouces. */
      job_data2 = astFree( job_data2 );
    }

    /* Return nsteps if requested */
    if( nsteps ) {
      msgOutiff( MSG__DEBUG, "", FUNC_NAME ": %zd bolos flagged", status,
                 ns );
      *nsteps = ns;
    }

    /* Clean up */
    alljump = smf_free( alljump, status );
  }

}











/* Function to be executed in thread: find steps in all bolos from b1 to b2 */

static void smfCorrectStepsParallel( void *job_data_ptr, int *status ) {

/* Local Variables */
  double *alljump=NULL;         /* Buffer containing DC jumps */
  size_t base;                  /* Index to start of current bolo */
  size_t b1;                    /* Index of first bolometer to be filledd */
  size_t b2;                    /* Index of last bolometer to be filledd */
  double *dat=NULL;             /* Pointer to bolo data */
  smfData *data;                /* Input data structure */
  double dcstep;                /* Size of DC steps to detect */
  size_t bstride;               /* Bolo stride */
  dim_t dcbox;                  /* Size of statistics box */
  double dcthresh;              /* Threshold for primary steps */
  int dcflag;                   /* Correction method */
  dim_t i;                      /* Loop Counter */
  size_t iend;                  /* Index end of data stream */
  int injump;                   /* Flag for DC jump detection */
  int isbad;                    /* Set if bolo is bad */
  size_t istart;                /* Index start of data stream */
  dim_t j;                      /* Loop Counter */
  dim_t k;                      /* Loop Counter */
  double maxdiff;               /* Max difference between mean1 and mean2 */
  dim_t maxind;                 /* index to location of maxdiff */
  double mean1;                 /* Box means to search for DC steps */
  double mean2;                 /* "    "                           */
  size_t ns=0;                  /* Number of steps encountered */
  size_t nmean1;                /* Number of samples in mean1 */
  size_t nmean2;                /* Number of samples in mean1 */
  dim_t ntslice=0;              /* Number of time slices */
  smfCorrectStepsData *pdata = NULL;/* Pointer to job data */
  unsigned char *qua=NULL;      /* Pointer to quality flags */
  size_t tstride;               /* Bolo stride */
  size_t wherebad=0;            /* Index causing bad bolo */

  /* Pointer to the structure holding information needed by this thread. */
  pdata = (smfCorrectStepsData *) job_data_ptr;

  /* Copy data from the above structure into local variables. */
  b1 = pdata->b1;
  b2 = pdata->b2;
  bstride = pdata->bstride;
  dat = pdata->dat;
  data = pdata->data;
  dcbox = pdata->dcbox;
  dcflag = pdata->dcflag;
  dcthresh = pdata->dcthresh;
  iend = pdata->iend;
  istart = pdata->istart;
  ntslice = pdata->ntslice;
  qua = pdata->qua;
  tstride = pdata->tstride;

  /* If we are using an alljump array that is common to all threads, we
     need to guard access to it using a mutex (provided in "pdata"). If
     we are using an alljump array specific to this thread, create it. */
  if( dcflag == 2 ) {
    alljump = pdata->alljump;
  } else {
    alljump = smf_malloc( ntslice, sizeof(*alljump), 1, status );
  }

  /* Loop over bolometers */
  for( i = b1; i <= b2 && *status==SAI__OK; i++ ) {
    base = i*bstride;
    isbad = 0;

    /* Continue if bolo stream is not flagged bad */
    if( !(qua[base] & SMF__Q_BADB) && (*status == SAI__OK) ) {

      /* initial conditions for jump detection */
      smf_stats1D( dat+base+istart*tstride, tstride, dcbox,
                   qua+base+istart*tstride, 0, SMF__Q_MOD, &mean1, NULL,
                   &nmean1, status);
      smf_stats1D( dat+base+(istart+dcbox)*tstride, tstride, dcbox,
                   qua+base+(istart+dcbox)*tstride, 0, SMF__Q_MOD,
                   &mean2, NULL, &nmean2, status );

      /* Estimate expected rms in a box as the bolo rms */
      dcstep = smf_quick_noise( data, i, dcbox, 10, qua, SMF__Q_MOD,
                                status ) * dcthresh;

      if( *status == SAI__OK ) {
        /* If handling all bolos independently, zero alljump here */
        if( dcflag != 2 ) {
          memset( alljump, 0, ntslice*sizeof(*alljump) );
        }

        injump = 0;  /* jump occured somewhere in boxes */
        maxdiff = 0; /* max difference between mean1 and mean2 for jump */
        maxind = 0;  /* index to location of maxdiff */

        /* counter is at mean1/mean2 boundary -- location potential jumps.
           Counter runs from istart+dcbox --> ntslice-dcbox. */

        for( j=istart+dcbox; j<=(iend-dcbox); j++ ) {

          /* is the difference between the two boxes significant? */
          if( fabs(mean2 - mean1) >= dcstep ) {

            if( !isbad ) {
              /* Found a new jump */
              isbad = 1;
              wherebad = j;
              ns++;
            }

            if( !injump ) {
              /* Starting new jump, initialize search */
              maxdiff = mean2 - mean1;
              maxind = j;
              injump = 1;
            } else if( fabs(mean2 - mean1) > fabs(maxdiff) ) {
              /* Update the search for the maximum step size */
              maxdiff = mean2 - mean1;
              maxind = j;
            }

          } else {
            /* If difference is small, but injump is set, that means we've
               finished the search for the last step */
            if( injump ) {

              /* if dcflag==2 first check to see if a jump near this
                 spot was already flagged. Only record the biggest one */

              if( dcflag == 2 ) {
                double biggest = maxdiff;
                size_t wherebiggest = maxind;

                /* Lock the alljump mutex so that we have exclusive
                   access to the alljump array. */
                smf_mutex_lock( pdata->alljump_mutex, status );

                /* Find biggest in interval */
                for( k=maxind-dcbox; k<maxind+dcbox; k++ ) {
                  if( alljump[k] && (fabs(alljump[k]) > fabs(biggest))  ) {
                    biggest = alljump[k];
                    wherebiggest = k;
                  }
                }

                /* Zero entire interval and record only biggest */
                for( k=maxind-dcbox; k<maxind+dcbox; k++ ) {
                  alljump[k] = 0;
                }

                alljump[wherebiggest] = biggest;
                wherebad = wherebiggest;

                /* Unlock the alljump mutex so that other threads can 
                   access the alljump array. */
                smf_mutex_unlock( pdata->alljump_mutex, status );

              } else {
                alljump[maxind] = maxdiff;
                /* update wherebad to more precise location */
                wherebad = maxind;
              }
                injump = 0;

            }
          }

          /* Move along the boxes and update the mean estimates */
          if( !(qua[base + (j-dcbox)*tstride] & SMF__Q_MOD) ) {
            /* Drop sample at j-dcbox from mean1 */
            mean1 = (nmean1*mean1 - dat[base + (j-dcbox)*tstride]) /
              (nmean1-1);
            nmean1 --;
          }
          if( !(qua[base + j*tstride] & SMF__Q_MOD) ) {
            /* Move sample at j from mean2 to mean1 */
            mean1 = (nmean1*mean1 + dat[base+j*tstride]) / (nmean1+1);
            nmean1 ++;

            mean2 = (nmean2*mean2 - dat[base+j*tstride]) / (nmean2-1);
            nmean2 --;

          }
          if( !(qua[base+((j+dcbox+1)*tstride)] & SMF__Q_MOD) ) {
            /* Add sample at j+dcbox+1 to mean1 */
            mean2 = (nmean2*mean2 + dat[base+((j+dcbox+1)*tstride)]) /
              (nmean2+1);
            nmean2 ++;
          }

          /* Don't need to continue if bad bolo, and dcflag set to 1 */
          if( (dcflag==1) && isbad ) break;
        }

        /* calculate the new corrected baseline if requested */
        if( !dcflag ) {
          smf__correct_steps_baseline( dat+i*bstride+istart*tstride,
                                       qua+i*bstride+istart*tstride,
                                       iend-istart+1, tstride, alljump+istart);
        }
      }
    }

    /* If we got a SMF__INSMP, flag entire bolometer as bad and annul */
    if( *status == SMF__INSMP ) {
      errAnnul( status );
      isbad = 1;
    }

    if( isbad && (dcflag==1) ) {
      msgOutiff( MSG__DEBUG, "", FUNC_NAME ": flagging bad bolo %" DIM_T_FMT " at %zd",
                 status, i, wherebad );
      for(j=0; j<ntslice; j++) {
        qua[base+j*tstride] |= SMF__Q_BADB;
      }
    }

    /* Flag entire 2*DCBOX window */
    for( j=wherebad-dcbox; j<wherebad+dcbox; j++ ) {
      qua[base+j*tstride] |= SMF__Q_JUMP;
    }
  }

  /* Free any thread local alljump array */
  if( dcflag != 2 ) alljump = smf_free( alljump, status );

  /* Return the number of flagged samples */
  pdata->ns = ns;

}





/* Function to be executed in thread: find secondary steps in all bolos from b1 to b2 */

static void smfCorrectStepsParallel2( void *job_data_ptr, int *status ) {

/* Local Variables */
  double *thisjump=NULL;
  double *alljump=NULL;         /* Buffer containing DC jumps */
  size_t b1;                    /* Index of first bolometer to be filledd */
  size_t b2;                    /* Index of last bolometer to be filledd */
  int correct;                  /* Are there any secondary steps to correct? */
  double *dat=NULL;             /* Pointer to bolo data */
  double dcstep;                /* Size of DC steps to detect */
  size_t bstride;               /* Bolo stride */
  dim_t dcbox;                  /* Size of statistics box */
  dim_t i;                      /* Loop Counter */
  size_t iend;                  /* Index end of data stream */
  size_t istart;                /* Index start of data stream */
  dim_t j;                      /* Loop Counter */
  dim_t k;                      /* Loop Counter */
  double mean1;                 /* Box means to search for DC steps */
  double mean2;                 /* "    "                           */
  size_t nmean1;                /* Number of samples in mean1 */
  size_t nmean2;                /* Number of samples in mean1 */
  dim_t ntslice=0;              /* Number of time slices */
  smfCorrectStepsData2 *pdata2 = NULL;/* Pointer to job data */
  unsigned char *qua=NULL;      /* Pointer to quality flags */
  size_t tstride;               /* Bolo stride */

  /* Pointer to the structure holding information needed by this thread. */
  pdata2 = (smfCorrectStepsData2 *) job_data_ptr;

  /* Copy data from the above structure into local variables. */
  alljump = pdata2->alljump;
  b1 = pdata2->b1;
  b2 = pdata2->b2;
  bstride = pdata2->bstride;
  dat = pdata2->dat;
  dcbox = pdata2->dcbox;
  dcstep = pdata2->dcstep;
  iend = pdata2->iend;
  istart = pdata2->istart;
  ntslice = pdata2->ntslice;
  qua = pdata2->qua;
  tstride = pdata2->tstride;

  /* Allocate local work array. */
  thisjump = smf_malloc( ntslice, sizeof(*thisjump), 1, status );

  /* Loop over bolometers */
  for( i = b1; i <= b2 && *status==SAI__OK; i++ ) {
    if( !(qua[i*bstride] & SMF__Q_BADB) ) {

      /* Loop over time slices for this bolometer and calc thisjump */
      memset( thisjump, 0, ntslice*sizeof(*thisjump) );
      correct = 0;
      for( j=0; j<ntslice; j++ ) {

        /* Jump previously found. Measure mean before and after */
        if( alljump[j] ) {
          smf_stats1D( dat+i*bstride+(j-dcbox)*tstride, tstride, dcbox,
                       qua+i*bstride+(j-dcbox)*tstride, 0, SMF__Q_MOD, &mean1,
                       NULL, &nmean1, status);

          smf_stats1D( dat+i*bstride+j*tstride, tstride, dcbox,
                       qua+i*bstride+j*tstride, 0, SMF__Q_MOD, &mean2,
                       NULL, &nmean2, status);
          if( *status == SMF__INSMP ) {
            /* If insufficient samples just annul and continue */
            errAnnul( status );
          } else {
            thisjump[j] = mean2 - mean1;
          }
  
          /* Flag entire 2*DCBOX window if the jump is significant,
             and set a flag indicating we need to call the correction
             function. */
          if( fabs( thisjump[j] ) > dcstep ) {
            for( k=j-dcbox; k<j+dcbox; k++ ) {
              qua[i*bstride+k*tstride] |= SMF__Q_JUMP;
            }
            correct = 1;
  
          /* Otherwise set the jump magnitude to zero to prevent it
             introducing any correction. */
          } else {
             thisjump[j] = 0.0;
          }
        }
      }
  
      /* Permform the correction only if any significant jumps were found in this
         bolometer. */
      if( correct ) {
         smf__correct_steps_baseline( dat+i*bstride+istart*tstride,
                                      qua+i*bstride+istart*tstride,
                                      iend-istart+1, tstride, thisjump+istart );
      }
    }
  } 

  /* Free resources */
  thisjump = smf_free( thisjump, status );
}



