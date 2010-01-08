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
*     smf_correct_steps( smfData *data, unsigned char *quality,
*                        double dcthresh, dim_t dcbox, int dcflag,
*                        size_t *nsteps, int *status )

*  Arguments:
*     data = smfData * (Given and Returned)
*        The data that will be repaired (in-place)
*     quality = unsigned char * (Given and Returned)
*        If set, use this buffer instead of QUALITY associated with data.
*        If NULL, use the QUALITY associated with data. Locations of steps
*        will have bit SMF__Q_JUMP set.
*     dcthresh = double (Given)
*        N-sigma threshold for DC jump to be detected
*     dcbox = dim_t (Given)
*        Length of box (in samples) over which to calculate statistics
*     dcflag = int (Given)
*        if 0 handle all bolos independently and attempt to fix steps
*        if 1 just flag entire bolo as bad if step encountered
*        if 2 identify steps, and then repair/flag ALL bolometers at those spots
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
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2009 Science and Technology Facilities Council.
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

/* -------------------------------------------------------------------------- */
/* Local routine for correcting baseline steps */

void smf_correct_steps_baseline( double *dat, unsigned char *qua,
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

void smf_correct_steps( smfData *data, unsigned char *quality,
                        double dcthresh, dim_t dcbox, int dcflag,
                        size_t *nsteps, int *status ) {

  /* Local Variables */
  double *alljump=NULL;         /* Buffer containing DC jumps */
  size_t base;                  /* Index to start of current bolo */
  double *dat=NULL;             /* Pointer to bolo data */
  double dcstep;                /* Size of DC steps to detect */
  size_t bstride;               /* Bolo stride */
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
  dim_t nbolo=0;                /* Number of bolometers */
  size_t ns=0;                  /* Number of steps encountered */
  dim_t nmean1;                 /* Number of samples in mean1 */
  dim_t nmean2;                 /* Number of samples in mean1 */
  dim_t ntslice=0;              /* Number of time slices */
  unsigned char *qua=NULL;      /* Pointer to quality flags */
  size_t tstride;               /* Bolo stride */
  size_t wherebad=0;            /* Index causing bad bolo */

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

    /* allocate alljump buffer */
    alljump = smf_malloc( ntslice, sizeof(*alljump), 1, status );

    /* identify first and last samples before/after padding+apodization */
    smf_get_goodrange( qua, ntslice, 1, SMF__Q_PAD|SMF__Q_APOD,
                       &istart, &iend, status );

    /* Loop over bolometers */
    for( i=0; (*status==SAI__OK) && (i<nbolo); i++ ) {
      base = i*bstride;
      isbad = 0;

      /* Continue if bolo stream is not flagged bad */
      if( !(qua[base] & SMF__Q_BADB) && (*status == SAI__OK) ) {

        /* initial conditions for jump detection */
        smf_stats1D( dat+base+istart*tstride, tstride, dcbox,
                     qua+base+istart*tstride, SMF__Q_MOD, &mean1, NULL,
                     &nmean1, status);
        smf_stats1D( dat+base+(istart+dcbox)*tstride, tstride, dcbox,
                     qua+base+(istart+dcbox)*tstride, SMF__Q_MOD,
                     &mean2, NULL,&nmean2, status );

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

                if( dcflag== 2 ) {
                  double biggest = maxdiff;
                  size_t wherebiggest = maxind;

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
            smf_correct_steps_baseline( dat+i*bstride, qua+i*bstride,
                                        ntslice, tstride, alljump );
          }
        }
      }

      /* If we got a SMF__INSMP, flag entire bolometer as bad and annul */
      if( *status == SMF__INSMP ) {
        errAnnul( status );
        isbad = 1;
      }

      if( isbad && (dcflag==1) ) {
        msgOutiff( MSG__DEBUG, "", FUNC_NAME ": flagging bad bolo %li at %li",
                   status, i, wherebad );
        for(j=0; j<ntslice; j++) {
          qua[base+j*tstride] |= SMF__Q_BADB;
        }

        qua[base+wherebad*tstride] |= SMF__Q_JUMP;
      }
    }

    /* If dcflag==2, go back to all locations of steps in each bolometer
       and correct / flag (handle small DC steps correlated with big ones) */
    if( (dcflag == 2) && (*status==SAI__OK) ) {
      double *thisjump=NULL;
      thisjump = smf_malloc( ntslice, sizeof(*thisjump), 1, status );

      for( i=0; (*status==SAI__OK)&&(i<nbolo); i++ ) {
        if( !(qua[i*bstride] & SMF__Q_BADB) ) {
          /* Loop over time slices for this bolometer and calc thisjump */
          memset( thisjump, 0, ntslice*sizeof(*thisjump) );

          for( j=0; j<ntslice; j++ ) {
            /* Jump previously found. Measure mean before and after */
            if( alljump[j] ) {
              smf_stats1D( dat+i*bstride+(j-dcbox)*tstride, tstride, dcbox,
                           qua+i*bstride+(j-dcbox)*tstride, SMF__Q_MOD, &mean1,
                           NULL, &nmean1, status);

              smf_stats1D( dat+i*bstride+j*tstride, tstride, dcbox,
                           qua+i*bstride+j*tstride, SMF__Q_MOD, &mean2,
                           NULL, &nmean2, status);
              if( *status == SMF__INSMP ) {
                /* If insufficient samples just annul and continue */
                errAnnul( status );
              } else {
                thisjump[j] = mean2 - mean1;
              }

              /* Flag entire 2*DCBOX window */
              for( k=j-dcbox; k<j+dcbox; k++ ) {
                qua[i*bstride+k*tstride] |= SMF__Q_JUMP;
              }
            }
          }

          smf_correct_steps_baseline( dat+i*bstride, qua+i*bstride,
                                      ntslice, tstride, thisjump );
        }
      }

      thisjump = smf_free( thisjump, status );
    }

    /* Return nsteps if requested */
    if( nsteps ) {
      msgOutiff( MSG__DEBUG, "", FUNC_NAME ": %li bolos flagged", status,
                 ns );
      *nsteps = ns;
    }

    /* Clean up */
    alljump = smf_free( alljump, status );
  }

}
