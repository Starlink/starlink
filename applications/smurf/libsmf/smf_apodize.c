
/*
*+
*  Name:
*     smf_apodize

*  Purpose:
*     Apodize (smooth roll-off) time-series data using Hanning window

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     SMURF subroutine

*  Invocation:
*     smf_apodize( smfData *data, dim_t len, int forward, int * status );

*  Arguments:
*     data = smfData* (Given)
*        Pointer to smfData that will be modified.
*     len = dim_t (Given)
*        Number of samples over which to apply apodization. Can be set to
*        SMF__MAXAPLEN in which case the routine will automatically apodize
*        the entire data stream (maximum valid value of len)
*     forward = int (Given)
*        If non-zero, the supplied data is apodised using the Hanning
*        window. If zero, the effects of previous apodisation is removed
*        from the supplied data using the inverse of the Hanning window.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Before operations such as filtering it can be useful to apodize
*     time-series data to avoid ringing caused by sharp edges. This
*     routine multiplies the start and end of each bolometer time
*     stream by a trig function that rolls-off to 0 in len samples (a
*     Hanning window):
*
*     window(i) = 0.5 - (1.0 - 0.5)*cos( i*pi / len )
*
*     where i runs from 0 to (len-1) at the beginning of the time
*     stream, and then the same window (flipped in time) is applied at
*     the end of the time stream.
*
*     If the data were previously padded (indicated by SMF__Q_PAD
*     quality bits) then the apodizations starts after the padding (or
*     before for the roll-off at the end). Apodized sections are
*     quality flagged SMF__Q_APOD. In addition to the LEN samples at
*     the start and end that are apodized, an additional section of
*     data LEN samples long beyond that are also quality flagged
*     SMF__Q_APOD to ensure that ringing caused by filtering does not
*     get used in the final map.

*  Notes:

*  Authors:
*     Ed Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2008-12-01 (EC):
*       Initial version.
*     2009-10-02 (EC):
*       Switch to using a softer Hanning window
*     2009-10-21 (EC):
*       Enable SMF__MAXAPLEN for len
*     2009-11-17 (EC):
*       Move range checking into smf_get_goodrange
*     2009-12-01 (EC):
*       Apodize and mask all detectors to ensure that smf_get_goodrange works!
*     2010-03-31 (EC):
*       Only apodize working detectors, even though all detectors are flagged.
*     2011-04-26 (DSB):
*       Added "forward" argument.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008-2010 University of British Columbia.
*     Copyright (C) 2011 Science & Technology Facilities Council.
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
#include "sae_par.h"
#include "star/ndg.h"
#include "prm_par.h"
#include "par_par.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "libsmf/smf_err.h"

/* Other includes */
#include <math.h>

#define LOWAP 1.0E-6
#define FUNC_NAME "smf_apodize"

void smf_apodize( smfData *data, dim_t len, int forward, int *status ) {

  double ap;                    /* Apodization factor at this step */
  dim_t bstride;               /* Bolometer stride in data array */
  double *dat=NULL;             /* Pointer to the DATA component */
  dim_t first;                 /* First sample apodization at start */
  dim_t i;                     /* index to bolometer start */
  dim_t j;                     /* index to sample */
  dim_t last;                  /* Last sample apodization at end */
  dim_t nbolo;                  /* Number of bolometers */
  dim_t ndata;                  /* Number of data points */
  dim_t ntslice;                /* Number of time slices */
  smf_qual_t *qua=NULL;      /* Pointer to the QUALITY array */
  dim_t thelen=0;              /* apodization length */
  dim_t tstride;               /* Time slice stride in data array */

  if ( *status != SAI__OK ) return;

  /* Check input parameters */
  if ( !smf_dtype_check_fatal( data, NULL, SMF__DOUBLE, status )) return;

  if( !data->pntr[0] ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": smfData does not contain a DATA component",
            status );
    return;
  }

  /* Calculate data dimensions */
  smf_get_dims( data,  NULL, NULL, &nbolo, &ntslice, &ndata, &bstride,
                &tstride, status );

  /* Obtain pointer to data and quality components */
  dat = data->pntr[0];
  qua = smf_select_qualpntr( data, NULL, status );

  /* Determine the first and last samples to apodize (after padding) */
  if (qua) {
    smf_get_goodrange( qua, ntslice, tstride, SMF__Q_PAD, &first, &last,
                       status );
  } else {
    first = 0;
    last = ntslice -1 ;
  }

  /* Can we apodize? */
  if( *status == SAI__OK ) {
    if( len == SMF__MAXAPLEN ) {
      thelen = (last-first+1)/2;
      msgOutiff( MSG__DEBUG, "", FUNC_NAME
                 ": Using maximum apodization length, %zu samples.",
                 status, thelen );
    } else if( (last-first+1) < (2*len) ) {
      *status = SAI__ERROR;
      errRepf("", FUNC_NAME
              ": Can't apodize, not enough samples (%zu < %zu).", status,
              last-first+1, 2*len);
    } else {
      thelen = len;
    }
  }

  /* Loop over bolometer */
  for( i=0; (*status==SAI__OK)&&(i<nbolo); i++ ) {

    /* Do the apodization */
    if( *status == SAI__OK ) {

      /* Quality recording version */
      if( qua ) {

        /* First roll-off the signal, working detectors only */
        if( !(qua[i*bstride] & SMF__Q_BADB) ) {

          /* Apply apodisation. */
          if( forward ) {
            for( j=0; j<thelen; j++ ) {
              ap = 0.5 - 0.5*cos( AST__DPI * (double) j / thelen );

              /* scale the signal */
              if( !(qua[i*bstride+(first+j)*tstride]&SMF__Q_MOD) ) {
                dat[i*bstride+(first+j)*tstride]*=ap;
              }

              if( !(qua[i*bstride+(last-j)*tstride]&SMF__Q_MOD) ) {
                dat[i*bstride+(last-j)*tstride]*=ap;
              }
            }

          /* Remove apodisation. In order to avoid the signal blowing up
             at the ends, very low apodisation factors are skipped, and the
             corresponding samples are flagged with SMF__Q_LOWAP (causing
             them to be gap-filled in future).*/
          } else {
            for( j=0; j<thelen; j++ ) {
              ap = 0.5 - 0.5*cos( AST__DPI * (double) j / thelen );
              if( fabs( ap ) > LOWAP ) {

                if( !(qua[i*bstride+(first+j)*tstride]&SMF__Q_MOD) ) {
                  dat[i*bstride+(first+j)*tstride] /= ap;
                }

                if( !(qua[i*bstride+(last-j)*tstride]&SMF__Q_MOD) ) {
                  dat[i*bstride+(last-j)*tstride] /= ap;
                }

              } else {
                qua[i*bstride+(first+j)*tstride] |= SMF__Q_LOWAP;
                qua[i*bstride+(last-j)*tstride] |= SMF__Q_LOWAP;
              }
            }
          }
        }

        /* Set quality for all detectors regardless of whether they are
           bad or not -- across 2*len samples at both start and finish. */

        for( j=0; j<2*thelen; j++ ) {
          qua[i*bstride+(first+j)*tstride]|=SMF__Q_APOD;
          qua[i*bstride+(last-j)*tstride]|=SMF__Q_APOD;
        }

      } else if (!qua) {
        /* Non-Quality checking version */

        if( forward ) {
          for( j=0; j<thelen; j++ ) {
            ap = 0.5 - 0.5*cos( AST__DPI * (double) j / thelen );

            if( dat[i*bstride+(first+j)*tstride]!=VAL__BADD ) {
              dat[i*bstride+(first+j)*tstride]*=ap;
            }

            if( dat[i*bstride+(last-j)*tstride]!=VAL__BADD ) {
              dat[i*bstride+(last-j)*tstride]*=ap;
            }
          }
        } else {
          for( j=0; j<thelen; j++ ) {
            ap = 0.5 - 0.5*cos( AST__DPI * (double) j / thelen );
            if( fabs( ap ) > LOWAP ) {

              if( dat[i*bstride+(first+j)*tstride]!=VAL__BADD ) {
                dat[i*bstride+(first+j)*tstride] /= ap;
              }

              if( dat[i*bstride+(last-j)*tstride]!=VAL__BADD ) {
                dat[i*bstride+(last-j)*tstride] /= ap;
              }

            } else {
              dat[i*bstride+(first+j)*tstride] = VAL__BADD;
              dat[i*bstride+(last-j)*tstride] = VAL__BADD;
            }
          }
        }
      }
    }
  }
}
