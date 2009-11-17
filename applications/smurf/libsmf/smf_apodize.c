
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
*     smf_apodize( smfData *data, unsigned char *quality, size_t len,
*                  int *status );

*  Arguments:
*     data = smfData* (Given)
*        Pointer to smfData that will be modified.
*     quality = unsigned char * (Given and Returned)
*        If set, use this buffer instead of QUALITY associated with data.
*        If NULL, use the QUALITY associated with data. Locations of spikes
*        will have bit SMF__Q_SPIKE set.
*     len = size_t (Given)
*        Number of samples over which to apply apodization. Can be set to
*        SMF__MAXAPLEN in which case the routine will automatically apodize
*        the entire data stream (maximum valid value of len)
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Before operations such as filtering it can be useful to apodize
*     time-series data to avoid ringing caused sharp edges. This
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
*     data beyond that are also quality flagged SMF__Q_APOD to ensure
*     that ringing caused by filtering does not get used in the final
*     map.

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
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008-2009 University of British Columbia.
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
#include "sae_par.h"
#include "star/ndg.h"
#include "prm_par.h"
#include "par_par.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "libsmf/smf_err.h"

/* Other includes */
#include <math.h>

#define FUNC_NAME "smf_apodize"

void smf_apodize( smfData *data, unsigned char *quality, size_t len,
                  int *status ) {

  double ap;                    /* Apodization factor at this step */
  size_t bstride;               /* Bolometer stride in data array */
  double *dat=NULL;             /* Pointer to the DATA component */
  size_t first;                 /* First sample apodization at start */
  size_t i;                     /* index to bolometer start */
  size_t j;                     /* index to sample */
  size_t last;                  /* Last sample apodization at end */
  dim_t nbolo;                  /* Number of bolometers */
  dim_t ndata;                  /* Number of data points */
  dim_t ntslice;                /* Number of time slices */
  unsigned char *qua=NULL;      /* Pointer to the QUALITY array */
  size_t thelen=0;              /* apodization length */
  size_t tstride;               /* Time slice stride in data array */

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

  if( quality ) {
    qua = quality;
  } else {
    qua = data->pntr[2];
  }

  /* Loop over bolometer */
  for( i=0; (*status==SAI__OK)&&(i<nbolo); i++ ) {

    /* Determine the first and last samples to apodize (after padding) */
    smf_get_goodrange( qua+i*bstride, ntslice, tstride, SMF__Q_PAD,
                       &first, &last, status );

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

    /* Do the apodization */
    if( *status == SAI__OK ) {

      /* Quality checking version */
      if( qua && !(qua[i*bstride]&SMF__Q_BADB)) {

        /* First roll-off the signal */
        for( j=0; j<thelen; j++ ) {
          ap = 0.5 - 0.5*cos( AST__DPI * (double) j / thelen );

          if( !(qua[i*bstride+(first+j)*tstride]&SMF__Q_MOD) ) {
            dat[i*bstride+(first+j)*tstride]*=ap;
            qua[i*bstride+(first+j)*tstride]|=SMF__Q_APOD;
          }

          if( !(qua[i*bstride+(last-j)*tstride]&SMF__Q_MOD) ) {
            dat[i*bstride+(last-j)*tstride]*=ap;
            qua[i*bstride+(last-j)*tstride]|=SMF__Q_APOD;
          }
        }
        /* then put in some extra flags, thelen samples again*/
        for( j=thelen; j<2*thelen; j++ ) {
          qua[i*bstride+(first+j)*tstride]|=SMF__Q_APOD;
          qua[i*bstride+(last-j)*tstride]|=SMF__Q_APOD;
        }

      } else if (!qua) {
        /* Non-Quality checking version */
        for( j=0; j<thelen; j++ ) {
          ap = 0.5 - 0.5*cos( AST__DPI * (double) j / thelen );

          if( dat[i*bstride+(first+j)*tstride]!=VAL__BADD ) {
            dat[i*bstride+(first+j)*tstride]*=ap;
          }

          if( dat[i*bstride+(last-j)*tstride]!=VAL__BADD ) {
            dat[i*bstride+(last-j)*tstride]*=ap;
          }
        }
      }
    }
  }
}
