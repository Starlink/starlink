
/*
*+
*  Name:
*     smf_apodize

*  Purpose:
*     Apodize (smooth roll-off) time-series data 

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     SMURF subroutine

*  Invocation:
*     smf_apodize( smfData *data, size_t len, int *status );

*  Arguments:
*     data = smfData* (Given)
*        Pointer to smfData that will be modified.
*     len = size_t (Given)
*        Number of samples over which to apply apodization.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Before operations such as filtering it can be useful to apodize
*     time-series data to avoid ringing caused sharp edges. This
*     routine multiplies the start and end of each bolometer time
*     stream by a trig function that rolls-off to 0 in len samples. If
*     the data were previously padded (indicated by SMF__Q_PAD quality
*     bits) then the apodizations starts after the padding (or before
*     for the roll-off at the end). Apodized sections are quality
*     flagged SMF__Q_APOD.

*  Notes:

*  Authors:
*     Ed Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2008-12-01 (EC):
*        Initial version.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008 University of British Columbia. 
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

void smf_apodize( smfData *data, size_t len, int *status ) {

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
  size_t off;                   /* Index offset */
  unsigned char *qua=NULL;      /* Pointer to the QUALITY array */
  size_t tstride;               /* Time slice stride in data array */
  unsigned char mask;           /* Quality bit mask */

  if ( *status != SAI__OK ) return;

  /* Check input parameters */
  smf_dtype_check_fatal( data, NULL, SMF__DOUBLE, status );

  if( !data->pntr[0] ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": smfData does not contain a DATA component", 
            status );
    return;
  }

  /* Calculate data dimensions */
  smf_get_dims( data, &nbolo, &ntslice, &ndata, &bstride, &tstride, status );

  /* Obtain pointer to data and quality components */
  dat = data->pntr[0];
  qua = data->pntr[2];

  /* Set the quality bitmask to decide which samples to apodize */
  mask = ~(SMF__Q_JUMP | SMF__Q_SPIKE);

  /* Loop over bolometer start index */
  for( i=0; (*status==SAI__OK)&&(i<nbolo*bstride); i+=bstride ) {

    /* Determine the first and last samples to apodize (after padding) */
    first = i;
    last = i+(ntslice-1)*tstride;

    if( qua && !(qua[i]&SMF__Q_BADB)) {
      /* First sample */
      for( j=i; j<(i+ntslice*tstride); j+=tstride ) {
        if( !(qua[j]&SMF__Q_PAD) ) break;
      }

      if( j==(i+ntslice*tstride) ) {
        *status=SAI__ERROR;
        errRep( "", FUNC_NAME ": Can't apodize, entire array is padding.", 
                status );
      } else {
        first = j;
        /* Last sample if first was found */
        for( j=i+(ntslice-1)*tstride; j>0; j-=tstride ) {
          if( !(qua[j]&SMF__Q_PAD) ) break;
        }
        last = j;
      }
    } 

    /* Can we apodize? */
    if( (*status==SAI__OK) && (last-first+1 < 2*len) ) {
      *status = SAI__ERROR;
      errRep("", FUNC_NAME ": Can't apodize, not enough samples.", status );
    }

    /* Do the apodization */
    if( *status == SAI__OK ) {

      /* Quality checking version */
      if( qua && !(qua[i]&SMF__Q_BADB)) {
        off = 0; /* Time slice offset from first/last  */
        for( j=0; (*status==SAI__OK)&&(j<len); j++ ) {
          ap = sin( AST__DPI/2. * (double) j / len );

          if( !(qua[first+off]&mask) ) {
            dat[first+off]*=ap;
            qua[first+off]|=SMF__Q_APOD;
          }
          
          if( !(qua[last-off]&mask) ) {
            dat[last-off]*=ap;
            qua[last-off]|=SMF__Q_APOD;
          }

          /* Increment time slice offset */
          off+= tstride; 
        }

      } else if (!qua) {
        /* Non-Quality checking version */
        off = 0; /* Time slice offset from first/last  */
        for( j=0; (*status==SAI__OK)&&(j<len); j++ ) {
          ap = sin( AST__DPI/2. * (double) j / len );

          if( dat[first+off]!=VAL__BADD ) {
            dat[first+off]*=ap;
          }

          if( dat[last-off]!=VAL__BADD ) {
            dat[last-off]*=ap;
          }
          
          /* Increment time slice offset */
          off+= tstride; 
        }
      }
    } 
  }  
}
