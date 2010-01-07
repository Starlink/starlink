/*
*+
*  Name:
*     smf_fillgaps

*  Purpose:
*     Fill flagged regions of data with constrained realization of noise

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:

*     smf_fillgaps( smfData *data, unsigned char *quality,
*                   unsigned char mask, int *status )

*  Arguments:
*     data = smfData * (Given and Returned)
*        The data that will be flagged
*     quality = unsigned char * (Given and Returned)
*        If set, use this buffer instead of QUALITY associated with data.
*        If NULL, use the QUALITY associated with data (however bad status
*        will be set if internal QUALITY is also NULL).
*     mask = unsigned char (Given)
*        Define which bits in quality indicate locations of gaps to be filled.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Identify continuous regions of each detector time series that match
*     the given quality mask (e.g. spikes). Replace the flagged region
*     of data with a constrained realization of noise: smoothly connect the
*     before/after boundaries of the gap to avoid ringing when filters are
*     applied.

*  Notes:

*  Authors:
*     Edward Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2010-01-06 (EC):
*        Initial code stub

*  Copyright:
*     Copyright (C) 2010 Univeristy of British Columbia.
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

#define FUNC_NAME "smf_fillgaps"

void  smf_fillgaps( smfData *data, unsigned char *quality, unsigned char mask, 
                    int *status ) {

  /* Local Variables */
  size_t bstride;               /* bolo stride */
  size_t i;                     /* Loop Counter */
  double *dat=NULL;             /* Pointer to bolo data */
  dim_t nbolo;                  /* Number of bolos */
  dim_t ntslice;                /* Number of time slices */
  unsigned char *qua=NULL;      /* Pointer to quality array */
  size_t tstride;               /* time slice stride */

  /* Main routine */
  if (*status != SAI__OK) return;

  if (!smf_dtype_check_fatal( data, NULL, SMF__DOUBLE, status )) return;

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
    errRep( "", FUNC_NAME ": smfData does not contain a DATA component",status);
    return;
  }

  if( *status == SAI__OK ) {
    /* obtain data dimensions */
    smf_get_dims( data,  NULL, NULL, &nbolo, &ntslice, NULL, &bstride, &tstride,
                  status );
  }

  /* Loop over bolometer */
  for( i=0; i<nbolo; i++ ) if( !(qua[i*bstride] & SMF__Q_BADB) ) {

    /* Loop over time series */
    for( j=0; j<ntslice; j++ ) {
      /* Is this sample flagged? */
      if( qua[i*bstride + j*tstride] & mask ) {

        /* Need to decide if this is the start or midpoint of an existing
           flagged region */

      } else {
        /* If the previous sample was flagged and we get here, this is
           the end of a flagged region. Time to start gap filling... */

        /* Measure mean level and noise in patch before and after the gap --
           see smf_stats1 to get mean and sigma. Perhaps we need to
           add a parmeter "blen" for the length of the boundary sections
           in samples over which we measure means/standard deviations */

        /* Need to be clever for endpoints of the data stream */

        /* Fill the gap with a straight line, + random gaussian noise
           with a standard deviation that is just the average of sigma before
           and sigma after */
      }
    }
  }
}
