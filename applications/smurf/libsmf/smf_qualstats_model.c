/*
*+
*  Name:
*     smf_qualstats_model

*  Purpose:
*     Calculate quality statistics for a QUA model

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_qualstats_model( const smfArray *qua, size_t qcount[8],
*                          size_t * ngoodbolo, size_t * nmap,
*                          size_t *nmax, dim_t *ntslice, size_t * ntgood,
*                          size_t * tbound, size_t * tpad, int * status );

*  Arguments:
*     qua = const smfArray *qua (Given)
*        Pointer to smfArray of smfData's containing quality
*     qcount = size_t[8] (Returned)
*        Pointer to array that will count number of occurences of each
*        quality bit in qual. Can be NULL.
*     ngoodbolo = size_t* (Returned)
*        If specified, return number of bolometers that are flagged as good.
*     nmap = size_t* (Returned)
*        If specified, return total number of samples that could be used
*        in the map (i.e., no quality bits in SMF__Q_GOOD set).
*     nmax = size_t* (Returned)
*        If specified, return the maximum theoretical number of samples that
*        could be used for a map -- excluding only SMF__Q_PAD|SMF__Q_APOD
*        (padding/apodization).
*     ntslice = size_t * (Returned)
*        Number of time slices in model. Can be NULL.
*     ntgood = size_t * (Returned)
*        Number of good time slices in model (not including padding). Can be NULL.
*     tbound = size_t * (Returned)
*        Number of time slice in boundary. Can be NULL.
*     tpad = size_t * (Returned)
*        Number of time slices in padding. Can be NULL.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Calculate statistics for a smfArray based on the quality information.
*     The quality flags will either be read from the primary data component
*     if the type is SMF__QUALTYPE or else will be read from the standard
*     quality location in each smfData. Count the total number of quality bits
*     and return them.

*  Notes:
*     The effective number of bolometers in the map can be derived
*     using
*          nbolo_eff = *nmap / *ntgood

*  Authors:
*     Edward Chapin (UBC)
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2010-05-27 (TIMJ):
*        Initial version inherited from smf_qualstats_report
*     2010-06-01 (TIMJ):
*        Use dim_t for ntslice in an attempt at consistency.

*  Copyright:
*     Copyright (C) 2010 University of British Columbia.
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

#define FUNC_NAME "smf_qualstats_report"

void
smf_qualstats_model( const smfArray *qua, size_t qcount[8], size_t * ngoodbolo,
                     size_t * nmap, size_t *nmax, dim_t * ntslice, size_t * ntgood,
                     size_t * tbound, size_t *tpad, int * status ) {

  /* Local Variables */
  size_t i;                     /* loop counter */
  dim_t idx;                    /* Subarray counter */
  size_t nbolo_tot;             /* total bolos in all subarrays */
  size_t nmap_tot;              /* number of good map samples */
  size_t nmax_tot;              /* theoretical maximum good map samples */
  dim_t ntslice_ref;            /* reference number of time slices */
  size_t tbound_local;          /* Local calculation of tbound */

  /* Main routine */
  if (*status != SAI__OK) return;

  if( !qua ) {
    *status = SAI__ERROR;
     errRep(" ", FUNC_NAME
            ": NULL qua pointer supplied.", status);
    return;
  }

  /* Initialize counts */
  if (qcount) memset( qcount, 0, SMF__NQBITS*sizeof(*qcount) );
  nmap_tot = 0;
  nmax_tot = 0;

  /* Loop over subarray */
  nbolo_tot = 0;
  ntslice_ref = 0;
  for( idx=0; (idx<qua->ndat)&&(*status==SAI__OK); idx++ ) {
    smf_qual_t *qual=NULL;     /* pointer to quality buffer */

    /* Get pointer to quality array and its dimensions */
    if( qua->sdata[idx]->dtype == SMF__QUALTYPE ) {
      qual = qua->sdata[idx]->pntr[0];
    } else {
      qual = qua->sdata[idx]->qual;
    }

    if( !qual ) {
      *status = SAI__ERROR;
      errRep(" ", FUNC_NAME
             ": NULL qual pointer encountered", status);
    } else {
      size_t bstride;               /* bolo stride */
      dim_t nbolo;                  /* number of bolos */
      dim_t nslices;                /* Number of time slices in this smfData */
      size_t subqcount[8];          /* subarray quality bit counter */
      size_t subnmap;               /* nmap for subarray */
      size_t subnmax;               /* nmax for subarray */
      size_t tstride;               /* time slice stride */

      smf_get_dims( qua->sdata[idx], NULL, NULL, &nbolo, &nslices,
                    NULL, &bstride, &tstride, status );


      /* get quality statistics for the current subarray */
      smf_qualstats( qual, nbolo, bstride, nslices, tstride, subqcount, NULL,
                     &subnmap, &subnmax, status );

      /* add to total number of bolometers and check for length consistency */
      nbolo_tot += nbolo;
      if( !ntslice_ref ) {
        ntslice_ref = nslices;
      } else if( nslices != ntslice_ref ) {
        *status = SAI__ERROR;
        errRep(" ", FUNC_NAME
               ": Different subarrays have mismatch in number of time slices.", status);
      }

      if( *status == SAI__OK ) {
        /* Add counts from this subarray to the total */
        if (qcount) {
          for( i=0; i<SMF__NQBITS; i++ ) {
            qcount[i] += subqcount[i];
          }
        }

        nmap_tot += subnmap;
        nmax_tot += subnmax;
      }

    }
  }

  /* Store results */
  if (nmap) *nmap = nmap_tot;
  if (nmax) *nmax = nmax_tot;
  if (ngoodbolo) *ngoodbolo = nbolo_tot;
  if (ntslice) *ntslice = ntslice_ref;

  /* Calculate time slices in boundary */
  tbound_local = ntslice_ref-nmax_tot/nbolo_tot;
  if (tbound) *tbound = tbound_local;

  /* Calculate time slices in padding */
  if (tpad) {
    size_t whichbit = 0;
    while( !((1<<whichbit) & SMF__Q_PAD) ) { /* which bit is SMF__Q_PAD? */
      whichbit++;
    }
    *tpad = qcount[whichbit] / nbolo_tot;
  }

  /* Calculate the number of time slices that can be used */
  if (ntgood) *ntgood = ntslice_ref - tbound_local;

}
