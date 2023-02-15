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
*     smf_qualstats_model( ThrWorkForce *wf, smf_qfam_t qfamily, int nopad,
*                          const smfArray *qua, dim_t qcount[SMF__NQBITS],
*                          dim_t * ngoodbolo, dim_t * nmap, dim_t *nmax,
*                          dim_t *ntslice, dim_t * ntgood, dim_t * tbound,
*                          dim_t * tpad, int * status );

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads
*     qfamily = smf_qfam_t (Given)
*        Quality family associated with this quality array.
*     nopad = int (Given)
*        If true the padded region will not be included in the calculation.
*     qua = const smfArray *qua (Given)
*        Pointer to smfArray of smfData's containing quality. Assumes that the
*        dimensionality of each smfData is the same and that the padding
*        is the same.
*     qcount = dim_t[SMF__NQBITS] (Returned)
*        Pointer to array that will count number of occurences of each
*        quality bit in qual. Will only use the number of elements determined
*        by the quality family. Can be NULL.
*     ngoodbolo = dim_t* (Returned)
*        If specified, return number of bolometers that are flagged as good.
*     nmap = dim_t* (Returned)
*        If specified, return total number of samples that could be used
*        in the map (i.e., no quality bits in SMF__Q_GOOD set).
*     nmax = dim_t* (Returned)
*        If specified, return the maximum theoretical number of samples that
*        could be used for a map -- excluding only SMF__Q_PAD|SMF__Q_APOD
*        (padding/apodization).
*     ntslice = dim_t * (Returned)
*        Number of time slices in model. Can be NULL. Will not include padding if "nopad"
*        is true.
*     ntgood = dim_t * (Returned)
*        Number of good time slices in model (not including padding or apodization). Can be NULL.
*     tbound = dim_t * (Returned)
*        Number of time slice in boundary. Can be NULL. Will not include padding if "nopad"
*        is true.
*     tpad = dim_t * (Returned)
*        Number of time slices in padding. Can be NULL. Will be zero if nopad is false.
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
*     2010-06-23 (TIMJ):
*        Add quality family support.
*     2010-07-16 (TIMJ):
*        Support "nopad" parameter so that we can calculate quality
*        stats ignoring padding.
*     2010-08-05 (TIMJ):
*        Fix nopad logic when more than one subarray is being used.

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
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

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

#define FUNC_NAME "smf_qualstats_model"

void
smf_qualstats_model( ThrWorkForce *wf, smf_qfam_t qfamily, int nopad,
                     const smfArray *qua, dim_t qcount[SMF__NQBITS],
                     dim_t * ngoodbolo, dim_t * nmap, dim_t *nmax, dim_t * ntslice,
                     dim_t * ntgood, dim_t * tbound, dim_t *tpad, int * status ) {

  /* Local Variables */
  dim_t i;                     /* loop counter */
  dim_t idx;                    /* Subarray counter */
  dim_t nbolo_tot;             /* total bolos in all subarrays */
  dim_t nmap_tot;              /* number of good map samples */
  dim_t nmax_tot;              /* theoretical maximum good map samples */
  dim_t nqbits = 0;            /* Number of quality bits in this family */
  dim_t ntslice_ref;            /* reference number of time slices */
  dim_t tbound_local;          /* Local calculation of tbound */
  dim_t tpad_ref;              /* Reference padding slices */

  /* Main routine */
  if (*status != SAI__OK) return;

  if( !qua ) {
    *status = SAI__ERROR;
     errRep(" ", FUNC_NAME
            ": NULL qua pointer supplied.", status);
    return;
  }

  /* Initialize counts */
  nqbits = smf_qfamily_count( qfamily, status );
  if (qcount) memset( qcount, 0, nqbits*sizeof(*qcount) );
  nmap_tot = 0;
  nmax_tot = 0;

  /* Loop over subarray */
  nbolo_tot = 0;
  ntslice_ref = 0;
  tpad_ref = 0;
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
      dim_t bstride;               /* bolo stride */
      dim_t nbolo;                  /* number of bolos */
      dim_t nslices;                /* Number of time slices in this smfData */
      dim_t subqcount[SMF__NQBITS];/* subarray quality bit counter */
      dim_t subnmap;               /* nmap for subarray */
      dim_t subnmax;               /* nmax for subarray */
      dim_t tstride;               /* time slice stride */
      dim_t tpadslices;            /* number of time slices in padding */

      smf_get_dims( qua->sdata[idx], NULL, NULL, &nbolo, &nslices,
                    NULL, &bstride, &tstride, status );


      /* get quality statistics for the current subarray */
      smf_qualstats( wf, qfamily, nopad, qual, nbolo, bstride, nslices, tstride, subqcount,
                     NULL, &subnmap, &subnmax, &tpadslices, status );

      /* Remove padding from this number if nopad is true */
      if (nopad) {
        nslices -= tpadslices;
      }

      /* add to total number of bolometers and check for length consistency */
      nbolo_tot += nbolo;
      if( !ntslice_ref ) {
        ntslice_ref = nslices;
      } else if( nslices != ntslice_ref ) {
        *status = SAI__ERROR;
        errRepf(" ", FUNC_NAME
               ": Different subarrays have mismatch in number of time slices (%"
               DIM_T_FMT " (current) vs %" DIM_T_FMT " (previous))", status, nslices, ntslice_ref);
      }

      if (!tpad_ref) {
        tpad_ref = tpadslices;
      } else if ( tpad_ref != tpadslices ) {
        *status = SAI__ERROR;
        errRep( "", FUNC_NAME
                ": Different subarrays have mismatch in padding", status );
      }

      if( *status == SAI__OK ) {
        /* Add counts from this subarray to the total */
        if (qcount) {
          for( i=0; i<nqbits; i++ ) {
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

  /* Calculate time slices in padding if we got padding statistics. The alternative
     is to calculate this using smf_get_goodrange for each smfData. */
  if (tpad) {
    if (!nopad) {
      *tpad = tpad_ref;
    } else {
      *tpad = 0;
    }
  }

  /* Calculate the number of time slices that can be used */
  if (ntgood) *ntgood = ntslice_ref - tbound_local;

}
