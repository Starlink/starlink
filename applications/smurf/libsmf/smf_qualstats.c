/*
*+
*  Name:
*     smf_qualstats

*  Purpose:
*     Produce statistics on quality flagging in a single time series array.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_qualstats( smf_qfam_t qfamily, int nopad, const smf_qual_t *qual, dim_t nbolo,
*                    size_t bstride, size_t ntslice, size_t tstride,
*                    size_t qcount[SMF__NQBITS], size_t *ngoodbolo, size_t *nmap,
*                    size_t *nmax, size_t *tpad, int *status )

*  Arguments:
*     qfamily = smf_qfam_t (Given)
*        Quality family associated with this quality array.
*     nopad = int (Given)
*        If true the padded region will not be included in the quality count.
*     qual = const smf_qual_t * (Given)
*        Pointer to quality array
*     nbolo = dim_t (Given)
*        Number of bolometers
*     bstride = size_t (Given)
*        How many elements to skip to get to the next bolometer at a given
*        time slice.
*     ntslice = dim_t (Given)
*        Number of time slices
*     tstride = size_t (Given)
*        How many elements to skip to get to the next time slice for the
*        current bolometer.
*     qcount = size_t[SMF__NQBITS] (Returned)
*        Pointer to array that will count number of occurences of each
*        quality bit in qual. Will only use the number of elements determined
*        by the quality family.
*     ngoodbolo = size_t* (Returned)
*        If specified, return number of bolometers that are flagged as good.
*     nmap = size_t* (Returned)
*        If specified, return total number of samples that could be used
*        in the map (i.e., no quality bits in SMF__Q_GOOD set).
*     nmax = size_t* (Returned)
*        If specified, return the maximum theoretical number of samples that
*        could be used for a map -- excluding only SMF__Q_PAD|SMF__Q_APOD
*        (padding/apodization).
*     tpad = size_t * (Returned)
*        Number of slices of padding. Can be NULL.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Traverse the quality array of a time series and count the number
*     of occurrences of each quality bit.

*  Notes:

*  Authors:
*     Edward Chapin (UBC)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2010-03-16 (EC):
*        Initial Version
*     2010-03-19 (EC):
*        Track samples that could go into the map (nmap, nmax)
*     2010-06-23 (TIMJ):
*        Add quality family support.
*     2010-07-16 (TIMJ):
*        Add ability to not include padded data. Add tpad parameter.
*     2010-10-08 (TIMJ):
*        Handle common cases with special code.

*  Copyright:
*     Copyright (C) 2010 University of British Columbia.
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

#define FUNC_NAME "smf_qualstats"

void smf_qualstats( smf_qfam_t qfamily, int nopad, const smf_qual_t *qual, dim_t nbolo,
                    size_t bstride, size_t ntslice, size_t tstride,
                    size_t qcount[SMF__NQBITS], size_t *ngoodbolo,
                    size_t *nmap, size_t *nmax, size_t *tpad,
                    int *status ) {

  /* Local Variables */
  size_t i;                     /* Loop counter */
  size_t j;                     /* Loop counter */
  size_t k;                     /* Loop counter */
  size_t numgoodbolo=0;
  size_t nummap=0;
  size_t nummax=0;
  size_t nqbits = 0;            /* Number of quality bits in this family */
  size_t offset;
  size_t slice_start = 0;       /* First time slice to analyse */
  size_t slice_end = 0;         /* last time slice */

  /* init */
  if (tpad) *tpad = 0;
  if (nmax) *nmax = 0;
  if (nmap) *nmap = 0;
  if (ngoodbolo) *ngoodbolo = 0;

  /* Main routine */
  if (*status != SAI__OK) return;

  if( !qual ) {
    *status = SAI__ERROR;
     errRep(" ", FUNC_NAME
            ": NULL qual pointer supplied.", status);
    return;
  }

  /* Initialize the counters */
  nqbits = smf_qfamily_count( qfamily, status );
  memset( qcount, 0, nqbits*sizeof(*qcount) );

  /* Determine start and end time slices */
  if (nopad) {
    smf_get_goodrange( qual, ntslice, tstride, SMF__Q_PAD, &slice_start,
                       &slice_end, status );
  } else {
    slice_start = 0;
    slice_end = ntslice-1;
  }

  /* Loop over bolo and time slice, and count occurrences of quality bits */
  for( i=0; i<nbolo; i++ ) {

    /* Count good bolos */
    if( !(qual[i*bstride]&SMF__Q_BADB) ) {
      numgoodbolo++;
    }

    for( j=slice_start; j<=slice_end; j++ ) {
      offset = i*bstride+j*tstride;

      /* Count samples for nmap and nmax */
      if( !(qual[offset]&SMF__Q_GOOD) ) {
        nummap++;
      }

      if( !(qual[offset]&SMF__Q_BOUND) ) {
        nummax++;
      }

      /* if the quality is 0 then we already know the
         answer without looping over all the bits */
      if ( qual[offset] != 0) {
        /* Handle some of the simplest cases explicitly to prevent
           a loop over all bits. In cases where only a single bit
           is set this saves a lot of time. BADDA+BADBOL is also
           very common */
        switch( qual[offset] ) {
        case BIT_TO_VAL(0):
          qcount[0]++;
          break;
        case BIT_TO_VAL(1):
          /* we do not need to worry about exceeding qcount bounds
             since we will only be accessing it if the case statement
             is true */
          qcount[1]++;
          break;
        case (BIT_TO_VAL(0)|BIT_TO_VAL(1)):
          qcount[0]++;
          qcount[1]++;
          break;
        default:
          /* Loop over bits */
          for( k=0; k<nqbits; k++ ) {
            if( qual[offset] & BIT_TO_VAL(k) ) {
              qcount[k]++;
            }
          }
        }
      }
    }
  }

  /* Return extra requested values */
  if( ngoodbolo ) {
    *ngoodbolo = numgoodbolo;
  }

  if( nmap ) {
    *nmap = nummap;
  }

  if( nmax ) {
    *nmax = nummax;
  }

  if (tpad) {
    if (nopad) {
      /* we got this directly */
      *tpad = slice_start +  ( (ntslice -1) - slice_end );
    } else {
      /* which bit is SMF__Q_PAD? */
      size_t whichbit = smf_qual_to_bit( SMF__Q_PAD, status );
      *tpad = qcount[whichbit] / nbolo;
    }
  }

}
