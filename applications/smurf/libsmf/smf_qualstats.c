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
*     smf_qualstats( const smf_qual_t *qual, dim_t nbolo, size_t bstride,
*                    size_t ntslice, size_t tstride,  size_t qcount[8],
*                    size_t *ngoodbolo, size_t *nmap, size_t *nmax,
*                    int *status )

*  Arguments:
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
*     qcount = size_t[8] (Returned)
*        Pointer to array that will count number of occurences of each
*        quality bit in qual.
*     ngoodbolo = size_t* (Returned)
*        If specified, return number of bolometers that are flagged as good.
*     nmap = size_t* (Returned)
*        If specified, return total number of samples that could be used
*        in the map (i.e., no quality bits in SMF__Q_GOOD set).
*     nmax = size_t* (Returned)
*        If specified, return the maximum theoretical number of samples that
*        could be used for a map -- excluding only SMF__Q_PAD|SMF__Q_APOD
*        (padding/apodization).
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Traverse the quality array of a time series and count the number
*     of occurrences of each quality bit.

*  Notes:

*  Authors:
*     Edward Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2010-03-16 (EC):
*        Initial Version
*     2010-03-19 (EC):
*        Track samples that could go into the map (nmap, nmax)

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

void smf_qualstats( const smf_qual_t *qual, dim_t nbolo, size_t bstride,
                    size_t ntslice, size_t tstride,
                    size_t qcount[SMF__NQBITS], size_t *ngoodbolo,
                    size_t *nmap, size_t *nmax,
                    int *status ) {

  /* Local Variables */
  size_t i;                     /* Loop counter */
  size_t j;                     /* Loop counter */
  size_t k;                     /* Loop counter */
  size_t numgoodbolo=0;
  size_t nummap=0;
  size_t nummax=0;
  size_t offset;

  /* Main routine */
  if (*status != SAI__OK) return;

  if( !qual ) {
    *status = SAI__ERROR;
     errRep(" ", FUNC_NAME
            ": NULL qual pointer supplied.", status);
    return;
  }

  /* Initialize the counters */
  memset( qcount, 0, SMF__NQBITS_TSERIES*sizeof(*qcount) );

  /* Loop over bolo and time slice, and count occurrences of quality bits */
  for( i=0; i<nbolo; i++ ) {

    /* Count good bolos */
    if( !(qual[i*bstride]&SMF__Q_BADB) ) {
      numgoodbolo++;
    }

    for( j=0; j<ntslice; j++ ) {
      offset = i*bstride+j*tstride;

      /* Count samples for nmap and nmax */
      if( !(qual[offset]&SMF__Q_GOOD) ) {
        nummap++;
      }

      if( !(qual[offset]&SMF__Q_BOUND) ) {
        nummax++;
      }

      /* Loop over bits */
      for( k=0; k<SMF__NQBITS_TSERIES; k++ ) {
        if( qual[offset] & BIT_TO_VAL(k) ) {
          qcount[k]++;
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


}
