/*
*+
*  Name:
*     smf_collapse_quality

*  Purpose:
*     Collapse a quality array along either the spatial or time axes

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     SMURF subroutine

*  Invocation:
*     smf_collapse_quality( const smf_qual_t *inqual, smf_qfam_t qfamily,
*                           dim_t nbolo, dim_t ntslice,
*                           dim_t bstride, dim_t tstride,
*                           int collapse_time, smf_qual_t **outqual,
*                           int *status )

*  Arguments:
*     inqual = const smf_qual_t * (Given)
*        3D quality array to be collapsed
*     qfamily = smf_qfam_t (Given)
*        Family associated with quality.
*     nbolo = dim_t (Given)
*        Number of bolometers
*     ntslice = dim_t (Given)
*        Number of time slices
*     bstride = dim_t (Given)
*        How many elements to skip to get to the next bolometer at a given
*        time slice.
*     tstride = dim_t (Given)
*        How many elements to skip to get to the next time slice for the
*        current bolometer.
*     collapse_time = int (Given)
*        If set collapse along the time dimension. Otherwise collapse spatially.
*     outqual = smf_qual_t** (Given)
*        Address of pointer to newly created collapsed quality. If collapse_time
*        set, the array will be of size nbolo. Otherwise it will be ntslice.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine collapses a quality array along either the spatial (bolo) or
*     time dimensions. The method used is to perform a logical AND of all
*     elements contributing to a collapsed value.

*  Notes:

*  Authors:
*     Ed Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2010-05-12 (EC):
*        Initial version
*     2010-07-09 (TIMJ):
*        Set default mask based on quality family.

*  Copyright:
*     Copyright (C) 2010 Science & Technology Facilities Council.
*     Copyright (C) 2010 Universty of British Columbia.
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

#if HAVE_CONFIG_H
#include <config.h>
#endif

/* System includes */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

/* Starlink includes */
#include "sae_par.h"
#include "mers.h"
#include "ndf.h"
#include "star/ndg.h"
#include "star/grp.h"
#include "msg_par.h"
#include "star/one.h"
#include "prm_par.h"

/* SMURF routines */
#include "smf.h"
#include "smf_typ.h"

#define FUNC_NAME "smf_collapse_quality"

void smf_collapse_quality( const smf_qual_t *inqual, smf_qfam_t qfamily,
                           dim_t nbolo, dim_t ntslice,
                           dim_t bstride, dim_t tstride,
                           int collapse_time, smf_qual_t **outqual,
                           int *status ) {

  dim_t clen;                  /* length collapsed axis */
  dim_t i;                     /* loop counter */
  dim_t j;                     /* loop counter */
  dim_t len;                   /* length non-collapsed axis */
  smf_qual_t mask = 0;          /* All bits set */
  dim_t nqbits = 0;            /* Number of quality bits in family */
  smf_qual_t *qual=NULL;     /* collapsed quality */
  dim_t stride;                /* stride non-collapsed axis */

  /* Check for valid input pointers */
  if( !inqual || !outqual ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": invalid quality pointer supplied.", status );
    return;
  }

  if( collapse_time ) {
    clen = ntslice;
    len = nbolo;
    stride = bstride;
  } else {
    clen = nbolo;
    len = ntslice;
    stride = tstride;
  }

  qual = astMalloc( len*sizeof(*qual) );

  /* Need to work out the initial bad quality for this family.
     When we collapse we retain a bit in the output if it
     is set in all collapsed data. We have an intial guess
     of all bits set for the family */
  nqbits = smf_qfamily_count( qfamily, status );
  mask = 0;
  for (i=0; i<nqbits; i++) {
    mask |= BIT_TO_VAL(i);
  }

  /* Loop over elements in collapsed array */
  for( i=0; i<len; i++ ) {
    qual[i] = mask;
    /* Loop over dimension to be collapsed */
    for( j=0; j<clen; j++ ) {
      qual[i] &= inqual[i*stride + j*bstride];
    }
  }

  *outqual = qual;

  return;
}
