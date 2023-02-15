/*
*+
*  Name:
*     smf_checkmem_map

*  Purpose:
*     Verify that there is enough memory to map components of output map

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_checkmem_map( const dim_t lbnd[2], const dim_t ubnd[2], int rebin,
*                       int nw, size_t available, const char *epsout,
*                       size_t *necessary, int *status );

*  Arguments:
*     lbnd = const dim_t[2] (Given)
*        2-element array indices for lower bounds of the output map
*     ubnd = const dim_t[2] (Given)
*        2-element array indices for upper bounds of the output map
*     rebin = int (Given)
*        If set calculate memory for method=rebin. Otherwise method=iterate.
*     nw = int (Given)
*        Number of worker threads, since iteratemap uses nw temp maps
*     available = size_t (Given)
*        Maximum memory in bytes that the mapped arrays may occupy
*     epsout = const char * (Given)
*        If non-NULL, then two buffers map-sized buffers are needed to
*        hold the difference maps needed by the EPSOUT config parameter.
*     necessary = size_t * (Returned)
*        If non-null return estimate of the actual amount of memory required
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This function estimates the amount of memory required for the ouput map
*     based on the DATA, VARIANCE, WEIGHTS and EXP_TIME arrays sizes given
*     their data-types and the lbnd/ubnd ranges. If this amount of memory
*     (necessary) exceeds available, SMF__NOMEM status is set.

*  Authors:
*     Edward Chapin (UBC)
*     Gaelen Marsden (AGM, UBC)
*     {enter_new_authors_here}

*  History:
*     2008-04-24 (EC):
*        Initial version.
*     2010-04-20 (EC):
*        Extra memory for quality array if iteratemap.
*     2010-05-31 (EC):
*        Extra space required for mapweight^2 array in iteratemap
*     2010-09-20 (TIMJ):
*        We are using MiB not Mb
*     2013-10-24 (AGM):
*        smf_iteratemap uses nw maps for temporary calculations
*     2014-05-27 (DSB):
*        Add epsout parameter.
*     {enter_further_changes_here}

*  Notes:

*  Copyright:
*     Copyright (C) 2008,2010 University of British Columbia.
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

#include <stdio.h>

/* Starlink includes */
#include "ast.h"
#include "mers.h"
#include "sae_par.h"
#include "star/ndg.h"

/* SMURF includes */
#include "smurf_par.h"
#include "libsmf/smf.h"
#include "libsmf/smf_err.h"


#define FUNC_NAME "smf_checkmem_map"

void smf_checkmem_map( const dim_t lbnd[], const dim_t ubnd[], int rebin, int nw,
		       size_t available, const char *epsout, size_t *necessary,
                       int *status ) {

 /* Local Variables */
  size_t mapsize;              /* Elements in output map */
  size_t total;                /* Total bytes required */

  /* Main routine */
  if (*status != SAI__OK) return;

  /* Check inputs */
  if( !lbnd ) {
    *status = SAI__ERROR;
    errRep("FUNC_NAME", "NULL lbnd supplied", status);
    return;
  }

  if( !ubnd ) {
    *status = SAI__ERROR;
    errRep("FUNC_NAME", "NULL ubnd supplied", status);
    return;
  }

  if( available < 1 ) {
    *status = SAI__ERROR;
    errRep("FUNC_NAME", "Invalid available memory: must be > 0", status);
    return;
  }

  if( (*status == SAI__OK) && ((lbnd[0] > ubnd[0]) || (lbnd[1] > ubnd[1])) ) {
    *status = SAI__ERROR;
    errRep("FUNC_NAME", "Elements of ubnd must be > lbnd", status);
    return;
  }

  if( *status == SAI__OK ) {
    /* Compute number of pixels in output map */
    mapsize = (ubnd[0] - lbnd[0] + 1) * (ubnd[1] - lbnd[1] + 1);

    /* Determine memory required by all arrays with mapsize
       elements. smurf_makemap always needs double precision arrays
       for MAP, VARIANCE, WEIGHTS, EXP_TIME. Also need space for lastmap
       and mapchange to test convergence. */

    total = 6*sizeof(double)*mapsize;

    if( rebin ) {
      /* For method=rebin we also need space for weights3d */
      total += 2*sizeof(double)*mapsize;
    } else {
      /* For method=iterate we need space for hitsmap */
      total += sizeof(unsigned int)*mapsize;

      /* space for quality map */
      total += sizeof(smf_qual_t)*mapsize;

      /* smf_iteratemap also uses a local buffer to accumulate weights^2 */
      total += sizeof(double)*mapsize;

      /* add (nw - 1) maps for threaded calculations
         uses 4 doubles (map, mapvar, weight, weightsq) and 1 int (hitsmap) */
      total += 4*(nw-1)*sizeof(double)*mapsize + (nw-1)*sizeof(int)*mapsize;

      /* add 3 data arrays (no variance or quality) if EPSOUT is specified. */
      if( epsout ) total += 3*sizeof(double)*mapsize;
    }

    /* Set bad status if too big */
    if( total > available ) {
      *status = SMF__NOMEM;
      msgSetk("REQ",total/SMF__MIB);
      msgSetk("AVAIL",available/SMF__MIB);
      errRep("FUNC_NAME",
	     "Requested memory ^REQ MiB for map exceeds available ^AVAIL MiB",
	     status);
    }

    /* Return the required space */
    if( necessary ) {
      *necessary = total;
    }

  }

}
