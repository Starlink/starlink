/*
*+
*  Name:
*     smf_checkmem_dimm

*  Purpose:
*     Verify that there is enough memory to run the dynamic iterative map-maker

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_checkmem_dimm( dim_t maxlen, inst_t instrument, int nrelated, 
*                        smf_modeltype *modeltyps, dim_t nmodels, 
*                        size_t available, size_t *necessary, int *status );

*  Arguments:
*     maxlen = dim_t (Given)
*        The longest chunk of data that will be handled by the DIMM (samples).
*     instrument = inst_t (Given)
*        For which instrument is the map-maker being run?
*     nrelated = int (Given)
*        How many subarrays are being processed simultaneously
*     modeltyps = smf_modeltype* (Given)
*        Array indicating which model components are being solved for
*     nmodels = dim_t (Given)
*        Number of elements in modeltyps
*     available = size_t (Given)
*        Maximum memory in bytes that the mapped arrays may occupy
*     necessary = size_t * (Returned)
*        If non-null return estimate of the actual amount of memory required
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This function checks the amount of memory required to run the
*     dynamic iterative map-maker (DIMM). The calculation obtains the
*     number of detectors at each time slice from the instrument
*     type. The estimate is based on all of the requested model
*     components, and the number of subarrays being processed
*     simultaneously.  If this amount of memory (necessary) exceeds
*     available, SMF__NOMEM status is set.

*  Authors:
*     Edward Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2008-04-28 (EC):
*        Initial version.
*     2008-04-30 (EC):
*        Added SMF__EXT
*     {enter_further_changes_here}

*  Notes:
*     This should match memory allocated in smf_grp_related.

*  Copyright:
*     Copyright (C) 2005-2007 Particle Physics and Astronomy Research Council.
*     Copyright (C) 2005-2008 University of British Columbia.
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

#include <stdio.h>

/* Starlink includes */
#include "ast.h"
#include "mers.h"
#include "sae_par.h"
#include "star/ndg.h"
#include "star/slalib.h"

/* SMURF includes */
#include "smurf_par.h"
#include "libsmf/smf.h"
#include "libsmf/smf_err.h"
#include "jcmt/state.h"


#define FUNC_NAME "smf_checkmem_dimm"

void smf_checkmem_dimm( dim_t maxlen, inst_t instrument, int nrelated, 
			smf_modeltype *modeltyps, dim_t nmodels, 
			size_t available, size_t *necessary, int *status ) {


 /* Local Variables */
  dim_t i;                     /* Loop counter */
  size_t ncol;                 /* Number of columns */
  size_t ndet;                 /* Number of detectors each time step */
  size_t nrow;                 /* Number of rows */
  size_t nsamp;                /* ndet*maxlen */
  size_t total;                /* Total bytes required */

  /* Main routine */
  if (*status != SAI__OK) return;

  /* Check inputs */  

  if( maxlen < 1 ) {
    *status = SAI__ERROR;
    errRep("", FUNC_NAME ": maxlen cannot be < 1", status);
  }

  if( (nrelated < 1) || (nrelated > SMF__MXSMF) ) {
    msgSeti("NREL",nrelated);
    msgSeti("MAXREL",SMF__MXSMF);
    *status = SAI__ERROR;
    errRep("", FUNC_NAME ": nrelated, ^NREL, must be in the range [1,^MAXREL]", 
	   status);    
  }

  if( modeltyps ) {
    if( nmodels < 1 ) {
    *status = SAI__ERROR;
    errRep("", FUNC_NAME ": modeltyps specified, mmodels cannot be < 1", 
           status);
    }
  }

  if( *status == SAI__OK ) {

    /* How many detectors at each time step? */
    switch( instrument ) {
    case INST__SCUBA2:
      /* Kludgey, but at least we check SMF__COL_INDEX so this will help
         us catch possible future problems if order is changed */
      if( SMF__COL_INDEX ) {
        ncol = 32;
        nrow = 40;
      } else {
        ncol = 40;
        nrow = 32;
      }
      ndet = ncol*nrow;
      break;
    default:
      *status = SAI__ERROR;
      errRep("", FUNC_NAME ": Invalid instrument given.", status);
    }
  }

  if( *status == SAI__OK ) {
    
    nsamp = ndet*maxlen;
    
    /* Calculate memory usage of static model components: */

    total = nsamp*smf_dtype_sz(SMF__DOUBLE,status);     /* RES */
    total += nsamp*smf_dtype_sz(SMF__DOUBLE,status);    /* AST */
    total += nsamp*smf_dtype_sz(SMF__INTEGER,status);   /* LUT */
    total += nsamp*smf_dtype_sz(SMF__UBYTE,status);     /* QUA */
  
    total *= nrelated;  /* All of these get multiplies by # subarrays */
  
  }

  if( *status == SAI__OK ) {
    /* Calculate memory usage of dynamic model components:  */

    if( modeltyps ) {
      for( i=0; i<nmodels; i++ ) {
	switch( modeltyps[i] ) {
	case SMF__NOI:
	  total += nsamp*smf_dtype_sz(SMF__DOUBLE,status)*nrelated;
	  break;
	case SMF__COM:
	  total += maxlen*smf_dtype_sz(SMF__DOUBLE,status);
	  break;
	case SMF__EXT:
	  total += nsamp*smf_dtype_sz(SMF__DOUBLE,status)*nrelated;
	  break;
        case SMF__DKS:
          total += (maxlen + nrow*3)*ncol*smf_dtype_sz(SMF__DOUBLE,status) *
            nrelated;
          break;
        case SMF__GAI:
          total += 3*nrow*ncol*smf_dtype_sz(SMF__DOUBLE,status)*nrelated;
          break;
	default:
	  *status = SAI__ERROR;
	  errRep("", FUNC_NAME ": Invalid smf_modeltype given.", status);
	}

	/* Exit on bad status */
	if( *status != SAI__OK ) {
	  i = nmodels;
	}
      }
    }

    /* Set bad status if too big */
    if( (total > available) && (*status == SAI__OK) ) {
      *status = SMF__NOMEM;
      msgSeti("REQ",total/SMF__MB);
      msgSeti("AVAIL",available/SMF__MB);
      errRep("", FUNC_NAME 
	     ": Requested memory ^REQ Mb for map exceeds available ^AVAIL Mb", 
	     status);
    }

    /* Return the required space */
    if( necessary ) {
      *necessary = total;
    }

  }
    
}
