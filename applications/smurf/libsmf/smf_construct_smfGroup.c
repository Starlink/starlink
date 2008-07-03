/*
*+
*  Name:
*     smf_construct_smfGroup

*  Purpose:
*     Create a smfGroup

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     SMURF subroutine

*  Invocation:
*     smfGroup* = smf_construct_smfGroup( Grp *igrp, int **subgroups, 
*                                         size_t *chunk,
*				          const dim_t ngroups, 
*                                         const dim_t nrelated,
*                                         const int copy, 
*                                         int *status );

*  Arguments:
*     igrp = Grp* (Given)
*        Input Grp to be copied
*     subgroups = int** (Given)
*        Pointer to array of pointers to arrays of indices into Grp
*     chunk = size_t* (Given)
*        Array of length ngroups flagging which subgroups are continuous
*     ngroups = dim_t (Given)
*        Number of subgroups in the smfGroup
*     nrelated = dim_t (Given)
*        Maximum number of related files in the smfGroup
*     copy = int (Given)
*        If non-zero copy subgroups & chunk. Otherwise use in-place.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:

*     This routine constructs a smfGroup containing a copy of the
*     input Grp of files, a 2-D array of subgroups containing the
*     indices into the Grp of related files, the number of subgroups
*     and the maximum number of related files in any one subgroup. The
*     routine returns a pointer to a smfGroup, which is NULL on error.


*  Notes:
*     If making a new smfGroup using the the same grouping as another
*     smfGroup use the copysubgroups flag to avoid sharing the memory.

*  Authors:
*     Andy Gibb (UBC)
*     Ed Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2006-06-24 (AGG):
*        Initial version
*     2007-07-16 (EC):
*        Added copysubgroups
*     2007-12-18 (AGG):
*        Update to use new smf_free behaviour
*     2008-04-16 (EC):
*        -added chunk to smfGroup
*     2008-07-03 (EC):
*        Changed ngroups/nrelated to dim_t

*  Copyright:
*     Copyright (C) 2006 University of British Columbia.  All Rights
*     Reserved.

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

#if HAVE_CONFIG_H
#include <config.h>
#endif

/* System includes */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

/* Starlink includes */
#include "sae_par.h"
#include "mers.h"
#include "ndf.h"
#include "star/ndg.h"
#include "star/grp.h"
#include "msg_par.h"

/* SMURF routines */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"

#define FUNC_NAME "smf_construct_smfGroup"

smfGroup *smf_construct_smfGroup( Grp *igrp, int **subgroups, size_t *chunk,
				  const dim_t ngroups,  const dim_t nrelated, 
				  const int copy, int *status ) {

  /* Local variables */
  smfGroup *group = NULL;
  int isize;
  dim_t i;
  int **newsubgroups=NULL;
  
  if ( *status != SAI__OK ) return NULL;

  /* Allocate space for smfGroup */
  group = smf_malloc( 1, sizeof(smfGroup), 0, status);
  if ( *status != SAI__OK ) {
    errRep(FUNC_NAME, "Unable to allocate memory for smfGroup", status);
    goto CLEANUP;
  }

  /* Copy the Grp */
  grpGrpsz( igrp, &isize, status);
  group->grp = grpCopy( igrp, 1, isize, 0, status);
  if ( *status != SAI__OK ) {
    errRep(FUNC_NAME, "Error copying Grp for smfGroup", status);
    goto CLEANUP;
  }

  /* Copy subgroups and chunk if requested */
  if( copy ) {
    /* subgroups */
    newsubgroups = smf_malloc( ngroups, sizeof(*newsubgroups), 1, status );
    for( i=0; i<ngroups; i++ ) {
      newsubgroups[i] = smf_malloc( nrelated, sizeof(**newsubgroups), 0, 
				    status );
      if( *status == SAI__OK ) {
	memcpy( newsubgroups[i], subgroups[i], 
		sizeof(**newsubgroups)*nrelated );
      }
    }

    if( *status == SAI__OK ) {
      group->subgroups = newsubgroups;
    }

    /* chunk */
    group->chunk = smf_malloc( ngroups, sizeof(*chunk), 0, status );
    if( *status == SAI__OK ) {
      memcpy( group->chunk, chunk, sizeof(*chunk)*ngroups );
    }

  } else {
    /* Otherwise use the inputs in-place */
    group->subgroups = subgroups;
    group->chunk = chunk;
  }

  if( *status == SAI__OK ) {
    group->ngroups = ngroups;
    group->nrelated = nrelated;
  }

  return group;

 CLEANUP:
  if ( group )
    group = smf_free( group, status );

  return NULL;

}
