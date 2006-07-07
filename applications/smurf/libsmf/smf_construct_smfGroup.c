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
*				          const int ngroups, const int nrelated, 
*                                         int *status );

*  Arguments:
*     igrp = Grp* (Given)
*        Input Grp to be copied
*     subgroups = int** (Given)
*        Pointer to array of pointers to arrays of indices into Grp
*     ngroups = int (Given)
*        Number of subgroups in the smfGroup
*     nrelated = in (Given)
*        Maximum number of related files in the smfGroup
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:

*     This routine constructs a smfGroup containing a copy of the
*     input Grp of files, a 2-D array of subgroups containing the
*     indices into the Grp of related files, the number of subgroups
*     and the maximum number of related files in any one subgroup. The
*     routine returns a pointer to a smfGroup, which is NULL on error.


*  Notes:

*  Authors:
*     Andy Gibb (UBC)
*     {enter_new_authors_here}

*  History:
*     2006-06-24 (AGG):
*        Initial version

*  Copyright:
*     Copyright (C) 2006 University of British Columbia.  All Rights
*     Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
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

smfGroup *smf_construct_smfGroup ( Grp *igrp, int **subgroups, const int ngroups, 
				   const int nrelated, int *status ) {

  /* Local variables */
  smfGroup *group = NULL;
  int isize;

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

  group->subgroups = subgroups;
  group->ngroups = ngroups;
  group->nrelated = nrelated;

  return group;

 CLEANUP:
  smf_free( &group, status );

  return NULL;

}
