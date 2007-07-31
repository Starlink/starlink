/*
*+
*  Name:
*     smf_close_smfGroup

*  Purpose:
*     Free resources associated with a smfGroup

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     SMURF subroutine

*  Invocation:
*     smf_close_smfGroup( smfGroup *group, int *status );

*  Arguments:
*     group = smfGroup * (Given)
*        Pointer to smfGroup to free
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine frees all of the resources associated with the
*     supplied smfGroup, setting it to a NULL pointer on exit.

*  Notes:

*  Authors:
*     Andy Gibb (UBC)
*     {enter_new_authors_here}

*  History:
*     2006-07-04 (AGG):
*        Initial version

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
#include "ast.h"
#include "star/ndg.h"
#include "star/grp.h"
#include "msg_par.h"

/* SMURF routines */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"

#define FUNC_NAME "smf_close_smfGroup"

void smf_close_smfGroup ( smfGroup **group, int *status ) {

  /* Local variables */
  int i;                      /* Loop counter */
  int **subgroups = NULL;     /* Array of pointers to arrays of Grp indices */
  int *indices = NULL;        /* Pointer to array of Grp indices */

  if ( *status != SAI__OK ) return;

  if ( *group == NULL ) {
    if ( *status == SAI__OK ) {
      /* Status is good so we have a problem */
      *status = SAI__ERROR;
      errRep( FUNC_NAME, "Attempt to release smfGroup when input pointer is NULL (possible programming error)",
              status );
    }
  } else {
    /* Free Grp */
    grpDelet( &((*group)->grp), status);

    /* Free subgroups */
    subgroups = (*group)->subgroups;
    for ( i=0; i<(*group)->ngroups; i++) {
      /* Retrieve pointer to array of indices */
      indices = subgroups[i];
      /* Free memory associated with current array of indices */
      smf_free( indices, status );
    }
    /* Free pointer to subgroups */
    smf_free( subgroups, status );

    /* Finally free the group */
    smf_free( *group, status);
    *group = NULL;
  }
}
