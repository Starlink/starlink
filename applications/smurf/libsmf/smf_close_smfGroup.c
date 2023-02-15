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
 *     Coskun Oba (UoL)
 *     {enter_new_authors_here}

 *  History:
 *     2006-07-04 (AGG):
 *        Initial version
 *     2007-12-18 (AGG):
 *        Update to use new smf_free behaviour
 *     2008-04-03 (AGG):
 *        Free resources even if status is bad
 *     2008-04-16 (EC):
 *        Free chunk
 *     2010-10-25 (EC):
 *        Add tlen to smfGroup
 *     2011-02-10 (COBA):
 *        Changed data types from dim_t to dim_t

 *  Copyright:
 *     Copyright (C) 2006-2008,2010 University of British Columbia.
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
  dim_t i;                      /* Loop counter */
  int **subgroups = NULL;       /* Array of pointers to arrays of Grp indices */
  int *indices = NULL;          /* Pointer to array of Grp indices */

  /* We need to be able to clean up even if input status is bad - this
     requires some defensive programming. */

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

    /* Free chunk */
    if( (*group)->chunk ) {
      (*group)->chunk = astFree( (*group)->chunk );
    } else {
      *status = SAI__ERROR;
      errRep( FUNC_NAME,
              "Input smfGroup has no chunk (possible programming error)",
              status );
    }

    /* Free tlen */
    if( (*group)->tlen ) {
      (*group)->tlen = astFree( (*group)->tlen );
    } else {
      *status = SAI__ERROR;
      errRep( FUNC_NAME,
              "Input smfGroup has no tlen (possible programming error)",
              status );
    }

    /* Free subgroups */
    subgroups = (*group)->subgroups;
    if ( subgroups == NULL ) {
      if ( *status == SAI__OK ) {
        *status = SAI__ERROR;
        errRep( FUNC_NAME, "Input smfGroup has no subgroups (possible programming error)", status );
      }
    } else {
      for ( i=0; i<(*group)->ngroups; i++) {
        /* Retrieve pointer to array of indices */
        if ( subgroups[i] == NULL ) {
          if ( *status == SAI__OK ) {
            msgSetk("I",i);
            *status = SAI__ERROR;
            errRep( FUNC_NAME, "Subgroup ^I is NULL (possible programming error)", status );
          }
        } else {
          indices = subgroups[i];
          /* Free memory associated with current array of indices */
          indices = astFree( indices );
        }
      }
    }
    /* Free pointer to subgroups */
    subgroups = astFree( subgroups );

    /* Finally free the group */
    *group = astFree( *group );
  }
}
