/*
*+
*  Name:
*     smf_close_related

*  Purpose:
*     Close a group of related files

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     SMURF subroutine

*  Invocation:
*     smf_close_related( smfArray **relfiles, int *status );

*  Arguments:
*     relfiles = smfArray** (Given and Returned)
*        Pointer to smfArray containing files to be closed. No action
*        performed if *relfiles is NULL.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine retrieves all of the smfDatas stored in the given
*     smfArray and frees up the resources for each one in turn. The
*     input smfArray is also freed and set to NULL on success. If the
*     "owndata" struct component is false, the smfDatas will not be
*     freed (they are assumed to be someone else's responsibility) and
*     only the struct memory will be freed.

*  Notes:
*      - This is the companion routine to smf_open_related.c and
*        smf_create_smfArray.c.

*  Authors:
*     Andy Gibb (UBC)
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2006-07-07 (AGG):
*        Initial version
*     2007-12-18 (AGG):
*        Update to use new smf_free behaviour
*     2008-04-03 (AGG):
*        Free resources even if status is bad
*     2008-07-14 (TIMJ):
*        Free dynamic memory.
*     2008-07-18 (TIMJ):
*        Check for NULL pointer.
*     2008-08-27 (TIMJ):
*        Respect "owndata"

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 2006-2008 University of British Columbia.  All
*     Rights Reserved.

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
#include "star/ndg.h"
#include "star/grp.h"
#include "msg_par.h"

/* SMURF routines */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"

#define FUNC_NAME "smf_close_related"

void smf_close_related ( smfArray **relfiles, int *status ) {

  /* Local variables */
  dim_t i;                  /* Loop counter */
  dim_t nrelated;           /* Number of related files */
  smfData *data;            /* Current smfData */

  /* Always attempt to free resources regardless of status */
  if (*relfiles == NULL) return;

  /* Retrieve number of data files */
  nrelated = (*relfiles)->ndat;

  /* Loop over the number of files and close each smfData */
  if ((*relfiles)->owndata) {
    for (i=0; i<nrelated; i++) {
      data = ((*relfiles)->sdata)[i];
      if ( data != NULL ) {
        smf_close_file( &data, status );
      }
    }
  }

  /* Free dynamically allocated buffer */
  if ((*relfiles)->dyndata) {
    (*relfiles)->dyndata = astFree( (*relfiles)->dyndata );
  }

  /* Free resources of struct */
  *relfiles = astFree( *relfiles );
}
