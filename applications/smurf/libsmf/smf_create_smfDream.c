/*
*+
*  Name:
*     smf_create_smfDream

*  Purpose:
*     Create an empty smfDream structure

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     dream = smf_create_smfDream( int * status );

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Return Value:
*     smf_create_smfDream = smfDream*
*        Pointer to newly created smfDream. NULL on error.

*  Description:
*     This function allocates memory for a smfDream struct and
*     initializes all values.

*  Authors:
*     Andy Gibb (UBC)
*     {enter_new_authors_here}

*  History:
*     2006-07-26 (AGG):
*        Initial version
*     2007-12-18 (AGG):
*        Update to use new smf_free behaviour
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2006-2007 University of British Columbia. All
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

/* System includes */
#include <stdlib.h>
#include <string.h>

/* Starlink includes */
#include "sae_par.h"
#include "mers.h"
#include "ndf.h"

/* SMURF routines */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"

#define FUNC_NAME "smf_create_smfDream"

smfDream *smf_create_smfDream( int * status ) {

  /* Need to make sure that any memory we malloc will be freed on error
     so make sure we NULL all pointers first. */
  smfDream *dream = NULL;      /* smfDream struct to fill */
  dim_t i;                    /* Loop counter */

  if (*status != SAI__OK) return NULL;

  /* Create an empty smfDream  */
  dream = astMalloc( 1*sizeof(smfDream) );
  if (*status != SAI__OK) {
    /* Add our own message to the stack */
    errRep(FUNC_NAME, "Unable to allocate memory for smfDream structure",
	   status);
    goto CLEANUP;
  }

  /* Integers */
  dream->nvert = 0;
  dream->ngrid = 0;
  dream->ncycles = 0;
  dream->nsampcycle = 0;
  for ( i = 0; i < DREAM__MXVERT; i++ ) {
    (dream->jigvert)[i][0] = 0;
    (dream->jigvert)[i][1] = 0;
  }
  for ( i = 0; i < DREAM__MXGRID; i++ ) {
    (dream->gridpts)[i][0] = 0;
    (dream->gridpts)[i][1] = 0;
  }

  /* Pointers to the grid weights and inverse matrix */
  dream->gridwts = NULL;
  dream->invmatx = NULL;

  /* Doubles */
  dream->jigscal = 0.0;
  dream->gridstep = 0.0;
  for ( i = 0; i < DREAM__MXSAM; i++ ) {
    (dream->jigpath)[i][0] = 0.0;
    (dream->jigpath)[i][1] = 0.0;
  }

  return dream;

 CLEANUP:
  dream = astFree( dream );
  return NULL;
}
