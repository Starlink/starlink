/*
 *+
 *  Name:
 *     sc2sim_get_recipe.c

 *  Purpose:
 *     Return default recipe name for current observation

 *  Language:
 *     Starlink ANSI C

 *  Type of Module:
 *     Subroutine

 *  Invocation:
 *     sc2sim_get_recipe ( const struct sc2sim_obs_struct *inx, char *recipe,
 *                         dim_t reclen, int *status )

 *  Arguments:
 *     inx = const sc2sim_obs_struct* (Given)
 *        Pointer to observation struct
 *     recipe = char* (Returned)
 *        Name of recipe
 *     reclen = dim_t (Given)
 *        Allocated size of "recipe".
 *     status = int* (Given and Returned)
 *        Pointer to global status.

 *  Description:
 *     Determine the default ORAC-DR recipe name on the basis of the
 *     observing mode (DREAM, STARE, PONG etc) and observation type
 *     (SCIENCE, FOCUS or POINTING). The name of the recipe should not
 *     exceed 30 characters.

 *  Authors:
 *     A.G. Gibb (UBC)
 *     {enter_new_authors_here}

 *  History :
 *     2008-03-19 (AGG):
 *        Original
 *     2008-05-04 (AGG):
 *        Add support for Lissajous observing mode
 *     2008-10-10 (AGG):
 *        Add support for NOISE observing mode
 *     2010-03-16 (TIMJ):
 *        Use one_strlcpy and change API.
 *     2010-07-14 (TM)
 *        Add support for EXTERNAL observing mode

 *  Copyright:
 *     Copyright (C) 2010 Science & Technology Facilities Council.
 *     Copyright (C) 2008 University of British Columbia. All Rights Reserved.

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

/* Standard includes */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Starlink includes */
#include "sae_par.h"
#include "mers.h"
#include "star/one.h"

/* SC2SIM includes */
#include "sc2sim.h"

void sc2sim_get_recipe ( const struct sc2sim_obs_struct *inx, char *recipe,
                         dim_t reclen, int *status ) {

  /* Local variable */
  int scan = 0;           /* Flag to denote SCAN mode data*/

  /* Check status */
  if ( *status != SAI__OK ) return;

  /* Check for SCAN mode data */
  if ( (strncmp( inx->obsmode, "PONG", 4) == 0) ||
       (strncmp( inx->obsmode, "SCAN", 4) == 0) ||
       (strncmp( inx->obsmode, "EXTERNAL", 4) == 0) ||
       (strncmp( inx->obsmode, "LISS", 4) == 0) ||
       (strncmp( inx->obsmode, "BOUS", 4) == 0) ) {
    scan = 1;
  } else {
    scan = 0;
  }

  /* Do we have FOCUS data? */
  if ( (strncmp( inx->obstype, "FOCUS", 5) == 0) ) {
    one_strlcpy( recipe, "REDUCE_FOCUS", reclen, status);
    if ( scan ) {
      one_strlcat( recipe, "_SCAN", reclen, status);
    }
  } else if ( (strncmp( inx->obstype, "POINT", 5) == 0) ) {
    /* POINTING observation */
    one_strlcpy( recipe, "REDUCE_POINTING", reclen, status);
    if ( scan ) {
      one_strlcat( recipe, "_SCAN", reclen, status);
    }
  } else if ( (strncmp( inx->obstype, "SCIENCE", 7) == 0) ) {
    /* SCIENCE observation - now check obsmode */
    if ( scan ) {
      one_strlcpy( recipe, "REDUCE_SCAN", reclen, status);
    } else if ( (strncmp( inx->obsmode, "DREAM", 5) == 0) ||
                (strncmp( inx->obsmode, "STARE", 5) == 0) ) {
      one_strlcpy( recipe, "REDUCE_DREAMSTARE", reclen, status);
    } else if (strncmp( inx->obsmode, "NOISE", 5) == 0) {
      one_strlcpy( recipe, "REDUCE_NOISE", reclen, status);
    } else {
      /* Shouldn't get here... */
      *status = SAI__ERROR;
      errRep("",
             "Error - unrecognized observing mode (possible programming error?)",
             status);
    }
  }

  return;
}
