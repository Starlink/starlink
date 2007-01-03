/*
*+
*  Name:
*     sc2sim_instap_calc

*  Purpose:
*     Calculate the focal plane offsets

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     sc2sim_instap_calc ( struct sc2sim_obs_struct *inx, double instap[2], 
*                          int *status )

*  Arguments:
*     inx = sc2sim_obs_struct* (Given)
*        Pointer to input OBS struct
*     instap[2] = double (Returned)
*        Focal plane offsets in radians
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This subroutine examines the value for the instap keyword in the
*     given OBS struct and sets the focal plane offsets to the centre
*     of that subarray. If instap is blank or not recognized then the
*     offsets are set to 0 (i.e. no offset tracking).

*  Notes:
*     - Only the s8* subarrays are supported currently

*  Authors:
*     A.G. Gibb (UBC)
*     {enter_new_authors_here}

*  History :
*     2007-01-03 (AGG):
*        Original version

*  Copyright:
*     Copyright (C) 2007 University of British Columbia. All Rights
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

/* Standard includes */
#include <math.h>
#include <string.h>

/* Starlink includes */
#include "ast.h"
#include "star/slalib.h"
#include "mers.h"
#include "sae_par.h"

/* SMURF includes */
#include "sc2sim.h"
#include "smurf_par.h"

#define FUNC_NAME "sc2sim_instap_calc"

void sc2sim_instap_calc
( 
struct sc2sim_obs_struct *inx, /* Pointer to observation struct */
double instap[2],        /* Returned focal plane offsets (radians) */
int *status            /* global status (given and returned) */
)

{

  /* Local variables */
  double halfx;
  double halfy;

  /* Check status */
  if ( *status != SAI__OK ) return;

  /* Calculate midpoint of subarray - include offset from boresight */
  halfx = 0.5 * (inx->nbolx + 4) * inx->bol_distx;
  halfy = 0.5 * (inx->nboly + 4) * inx->bol_disty;

  /* Check for valid subarray */
  if ( strncmp( inx->instap, "s8a", 3 ) == 0 ) {
    inx->instap_x = halfx;
    inx->instap_y = -halfy;
  } else if ( strncmp( inx->instap, "s8b", 3 ) == 0 ) {
    inx->instap_x = -halfy;
    inx->instap_y = -halfx;
  } else if ( strncmp( inx->instap, "s8c", 3 ) == 0 ) {
    inx->instap_x = -halfx;
    inx->instap_y = halfy;
  } else if ( strncmp( inx->instap, "s8d", 3 ) == 0 ) {
    inx->instap_x = halfy;
    inx->instap_y = halfx;
  } else {
    if ( strncmp( inx->instap, " ", 1 ) == 0 ) {
      msgSetc("S", inx->instap);
      msgOutif( MSG__VERB, " ", "Unrecognized subarray name, ^S, assuming zero offsets", status );
    }
    inx->instap_x = 0.0;
    inx->instap_y = 0.0;
  } 
  instap[0] = DAS2R * inx->instap_x;
  instap[1] = DAS2R * inx->instap_y;
}

