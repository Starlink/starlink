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
 *     sc2sim_instap_calc ( struct sc2sim_obs_struct *inx, int mstp,
 *                          double instap[2], int *status )

 *  Arguments:
 *     inx = sc2sim_obs_struct* (Given)
 *        Pointer to input OBS struct
 *     mstp = int
 *        Current microstep
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
 *     C. VanLaerhoven (UBC)
 *     {enter_new_authors_here}

 *  History :
 *     2007-01-03 (AGG):
 *        Original version
 *     2007-08-27 (CV):
 *        Added microstepping
 *     2008-04-17 (AGG):
 *        Deal with 450um arrays

 *  Copyright:
 *     Copyright (C) 2007-2008 University of British Columbia. All
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

/* Standard includes */
#include <math.h>
#include <string.h>

/* Starlink includes */
#include "ast.h"
#include "mers.h"
#include "sae_par.h"

/* SMURF includes */
#include "sc2sim.h"
#include "smurf_par.h"

#define FUNC_NAME "sc2sim_instap_calc"

void sc2sim_instap_calc
(
 struct sc2sim_obs_struct *inx, /* Pointer to observation struct */
 int mstp,              /* current microstep */
 double instap[2],      /* Returned focal plane offsets (radians) */
 int *status            /* global status (given and returned) */
 )

{

  /* Local variables */
  double halfx;
  double halfy;
  double instap_arr[2]={0,0};
  double instap_ms[2]={0,0};

  /* Check status */
  if ( *status != SAI__OK ) return;

  /* Calculate midpoint of subarray - include offset from boresight */
  halfx = 0.5 * (inx->colsize + 4) * inx->bol_distx;
  halfy = 0.5 * (inx->rowsize + 4) * inx->bol_disty;

  if  ( strncmp( inx->instap, " ", 1 ) != 0 ) {
    /* Check for valid subarray */
    if ( (strncmp( inx->instap, "s8a", 3 ) == 0 ) ||
         (strncmp( inx->instap, "s4d", 3 ) == 0 ) ){
      instap_arr[0] = halfx;
      instap_arr[1] = -halfy;
    } else if ( (strncmp( inx->instap, "s8b", 3 ) == 0 ) ||
                (strncmp( inx->instap, "s4c", 3 ) == 0 ) ) {
      instap_arr[0] = -halfy;
      instap_arr[1] = -halfx;
    } else if ( (strncmp( inx->instap, "s8c", 3 ) == 0 ) ||
                (strncmp( inx->instap, "s4b", 3 ) == 0 ) ) {
      instap_arr[0] = -halfx;
      instap_arr[1] = halfy;
    } else if ( (strncmp( inx->instap, "s8d", 3 ) == 0 ) ||
                (strncmp( inx->instap, "s4a", 3 ) == 0 ) ) {
      instap_arr[0] = halfy;
      instap_arr[1] = halfx;
    } else {
      if ( strncmp( inx->instap, " ", 1 ) == 0 ) {
        msgSetc("S", inx->instap);
        msgOutif( MSG__VERB, " ",
                  "Unrecognized subarray name, ^S, assuming zero offsets",
                  status );
      }
      instap_arr[0] = 0.0;
      instap_arr[1] = 0.0;
    }
  }

  if ( inx->nmicstep > 1 ) {
    instap_ms[0] = inx->mspat_x[mstp] * inx->bol_distx;
    instap_ms[1] = inx->mspat_y[mstp] * inx->bol_disty;
  }

  inx->instap_x = instap_arr[0] + instap_ms[0];
  inx->instap_y = instap_arr[1] + instap_ms[1];

  instap[0] = ERFA_DAS2R * inx->instap_x;
  instap[1] = ERFA_DAS2R * inx->instap_y;

}

