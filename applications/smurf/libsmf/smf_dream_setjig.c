/*
 *+
 *  Name:
 *     smf_dream_setjig

 *  Purpose:
 *     Routine to convert the DREAM jiggle path into positions in the
 *     reconstruction grid

 *  Language:
 *     Starlink ANSI C

 *  Type of Module:
 *     Subroutine

 *  Invocation:
 *     smf_dream_setjig ( char *subarray, int nsampcycle, double gridstep,
 *                        double jigpath[DREAM__MXSAM][2], int *status )

 *  Arguments:
 *     subarray = char (Given)
 *        Name of subarray
 *     nsampcycle = int (Given)
 *        Number of samples during one DREAM cycle
 *     gridstep (Given)
 *        Size of jiggle grid step in arcsec
 *     jigpath[][2] = double (Given and Returned)
 *        2-D array holding jiggle grid positions
 *     status = int* (Given and Returned)
 *        Pointer to global status.

 *  Description:

 *  Notes:

 *  Authors:
 *     Andy Gibb (UBC)
 *     {enter_new_authors_here}

 *  History:
 *     2006-05-15 (AGG):
 *        Initial test version: copied from calcmapwt.c
 *     {enter_further_changes_here}

 *  Copyright:
 *     Copyright (C) 2006 University of British Columbia. All Rights
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
 *     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 *     MA 02110-1301, USA

 *  Bugs:
 *     {note_any_bugs_here}
 *-
 */

/* Standard includes */
#include <stdio.h>
#include <string.h>

/* Starlink includes */
#include "sae_par.h"
#include "mers.h"
#include "msg_par.h"

/* SMURF includes */
#include "smf.h"

/* Simple default string for errRep */
#define FUNC_NAME "smf_dream_setjig"

void smf_dream_setjig( char *subarray, int nsampcycle, double gridstep,
                       double jigpath[DREAM__MXSAM][2], int *status) {

  /* Local variables */
  int i;                 /* Loop counter */
  double t;              /* Temporary variable */

  /* Check status */
  if (*status != SAI__OK) return;

  if ( strncmp ( subarray, "s4a", 3 ) == 0 ) {
    for ( i=0; i<nsampcycle; i++ ) {
      jigpath[i][0] = jigpath[i][0] / gridstep;
      jigpath[i][1] = jigpath[i][1] / gridstep;
    }
  } else if ( strncmp ( subarray, "s4b", 3 ) == 0 ) {
    for ( i=0; i<nsampcycle; i++ ) {
      t = jigpath[i][1] / gridstep;
      jigpath[i][1] = -jigpath[i][0] / gridstep;
      jigpath[i][0] = t;
    }
  } else if ( strncmp ( subarray, "s4c", 3 ) == 0 ) {
    for ( i=0; i<nsampcycle; i++ ) {
      jigpath[i][0] = -jigpath[i][0] / gridstep;
      jigpath[i][1] = -jigpath[i][1] / gridstep;
    }
  } else if ( strncmp ( subarray, "s4d", 3 ) == 0 ) {
    for ( i=0; i<nsampcycle; i++ ) {
      t = -jigpath[i][1] / gridstep;
      jigpath[i][1] = jigpath[i][0] / gridstep;
      jigpath[i][0] = t;
    }
  } else if ( strncmp ( subarray, "s8a", 3 ) == 0 ) {
    for ( i=0; i<nsampcycle; i++ ) {
      jigpath[i][0] = jigpath[i][0] / gridstep;
      jigpath[i][1] = -jigpath[i][1] / gridstep;
    }
  } else if ( strncmp ( subarray, "s8b", 3 ) == 0 ) {
    for ( i=0; i<nsampcycle; i++ ) {
      t = -jigpath[i][1] / gridstep;
      jigpath[i][1] = -jigpath[i][0] / gridstep;
      jigpath[i][0] = t;
    }
  } else if ( strncmp ( subarray, "s8c", 3 ) == 0 ) {
    for ( i=0; i<nsampcycle; i++ ) {
      jigpath[i][0] = -jigpath[i][0] / gridstep;
      jigpath[i][1] = jigpath[i][1] / gridstep;
    }
  } else if ( strncmp ( subarray, "s8d", 3 ) == 0 ) {
    for ( i=0; i<nsampcycle; i++ ) {
      t = jigpath[i][1] / gridstep;
      jigpath[i][1] = jigpath[i][0] / gridstep;
      jigpath[i][0] = t;
    }
  } else {
    if ( *status == SAI__OK ) {
      msgSetc("S", subarray);
      *status = SAI__ERROR;
      errRep( FUNC_NAME, "Invalid subarray name, ^S", status);
    }
  }

}
