/*
 *+
 *  Name:
 *     smf_dream_getgrid

 *  Purpose:
 *     Routine to establish the DREAM reconstruction grid

 *  Language:
 *     Starlink ANSI C

 *  Type of Module:
 *     Subroutine

 *  Invocation:
 *     smf_dream_getgrid ( AstKeyMap *keymap, double * gridstep, int * ngrid,
 *                        int gridminmax[4], int gridpts[DREAM__MXGRID][2],
 *                        int *status )

 *  Arguments:
 *     keymap = AstKeyMap * (Given)
 *        AST keymap with grid parameters
 *     gridstep = double* (Returned)
 *        Grid spacing in arcsec
 *     ngrid = int* (Returned)
 *        Number of points in reconstruction grid
 *     gridminmax = int[4] (Returned)
 *        Pointer to array of size 4 to receive the extent of the
 *        grid. Elements are xmin, xmax, ymin, ymax.
 *     gridpts = int (Returned)
 *        Array of positions in the reconstruction grid
 *     status = int* (Given and Returned)
 *        Pointer to global status.

 *  Description:
 *     This routine retrieves the grid specifications from the config
 *     file (stored in the given AST keymap) and returns the relevant
 *     grid parameters. An error is given if the keymap does not
 *     contain the relevant parameters. The pointer to gridminmax is
 *     returned NULL on error.

 *  Notes:

 *  Authors:
 *     Andy Gibb (UBC)
 *     TIMJ: Tim Jenness (JAC, Hawaii)
 *     {enter_new_authors_here}

 *  History:
 *     2006-09-15 (AGG):
 *        Initial version
 *     2007-12-18 (AGG):
 *        Update to use new smf_free behaviour
 *     2009-09-18 (TIMJ):
 *        Tweak API to remove unnecessary malloc. GRIDMINMAX is now
 *        a vector rather than 4 elements in the keymap. Update prologue.
 *     {enter_further_changes_here}

 *  Copyright:
 *     Copyright (C) 2009 Science and Technology Facilities Council.
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
#define FUNC_NAME "smf_dream_getgrid"

/* minmax locations */
#define XMIN 0
#define XMAX 1
#define YMIN 2
#define YMAX 3

void smf_dream_getgrid( AstKeyMap *keymap, double *gridstep, int *ngrid,
                        int gridminmax[4], int gridpts[DREAM__MXGRID][2],
                        int *status) {

  /* Local variables */
  int k;                     /* Loop counter */
  int tmp;                   /* General purpose temporary variable */
  int xgrid;                 /* X position in reconstruction grid */
  int ygrid;                 /* Y position in reconstruction grid */

  /* Check status */
  if (*status != SAI__OK) return;

  /* Retrieve relevant settings from config file */
  if( !astMapGet0D( keymap, "GRIDSTEP", gridstep ) ) {
    *gridstep = 6.28; /* Define default value */
  }

  /* Get the grid extent, prefill it just in case */
  gridminmax[XMIN] = 0;
  gridminmax[XMAX] = 1;
  gridminmax[YMIN] = 0;
  gridminmax[YMAX] = 1;
  if( !astMapGet1I( keymap, "GRIDMINMAX", 4, &tmp, gridminmax ) ) {
    *status = SAI__ERROR;
    errRep(FUNC_NAME, "GRIDXMIN unspecified", status);
  }

  if ( *status == SAI__OK ) {
    /* Check gridxmax > gridxmin etc: swap them round by default? */
    if ( gridminmax[XMIN] > gridminmax[XMAX] ) {
      msgOutif(MSG__VERB," ", "Xmin > Xmax: swapping them round", status );
      tmp = gridminmax[XMIN];
      gridminmax[XMIN] = gridminmax[XMAX];
      gridminmax[XMAX] = tmp;
    }
    if ( gridminmax[YMIN] > gridminmax[YMAX] ) {
      msgOutif(MSG__VERB," ", "Ymin > Ymax: swapping them round", status );
      tmp = gridminmax[YMIN];
      gridminmax[YMIN] = gridminmax[YMAX];
      gridminmax[YMAX] = tmp;
    }

    /* Create gridpts array from min/max extent */
    *ngrid = ((gridminmax[XMAX] - gridminmax[XMIN] + 1) *
	      (gridminmax[YMAX] - gridminmax[YMIN] + 1));
    k = 0;
    for ( ygrid=gridminmax[YMIN]; ygrid<=gridminmax[YMAX]; ygrid++ ) {
      for ( xgrid=gridminmax[XMIN]; xgrid<=gridminmax[XMAX]; xgrid++ ) {
        gridpts[k][0] = xgrid;
        gridpts[k][1] = ygrid;
        k++;
      }
    }

  }

}
