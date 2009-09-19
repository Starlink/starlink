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
 *     smf_dream_getgrid ( const AstKeyMap *keymap, double gridstep, int ngrid,
 *                        int gridpts[DREAM__MXGRID][2], int *status )

 *  Arguments:
 *     keymap = AstKeyMap * (Given)
 *        AST keymap with grid parameters
 *     gridstep = double (Returned)
 *        Grid spacing in arcsec
 *     ngrid = int (Returned)
 *        Number of points in reconstruction grid
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
 *     {enter_new_authors_here}

 *  History:
 *     2006-09-15 (AGG):
 *        Initial version
 *     2007-12-18 (AGG):
 *        Update to use new smf_free behaviour
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
 *     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
 *     MA 02111-1307, USA

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

void smf_dream_getgrid( const AstKeyMap *keymap, double *gridstep, int *ngrid,
                        int **gridminmax, int gridpts[DREAM__MXGRID][2],
                        int *status) {

  /* Local variables */
  int gridxmin = 0;          /* Minimum X extent of reconstruction grid */
  int gridxmax = 1;          /* Maximum X extent of reconstruction grid */
  int gridymin = 0;          /* Minimum Y extent of reconstruction grid */
  int gridymax = 1;          /* Maximum Y extent of reconstruction grid */
  int k;                     /* Loop counter */
  size_t nelem;              /* Number of elements in array */
  int tmp;                   /* General purpose temporary variable */
  int xgrid;                 /* X position in reconstruction grid */
  int ygrid;                 /* Y position in reconstruction grid */

  /* Check status */
  if (*status != SAI__OK) return;

  /* Retrieve relevant settings from config file */
  if( !astMapGet0D( keymap, "GRIDSTEP", gridstep ) ) {
    *gridstep = 6.28; /* Define default value */
  }
  if( !astMapGet0I( keymap, "GRIDXMIN", &gridxmin ) ) {
    *status = SAI__ERROR;
    errRep(FUNC_NAME, "GRIDXMIN unspecified", status);
  }
  if( !astMapGet0I( keymap, "GRIDXMAX", &gridxmax ) ) {
    *status = SAI__ERROR;
    errRep(FUNC_NAME, "GRIDXMAX unspecified", status);
  }
  if( !astMapGet0I( keymap, "GRIDYMIN", &gridymin ) ) {
    *status = SAI__ERROR;
    errRep(FUNC_NAME, "GRIDYMIN unspecified", status);
  }
  if( !astMapGet0I( keymap, "GRIDYMAX", &gridymax ) ) {
    *status = SAI__ERROR;
    errRep(FUNC_NAME, "GRIDYMAX unspecified", status);
  }

  if ( *status == SAI__OK ) {
    /* Check gridxmax > gridxmin etc: swap them round by default? */
    if ( gridxmin > gridxmax ) {
      msgOutif(MSG__VERB," ", "Xmin > Xmax: swapping them round", status );
      tmp = gridxmin;
      gridxmin = gridxmax;
      gridxmax = tmp;
    }
    if ( gridymin > gridymax ) {
      msgOutif(MSG__VERB," ", "Ymin > Ymax: swapping them round", status );
      tmp = gridymin;
      gridymin = gridymax;
      gridymax = tmp;
    }

    /* Create gridpts array from min/max extent */
    *ngrid = ((gridxmax - gridxmin + 1) * (gridymax - gridymin + 1));
    k = 0;
    for ( ygrid=gridymin; ygrid<=gridymax; ygrid++ ) {
      for ( xgrid=gridxmin; xgrid<=gridxmax; xgrid++ ) {
        gridpts[k][0] = xgrid;
        gridpts[k][1] = ygrid;
        k++;
      }
    }
    /* Array to store gridextent */
    nelem = 4;
    *gridminmax = smf_malloc( nelem, sizeof(int), 1, status);
    if ( *status == SAI__OK ) {
      (*gridminmax)[0] = gridxmin;
      (*gridminmax)[1] = gridxmax;
      (*gridminmax)[2] = gridymin;
      (*gridminmax)[3] = gridymax;
    } else {
      *gridminmax = smf_free( gridminmax, status );
      errRep(FUNC_NAME, "Unable to allocate memory for grid extent array", status);
    }
  } else {
    *gridminmax = NULL;
  }

}
