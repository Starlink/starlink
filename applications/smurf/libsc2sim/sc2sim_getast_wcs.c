/*
 *+
 *  Name:
 *     sc2sim_getast_wcs

 *  Purpose:
 *     Sample simulated astronomical image using wcs info

 *  Language:
 *     Starlink ANSI C

 *  Type of Module:
 *     Subroutine

 *  Invocation:
 *     sc2sim_getast_wcs ( int nboll, double *xbolo, double *ybolo,
 *                         AstCmpMap *bolo2map, double *astsim, int astnaxes[2],
 *                         double *dbuf, int *status )

 *  Arguments:
 *     nboll = int (Given)
 *        Total number of bolometers
 *     xbolo = double* (Given)
 *        X-bolometer coordinates for array
 *     ybolo = double* (Given)
 *        Y-bolometer coordinates for array
 *     bolo2map = AstCmpMap* (Given)
 *        Mapping bolo->sky image coordinates
 *     astsim = double* (Given)
 *        Astronomical image
 *     astnaxes = int[] (Given)
 *        Dimensions of simulated image
 *     dbuf = double* (Returned)
 *        Pointer to bolo output
 *     status = int* (Given and Returned)
 *        Pointer to global status.

 *  Description:
 *     Interpolate sky values for all bolometers at the current position
 *     in the astronomical input sky image.
 *     Use Ast mappings to handle bolometer <--> image pixel coordinates
 *     transformation.

 *  Authors:
 *     E.Chapin (UBC)
 *     {enter_new_authors_here}

 *  History :
 *     2006-02-28 (EC):
 *        Original
 *     2006-03-24 (EC):
 *        Use astTranGrid instead of astTran2
 *     2006-07-20 (JB):
 *        Split from dsim.c
 *     2007-05-29 (AGG):
 *        Set bad values to zero
 *     2007-12-18 (AGG):
 *        Update to use new smf_free behaviour

 *  Copyright:
 *     Copyright (C) 2005-2007 Particle Physics and Astronomy Research
 *     Council. University of British Columbia. All Rights Reserved.

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

/* Starlink includes */
#include "ast.h"
#include "prm_par.h"

/* SC2SIM includes */
#include "sc2sim.h"

/* SMURF includes */
#include "libsmf/smf.h"

void sc2sim_getast_wcs
(
 int nboll,                   /* total number of bolometers (given) */
 double *xbolo,               /* x-bolometer coordinates for array (given) */
 double *ybolo,               /* y-bolometer coordinates for array (given) */
 AstCmpMap *bolo2map,         /* mapping bolo->sky image coordinates (given ) */
 double *astsim,              /* astronomical image (given) */
 int astnaxes[2],             /* dimensions of simulated image (given) */
 double *dbuf,                /* pointer to bolo output (returned) */
 int *status                  /* global status (given and returned) */
 )

{
  /* Local variables */
  int i;                    /* Loop counter */
  int xnear;                /* Nearest-neighbour x-pixel coordinate */
  int ynear;                /* Nearest-neighbour y-pixel coordinate */
  double *skycoord;         /* x- and y- sky map pixel coordinates */
  int lbnd_in[2];           /* Pixel bounds for astRebin */
  int ubnd_in[2];

  /* Check status */
  if ( !StatusOkP(status) ) return;

  /* astTranGrid method ----------------------------------------- */

  /* Allocate space for arrays */

  skycoord = smf_malloc( nboll*2, sizeof(*skycoord), 1, status );

  lbnd_in[0] = 1;
  ubnd_in[0] = BOLROW;
  lbnd_in[1] = 1;
  ubnd_in[1] = BOLCOL;

  /* Transform bolo offsets into positions on the input sky image */

  astTranGrid( bolo2map, 2, lbnd_in, ubnd_in, 0.1, 1000000, 1,
               2, nboll, skycoord );

  /* Nearest-neighbour sampling of image
     Notes: -1 to account for FORTRAN array indices starting at 1, and
     +0.5 so that we round to the nearest pixel */

  for ( i=0; i<nboll; i++ ) {
    /* Fortran 2d array so stored by column rather than row! */
    xnear = (int) (skycoord[i] - 1. + 0.5);
    ynear = (int) (skycoord[nboll+i] - 1. + 0.5);
    if( (xnear >= 0) && (xnear < astnaxes[0]) &&
        (ynear >= 0) && (ynear < astnaxes[1]) ) {
      dbuf[i] = astsim[xnear + astnaxes[0]*ynear];
      /* Set to zero if we have a bad value */
      if ( dbuf[i] == VAL__BADD ) {
        dbuf[i] = 0.0;
      }
    } else {
      dbuf[i] = 0.0;
    }
  }

  skycoord = smf_free(skycoord, status);
}




