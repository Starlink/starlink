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
 *     sc2sim_getast_wcs ( dim_t colsize, dim_t rowsize, const double *xbolo,
 *                         const double *ybolo, AstCmpMap *bolo2map, const double *astsim,
 *                         const int astnaxes[2], int interp, const double *params,
 *                         double *dbuf, int *status )

 *  Arguments:
 *     colsize = int (Given)
 *        Number of bolometers in a column.
 *     rowsize = int (Given)
 *        Number of bolometers in a row
 *     xbolo = const double* (Given)
 *        X-bolometer coordinates for array
 *     ybolo = const double* (Given)
 *        Y-bolometer coordinates for array
 *     bolo2map = AstCmpMap* (Given)
 *        Mapping bolo->sky image coordinates
 *     astsim = const double* (Given)
 *        Astronomical image
 *     astnaxes = const int[] (Given)
 *        Dimensions of simulated image
 *     interp = int (Given)
 *        Interpolation method. See docs for astResample for available
 *        options.
 *     params = const double * (Given)
 *        Parameters for the interpolation scheme specified by "interp". See docs for
 *        astResample.
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
 *     2009-11-20 (DSB):
 *        Support other interpolation methods in addition to nearest
 *        neighbour.
 *     2009-11-24 (DSB):
 *        Put zeros ito the returned array instead of VAL__BADD values.
 *        Also correct the determination of nearest integer for negative
 *        values of "skycoord".

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
 *     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 *     MA 02110-1301, USA

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
 dim_t colsize,              /* number of bolometers in column (given) */
 dim_t rowsize,              /* number of bolometers in row (given) */
 const double *xbolo __attribute__((unused)), /* x-bolometer coordinates for array (given) */
 const double *ybolo __attribute__((unused)), /* y-bolometer coordinates for array (given) */
 AstCmpMap *bolo2map,         /* mapping bolo->sky image coordinates (given ) */
 const double *astsim,        /* astronomical image (given) */
 const int astnaxes[2],       /* dimensions of simulated image (given) */
 int interp,                  /* interpolation method (given) */
 const double *params,        /* parameters for interpolation method (given) */
 double *dbuf,                /* pointer to bolo output (returned) */
 int *status                  /* global status (given and returned) */
 )

{
  /* Local variables */
  int i;                    /* Loop counter */
  int xnear;                /* Nearest-neighbour x-pixel coordinate */
  int ynear;                /* Nearest-neighbour y-pixel coordinate */
  double *skycoord;         /* x- and y- sky map pixel coordinates */
  int lbnd_in[2];           /* Pixel bounds for astResample */
  int ubnd_in[2];
  int lbnd_out[2];          /* Pixel bounds for astResample */
  int ubnd_out[2];
  int nboll;

  /* Check status */
  if ( !StatusOkP(status) ) return;

  /* First deal with nearest neighbour interpolation... */
  if( interp == AST__NEAREST ) {

     /* astTranGrid method ----------------------------------------- */

     /* Allocate space for arrays */

     nboll = colsize * rowsize;
     skycoord = astCalloc( nboll*2, sizeof(*skycoord) );

     lbnd_in[0] = 1;
     ubnd_in[SC2STORE__ROW_INDEX] = colsize;
     lbnd_in[1] = 1;
     ubnd_in[SC2STORE__COL_INDEX] = rowsize;

     /* Transform bolo offsets into positions on the input sky image */

     astTranGrid( bolo2map, 2, lbnd_in, ubnd_in, 0.1, 1000000, 1,
                  2, nboll, skycoord );

     /* Nearest-neighbour sampling of image
        Notes: -1 to account for FORTRAN array indices starting at 1, and
        +/-0.5 so that we round to the nearest pixel */

     for ( i=0; i<nboll; i++ ) {
       /* Fortran 2d array so stored by column rather than row! */
       xnear = (int) (skycoord[i] + (skycoord[i]>0?0.5:-0.5)) - 1;
       ynear = (int) (skycoord[nboll+i] + (skycoord[nboll+i]>0?0.5:-0.5)) - 1;
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

     skycoord = astFree( skycoord );

  /* Now deal with all other interpolation methods... */

  } else {

     /* astResample requires the inverse Mapping, so invert the supplied
        Mapping temporarily. */

     astInvert( bolo2map );

     /* We want to generate zeros rather than VAL__BADDs in the output
        array, so initialise the output array to hold zeros, and use the
        AST__NOBAD flag when calling astResample. We need to also use the
        AST__USEBAD flag so that bad values in the input are recognised. */
     nboll = colsize * rowsize;
     for ( i=0; i<nboll; i++ ) dbuf[i] = 0.0;

     /* The inputs and outputs of bolo2map are one-based GRID coordinates.
        Store the bounds of the input (sky) array, and output (bolometer)
        array. */

     lbnd_in[0] = 1;
     lbnd_in[1] = 1;
     ubnd_in[0] = astnaxes[0];
     ubnd_in[1] = astnaxes[1];

     lbnd_out[0] = 1;
     lbnd_out[1] = 1;
     ubnd_out[SC2STORE__ROW_INDEX] = colsize;
     ubnd_out[SC2STORE__COL_INDEX] = rowsize;

     /* Resample the sky image into the bolometer array. Not sure if
        there will be any bad values in the sky image, but use the AST__USEBAD
        flag just in case (astResample runs slightly faster without it).
        Each bolometer array contains relatively few pixels, so switch
        off the facility for approximating the Mapping by a linear
        transformation since it won't gain us much in terms of speed (and
        may actually be slower). */

     astResampleD( bolo2map, 2, lbnd_in, ubnd_in, astsim, NULL, interp, NULL,
                   params, AST__USEBAD | AST__NOBAD, 0, 0, VAL__BADD,
                   2, lbnd_out, ubnd_out, lbnd_out, ubnd_out, dbuf, NULL );

     /* Re-invert the supplied Mapping to bring it back to its original state. */

     astInvert( bolo2map );
  }
}




