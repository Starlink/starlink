/*
*+
*  Name:
*     smf_find_acsis_corners

*  Purpose:
*     Find the corner detectors.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_find_acsis_corners( smfData *data, double xc[4], double yc[4],
*                             int *status );

*  Arguments:
*     data = smfData * (Given)
*        Point to the structure holding the acsis data.
*     xc = double[ 4 ] (Returned)
*        GRID X coord for the corner detectors.
*     yc = double[ 4 ] (Returned)
*        GRID Y coord for the corner detectors.
*     status = int* (Given and Returned)
*        Pointer to inherited status.

*  Description:
*     This function finds the GRID coords of the corner detectors in an
*     ACSIS data structure. Note, it is possible that there is no
*     detector at one or more of the corners, in which case the detector
*     closest to the corner will be returned.

*  Authors:
*     David S Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     14-DEC-2009 (DSB):
*        Initial version.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2009 Science & Technology Facilities Council.
*     All Rights Reserved.

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

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <math.h>

/* Starlink includes */
#include "ast.h"
#include "par.h"
#include "mers.h"
#include "sae_par.h"
#include "prm_par.h"
#include "star/ndg.h"
#include "star/kaplibs.h"

/* SMURF includes */
#include "smurf_par.h"
#include "smf.h"
#include "sc2da/sc2ast.h"

#define FUNC_NAME "smf_find_acsis_corners"


void smf_find_acsis_corners( smfData *data, double xc[4], double yc[4],
                             int *status ){

/* Local Variables */
   AstFrameSet *wcs = NULL;   /* Focal plane FrameSet */
   double *work = NULL;       /* Array holding focal plane positions */
   double *px = NULL;         /* Pointer to next focal plane X value */
   double *py = NULL;         /* Pointer to next focal plane Y value */
   double d2;                 /* Squared distance from detector to corner */
   double d2_min;             /* Min squared distance from detector to corner */
   double dx;                 /* X offset from detector to corner */
   double dy;                 /* Y offset from detector to corner */
   double fxhi;               /* Upper limit on focal plane X coord */
   double fxlo;               /* Lower limit on focal plane X coord */
   double fyhi;               /* Upper limit on focal plane Y coord */
   double fylo;               /* Lower limit on focal plane Y coord */
   dim_t i;                   /* Detector index */
   int lbnd[ 2 ];             /* GRID coords at lower bounds of grid */
   int ubnd[ 2 ];             /* GRID coords at upper bounds of grid */
   smfHead *hdr;              /* Pointer to the header structure */

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Get a FrameSet in which the base Frame is GRID coords and the current
   Frame is focal plane offsets in radians. */
   hdr = data->hdr;
   hdr->cache2 = smf_create_lutwcs( 0, hdr->fplanex, hdr->fplaney,
                                    hdr->ndet, NULL, 0.0, VAL__BADD,
                                    hdr->instap, hdr->telpos, &wcs,
                                    hdr->cache2, status );

/* The X axis in GRID coords represents detector index, The Y axis in GRID
   coords is just a degenerate axis needed to ensure we have 2 axes (it
   should always have the value 1). Transform every GRID position into
   focal plane coords. */
   lbnd[ 0 ] = 1;
   ubnd[ 0 ] = hdr->ndet;
   lbnd[ 1 ] = 1;
   ubnd[ 1 ] = 1;
   work = astMalloc( 2*hdr->ndet*sizeof( *work ) );
   astTranGrid( wcs, 2, lbnd, ubnd, 0.0, 0, 1, 2, hdr->ndet, work );

/* Find the extereme values of focal plane X and Y. */
   fxlo = VAL__MAXD;
   fylo = VAL__MAXD;
   fxhi = VAL__MIND;
   fyhi = VAL__MIND;
   px = work;
   py = work + hdr->ndet;
   for( i = 0; i < hdr->ndet; i++,px++,py++ ) {
      if( *px < fxlo ) fxlo = *px;
      if( *px > fxhi ) fxhi = *px;
      if( *py < fylo ) fylo = *py;
      if( *py > fyhi ) fyhi = *py;
   }

/* Find the detector that is closest to the bottom left corner. */
   d2_min = VAL__MAXD;
   px = work;
   py = work + hdr->ndet;
   for( i = 1; i <= hdr->ndet; i++ ) {
      dx =  *(px++) - fxlo;
      dy =  *(py++) - fylo;
      d2 = dx*dx + dy*dy;
      if( d2 < d2_min ) {
         xc[ 0 ] = (double) i;
         yc[ 0 ] = 1.0;
         d2_min = d2;
      }
   }


/* Find the detector that is closest to the top left corner. */
   d2_min = VAL__MAXD;
   px = work;
   py = work + hdr->ndet;
   for( i = 1; i <= hdr->ndet; i++ ) {
      dx =  *(px++) - fxlo;
      dy =  *(py++) - fyhi;
      d2 = dx*dx + dy*dy;
      if( d2 < d2_min ) {
         xc[ 1 ] = (double) i;
         yc[ 1 ] = 1.0;
         d2_min = d2;
      }
   }

/* Find the detector that is closest to the top right corner. */
   d2_min = VAL__MAXD;
   px = work;
   py = work + hdr->ndet;
   for( i = 1; i <= hdr->ndet; i++ ) {
      dx =  *(px++) - fxhi;
      dy =  *(py++) - fyhi;
      d2 = dx*dx + dy*dy;
      if( d2 < d2_min ) {
         xc[ 2 ] = (double) i;
         yc[ 2 ] = 1.0;
         d2_min = d2;
      }
   }

/* Find the detector that is closest to the bottom right corner. */
   d2_min = VAL__MAXD;
   px = work;
   py = work + hdr->ndet;
   for( i = 1; i <= hdr->ndet; i++ ) {
      dx =  *(px++) - fxhi;
      dy =  *(py++) - fylo;
      d2 = dx*dx + dy*dy;
      if( d2 < d2_min ) {
         xc[ 3 ] = (double) i;
         yc[ 3 ] = 1.0;
         d2_min = d2;
      }
   }

/* Free resources */
   wcs = astAnnul( wcs );
   work = astFree( work );
}
