#include <math.h>
#include "sae_par.h"
#include "star/kaplibs.h"
#include "cupid.h"

/* The square root of two. */
#define ROOT_TWO 1.4142136

AstRegion *cupidEllipseDesc( AstFrame *pixel_frm, int space_axes[ 2 ],
                             double beamcorr[ 3 ], double cu,
                             double cu2, double cv2, double cv, double cx,
                             double cy, float sig[ 4 ], int deconv, int *ok,
                             AstMapping *wcsmap, AstFrame *space_frm,
                             AstMapping *space_map, int *status ){
/*
*+
*  Name:
*     cupidEllipseDesc

*  Purpose:
*     Create an AST Ellipse describing the spatial extent of a clump.

*  Language:
*     Starlink C

*  Synopsis:
*     AstRegion *cupidEllipseDesc( AstFrame *pixel_frm, int space_axes[ 2 ],
*                                  double beamcorr[ 3 ], double cu,
*                                  double cu2, double cv2, double cv, double cx,
*                                  double cy, float sig[ 4 ], int deconv, int *ok,
*                                  AstMapping *wcsmap, AstFrame *space_frm,
*                                  AstMapping *space_map, int *status )

*  Description:
*     This function returns an Ellipse describing the spatial extent of the
*     clump specified by the supplied statistics.
*
*     The supplied statistics include the ellipse centre, and the distance
*     from the centre to the ellipse perimeter in four different directions,
*     all specified in pixels coords. These directions are the pixel X and Y
*     axes, plus the "U" and "V" axes, which are axes at 45 degrees to the X
*     and Y spatial axes.

*  Parameters:
*     pixel_frm
*        A 2D Frame describing spatial pixel coords.
*     space_axes[ 2 ]
*        Zero based indices of the two spatial pixel axes.
*     beamcorr
*        An array holding the FWHM (in pixels) describing the instrumental
*        smoothing along each pixel axis. If "deconv" is non-zero, the clump
*        widths and peak values stored in the output catalogue are modified
*        to correct for this smoothing.
*     cu
*        Ellipse centre on the U axis (in pixels).
*     cu2
*        Mean weighted squared U pixel coord.
*     cv2
*        Mean weighted squared V pixel coord.
*     cv
*        Ellipse centre on the V axis (in pixels).
*     cx
*        Ellipse centre on the X axis (in pixels).
*     cy
*        Ellipse centre on the Y axis (in pixels).
*     sig
*        Array to hold ellipse widths along the X, U, Y and V axes.
*        Values for X and Y (elements 0 and 2 ) should be supplied on
*        entry. Values for U and V will be added by this function.
*     deconv
*        If non-zero then the clump property values stored in the
*        catalogue and NDF are modified to remove the smoothing effect
*        introduced by the beam width. If zero, the undeconvolved values
*        are stored in the output catalogue and NDF. Note, the filter to
*        remove clumps smaller than the beam width is still applied, even
*        if "deconv" is zero.
*     ok
*        Pointer to an int in which to return a flag indicating if the
*        clump can be used or not. This will be set to zero if the clump
*        size is zero after correction for the effect of beam smoothing.
*     wcsmap
*        If the returns Ellipse is to be deifne din pixel coordinates, then
*        a NULL pointer should be supplied for "wcsmap". Otherwise, a pointer
*        to a Mapping from the input PIXEL Frame to the WCS Frame should be
*        supplied.
*     space_frm
*        A pointer to the 2D spatial WCS Frame. Ignored if "wcsmap" is NULL.
*     space_map
*        A pointer to the 2D spatial pixel->WCS Mapping. Ignored if "wcsmap"
*        is NULL.
*     status
*        Pointer to the inherited status value.

*  Returned Value:
*     A pointer to the Ellipse, or NULL if no Ellipse can be created.

*  Copyright:
*     Copyright (C) 2009 Science & Technology Facilities Council.
*     All Rights Reserved.

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
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David S. Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     25-MAY-2009 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Local Variables: */
   AstEllipse *ellipse;     /* Returned Ellipse */
   double alpha[ 3 ];       /* List of transformed wcs x values */
   double angle;            /* Inclination of major axis to Y axis */
   double axlens[ 2 ];      /* Semi-major and semi-minor axis lengths */
   double bc2;              /* Squared beam correction for U and V axes */
   double beta[ 3 ];        /* List of transformed wcs y values */
   double cen[ 2 ];         /* Centroid (X,Y) position */
   double p1[ 2 ];          /* End of ellipse major axis */
   double p2[ 2 ];          /* End of ellipse minor axis */
   double v0;               /* Variance before corr'n for instrumental blurring */
   double x[ 3 ];           /* List of pixel x values to transform */
   double y[ 3 ];           /* List of pixel y values to transform */
   float axisr;             /* Ratio of major to minor ellipse axis */
   float sig0;              /* Semi-minor axis of ellipse in pixels */
   float theta;             /* Inclination of major axis to X axis */

/* Abort if an error has already occurred. */
   if( *status != SAI__OK ) return NULL;

/* Initialisation. */
   ellipse = NULL;

/* Estimate the squared instrumental smoothing factor along the U and V
   axes. */
   bc2 =  0.5*( beamcorr[ space_axes[ 0 ] ]*beamcorr[ space_axes[ 0 ] ] +
                beamcorr[ space_axes[ 1 ] ]*beamcorr[ space_axes[ 1 ] ] );

/* Find the RMS width of the clump along the U and V axes. */
   v0 = cu2 - cu*cu;
   if( deconv ) v0 -= bc2/5.5451774;
   if( v0 > 0 ) {
      sig[ 1 ] = sqrt( v0 );
   } else {
      *ok = 0;
   }

   v0 = cv2 - cv*cv;

   if( deconv ) v0 -= bc2/5.5451774;
   if( v0 > 0 ) {
      sig[ 3 ] = sqrt( v0 );
   } else {
      *ok = 0;
   }

/* If the widths are all positive, find the parameters of the ellipse
   that has the same widths as the clump. */
   if( *ok ) {
      kpg1Elgau( sig, &sig0, &axisr, &theta, status );

/* Create an AST Ellipse with these parameters, defined in a 2D PIXEL
   frame. */
      cen[ 0 ] = 0.5*( cx + ( cu - cv )/ROOT_TWO );
      cen[ 1 ] = 0.5*( cy + ( cu + cv )/ROOT_TWO );

      axlens[ 0 ] = sig0*axisr;
      axlens[ 1 ] = sig0;
      angle = AST__DPIBY2 - theta;

      ellipse = astEllipse( pixel_frm, 1, cen, axlens, &angle, NULL, " " );

/* If required, convert the parameter values from pixel units to WCS
   units. */
      if( wcsmap && space_frm && space_map ) {

/* If an STC-S description is required, get the positions of the centre,
   and the end points of the major and minor axes. */
         astEllipsePars( ellipse, cen, axlens, axlens + 1, &angle, p1, p2 );

/* Transform them from pixel coords to WCS coord.s */
         x[ 0 ] = cen[ 0 ];
         x[ 1 ] = p1[ 0 ];
         x[ 2 ] = p2[ 0 ];

         y[ 0 ] = cen[ 1 ];
         y[ 1 ] = p1[ 1 ];
         y[ 2 ] = p2[ 1 ];

         astTran2( space_map, 3, x, y, 1, alpha, beta );

         cen[ 0 ] = alpha[ 0 ];
         p1[ 0 ] = alpha[ 1 ];
         p2[ 0 ] = alpha[ 2 ];

         cen[ 1 ] = beta[ 0 ];
         p1[ 1 ] = beta[ 1 ];
         p2[ 1 ] = beta[ 2 ];

/* Create a new ellipse defined in the WCS Frame. */
         (void) astAnnul( ellipse );
         ellipse = astEllipse( space_frm, 0, cen, p1, p2, NULL, " " );
      }
   }

/* Return the Region pointer. */
   return (AstRegion *) ellipse;
}


