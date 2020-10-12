#include <math.h>
#include "sae_par.h"
#include "ast.h"
#include "ndf.h"
#include "cupid.h"
#include "prm_par.h"

static void cupid1FitParabola( int nbin, double *hist, int box, int oper,
                               hdsdim *icen, double *vcen, int *status );

AstRegion *cupidEllipseDescNew( AstFrame *pixel_frm, double *ipd, int velax,
                                double *cent, int space_axes[ 2 ], int ndim,
                                hdsdim *lbnd, hdsdim *ubnd, AstMapping *wcsmap,
                                AstFrame *space_frm, AstMapping *space_map,
                                int weight, int *status ){
/*
*+
*  Name:
*     cupidEllipseDescNew

*  Purpose:
*     Create an AST Ellipse describing the spatial extent of a clump.

*  Language:
*     Starlink C

*  Synopsis:
*     AstRegion *cupidEllipseDescNew( AstFrame *pixel_frm, double *ipd, int velax,
*                                     double *cent, int space_axes[ 2 ], int ndim,
*                                     hdsdim *lbnd, hdsdim *ubnd, AstMapping *wcsmap,
*                                     AstFrame *space_frm, AstMapping *space_map,
*                                     int weight, int *status )

*  Description:
*     This function returns an Ellipse describing the spatial extent of the
*     clump specified by the supplied masked clump data array.
*
*     It forms an azimuthal profile of the clump, which gives the mean
*     radius of the clump at each azimuthal angle from zero to 360 (the
*     data value is used as a weight if "weight" is non-zero). It then finds
*     the azimuth with the largest radius (i.e. the largest value in the
*     profile), and fits a parabola to the peak in the profile to determine
*     the position and size of the accurate peak. This defines the semi-major
*     axis of the ellipse. The semi-minor axis is determines from the
*     profile value corresponding to the angle at right angles to the
*     semi-major axis.
*
*     This scheme avoids the very long thin ellipses that the original
*     algorithm (implemenmted in file cupidellipse.c) could return for
*     non-elliptical clumps.

*  Parameters:
*     pixel_frm
*        A 2D Frame describing spatial pixel coords.
*     ipd
*        Pointer to the 2D or 3D masked clump data array.
*     velax
*        The zero-based index of the velocity pixel axis. Should be -1 if
*        there is no velocity axis.
*     cent
*        Array holding pixel coords of the ellipse centre (usually the
*        clump centroid or the clump peak).
*     space_axes[ 2 ]
*        Zero based indices of the two spatial pixel axes.
*     ndim
*        Number of pixel axes.
*     lbnd
*        Point to array holding lower pixel indices of array "ipd".
*     ubnd
*        Point to array holding upper pixel indices of array "ipd".
*     wcsmap
*        If the returned Ellipse is to be defined in pixel coordinates, then
*        a NULL pointer should be supplied for "wcsmap". Otherwise, a pointer
*        to a Mapping from the input PIXEL Frame to the WCS Frame should be
*        supplied.
*     space_frm
*        A pointer to the 2D spatial WCS Frame. Ignored if "wcsmap" is NULL.
*     space_map
*        A pointer to the 2D spatial pixel->WCS Mapping. Ignored if "wcsmap"
*        is NULL.
*     weight
*        If non-zero, the radial distance of each pixel is weighted by the
*        pixel value when forming the mean radial distance at each azimuth.
*        Otherwise, the pixel value is not used as a weight.
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
*        Complete re-write to avoid suprious very long thin ellipses
*        being returned.
*     21-SEP-2018 (DSB):
*        Added argument "weight".
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#define NBIN 360

/* Local Variables: */
   AstEllipse *ellipse;
   double *hist1;
   double *hist2;
   double *mf;
   double *pd;
   double alpha[3];
   double az[4];
   double beta[3];
   double binsize;
   double c1;
   double c2;
   double centre[2];
   double d1;
   double d2;
   double db;
   double df;
   double mf0;
   double mf1;
   double mfsum;
   double p1[2];
   double p2[2];
   double point1[2];
   double r;
   double rmaj;
   double rmin1;
   double rmin2;
   double rmin;
   double t;
   double theta;
   double x[3];
   double xblo;
   double y[3];
   hdsdim *spax2;
   hdsdim *spax1;
   hdsdim i;
   hdsdim ib0;
   hdsdim ib;
   hdsdim ibhi;
   hdsdim iblo;
   hdsdim ibmax;
   hdsdim j;
   hdsdim k;
   int ii;
   int jj;
   int sorted;

/* Abort if an error has already occurred, or if the data is
   one-dimensional. */
   if( *status != SAI__OK || ndim == 1 ) return NULL;

/* Initialisation. */
   ellipse = NULL;

/* Allocate memory for a pair of 1-dimensional arrays in which index
   corresponds to azimuthal angle within the spatial plane, with the supplied
   centre defining the origin and the first spatial pixel axis defining zero
   azimuth. Initialise the contents to zero by using astCalloc rather than
   astMalloc. */
   hist1 = astCalloc( NBIN, sizeof(*hist1) );
   hist2 = astCalloc( NBIN, sizeof(*hist2) );
   mf = astCalloc( NBIN, sizeof(*mf) );
   if( astOK ) {

/* The size of each bin, as a range of azimuthal angle in radians. */
      binsize = 2*AST__DPI/NBIN;

/* Set pointers to the variables holding the indices on the spatial axes. */
      if( velax == 0 ) {
         spax1 = &j;
         spax2 = &k;
         c1 = cent[1];
         c2 = cent[2];
      } else if( velax == 1 ) {
         spax1 = &i;
         spax2 = &k;
         c1 = cent[0];
         c2 = cent[2];
      } else {
         spax1 = &i;
         spax2 = &j;
         c1 = cent[0];
         c2 = cent[1];
      }

/* Loop over all good pixels in the clump to create profiles giving
   weighted radius and total weight as a function of azimuthal angle.  */
      pd = ipd;
      for( k = lbnd[ 2 ]; k <= ubnd[ 2 ]; k++ ) {
         for( j = lbnd[ 1 ]; j <= ubnd[ 1 ]; j++ ) {
            for( i = lbnd[ 0 ]; i <= ubnd[ 0 ]; i++, pd++ ) {
               if( *pd != VAL__BADD && *pd > 0.0 ) {

/* Get the distance from the ellipse centre to the current pixel, projected
   onto the spatial plane. */
                  d1 = *spax1 - c1;
                  d2 = *spax2 - c2;
                  r = sqrt( d1*d1 + d2*d2 );

/* Get the range of azimuthal angle subtended by the current pixel, as seen
   from the centre. This is the angle from the first spatial axis to the
   line from the centre to the current pixel, positive from spatial axis 1
   to spatial axis 2, in radians. Find the azimuth at the four pixel
   corners. */
                  az[ 0 ] = atan2( d2, d1 );
                  az[ 1 ] = atan2( d2-1.0, d1 );
                  az[ 2 ] = atan2( d2, d1-1.0 );
                  az[ 3 ] = atan2( d2-1.0, d1-1.0 );

/* Sort azimuth values into increasing order (bubblesort). */
                  jj = 3;
                  sorted = 0;
                  while( !sorted ) {
                     jj--;
                     sorted = 1;
                     for( ii = 0; ii <= jj; ii++ ) {
                        if( az[ ii + 1 ] < az[ ii ] ) {
                           t = az[ ii ];
                           az[ ii ] = az[ ii + 1 ];
                           az[ ii + 1 ] = t;
                           sorted = 0;
                        }
                     }
                  }

/* If the range straddles the discontinuity at -PI, add 2PI to the
   negative values to put them in the same cycle as the positive values. */
                  if( az[ 3 ] - az[ 0 ] > AST__DPI ) {
                     if( az[ 0 ] < 0.0 ) az[ 0 ] += 2*AST__DPI;
                     if( az[ 1 ] < 0.0 ) az[ 1 ] += 2*AST__DPI;
                     if( az[ 2 ] < 0.0 ) az[ 2 ] += 2*AST__DPI;

/* Sort them again. */
                     jj = 3;
                     sorted = 0;
                     while( !sorted ) {
                        jj--;
                        sorted = 1;
                        for( ii = 0; ii <= jj; ii++ ) {
                           if( az[ ii + 1 ] < az[ ii ] ) {
                              t = az[ ii ];
                              az[ ii ] = az[ ii + 1 ];
                              az[ ii + 1 ] = t;
                              sorted = 0;
                           }
                        }
                     }
                  }

/* We add the current radial distance into the azimuthal profiles,
   weighted by the pixel value, but modified to give smaller weights to
   the azimuthal angles that correspond to the edges of the pixel. We
   also arrange that the sum of these modifying factors is unity, so the
   total weight for each pixel is equal to its data value. The modifying
   factors increase from zero at the lowest azimuth (az[0]) to a constant
   value at az[1] which is maintained until az[2] is reached, at which point
   it drops down to zero again at az[3]. */

/* Change in modifying factor per bin, for the first section
   (az[0]->az[1]) - "df". The middle section has an initial modifying
   factor of unity. */
                  df = binsize/(az[1] - az[0]);

/* The modifying factor at the upper edge of the first bin - "mf0". */
                  xblo = (az[ 0 ] + AST__DPI)/binsize;
                  ib0 = iblo = (int) xblo;
                  db =  1.0 - ( xblo - iblo );
                  mf0 = db*df;
                  if( mf0 > 1.0 ) mf0 = 1.0;

/* Mean modifier value over the first bin. */
                  mfsum = mf[ 0 ] = 0.5*mf0*db;
                  ii = 1;
                  mf1 = 0.0;
                  while( mf1 < 1.0 ) {

/* Modifying factor at upper edge of next bin. */
                     mf1 = mf0 + df;
                     if( mf1 > 1.0 ) mf1 = 1.0;

/* Mean value over next bin (and increment sum of modifying factors). */
                     mfsum += ( mf[ ii++ ] = 0.5*( mf0 + mf1 ) );

/* Upper edge of current bin becomes lower edge of next bin. */
                     mf0 = mf1;
                  }

/* Middle section of modifying factors array is filled with unity. */
                  xblo = (az[ 2 ] + AST__DPI)/binsize;
                  iblo = (int) xblo;
                  for( ; ii < iblo - ib0; ii++ ){
                     mf[ ii ] = 1.0;
                     mfsum += 1.0;
                  }

/* Last section of modifying factors array falls from unity to zero at az[3]. */
                  df = binsize/(az[3] - az[2]);
                  db =  1.0 - ( xblo - iblo );
                  t = 1.0 - 0.5*db*db*df;
                  if( t < 0.0 ) t = 0.0;
                  mf[ ii++ ] = t;
                  mfsum += t;
                  mf0 = 1.0 - db*df;
                  while( mf0 > 0.0 ) {
                     mf1 = mf0 - df;
                     if( mf1 < 0.0 ) mf1 = 0.0;
                     mfsum += ( mf[ ii++ ] = 0.5*( mf0 + mf1 ) );
                     mf0 = mf1;
                  }

/* Normalize the modifying factors to have a sum of unity, then
   multiply them by the pixel data value to get the final weights. */
                  if( weight ) mfsum /= *pd;
                  for( jj = 0; jj < ii; jj++ ) {
                     mf[ jj ] /= mfsum;
                  }

/* Paste the current pixel into the profile arrays, using the weights
   found above (in "mf") for each azimuth bin. Handle the wrap around for
   pixels that straddle the -PI/+PI discontinuity. */
                  for( jj = 0; jj < ii; jj++,ib0++ ) {
                     if( ib0 == NBIN ) ib0 = 0;
                     hist1[ ib0 ] += r*mf[ jj ];
                     hist2[ ib0 ] += mf[ jj ];
                  }
               }
            }
         }
      }

/* Normalize the "hist1" array so that it holds the radius of the clump
   at each azimuth angle. Find the angle at which the maximum radius
   occurs. */
      ibmax = -1;
      rmaj = -1.0;
      for( ib = 0; ib < NBIN; ib++ ) {
         if( hist2[ ib ] > 0.0 ) {
            hist1[ ib ] /= hist2[ ib ];
            if( hist1[ ib ] > rmaj ) {
               rmaj = hist1[ ib ];
               ibmax = ib;
            }
         } else {
            hist1[ ib ] = VAL__BADD;
         }
      }

/* Fit a parabola to the 21 values centred on the maximum. Find the
   position and value of the maximum. This defines the semi-major axis
   of the required ellipse. */
      cupid1FitParabola( NBIN, hist1, 30, 0, &ibmax, &rmaj, status );

/* Get the angle of the semi-major axis, in the form required by the Ellipse
   constructor. */
      theta = AST__DPIBY2 - ( ( ibmax + 0.5 )*binsize - AST__DPI );

/* Get the length of the semi-minor axis. This is the length perpendicular
   to the semimajor axis. Fit a quadratic to the points close by and use
   the fit to determine the radius. Do it on both sides of the major axis
   and use the mean. */
      iblo = ibmax - AST__DPIBY2/binsize;
      if( iblo < 0 ) iblo += NBIN;
      cupid1FitParabola( NBIN, hist1, 30, 1, &iblo, &rmin1, status );

      ibhi = ibmax + AST__DPIBY2/binsize;
      if( ibhi >= NBIN ) ibhi -= NBIN;
      cupid1FitParabola( NBIN, hist1, 30, 1, &ibhi, &rmin2, status );

      rmin = 0.5*( rmin1 + rmin2 );

/* Create the Ellipse. Magnify it by a factor that causes this algorithm
   to produce ellipses that are the same size as the old algorithm. */
      centre[ 0 ] = c1;
      centre[ 1 ] = c2;
      if( weight ) {
         point1[ 0 ] = rmaj*0.73;
         point1[ 1 ] = rmin*0.73;
      } else {
         point1[ 0 ] = rmaj*0.55;
         point1[ 1 ] = rmin*0.55;
      }
      ellipse = astEllipse( pixel_frm, 1, centre, point1, &theta, NULL, " " );

/* If required, convert the parameter values from pixel units to WCS
   units. */
      if( wcsmap && space_frm && space_map ) {

/* If an STC-S description is required, get the positions of the centre,
   and the end points of the major and minor axes. */
         astEllipsePars( ellipse, centre, point1, point1 + 1, &theta, p1, p2 );

/* Transform them from pixel coords to WCS coord.s */
         x[ 0 ] = centre[ 0 ];
         x[ 1 ] = p1[ 0 ];
         x[ 2 ] = p2[ 0 ];

         y[ 0 ] = centre[ 1 ];
         y[ 1 ] = p1[ 1 ];
         y[ 2 ] = p2[ 1 ];

         astTran2( space_map, 3, x, y, 1, alpha, beta );

         centre[ 0 ] = alpha[ 0 ];
         p1[ 0 ] = alpha[ 1 ];
         p2[ 0 ] = alpha[ 2 ];

         centre[ 1 ] = beta[ 0 ];
         p1[ 1 ] = beta[ 1 ];
         p2[ 1 ] = beta[ 2 ];

/* Create a new ellipse defined in the WCS Frame. */
         (void) astAnnul( ellipse );
         ellipse = astEllipse( space_frm, 0, centre, p1, p2, NULL, " " );
      }
   }

/* Free resources. */
   mf = astFree( mf );
   hist1 = astFree( hist1 );
   hist2 = astFree( hist2 );

/* Return the Region pointer. */
   return (AstRegion *) ellipse;
}



#undef NBIN







static void cupid1FitParabola( int nbin, double *hist, int box, int oper,
                               hdsdim *icen, double *vcen, int *status ){

/* Local Variables: */
   double a;
   double b;
   double c;
   double d;
   double dx;
   double dy;
   double dz;
   double nsigma = 3.0;
   double res;
   double rms;
   double sx1;
   double sx1y;
   double sx2;
   double sx2y;
   double sx3;
   double sx4;
   double sy;
   double t;
   double x;
   double xmax;
   double y;
   hdsdim i;
   hdsdim ihi;
   hdsdim ilo;
   hdsdim j;
   hdsdim n;
   hdsdim nrej;
   int iter;
   int niter = 3;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Get the indices of the first and last histogram bin to include in the
   fit. */
   ilo = *icen - box/2;
   ihi = ilo + box - 1;

/* Form the required running sums, allowing for wrap-around between the
   two ends of the array. Scale the X axis values to [0,1].*/
   sx1 = 0.0;
   sx2 = 0.0;
   sx3 = 0.0;
   sx4 = 0.0;
   sx2y = 0.0;
   sx1y = 0.0;
   sy = 0.0;
   n = 0;

   j = ( ilo < 0 ) ? ilo + nbin : ilo;
   for( i = ilo; i <= ihi; i++,j++ ) {

/* Check for wrap-around. */
      if( j == nbin) j = 0;

      y = hist[ j ];
      if( y != VAL__BADD ) {
         t = x = (double) i/(double)nbin;
         sx1 += t;
         sx1y += t*y;
         t *= x;
         sx2 += t;
         sx2y += t*y;
         t *= x;
         sx3 += t;
         t *= x;
         sx4 += t;

         sy += y;
         n++;
      }
   }

/* Form the determinants required to use Cramer's rule to solve the
   normal equations that give the least squares fit. */
   d = sx4*(sx2*n - sx1*sx1) - sx3*(sx3*n - sx1*sx2) + sx2*(sx3*sx1 - sx2*sx2);
   if( d != 0.0 ) {
      dx = sx2y*(sx2*n - sx1*sx1) - sx3*(sx1y*n - sx1*sy) + sx2*(sx1y*sx1 - sx2*sy);
      dy = sx4*(sx1y*n - sx1*sy) - sx2y*(sx3*n - sx1*sx2) + sx2*(sx3*sy - sx1y*sx2);
      dz = sx4*(sx2*sy - sx1y*sx1) - sx3*(sx3*sy - sx1y*sx2) + sx2y*(sx3*sx1 - sx2*sx2);

/* The coefficients of the best fitting quadratic. */
      a = dx/d;
      b = dy/d;
      c = dz/d;
   }

/* Find the RMS residual. */
   sx2 = 0.0;
   n = 0;
   j = ( ilo < 0 ) ? ilo + nbin : ilo;
   for( i = ilo; i <= ihi; i++,j++ ) {
      if( j == nbin) j = 0;
      y = hist[ j ];
      if( y != VAL__BADD ) {
         x = (double) i/(double)nbin;
         res = a*x*x + b*x + c - y;
         sx2 += res*res;
         n++;
      }
   }
   rms = sqrt(sx2/n);

/* Do some sigma clipping iterations. */
   for( iter = 0; iter < niter; iter++ ) {

      sx1 = 0.0;
      sx2 = 0.0;
      sx3 = 0.0;
      sx4 = 0.0;
      sx2y = 0.0;
      sx1y = 0.0;
      sy = 0.0;
      n = 0;
      nrej = 0;

      j = ( ilo < 0 ) ? ilo + nbin : ilo;
      for( i = ilo; i <= ihi; i++,j++ ) {
         if( j == nbin) j = 0;

         y = hist[ j ];
         if( y != VAL__BADD ) {
            t = x = (double) i/(double)nbin;
            res = a*x*x + b*x + c - y;
            if( fabs( res ) > nsigma*rms ) {
               hist[ j ] = VAL__BADD;
               nrej++;
            } else {
               sx1 += t;
               sx1y += t*y;
               t *= x;
               sx2 += t;
               sx2y += t*y;
               t *= x;
               sx3 += t;
               t *= x;
               sx4 += t;

               sy += y;
               n++;
            }
         }
      }

/* If no pointers were rejected we do not need to do any more iterations. */
      if( nrej == 0 ) iter = niter - 1;

/* Form the determinants required to use Cramer's rule to solve the
   normal equations that give the least squares fit. */
      d = sx4*(sx2*n - sx1*sx1) - sx3*(sx3*n - sx1*sx2) + sx2*(sx3*sx1 - sx2*sx2);
      if( d != 0.0 ) {
         dx = sx2y*(sx2*n - sx1*sx1) - sx3*(sx1y*n - sx1*sy) + sx2*(sx1y*sx1 - sx2*sy);
         dy = sx4*(sx1y*n - sx1*sy) - sx2y*(sx3*n - sx1*sx2) + sx2*(sx3*sy - sx1y*sx2);
         dz = sx4*(sx2*sy - sx1y*sx1) - sx3*(sx3*sy - sx1y*sx2) + sx2y*(sx3*sx1 - sx2*sx2);

/* The coefficients of the best fitting quadratic. */
         a = dx/d;
         b = dy/d;
         c = dz/d;

/* If we have done the last iteration, return the required values. */
         if( iter == niter - 1) {

/* The position and value at the maximum. Only allow it to be 10 pixels away
   from the original maximum. */
            if( oper == 0 ) {
               if( a < 0.0 ) {
                  xmax = (-0.5*b/a)*nbin;
                  if( xmax < 0.0 ) xmax += nbin;
                  if( xmax > *icen - 10 && xmax < *icen + 10 ){
                     *icen = (int) xmax;
                     *vcen = c - 0.25*b*b/a;
                  }
               }

/* The accurate radius at the supplied angle. The supplied angle is left
   unchanged. */
            } else {
               x = ((double) *icen )/nbin;
               *vcen = a*x*x + b*x + c;
            }
         }
      }

/* Find the RMS residual. */
      sx2 = 0.0;
      n = 0;
      j = ( ilo < 0 ) ? ilo + nbin : ilo;
      for( i = ilo; i <= ihi; i++,j++ ) {
         if( j == nbin) j = 0;
         y = hist[ j ];
         if( y != VAL__BADD ) {
            x = (double) i/(double)nbin;
            res = a*x*x + b*x + c - y;
            sx2 += res*res;
            n++;
         }
      }
      rms = sqrt(sx2/n);
   }
}





