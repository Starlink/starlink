/*+
 *  Name:
 *     userradii
 *
 *  Purpose:
 *     Compute a series of isophotal radii.
 *
 *  Description:
 *     This routine computes a series of isophotal radii for an
 *     object. The thresholds used for each isophote are given either
 *     as surface brightnesses or intensities using the parameters,
 *     RAD_TYPE and RAD_THRESH.
 *
 *     RAD_TYPE can be either "SB" or "INT" (with the obvious meanings
 *     of surface brightness or intensities).
 *
 *     RAD_THRESH can have up to three values, depending on the value
 *     of RAD_TYPE.
 *
 *     If RAD_TYPE is "SB" then they are:
 *
 *        step[,start,zp]
 *
 *     step being the interval between thresholds, start being the
 *     threshold used as the first level and zp the data zero point
 *     (all in magnitudes per square arcsecond). If only one value is
 *     given then the starting point is assumed to be the analysis
 *     threshold and the zero point is derived from the photometric
 *     value (MAG_ZEROPOINT).
 *
 *     If a RAD_THRESH value is not given then the default step is
 *     0.75 magnitudes.
 *
 *     If RAD_TYPE is "INT" then they are:
 *
 *        step[,start]
 *
 *     step being the interval between levels in magnitudes and start
 *     being the threshold used as the first level. If start is not
 *     given then the analysis threshold is used.
 *
 *     If no RAD_THRESH values are given then the APM analysis method
 *     is used:
 *
 *        Ii = It * 2^( i + 2 ), i = 1, NRAD-1
 *
 *     where Ii = threshold for ith level, It = measurement
 *     threshold. This gives approx 0.75 magnitude steps ( =
 *     2.5*log(2) ).
 *
 *     The actual measurements are associated with the catalogue
 *     parameters, RAD0 through RAD15, at least one of which must be
 *     requested before the results will be calculated.
 *
 *  Authors:
 *     (PWD): Peter W. Draper (Starlink, Durham University)
 *
 *  History:
 *     15-FEB-2000 (PWD):
 *        Original version.
 *- */

#include <math.h>
#include "define.h"
#include "globals.h"
#include "plist.h"
#include "userradii.h"

void userradii( picstruct *field, picstruct *dfield, objstruct *obj,
                pliststruct *pixel )
{
   pliststruct	*pixt;
   PIXTYPE      *thresht;
   PIXTYPE      threshs[NRAD];
   PIXTYPE      tmp;
   double       dval;
   float        *rad;
   float        start;
   float        zero;
   float        step;
   int          i;
   int          j;
   float        area;

   /*  If these measurements are required */
   if ( FLAG(obj.rad[0]) ) {

      /*  Measurement and detection images may be same */
      if ( ! dfield ) {
         dfield = field;
      }

      /*  Intensity and surface brightness thresholds have different
          rules */
      if ( prefs.rad_type == RAD_INT ) {

         /* ====================== */
         /* Intensity calculations */
         /* ====================== */

         /*  Set up the intervals and starting intensities according to
             the differing number of parameters given */
         if ( prefs.nrad == 0 ) {

            /*  No values given. The intervals follow the APM
                description:

                Ii = It * 2^( i + 2 ), i = 1, NRAD-1

                where Ii = threshold for ith level, It = measurement
                threshold. This gives approx 0.75 magnitude steps
                ( = 2.5*log(2) )
            */
            for ( i = 1; i < NRAD; i++ ) {
               threshs[i] = field->thresh * pow( 2, i + 2 );
            }
            threshs[0] = field->thresh;

         } else if ( prefs.nrad == 1 ) {

            /*  Given interval in mags, starting point is analysis
                threshold */
            for ( i = 0; i < NRAD; i++ ) {
               threshs[i] = field->thresh *
                            pow( 10, -0.4 * prefs.rad[0] * (double)i );
            }

         } else {

            /*  Different starting point */
            for ( i = 0; i < NRAD; i++ ) {
               threshs[i] = prefs.rad[1] *
                            pow( 10, -0.4 * prefs.rad[0] * (double)i );
            }
         }

         /*  Later routines expect increasing thresholds so swap if
             around if necessary (step could have wrong sign) */
         if ( prefs.rad[0] > 0 ) {
            for ( i = 0, j = NRAD - 1; i < NRAD/2; i++, j-- ) {
               tmp = threshs[i];
               threshs[i] = threshs[j];
               threshs[j] = tmp;
            }
         }

         /*  Check thresholds are above measurement one */
         for( i = 0; i < NRAD; i++ ) {
            if ( threshs[i] < field->thresh ) {
               threshs[i] = BIG;
            }
         }

      } else {

         /* =============================== */
         /* Surface brightness calculations */
         /* =============================== */
         /*  Area of pixel in arcsec**2 */
         area = field->pixscale * field->pixscale;

         /*  Set up the intervals and starting intensities according to
             the differing number of parameters given */
         if ( prefs.nrad == 0 ) {

            /*  No values given. Use the measurement threshold as
                starting point, the interval is one magnitude per square
                arcsec and the zero point the photometric value */
            step = -0.75;
            start = -2.5 * ( log10( field->thresh ) - log10( area ) );
            zero = prefs.mag_zeropoint / area;
            start += zero;

         } else if ( prefs.nrad == 1 ) {

            /*  Start is analysis threshold and zero point is
                photometry based */
            step = prefs.rad[0];
            start = -2.5 * ( log10( field->thresh ) - log10( area ) );
            zero = prefs.mag_zeropoint / area;
            start += zero;

         } else if ( prefs.nrad == 2 ) {

            /*  Zero point is photometry based*/
            step = prefs.rad[0];
            start = prefs.rad[1];
            zero = prefs.mag_zeropoint / area;

         } else {

            /*  User supplied starting value, zero point and interval
                and in units of surface brightness */
            step = prefs.rad[0];
            start = prefs.rad[1];
            zero = prefs.rad[2];
         }

         /*  Generate thresholds */
         for ( i=0; i < NRAD; i++ ) {

            /*  Correct for zero point of surface brightness */
            dval = start - zero;

            /*  And generate threshold intensities */
            for( i = 0; i < NRAD; i++ ) {
               threshs[i] = area * pow( 10.0, -0.4 * dval );

               /*  Thresholds need to be equal to at least the analysis
                   threshold to have any significance */
               if ( threshs[i] < field->thresh ) {
                  threshs[i] = BIG;
               }
               dval += step; /*  Increment to next threshold */
            }
         }
      }

      /*  Measurements */
      /*  ============ */

      /*  If image intensities are photographic then convert from
          plain intensities to densities */
      if ( prefs.detect_type == PHOTO ) {
         for( i = 0; i < NRAD; i++ ) {
            if ( threshs[i] != BIG ) {
               threshs[i] = log( threshs[i] ) * field->ngamma;
            }
         }
      }

      /*  Look at all pixels in the current object, counting those
          that lie above each of the thresholds, assumes thresholds
          are sorted in increasing order (except those set to BIG,
          which are ignored) */
      for( pixt  = pixel + obj->firstpix; pixt >= pixel;
           pixt  = pixel + PLIST( pixt, nextpix ) ) {
         for( i = 0, rad = obj->rad, thresht = threshs;
              i < NRAD ; i++, rad++, thresht++ ) {
            if ( PLIST( pixt, value ) > *thresht && *thresht != BIG ) {
               (*rad)++;
            } else if ( *thresht != BIG ) {
               continue;
            }
         }
      }

      /*  Convert pixel areas into radii */
      for( i = 0, rad = obj->rad; i < NRAD; i++ ) {
         *rad = sqrt( *rad / PI );
         rad++;
      }
   }
}
