/*+
 *  Name:
 *     userradii
 *
 *  Purpose:
 *     Compute average radii at user supplied surface brightness
 *     levels (or plain intensities?).
 *
 *  Authors:
 *     (PWD): Peter W. Draper (Starlink, Durham University)
 *
 *  History:
 *     15-FEB-2000 (PWD):
 *        Original version.
 *-
 */

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
                
                   Ii = It * 2^( i + 2 ), i = 2, NRAD
                
                where Ii = threshold for ith level, It = measurement
                threshold. This gives approx 0.75 magnitude steps 
                ( = 2.5*log(2) )
            */

            /* RAD0 is total isophotal area */
            obj->rad[0] = obj->npix;
            for ( i = 1; i < NRAD; i++ ) {
               threshs[i] = field->thresh * pow( 2, i + 2 );
            }

         } else if ( prefs.nrad == 1 ) {
            
            /*  Different start threshold, interval still 0.75 mags */
            for ( i = 0; i < NRAD; i++ ) {
               threshs[i] = prefs.rad[0] * pow( 2, i + 2 );
            }
            
         } else {
            
            /*  Step is in magnitudes. */
            for ( i = 0; i < NRAD; i++ ) {
               threshs[i] = prefs.rad[0] * 
                            pow( 10, -0.4 * prefs.rad[1] * (double)i );
            }

            /*  Later routines expect increasing thresholds so swap if 
                around if necessary */
            if ( prefs.rad[1] > 0 ) {
               for ( i = 0, j = NRAD - 1; i < NRAD/2; i++, j-- ) {
                  tmp = threshs[i];
                  threshs[i] = threshs[j];
                  threshs[j] = tmp;
               }
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
            start = -2.5 * ( log10( field->thresh ) - log10( area ) );
            zero = prefs.mag_zeropoint / area;
            start += zero;
            step = -1.0;
            
            /* RAD0 is total isophotal area */
            obj->rad[0] = obj->npix;
            
         } else if ( prefs.nrad == 1 ) {
            
            /*  Step is one magnitude, zero point is photometry based */
            start = prefs.rad[0];
            zero = prefs.mag_zeropoint / area;
            step = -1.0;
            
         } else if ( prefs.nrad == 2 ) {
            
            /*  Step is one magnitude. */
            start = prefs.rad[0];
            zero = prefs.rad[1];
            step = -1.0;
            
         } else {
            start = prefs.rad[0];
            zero = prefs.rad[1];
            step = prefs.rad[2];
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

      /*  If image intensities are photographic */
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
            if ( PLIST( pixt, value ) >= *thresht && *thresht != BIG ) {
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
