/*+
 *  Name:
 *     userradii
 *
 *  Purpose:
 *     Compute average radii at user supplied surface brightness
 *     levels (or plain intensities).
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
   double       dval;
   float        *mu_rad;
   int          i;

   /*  If these measurements are required */
   if ( FLAG(obj.mu_rad[0]) ) {
 
      /*  Measurement and detection images may be same */
      if ( ! dfield ) {
         dfield = field;
      }
      for ( i=0; i < NRAD; i++ ) {

         /*  Correct for zero point of surface brightness */
         dval = prefs.mu_rad[0] - prefs.mu_rad[1];
         
         /*  And generate threshold intensities */
         for( i = 0; i < NRAD; i++ ) {
            threshs[i] = field->pixscale * field->pixscale * 
               pow( 10.0, -0.4 * dval );
            
            /*  Thresholds need to be equal to at least the analysis
                threshold to have any significance */
            if ( threshs[i] < field->thresh ) {
               threshs[i] = BIG;
            }
            dval += prefs.mu_rad[2]; /*  Increment to next
                                         threshold */
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
         for( i = 0, mu_rad = obj->mu_rad, thresht = threshs;
              i < NRAD ; i++, mu_rad++, thresht++ ) {
            if ( PLIST( pixt, value ) > *thresht && *thresht != BIG ) {
               (*mu_rad)++;
            } else if ( *thresht != BIG ) {
               continue;
            }
         }
      }

      /*  Convert counts into radii */
      for( i = 0, mu_rad = obj->mu_rad; i < NRAD; i++ ) {
         *mu_rad = sqrt( *mu_rad / PI );
         mu_rad++;
      }      
   }
}
